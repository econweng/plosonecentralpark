library(dplyr)
library(foreign)
library(tidyverse)
library(sandwich)
library(ggplot2)
library(lubridate)
library(ggpubr)
library(tigris)
library(tidycensus)
library(tis)
#================Import cell phone data=================================

cp_visitor<- read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Submission/PLOS ONE/R&R/Code/centralpark_visit_1920.csv")


#===============Merge covid, policy, and weather variables=================
covidcase<-readRDS("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Control variables/COVID-19_Unified-Dataset-master/COVID-19.rds")

NYCcase<-covidcase%>%
  filter(ID=="US36005"|ID=="US36047"|ID=="US36061"|ID=="US36081"|ID=="US36085")%>%filter(Type=="Confirmed")

NYCcase$week<-week(NYCcase$Date)
NYCcase$year<-year(NYCcase$Date)
NYCcase$Day<-as.numeric(NYCcase$Date)
NYCcase$stayhomepolicy<-ifelse(NYCcase$Day<18425&NYCcase$Day>18329,1,0)
NYCcase$county<-substr(NYCcase$ID,3,7)

NYCweekcase<-NYCcase%>%group_by(year,week,county)%>%
  summarise(case=sum(Cases),case_new=sum(Cases_New))
NYCweekcase$stayhomepolicy<-ifelse(NYCweekcase$week<24&NYCweekcase$week>10,1,0)

covidpolicy<-readRDS("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Control variables/COVID-19_Unified-Dataset-master/Policy.rds")
NYSpolicy<-covidpolicy%>%filter(ID=="US36")

NYSC6<-NYSpolicy%>%filter(PolicyType=="C6")
NYSC5<-NYSpolicy%>%filter(PolicyType=="C5")

countydeclaration<-read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Control variables/County_Declaration_and_Policies.csv")
NYCdeclare<-countydeclaration%>%
  filter(FIPS=="36005"|FIPS=="36047"|FIPS=="36061"|FIPS=="36081"|FIPS=="36085")


## weather variables
weather19<-read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Control variables/weather/weather_county_2019.csv")

NYCweather19<-weather19%>%
  filter(county=="36005"|county=="36047"|county=="36061"|county=="36081"|county=="36085")

NYCweather19$week<-week(NYCweather19$date)
NYCweather19$year<-year(NYCweather19$date)

weather20<-read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Control variables/weather/weather_county_2020.csv")

NYCweather20<-weather20%>%
  filter(county=="36005"|county=="36047"|county=="36061"|county=="36081"|county=="36085")

NYCweather20$week<-week(NYCweather20$date)
NYCweather20$year<-year(NYCweather20$date)

NYCweather<-rbind(NYCweather19,NYCweather20)

NYCweekweather<-NYCweather%>%
  group_by(year,week,county)%>%
  summarise(mean_precip=mean(precip),
            mean_tmax=mean(tmax),
            mean_tmin=mean(tmin)
            )

NYCweekweather$county<-as.character(NYCweekweather$county)
NYCcontrol<-left_join(NYCweekweather,NYCweekcase,by=c("week","year","county"))

NYCcontrol$case<- ifelse(is.na(NYCcontrol$case), 0, NYCcontrol$case)
NYCcontrol$case_new<- ifelse(is.na(NYCcontrol$case_new), 0, NYCcontrol$case_new)
NYCcontrol$stayhomepolicy<- ifelse(is.na(NYCcontrol$stayhomepolicy), 0, NYCcontrol$stayhomepolicy)

#======Construct sociodemographic information at blockgroup level======

bgincome_2019<-get_acs(geography = "block group",variables="B19301_001",year=2019,survey="acs5",state="NY")
bgincome_2019$year<-2019

bgincome_2020<-get_acs(geography = "block group",variables="B19301_001",year=2020,survey="acs5",state="NY")
bgincome_2020$year<-2020

bgincome_2019<-bgincome_2019%>%rename("npercapitaincome"="estimate")
bgincome_2020<-bgincome_2020%>%rename("npercapitaincome"="estimate")

bgpercapitaincome_1920<-rbind(bgincome_2019,bgincome_2020)

#get total population for each year
bgtotalpopulation_2019<-get_acs(geography = "block group",variables="B01001_001",year=2019,survey="acs5",state="NY")
bgtotalpopulation_2019$year<-2019

bgtotalpopulation_2020<-get_acs(geography = "block group",variables="B01001_001",year=2020,survey="acs5",state="NY")
bgtotalpopulation_2020$year<-2020

bgtotalpopulation_2019<-bgtotalpopulation_2019%>%rename("totalpopulation"="estimate")
bgtotalpopulation_2020<-bgtotalpopulation_2020%>%rename("totalpopulation"="estimate")

bgtotalpopulation_1920<-rbind(bgtotalpopulation_2019,bgtotalpopulation_2020)

#medianincome
bgmedianincome_2019<-get_acs(geography = "block group",variables="B19013_001",year=2019,survey="acs5",state="NY")
bgmedianincome_2019$year<-2019

bgmedianincome_2020<-get_acs(geography = "block group",variables="B19013_001",year=2020,survey="acs5",state="NY")
bgmedianincome_2020$year<-2020

bgmedianincome_2019<-bgmedianincome_2019%>%rename("medianincome"="estimate")
bgmedianincome_2020<-bgmedianincome_2020%>%rename("medianincome"="estimate")

bgmedianincome_1920<-rbind(bgmedianincome_2019,bgmedianincome_2020)


#education
bgabovehighschool_19 <- get_acs(
  geography = "block group",
  variables = paste0("B15003_0", 17:25),  # hs diploma and above variables
  summary_var = "B15003_001",             # pop 25 years and older - denominator
  year=2019,survey="acs5",
  state ="NY"
)

bgabovehighschool_19$year<-2019

bgabovehighschool_20 <- get_acs(
  geography = "block group",
  variables = paste0("B15003_0", 17:25),  # hs diploma and above variables
  summary_var = "B15003_001",             # pop 25 years and older - denominator
  year=2020,survey="acs5",
  state = "NY"
)

bgabovehighschool_20$year<-2020

cbgabovehighschool_19 <-bgabovehighschool_19%>% 
  group_by(GEOID) %>% 
  summarize(
    n_hs_above = sum(estimate),
    n_pop_over_25 = summary_est[1]
  ) 

cbgabovehighschool_19$pct_hs_above<-cbgabovehighschool_19$n_hs_above/cbgabovehighschool_19$n_pop_over_25

cbgabovehighschool_19$year<-2019

cbgabovehighschool_20 <-bgabovehighschool_20%>% 
  group_by(GEOID) %>% 
  summarize(
    n_hs_above = sum(estimate),
    n_pop_over_25 = summary_est[1]
  ) 

cbgabovehighschool_20$pct_hs_above<-cbgabovehighschool_20$n_hs_above/cbgabovehighschool_20$n_pop_over_25

cbgabovehighschool_20$year<-2020

cbgabovehighschool_1920<-rbind(cbgabovehighschool_19,cbgabovehighschool_20)

#age
bgmedianage_2019<-get_acs(geography = "block group",variables="B01002_001",year=2019,survey="acs5",state="NY")

bgmedianage_2019$year<-2019

bgmedianage_2020<-get_acs(geography = "block group",variables="B01002_001",year=2020,survey="acs5",state="NY")

bgmedianage_2020$year<-2020

bgmedianage_2019<-bgmedianage_2019%>%rename("medianage"="estimate")
bgmedianage_2020<-bgmedianage_2020%>%rename("medianage"="estimate")

bgmedianage_1920<-rbind(bgmedianage_2019,bgmedianage_2020)

bg_socio<-bgpercapitaincome_1920%>%left_join(bgmedianage_1920,by="GEOID")
bg_socio<-bg_socio%>%left_join(cbgabovehighschool_1920,by="GEOID")
bg_socio<-bg_socio%>%left_join(bgmedianincome_1920,by="GEOID")
bg_socio<-bg_socio%>%left_join(bgtotalpopulation_1920,by="GEOID")

bg_socio<-bg_socio%>%rename("visitor_home_cbgs"="GEOID")

#======Construct NTA level sociodemographic info======

bgtonta<-read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/NYC/NTA/newbgtonta.csv")
names(bgtonta)[9]<-"visitor_home_cbgs"

bgtonta$visitor_home_cbgs<-as.character(bgtonta$visitor_home_cbgs)

bg_sociodemo_merge<-left_join(bg_socio,bgtonta,by="visitor_home_cbgs")

bg_sociodemo_merge$county<-substr(bg_sociodemo_merge$visitor_home_cbgs,1,5)

NYCbg_socio<-bg_sociodemo_merge%>%
  filter(county=="36005"|county=="36047"|county=="36061"|county=="36081"|county=="36085")

nta_sociodemo<-NYCbg_socio%>%
  group_by(NTACode,county,year)%>%
  summarize(nta_median_household_income=weighted.mean(medianincome,totalpopulation,na.rm = TRUE),
            nta_percapitaincome=weighted.mean(npercapitaincome,totalpopulation,na.rm=TRUE),
            nta_totalpopulation=sum(totalpopulation),
            nta_medianage=weighted.mean(medianage,totalpopulation,na.rm=TRUE),
            nta_pct_hs_above=weighted.mean(pct_hs_above,totalpopulation,na.rm=TRUE)
  )

#======Construct NTA level visitor data======

cp_visitor<-cp_visitor%>%rename("visitor_home_cbgs"="visitor_home_cbgs.y")

cp_visitor$visitor_home_cbgs<-as.character(cp_visitor$visitor_home_cbgs)

nta_merge<-left_join(cp_visitor,bgtonta,by="visitor_home_cbgs")

nta_merge$county<-substr(nta_merge$visitor_home_cbgs,1,5)

NYC_nta_merge<-nta_merge%>%
  filter(county=="36005"|county=="36047"|county=="36061"|county=="36081"|county=="36085")

NYC_nta_merge$visitortotalcost<-NYC_nta_merge$totalcost*NYC_nta_merge$visitor_cbg_count

nta_visitor<-NYC_nta_merge%>%
  group_by(NTACode,week,year,county)%>%
  summarize( nta_visitor=sum(visitor_cbg_count),
            nta_averagetotalcost=sum(visitortotalcost)/nta_visitor
  )


#======Build data for zonal model==============

#merge travel cost data with control variables
nta_code = bgtonta$NTACode

nta_code_unique=unique(nta_code)

cp_nta = data.frame(NTACode=rep(nta_code_unique,52*2),
                       week = rep(rep(1:52,each=length(nta_code_unique)),2),
                       year = rep(2019:2020,each=length(nta_code_unique)*52))

cp_nta$week<-as.character(cp_nta$week)
cp_nta$year<-as.character(cp_nta$year)

#merge socio-demographic info

nta_sociodemo$year<-as.character(nta_sociodemo$year)

cp_nta_social<-cp_nta %>% 
  left_join(nta_sociodemo,by=c("NTACode","year"))

#merge visitor info
nta_visitor$year<-as.character(nta_visitor$year)
nta_visitor$week<-as.character(nta_visitor$week)

cp_nta_social_visitor<-cp_nta_social %>% 
  left_join(nta_visitor,by=c("NTACode","year","week","county"))


#edit data for missed total costs
ntatocp <- read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Dataset/Central Park/nta_to_cp.csv") 

new_cp_NY_nta_edit<-left_join(cp_nta_social_visitor,ntatocp,by=c("NTACode"))

new_cp_NY_nta_edit$ntadrivingcost<-(new_cp_NY_nta_edit$meters-321.90)/321.90*0.5*2
new_cp_NY_nta_edit$ntatimecost<-new_cp_NY_nta_edit$nta_percapitaincome/(2080*60)*new_cp_NY_nta_edit$minutes*2
new_cp_NY_nta_edit$nta_edittotalcost<-new_cp_NY_nta_edit$ntadrivingcost+new_cp_NY_nta_edit$ntatimecost


new_cp_NY_nta_edit$nta_travelcost<- ifelse(is.na(new_cp_NY_nta_edit$nta_averagetotalcost), new_cp_NY_nta_edit$nta_edittotalcost, new_cp_NY_nta_edit$nta_averagetotalcost)

#remove missing values
new_cp_NY_nta_edit<-new_cp_NY_nta_edit%>%filter(!is.na(new_cp_NY_nta_edit$nta_travelcost))

new_cp_NY_nta_edit<-new_cp_NY_nta_edit%>%
  filter(NTACode!="MN99")

#calculate visitation rates of 1000 people
new_cp_NY_nta_edit$nta_visitor_count<- ifelse(is.na(new_cp_NY_nta_edit$nta_visitor), 0, new_cp_NY_nta_edit$nta_visitor)

new_cp_NY_nta_edit$nta_visitor_count<-round((new_cp_NY_nta_edit$nta_visitor_count/(0.1*0.81)),digits=0)

new_cp_NY_nta_edit$visitrateper1000<-new_cp_NY_nta_edit$nta_visitor_count/new_cp_NY_nta_edit$nta_totalpopulation*1000
  
#merge other control variables

NYCcontrol$year<-as.character(NYCcontrol$year)
NYCcontrol$week<-as.character(NYCcontrol$week)

new_cp_NY_nta_edit<-new_cp_NY_nta_edit %>% 
  left_join(NYCcontrol,by=c("county","year","week"))

#generate indicator for weeks with federal holidays
hld_2019 <- tis::holidays(2019)

hld_2020 <- tis::holidays(2020)


hld<-c(hld_2019,hld_2020)

hld<- as.Date(as.character(hld),          # as.Date & as.character functions
                   format = "%Y%m%d")

hld<-as.data.frame(hld)
hld$week<-isoweek(hld$hld)
hld$year<-year(hld$hld)

hld$holidayweek<-1

hld$year<-as.character(hld$year)
hld$week<-as.character(hld$week)

new_cp_NY_nta_edit<-new_cp_NY_nta_edit %>% 
  left_join(hld,by=c("year","week"))

new_cp_NY_nta_edit$holidayweek[is.na(new_cp_NY_nta_edit$holidayweek)]<-0

#remove NTA=QN30 due to unrealistic number
new_cp_NY_nta_edit<- new_cp_NY_nta_edit[new_cp_NY_nta_edit$NTACode != "QN30", ]


write_csv(new_cp_NY_nta_edit,"/Users/wweng/Dropbox/Research/Pandemic and Recreation/Submission/PLOS ONE/R&R/Code/centralpark_nta.csv")



