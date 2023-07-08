library(dplyr)
library(foreign)
library(tidyverse)
library(sandwich)
library(ggplot2)
library(lubridate)
library(ggpubr)

nta_data<-read.csv("/Users/wweng/Dropbox/Research/Pandemic and Recreation/Submission/PLOS ONE/R&R/Code/centralpark_nta.csv")

nta_data$stayhomepolicy<-NULL
nta_data$stayhomepolicy[nta_data$year==2020&nta_data$week>11&nta_data$week<27]<-1
nta_data$stayhomepolicy[nta_data$year==2020&nta_data$week<12]<-0
nta_data$stayhomepolicy[nta_data$year==2020&nta_data$week>26]<-0
nta_data$stayhomepolicy[nta_data$year==2019]<-0

nta_data$period<-NULL
nta_data$period[nta_data$year==2019&nta_data$week<12]<-1
nta_data$period[nta_data$year==2019&(nta_data$week>11&nta_data$week<27)]<-2
nta_data$period[nta_data$year==2019&(nta_data$week>26&nta_data$week<53)]<-3
nta_data$period[nta_data$year==2020&nta_data$week<12]<-4
nta_data$period[nta_data$year==2020&(nta_data$week>11&nta_data$week<27)]<-5
nta_data$period[nta_data$year==2020&(nta_data$week>26&nta_data$week<53)]<-6


nta_data$pandemicweek<-NULL
nta_data$pandemicweek[nta_data$period==4]<-nta_data$week[nta_data$period==4]-11
nta_data$pandemicweek[nta_data$period==1]<-0
nta_data$pandemicweek[nta_data$period==2]<-0
nta_data$pandemicweek[nta_data$period==3]<-0


nta_data$month<-NULL
nta_data$month[nta_data$week<5&nta_data$week>0]<-1
nta_data$month[nta_data$week<9&nta_data$week>4]<-2
nta_data$month[nta_data$week<14&nta_data$week>8]<-3
nta_data$month[nta_data$week<18&nta_data$week>13]<-4
nta_data$month[nta_data$week<23&nta_data$week>17]<-5
nta_data$month[nta_data$week<27&nta_data$week>22]<-6
nta_data$month[nta_data$week<31&nta_data$week>26]<-7
nta_data$month[nta_data$week<36&nta_data$week>30]<-8
nta_data$month[nta_data$week<40&nta_data$week>35]<-9
nta_data$month[nta_data$week<45&nta_data$week>39]<-10
nta_data$month[nta_data$week<49&nta_data$week>44]<-11
nta_data$month[nta_data$week<53&nta_data$week>48]<-12

nta_data$Jan<-0
nta_data$Jan[nta_data$month==1]<-1

nta_data$Feb<-0
nta_data$Feb[nta_data$month==2]<-1

nta_data$Mar<-0
nta_data$Mar[nta_data$month==3]<-1

nta_data$Apr<-0
nta_data$Apr[nta_data$month==4]<-1

nta_data$May<-0
nta_data$May[nta_data$month==5]<-1

nta_data$Jun<-0
nta_data$Jun[nta_data$month==6]<-1

nta_data$Jul<-0
nta_data$Jul[nta_data$month==7]<-1

nta_data$Aug<-0
nta_data$Aug[nta_data$month==8]<-1

nta_data$Sep<-0
nta_data$Sep[nta_data$month==9]<-1

nta_data$Oct<-0
nta_data$Oct[nta_data$month==10]<-1

nta_data$Nov<-0
nta_data$Nov[nta_data$month==11]<-1

nta_data$Dec<-0
nta_data$Dec[nta_data$month==12]<-1


nta_mean_period<-nta_data%>%
  group_by(period)%>%
  summarise(sum_visitor=sum(nta_visitor_count),
    mean_visitor=mean(nta_visitor_count),
            mean_totalpop=mean(nta_totalpopulation),
            mean_income=mean(nta_median_household_income,na.rm = TRUE),
            mean_age=mean(nta_medianage),
            mean_highedu=mean(nta_pct_hs_above),
            mean_precip=mean(mean_precip),
            mean_tmax=mean(mean_tmax),
            mean_tmin=mean(mean_tmin),
            mean_newcase=mean(case_new),
            mean_totalcost=mean(nta_travelcost),
            mean_pandemicweek=mean(pandemicweek),
            mean_holidayweek=mean(holidayweek),
            mean_Jan=mean(Jan),
            mean_Feb=mean(Feb),
            mean_Mar=mean(Mar),
            mean_Apr=mean(Apr),
            mean_May=mean(May),
            mean_Jun=mean(Jun),
            mean_Jul=mean(Jul),
            mean_Aug=mean(Aug),
            mean_Sep=mean(Sep),
            mean_Oct=mean(Oct),
            mean_Nov=mean(Nov),
            mean_Dec=mean(Dec))


#====================Calculate mean of CS================

#period 1 (2019, week 1-11)

nta_p1<-nta_data%>%
  filter(period==1)

period1<-nta_mean_period%>%
  filter(period==1)

period1$beta0<--0.0000681*period1$mean_newcase+0.0000133*period1$mean_income+0.0311546*period1$mean_age-0.0067061*period1$mean_precip+0.0539785*period1$mean_tmax+.2410574*period1$mean_holidayweek-0.3782999*period1$mean_Mar

period1$beta1<--1.922659

nta_p1$poptc<-nta_p1$nta_totalpopulation/1000*(nta_p1$nta_travelcost)^(1-1.922659)

period1$meancs<-(-1)*exp(period1$beta0)/(1+period1$beta1)*sum(nta_p1$poptc)


#weekly consumer surplus
(period1$meancs/11)/0.11


#period 2 (2019, week 12-26)

nta_p2<-nta_data%>%
  filter(period==2)

period2<-nta_mean_period%>%
  filter(period==2)

period2$beta0<--0.0001291*period2$mean_newcase+0.0000134*period2$mean_income+0.0344715*period2$mean_age+0.0338816*period2$mean_tmax+0.2759899

period2$beta1<--2.014236

nta_p2$poptc<-nta_p2$nta_totalpopulation/1000*(nta_p2$nta_travelcost)^(1-2.014236)

period2$meancs<-(-1)*exp(period2$beta0)/(1+period2$beta1)*sum(nta_p2$poptc)


#weekly consumer surplus
(period2$meancs/15)/0.11

#period 3 (2019, week 27-52)

nta_p3<-nta_data%>%
  filter(period==3)

period3<-nta_mean_period%>%
  filter(period==3)

nta_p13<-nta_data%>%
  filter(period==1)

period13<-nta_mean_period%>%
  filter(period==1)

period13$beta0<-0.0002443*period1$mean_newcase+0.0000144*period1$mean_income+0.0325213*period1$mean_age-0.0275361*period1$mean_precip+0.0334077*period1$mean_tmax+.2872425*period1$mean_holidayweek-0.2259468*period1$mean_Mar

period13$beta1<--1.865395

nta_p13$poptc<-nta_p13$nta_totalpopulation/1000*(nta_p13$nta_travelcost)^(1-1.865395)

period13$meancs<-(-1)*exp(period13$beta0)/(1+period13$beta1)*sum(nta_p13$poptc)

(period13$meancs/11)/0.11

#weekly consumer surplus
(period3$meancs/26)/0.11


period3$beta0<-0.0002443*period3$mean_newcase+0.0000144*period3$mean_income+0.0325213*period3$mean_age-0.0275361*period3$mean_precip+0.0334077*period3$mean_tmax+.2872425*period3$mean_holidayweek-0.6140076*period3$mean_Aug-0.3777834*period3$mean_Oct+0.3760982

period3$beta1<--1.865395

nta_p3$poptc<-nta_p3$nta_totalpopulation/1000*(nta_p3$nta_travelcost)^(1-1.865395)

period3$meancs<-(-1)*exp(period3$beta0)/(1+period3$beta1)*sum(nta_p3$poptc)

#weekly consumer surplus
(period3$meancs/26)/0.11


#period 4 (2020, week 1-11)

nta_p4<-nta_data%>%
  filter(period==4)

period4<-nta_mean_period%>%
  filter(period==4)

period4$beta0<--0.0000681*period1$mean_newcase+0.0000133*period1$mean_income+0.0311546*period1$mean_age-0.0067061*period1$mean_precip+0.0539785*period1$mean_tmax+.2410574*period1$mean_holidayweek-0.3782999*period1$mean_Mar-0.3789562

period4$beta1<--1.922659

nta_p4$poptc<-nta_p4$nta_totalpopulation/1000*(nta_p4$nta_travelcost)^(1-1.922659)

period4$meancs<-(-1)*exp(period4$beta0)/(1+period4$beta1)*sum(nta_p4$poptc)

#weekly consumer surplus
(period4$meancs/11)/0.11

#period 5 (2020, week 12-26)

nta_p5<-nta_data%>%
  filter(period==5)

period5<-nta_mean_period%>%
  filter(period==5)

period5$beta0<--0.0001291*period2$mean_newcase+0.0000134*period2$mean_income+0.0344715*period2$mean_age+0.0338816*period2$mean_tmax+0.2759899-0.2751134-3.714945

period5$beta1<--2.014236

nta_p5$poptc<-nta_p5$nta_totalpopulation/1000*(nta_p5$nta_travelcost)^(1-2.014236)

period5$meancs<-(-1)*exp(period5$beta0)/(1+period5$beta1)*sum(nta_p5$poptc)

#weekly consumer surplus
(period5$meancs/15)/0.11

#period 6 (2020, week 27-52)

nta_p6<-nta_data%>%
  filter(period==6)

period6<-nta_mean_period%>%
  filter(period==6)

period6$beta0<-0.0002443*period3$mean_newcase+0.0000144*period3$mean_income+0.0325213*period3$mean_age-0.0275361*period3$mean_precip+0.0334077*period3$mean_tmax+.2872425*period3$mean_holidayweek-0.6140076*period3$mean_Aug-0.3777834*period3$mean_Oct+0.3760982-0.2891564-2.605221


period6$beta1<--1.865395

nta_p6$poptc<-nta_p6$nta_totalpopulation/1000*(nta_p6$nta_travelcost)^(1-1.865395)

period6$meancs<-(-1)*exp(period6$beta0)/(1+period6$beta1)*sum(nta_p6$poptc)

#weekly consumer surplus
(period6$meancs/26)/0.11

##aggregate welfare

#2019: 
(period1$meancs/11)/0.11*11+(period2$meancs/15)/0.11*15+(period3$meancs/26)/0.11*26

#2020
(period4$meancs/11)/0.11*11+(period5$meancs/15)/0.11*15+(period6$meancs/26)/0.11*26

#====================Calculate SE of CS================

##Calculate consumer surplus for each period
#period 1 (2019, week 1-11)

nta_p1<-nta_data%>%
  filter(period==1)

nta_p1$poptc<-nta_p1$nta_totalpopulation/1000*(nta_p1$nta_travelcost)^(1-0.704)

nta_p1$poptclntc<-nta_p1$nta_totalpopulation/1000*(nta_p1$nta_travelcost)^(1-0.704)*log(nta_p1$nta_travelcost)


period1<-nta_mean_period%>%
  filter(period==1)

period1$beta0<-.00001*period1$mean_newcase+0.00000606*period1$mean_income+.0154*period1$mean_age+0.477*period1$mean_highedu-0.477*period1$mean_precip+0.0096*period1$mean_tmax+.093*period1$mean_holidayweek-.042*period1$mean_Mar

period1$beta1<--0.704

period1$varbeta0<-(period1$mean_newcase)^2*3.170e-11+(period1$mean_income)^2*2.870e-13+(period1$mean_age)^2*5.566e-06+(period1$mean_highedu)^2*.03613549+(period1$mean_tmax)^2*3.818e-06+(period1$mean_holidayweek)^2*.00063692+(period1$mean_Mar)^2*.00280991+2*period1$mean_newcase*period1$mean_income*2.170e-13+2*period1$mean_newcase*period1$mean_age*(-1.647e-10)+2*period1$mean_newcase*period1$mean_highedu*4.112e-08+2*period1$mean_newcase*period1$mean_tmax*9.932e-10+2*period1$mean_newcase*period1$mean_holidayweek*(-2.302e-09)+2*period1$mean_newcase*period1$mean_Mar*(-8.554e-09)+2*period1$mean_income*period1$mean_age*(-5.783e-10)+2*period1$mean_income*period1$mean_highedu*(7.533e-08)+2*period1$mean_income*period1$mean_tmax*(-1.713e-11)+2*period1$mean_income*period1$mean_holidayweek*8.589e-11+2*period1$mean_income*period1$mean_Mar*8.292e-11+2*period1$mean_age*period1$mean_highedu*2.502e-08+2*period1$mean_age*period1$mean_tmax*(-6.245e-08)+2*period1$mean_age*period1$mean_holidayweek*(-6.245e-08)+2*period1$mean_age*period1$mean_Mar*(3.706e-07)+2*period1$mean_highedu*period1$mean_tmax*4.876e-06+2*period1$mean_holidayweek*period1$mean_highedu*(-8.032e-06)+2*period1$mean_highedu*period1$mean_Mar*(-.00008405)+2*period1$mean_tmax*period1$mean_holidayweek*(-9.930e-06)+2*period1$mean_tmax*period1$mean_Mar*(-.00002907)+2*period1$mean_holidayweek*period1$mean_Mar*.00038722

period1$varbeta1<-.00036229

period1$covbeta0beta1=-4.104e-09*period1$mean_newcase+-4.654e-09*period1$mean_income-4.576e-06*period1$mean_age-.00186608*period1$mean_highedu-3.214e-07*period1$mean_tmax+6.722e-06*period1$mean_holidayweek-1.357e-06*period1$mean_Mar


period1$meancs<-(-1)*exp(period1$beta0)/(1+period1$beta1)*sum(nta_p1$poptc)

period1$alphabeta0<-(-1)*exp(period1$beta0)/(1+period1$beta1)*sum(nta_p1$poptc)

period1$alphabeta1<-exp(period1$beta0)/(1+period1$beta1)*sum(nta_p1$poptc)-exp(period1$beta0)/(1+period1$beta1)*sum(nta_p1$poptclntc)

period1$varcs<-(period1$alphabeta0)^2*period1$varbeta0+(period1$alphabeta1)^2*period1$varbeta1+2*period1$alphabeta0*period1$alphabeta1*period1$covbeta0beta1

period1$lowercs<-period1$meancs-1.96*sqrt(period1$varcs)

period1$uppercs<-period1$meancs+1.96*sqrt(period1$varcs)


#weekly consumer surplus
(period1$meancs/11)/0.063
(period1$lowercs/11)/0.063
(period1$uppercs/11)/0.063

#period 4 (2020, week 1-11)

nta_p4<-nta_data%>%
  filter(period==4)

nta_p4$poptc<-nta_p4$nta_totalpopulation.y/1000*(nta_p4$totalcost)^(1-1.235995)

nta_p4$poptclntc<-nta_p4$nta_totalpopulation.y/1000*(nta_p4$totalcost)^(1-1.235995)*log(nta_p4$totalcost)


period4<-nta_mean_period%>%
  filter(period==4)

period4$beta0<-.00003*period4$mean_newcase+.00002*period4$mean_income+.023*period4$mean_age-2.518*period4$mean_highedu+.022*period4$mean_tmax+.059*period4$mean_holidayweek-.189*period4$mean_Mar-.1714767

period4$beta0a<--0.169+.00002*period2$mean_income+.027*period2$mean_age-2.79*period2$mean_highedu+.021*period2$mean_tmax-.094*period2$mean_holidayweek-.261*period2$mean_Mar-.240*period2$mean_Apr-.202*period2$mean_May


period4$beta1<--1.235995

period4$varbeta0<-(period4$mean_newcase)^2*3.170e-11+(period4$mean_income)^2*2.870e-13+(period4$mean_age)^2*5.566e-06+(period4$mean_highedu)^2*.03613549+(period4$mean_tmax)^2*3.818e-06+(period4$mean_holidayweek)^2*.00063692+(period4$mean_Mar)^2*.00280991+2*period4$mean_newcase*period1$mean_income*2.170e-13+2*period4$mean_newcase*period4$mean_age*(-1.647e-10)+2*period4$mean_newcase*period4$mean_highedu*4.112e-08+2*period4$mean_newcase*period4$mean_tmax*9.932e-10+2*period4$mean_newcase*period4$mean_holidayweek*(-2.302e-09)+2*period4$mean_newcase*period4$mean_Mar*(-8.554e-09)+2*period4$mean_income*period4$mean_age*(-5.783e-10)+2*period4$mean_income*period4$mean_highedu*(7.533e-08)+2*period4$mean_income*period4$mean_tmax*(-1.713e-11)+2*period4$mean_income*period4$mean_holidayweek*8.589e-11+2*period4$mean_income*period4$mean_Mar*8.292e-11+2*period4$mean_age*period4$mean_highedu*2.502e-08+2*period4$mean_age*period4$mean_tmax*(-6.245e-08)+2*period4$mean_age*period4$mean_holidayweek*(-6.245e-08)+2*period4$mean_age*period4$mean_Mar*(3.706e-07)+2*period4$mean_highedu*period4$mean_tmax*4.876e-06+2*period4$mean_holidayweek*period4$mean_highedu*(-8.032e-06)+2*period4$mean_highedu*period4$mean_Mar*(-.00008405)+2*period4$mean_tmax*period4$mean_holidayweek*(-9.930e-06)+2*period4$mean_tmax*period4$mean_Mar*(-.00002907)+2*period4$mean_holidayweek*period4$mean_Mar*.000387222+.0016712+2*period4$mean_newcase*(-4.922e-09)+2*period4$mean_income*2.769e-11+2*period4$mean_age*2.686e-07+2*period4$mean_highedu*(-.00002268)+2*period4$mean_tmax*(-.00002106)+2*period4$mean_holidayweek*(.00004821)+2*period4$mean_Mar*(.00017099)

period4$varbeta1<-.00036229

period4$covbeta0beta1=-4.104e-09*period4$mean_newcase+-4.654e-09*period4$mean_income-4.576e-06*period4$mean_age-.00186608*period4$mean_highedu-3.214e-07*period4$mean_tmax+6.722e-06*period4$mean_holidayweek-1.357e-06*period4$mean_Mar-.00002585


period4$meancs<-(-1)*exp(period4$beta0)/(1+period4$beta1)*sum(nta_p4$poptc)

period4$meancsa<-(-1)*exp(period4$beta0a)/(1+period4$beta1a)*sum(nta_p4$poptc)

period4$alphabeta0<-(-1)*exp(period4$beta0)/(1+period4$beta1)*sum(nta_p4$poptc)

period4$alphabeta1<-exp(period4$beta0)/(1+period4$beta1)*sum(nta_p4$poptc)-exp(period4$beta0)/(1+period4$beta1)*sum(nta_p4$poptclntc)

period4$varcs<-(period4$alphabeta0)^2*period4$varbeta0+(period4$alphabeta1)^2*period4$varbeta1+2*period4$alphabeta0*period4$alphabeta1*period4$covbeta0beta1

period4$lowercs<-period4$meancs-1.96*sqrt(period4$varcs)

period4$uppercs<-period4$meancs+1.96*sqrt(period4$varcs)


#weekly consumer surplus
(period4$meancs/11)/0.063
(period4$lowercs/11)/0.063
(period4$uppercs/11)/0.063

#period 2 (2019, week 12-26)

nta_p2<-nta_data%>%
  filter(period==2)

nta_p2$poptc<-nta_p2$nta_totalpopulation.y/1000*(nta_p2$totalcost)^(1-1.256778)

nta_p2$poptclntc<-nta_p2$nta_totalpopulation.y/1000*(nta_p2$totalcost)^(1-1.256778)*log(nta_p2$totalcost)


period2<-nta_mean_period%>%
  filter(period==2)

period2$beta0<-.124+.00002*period2$mean_income+.027*period2$mean_age-2.79*period2$mean_highedu+.021*period2$mean_tmax-.094*period2$mean_holidayweek-.261*period2$mean_Mar-.240*period2$mean_Apr-.202*period2$mean_May

period2$beta1<--1.256778

period2$varbeta0<-.00463388+(period2$mean_income)^2*5.418e-13+(period2$mean_age)^2*.0000103+(period2$mean_highedu)^2*.06803738
+(period2$mean_tmax)^2*9.943e-06+(period2$mean_holidayweek)^2*.00194363+(period2$mean_Mar)^2*.00375741+(period2$mean_Apr)^2*.00754973+(period2$mean_May)^2*.01000759+2*(-2.630e-10)+2*(1.159e-06)+2*(.00013859)+2*(-.00009)+2*.00065486+2*(-.00043929)+2*( -.00136281)+2*(-.00094029)+2*period2$mean_income*period2$mean_age*( -1.064e-09)+2*period2$mean_income*period2$mean_highedu*(1.423e-07)
+2*period2$mean_income*period2$mean_tmax*(-1.750e-11)+2*period2$mean_income*period2$mean_holidayweek*1.576e-10+2*period2$mean_income*period2$mean_Mar*3.567e-12+2*period2$mean_income*period2$mean_Apr*(-5.078e-10)+2*period2$mean_income*period2$mean_May*(1.173e-09)+
2*period2$mean_age*period2$mean_highedu*(-.00017164)+2*period2$mean_age*period2$mean_tmax*(-1.275e-07)+2*period2$mean_age*period2$mean_holidayweek*2.201e-06+2*period2$mean_age*period2$mean_Mar* 3.067e-06+2*period2$mean_age*period2$mean_Apr*3.981e-06+2*period2$mean_age*period2$mean_May*3.520e-06+2*period2$mean_highedu*period2$mean_tmax*7.196e-06+2*period2$mean_holidayweek*period2$mean_highedu*(-.0000602)+2*period2$mean_highedu*period2$mean_Mar*(-.00018757)+2*period2$mean_highedu*period2$mean_Apr*(-.00030664)+2*period2$mean_highedu*(-.00006007)+2*period2$mean_tmax*period2$mean_holidayweek*(-.0000137)+2*period2$mean_tmax*period2$mean_Mar*(-.00009723)+2*period2$mean_tmax*period2$mean_Apr*(-.00015344)+2*period2$mean_tmax*period2$mean_May*(-.00022524)+2*period2$mean_holidayweek*period2$mean_Mar*.00141514+2*period2$mean_holidayweek*period2$mean_Apr*.00176544+2*period2$mean_holidayweek*period2$mean_May*.00204437+2*period2$mean_Mar*period2$mean_Apr*.00427554+2*period2$mean_Mar*period2$mean_May*.00486336+2*period2$mean_Apr*period2$mean_May*.00743439

period2$varbeta1<-.0006636

period2$covbeta0beta1<-.00004169-8.875e-09*period2$mean_income-8.091e-06*period2$mean_age-.00340723*period2$mean_highedu-1.392e-06*period2$mean_tmax+.00001495*period2$mean_holidayweek+3.468e-06*period2$mean_Mar+.00001945*period2$mean_Apr+.00002103*period2$mean_May


period2$meancs<-(-1)*exp(period2$beta0)/(1+period2$beta1)*sum(nta_p2$poptc)

period2$alphabeta0<-(-1)*exp(period2$beta0)/(1+period2$beta1)*sum(nta_p2$poptc)

period2$alphabeta1<-exp(period2$beta0)/(1+period2$beta1)*sum(nta_p2$poptc)-exp(period2$beta0)/(1+period2$beta1)*sum(nta_p2$poptclntc)

period2$varcs<-(period2$alphabeta0)^2*period2$varbeta0+(period2$alphabeta1)^2*period2$varbeta1+2*period2$alphabeta0*period2$alphabeta1*period2$covbeta0beta1

period2$lowercs<-period2$meancs-1.96*sqrt(period2$varcs)

period2$uppercs<-period2$meancs+1.96*sqrt(period2$varcs)


#weekly consumer surplus
(period2$meancs/15)/0.063
(period2$lowercs/15)/0.063
(period2$uppercs/15)/0.063

#period 5 (2020, week 12-26)

nta_p5<-nta_data%>%
  filter(period==5)

nta_p5$poptc<-nta_p5$nta_totalpopulation.y/1000*(nta_p5$totalcost)^(1-1.256778)

nta_p5$poptclntc<-nta_p5$nta_totalpopulation.y/1000*(nta_p5$totalcost)^(1-1.256778)*log(nta_p5$totalcost)


period5<-nta_mean_period%>%
  filter(period==5)

period5$beta0<-.124-.169-1.51+.00002*period5$mean_income+.027*period5$mean_age-2.79*period5$mean_highedu+.021*period5$mean_tmax-.094*period5$mean_holidayweek-.261*period5$mean_Mar-.240*period5$mean_Apr-.202*period5$mean_May

period5$beta1<--1.256778

period5$varbeta0<-.00463388+(period5$mean_income)^2*5.418e-13+(period5$mean_age)^2*.0000103+(period5$mean_highedu)^2*.06803738
+(period5$mean_tmax)^2*9.943e-06+(period5$mean_holidayweek)^2*.00194363+(period5$mean_Mar)^2*.00375741+(period5$mean_Apr)^2*.00754973+(period5$mean_May)^2*.01000759+2*(-2.630e-10)+2*(1.159e-06)+2*(.00013859)+2*(-.00009)+2*.00065486+2*(-.00043929)+2*( -.00136281)+2*(-.00094029)+2*period5$mean_income*period5$mean_age*( -1.064e-09)+2*period5$mean_income*period5$mean_highedu*(1.423e-07)
+2*period5$mean_income*period5$mean_tmax*(-1.750e-11)+2*period5$mean_income*period5$mean_holidayweek*1.576e-10+2*period5$mean_income*period5$mean_Mar*3.567e-12+2*period5$mean_income*period5$mean_Apr*(-5.078e-10)+2*period5$mean_income*period5$mean_May*(1.173e-09)+
  2*period5$mean_age*period5$mean_highedu*(-.00017164)+2*period5$mean_age*period5$mean_tmax*(-1.275e-07)+2*period5$mean_age*period5$mean_holidayweek*2.201e-06+2*period5$mean_age*period5$mean_Mar* 3.067e-06+2*period5$mean_age*period5$mean_Apr*3.981e-06+2*period5$mean_age*period5$mean_May*3.520e-06+2*period5$mean_highedu*period5$mean_tmax*7.196e-06+2*period5$mean_holidayweek*period5$mean_highedu*(-.0000602)+2*period5$mean_highedu*period5$mean_Mar*(-.00018757)+2*period5$mean_highedu*period5$mean_Apr*(-.00030664)+2*period5$mean_highedu*(-.00006007)+2*period5$mean_tmax*period5$mean_holidayweek*(-.0000137)+2*period5$mean_tmax*period5$mean_Mar*(-.00009723)+2*period5$mean_tmax*period5$mean_Apr*(-.00015344)+2*period5$mean_tmax*period5$mean_May*(-.00022524)+2*period5$mean_holidayweek*period5$mean_Mar*.00141514+2*period5$mean_holidayweek*period5$mean_Apr*.00176544+2*period5$mean_holidayweek*period5$mean_May*.00204437+2*period5$mean_Mar*period5$mean_Apr*.00427554+2*period5$mean_Mar*period5$mean_May*.00486336+2*period5$mean_Apr*period5$mean_May*.00743439+.00188637+.00338709+2*(9.611e-12*period5$mean_income+1.494e-06*period5$mean_age-.00001737*period5$mean_highedu-.00005469*period5$mean_tmax+.00037031*period5$mean_holidayweek+.00053689*period5$mean_Mar+.0008947*period5$mean_Apr+.00129291*period5$mean_May)+2*(4.061e-10*period5$mean_income-2.134e-06*period5$mean_age -.00028152*period5$mean_highedu+.00006655*period5$mean_tmax-.00051049*period5$mean_holidayweek-.00070862*period5$mean_Mar-.00081215*period5$mean_Apr-.00183569*period5$mean_May)

period5$varbeta1<-.0006636

period5$covbeta0beta1<-.00004169-8.875e-09*period5$mean_income-8.091e-06*period5$mean_age-.00340723*period5$mean_highedu-1.392e-06*period5$mean_tmax+.00001495*period5$mean_holidayweek+3.468e-06*period5$mean_Mar+.00001945*period5$mean_Apr+.00002103*period5$mean_May+.00002474-.00009346


period5$meancs<-(-1)*exp(period5$beta0)/(1+period5$beta1)*sum(nta_p5$poptc)

period5$alphabeta0<-(-1)*exp(period5$beta0)/(1+period5$beta1)*sum(nta_p5$poptc)

period5$alphabeta1<-exp(period5$beta0)/(1+period5$beta1)*sum(nta_p5$poptc)-exp(period5$beta0)/(1+period5$beta1)*sum(nta_p5$poptclntc)

period5$varcs<-(period5$alphabeta0)^2*period5$varbeta0+(period5$alphabeta1)^2*period5$varbeta1+2*period5$alphabeta0*period5$alphabeta1*period5$covbeta0beta1

period5$lowercs<-period5$meancs-1.96*sqrt(period5$varcs)

period5$uppercs<-period5$meancs+1.96*sqrt(period5$varcs)


#weekly consumer surplus
(period5$meancs/15)/0.063
(period5$lowercs/15)/0.063
(period5$uppercs/15)/0.063


#period 3 (2019, week 27-52)

nta_p3<-nta_data%>%
  filter(period==3)

nta_p3$poptc<-nta_p3$nta_totalpopulation.y/1000*(nta_p3$totalcost)^(1-1.246398)

nta_p3$poptclntc<-nta_p3$nta_totalpopulation.y/1000*(nta_p3$totalcost)^(1-1.246398)*log(nta_p3$totalcost)


period3<-nta_mean_period%>%
  filter(period==3)

period3$beta0<-.0001369*period3$mean_newcase+.00002*period3$mean_income+.020*period3$mean_age-2.407*period3$mean_highedu+.015*period3$mean_tmax+.073*period3$mean_holidayweek+.3997113

period3$beta1<--1.246398

period3$varbeta0<-.01894596+(period3$mean_newcase)^2*1.331e-10+(period3$mean_income)^2*4.100e-13+(period3$mean_age)^2*7.997e-06+(period3$mean_highedu)^2*.0524869+(period3$mean_tmax)^2*5.160e-06+(period3$mean_holidayweek)^2*.00072058+2*(4.422e-09*period3$mean_newcase-1.692e-09*period3$mean_income-.00020233*period3$mean_age-.00671697*period3$mean_highedu-.00020417*period3$mean_tmax+4.136e-06*period3$mean_holidayweek)+2*period3$mean_newcase*(period3$mean_income*3.363e-13-period3$mean_age*3.084e-10+period3$mean_highedu*5.481e-08-period3$mean_tmax*2.924e-10-period3$mean_holidayweek*6.216e-09)+2*period3$mean_income*(-8.109e-10*period3$mean_age+period3$mean_highedu*1.101e-07-period3$mean_tmax*3.282e-11+period3$mean_holidayweek*5.979e-11)+2*period3$mean_age*(-period3$mean_highedu*.0001278+period3$mean_tmax*6.495e-08-period3$mean_holidayweek*1.275e-08)+2*period3$mean_highedu*(3.585e-06*period3$mean_tmax-period3$mean_holidayweek*.00001846)-2*period3$mean_tmax*6.760e-06*period3$mean_holidayweek
  
  

period3$varbeta1<-.0005251

period3$covbeta0beta1<--.00006814+(-1.031e-08)*period3$mean_newcase+(-6.760e-09)*period3$mean_income+(-6.402e-06)*period3$mean_age+(-.0027158)*period3$mean_highedu+(-3.170e-07)*period3$mean_tmax+.00001202*period3$mean_holidayweek


period3$meancs<-(-1)*exp(period3$beta0)/(1+period3$beta1)*sum(nta_p3$poptc)

period3$alphabeta0<-(-1)*exp(period3$beta0)/(1+period3$beta1)*sum(nta_p3$poptc)

period3$alphabeta1<-exp(period3$beta0)/(1+period3$beta1)*sum(nta_p3$poptc)-exp(period3$beta0)/(1+period3$beta1)*sum(nta_p3$poptclntc)

period3$varcs<-(period3$alphabeta0)^2*period3$varbeta0+(period3$alphabeta1)^2*period3$varbeta1+2*period3$alphabeta0*period3$alphabeta1*period3$covbeta0beta1

period3$lowercs<-period3$meancs-1.96*sqrt(period3$varcs)

period3$uppercs<-period3$meancs+1.96*sqrt(period3$varcs)


#weekly consumer surplus
(period3$meancs/26)/0.063
(period3$lowercs/26)/0.063
(period3$uppercs/26)/0.063

#period 6 (2020, week 27-52)

nta_p6<-nta_data%>%
  filter(period==6)

nta_p6$poptc<-nta_p6$nta_totalpopulation.y/1000*(nta_p6$totalcost)^(1-1.246398)

nta_p6$poptclntc<-nta_p6$nta_totalpopulation.y/1000*(nta_p6$totalcost)^(1-1.246398)*log(nta_p6$totalcost)


period6<-nta_mean_period%>%
  filter(period==6)

period6$beta0<-.0001369*period6$mean_newcase+.00002*period6$mean_income+.020*period6$mean_age-2.407*period6$mean_highedu+.015*period6$mean_tmax+.073*period6$mean_holidayweek+.3997113-.1432051-1.154952

period6$beta1<--1.246398

period6$varbeta0<-.01894596+(period6$mean_newcase)^2*1.331e-10+(period6$mean_income)^2*4.100e-13+(period6$mean_age)^2*7.997e-06+(period6$mean_highedu)^2*.0524869+(period6$mean_tmax)^2*5.160e-06+(period6$mean_holidayweek)^2*.00072058+2*(4.422e-09*period6$mean_newcase-1.692e-09*period6$mean_income-.00020233*period6$mean_age-.00671697*period6$mean_highedu-.00020417*period6$mean_tmax+4.136e-06*period6$mean_holidayweek)+2*period6$mean_newcase*(period6$mean_income*3.363e-13-period6$mean_age*3.084e-10+period6$mean_highedu*5.481e-08-period6$mean_tmax*2.924e-10-period6$mean_holidayweek*6.216e-09)+2*period6$mean_income*(-8.109e-10*period6$mean_age+period6$mean_highedu*1.101e-07-period6$mean_tmax*3.282e-11+period6$mean_holidayweek*5.979e-11)+2*period6$mean_age*(-period6$mean_highedu*.0001278+period6$mean_tmax*6.495e-08-period6$mean_holidayweek*1.275e-08)+2*period6$mean_highedu*(3.585e-06*period6$mean_tmax-period6$mean_holidayweek*.00001846)-2*period6$mean_tmax*6.760e-06*period6$mean_holidayweek+.00171688+.00268636+2*(-.00169899+7.125e-09+1.076e-10+2.340e-08-.00001599-.00002833+.00003716)+2*(-2.341e-07-2.381e-10-2.641e-07-.00015168+.00002273-.00002302)



period6$varbeta1<-.0005251

period6$covbeta0beta1<--.00006814+(-1.031e-08)*period6$mean_newcase+(-6.760e-09)*period6$mean_income+(-6.402e-06)*period6$mean_age+(-.0027158)*period6$mean_highedu+(-3.170e-07)*period6$mean_tmax+.00001202*period6$mean_holidayweek+.00001468-.00002229


period6$meancs<-(-1)*exp(period6$beta0)/(1+period6$beta1)*sum(nta_p6$poptc)

period6$alphabeta0<-(-1)*exp(period6$beta0)/(1+period6$beta1)*sum(nta_p6$poptc)

period6$alphabeta1<-exp(period6$beta0)/(1+period6$beta1)*sum(nta_p6$poptc)-exp(period6$beta0)/(1+period6$beta1)*sum(nta_p6$poptclntc)

period6$varcs<-(period6$alphabeta0)^2*period6$varbeta0+(period6$alphabeta1)^2*period6$varbeta1+2*period6$alphabeta0*period6$alphabeta1*period6$covbeta0beta1

period6$lowercs<-period6$meancs-1.96*sqrt(period6$varcs)

period6$uppercs<-period6$meancs+1.96*sqrt(period6$varcs)


#weekly consumer surplus
(period6$meancs/26)/0.063
(period6$lowercs/26)/0.063
(period6$uppercs/26)/0.063


