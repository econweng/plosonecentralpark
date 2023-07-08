*--------------------------------------------------
* Pandemic and Recreation: zonal analysis of central park visitors
* 7/15/2022
* Weizhe Weng
*--------------------------------------------------


import delimited "/Users/wweng/Dropbox/Research/Pandemic and Recreation/Submission/PLOS ONE/R&R/Code/centralpark_nta.csv",clear

gen period=1 if week<12&year==2019
replace period=2 if week>11&week<25&year==2019
replace period=3 if week>24 &year==2019
replace period=4 if week<12&year==2020
replace period=5 if week>11&week<25&year==2020
replace period=6 if week>24 &year==2020


by period, sort: summarize visitrateper1000
by period, sort: summarize nta_travelcost
by period, sort: summarize case_new
by period, sort: summarize nta_median_household_income
by period, sort: summarize nta_medianage
by period, sort: summarize nta_pct_hs_above
by period, sort: summarize mean_precip
by period, sort: summarize mean_tmax
by period, sort: summarize holidayweek



gen lntc=log(nta_travelcost)
gen lnvisit=log(visitrateper1000+0.0001)


gen dpandemicweek=1 if (week>11)
replace dpandemicweek=0 if missing(dpandemicweek)

gen treatyear=1 if (year==2020)
replace treatyear=0 if missing(treatyear)

gen dpandemicweektreat=dpandemicweek*treatyear

gen month=1 if week<5&week>0
replace month=2 if week<9&week>4
replace month=3 if week<14&week>8
replace month=4 if week<18&week>13
replace month=5 if week<23&week>17
replace month=6 if week<27&week>22
replace month=7 if week<31&week>26
replace month=8 if week<36&week>30
replace month=9 if week<40&week>35
replace month=10 if week<45&week>39
replace month=11 if week<49&week>44
replace month=12 if week<53&week>48

gen pandemicpolicy=1 if week>11&week<25&year==2020
replace pandemicpolicy=0 if missing(pandemicpolicy)


gen pandemicpolicylntc=pandemicpolicy*lntc
gen pandemiclntc=dpandemicweek*lntc

gen pandemicnonpolicy=1 if week>24&year==2020
replace pandemicnonpolicy=0 if missing(pandemicnonpolicy)

gen pandemicnonpolicylntc=pandemicnonpolicy*lntc


*test pre-trend
diff (lnvisit) (dpandemicweektreat), group(treatyear) time(week) aggregate(dlang)
estat ptremds


*new estimation
reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)],r 
est store moden1
estat vce

reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat pandemicpolicy case_new  nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)],r 
est store moden2
estat vce

reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat case_new pandemicpolicy pandemicpolicylntc pandemicnonpolicylntc nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)],r 
est store moden3
estat vce


*test coefficients

reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)]
est store moden1
*estat vce

reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat pandemicpolicy case_new  nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)]
est store moden2
*estat vce

reg lnvisit lntc dpandemicweek treatyear dpandemicweektreat case_new pandemicpolicy pandemicpolicylntc pandemicnonpolicylntc nta_median_household_income nta_medianage nta_pct_hs_above mean_precip mean_tmax holidayweek i.month [aweight=sqrt(nta_totalpopulation)]
est store moden3
*estat vce

suest moden1 moden2 moden3


test _b[moden1_mean:lntc]=_b[moden2_mean:lntc]
test _b[moden1_mean:lntc]=_b[moden3_mean:lntc]
test _b[moden2_mean:lntc]=_b[moden3_mean:lntc]

test _b[moden1_mean:dpandemicweek]=_b[moden2_mean:dpandemicweek]
test _b[moden1_mean:dpandemicweek]=_b[moden3_mean:dpandemicweek]
test _b[moden2_mean:dpandemicweek]=_b[moden3_mean:dpandemicweek]

test _b[moden1_mean:treatyear]=_b[moden2_mean:treatyear]
test _b[moden1_mean:treatyear]=_b[moden3_mean:treatyear]
test _b[moden2_mean:treatyear]=_b[moden3_mean:treatyear]

test _b[moden1_mean:dpandemicweektreat]=_b[moden2_mean:dpandemicweektreat]
test _b[moden1_mean:dpandemicweektreat]=_b[moden3_mean:dpandemicweektreat]
test _b[moden2_mean:dpandemicweektreat]=_b[moden3_mean:dpandemicweektreat]




