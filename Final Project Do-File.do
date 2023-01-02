//STAT613
//Final Project
//Ryan Bresnahan

*data
clear 
use "C:\Users\bresn\Downloads\gss2021.dta"

//////////////////////////////////////////////////////////////////////////////
//ANALYSIS 1: REALINC ANOVA WITH INTERACTION OF RACE AND EDUCATION
//STEP 1
*combining ethnicities, other is not included in race variable
generate race_asian=raceacs4+raceacs5+raceacs6+raceacs7+raceacs8+raceacs9+raceacs10
replace race_asian=1 if race_asian==2
tab race_asian
generate race_pacific=raceacs11+raceacs12+raceacs13+raceacs14
replace race_pacific=1 if race_pacific==2
tab race_pacific

rename raceacs1 race_white
rename raceacs2 race_black
rename raceacs16 race_hisp
rename raceacs15 race_other

generate race=.
replace race=1 if race_white==1
replace race=2 if race_black==1
replace race=3 if race_asian==1
replace race=4 if race_hisp==1


label define race 1 "white" 2 "black" 3 "asian" 4 "hispanic"
label values race race
tab race

*hist realinc
*hist numrooms
//qnorm realinc
//qnorm numrooms
//asdoc swilk realinc
//asdoc swilk numrooms

replace realinc = log(realinc)
replace numrooms = log(numrooms+1)
//remember to show the distributions of transformed variables

//levene's test, collinearity check, then manova
*asdoc robvar numrooms, by(race)
*asdoc robvar numrooms, by(degree)
*asdoc robvar numrooms, by(sex)
*asdoc robvar realinc, by(race)
*asdoc robvar realinc, by(degree)
*asdoc robvar realinc, by(sex)

*correlate numrooms realinc race degree sex

replace realinc=round(realinc,1)

label define degree 0 "<High School" 1 "High School" 2 "Associate" 3 "Bachelor" 4 "Graduate"
label values degree degree

label define sex 1 "Male" 2 "Female"
label values sex sex

label define incom16 1 "Far below average" 2 "Below average" 3 "Average" 4 "Above average" 5 "Far above average"
label values incom16 incom16

_manova numrooms realinc= race degree sex


//Getting around to this, make sure to show the actual income differences w/out transformed variables (only the values). This might be best put into an appendix, as the results are not as good. On second thought, the values are messed up because of methodology (dollars are same value as when survey first made). Note that log is better. Also include uncorrected and corrected p-values. Corrected is needed because of levene's test.
anova realinc race degree sex race|degree|sex
contrast g.race@degree, pveffects
contrast g.race|degree, pveffects mcompare(bonferroni)
contrast g.race|degree|sex, pveffects
contrast g.race|degree|sex, pveffects mcompare(bonferroni)
pwcompare race, asbalanced emptycells(reweight) effects



///////////////////////////////////////////////////////////////////////////////
//ANALYSIS 2: Multivariate regression

clear 
use "C:\Users\bresn\Downloads\gss2021.dta"

generate race_asian=raceacs4+raceacs5+raceacs6+raceacs7+raceacs8+raceacs9+raceacs10
replace race_asian=1 if race_asian==2
generate race_pacific=raceacs11+raceacs12+raceacs13+raceacs14
replace race_pacific=1 if race_pacific==2

rename raceacs1 race_white
rename raceacs2 race_black
rename raceacs16 race_hisp
rename raceacs15 race_other

generate race=.
replace race=1 if race_white==1
replace race=2 if race_black==1
replace race=3 if race_asian==1
replace race=4 if race_hisp==1


label define race 1 "white" 2 "black" 3 "asian" 4 "hispanic"
label values race race

label define degree 0 "<High School" 1 "High School" 2 "Associate" 3 "Bachelor" 4 "Graduate"
label values degree degree

label define sex 1 "Male" 2 "Female"
label values sex sex

label define incom16 1 "Far below average" 2 "Below average" 3 "Average" 4 "Above average" 5 "Far above average"
label values incom16 incom16

replace realinc = log(realinc)
replace numrooms = log(numrooms+1)


//mvreg includes corr which reports correlations between residuals and breusch-pagan tests. Note that other transformations, such as inverse sqrt did not solve this issue.

mvreg realinc numrooms = born i.race incom16 sex, corr

estat durbinalt

margins race

//model is homoskedastic

asdoc by race, sort : regress realinc incom16, replace
asdoc by race, sort : regress numrooms incom16, replace

twoway (lfitci realinc incom16), by(, title(Predicted Income by Age 16 Family Income)) scheme(s2color) by(race)

twoway (lfitci numrooms incom16), by(, title(Predicted # Rooms by Age 16 Family Income)) scheme(s2color) by(race)

reg realinc born i.race incom16 sex

asdoc estat vif

reg realinc born i.race incom16 sex

asdoc estat vif


//hisp#4 omitted for collinearity, and hisp#5 omitted because of low sample size

*creating categorical variable of race