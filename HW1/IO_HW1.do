
* UPenn - Fall 2022 - Industrial Organization
* Homework 1
* September 6th, 2022

clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1"

* Reading Data
use "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1/GMdata.dta"
gen d357 = (sic3==357)
sort index
by index: gen cnt = (_N)
sort yr
by yr: gen firm = (_N) 

preserve
************************  	Question 1    ******************************

* All Data
tabstat ldsal lemp ldnpt ldrst ldrnd ldinv, statistics( mean sd min p25 median p75 max n)  format(%7.4g)
twoway (histogram yr, discrete xlabel(73 78 83 88) frequency ) (histogram yr if d357==1, discrete xlabel(73 78 83 88) frequency color(green)),legend(order(1 "All Firms" 2 "sic3=357" ))
graph export q1_all.png, width(600) height(450) replace
* At least 2 Observation
restore
preserve
keep if cnt>=2
sort yr
by yr: replace firm = (_N)
tabstat ldsal lemp ldnpt ldrst ldrnd ldinv, statistics( mean sd min p25 median p75 max n)  format(%7.4g)
twoway (histogram yr, discrete xlabel(73 78 83 88) frequency ) (histogram yr if d357==1, discrete xlabel(73 78 83 88) frequency color(green)),legend(order(1 "All Firms" 2 "sic3=357" ))
graph export q1_2.png, width(600) height(450) replace

* Balanced Panel
restore
keep if cnt==4
sort yr
by yr: replace firm = (_N)
tabstat ldsal lemp ldnpt ldrst ldrnd ldinv, statistics( mean sd min p25 median p75 max n)  format(%7.4g)
twoway (histogram yr, discrete xlabel(73 78 83 88) frequency ) (histogram yr if d357==1, discrete xlabel(73 78 83 88) frequency color(green)),legend(order(1 "All Firms" 2 "sic3=357" ))
graph export q1_balanced.png, width(600) height(450) replace


************************  	Question 2    ******************************
clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1"

* Reading Data
use "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1/GMdata.dta"
sort index
by index: gen cnt = (_N)

keep if cnt==4
xtset index yr, delta(5)


* Sector and Time Dummy
gen d357_73 = (sic3==357) & (yr==73)
gen d357_78 = (sic3==357) & (yr==78)
gen d357_83 = (sic3==357) & (yr==83)
gen d357_88 = (sic3==357) & (yr==88)

eststo clear
* Regression: Total (with Fixed Effect)
reg ldsal lemp ldnpt ldrst i.yr d357_73 d357_78 d357_83 d357_88
eststo total, title("Total")

* Regression: between
xtreg ldsal lemp ldnpt ldrst i.yr d357_73 d357_78 d357_83 d357_88, be
eststo between, title("Between")

* Regression: within
xtreg ldsal lemp ldnpt ldrst i.yr d357_73 d357_78 d357_83 d357_88, fe
eststo within, title("Within")



* Regression: Random
xtreg ldsal lemp ldnpt ldrst i.yr d357_73 d357_78 d357_83 d357_88, re
eststo random, title("Random")



esttab total between within random using Q2Reg.tex, replace mtitle

esttab total between within random, mtitle


* Hausman Test

hausman within random, alleqs 


************************  	Question 3    ******************************
clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1"

* Reading Data
use "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1/GMdata.dta"
sort index
by index: gen cnt = (_N)

keep if cnt==4
xtset index yr, delta(5)

* Sector and Time Dummy
gen d357_73 = (sic3==357) & (yr==73)
gen d357_78 = (sic3==357) & (yr==78)
gen d357_83 = (sic3==357) & (yr==83)
gen d357_88 = (sic3==357) & (yr==88)
gen d73 = (yr==73)
gen d78 = (yr==78)
gen d83 = (yr==83)
gen d88 = (yr==88)

eststo clear
* Regression: First Difference
reg D.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88)
eststo first, title("1st diff")

* Regression: Second Differenc
reg D2.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88)
eststo second, title("2nd diff")

* Regression: Third Difference
reg D3.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88)
eststo third, title("3rd diff")

esttab first second third using Q3RegResult.tex, replace mtitle rename(D.lemp lemp D2.lemp lemp D3.lemp lemp D.ldnpt ldnpt D2.ldnpt ldnpt D3.ldnpt ldnpt D.ldrst ldrst D2.ldrst ldrst D3.ldrst ldrst oD.d73 d73  oD2.d73 d73 oD3.d73 d73 D.d78 d78 D2.d78 d78 oD3.d78 d78 D.d83 d83 oD2.d83 d83 oD3.d83 d83 oD.d88 d88 oD2.d88 d88 oD3.d88 d88 oD.d357_73 d357_73 oD2.d357_73 d357_73 oD3.d357_73 d357_73 D.d357_78 d357_78 D2.d357_78 d357_78 D3.d357_78 d357_78 D.d357_83 d357_83 D2.d357_83 d357_83 oD3.d357_83 d357_83 D.d357_88 d357_88 oD2.d357_88 d357_88 oD3.d357_88 d357_88)



************************  	Question 4    ******************************
clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1"

* Reading Data
use "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW1/GMdata.dta"
sort index
by index: gen cnt = (_N)

* Sector and Time Dummy
gen d357_73 = (sic3==357) & (yr==73)
gen d357_78 = (sic3==357) & (yr==78)
gen d357_83 = (sic3==357) & (yr==83)
gen d357_88 = (sic3==357) & (yr==88)
gen d73 = (yr==73)
gen d78 = (yr==78)
gen d83 = (yr==83)
gen d88 = (yr==88)
preserve

xtset index yr, delta(5)


eststo clear
* Regression: Total
reg ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88
eststo total, title("Total")

* Regression: First Difference
reg D.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88)
eststo first, title("1st diff")


esttab total first using Q4RegResult1.tex, replace mtitle rename(D.lemp lemp D.ldnpt ldnpt D.ldrst ldrst oD.d73 d73 D.d78 d78 D.d83 d83 oD.d88 d88 oD.d357_73 d357_73 D.d357_78 d357_78 D.d357_83 d357_83 D.d357_88 d357_88)

keep if cnt>=2
* Regression: 2 or more
reg ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88
eststo T_total, title("Total")

* Regression: First Difference
reg D.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88)
eststo T_first, title("1st diff")


esttab T_total T_first using Q4RegResult2.tex, replace mtitle  rename(D.lemp lemp D.ldnpt ldnpt D.ldrst ldrst oD.d73 d73 D.d78 d78 D.d83 d83 oD.d88 d88 oD.d357_73 d357_73 D.d357_78 d357_78 D.d357_83 d357_83 D.d357_88 d357_88)


** Doing Probit
restore


xtset index yr, delta(5)

sort index yr
by index: gen next_period = yr[_n+1]
replace next_period = 0 if missing(next_period[_n])
replace next_period = 1 if next_period[_n]>0
replace next_period = . if yr[_n]==88


probit next_period ldnpt ldrst ldinv
eststo probit, title("probit")
esttab probit using Q4RegResult3.tex, replace mtitle 

predict phat,xb
gen mills = exp(-.5*phat^2)/(sqrt(2*_pi)*normprob(phat))
gen inv_mills = 1/mills


keep if cnt>=2
* Regression: 2 or more
reg ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88 inv_mills
eststo T_total, title("Total")

* Regression: First Difference
reg D.(ldsal lemp ldnpt ldrst d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88 inv_mills)
eststo T_first, title("1st diff")


esttab T_total T_first using Q4RegResult4.tex, replace mtitle rename(D.lemp lemp D.ldnpt ldnpt D.ldrst ldrst oD.d73 d73 D.d78 d78 D.d83 d83 oD.d88 d88 oD.d357_73 d357_73 D.d357_78 d357_78 D.d357_83 d357_83 D.d357_88 d357_88 D.inv_mills inv_mills)




