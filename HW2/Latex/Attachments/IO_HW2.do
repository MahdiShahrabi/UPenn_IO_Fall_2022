
* UPenn - Fall 2022 - Industrial Organization
* Homework 2
* September 22th, 2022

clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW2"

* Reading Data
use "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW2/GMdata.dta"
gen d357 = (sic3==357)
sort index
by index: gen cnt = (_N) 

xtset index yr, delta(5)


*** Modifying Data
* Sector and Time Dummy
gen d357_73 = (sic3==357) & (yr==73)
gen d357_78 = (sic3==357) & (yr==78)
gen d357_83 = (sic3==357) & (yr==83)
gen d357_88 = (sic3==357) & (yr==88)
gen d73 = (yr==73)
gen d78 = (yr==78)
gen d83 = (yr==83)
gen d88 = (yr==88)

* Making Lagged Values for using as IV
sort index yr
gen l1_lemp = L1.lemp
gen l2_lemp = L2.lemp
gen l3_lemp = L3.lemp

gen l1_ldnpt = L1.ldnpt
gen l2_ldnpt = L2.ldnpt
gen l3_ldnpt = L3.ldnpt

gen l1_ldrst = L1.ldrst
gen l2_ldrst = L2.ldrst
gen l3_ldrst = L3.ldrst

gen l1_ldinv = L1.ldinv

* Making Diff Values
gen d1_ldsal = D1.ldsal
gen d1_lemp = D1.lemp
gen d1_ldnpt = D1.ldnpt
gen d1_ldrst = D1.ldrst

************************  	Question 1    ******************************

* Making panel balances
preserve
keep if cnt==4
sort index yr

****    (a)    ****

* Regressions with 2 lagged variables as IV
ivregress gmm  D1.ldsal d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88 (D1.(lemp ldnpt ldrst) = l2_lemp l2_ldrst l2_ldnpt), first
eststo IV_2L, title("IV Regressions with 2L")

* Regressions with 3 lagged variables as IV
ivregress gmm  D1.ldsal d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88 (D1.(lemp ldnpt ldrst) = l2_lemp l2_ldrst l2_ldnpt l3_lemp l3_ldrst l3_ldnpt), first
eststo IV_3L, title("IV Regressions with 3L")

esttab IV_2L IV_3L, replace mtitle 


************************  	Question 2    ******************************
restore


****    (a)    ****
****    (i)

reg ldsal lemp d73 d78 d83 d88 d357_73 d357_78 d357_83 d357_88 c.(ldnpt ldrst ldinv)##c.(ldnpt ldrst ldinv)
eststo q2_a_i, title("lemp and dummies")
esttab q2_a_i, mtitle

****    (ii)
matrix b = e(b)
gen pi_hat = (_b[ldnpt]*ldnpt) + (_b[ldrst]*ldrst) + (_b[ldinv]*ldinv) + (_b[ldnpt#ldnpt]*ldnpt*ldnpt) + (_b[ldnpt#ldrst]*ldnpt*ldrst) + (_b[ldnpt#ldinv]*ldnpt*ldinv) + (_b[ldrst#ldrst]*ldrst*ldrst) + (_b[ldrst#ldinv]*ldinv*ldrst) + (_b[ldinv#ldinv]*ldinv*ldinv) + (_b[_cons])

gen l1_pi_hat = L1.pi_hat
preserve
* Dropping values which are missing

drop if missing(l1_ldnpt)

* NLLS
nl (pi_hat={beta2}*ldnpt + {beta3}*ldrst + {b1}*(l1_pi_hat-{beta2}*l1_ldnpt-{beta3}*l1_ldrst) + {b2}*(l1_pi_hat-{beta2}*l1_ldnpt-{beta3}*l1_ldrst)^2 )
eststo q2_a_ii, title("h^")


****    (b)    ****
restore
sort index yr
by index: gen next_period = yr[_n+1]
replace next_period = 0 if missing(next_period[_n])
replace next_period = 1 if next_period[_n]>0
replace next_period = . if yr[_n]==88


* Probit
probit next_period l1_ldnpt l1_ldrst l1_ldinv
predict P_hat
gen l1_P_hat = L1.P_hat
preserve


* NLLS: P_hat
drop if missing(l1_P_hat)
nl (pi_hat={beta2}*ldnpt + {beta3}*ldrst + {b1}*(l1_P_hat) + {b2}*(l1_P_hat^2))
eststo q2_b, title("P^")

****    (c)    ****


nl (pi_hat={beta2}*ldnpt + {beta3}*ldrst + {b1}*(l1_P_hat) + {b2}*(l1_P_hat^2)+{b3}*(l1_pi_hat-{beta2}*l1_ldnpt-{beta3}*l1_ldrst) + {b4}*(l1_pi_hat-{beta2}*l1_ldnpt-{beta3}*l1_ldrst)^2)

eststo q2_c, title("h^ & P^")

esttab q2_a_ii q2_b q2_c, mtitle


