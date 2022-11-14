* HW 3 - Industrial Organization
* Mahdi Shahrabi & Anya Shchetkina
* Octobor, 2022


**# Prepration
clear all
cls
cd "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW3/Codes"

**# Reading Data
import delimited "/Users/mahdishahrabi/Library/Mobile Documents/com~apple~CloudDocs/PhD/Year 2 - 2022/Term 3/IO/UPenn_IO_Fall_2022/HW3/Codes/Data/PS3_data.csv", clear 
 
 **# Question 1
sum market_share price calories organic
eststo des1: quietly estpost sum market_share price calories organic
esttab des1 using Q1_table1.tex, replace cells("count mean sd min max") noobs label

table product_id market
collect export Q1_table2.tex, replace

**# Question 2
eststo clear
xtset product_id

**# Question 2: OLS 
bysort market period: egen out_share = sum(market_share)
replace out_share = 1 - out_share
gen logit_dv= ln(market_share/out_share)
reg logit_dv price calories organic
eststo logit_ols

**# Question 2: Hausman IVs
* Average price of products in other markets
bysort period product_id: gen num = _N
bysort period product_id: egen avg_price = mean(price)
replace avg_price = (num * avg_price - price )/ (num-1)

ivregress 2sls logit_dv (price = avg_price) calories organic
eststo logit_hausman 

**# Question 2: BLP like IVs
* Number of competing products
bysort period market: gen NCompet = _N
replace NCompet = NCompet - 1

* Average calories of competing products
bysort period market: egen avg_calories = mean(calories)
replace avg_calories = ((NCompet+1) * avg_calories - calories )/ NCompet

* Number of organic products and interacted with dummy
bysort period market: egen NOrganic = sum(organic)
sort period market calories 
gen NOrganic2 = NOrganic * organic


* Closest competitor in calories space 
bys period market: gen low = calories[_n-1]
bys period market: gen high = calories[_n+1]
gen diff_low = calories - low
gen diff_high = calories - high

replace diff_low = -diff_low if diff_low <0 
replace diff_high = -diff_high if diff_high <0 

gen min_dist_col = low if diff_low < = diff_high
replace min_dist_col = high if diff_low > diff_high

ivregress 2sls logit_dv (price = NCompet avg_calories NOrganic NOrganic2 min_dist_col) calories organic 
eststo logit_blp

**# Question 2: IV regression with both Hausman and BLP IVs
ivregress 2sls logit_dv (price = avg_price NCompet avg_calories NOrganic NOrganic2 min_dist_col) calories organic 
eststo logit_both

esttab logit* using Q2_table1.tex, mtitle("OLS" "Hausman IV" "BLP IV" "Both IV") replace

* export dataset for later
sort period market product_id
egen market_id = group(period market)
drop _est* low high diff_low diff_high
export delimited using new_data.csv, replace


**# Question 3
** Done in Python!


**# Question 4: Elasticity IV Logit 
est restore logit_both 
local b = _b[price]
drop if missing(avg_price)
predict expxb, xb
replace expxb = exp(expxb)
egen sum_expxb = sum(expxb), by(market_id)
gen market_share2 = expxb / (1+sum_expxb)
* for each product j 
forvalues j = 1/20{
	gen eta_`j'k = 0 
	* calculate own price elasticity 
	replace eta_`j'k = -price*(1-market_share2) if `j' == product_id
	* calculate cross price elasticity 
	replace eta_`j'k = price*market_share2 if `j' != product_id
	* change the elasticity to missing if product j does not exist 
	egen mkt_`j' = max(product_id == `j'), by(market_id)
	replace eta_`j'k = . if mkt_`j'==0
	drop mkt_`j'
	
	replace eta_`j'k = -(`b'*eta_`j'k)
	bys product_id: egen eta_`j'k_median = median(eta_`j'k)	
}

preserve 
keep product_id eta_*k_median 
sort product_id
duplicates drop

* keep only 3 digits
forvalues j = 1/20{
	replace eta_`j'k_median = round(eta_`j'k_median, 0.001)
	rename eta_`j'k_median eta`j'
}

estpost tabstat eta1 - eta20, by(product_id)
matrix A = e(eta1)', e(eta2)', e(eta3)', e(eta4)', e(eta5)', e(eta6)', e(eta7)', ///
e(eta8)', e(eta9)', e(eta10)', e(eta11)', e(eta12)', e(eta13)', e(eta14)', e(eta15)', ///
e(eta16)', e(eta17)', e(eta18)', e(eta19)', e(eta20)' 
esttab matrix(A) using ivlogit_elasticity.tex, title("IV Logit Elasticities") ///
nomtitle drop(Total) tex replace

export delimited using ivlogit_elasticity.csv, replace
restore




**# Question 5: Markups IV Logit 
drop eta_*_median
sort period market product_id

* single product Nash-Bertrand 
preserve 
gen omega = .
forvalues j = 1/20{
	replace omega = eta_`j'k*market_share2/price if product_id == `j'
}
replace omega = 1/omega
gen markup = -omega*market_share2
gen markuprate = markup/price*100

collapse (median) markup (median) markuprate, by(product_id)

sum markup markuprate, d
restore

* joint pricing of all products
forvalues m = 1/150{
	preserve
	quietly keep if market_id == `m'
	quietly nmissing
	quietly drop `r(varlist)'
	mkmat eta_*, matrix(omega)
	matrix omega = omega'
	drop eta*
	svmat omega, name(eta)
	quietly ds eta*
	local vvars `r(varlist)'
	foreach v of local vvars {
		quietly replace `v' = `v'*market_share2/price
	}
	mkmat eta*, matrix(omega)
	mkmat market_share2, matrix(s)
	matrix markup = -inv(omega)*s
	matrix M = M\markup
	restore
}

svmat M, name(markuptemp)
gen markup = markuptemp[_n+1]
drop markuptemp
drop if missing(market)
gen markuprate = markup/price* 100

collapse (median) markup (median) markuprate, by(product_id)

sum markup markuprate, d
