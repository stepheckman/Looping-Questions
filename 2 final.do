set more off
clear all

capture log close
log using "$results\hmga_$date.smcl", replace


*****************************************************
* Data section 

use hmga_wbreaks, replace

tab breakoff, mis

* time in survey
sum time if !breakoff, det
mean time if !breakoff, over(hm) coefl
test _b[0] = _b[1]
* no diff in time in survey

* time in HMGA sections
sum time_hmga if !breakoff, det
* medians identical!
bys hm: sum time_hmga if !breakoff, det
* no sig diff in mean
mean time_hmga if !breakoff, over(hm) coefl
test _b[0] = _b[1]
* no diff in time in hmga sections

tab mobile if !breakoff

tab break, mis


* consent rate
use hmga, replace

mean consent
mean consent, over(hm) 
test _b[0]=_b[1]

* none who consented not linked
tab admin if consent, mis

* no diff in admin by format
mean no_betriebe_orig, over(hm)
test _b[0]=_b[1]
mean no_betriebe, over(hm)
mean no_wohnorte, over(hm)



*****************************************************
* RQ1
use hmga, replace

* extra answers in HM condition
tab hmextra 
unique id if hmextra > 0 & !mi(hmextra)

tab1 Q2_extrahm Q3_extrahm
total Q2_extrahm Q3_extrahm
total hm


* wo extra events
forvalues v = 2/3 {
	di
	di
	d q`v'ct_woextra
	di 
	mean q`v'ct_woextra, over(hm) 
	test _b[0] = _b[1]
	di 
	mean q`v'ct_woextra if consent==1, over(hm) 
	test _b[0] = _b[1]
}

* w extra events
forvalues v = 2/3 {
	di
	di
	d q`v'ct
	di 
	mean q`v'ct, over(hm) 
	test _b[0] = _b[1]
	di 
	mean q`v'ct if consent==1, over(hm) 
	test _b[0] = _b[1]
}

* no diffs by gender
forv s = 1/2 {
	tab q2ct_woextra hm if sex==`s', chi
	tab q3ct_woextra hm if sex==`s', chi
}



*****************************************************
* RQ1.1 -- model
				
* w/o extra responses in HM format
use hmga_expand, replace

describe ct*
drop ct ctfirst /// leaves only ctwoextra

* mean, var of DV
sum ct, det

svyset idnum

* with consent, all cases
svy: poisson ctwoextra ib1.hm##(ib1.first i.consent) i.q
* same results with clustered std errors
* poisson ctwoextra ib1.hm##(ib1.first i.consent) i.q, vce(cluster idnum)
gen p1 = e(sample)
est sto p1
margins, dydx(0.hm 3.q 0.first 1.consent)

* with sex, linked cases only
svy: poisson ctwoextra ib1.hm##(ib1.first i.sex) i.q if consent
gen p2 = e(sample)
est sto p2
margins, dydx(0.hm 3.q 0.first 2.sex)

tab1 p1 p2

esttab p1 p2, se drop(1.hm* *1.first *1.sex 2.q _cons *0.consent) ///
	order(0.hm 3.q 0.first 1.consent 2.sex 0.hm#0.first 0.hm#2.sex) stats(N N_psu F p)

* case ns for table
unique idnum if p1
unique idnum if p2
unique idnum if consent		


*****************************************************
* figure 3

* revised 1 data prep code 21050521 to use only HM events mentioned in HM q
* that is, not extra events

use hmga_expand, clear

* ensure ctwoextra is used when var ct is named 
drop ct ctfirst

d group
lab def group 1 "How-Many, No Re-Coding" 2 "How-Many, Re-Coded", modify

hist ct if q==2, by(group, cols(1) note(" ") title("Employers",span)) ///
	scheme(s1mono) xlab(0(5)25)  ylabel(, nogrid) ///
	discrete freq  fxsize(68) saving(q2, replace) yscale(r(0 150)) ///
	xtitle(,size(large)) ytitle(,size(large)) xtitle("Events reported")
hist ct if q==3, by(group, cols(1) note(" ") title("Locations")) ///
	scheme(s1mono) xlab(0(5)25)  ylabel(none, nolabels grid) ///
	discrete freq ytit(" ") yscale(r(0 150)) saving(q3, replace) ///
	xtitle(,size(large)) ytitle(,size(large)) xtitle("Events reported")
gr combine q2.gph q3.gph, ycommon commonscheme scheme(s1mono) ///
	imargin(0 0 0 0)
gr export "reports_by_format_woextra.eps", replace
gr export "reports_by_format_woextra.png", replace
gr export "reports_by_format_woextra.tif", replace width(5000)






*****************************************************
* RQ2
	
use hmga, replace

* admin var -- # employers
* top coded to 7 or 5
d no_betriebe

* response == # employers
* without extra reports due to final q in HM condition
* top coded to 7 or 5
d q2ct_woextra 

mean no_betriebe q2ct_woextra, over(hm)
test _b[no_betriebe:0] = _b[q2ct_woextra:0]
test _b[no_betriebe:1] = _b[q2ct_woextra:1]


* output to send back to Steph
tab err_signed hm

mean no_betriebe q2ct_woextra, over(hm)
test _b[no_betriebe:0] = _b[q2ct_woextra:0]
test _b[no_betriebe:1] = _b[q2ct_woextra:1]


*****************************************************
* figure 4 

* new runs at IAB, by GCH
cd "\\iab.baintern.de\DFS\017\Ablagen\D01700-Projekte\D01700-Filterfragen\LMU_Filterfragen\Data\HMGA\"

use hmga, clear

* admin var -- # employers
* top coded to 7 or 5
d no_betriebe
assert no_betriebe <= 7

* response == # employers
* without extra reports due to final q in HM condition
* top coded to 7 or 5
d q2ct_woextra 

sum no_betriebe  q2ct_woextra


* 2 new error vars reviewers wants
gen err_signed = q2ct_woextra - no_betriebe
* if err_signed > 0 -- overreport

* results -- from GCH 20170922

/*
      |   R in HM condition
           |   (both wohnorte &
           |       betriebe)
err_signed |         0          1 |     Total
-----------+----------------------+----------
        -7 |         1          0 |         1 
        -6 |        13          2 |        15 
        -5 |        21          4 |        25 
        -4 |        41         16 |        57 
        -3 |        59         28 |        87 
        -2 |        67         40 |       107 
        -1 |        64         71 |       135 
         0 |        62        135 |       197 
         1 |        10         23 |        33 
         2 |         4          7 |        11 
         3 |         2          2 |         4 
         4 |         1          3 |         4 
         5 |         1          1 |         2 
         6 |         0          1 |         1 
*/

use error_freqs, replace

expand freq

rename value error_signed
drop freq

tab hm
recode hm (0=2)
lab def hm 2 "Go-Again" 1 "How-Many"
lab val hm hm 
tab hm

histogram error_signed, discrete percent ///
	by(hm, note("") title(Distribution of Error by Format) ) ///
	scheme(s1mono) xtitle(Error: # reported - # in admin) ///
	saving(g2, replace)
gr export "err_signed.eps", replace
gr export "err_signed.png", replace
	
	

*****************************************************
* RQ3
use hmga, replace

* only missing data indicators
keep id hm q?fu?_*_miss

forv l = 1/7 {
	rename q2fu`l'_startdt_miss q2fu1_`l'
	rename q2fu`l'_enddt_miss q2fu2_`l'
	rename q2fu`l'_hours_miss q2fu3_`l'
	rename q2fu`l'_position_miss q2fu4_`l'
	
	rename q3fu`l'_startdt_miss q3fu1_`l'
	rename q3fu`l'_enddt_miss q3fu2_`l'
	rename q3fu`l'_resid_miss q3fu3_`l'
	rename q3fu`l'_state_miss q3fu4_`l'
}
reshape long q2fu1_ q2fu2_ q2fu3_ q2fu4_ ///
	q3fu1_ q3fu2_ q3fu3_ q3fu4_, i(id) j(loop)

* missing data rates across loops, by section and FU
* most missing in data dates in both sections
sum q2*
sum q3*

* sig check to go with figure
* by section
egen q2fumiss = rowtotal(q2*), missing
egen q3fumiss = rowtotal(q3*), missing

drop q2*_ q3*_

* all diffs in fig 2 sig expect for loop 1 in q3 (resid)
forv l = 1/7 {
	mean q2fumiss if loop==`l', over(hm)
	test _b[0] = _b[1] 
}

forv l = 1/7 {
	mean q3fumiss if loop==`l', over(hm)
	test _b[0] = _b[1] 
}


*****************************************************
* figure 5

* revised 1 data prep code 21050522 to use only HM events mentioned in HM q
* that is, not extra events
use hmga_summary, replace

twoway (rcap lowerci1 upperci1 q, horizontal) ///
	(scatter q mean1, m(oh) msize(large)) ///
	(rcap lowerci0 upperci0 q, horizontal) ///
	(scatter q mean0, m(th) msize(large)), ysc(reverse r(1(1)7)) ylab(1(1)7) ///
	scheme(s1mono) legend(on lab(2 "How-Many") lab(4 "Go-Again") ///
		order(2 4)) ytit("Loop") ///
	xtit("Average Number of Follow Ups Missing")
gr export "fumiss_byq.png", replace	
gr export "fumiss_byq.eps", replace
gr export "fumiss_byq.tif", replace width(5000)


	
*****************************************************
* RQ4 -- breakoffs

use hmga_wbreaks, clear

* redefine breakoffs to only those breaking during loops
* n=109
replace breakoff=0 if break==4

tab breakoff, mis
tab2 breakoff hm q2first, firstonly col freq chi exact

* repeat with cases that did not receive filters
tab2 breakoff hm q2first if fformat2==0, firstonly col freq chi exact

capture log close




exit





