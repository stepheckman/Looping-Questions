* according to BF 6/19/2012, final data set is: DGE05386INT_V01
* according to BF 3/7/2013, final data set is: DGE05386INT_V01_2_old

cap log close
log using "$results\1_$date.smcl", replace

* get strat indicator
* copied data set to $data folder
* use "\\iab.baintern.de\DFS\017\Ablagen\D01700-Projekte\D01700-Filterfragen\LMU_Filterfragen\Data\Barbara\xwalk_web_both_tranches", clear
* _new version made by BF 20150128 -- contains tranche indicator
use "xwalk_web_both_tranches_new", clear

drop if tranche==2

keep User_Id stratum response

tempfile strat
save `strat' 


* response data
use $inputdir\dge05386int_V01_2, replace

* append breakoffs
qui: append using "$inputdir\DGE05386Abbruch_V01", gen(breakoff)

merge 1:1 User_Id using `strat'
tab _m response, mis
keep if _merge==3
drop _merge
	
tab breakoff


* where did breakoff occur
* Rs who responded to Q2 loop (saw first question)
gen sawq2 = !mi(TQ2_1HM) | !mi(TQ2_1GA)
lab var sawq2 "flags Rs who saw 1st question in Q2"

* Rs who responded to Q3 loop (saw first follow up)
gen sawq3 = !mi(TQ3_1a)
lab var sawq3 "flags Rs who saw 1st FU in Q3"

* Rs who responded to qs after HMGA
gen sawq4 = !mi(Q4_1)
lab var sawq4 "flags Rs who responded to 1st Q in Q4"

tab2 breakoff saw*, firstonly mis

* can't get q4 if you did not do both q2 & q3
assert sawq4==0 if sawq2+sawq3<=1

gen break = sawq2+sawq3+sawq4+1
lab def break 1 "before HMGA" 2 "during 1st HMGA" ///
	3 "during 2nd HMGA" 4 "after both HMGA" 
assert break == 4 if !breakoff
replace break = . if !breakoff
lab val break break
lab var break "where case broke off interview"

tab break breakoff, mis col

* drop those that broke off before HMGA qs
drop if break==1

rename User_Id id
*unique id

* time in interview
gen time = interviewLength/60
lab var time "time in survey, minutes"
sum time, det

* time in HMGA sections
egen time_hmga1 = rowmin(TQ2* TQ3*)
egen time_hmga2 = rowmax(TQ2* TQ3*)
gen time_hmga = (time_hmga2 - time_hmga1)/60/60/60
drop time_hmga1 time_hmga2
sum time_hmga, det

rename hMobile mobile



*******************************************************************
* experimental asg

rename FBlock_23 order23
lab var order23 "Q2 or Q3 asked first"
gen q2first = (order23 == 1)
lab var q2first "Q2 asked first"

rename FConsent_Option consent_exper
lab var consent_exper "R asg to consent experiment conditions"

gen consent = (Consent==1)
lab var consent "R consented to link of admin and survey data"
tab1 consent Consent, mis
tab consent Consent, mis
drop Consent


*******************************************************************
* how many qs
* Q2_1HM -- Wie viele Arbeitgeber hatten Sie insgesamt im Laufe Ihres Erwerbslebens?
* Q3_1HM -- In wie vielen Orten haben Sie bis heute gewohnt?

* go again qs
* Q2_1GA -- Waren Sie jemals erwerbstätig?
* Q2_?eGA -- Und hatten Sie danach noch einen weiteren Arbeitgeber?
* Q3_?eGA -- Haben Sie jemals noch an einem anderen Ort gelebt?

sum Q2_1HM Q3_1HM Q2_1GA Q2_?eGA Q3_?eGA
* Q3_1GA does not exist "have you ever lived anywhere"

* Q2_?eGA Q3_?eGA -- these are go-again qs


*******************************************************************
* flag for HM condition
gen hm = !mi(Q2_1HM) | !mi(Q3_1HM)
lab var hm "R in HM condition (both wohnorte & betriebe)"

* look at Rs in HM condition who said "wait there's another" in f follow up
* no WN, KA on these qs
* li Q2_?fHM Q3_?fHM if hm0
egen Q2_extrahm = anycount(Q2_?fHM), v(1)
lab var Q2_extrahm "extra loops due to FU in HM--any you forgot?"
tab Q2_extrahm, mis 
egen Q3_extrahm = anycount(Q3_?fHM), v(1)
lab var Q3_extrahm "extra loops due to FU in HM--any you forgot?"
tab Q3_extrahm, mis 

* interesting, seem to have turned it into GA!
li Q2_?fHM if Q2_extrahm > 1
li Q3_?fHM if Q3_extrahm > 1


*******************************************************************
* clean missing values
* .a = WN
* .b = KA
mvdecode Q2_1HM Q3_1HM Q?_??Jahr, mv(8888=.a \ 9999=.b)
mvdecode Q?_??Monat, mv(14=.a \ 15=.b)
mvdecode Q2_1GA Q2_?eGA Q3_?eGA, mv(3=.a \ 4=.b)
mvdecode Q3_?d, mv(18=.a \ 19=.b)
mvdecode Q2_?c, mv(88=.a \ 99=.b)
mvdecode Q2_?d, mv(7=.a \ 8=.b)

* values stored in exp notation, can't get mvdecode to work
foreach q of varlist Q3_1c Q3_2c Q3_3c Q3_4c Q3_5c Q3_6c Q3_7c {
	tab `q' if `q' > 7777777, mis
	replace `q' = .a if `q' > 8880000000 & `q' < 9900000000
	replace `q' = .b if `q' > 9900000000 & !mi(`q')
	tab `q' if `q' > 7777777 | mi(`q'), mis
}



*******************************************************************
* count yeses in go again condition
egen q2ct_ga = anycount(Q2_1GA Q2_?eGA), v(1)
lab var q2ct_ga "GA: Wie viele Arbeitgeber hatten Sie insgesamt im Laufe Ihres Erwerbslebens"
replace q2ct_ga = 0 if inlist(Q2_1GA,2,.a,.b)
* Rs in HM condition get missing here
replace q2ct_ga = . if Q2_1GA==.

egen q3ct_ga = anycount(Q3_?eGA), v(1)
* add 1 here to include the first city, which everyone is asked about
replace q3ct_ga = q3ct_ga + 1 if !mi(q3ct_ga)
replace q3ct_ga = . if Q3_1eGA==.
lab var q3ct_ga "GA: In wie vielen Orten haben Sie bis heute gewohnt"
* count GA loops again, but drop those entirely before 1999
* because admin data does not have Orte before 1999
gen q3ct_ga3 = q3ct_ga 
* FU Q3_1bJahr is when did you stop living at that place
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_1bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_2bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_3bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_4bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_5bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_6bJahr < 1999
replace q3ct_ga3 = q3ct_ga3 - 1 if Q3_7bJahr < 1999
tab q3ct_ga3 q3ct_ga


*******************************************************************
* get same counts from HM, topcode to match GA
* topcode to 7 if first section, 5 if not
* with & wo extras added in HM by "do you have another to report?" question
gen q2ct_hm = Q2_1HM 
lab var q2ct_hm "HM: Q2, wo extra loops added due to FU f"
gen q2ct_hm2 = Q2_1HM + Q2_extrahm
lab var q2ct_hm2 "HM: Q2, with extra loops added due to FU f"

* topcode to 7 if first section, 5 if not
clonevar q2ct_hm_notop = q2ct_hm
replace q2ct_hm = 7 if q2ct_hm > 7 & hm & q2first==1
replace q2ct_hm = 5 if q2ct_hm > 5 & hm & q2first==0
clonevar q2ct_hm2_notop = q2ct_hm2
replace q2ct_hm2 = 7 if q2ct_hm2 > 7 & hm & q2first==1
replace q2ct_hm2 = 5 if q2ct_hm2 > 5 & hm & q2first==0
* Rs in GA condition get missing here 
replace q2ct_hm = . if !mi(Q2_1GA)


gen q3ct_hm = Q3_1HM 
lab var q3ct_hm "HM: Q3, wo extra loops added due to FU f"
gen q3ct_hm2 = Q3_1HM + Q3_extrahm
lab var q3ct_hm2 "HM: Q3, with extra loops added due to FU f"
* count HM loops again, but drop those entirely before 1999
* because admin data does not have Orte before 1999
gen q3ct_hm3 = Q3_1HM + Q3_extrahm 
* FU Q3_1bJahr is when did you stop living at that place
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_1bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_2bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_3bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_4bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_5bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_6bJahr < 1999
replace q3ct_hm3 = q3ct_hm3 - 1 if Q3_7bJahr < 1999
lab var q3ct_hm3 "HM: Orte since 1999"
tab q3ct_hm3 q3ct_hm2
* topcode to 7 if first section, 5 if not
clonevar q3ct_hm_notop = q3ct_hm
replace q3ct_hm = 7 if q3ct_hm > 7 & hm & q2first==0 // q3first==1
replace q3ct_hm = 5 if q3ct_hm > 5 & hm & q2first==1
replace q3ct_hm = . if !mi(Q3_1eGA)
clonevar q3ct_hm2_notop = q3ct_hm2
replace q3ct_hm2 = 7 if q3ct_hm2 > 7 & hm & q2first==0
replace q3ct_hm2 = 5 if q3ct_hm2 > 5 & hm & q2first==1
replace q3ct_hm2 = . if !mi(Q3_1eGA)
replace q3ct_hm3 = 7 if q3ct_hm3 > 7 & hm & q2first==0
replace q3ct_hm3 = 5 if q3ct_hm3 > 5 & hm & q2first==1
replace q3ct_hm3 = . if !mi(Q3_1eGA)

* no adjustment here, everyone has lived someplace

tab1 q?ct_hm q?ct_ga, nol mis


*******************************************************************
* make 1 var for each of Q2 and Q3
egen q2ct = rowfirst(q2ct_hm2 q2ct_ga)
lab var q2ct "HM, GA: EMPLOYERS: w extra loops in HM, topcoded"
egen q3ct = rowfirst(q3ct_hm2 q3ct_ga)
lab var q3ct "HM, GA: PLACES: w extra loops in HM, topcoded" 
egen q3ct99 = rowfirst(q3ct_hm3 q3ct_ga3)
lab var q3ct99 "HM, GA: PLACES>1999: w extra loops in HM, topcoded" 

egen q2ct_woextra = rowfirst(q2ct_hm q2ct_ga)
lab var q2ct_woextra "HM + GA: EMPLOYERS: wo extra loops in HM, topcoded"
egen q3ct_woextra = rowfirst(q3ct_hm q3ct_ga)
lab var q3ct_woextra "HM + GA: PLACES: wo extra loops in HM, topcoded"

* flag indicating go again condition, separately for each question
gen goagain2 = !mi(q2ct_ga) if !(mi(q2ct_hm) & mi(q2ct_ga))
gen goagain3 = !mi(q3ct_ga) if !(mi(q3ct_hm) & mi(q3ct_ga))

sum q2ct_hm2 q2ct_ga q3ct_hm2 q3ct_ga q2ct q3ct q3ct99 goagain*


*******************************************************************
* filter questions
rename Filter_Forma~12 fformat
tab fformat
lab var fformat "filter format in clothing section (1/2 skipped)"
lab def fformat 1 "grouped" 2 "interleafed" 3 "skipped"
lab val fformat fformat

recode fformat (2=1) (3=0), gen(fformat2)
lab var fformat2 "indicates R recieved filter qs"

lab list FILTER_F
* format "ohne" means some cases did not receive filter qs

forv f = 1/6 {
	egen fu`f'_1 = rowfirst(Q1_`f'aa Q1_`f'ab)
	rename Q1_`f'b fu`f'_2
	rename Q1_`f'c fu`f'_3
	rename Q1_`f'd fu`f'_4
	
	rename Q1_`f' filter`f'
	tab filter`f', nol
	recode filter`f' (2=0) (3=.) (4=.) 
}

egen filtersum = rowtotal(filter?)
lab var filtersum "num filters triggered by this R"


*****************************************************
* merge in admin data 

* see 1.1 admin code -- from BF

tab consent breakoff, mis

* missing values in username are duplicates below, making problems
* fix by creating bad usernames
sum username
clonevar username_orig = username 
replace username = _n if breakoff

destring id, gen(idnum)

preserve
	keep if consent==0
	
	tempfile noconsent
	save `noconsent'
restore

drop if consent==0

preserve
	* bring in admin data -- see email from BF 11/10/2013, 25/10/2013
	use hmga_admin, replace	
	sum no_betriebe no_wohnorte 
	
	drop ct
	
	rename username idnum
	
	tempfile admin
	save `admin'
restore

merge 1:1 idnum using `admin'
drop if _merge==2 // cases only in admin, not in my dataset
qui: append using `noconsent'

assert _merge !=3 if consent==0

gen admin = (_merge==3)
lab var admin "admin data avail for this R"

* these cases should not merge, as username is fake
tab admin breakoff if consent, mis

sum no_*
tab no_betriebe consent, mis 
tab no_wohnorte consent, mis 

clonevar no_betriebe_orig = no_betriebe
clonevar no_wohnorte_orig = no_wohnorte
lab var no_betriebe_orig "Number of employers, before topcoding"
lab var no_wohnorte_orig "Number of wohnorte, before topcoding"


*****************************************************
* use admin data to get over/under report markers

* need to top code admin data to reflect top coding in GA condition
replace no_betriebe = 7 if no_betriebe > 7 & q2first & consent
replace no_betriebe = 5 if no_betriebe > 5 & !q2first & consent
replace no_wohnorte = 7 if no_wohnorte > 7 & !q2first & consent
replace no_wohnorte = 5 if no_wohnorte > 5 & q2first & consent

cap lab drop qc
lab def qc 3 "underreport" 2 "correct" 1 "overreport"

gen qc_betriebe = 3 if (no_betriebe > q2ct)
replace qc_betriebe= 2 if (no_betriebe == q2ct)
replace qc_betriebe = 1 if (no_betriebe < q2ct)
replace qc_betriebe = . if mi(no_betriebe) | mi(q2ct)
replace qc_betriebe = . if consent != 1
lab var qc_b "ME indicator for employer loop, w extra HM reports"

/* not doing wohnorte in paper
gen qc_wohnorte = 3 if (no_wohnorte > q3ct99)
replace qc_wohnorte = 2 if (no_wohnorte == q3ct99)
replace qc_wohnorte = 1 if (no_wohnorte < q3ct99)
replace qc_wohnorte = . if mi(no_wohnorte) | mi(q3ct99)
replace qc_wohnorte = . if consent != 1
lab var qc_w "ME indicator for employer loop, w extra HM reports"
*/

gen qc_betriebe_woextra = 3 if (no_betriebe > q2ct_woextra)
replace qc_betriebe_woextra= 2 if (no_betriebe == q2ct_woextra)
replace qc_betriebe_woextra = 1 if (no_betriebe < q2ct_woextra)
replace qc_betriebe_woextra = . if mi(no_betriebe) | mi(q2ct_woextra)
replace qc_betriebe_woextra = . if consent != 1
lab var qc_betriebe_woextra "ME indicator for employer loop, wo extra HM reports"


lab val qc* qc
sum qc*

tab1 qc*, mis


*****************************************************
* process follow ups Q2

* some strange issues wtih month follow ups, both Q2 and Q3
* 2 is Jan, 13 is Dez
* don't know what 1 is
* don't need real answer, just need to know if it's a valid value or not
recode Q2_?bMonat Q3_?bMonat (13=12)


* process follow ups Q2
* once for each of 7 possible loops
forv c = 1 / 7 {   

    * figure out if R should have answered fups in this loop
	* ignores when R said > 7 (or 5) because FUs not asked of these events
    gen applies_`c' = (q2ct >= `c') if !mi(q2ct)
    replace applies_`c' = 0 if mi(q2ct)

    gen q2fu`c'_startdt = mdy(Q2_`c'aMonat, 1, Q2_`c'aJahr)
    
	* fix above needed so that mdy fn works
    gen q2fu`c'_enddt = mdy(Q2_`c'bMonat, 1, Q2_`c'bJahr)
    * job still going on 
    replace q2fu`c'_enddt = mdy(1,1,2014) ///    
        if Q2_`c'bMonat == 16 | Q2_`c'bJahr == 7777
        
    gen q2fu`c'_hours = Q2_`c'c
    * 77 means unregelmassig, just leave as is

    gen q2fu`c'_position = Q2_`c'd
    * 6 means NA, just leave as is

    format q2fu`c'_startdt q2fu`c'_enddt %td
    
    * count number of missing values, if FUs should have been asked
    egen q2fu`c'_miss = rowmiss(q2fu`c'_*) if applies_`c'==1
	
	
	* missing indicators for each FU
	foreach v in q2fu`c'_startdt q2fu`c'_enddt q2fu`c'_hours q2fu`c'_position {
		gen `v'_miss = mi(`v') if applies_`c'==1
	}
}

egen q2fu_miss = rowtotal(q2fu?_miss), mis
gen q2fu_miss_pct = q2fu_miss / (q2ct * 4)
drop applies*

* process follow ups Q3
forv c = 1 / 7 {  // once for each of 7 possible loops

    * figure out if R should have answered fups in this loop
	* this code ignores HM respondents who volunteered extra loops
	* but I think that's OK, there are few of them
	* also ignores when R said > 7 (or 5) because FUs not asked of these events
    gen applies_`c' = (q3ct >= `c') if !mi(q3ct_woextra)
    replace applies_`c' = 0 if mi(q3ct_woextra)
    
	* fix above needed so that mdy fn works
    gen q3fu`c'_startdt = mdy(Q3_`c'aMonat, 1, Q3_`c'aJahr)
	* don't want missing when person says "seit mein geburt"
	replace q3fu`c'_startdt = 6666 if Q3_`c'aJahr==6666
    
    gen q3fu`c'_enddt = mdy(Q3_`c'bMonat, 1, Q3_`c'bJahr)
    * job still going on 
    replace q3fu`c'_enddt = mdy(1,1,2014) ///    
        if Q3_`c'bMonat == 16 | Q3_`c'bJahr == 7777
        
    gen q3fu`c'_resid = Q3_`c'c
    gen q3fu`c'_state = Q3_`c'd

    format q3fu`c'_startdt q3fu`c'_enddt %td
	
    * count number of missing values, if FUs should have been asked
    egen q3fu`c'_miss = rowmiss(q3fu`c'_*) if applies_`c'==1
	
		
	* missing indicators for each FU
	foreach v in q3fu`c'_startdt q3fu`c'_enddt q3fu`c'_resid q3fu`c'_state {
		gen `v'_miss = mi(`v') if applies_`c'==1
	}
}

egen q3fu_miss = rowtotal(q3fu?_miss), mis
gen q3fu_miss_pct = q3fu_miss / (q2ct * 4)
drop applies*

* combine miss pct across Q2 & Q3
egen qfu_miss = rowtotal(q?fu_miss), mis
gen qfu_miss_pct = qfu_miss / ((q2ct + q3ct) *4)

sum q?fu?_*_miss
egen q2fu_startdt_missct = rowtotal(q2fu?_startdt_miss), mis
egen q2fu_enddt_missct = rowtotal(q2fu?_enddt_miss), mis
egen q2fu_hours_missct = rowtotal(q2fu?_hours_miss), mis
egen q2fu_position_missct = rowtotal(q2fu?_position_miss), mis

egen q3fu_startdt_missct = rowtotal(q3fu?_startdt_miss), mis
egen q3fu_enddt_missct = rowtotal(q3fu?_enddt_miss), mis
egen q3fu_resid_missct = rowtotal(q3fu?_resid_miss), mis
egen q3fu_state_missct = rowtotal(q3fu?_state_miss), mis

sum q?fu_*_missct


* extra answers in HM condition
egen hmextra = rowtotal(Q2_extra Q3_extra)
replace hmextra = . if hmextra==0 & !hm
lab var hmextra "R mentioned more events than reported in HM question"


/* merge in pr_weight from combined_validation data set
* not certain, but I think this is resp prop weight developed by BF
* Ruben will make a better RP 
preserve
	use "$bf\combined_validation", replace
	keep username pr_weight
	
	tempfile rp
	save `rp'
restore
*/

d q2ct q3ct
gen qfirst = q2ct if q2first
replace qfirst = q3ct if !q2first
lab var qfirst "# events in 1st loop (q2 or q3), topcoded"

* 3 cases with missing data in 1st loop, but technically SAW the question
* count these as breaking off before HMGA sections (i.e. as break==1)
tab qfirst break, mis
recode break (2=1) if qfirst==.
drop if break==1

cap drop _merge
qui: compress
save hmga_wbreaks, replace



* drop cases that break off during looping section
drop if breakoff & break != 4

d q?ct_ga q?ct_hm* q?ct*

lab var q2ct_hm2_no "q2ct_hm2 wo top coding" 
lab var q3ct_hm2_no "q3ct_hm2 wo top coding"
lab var q2ct_hm_no "q2ct_hm wo top coding" 
lab var q3ct_hm_no "q3ct_hm wo top coding"
lab var q2ct_hm "HM #, wo extra due to FU"
lab var q3ct_hm "HM #, wo extra due to FU"
lab var q2ct_hm2 "HM #, w extra due to FU"
lab var q3ct_hm2 "HM #, w extra due to FU"
lab var q2ct "HM, GA #, w extra due to FU in HM"
lab var q3ct "HM, GA #, w extra due to FU in HM"
lab var q2ct_woextra "HM, GA #, wo extra due to FU in HM"
lab var q3ct_woextra "HM, GA #, wo extra due to FU in HM"

save hmga, replace







***************************************************
* summarize missing FU responses by Q
use hmga, clear

keep id hm q2fu?_miss q2ct_woextra

forv q=1/7 {

	* numerator -- # missing FUs in this loop
	rename q2fu`q'_miss fu_miss`q'

	* denominator -- whether this loop asked or not
	gen ct`q' = 0
	replace ct`q' = 1 if q2ct >= `q'
}
reshape long fu_miss ct, i(id) j(q)
gen Q=2

tempfile q2long
save `q2long'


* repeat for Q3
use hmga, replace

keep id hm q3fu?_miss q3ct_woextra

forv q=1/7 {

	* numerator -- # missing FUs in this loop
	rename q3fu`q'_miss fu_miss`q'

	* denominator -- whether this loop asked or not	
	gen ct`q' = 0
	replace ct`q' = 1 if q3ct >= `q'
}
reshape long fu_miss ct, i(id) j(q)
gen Q=3

append using `q2long'
drop q?ct*

gen hm2 = hm
recode hm2 (0=2)
lab def hm2 2 "Go-Again" 1 "How Many"
lab val hm2 hm2

lab def Q 2 "Employers" 3 "Locations"
lab val Q Q

lab var q "Reported Event"
lab var Q "Section"

egen events = total(ct), by(Q id)
lab var events "# events triggered by R for this section"

svyset id

* these loops not triggered
drop if ct == 0

order id hm* Q q events ct


* get svy robust results for stderr
svy: mean fu_miss, over(q hm2)
tempname sr
postfile `sr' int(q) double(mean0 stderr0 mean1 stderr1) ///
	using svyresults, replace
forv q = 1/7 {
	svy: mean fu_miss if q==`q', over(hm)
	mat b = e(b)
	mat v = e(V)
	
	post `sr' (`q') (b[1,1]) (sqrt(v[1,1])) (b[1,2]) (sqrt(v[2,2]))
}	
postclose `sr'

preserve
	use svyresults, replace

	gen lowerci0 = mean0 - 2*stderr0
	gen upperci0 = mean0 + 2*stderr0
	gen lowerci1 = mean1 - 2*stderr1
	gen upperci1 = mean1 + 2*stderr1
	
	drop stderr?
		
	save hmga_summary, replace
restore

* by section, but without svy	
preserve
	collapse (mean) fu_miss (semean) se=fu_miss ///
		(count) n=fu_miss, by(Q q hm2)
	gen lowerci = fu_miss - 2*se
	gen upperci = fu_miss + 2*se
	drop se n

	reshape wide fu_miss lowerci upperci, i(Q q) j(hm2)
	save hmga_summary_bysec, replace
restore


* repeat for only high triggering folks
keep if events >=5
collapse (mean) fu_miss (semean) se=fu_miss ///
		(count) n=fu_miss, by(q hm2)
gen lowerci = fu_miss - 2*se
gen upperci = fu_miss + 2*se
drop se n 
reshape wide fu_miss lowerci upperci, i(q) j(hm2)
save hmga_summary_qc, replace



***************************************************
* data set of Rs and NRs
use "xwalk_web_both_tranches", clear
tempfile strat
save `strat'
rename coop resppartial
lab var resppartial "full or partial R"
lab var response "full R"
merge 1:1 case_id using 1_xwalk_ff_cati_web_plus_partials.dta
keep if _m==3
tab resppartial _m, mis
drop passwort pw respondent partial User_Id phone* disp* nicht ///
	_m link tranche web_mode panel consent

logit response i.stratum var1

save rpmodel, replace



***************************************************
* specialized data set for figure 1
use hmga, clear

* idnum missing, make new one
gen idnum = _n

expand 2, gen(new)

gen q = 2 if !new
replace q = 3 if new

gen ct = q2ct if !new
replace ct = q3ct if new

egen ct2woextra = rowfirst(q2ct_hm q2ct_ga) if q==2
egen ct3woextra = rowfirst(q3ct_hm q3ct_ga) if q==3
gen ctwoextra = ct2woextra if q==2
replace ctwoextra = ct3woextra if q==3
drop ct?woextra

tab ctwoextra ct, mis
assert ctwoextra==ct if !hm
lab var ctwoextra "events reported, w/0 HM cases reporting extra events"

* make a set of responses without top coding
expand 2 if hm, gen(notop)
egen ct2notop = rowfirst(q2ct_hm2_notop q2ct_ga) if q==2
egen ct3notop = rowfirst(q3ct_hm2_notop q3ct_ga) if q==3 
egen ct2notopwoextra = rowfirst(q2ct_hm_notop q2ct_ga) if q==2
egen ct3notopwoextra = rowfirst(q3ct_hm_notop q3ct_ga) if q==3 
replace ct = ct2notop if notop & q==2
replace ct = ct3notop if notop & q==3
replace ctwoextra = ct2notopwoextra if notop & q==2
replace ctwoextra = ct3notopwoextra if notop & q==3

tab notop new

keep ct ctwoextra q q2first hm fformat notop consent idnum 

lab var ct "Events reported"
lab var q "Looping question"
lab def q 2 "Employers" 3 "Locations"
lab val q q 
lab var hm "Format"
lab def hm 0 "Go-Again" 1 "How-Many"
lab def hm2 2 "Go-Again" 1 "How-Many"
lab def hm3 2 "GA" 1 "HM"
lab val hm hm
clonevar hm2 = hm
recode hm2 (0=2)
lab val hm2 hm2

gen group = 1 if hm & notop
replace group = 2 if hm & !notop
replace group = 3 if !hm 
lab def group 1 "How Many, No Re-Coding" ///
	2 "How Many, Re-Coded" 3 "Go-Again"
lab val group group

gen g = 1 if q==2 & q2first
replace g = 2 if q==2 & !q2first
replace g = 3 if q==3 & !q2first
replace g = 4 if q==3 & q2first

lab def g 1 "Employers, 1st" 2 "Employers, 2nd" ///
	3 "Locations, 1st" 4 "Locations, 2nd"
lab val g g

gen first = (q2first==1 & q==2) | (q2first==0 & q==3)

bys id (first): gen ctfirst = ct[1]
lab var ctfirst "ct in first loop"

gen filter = inlist(fformat,1,2)
lab var filter "R received filter qs"

egen rtag = tag(idnum)
lab var rtag "R level indicator"

gen extra = (ct != ctwoextra)

svyset id
xtset id
save hmga_expand, replace



** make item level dataset for filter analysis
use hmga_wbreaks, clear

keep fformat filter* id 
drop filtersum
drop if mi(fformat)
lab val filter*

reshape long filter, i(id) j(fq)

svyset id

save hmga_filter, replace



cap log close
exit


* check that no one is in both conditions!
* indicators for which condition each R is in
gen hm2 = !mi(q2ct_hm) | q2ct_hm==.a | q2ct_hm==.b
lab var hm2 "R in HOW MANY format - employers"
gen ga2 = !mi(q2ct_ga) | q2ct_ga==.a | q2ct_ga==.b
lab var ga2 "R in GO AGAIN format - employers"
gen hm3 = !mi(q3ct_hm) | q3ct_hm==.a | q3ct_hm==.b
lab var hm3 "R in HOW MANY format - places lived"
gen ga3 = !mi(q3ct_ga) | q3ct_ga==.a | q3ct_ga==.b
lab var ga3 "R in GO AGAIN format - places lived"
assert hm2 + ga2 == 1
assert hm3 + ga3 == 1

