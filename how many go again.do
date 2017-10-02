set more off
version 14.2
global date 20170927

global dir "C:\Users\seckman\Dropbox\Motivated Underreporting\papers\How Many Go Again\analysis\final"
*global data "$dir"
*global results "$dir\Aug16 visit\"
*global code "$dir\Aug16 visit\"
 
cd "$dir"




do "$code\1 new 20160818.do"

do "$code\2 new 20160818.do"


* from new runs by GCH 20170922

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

gen error_abs = abs(error_signed)

tab hm
recode hm (0=2)
lab def hm 2 "Go-Again" 1 "How Many"
lab val hm hm 
tab hm


histogram error_abs, discrete percent ///
	by(hm, note("") title(Distribution of Error by Format)) ///
	scheme(s1mono) xtitle(Absolute Value of Error) ///
	saving(g1, replace)
gr export "err_abs.eps", replace
gr export "err_abs.png", replace

histogram error_signed, discrete percent ///
	by(hm, note("") title(Distribution of Error by Format) ) ///
	scheme(s1mono) xtitle(Error: # reported - # in admin) ///
	saving(g2, replace)
gr export "err_signed.eps", replace
gr export "err_signed.png", replace
	
	
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


* filter effect in clothing items
*do "$code\2.1 filter effect.do"





