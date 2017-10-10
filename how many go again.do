set more off
version 14.2
global date 20170927

global dir **SUPPRESSED**
 
cd "$dir"




do "$code\1 data prep.do"

do "$code\2 final.do"

exit
