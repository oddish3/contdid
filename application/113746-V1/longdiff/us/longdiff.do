clear
set more 1
set mem 50m
capture log close
log using longdiff.log,replace


use longdiff_us,clear



local malvar malmort1890

* OLS

run scatter4a `malvar'  "south lebergott99"  
run scatter4a `malvar'  "south imr1890 docs1898 spend infkof" 
run scatter4a `malvar'  "south aadlit91 chgterm chgptratio chgsal" 
run scatter4a `malvar'  "south aurb90 ablack91 unemp" 
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 

* 2SLS

run scatter4b `malvar' "south lebergott99 " "malecol mell malpc1920"
run scatter4b `malvar'  "south lebergott99"  "temp alt ta"
run scatter4b `malvar' "south lebergott99 " "temp alt ta malecol mell malpc1920"


local malvar malecol_hong

* OLS

run scatter4a `malvar'  "south lebergott99"  
run scatter4a `malvar'  "south imr1890 docs1898 spend infkof" 
run scatter4a `malvar'  "south aadlit91 chgterm chgptratio chgsal" 
run scatter4a `malvar'  "south aurb90 ablack91 unemp" 
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 

* 2SLS

run scatter4b `malvar' "south lebergott99 " "malmort1890 mell malpc1920"
run scatter4b `malvar' "south lebergott99 " "temp alt ta"
run scatter4b `malvar' "south lebergott99 " "temp alt ta malmort1890 mell malpc1920"




generate byte border = 0
replace border = 1 if state2=="NM"
replace border = 1 if state2=="OK"
replace border = 1 if state2=="MO"
replace border = 1 if state2=="IL"
replace border = 1 if state2=="IN"
replace border = 1 if state2=="OH"
replace border = 1 if state2=="MD"
replace border = 1 if state2=="DE"
replace border = 1 if state2=="PA"



**** SOUTH+BORDER
preserve
keep if south | border
local malvar malmort1890
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 
local malvar malecol_hong
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 
restore

**** NON-SOUTH and NON-BORDER
preserve
drop if south | border
local malvar malmort1890
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 
local malvar malecol_hong
run scatter4a `malvar'  "south lebergott99 l2 l3 imr1890 docs1898 spend infkof aurb90 ablack91 unemp aadlit91 chgterm chgptratio chgsal" 
restore


log close
exit

