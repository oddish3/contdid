capture log close
log using panel_results.log,replace

set more 1
clear 
set mem 500m
set matsize 800



/* UNITED STATES */

use paneldata_us,replace

local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, BASELINE
macro drop _all



generate byte post20 = year>1920
generate byte post20xyr = (year-1920)*post20
local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.bplg*post20 i.bplg*post20xyr 
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, POST-1920 BREAK IN TREND
macro drop _all
drop post20*


local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year i.bplg*year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year i.bplg*year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.bplg*year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year i.bplg*year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year i.bplg*year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.bplg*year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, ALLOW FOR BPL x TIME EFFECTS
macro drop _all



preserve
drop if year<1930
local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, DROP EARLY YEARS
macro drop _all
restore




egen regionxyearxyob = group(year yob south)
local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year
areg occscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, INCLUDE REGION x YEAR x YOB EFFECTS
macro drop _all
drop regionxyearxyob


local alt_mal "malecol_hong tempavg altitude"
foreach x of local alt_mal  {
	replace `x' = `x' * exp
}
local meanrev "lebergott09 south"
local the_rest "imr1890 aurb90 aadlit91 docs1898 spend hookworm ablack91 unemp_1930 chgterm chgptratio "
xi i.bplg i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplg)
modl A nocon mal=malxexp
xi i.bplg*yob i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplg)
modl B nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplg)
modl C nocon mal=malxexp
xi i.bplg i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplg)
modl D nocon mal=malxexp
xi i.bplg*yob i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplg)
modl E nocon mal=malxexp
xi i.bplg*yob i.bplg*yob2 i.year i.yob
ivreg occscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplg)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, CORRECT FOR MEAS ERROR WITH IV
macro drop _all








/* BRAZIL */

use paneldata_br,clear

local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, BASELINE
macro drop _all


local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bplregbr*yrexp i.bpls i.year i.bpls*year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year i.bpls*year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year i.bpls*year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls i.year i.bpls*year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year i.bpls*year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year i.bpls*year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, ALLOW FOR BPL x TIME EFFECTS
macro drop _all

preserve
drop if year<1970
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, DROP EARLY YEARS
macro drop _all
restore

egen regionxyearxyob = group(year yob bplreg)
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year
areg inctot malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, INCLUDE REGION x YEAR x YOB EFFECTS
macro drop _all
drop regionxyearxyob

local alt_mal  "tempavg altitude tempxalt"
foreach x of local alt_mal  {
	replace `x' = `x' * yrexp
}
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bplregbr*yrexp i.bpls i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl A nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl B nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl C nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl D nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl E nocon mal=malxexp
xi i.bplregbr*yrexp i.bpls*yob i.bpls*yob2 i.year i.yob
ivreg inctot (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, CORRECT FOR MEAS ERROR WITH IV
macro drop _all









/* MEXICO */

use paneldata_mx,replace

local meanrev "dens lnele norte sur"
local the_rest " inmort ea eaag eaexin eaintr eatran easer eaoth"

xi i.bpls i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bpls*yob i.year
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bpls i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, BASELINE
macro drop _all




local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bpls i.year i.bpls*year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bpls*yob i.year i.bpls*year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year i.bpls*year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bpls i.year i.bpls*year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bpls*yob i.year i.bpls*year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year i.bpls*year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, ALLOW FOR BPL x TIME EFFECTS
macro drop _all



preserve
drop if year<1970
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bpls i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bpls i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(yob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, DROP EARLY YEARS
macro drop _all
restore




egen regionxyearxyob = group(year yob bplreg)
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bpls i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl A nocon mal=malxexp
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl B nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl C nocon mal=malxexp
xi i.bpls i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl D nocon mal=malxexp
xi i.bpls*yob i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl E nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year
areg incearn malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, INCLUDE REGION x YEAR x YOB EFFECTS
macro drop _all
drop regionxyearxyob



local alt_mal  "tempavg altitude tempxalt"
foreach x of local alt_mal  {
	replace `x' = `x' * yrexp
}
local meanrev "dens lnele"
local the_rest "inmort ea eaag eaexin eaintr eatran easer eaoth"
xi i.bpls i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl A nocon mal=malxexp
xi i.bpls*yob i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl B nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplstate)
modl C nocon mal=malxexp
xi i.bpls i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl D nocon mal=malxexp
xi i.bpls*yob i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl E nocon mal=malxexp
xi i.bpls*yob i.bpls*yob2 i.year i.yob
ivreg incearn (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplstate)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, CORRECT FOR MEAS ERROR WITH IV
macro drop _all


















/* COLOMBIA */


clear
clear matrix
set mem 1000m
set matsize 4000
use paneldata_co,clear

local meanrev "lndens nivel"
local the_rest "helm hook leish yelfev land vio* cafetera carbon ganad mkta manuf"
xi  i.yob i.bplregcol*yrexp i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl A nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl B nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl C nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl D nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl E nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, BASELINE
macro drop _all


local meanrev "lndens nivel"
local the_rest "helm hook leish yelfev land vio* cafetera carbon ganad mkta manuf"
xi  i.yob i.bplregcol*yrexp i.year i.bplcol1*year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl A nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year i.bplcol1*year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl B nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year i.bplcol1*year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl C nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.year i.bplcol1*year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl D nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year i.bplcol1*year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl E nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year i.bplcol1*year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(bplcol2) cluster(bplcol2)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, ALLOW FOR BPL x TIME EFFECTS
macro drop _all


egen regionxyearxyob = group(year yob bplregcol)
local meanrev "lndens nivel"
local the_rest "helm hook leish yelfev land vio* cafetera carbon ganad mkta manuf"
xi  i.yob i.bplregcol*yrexp i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl A nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl B nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year
areg incscore malxexp             `meanrev'  _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl C nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl D nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl E nocon mal=malxexp
xi  i.yob i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year
areg incscore malxexp             `meanrev' `the_rest'   _I* [aw=wtbpl], absorb(regionxyearxyob) cluster(bplcol2)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, INCLUDE REGION x YEAR x YOB EFFECTS
macro drop _all
drop regionxyearxyob



local alt_mal  "temperature altitude mell m46 areamal ecozone_stdry ecozone_stwet ecozone_trdry ecozone_trwet ecozone_warm"
foreach x of local alt_mal  {
	replace `x' = `x' * yrexp
}
local meanrev "lndens nivel"
local the_rest "helm hook leish yelfev land vio* cafetera carbon ganad mkta manuf"
xi  i.bplcol2 i.bplregcol*yrexp i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplcol2)
modl A nocon mal=malxexp
xi  i.bplcol2 i.bplregcol*yrexp i.bplcol1*yob i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplcol2)
modl B nocon mal=malxexp
xi  i.bplcol2 i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev'  _I* [aw=wtbpl],  cluster(bplcol2)
modl C nocon mal=malxexp
xi  i.bplcol2 i.bplregcol*yrexp i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplcol2)
modl D nocon mal=malxexp
xi  i.bplcol2 i.bplregcol*yrexp i.bplcol1*yob i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplcol2)
modl E nocon mal=malxexp
xi  i.bplcol2 i.bplregcol*yrexp i.bplcol1*yob i.bplcol1*yob2 i.year i.yob
ivreg incscore (malxexp = `alt_mal')            `meanrev' `the_rest'   _I* [aw=wtbpl],  cluster(bplcol2)
modl F nocon mal=malxexp
modltbl se (4) A B C D E F, CORRECT FOR MEAS ERROR WITH IV
macro drop _all








log close
exit


