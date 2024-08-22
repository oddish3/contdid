capture log close
log using longdiff.log,replace


* basic results:

clear
set more 1
set mem 25m
set matsize 800


use longdiff_col,clear


local holdridge = "ecozone_stdry ecozone_stwet ecozone_trdry ecozone_trwet ecozone_warm"

local conflict =   "vioearly violate "
local conflict2 =  "vioe=vioearly viol=violate "

local endowment =  "cafetera carbon ganadera_neuva mktaccess manuf nivel_de_vida lndens"
local endowment2 = "cafe=cafetera carbon ganad=ganadera_neuva mkta=mktaccess manuf nivel=nivel_de_vida dens=lndens"

local both =       "`conflict' `endowment'"
local both2 =      "`conflict2' `endowment2'"

local diseases =   "helminth_nh hookworm leishmaniasis yelfev"
local diseases2 =  "helmnh=helminth_nh hook=hookworm leish=leishmaniasis yelfev"

local allthree = "helm hook leish yel land vio* cafetera carbon ganad mkta manuf nivel"


**** first with poveda measure

xi i.bplreg 
run scatter1a "_I* " "Reg56"
run scatter1a "_I* `conflict'" "Reg56 `conflict2'"
run scatter1a "_I* `endowment'" "Reg56 cafe=cafetera carbon ganad=ganadera_neuva mkta=mktaccess manuf nivel=nivel_de_vida"
run scatter1a "_I* `diseases'" "Reg56 `diseases2'"
run scatter1a "_I* `allthree'" "Reg56 All3 "

run scatter1b "_I*" "Reg56" "temp alt at" "poveda"
run scatter1b "_I*" "Reg56" "mell"        "poveda"
run scatter1b "_I*" "Reg56" "`holdridge'" "poveda"
run scatter1b "_I*" "Reg56" "temp alt at mell `holdridge' m46 areamal" "poveda"



**** now with mellinger measure

xi i.bplreg 
run scatter1a "_I* " "Reg56" mell
run scatter1a "_I* `conflict'" "Reg56 `conflict2'" mell
run scatter1a "_I* `endowment'" "Reg56 cafe=cafetera carbon ganad=ganadera_neuva mkta=mktaccess manuf nivel=nivel_de_vida" mell
run scatter1a "_I* `diseases'" "Reg56 `diseases2'" mell
run scatter1a "_I* `allthree'" "Reg56 All3 " mell

run scatter1b "_I*" "Reg56" "temp alt at" "mell"
run scatter1b "_I*" "Reg56" "poveda"        "mell"
run scatter1b "_I*" "Reg56" "`holdridge'" "mell"
run scatter1b "_I*" "Reg56" "temp alt at poveda `holdridge' m46 areamal" "mell"


log close
exit

