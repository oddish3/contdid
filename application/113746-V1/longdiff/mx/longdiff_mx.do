capture log close
log using longdiff_mx.log,replace

clear
set more 1
set mem 100m

use longdiff_mx,clear

local excliv "tempavg altitude ta rainmm tr ar"

* basic
run scatter3a "lnele norte sur " 

local extravars inmort
run scatter3a "`extravars' lnele norte sur " 

local extravars "eaag eaexin eaintr eatran easer eaoth"
run scatter3a "`extravars' lnele norte sur " 

local extravars "inmort eaag eaexin eaintr eatran easer eaoth ea dens"
run scatter3a "`extravars' lnele norte sur " 

run scatter4a "lnele norte sur "  "`excliv'"
run scatter4a "`extravars' lnele norte sur "  "`excliv'"



log close
exit

