capture log close
log using longdiff_br.log,replace

clear
set more 1
set mem 100m

use longdiff_br

xi i.bplreg

* basic
run scatter3a "lnele _Ib*  [aw=wtbpl] " 

local extravars inmort
run scatter3a "`extravars' lnele _Ib*  [aw=wtbpl] " 

local extravars "eaag eaexin eaintr eatran easer eaoth"
run scatter3a "`extravars' lnele _Ib*  [aw=wtbpl] " 

local extravars "inmort eaag eaexin eaintr eatran easer eaoth ea dens"
run scatter3a "`extravars' lnele _Ib*  [aw=wtbpl] " 

local excliv "tempavg altitude ta"
run scatter4a "lnele dens _Ib*  [aw=wtbpl] "  "`excliv'"
run scatter4a "`extravars' lnele dens _Ib*  [aw=wtbpl] "  "`excliv'"

log close
exit
