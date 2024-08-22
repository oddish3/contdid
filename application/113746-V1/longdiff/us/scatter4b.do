ivreg docc (`1'=`3') `2' [aw=wtbpl], robust
modl A nocon mal=`1' 

ivreg dsei (`1'=`3') `2' [aw=wtbpl], robust
modl B nocon mal=`1' 

noisily modltbl se (4) A B ,  `1'
macro drop _all
