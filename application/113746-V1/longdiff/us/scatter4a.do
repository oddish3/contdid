reg docc `1' `2' [aw=wtbpl], robust
modl A nocon mal=`1'

reg dsei `1' `2' [aw=wtbpl], robust
modl B nocon mal=`1' 

noisily modltbl se (4) A B ,  `1'
macro drop _all
