if "`2'"=="" {
	local modlinput = "`1'"
}
else {
	local modlinput = "`2'"
}

if "`3'"=="" {
	local endog = "poveda"
}
else {
	local endog = "`3'"
}

reg dlit `endog' `1' [aw=wtbpl], robust
modl A nocon mal=`endog' `modlinput'

reg dsch `endog' `1' [aw=wtbpl], robust
modl C nocon mal=`endog' `modlinput'

*reg dinc `endog'  `1' [aw=wtbpl], robust
*modl E nocon mal=`endog' `modlinput'

reg dscore `endog'  `1' [aw=wtbpl], robust
modl G nocon mal=`endog' `modlinput'

noisily modltbl se (5) A C G,  `1'
macro drop _all

