if "`2'"=="" {
	local modlinput = "`1'"
}
else {
	local modlinput = "`2'"
}

if "`4'"=="" {
	local endog = "poveda"
}
else {
	local endog = "`4'"
}

ivreg dlit (`endog' = `3') `1' [aw=wtbpl], robust
modl A nocon mal=`endog' `modlinput'

ivreg dsch (`endog' = `3') `1' [aw=wtbpl], robust
modl C nocon mal=`endog' `modlinput'

ivreg dscore (`endog' = `3')  `1' [aw=wtbpl], robust
modl G nocon mal=`endog' `modlinput'


noisily modltbl se (5) A C G,  `1'
macro drop _all

