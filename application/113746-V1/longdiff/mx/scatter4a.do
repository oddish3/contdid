ivreg dlit (mellinger = `2')    `1'     ,robust
modl A nocon mal=mellinger

ivreg dsch (mellinger = `2')    `1'     ,robust
modl B nocon mal=mellinger

*ivreg dinctot (mellinger = `2')    `1'     ,robust
*modl C nocon mal=mellinger

ivreg dincearn (mellinger = `2')    `1'     ,robust
modl D nocon mal=mellinger

noisily modltbl se (5) A B D , `1'
macro drop _all
