reg dlit mellinger    `1'     ,robust
modl A nocon mal=mellinger

reg dsch mellinger    `1'     ,robust
modl B nocon mal=mellinger

*reg dinctot mellinger    `1'     ,robust
*modl C nocon mal=mellinger

reg dincearn mellinger    `1'     ,robust
modl D nocon mal=mellinger

noisily modltbl se (5) A B D , `1'
macro drop _all
