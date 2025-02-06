echo on

Getchipdata
copy chipdata\lflist.dat chipdata\lflist.all

dir incfiles\p1*.inc /b > inclist
Rem call Getchipdata via GAWK to remove the suffix 'p'.
GAWK -f GEN_LegacyPIC.AWK inclist > chipdata\chipdata.log


del chipdata\lflist.dat
copy chipdata\lflist.all chipdata\lflist.dat
del chipdata\lflist.all
