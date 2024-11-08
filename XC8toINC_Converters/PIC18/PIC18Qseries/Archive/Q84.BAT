cd \DAT\XC8toINC_Converters\PIC18\PIC18Qseries
..\18F_PIC_to_INCv5_50 PIC%1  >  D:\DAT\incfiles\OrgFiles\p%1.inc

gawk -f \dat\preprocessIncFile.awk \dat\incfiles\OrgFiles\p%1.inc > \dat\incfiles\p%1.inc


rem pause
cd \dat
\dat\getchipdata %1
rem pause
