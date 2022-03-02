\DAT\XC8toINC_Converters\PIC10_12_16\16F_PIC_to_INCv5 PIC%1 >  \dat\incfiles\OrgFiles\p%1.inc

\DAT\gawk -f \dat\preprocessIncFile.awk \dat\incfiles\OrgFiles\p%1.inc > \dat\incfiles\p%1.inc

cd \dat
\dat\getchipdata %1
:fin
