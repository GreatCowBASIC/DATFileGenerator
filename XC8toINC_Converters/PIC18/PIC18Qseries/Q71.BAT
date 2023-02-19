cd D:\GreatCowBASICGits\DATFileGenerator.git\trunk\XC8toINC_Converters\PIC18\PIC18Qseries

..\18F_PIC_to_INCv6_05 PIC%1  > ..\..\..\incfiles\OrgFiles\p%1.inc

gawk -f ..\..\..\preprocessIncFile.awk ..\..\..\incfiles\OrgFiles\p%1.inc > ..\..\..\incfiles\p%1.inc


rem pause
cd ..\..\..\
getchipdata %1

cd D:\GreatCowBASICGits\DATFileGenerator.git\trunk\XC8toINC_Converters\PIC18\PIC18Qseries

rem pause
