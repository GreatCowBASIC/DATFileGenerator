cd D:\GreatCowBASICGits\DATFileGenerator.git\trunk\XC8toINC_Converters\PIC18\PICJ94Series
..\18F_PIC_to_INCv6_15 PIC%1  >  D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\OrgFiles\p%1.inc

gawk -f  D:\GreatCowBASICGits\DATFileGenerator.git\trunk\PreProcessIncFile.awk D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\OrgFiles\p%1.inc > D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\p%1.inc


cd D:\GreatCowBASICGits\DATFileGenerator.git\trunk 
getchipdata %1
