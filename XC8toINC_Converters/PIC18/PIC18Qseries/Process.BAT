cd D:\GreatCowBASICGits\DATFileGenerator\XC8toINC_Converters\PIC18

18F_PIC_to_INCv6_15a PIC%1  > D:\GreatCowBASICGits\DATFileGenerator\incfiles\OrgFiles\p%1.inc

D:\GreatCowBASICGits\DATFileGenerator\gawk -f D:\GreatCowBASICGits\DATFileGenerator\PreProcessIncFile.awk D:\GreatCowBASICGits\DATFileGenerator\incfiles\OrgFiles\p%1.inc > D:\GreatCowBASICGits\DATFileGenerator\incfiles\p%1.inc


rem pause
cd D:\GreatCowBASICGits\DATFileGenerator
getchipdata %1

cd D:\GreatCowBASICGits\DATFileGenerator\XC8toINC_Converters\PIC18\PIC18Qseries


