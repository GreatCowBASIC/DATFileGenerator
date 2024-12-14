cd D:\GreatCowBASICGits\DATFileGenerator.git\XC8toINC_Converters\PIC18

18F_PIC_to_INCv6_20 PIC%1  > D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\OrgFiles\p%1.inc
D:\GreatCowBASICGits\DATFileGenerator.git\gawk -f D:\GreatCowBASICGits\DATFileGenerator.git\PreProcessIncFile.awk D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\OrgFiles\p%1.inc > D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\p%1.inc

cd D:\GreatCowBASICGits\DATFileGenerator.git
getchipdata %1

cd D:\GreatCowBASICGits\DATFileGenerator.git\XC8toINC_Converters\PIC18\PIC18Qseries


