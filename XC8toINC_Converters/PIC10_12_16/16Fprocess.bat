D:\GreatCowBASICGits\DATFileGenerator.git\XC8toINC_Converters\PIC10_12_16\16F_PIC_to_IN_6.15 PIC%1 >  D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\OrgFiles\p%1.inc

gawk -f D:\GreatCowBASICGits\DATFileGenerator.git\preprocessIncFile.awk D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\OrgFiles\p%1.inc > D:\GreatCowBASICGits\DATFileGenerator.git\incfiles\p%1.inc

cd D:\GreatCowBASICGits\DATFileGenerator.git
D:\GreatCowBASICGits\DATFileGenerator.git\getchipdata %1
:fin
