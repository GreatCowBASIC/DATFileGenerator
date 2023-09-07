D:\GreatCowBASICGits\DATFileGenerator.git\trunk\XC8toINC_Converters\PIC10_12_16\16F_PIC_to_IN_6.15 PIC%1 >  D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\OrgFiles\p%1.inc

gawk -f D:\GreatCowBASICGits\DATFileGenerator.git\trunk\preprocessIncFile.awk D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\OrgFiles\p%1.inc > D:\GreatCowBASICGits\DATFileGenerator.git\trunk\incfiles\p%1.inc

cd D:\GreatCowBASICGits\DATFileGenerator.git\trunk
D:\GreatCowBASICGits\DATFileGenerator.git\trunk\getchipdata %1
:fin
