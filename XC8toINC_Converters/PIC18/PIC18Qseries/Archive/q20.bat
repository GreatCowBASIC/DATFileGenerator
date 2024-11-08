cd D:\GreatCowBASICGits\DATFileGenerator\XC8toINC_Converters\PIC18\PIC18Qseries

D:\GreatCowBASICGits\DATFileGenerator\XC8toINC_Converters\PIC18\18F_PIC_to_INCv6_15a PIC%1  >  D:\GreatCowBASICGits\DATFileGenerator\incfiles\OrgFiles\p%1.inc

cd D:\GreatCowBASICGits\DATFileGenerator
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p%1.inc > incfiles\p%1.inc

D:\GreatCowBASICGits\DATFileGenerator\getchipdata %1

