goto %1
18F_PIC_to_INCv4 PIC18F04Q40  >  D:\DAT\incfiles\OrgFiles\p18F04Q40.inc
18F_PIC_to_INCv4 PIC18F05Q40  >  D:\DAT\incfiles\OrgFiles\p18F05Q40.inc
18F_PIC_to_INCv4 PIC18F06Q40  >  D:\DAT\incfiles\OrgFiles\p18F06Q40.inc
18F_PIC_to_INCv4 PIC18F14Q40  >  D:\DAT\incfiles\OrgFiles\p18F14Q40.inc
18F_PIC_to_INCv4 PIC18F15Q40  >  D:\DAT\incfiles\OrgFiles\p18F15Q40.inc
18F_PIC_to_INCv4 PIC18F16Q40  >  D:\DAT\incfiles\OrgFiles\p18F16Q40.inc

gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F04Q40.inc > incfiles\p18F04Q40.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F04Q40.inc > incfiles\p18F05Q40.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F05Q40.inc > incfiles\p18F06Q40.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F14Q40.inc > incfiles\p18F14Q40.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F15Q40.inc > incfiles\p18F15Q40.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F16Q40.inc > incfiles\p18F16Q40.inc

:skip

getchipdata 18F04Q40  
getchipdata 18F05Q40  
getchipdata 18F06Q40  
getchipdata 18F14Q40  
getchipdata 18F15Q40  
getchipdata 18F16Q40  

