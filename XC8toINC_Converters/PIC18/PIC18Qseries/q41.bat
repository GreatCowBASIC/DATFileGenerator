goto %1
18F_PIC_to_INCv4 PIC18F04Q41  >  D:\DAT\incfiles\OrgFiles\p18F04Q41.inc
18F_PIC_to_INCv4 PIC18F05Q41  >  D:\DAT\incfiles\OrgFiles\p18F05Q41.inc
18F_PIC_to_INCv4 PIC18F06Q41  >  D:\DAT\incfiles\OrgFiles\p18F06Q41.inc
18F_PIC_to_INCv4 PIC18F14Q41  >  D:\DAT\incfiles\OrgFiles\p18F14Q41.inc
18F_PIC_to_INCv4 PIC18F15Q41  >  D:\DAT\incfiles\OrgFiles\p18F15Q41.inc
18F_PIC_to_INCv4 PIC18F16Q41  >  D:\DAT\incfiles\OrgFiles\p18F16Q41.inc

gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F04Q41.inc > incfiles\p18F04Q41.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F04Q41.inc > incfiles\p18F05Q41.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F05Q41.inc > incfiles\p18F06Q41.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F14Q41.inc > incfiles\p18F14Q41.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F15Q41.inc > incfiles\p18F15Q41.inc
gawk -f preprocessIncFile.awk incfiles\OrgFiles\p18F16Q41.inc > incfiles\p18F16Q41.inc


:skip

getchipdata 18F04Q41  
getchipdata 18F05Q41  
getchipdata 18F06Q41  
getchipdata 18F14Q41  
getchipdata 18F15Q41  
getchipdata 18F16Q41  

