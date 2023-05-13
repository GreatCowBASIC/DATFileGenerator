        LIST
;==========================================================================
;
;  Built by Great Cow BASIC convertor
;  MPASM processor include for the chip shown below
;
; C:\Program Files\Microchip\MPLABX\v5.50\packs\index.idx
; C:\Program Files\Microchip\MPLABX\v5.50\packs\Microchip\PIC18F-Q_DFP\1.11.185\xc8\pic\dat\ini\18F26Q83.ini
; C:\Program Files\Microchip\MPLABX\v5.50\packs\Microchip\PIC18F-Q_DFP\1.11.185\edc\18F26Q83.PIC
; C:\Program Files\Microchip\MPLABX\v5.50\packs\Microchip\PIC18F-Q_DFP\1.11.185\xc8\pic\dat\cfgdata\18F26Q83.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __PIC18F26Q83
           MESSG "Processor-header file mismatch.  Verify selected processor."
        ENDIF
;==========================================================================
;       18xxxx Family        EQUates
;==========================================================================
FSR0             EQU  0
FSR1             EQU  1
FSR2             EQU  2
FAST             EQU  1
W                EQU  0
A                EQU  0
ACCESS           EQU  0
BANKED           EQU  1
;==========================================================================
;
;       Register Definitions
;
;==========================================================================

;----- Register Files -----------------------------------------------------
BOOTREG          EQU  H'0038'
CLKRCON          EQU  H'0039'
CLKRCLK          EQU  H'003A'
NVMCON0          EQU  H'0040'
NVMCON1          EQU  H'0041'
NVMLOCK          EQU  H'0042'
NVMADR           EQU  H'0043'
NVMADRL          EQU  H'0043'
NVMADRH          EQU  H'0044'
NVMADRU          EQU  H'0045'
NVMDAT           EQU  H'0046'
NVMDATL          EQU  H'0046'
NVMDATH          EQU  H'0047'
VREGCON          EQU  H'0048'
BORCON           EQU  H'0049'
HLVDCON0         EQU  H'004A'
HLVDCON1         EQU  H'004B'
ZCDCON           EQU  H'004C'
PMD0             EQU  H'0060'
PMD1             EQU  H'0061'
PMD2             EQU  H'0062'
PMD3             EQU  H'0063'
PMD4             EQU  H'0064'
PMD5             EQU  H'0065'
PMD6             EQU  H'0066'
PMD7             EQU  H'0067'
PMD8             EQU  H'0068'
MD1CON0          EQU  H'006A'
MD1CON1          EQU  H'006B'
MD1SRC           EQU  H'006C'
MD1CARL          EQU  H'006D'
MD1CARH          EQU  H'006E'
CMOUT            EQU  H'006F'
CM1CON0          EQU  H'0070'
CM1CON1          EQU  H'0071'
CM1NCH           EQU  H'0072'
CM1PCH           EQU  H'0073'
CM2CON0          EQU  H'0074'
CM2CON1          EQU  H'0075'
CM2NCH           EQU  H'0076'
CM2PCH           EQU  H'0077'
WDTCON0          EQU  H'0078'
WDTCON1          EQU  H'0079'
WDTPSL           EQU  H'007A'
WDTPSH           EQU  H'007B'
WDTTMR           EQU  H'007C'
DAC1DAT          EQU  H'007D'
DAC1DATL         EQU  H'007D'
DAC1CON          EQU  H'007F'
SPI1RXB          EQU  H'0080'
SPI1TXB          EQU  H'0081'
SPI1TCNT         EQU  H'0082'
SPI1TCNTL        EQU  H'0082'
SPI1TCNTH        EQU  H'0083'
SPI1CON0         EQU  H'0084'
SPI1CON1         EQU  H'0085'
SPI1CON2         EQU  H'0086'
SPI1STATUS       EQU  H'0087'
SPI1TWIDTH       EQU  H'0088'
SPI1BAUD         EQU  H'0089'
SPI1INTF         EQU  H'008A'
SPI1INTE         EQU  H'008B'
SPI1CLK          EQU  H'008C'
SPI2RXB          EQU  H'008D'
SPI2TXB          EQU  H'008E'
SPI2TCNT         EQU  H'008F'
SPI2TCNTL        EQU  H'008F'
SPI2TCNTH        EQU  H'0090'
SPI2CON0         EQU  H'0091'
SPI2CON1         EQU  H'0092'
SPI2CON2         EQU  H'0093'
SPI2STATUS       EQU  H'0094'
SPI2TWIDTH       EQU  H'0095'
SPI2BAUD         EQU  H'0096'
SPI2INTF         EQU  H'0097'
SPI2INTE         EQU  H'0098'
SPI2CLK          EQU  H'0099'
ACTCON           EQU  H'00AC'
OSCCON1          EQU  H'00AD'
OSCCON2          EQU  H'00AE'
OSCCON3          EQU  H'00AF'
OSCTUNE          EQU  H'00B0'
OSCFRQ           EQU  H'00B1'
OSCFREQ          EQU  H'00B1'
OSCSTAT          EQU  H'00B2'
OSCSTAT1         EQU  H'00B2'
OSCEN            EQU  H'00B3'
PRLOCK           EQU  H'00B4'
SCANPR           EQU  H'00B5'
DMA1PR           EQU  H'00B6'
DMA2PR           EQU  H'00B7'
DMA3PR           EQU  H'00B8'
DMA4PR           EQU  H'00B9'
DMA5PR           EQU  H'00BA'
DMA6PR           EQU  H'00BB'
DMA7PR           EQU  H'00BC'
DMA8PR           EQU  H'00BD'
MAINPR           EQU  H'00BE'
ISRPR            EQU  H'00BF'
CLCDATA          EQU  H'00D4'
CLCSELECT        EQU  H'00D5'
CLCNCON          EQU  H'00D6'
CLCNPOL          EQU  H'00D7'
CLCNSEL0         EQU  H'00D8'
CLCNSEL1         EQU  H'00D9'
CLCNSEL2         EQU  H'00DA'
CLCNSEL3         EQU  H'00DB'
CLCNGLS0         EQU  H'00DC'
CLCNGLS1         EQU  H'00DD'
CLCNGLS2         EQU  H'00DE'
CLCNGLS3         EQU  H'00DF'
DMASELECT        EQU  H'00E8'
DMANBUF          EQU  H'00E9'
DMANDCNT         EQU  H'00EA'
DMANDCNTL        EQU  H'00EA'
DMANDCNTH        EQU  H'00EB'
DMANDPTR         EQU  H'00EC'
DMANDPTRL        EQU  H'00EC'
DMANDPTRH        EQU  H'00ED'
DMANDSZ          EQU  H'00EE'
DMANDSZL         EQU  H'00EE'
DMANDSZH         EQU  H'00EF'
DMANDSA          EQU  H'00F0'
DMANDSAL         EQU  H'00F0'
DMANDSAH         EQU  H'00F1'
DMANSCNT         EQU  H'00F2'
DMANSCNTL        EQU  H'00F2'
DMANSCNTH        EQU  H'00F3'
DMANSPTR         EQU  H'00F4'
DMANSPTRL        EQU  H'00F4'
DMANSPTRH        EQU  H'00F5'
DMANSPTRU        EQU  H'00F6'
DMANSSZ          EQU  H'00F7'
DMANSSZL         EQU  H'00F7'
DMANSSZH         EQU  H'00F8'
DMANSSA          EQU  H'00F9'
DMANSSAL         EQU  H'00F9'
DMANSSAH         EQU  H'00FA'
DMANSSAU         EQU  H'00FB'
DMANCON0         EQU  H'00FC'
DMANCON1         EQU  H'00FD'
DMANAIRQ         EQU  H'00FE'
DMANSIRQ         EQU  H'00FF'
C1CONL           EQU  H'0100'
C1CONH           EQU  H'0101'
C1CONU           EQU  H'0102'
C1CONT           EQU  H'0103'
C1NBTCFGL        EQU  H'0104'
C1NBTCFGH        EQU  H'0105'
C1NBTCFGU        EQU  H'0106'
C1NBTCFGT        EQU  H'0107'
C1DBTCFGL        EQU  H'0108'
C1DBTCFGH        EQU  H'0109'
C1DBTCFGU        EQU  H'010A'
C1DBTCFGT        EQU  H'010B'
C1TDCL           EQU  H'010C'
C1TDCH           EQU  H'010D'
C1TDCU           EQU  H'010E'
C1TBC            EQU  H'0110'
C1TBCL           EQU  H'0110'
C1TBCH           EQU  H'0111'
C1TBCU           EQU  H'0112'
C1TBCT           EQU  H'0113'
C1TSCONL         EQU  H'0114'
C1TSCONH         EQU  H'0115'
C1TSCONU         EQU  H'0116'
C1TSCONT         EQU  H'0117'
C1VECL           EQU  H'0118'
C1VECH           EQU  H'0119'
C1VECU           EQU  H'011A'
C1VECT           EQU  H'011B'
C1INTL           EQU  H'011C'
C1INTH           EQU  H'011D'
C1INTU           EQU  H'011E'
C1INTT           EQU  H'011F'
C1RXIF           EQU  H'0120'
C1RXIFL          EQU  H'0120'
C1RXIFH          EQU  H'0121'
C1RXIFU          EQU  H'0122'
C1RXIFT          EQU  H'0123'
C1TXIF           EQU  H'0124'
C1TXIFL          EQU  H'0124'
C1TXIFH          EQU  H'0125'
C1TXIFU          EQU  H'0126'
C1TXIFT          EQU  H'0127'
C1RXOVIF         EQU  H'0128'
C1RXOVIFL        EQU  H'0128'
C1RXOVIFH        EQU  H'0129'
C1RXOVIFU        EQU  H'012A'
C1RXOVIFT        EQU  H'012B'
C1TXATIF         EQU  H'012C'
C1TXATIFL        EQU  H'012C'
C1TXATIFH        EQU  H'012D'
C1TXATIFU        EQU  H'012E'
C1TXATIFT        EQU  H'012F'
C1TXREQ          EQU  H'0130'
C1TXREQL         EQU  H'0130'
C1TXREQH         EQU  H'0131'
C1TXREQU         EQU  H'0132'
C1TXREQT         EQU  H'0133'
C1TRECL          EQU  H'0134'
C1TRECH          EQU  H'0135'
C1TRECU          EQU  H'0136'
C1TRECT          EQU  H'0137'
C1BDIAG0L        EQU  H'0138'
C1BDIAG0H        EQU  H'0139'
C1BDIAG1L        EQU  H'013C'
C1BDIAG1H        EQU  H'013D'
C1BDIAG1U        EQU  H'013E'
C1BDIAG1T        EQU  H'013F'
C1TEFCONL        EQU  H'0140'
C1TEFCONH        EQU  H'0141'
C1TEFCONU        EQU  H'0142'
C1TEFCONT        EQU  H'0143'
C1TEFSTAL        EQU  H'0144'
C1TEFSTAH        EQU  H'0145'
C1TEFSTAU        EQU  H'0146'
C1TEFSTAT        EQU  H'0147'
C1TEFUA          EQU  H'0148'
C1TEFUAL         EQU  H'0148'
C1TEFUAH         EQU  H'0149'
C1TEFUAU         EQU  H'014A'
C1TEFUAT         EQU  H'014B'
C1FIFOBA         EQU  H'014C'
C1FIFOBAL        EQU  H'014C'
C1FIFOBAH        EQU  H'014D'
C1FIFOBAU        EQU  H'014E'
C1FIFOBAT        EQU  H'014F'
C1TXQCONL        EQU  H'0150'
C1TXQCONH        EQU  H'0151'
C1TXQCONU        EQU  H'0152'
C1TXQCONT        EQU  H'0153'
C1TXQSTAL        EQU  H'0154'
C1TXQSTAH        EQU  H'0155'
C1TXQSTAU        EQU  H'0156'
C1TXQSTAT        EQU  H'0157'
C1TXQUA          EQU  H'0158'
C1TXQUAL         EQU  H'0158'
C1TXQUAH         EQU  H'0159'
C1TXQUAU         EQU  H'015A'
C1TXQUAT         EQU  H'015B'
C1FIFOCON1       EQU  H'015C'
C1FIFOCON1L      EQU  H'015C'
C1FIFOCON1H      EQU  H'015D'
C1FIFOCON1U      EQU  H'015E'
C1FIFOCON1T      EQU  H'015F'
C1FIFOSTA1       EQU  H'0160'
C1FIFOSTA1L      EQU  H'0160'
C1FIFOSTA1H      EQU  H'0161'
C1FIFOSTA1U      EQU  H'0162'
C1FIFOSTA1T      EQU  H'0163'
C1FIFOUA1        EQU  H'0164'
C1FIFOUA1L       EQU  H'0164'
C1FIFOUA1H       EQU  H'0165'
C1FIFOUA1U       EQU  H'0166'
C1FIFOUA1T       EQU  H'0167'
C1FIFOCON2       EQU  H'0168'
C1FIFOCON2L      EQU  H'0168'
C1FIFOCON2H      EQU  H'0169'
C1FIFOCON2U      EQU  H'016A'
C1FIFOCON2T      EQU  H'016B'
C1FIFOSTA2       EQU  H'016C'
C1FIFOSTA2L      EQU  H'016C'
C1FIFOSTA2H      EQU  H'016D'
C1FIFOSTA2U      EQU  H'016E'
C1FIFOSTA2T      EQU  H'016F'
C1FIFOUA2        EQU  H'0170'
C1FIFOUA2L       EQU  H'0170'
C1FIFOUA2H       EQU  H'0171'
C1FIFOUA2U       EQU  H'0172'
C1FIFOUA2T       EQU  H'0173'
C1FIFOCON3       EQU  H'0174'
C1FIFOCON3L      EQU  H'0174'
C1FIFOCON3H      EQU  H'0175'
C1FIFOCON3U      EQU  H'0176'
C1FIFOCON3T      EQU  H'0177'
C1FIFOSTA3       EQU  H'0178'
C1FIFOSTA3L      EQU  H'0178'
C1FIFOSTA3H      EQU  H'0179'
C1FIFOSTA3U      EQU  H'017A'
C1FIFOSTA3T      EQU  H'017B'
C1FIFOUA3        EQU  H'017C'
C1FIFOUA3L       EQU  H'017C'
C1FIFOUA3H       EQU  H'017D'
C1FIFOUA3U       EQU  H'017E'
C1FIFOUA3T       EQU  H'017F'
C1FLTCON0L       EQU  H'0180'
C1FLTCON0H       EQU  H'0181'
C1FLTCON0U       EQU  H'0182'
C1FLTCON0T       EQU  H'0183'
C1FLTCON1L       EQU  H'0184'
C1FLTCON1H       EQU  H'0185'
C1FLTCON1U       EQU  H'0186'
C1FLTCON1T       EQU  H'0187'
C1FLTCON2L       EQU  H'0188'
C1FLTCON2H       EQU  H'0189'
C1FLTCON2U       EQU  H'018A'
C1FLTCON2T       EQU  H'018B'
C1FLTOBJ0        EQU  H'018C'
C1FLTOBJ0L       EQU  H'018C'
C1FLTOBJ0H       EQU  H'018D'
C1FLTOBJ0U       EQU  H'018E'
C1FLTOBJ0T       EQU  H'018F'
C1MASK0          EQU  H'0190'
C1MASK0L         EQU  H'0190'
C1MASK0H         EQU  H'0191'
C1MASK0U         EQU  H'0192'
C1MASK0T         EQU  H'0193'
C1FLTOBJ1        EQU  H'0194'
C1FLTOBJ1L       EQU  H'0194'
C1FLTOBJ1H       EQU  H'0195'
C1FLTOBJ1U       EQU  H'0196'
C1FLTOBJ1T       EQU  H'0197'
C1MASK1          EQU  H'0198'
C1MASK1L         EQU  H'0198'
C1MASK1H         EQU  H'0199'
C1MASK1U         EQU  H'019A'
C1MASK1T         EQU  H'019B'
C1FLTOBJ2        EQU  H'019C'
C1FLTOBJ2L       EQU  H'019C'
C1FLTOBJ2H       EQU  H'019D'
C1FLTOBJ2U       EQU  H'019E'
C1FLTOBJ2T       EQU  H'019F'
C1MASK2          EQU  H'01A0'
C1MASK2L         EQU  H'01A0'
C1MASK2H         EQU  H'01A1'
C1MASK2U         EQU  H'01A2'
C1MASK2T         EQU  H'01A3'
C1FLTOBJ3        EQU  H'01A4'
C1FLTOBJ3L       EQU  H'01A4'
C1FLTOBJ3H       EQU  H'01A5'
C1FLTOBJ3U       EQU  H'01A6'
C1FLTOBJ3T       EQU  H'01A7'
C1MASK3          EQU  H'01A8'
C1MASK3L         EQU  H'01A8'
C1MASK3H         EQU  H'01A9'
C1MASK3U         EQU  H'01AA'
C1MASK3T         EQU  H'01AB'
C1FLTOBJ4        EQU  H'01AC'
C1FLTOBJ4L       EQU  H'01AC'
C1FLTOBJ4H       EQU  H'01AD'
C1FLTOBJ4U       EQU  H'01AE'
C1FLTOBJ4T       EQU  H'01AF'
C1MASK4          EQU  H'01B0'
C1MASK4L         EQU  H'01B0'
C1MASK4H         EQU  H'01B1'
C1MASK4U         EQU  H'01B2'
C1MASK4T         EQU  H'01B3'
C1FLTOBJ5        EQU  H'01B4'
C1FLTOBJ5L       EQU  H'01B4'
C1FLTOBJ5H       EQU  H'01B5'
C1FLTOBJ5U       EQU  H'01B6'
C1FLTOBJ5T       EQU  H'01B7'
C1MASK5          EQU  H'01B8'
C1MASK5L         EQU  H'01B8'
C1MASK5H         EQU  H'01B9'
C1MASK5U         EQU  H'01BA'
C1MASK5T         EQU  H'01BB'
C1FLTOBJ6        EQU  H'01BC'
C1FLTOBJ6L       EQU  H'01BC'
C1FLTOBJ6H       EQU  H'01BD'
C1FLTOBJ6U       EQU  H'01BE'
C1FLTOBJ6T       EQU  H'01BF'
C1MASK6          EQU  H'01C0'
C1MASK6L         EQU  H'01C0'
C1MASK6H         EQU  H'01C1'
C1MASK6U         EQU  H'01C2'
C1MASK6T         EQU  H'01C3'
C1FLTOBJ7        EQU  H'01C4'
C1FLTOBJ7L       EQU  H'01C4'
C1FLTOBJ7H       EQU  H'01C5'
C1FLTOBJ7U       EQU  H'01C6'
C1FLTOBJ7T       EQU  H'01C7'
C1MASK7          EQU  H'01C8'
C1MASK7L         EQU  H'01C8'
C1MASK7H         EQU  H'01C9'
C1MASK7U         EQU  H'01CA'
C1MASK7T         EQU  H'01CB'
C1FLTOBJ8        EQU  H'01CC'
C1FLTOBJ8L       EQU  H'01CC'
C1FLTOBJ8H       EQU  H'01CD'
C1FLTOBJ8U       EQU  H'01CE'
C1FLTOBJ8T       EQU  H'01CF'
C1MASK8          EQU  H'01D0'
C1MASK8L         EQU  H'01D0'
C1MASK8H         EQU  H'01D1'
C1MASK8U         EQU  H'01D2'
C1MASK8T         EQU  H'01D3'
C1FLTOBJ9        EQU  H'01D4'
C1FLTOBJ9L       EQU  H'01D4'
C1FLTOBJ9H       EQU  H'01D5'
C1FLTOBJ9U       EQU  H'01D6'
C1FLTOBJ9T       EQU  H'01D7'
C1MASK9          EQU  H'01D8'
C1MASK9L         EQU  H'01D8'
C1MASK9H         EQU  H'01D9'
C1MASK9U         EQU  H'01DA'
C1MASK9T         EQU  H'01DB'
C1FLTOBJ10       EQU  H'01DC'
C1FLTOBJ10L      EQU  H'01DC'
C1FLTOBJ10H      EQU  H'01DD'
C1FLTOBJ10U      EQU  H'01DE'
C1FLTOBJ10T      EQU  H'01DF'
C1MASK10         EQU  H'01E0'
C1MASK10L        EQU  H'01E0'
C1MASK10H        EQU  H'01E1'
C1MASK10U        EQU  H'01E2'
C1MASK10T        EQU  H'01E3'
C1FLTOBJ11       EQU  H'01E4'
C1FLTOBJ11L      EQU  H'01E4'
C1FLTOBJ11H      EQU  H'01E5'
C1FLTOBJ11U      EQU  H'01E6'
C1FLTOBJ11T      EQU  H'01E7'
C1MASK11         EQU  H'01E8'
C1MASK11L        EQU  H'01E8'
C1MASK11H        EQU  H'01E9'
C1MASK11U        EQU  H'01EA'
C1MASK11T        EQU  H'01EB'
PPSLOCK          EQU  H'0200'
RA0PPS           EQU  H'0201'
RA1PPS           EQU  H'0202'
RA2PPS           EQU  H'0203'
RA3PPS           EQU  H'0204'
RA4PPS           EQU  H'0205'
RA5PPS           EQU  H'0206'
RA6PPS           EQU  H'0207'
RA7PPS           EQU  H'0208'
RB0PPS           EQU  H'0209'
RB1PPS           EQU  H'020A'
RB2PPS           EQU  H'020B'
RB3PPS           EQU  H'020C'
RB4PPS           EQU  H'020D'
RB5PPS           EQU  H'020E'
RB6PPS           EQU  H'020F'
RB7PPS           EQU  H'0210'
RC0PPS           EQU  H'0211'
RC1PPS           EQU  H'0212'
RC2PPS           EQU  H'0213'
RC3PPS           EQU  H'0214'
RC4PPS           EQU  H'0215'
RC5PPS           EQU  H'0216'
RC6PPS           EQU  H'0217'
RC7PPS           EQU  H'0218'
CANRXPPS         EQU  H'023D'
INT0PPS          EQU  H'023E'
INT1PPS          EQU  H'023F'
INT2PPS          EQU  H'0240'
T0CKIPPS         EQU  H'0241'
T1CKIPPS         EQU  H'0242'
T1GPPS           EQU  H'0243'
T3CKIPPS         EQU  H'0244'
T3GPPS           EQU  H'0245'
T5CKIPPS         EQU  H'0246'
T5GPPS           EQU  H'0247'
T2INPPS          EQU  H'0248'
T4INPPS          EQU  H'0249'
T6INPPS          EQU  H'024A'
TUIN0PPS         EQU  H'024B'
TUIN1PPS         EQU  H'024C'
TUIN2PPS         EQU  H'024D'
TUIN3PPS         EQU  H'024E'
CCP1PPS          EQU  H'024F'
CCP2PPS          EQU  H'0250'
CCP3PPS          EQU  H'0251'
PWM1ERSPPS       EQU  H'0253'
PWM2ERSPPS       EQU  H'0254'
PWM3ERSPPS       EQU  H'0255'
PWM4ERSPPS       EQU  H'0256'
PWMIN0PPS        EQU  H'0257'
PWMIN1PPS        EQU  H'0258'
SMT1WINPPS       EQU  H'0259'
SMT1SIGPPS       EQU  H'025A'
CWG1PPS          EQU  H'025B'
CWG1INPPS        EQU  H'025B'
CWG2PPS          EQU  H'025C'
CWG2INPPS        EQU  H'025C'
CWG3PPS          EQU  H'025D'
CWG3INPPS        EQU  H'025D'
MD1CARLPPS       EQU  H'025E'
MDCARLPPS        EQU  H'025E'
MD1CARHPPS       EQU  H'025F'
MDCARHPPS        EQU  H'025F'
MD1SRCPPS        EQU  H'0260'
MDSRCPPS         EQU  H'0260'
CLCIN0PPS        EQU  H'0261'
CLCIN1PPS        EQU  H'0262'
CLCIN2PPS        EQU  H'0263'
CLCIN3PPS        EQU  H'0264'
CLCIN4PPS        EQU  H'0265'
CLCIN5PPS        EQU  H'0266'
CLCIN6PPS        EQU  H'0267'
CLCIN7PPS        EQU  H'0268'
ADACTPPS         EQU  H'0269'
SPI1SCKPPS       EQU  H'026A'
SPI1SDIPPS       EQU  H'026B'
SPI1SSPPS        EQU  H'026C'
SPI2SCKPPS       EQU  H'026D'
SPI2SDIPPS       EQU  H'026E'
SPI2SSPPS        EQU  H'026F'
I2C1SDAPPS       EQU  H'0270'
I2C1SCLPPS       EQU  H'0271'
U1RXPPS          EQU  H'0272'
U1CTSPPS         EQU  H'0273'
U2RXPPS          EQU  H'0274'
U2CTSPPS         EQU  H'0275'
U3RXPPS          EQU  H'0276'
U3CTSPPS         EQU  H'0277'
U4RXPPS          EQU  H'0278'
U4CTSPPS         EQU  H'0279'
U5RXPPS          EQU  H'027A'
U5CTSPPS         EQU  H'027B'
RC4I2C           EQU  H'0286'
RC3I2C           EQU  H'0287'
RB2I2C           EQU  H'0288'
RB1I2C           EQU  H'0289'
I2C1RXB          EQU  H'028A'
I2C1TXB          EQU  H'028B'
I2C1CNTL         EQU  H'028C'
I2C1CNTH         EQU  H'028D'
I2C1ADB0         EQU  H'028E'
I2C1ADB1         EQU  H'028F'
I2C1ADR0         EQU  H'0290'
I2C1ADR1         EQU  H'0291'
I2C1ADR2         EQU  H'0292'
I2C1ADR3         EQU  H'0293'
I2C1CON0         EQU  H'0294'
I2C1CON1         EQU  H'0295'
I2C1CON2         EQU  H'0296'
I2C1ERR          EQU  H'0297'
I2C1STAT0        EQU  H'0298'
I2C1STAT1        EQU  H'0299'
I2C1PIR          EQU  H'029A'
I2C1PIE          EQU  H'029B'
I2C1BTO          EQU  H'029C'
I2C1BAUD         EQU  H'029D'
I2C1CLK          EQU  H'029E'
I2C1BTOC         EQU  H'029F'
U1RXB            EQU  H'02A1'
U1RXBL           EQU  H'02A1'
U1RXCHK          EQU  H'02A2'
U1TXB            EQU  H'02A3'
U1TXBL           EQU  H'02A3'
U1TXCHK          EQU  H'02A4'
U1P1             EQU  H'02A5'
U1P1L            EQU  H'02A5'
U1P1H            EQU  H'02A6'
U1P2             EQU  H'02A7'
U1P2L            EQU  H'02A7'
U1P2H            EQU  H'02A8'
U1P3             EQU  H'02A9'
U1P3L            EQU  H'02A9'
U1P3H            EQU  H'02AA'
U1CON0           EQU  H'02AB'
U1CON1           EQU  H'02AC'
U1CON2           EQU  H'02AD'
U1BRG            EQU  H'02AE'
U1BRGL           EQU  H'02AE'
U1BRGH           EQU  H'02AF'
U1FIFO           EQU  H'02B0'
U1UIR            EQU  H'02B1'
U1ERRIR          EQU  H'02B2'
U1ERRIE          EQU  H'02B3'
U2RXB            EQU  H'02B4'
U2RXBL           EQU  H'02B4'
U2RXCHK          EQU  H'02B5'
U2TXB            EQU  H'02B6'
U2TXBL           EQU  H'02B6'
U2TXCHK          EQU  H'02B7'
U2P1             EQU  H'02B8'
U2P1L            EQU  H'02B8'
U2P1H            EQU  H'02B9'
U2P2             EQU  H'02BA'
U2P2L            EQU  H'02BA'
U2P2H            EQU  H'02BB'
U2P3             EQU  H'02BC'
U2P3L            EQU  H'02BC'
U2P3H            EQU  H'02BD'
U2CON0           EQU  H'02BE'
U2CON1           EQU  H'02BF'
U2CON2           EQU  H'02C0'
U2BRG            EQU  H'02C1'
U2BRGL           EQU  H'02C1'
U2BRGH           EQU  H'02C2'
U2FIFO           EQU  H'02C3'
U2UIR            EQU  H'02C4'
U2ERRIR          EQU  H'02C5'
U2ERRIE          EQU  H'02C6'
U3RXB            EQU  H'02C7'
U3RXBL           EQU  H'02C7'
U3TXB            EQU  H'02C9'
U3TXBL           EQU  H'02C9'
U3P1             EQU  H'02CB'
U3P1L            EQU  H'02CB'
U3P2             EQU  H'02CD'
U3P2L            EQU  H'02CD'
U3P3             EQU  H'02CF'
U3P3L            EQU  H'02CF'
U3CON0           EQU  H'02D1'
U3CON1           EQU  H'02D2'
U3CON2           EQU  H'02D3'
U3BRG            EQU  H'02D4'
U3BRGL           EQU  H'02D4'
U3BRGH           EQU  H'02D5'
U3FIFO           EQU  H'02D6'
U3UIR            EQU  H'02D7'
U3ERRIR          EQU  H'02D8'
U3ERRIE          EQU  H'02D9'
U4RXB            EQU  H'02DA'
U4RXBL           EQU  H'02DA'
U4TXB            EQU  H'02DC'
U4TXBL           EQU  H'02DC'
U4P1             EQU  H'02DE'
U4P1L            EQU  H'02DE'
U4P2             EQU  H'02E0'
U4P2L            EQU  H'02E0'
U4P3             EQU  H'02E2'
U4P3L            EQU  H'02E2'
U4CON0           EQU  H'02E4'
U4CON1           EQU  H'02E5'
U4CON2           EQU  H'02E6'
U4BRG            EQU  H'02E7'
U4BRGL           EQU  H'02E7'
U4BRGH           EQU  H'02E8'
U4FIFO           EQU  H'02E9'
U4UIR            EQU  H'02EA'
U4ERRIR          EQU  H'02EB'
U4ERRIE          EQU  H'02EC'
U5RXB            EQU  H'02ED'
U5RXBL           EQU  H'02ED'
U5TXB            EQU  H'02EF'
U5TXBL           EQU  H'02EF'
U5P1             EQU  H'02F1'
U5P1L            EQU  H'02F1'
U5P2             EQU  H'02F3'
U5P2L            EQU  H'02F3'
U5P3             EQU  H'02F5'
U5P3L            EQU  H'02F5'
U5CON0           EQU  H'02F7'
U5CON1           EQU  H'02F8'
U5CON2           EQU  H'02F9'
U5BRG            EQU  H'02FA'
U5BRGL           EQU  H'02FA'
U5BRGH           EQU  H'02FB'
U5FIFO           EQU  H'02FC'
U5UIR            EQU  H'02FD'
U5ERRIR          EQU  H'02FE'
U5ERRIE          EQU  H'02FF'
SMT1TMR          EQU  H'0300'
SMT1TMRL         EQU  H'0300'
SMT1TMRH         EQU  H'0301'
SMT1TMRU         EQU  H'0302'
SMT1CPR          EQU  H'0303'
SMT1CPRL         EQU  H'0303'
SMT1CPRH         EQU  H'0304'
SMT1CPRU         EQU  H'0305'
SMT1CPW          EQU  H'0306'
SMT1CPWL         EQU  H'0306'
SMT1CPWH         EQU  H'0307'
SMT1CPWU         EQU  H'0308'
SMT1PR           EQU  H'0309'
SMT1PRL          EQU  H'0309'
SMT1PRH          EQU  H'030A'
SMT1PRU          EQU  H'030B'
SMT1CON0         EQU  H'030C'
SMT1CON1         EQU  H'030D'
SMT1STAT         EQU  H'030E'
SMT1CLK          EQU  H'030F'
SMT1SIG          EQU  H'0310'
SMT1WIN          EQU  H'0311'
TMR0L            EQU  H'0318'
TMR0             EQU  H'0318'
TMR0H            EQU  H'0319'
PR0              EQU  H'0319'
T0CON0           EQU  H'031A'
T0CON1           EQU  H'031B'
TMR1L            EQU  H'031C'
TMR1H            EQU  H'031D'
T1CON            EQU  H'031E'
TMR1CON          EQU  H'031E'
T1GCON           EQU  H'031F'
TMR1GCON         EQU  H'031F'
T1GATE           EQU  H'0320'
TMR1GATE         EQU  H'0320'
T1CLK            EQU  H'0321'
TMR1CLK          EQU  H'0321'
T2TMR            EQU  H'0322'
TMR2             EQU  H'0322'
T2PR             EQU  H'0323'
PR2              EQU  H'0323'
T2CON            EQU  H'0324'
T2HLT            EQU  H'0325'
T2CLKCON         EQU  H'0326'
T2CLK            EQU  H'0326'
T2RST            EQU  H'0327'
TMR3L            EQU  H'0328'
TMR3H            EQU  H'0329'
T3CON            EQU  H'032A'
TMR3CON          EQU  H'032A'
T3GCON           EQU  H'032B'
TMR3GCON         EQU  H'032B'
T3GATE           EQU  H'032C'
TMR3GATE         EQU  H'032C'
T3CLK            EQU  H'032D'
TMR3CLK          EQU  H'032D'
T4TMR            EQU  H'032E'
TMR4             EQU  H'032E'
T4PR             EQU  H'032F'
PR4              EQU  H'032F'
T4CON            EQU  H'0330'
T4HLT            EQU  H'0331'
T4CLKCON         EQU  H'0332'
T4CLK            EQU  H'0332'
T4RST            EQU  H'0333'
TMR5L            EQU  H'0334'
TMR5H            EQU  H'0335'
T5CON            EQU  H'0336'
TMR5CON          EQU  H'0336'
T5GCON           EQU  H'0337'
TMR5GCON         EQU  H'0337'
T5GATE           EQU  H'0338'
TMR5GATE         EQU  H'0338'
T5CLK            EQU  H'0339'
TMR5CLK          EQU  H'0339'
T6TMR            EQU  H'033A'
TMR6             EQU  H'033A'
T6PR             EQU  H'033B'
PR6              EQU  H'033B'
T6CON            EQU  H'033C'
T6HLT            EQU  H'033D'
T6CLKCON         EQU  H'033E'
T6CLK            EQU  H'033E'
T6RST            EQU  H'033F'
CCPR1            EQU  H'0340'
CCPR1L           EQU  H'0340'
CCPR1H           EQU  H'0341'
CCP1CON          EQU  H'0342'
CCP1CAP          EQU  H'0343'
CCPR2            EQU  H'0344'
CCPR2L           EQU  H'0344'
CCPR2H           EQU  H'0345'
CCP2CON          EQU  H'0346'
CCP2CAP          EQU  H'0347'
CCPR3            EQU  H'0348'
CCPR3L           EQU  H'0348'
CCPR3H           EQU  H'0349'
CCP3CON          EQU  H'034A'
CCP3CAP          EQU  H'034B'
CCPTMRS0         EQU  H'034C'
CRCDATA          EQU  H'034F'
CRCDATAL         EQU  H'034F'
CRCDATAH         EQU  H'0350'
CRCDATAU         EQU  H'0351'
CRCDATAT         EQU  H'0352'
CRCOUT           EQU  H'0353'
CRCSHFT          EQU  H'0353'
CRCXOR           EQU  H'0353'
CRCOUTL          EQU  H'0353'
CRCSHFTL         EQU  H'0353'
CRCSHIFTL        EQU  H'0353'
CRCXORL          EQU  H'0353'
CRCOUTH          EQU  H'0354'
CRCSHFTH         EQU  H'0354'
CRCSHIFTH        EQU  H'0354'
CRCXORH          EQU  H'0354'
CRCOUTU          EQU  H'0355'
CRCSHFTU         EQU  H'0355'
CRCSHIFTU        EQU  H'0355'
CRCXORU          EQU  H'0355'
CRCOUTT          EQU  H'0356'
CRCSHFTT         EQU  H'0356'
CRCSHIFTT        EQU  H'0356'
CRCXORT          EQU  H'0356'
CRCCON0          EQU  H'0357'
CRCCON1          EQU  H'0358'
CRCCON2          EQU  H'0359'
SCANLADR         EQU  H'035A'
SCANLADRL        EQU  H'035A'
SCANLADRH        EQU  H'035B'
SCANLADRU        EQU  H'035C'
SCANHADR         EQU  H'035D'
SCANHADRL        EQU  H'035D'
SCANHADRH        EQU  H'035E'
SCANHADRU        EQU  H'035F'
SCANCON0         EQU  H'0360'
SCANTRIG         EQU  H'0361'
IPR0             EQU  H'0362'
IPR1             EQU  H'0363'
IPR2             EQU  H'0364'
IPR3             EQU  H'0365'
IPR4             EQU  H'0366'
IPR5             EQU  H'0367'
IPR6             EQU  H'0368'
IPR7             EQU  H'0369'
IPR8             EQU  H'036A'
IPR9             EQU  H'036B'
IPR10            EQU  H'036C'
IPR11            EQU  H'036D'
IPR12            EQU  H'036E'
IPR13            EQU  H'036F'
IPR14            EQU  H'0370'
IPR15            EQU  H'0371'
STATUS_CSHAD     EQU  H'0373'
WREG_CSHAD       EQU  H'0374'
BSR_CSHAD        EQU  H'0375'
SHADCON          EQU  H'0376'
STATUS_SHAD      EQU  H'0377'
WREG_SHAD        EQU  H'0378'
BSR_SHAD         EQU  H'0379'
PCLATH_SHAD      EQU  H'037A'
PCLATU_SHAD      EQU  H'037B'
FSR0SH           EQU  H'037C'
FSR0L_SHAD       EQU  H'037C'
FSR0H_SHAD       EQU  H'037D'
FSR1SH           EQU  H'037E'
FSR1L_SHAD       EQU  H'037E'
FSR1H_SHAD       EQU  H'037F'
FSR2SH           EQU  H'0380'
FSR2L_SHAD       EQU  H'0380'
FSR2H_SHAD       EQU  H'0381'
PRODSH           EQU  H'0382'
PRODL_SHAD       EQU  H'0382'
PRODH_SHAD       EQU  H'0383'
TU16ACON0        EQU  H'0387'
TU16ACON1        EQU  H'0388'
TU16AHLT         EQU  H'0389'
TU16APS          EQU  H'038A'
TU16ATMR         EQU  H'038B'
TU16ACR          EQU  H'038B'
TU16ATMRL        EQU  H'038B'
TU16ACRL         EQU  H'038B'
TU16ATMRH        EQU  H'038C'
TU16ACRH         EQU  H'038C'
TU16APR          EQU  H'038D'
TU16APRL         EQU  H'038D'
TU16APRH         EQU  H'038E'
TU16ACLK         EQU  H'038F'
TU16AERS         EQU  H'0390'
TU16BCON0        EQU  H'0393'
TU16BCON1        EQU  H'0394'
TU16BHLT         EQU  H'0395'
TU16BPS          EQU  H'0396'
TU16BTMR         EQU  H'0397'
TU16BCR          EQU  H'0397'
TU16BTMRL        EQU  H'0397'
TU16BCRL         EQU  H'0397'
TU16BTMRH        EQU  H'0398'
TU16BCRH         EQU  H'0398'
TU16BPR          EQU  H'0399'
TU16BPRL         EQU  H'0399'
TU16BPRH         EQU  H'039A'
TU16BCLK         EQU  H'039B'
TU16BERS         EQU  H'039C'
TUCHAIN          EQU  H'03BB'
CWG1CLK          EQU  H'03BC'
CWG1CLKCON       EQU  H'03BC'
CWG1ISM          EQU  H'03BD'
CWG1DBR          EQU  H'03BE'
CWG1DBF          EQU  H'03BF'
CWG1CON0         EQU  H'03C0'
CWG1CON1         EQU  H'03C1'
CWG1AS0          EQU  H'03C2'
CWG1AS1          EQU  H'03C3'
CWG1STR          EQU  H'03C4'
CWG2CLK          EQU  H'03C5'
CWG2CLKCON       EQU  H'03C5'
CWG2ISM          EQU  H'03C6'
CWG2DBR          EQU  H'03C7'
CWG2DBF          EQU  H'03C8'
CWG2CON0         EQU  H'03C9'
CWG2CON1         EQU  H'03CA'
CWG2AS0          EQU  H'03CB'
CWG2AS1          EQU  H'03CC'
CWG2STR          EQU  H'03CD'
CWG3CLK          EQU  H'03CE'
CWG3CLKCON       EQU  H'03CE'
CWG3ISM          EQU  H'03CF'
CWG3DBR          EQU  H'03D0'
CWG3DBF          EQU  H'03D1'
CWG3CON0         EQU  H'03D2'
CWG3CON1         EQU  H'03D3'
CWG3AS0          EQU  H'03D4'
CWG3AS1          EQU  H'03D5'
CWG3STR          EQU  H'03D6'
FVRCON           EQU  H'03D7'
ADCPCON          EQU  H'03D8'
ADCP             EQU  H'03D8'
ADLTH            EQU  H'03D9'
ADLTHL           EQU  H'03D9'
ADLTHH           EQU  H'03DA'
ADUTH            EQU  H'03DB'
ADUTHL           EQU  H'03DB'
ADUTHH           EQU  H'03DC'
ADERR            EQU  H'03DD'
ADERRL           EQU  H'03DD'
ADERRH           EQU  H'03DE'
ADSTPT           EQU  H'03DF'
ADSTPTL          EQU  H'03DF'
ADSTPTH          EQU  H'03E0'
ADFLTR           EQU  H'03E1'
ADFLTRL          EQU  H'03E1'
ADFLTRH          EQU  H'03E2'
ADACC            EQU  H'03E3'
ADACCL           EQU  H'03E3'
ADACCH           EQU  H'03E4'
ADACCU           EQU  H'03E5'
ADCNT            EQU  H'03E6'
ADRPT            EQU  H'03E7'
ADPREV           EQU  H'03E8'
ADPREVL          EQU  H'03E8'
ADPREVH          EQU  H'03E9'
ADRES            EQU  H'03EA'
ADRESL           EQU  H'03EA'
ADRESH           EQU  H'03EB'
ADPCH            EQU  H'03EC'
ADACQ            EQU  H'03EE'
ADACQL           EQU  H'03EE'
ADACQH           EQU  H'03EF'
ADCAP            EQU  H'03F0'
ADPRE            EQU  H'03F1'
ADPREL           EQU  H'03F1'
ADPREH           EQU  H'03F2'
ADCON0           EQU  H'03F3'
ADCON1           EQU  H'03F4'
ADCON2           EQU  H'03F5'
ADCON3           EQU  H'03F6'
ADSTAT           EQU  H'03F7'
ADREF            EQU  H'03F8'
ADACT            EQU  H'03F9'
ADCLK            EQU  H'03FA'
ADCTX            EQU  H'03FB'
ADCSEL1          EQU  H'03FC'
ADCSEL2          EQU  H'03FD'
ADCSEL3          EQU  H'03FE'
ADCSEL4          EQU  H'03FF'
ANSELA           EQU  H'0400'
WPUA             EQU  H'0401'
ODCONA           EQU  H'0402'
SLRCONA          EQU  H'0403'
INLVLA           EQU  H'0404'
IOCAP            EQU  H'0405'
IOCAN            EQU  H'0406'
IOCAF            EQU  H'0407'
ANSELB           EQU  H'0408'
WPUB             EQU  H'0409'
ODCONB           EQU  H'040A'
SLRCONB          EQU  H'040B'
INLVLB           EQU  H'040C'
IOCBP            EQU  H'040D'
IOCBN            EQU  H'040E'
IOCBF            EQU  H'040F'
ANSELC           EQU  H'0410'
WPUC             EQU  H'0411'
ODCONC           EQU  H'0412'
SLRCONC          EQU  H'0413'
INLVLC           EQU  H'0414'
IOCCP            EQU  H'0415'
IOCCN            EQU  H'0416'
IOCCF            EQU  H'0417'
WPUE             EQU  H'0421'
INLVLE           EQU  H'0424'
IOCEP            EQU  H'0425'
IOCEN            EQU  H'0426'
IOCEF            EQU  H'0427'
NCO1ACC          EQU  H'0440'
NCO1ACCL         EQU  H'0440'
NCO1ACCH         EQU  H'0441'
NCO1ACCU         EQU  H'0442'
NCO1INC          EQU  H'0443'
NCO1INCL         EQU  H'0443'
NCO1INCH         EQU  H'0444'
NCO1INCU         EQU  H'0445'
NCO1CON          EQU  H'0446'
NCO1CLK          EQU  H'0447'
NCO2ACC          EQU  H'0448'
NCO2ACCL         EQU  H'0448'
NCO2ACCH         EQU  H'0449'
NCO2ACCU         EQU  H'044A'
NCO2INC          EQU  H'044B'
NCO2INCL         EQU  H'044B'
NCO2INCH         EQU  H'044C'
NCO2INCU         EQU  H'044D'
NCO2CON          EQU  H'044E'
NCO2CLK          EQU  H'044F'
NCO3ACC          EQU  H'0450'
NCO3ACCL         EQU  H'0450'
NCO3ACCH         EQU  H'0451'
NCO3ACCU         EQU  H'0452'
NCO3INC          EQU  H'0453'
NCO3INCL         EQU  H'0453'
NCO3INCH         EQU  H'0454'
NCO3INCU         EQU  H'0455'
NCO3CON          EQU  H'0456'
NCO3CLK          EQU  H'0457'
FSCMCON          EQU  H'0458'
IVTLOCK          EQU  H'0459'
IVTAD            EQU  H'045A'
IVTADL           EQU  H'045A'
IVTADH           EQU  H'045B'
IVTADU           EQU  H'045C'
IVTBASE          EQU  H'045D'
IVTBASEL         EQU  H'045D'
IVTBASEH         EQU  H'045E'
IVTBASEU         EQU  H'045F'
PWM1ERS          EQU  H'0460'
PWM1CLK          EQU  H'0461'
PWM1LDS          EQU  H'0462'
PWM1PR           EQU  H'0463'
PWM1PRL          EQU  H'0463'
PWM1PRH          EQU  H'0464'
PWM1CPRE         EQU  H'0465'
PWM1PIPOS        EQU  H'0466'
PWM1GIR          EQU  H'0467'
PWM1GIE          EQU  H'0468'
PWM1CON          EQU  H'0469'
PWM1S1CFG        EQU  H'046A'
PWM1S1P1         EQU  H'046B'
PWM1S1P1L        EQU  H'046B'
PWM1S1P1H        EQU  H'046C'
PWM1S1P2         EQU  H'046D'
PWM1S1P2L        EQU  H'046D'
PWM1S1P2H        EQU  H'046E'
PWM2ERS          EQU  H'046F'
PWM2CLK          EQU  H'0470'
PWM2LDS          EQU  H'0471'
PWM2PR           EQU  H'0472'
PWM2PRL          EQU  H'0472'
PWM2PRH          EQU  H'0473'
PWM2CPRE         EQU  H'0474'
PWM2PIPOS        EQU  H'0475'
PWM2GIR          EQU  H'0476'
PWM2GIE          EQU  H'0477'
PWM2CON          EQU  H'0478'
PWM2S1CFG        EQU  H'0479'
PWM2S1P1         EQU  H'047A'
PWM2S1P1L        EQU  H'047A'
PWM2S1P1H        EQU  H'047B'
PWM2S1P2         EQU  H'047C'
PWM2S1P2L        EQU  H'047C'
PWM2S1P2H        EQU  H'047D'
PWM3ERS          EQU  H'047E'
PWM3CLK          EQU  H'047F'
PWM3LDS          EQU  H'0480'
PWM3PR           EQU  H'0481'
PWM3PRL          EQU  H'0481'
PWM3PRH          EQU  H'0482'
PWM3CPRE         EQU  H'0483'
PWM3PIPOS        EQU  H'0484'
PWM3GIR          EQU  H'0485'
PWM3GIE          EQU  H'0486'
PWM3CON          EQU  H'0487'
PWM3S1CFG        EQU  H'0488'
PWM3S1P1         EQU  H'0489'
PWM3S1P1L        EQU  H'0489'
PWM3S1P1H        EQU  H'048A'
PWM3S1P2         EQU  H'048B'
PWM3S1P2L        EQU  H'048B'
PWM3S1P2H        EQU  H'048C'
PWM4ERS          EQU  H'048D'
PWM4CLK          EQU  H'048E'
PWM4LDS          EQU  H'048F'
PWM4PR           EQU  H'0490'
PWM4PRL          EQU  H'0490'
PWM4PRH          EQU  H'0491'
PWM4CPRE         EQU  H'0492'
PWM4PIPOS        EQU  H'0493'
PWM4GIR          EQU  H'0494'
PWM4GIE          EQU  H'0495'
PWM4CON          EQU  H'0496'
PWM4S1CFG        EQU  H'0497'
PWM4S1P1         EQU  H'0498'
PWM4S1P1L        EQU  H'0498'
PWM4S1P1H        EQU  H'0499'
PWM4S1P2         EQU  H'049A'
PWM4S1P2L        EQU  H'049A'
PWM4S1P2H        EQU  H'049B'
PWMLOAD          EQU  H'049C'
PWMEN            EQU  H'049D'
PIE0             EQU  H'049E'
PIE1             EQU  H'049F'
PIE2             EQU  H'04A0'
PIE3             EQU  H'04A1'
PIE4             EQU  H'04A2'
PIE5             EQU  H'04A3'
PIE6             EQU  H'04A4'
PIE7             EQU  H'04A5'
PIE8             EQU  H'04A6'
PIE9             EQU  H'04A7'
PIE10            EQU  H'04A8'
PIE11            EQU  H'04A9'
PIE12            EQU  H'04AA'
PIE13            EQU  H'04AB'
PIE14            EQU  H'04AC'
PIE15            EQU  H'04AD'
PIR0             EQU  H'04AE'
PIR1             EQU  H'04AF'
PIR2             EQU  H'04B0'
PIR3             EQU  H'04B1'
PIR4             EQU  H'04B2'
PIR5             EQU  H'04B3'
PIR6             EQU  H'04B4'
PIR7             EQU  H'04B5'
PIR8             EQU  H'04B6'
PIR9             EQU  H'04B7'
PIR10            EQU  H'04B8'
PIR11            EQU  H'04B9'
PIR12            EQU  H'04BA'
PIR13            EQU  H'04BB'
PIR14            EQU  H'04BC'
PIR15            EQU  H'04BD'
LATA             EQU  H'04BE'
LATB             EQU  H'04BF'
LATC             EQU  H'04C0'
TRISA            EQU  H'04C6'
TRISB            EQU  H'04C7'
TRISC            EQU  H'04C8'
TRISE            EQU  H'04CA'
PORTA            EQU  H'04CE'
PORTB            EQU  H'04CF'
PORTC            EQU  H'04D0'
PORTE            EQU  H'04D2'
INTCON0          EQU  H'04D6'
INTCON1          EQU  H'04D7'
STATUS           EQU  H'04D8'
; SFR=FSR2,4D9,16 is a reserved word
FSR2L            EQU  H'04D9'
FSR2H            EQU  H'04DA'
PLUSW2           EQU  H'04DB'
PREINC2          EQU  H'04DC'
POSTDEC2         EQU  H'04DD'
POSTINC2         EQU  H'04DE'
INDF2            EQU  H'04DF'
BSR              EQU  H'04E0'
; SFR=FSR1,4E1,16 is a reserved word
FSR1L            EQU  H'04E1'
FSR1H            EQU  H'04E2'
PLUSW1           EQU  H'04E3'
PREINC1          EQU  H'04E4'
POSTDEC1         EQU  H'04E5'
POSTINC1         EQU  H'04E6'
INDF1            EQU  H'04E7'
WREG             EQU  H'04E8'
; SFR=W,4E8,8 is a reserved word
; SFR=FSR0,4E9,16 is a reserved word
FSR0L            EQU  H'04E9'
FSR0H            EQU  H'04EA'
PLUSW0           EQU  H'04EB'
PREINC0          EQU  H'04EC'
POSTDEC0         EQU  H'04ED'
POSTINC0         EQU  H'04EE'
INDF0            EQU  H'04EF'
PCON0            EQU  H'04F0'
PCON1            EQU  H'04F1'
CPUDOZE          EQU  H'04F2'
PROD             EQU  H'04F3'
PRODL            EQU  H'04F3'
PRODH            EQU  H'04F4'
TABLAT           EQU  H'04F5'
TBLPTR           EQU  H'04F6'
TBLPTRL          EQU  H'04F6'
TBLPTRH          EQU  H'04F7'
TBLPTRU          EQU  H'04F8'
PCLAT            EQU  H'04F9'
PCL              EQU  H'04F9'
PCLATH           EQU  H'04FA'
PCLATU           EQU  H'04FB'
STKPTR           EQU  H'04FC'
TOS              EQU  H'04FD'
TOSL             EQU  H'04FD'
TOSH             EQU  H'04FE'
TOSU             EQU  H'04FF'


;----- PMD1 Bits -----------------------------------------------------
TMR0MD           EQU  H'0000'
TMR1MD           EQU  H'0001'
TMR2MD           EQU  H'0002'
TMR3MD           EQU  H'0003'
TMR4MD           EQU  H'0004'
TMR5MD           EQU  H'0005'
TMR6MD           EQU  H'0006'
SMT1MD           EQU  H'0007'


;----- CM1CON0 Bits -----------------------------------------------------
SYNC             EQU  H'0000'
HYS              EQU  H'0001'
POL_CM1CON0      EQU  H'0004'
OUT_CM1CON0      EQU  H'0006'
EN               EQU  H'0007'
C1SYNC           EQU  H'0000'
C1HYS            EQU  H'0001'
C1POL            EQU  H'0004'
C1OUT_CM1CON0    EQU  H'0006'
C1EN             EQU  H'0007'


;----- CM1CON1 Bits -----------------------------------------------------
INTN_CM1CON1     EQU  H'0000'
INTP_CM1CON1     EQU  H'0001'
C1INTN           EQU  H'0000'
C1INTP           EQU  H'0001'


;----- CM1NCH Bits -----------------------------------------------------
NCH0_CM1NCH      EQU  H'0000'
NCH1_CM1NCH      EQU  H'0001'
NCH2_CM1NCH      EQU  H'0002'
C1NCH0           EQU  H'0000'
C1NCH1           EQU  H'0001'
C1NCH2           EQU  H'0002'


;----- CM1PCH Bits -----------------------------------------------------
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
C1PCH0           EQU  H'0000'
C1PCH1           EQU  H'0001'
C1PCH2           EQU  H'0002'


;----- SPI1CON1 Bits -----------------------------------------------------
SDOP             EQU  H'0000'
SDIP             EQU  H'0001'
SSP              EQU  H'0002'
FST              EQU  H'0004'
CKP              EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'
SPI1SDOP         EQU  H'0000'
SPI1SDIP         EQU  H'0001'
SPI1SSP          EQU  H'0002'
SPI1FST          EQU  H'0004'
SPI1CKP          EQU  H'0005'
SPI1CKE          EQU  H'0006'
SPI1SMP          EQU  H'0007'


;----- SPI1RXB Bits -----------------------------------------------------
RXB0             EQU  H'0000'
RXB1             EQU  H'0001'
RXB2             EQU  H'0002'
RXB3             EQU  H'0003'
RXB4             EQU  H'0004'
RXB5             EQU  H'0005'
RXB6             EQU  H'0006'
RXB7             EQU  H'0007'


;----- SPI1TXB Bits -----------------------------------------------------
TXB0             EQU  H'0000'
TXB1             EQU  H'0001'
TXB2             EQU  H'0002'
TXB3             EQU  H'0003'
TXB4             EQU  H'0004'
TXB5             EQU  H'0005'
TXB6             EQU  H'0006'
TXB7             EQU  H'0007'


;----- SPI1CON0 Bits -----------------------------------------------------
BMODE            EQU  H'0000'
MST              EQU  H'0001'
LSBF             EQU  H'0002'
EN               EQU  H'0007'
SPI1BMODE        EQU  H'0000'
SPI1MST          EQU  H'0001'
SPI1LSBF         EQU  H'0002'
SPI1SPIEN        EQU  H'0007'


;----- SPI1CON2 Bits -----------------------------------------------------
RXR              EQU  H'0000'
TXR              EQU  H'0001'
SSET             EQU  H'0002'
SSFLT            EQU  H'0006'
BUSY_SPI1CON2    EQU  H'0007'
SPI1RXR          EQU  H'0000'
SPI1TXR          EQU  H'0001'
SPI1SSET         EQU  H'0002'
SPI1SSFLT        EQU  H'0006'
SPI1BUSY         EQU  H'0007'


;----- SPI1STATUS Bits -----------------------------------------------------
RXBF             EQU  H'0000'
CLRBF            EQU  H'0002'
RXRE             EQU  H'0003'
TXBE             EQU  H'0005'
TXWE             EQU  H'0007'
SPI1RXBF         EQU  H'0000'
SPI1CLRBF        EQU  H'0002'
SPI1RXRE         EQU  H'0003'
SPI1TXBE         EQU  H'0005'
SPI1TXWE         EQU  H'0007'


;----- SPI1TWIDTH Bits -----------------------------------------------------
TWIDTH0          EQU  H'0000'
TWIDTH1          EQU  H'0001'
TWIDTH2          EQU  H'0002'


;----- SPI1BAUD Bits -----------------------------------------------------
BAUD0            EQU  H'0000'
BAUD1            EQU  H'0001'
BAUD2            EQU  H'0002'
BAUD3            EQU  H'0003'
BAUD4            EQU  H'0004'
BAUD5            EQU  H'0005'
BAUD6            EQU  H'0006'
BAUD7            EQU  H'0007'


;----- SPI1INTF Bits -----------------------------------------------------
TXUIF            EQU  H'0001'
RXOIF            EQU  H'0002'
EOSIF            EQU  H'0004'
SOSIF            EQU  H'0005'
TCZIF            EQU  H'0006'
SRMTIF           EQU  H'0007'
SPI1TXUIF        EQU  H'0001'
SPI1RXOIF        EQU  H'0002'
SPI1EOSIF        EQU  H'0004'
SPI1SOSIF        EQU  H'0005'
SPI1TCZIF        EQU  H'0006'
SPI1SRMTIF       EQU  H'0007'


;----- SPI1INTE Bits -----------------------------------------------------
TXUIE            EQU  H'0001'
RXOIE            EQU  H'0002'
EOSIE            EQU  H'0004'
SOSIE            EQU  H'0005'
TCZIE            EQU  H'0006'
SRMTIE           EQU  H'0007'
SPI1TXUIE        EQU  H'0001'
SPI1RXOIE        EQU  H'0002'
SPI1EOSIE        EQU  H'0004'
SPI1SOSIE        EQU  H'0005'
SPI1TCZIE        EQU  H'0006'
SPI1SRMTIE       EQU  H'0007'


;----- SPI1CLK Bits -----------------------------------------------------
CLKSEL0          EQU  H'0000'
CLKSEL1          EQU  H'0001'
CLKSEL2          EQU  H'0002'
CLKSEL3          EQU  H'0003'
CLKSEL4          EQU  H'0004'
SPI1CLKSEL0      EQU  H'0000'
SPI1CLKSEL1      EQU  H'0001'
SPI1CLKSEL2      EQU  H'0002'
SPI1CLKSEL3      EQU  H'0003'
SPI1CLKSEL4      EQU  H'0004'


;----- OSCCON1 Bits -----------------------------------------------------
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'
NOSC2            EQU  H'0006'


;----- DMA1PR Bits -----------------------------------------------------
PR0_DMA1PR       EQU  H'0000'
PR1_DMA1PR       EQU  H'0001'
PR2_DMA1PR       EQU  H'0002'
DMA1PR0          EQU  H'0000'
DMA1PR1          EQU  H'0001'
DMA1PR2          EQU  H'0002'


;----- CLCNSEL1 Bits -----------------------------------------------------
D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'
D2S6             EQU  H'0006'
D2S7             EQU  H'0007'


;----- CLCNGLS1 Bits -----------------------------------------------------
G2D1N            EQU  H'0000'
G2D1T            EQU  H'0001'
G2D2N            EQU  H'0002'
G2D2T            EQU  H'0003'
G2D3N            EQU  H'0004'
G2D3T            EQU  H'0005'
G2D4N            EQU  H'0006'
G2D4T            EQU  H'0007'


;----- C1FIFOCON1 Bits -----------------------------------------------------
TFNRFNIE         EQU  H'0000'
TFHRFHIE         EQU  H'0001'
TFERFFIE         EQU  H'0002'
RXOVIE           EQU  H'0003'
TXATIE           EQU  H'0004'
RXTSEN           EQU  H'0005'
RTREN            EQU  H'0006'
TXEN             EQU  H'0007'


;----- C1FIFOCON1H Bits -----------------------------------------------------
UINC             EQU  H'0000'
TXREQ            EQU  H'0001'
FRESET           EQU  H'0002'


;----- C1FIFOCON1U Bits -----------------------------------------------------
TXPRI0           EQU  H'0000'
TXPRI1           EQU  H'0001'
TXPRI2           EQU  H'0002'
TXPRI3           EQU  H'0003'
TXPRI4           EQU  H'0004'
TXAT0            EQU  H'0005'
TXAT1            EQU  H'0006'


;----- C1FIFOCON1T Bits -----------------------------------------------------
FSIZE0           EQU  H'0000'
FSIZE1           EQU  H'0001'
FSIZE2           EQU  H'0002'
FSIZE3           EQU  H'0003'
FSIZE4           EQU  H'0004'


;----- C1FIFOSTA1 Bits -----------------------------------------------------
TFNRFNIF         EQU  H'0000'
TFHRFHIF         EQU  H'0001'
TFERFFIF         EQU  H'0002'
RXOVIF           EQU  H'0003'
TXATIF           EQU  H'0004'
TXERR            EQU  H'0005'
TXLARB           EQU  H'0006'
TXABT            EQU  H'0007'


;----- C1FIFOSTA1H Bits -----------------------------------------------------
FIFOCI0          EQU  H'0000'
FIFOCI1          EQU  H'0001'
FIFOCI2          EQU  H'0002'
FIFOCI3          EQU  H'0003'
FIFOCI4          EQU  H'0004'


;----- C1FIFOUA1 Bits -----------------------------------------------------
FIFOUA0          EQU  H'0000'
FIFOUA1          EQU  H'0001'
FIFOUA2          EQU  H'0002'
FIFOUA3          EQU  H'0003'
FIFOUA4          EQU  H'0004'
FIFOUA5          EQU  H'0005'
FIFOUA6          EQU  H'0006'
FIFOUA7          EQU  H'0007'


;----- C1FIFOUA1H Bits -----------------------------------------------------
FIFOUA8          EQU  H'0000'
FIFOUA9          EQU  H'0001'
FIFOUA10         EQU  H'0002'
FIFOUA11         EQU  H'0003'
FIFOUA12         EQU  H'0004'
FIFOUA13         EQU  H'0005'
FIFOUA14         EQU  H'0006'
FIFOUA15         EQU  H'0007'


;----- C1FIFOUA1U Bits -----------------------------------------------------
FIFOUA16         EQU  H'0000'
FIFOUA17         EQU  H'0001'
FIFOUA18         EQU  H'0002'
FIFOUA19         EQU  H'0003'
FIFOUA20         EQU  H'0004'
FIFOUA21         EQU  H'0005'
FIFOUA22         EQU  H'0006'
FIFOUA23         EQU  H'0007'


;----- C1FIFOUA1T Bits -----------------------------------------------------
FIFOUA24         EQU  H'0000'
FIFOUA25         EQU  H'0001'
FIFOUA26         EQU  H'0002'
FIFOUA27         EQU  H'0003'
FIFOUA28         EQU  H'0004'
FIFOUA29         EQU  H'0005'
FIFOUA30         EQU  H'0006'
FIFOUA31         EQU  H'0007'


;----- C1FLTCON1L Bits -----------------------------------------------------
FLTEN4           EQU  H'0007'
F4BP0            EQU  H'0000'
F4BP1            EQU  H'0001'
F4BP2            EQU  H'0002'
F4BP3            EQU  H'0003'
F4BP4            EQU  H'0004'


;----- C1FLTCON1H Bits -----------------------------------------------------
FLTEN5           EQU  H'0007'
F5BP0            EQU  H'0000'
F5BP1            EQU  H'0001'
F5BP2            EQU  H'0002'
F5BP3            EQU  H'0003'
F5BP4            EQU  H'0004'


;----- C1FLTCON1U Bits -----------------------------------------------------
FLTEN6           EQU  H'0007'
F6BP0            EQU  H'0000'
F6BP1            EQU  H'0001'
F6BP2            EQU  H'0002'
F6BP3            EQU  H'0003'
F6BP4            EQU  H'0004'


;----- C1FLTCON1T Bits -----------------------------------------------------
FLTEN7           EQU  H'0007'
F7BP0            EQU  H'0000'
F7BP1            EQU  H'0001'
F7BP2            EQU  H'0002'
F7BP3            EQU  H'0003'
F7BP4            EQU  H'0004'


;----- C1FLTOBJ1 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ1H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ1U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ1T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK1 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK1H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK1U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK1T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- RA1PPS Bits -----------------------------------------------------
RA1PPS0          EQU  H'0000'
RA1PPS1          EQU  H'0001'
RA1PPS2          EQU  H'0002'
RA1PPS3          EQU  H'0003'
RA1PPS4          EQU  H'0004'
RA1PPS5          EQU  H'0005'


;----- RB1PPS Bits -----------------------------------------------------
RB1PPS0          EQU  H'0000'
RB1PPS1          EQU  H'0001'
RB1PPS2          EQU  H'0002'
RB1PPS3          EQU  H'0003'
RB1PPS4          EQU  H'0004'
RB1PPS5          EQU  H'0005'


;----- RC1PPS Bits -----------------------------------------------------
RC1PPS0          EQU  H'0000'
RC1PPS1          EQU  H'0001'
RC1PPS2          EQU  H'0002'
RC1PPS3          EQU  H'0003'
RC1PPS4          EQU  H'0004'
RC1PPS5          EQU  H'0005'


;----- INT1PPS Bits -----------------------------------------------------
INT1PPS0         EQU  H'0000'
INT1PPS1         EQU  H'0001'
INT1PPS2         EQU  H'0002'
INT1PPS3         EQU  H'0003'
INT1PPS4         EQU  H'0004'


;----- T1CKIPPS Bits -----------------------------------------------------
T1CKIPPS0        EQU  H'0000'
T1CKIPPS1        EQU  H'0001'
T1CKIPPS2        EQU  H'0002'
T1CKIPPS3        EQU  H'0003'
T1CKIPPS4        EQU  H'0004'
T1CKIPPS5        EQU  H'0005'


;----- T1GPPS Bits -----------------------------------------------------
T1GPPS0          EQU  H'0000'
T1GPPS1          EQU  H'0001'
T1GPPS2          EQU  H'0002'
T1GPPS3          EQU  H'0003'
T1GPPS4          EQU  H'0004'


;----- TUIN1PPS Bits -----------------------------------------------------
TUIN1PPS0        EQU  H'0000'
TUIN1PPS1        EQU  H'0001'
TUIN1PPS2        EQU  H'0002'
TUIN1PPS3        EQU  H'0003'
TUIN1PPS4        EQU  H'0004'
TUIN1PPS5        EQU  H'0005'


;----- CCP1PPS Bits -----------------------------------------------------
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'
CCP1PPS5         EQU  H'0005'


;----- PWM1ERSPPS Bits -----------------------------------------------------
PWM1ERSPPS0      EQU  H'0000'
PWM1ERSPPS1      EQU  H'0001'
PWM1ERSPPS2      EQU  H'0002'
PWM1ERSPPS3      EQU  H'0003'
PWM1ERSPPS4      EQU  H'0004'


;----- CWG1PPS Bits -----------------------------------------------------
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'
CWG1INPPS0       EQU  H'0000'
CWG1INPPS1       EQU  H'0001'
CWG1INPPS2       EQU  H'0002'
CWG1INPPS3       EQU  H'0003'
CWG1INPPS4       EQU  H'0004'


;----- CLCIN1PPS Bits -----------------------------------------------------
CLCIN1PPS0       EQU  H'0000'
CLCIN1PPS1       EQU  H'0001'
CLCIN1PPS2       EQU  H'0002'
CLCIN1PPS3       EQU  H'0003'
CLCIN1PPS4       EQU  H'0004'


;----- SPI1SCKPPS Bits -----------------------------------------------------
SPI1SCKPPS0      EQU  H'0000'
SPI1SCKPPS1      EQU  H'0001'
SPI1SCKPPS2      EQU  H'0002'
SPI1SCKPPS3      EQU  H'0003'
SPI1SCKPPS4      EQU  H'0004'


;----- SPI1SDIPPS Bits -----------------------------------------------------
SPI1SDIPPS0      EQU  H'0000'
SPI1SDIPPS1      EQU  H'0001'
SPI1SDIPPS2      EQU  H'0002'
SPI1SDIPPS3      EQU  H'0003'
SPI1SDIPPS4      EQU  H'0004'


;----- SPI1SSPPS Bits -----------------------------------------------------
SPI1SSPPS0       EQU  H'0000'
SPI1SSPPS1       EQU  H'0001'
SPI1SSPPS2       EQU  H'0002'
SPI1SSPPS3       EQU  H'0003'
SPI1SSPPS4       EQU  H'0004'


;----- U1RXPPS Bits -----------------------------------------------------
U1RXPPS0         EQU  H'0000'
U1RXPPS1         EQU  H'0001'
U1RXPPS2         EQU  H'0002'
U1RXPPS3         EQU  H'0003'
U1RXPPS4         EQU  H'0004'
U1RXPPS5         EQU  H'0005'


;----- U1CTSPPS Bits -----------------------------------------------------
U1CTSPPS0        EQU  H'0000'
U1CTSPPS1        EQU  H'0001'
U1CTSPPS2        EQU  H'0002'
U1CTSPPS3        EQU  H'0003'
U1CTSPPS4        EQU  H'0004'
U1CTSPPS5        EQU  H'0005'


;----- RB1I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- U1P1H Bits -----------------------------------------------------
P1H              EQU  H'0000'


;----- U1CON1 Bits -----------------------------------------------------
SENDB            EQU  H'0000'
BRKOVR           EQU  H'0001'
RXBIMD           EQU  H'0003'
WUE              EQU  H'0004'
ON_U1CON1        EQU  H'0007'
U1SENDB          EQU  H'0000'
U1BRKOVR         EQU  H'0001'
U1RXBIMD         EQU  H'0003'
U1WUE            EQU  H'0004'
U1ON             EQU  H'0007'


;----- U1P2H Bits -----------------------------------------------------
P2H              EQU  H'0000'


;----- U1P3H Bits -----------------------------------------------------
P3H              EQU  H'0000'


;----- U1CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
U1RXEN           EQU  H'0004'
U1TXEN           EQU  H'0005'
U1ABDEN          EQU  H'0006'
U1BRGS           EQU  H'0007'
U1MODE0          EQU  H'0000'
U1MODE1          EQU  H'0001'
U1MODE2          EQU  H'0002'
U1MODE3          EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


;----- U1CON2 Bits -----------------------------------------------------
TXPOL            EQU  H'0002'
C0EN             EQU  H'0003'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U1TXPOL          EQU  H'0002'
U1C0EN           EQU  H'0003'
U1RXPOL          EQU  H'0006'
U1RUNOVF         EQU  H'0007'
U1FLO0           EQU  H'0000'
U1FLO1           EQU  H'0001'
U1STP0           EQU  H'0004'
U1STP1           EQU  H'0005'


;----- U1FIFO Bits -----------------------------------------------------
RXBF             EQU  H'0000'
RXBE             EQU  H'0001'
XON              EQU  H'0002'
RXIDL            EQU  H'0003'
TXBF             EQU  H'0004'
TXBE             EQU  H'0005'
STPMD            EQU  H'0006'
TXWRE            EQU  H'0007'
U1RXBF           EQU  H'0000'
U1RXBE           EQU  H'0001'
U1XON            EQU  H'0002'
U1RXIDL          EQU  H'0003'
U1TXBF           EQU  H'0004'
U1TXBE           EQU  H'0005'
U1STPMD          EQU  H'0006'
U1TXWRE          EQU  H'0007'
U1RCIDL          EQU  H'0003'
RCIDL            EQU  H'0003'


;----- U1UIR Bits -----------------------------------------------------
ABDIE            EQU  H'0002'
ABDIF            EQU  H'0006'
WUIF             EQU  H'0007'
U1ABDIE          EQU  H'0002'
U1ABDIF          EQU  H'0006'
U1WUIF           EQU  H'0007'


;----- U1ERRIR Bits -----------------------------------------------------
TXCIF            EQU  H'0000'
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
CERIF            EQU  H'0004'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U1TXCIF          EQU  H'0000'
U1RXFOIF         EQU  H'0001'
U1RXBKIF         EQU  H'0002'
U1FERIF          EQU  H'0003'
U1CERIF          EQU  H'0004'
U1ABDOVF         EQU  H'0005'
U1PERIF          EQU  H'0006'
U1TXMTIF         EQU  H'0007'


;----- U1ERRIE Bits -----------------------------------------------------
TXCIE            EQU  H'0000'
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
CERIE            EQU  H'0004'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U1TXCIE          EQU  H'0000'
U1RXFOIE         EQU  H'0001'
U1RXBKIE         EQU  H'0002'
U1FERIE          EQU  H'0003'
U1CERIE          EQU  H'0004'
U1ABDOVE         EQU  H'0005'
U1PERIE          EQU  H'0006'
U1TXMTIE         EQU  H'0007'


;----- U3CON1 Bits -----------------------------------------------------
SENDB            EQU  H'0000'
BRKOVR           EQU  H'0001'
RXBIMD           EQU  H'0003'
WUE              EQU  H'0004'
ON_U3CON1        EQU  H'0007'
U3SENDB          EQU  H'0000'
U3BRKOVR         EQU  H'0001'
U3RXBIMD         EQU  H'0003'
U3WUE            EQU  H'0004'
U3ON             EQU  H'0007'


;----- U4CON1 Bits -----------------------------------------------------
SENDB            EQU  H'0000'
BRKOVR           EQU  H'0001'
RXBIMD           EQU  H'0003'
WUE              EQU  H'0004'
ON_U4CON1        EQU  H'0007'
U4SENDB          EQU  H'0000'
U4BRKOVR         EQU  H'0001'
U4RXBIMD         EQU  H'0003'
U4WUE            EQU  H'0004'
U4ON             EQU  H'0007'


;----- U5CON1 Bits -----------------------------------------------------
SENDB            EQU  H'0000'
BRKOVR           EQU  H'0001'
RXBIMD           EQU  H'0003'
WUE              EQU  H'0004'
ON_U5CON1        EQU  H'0007'
U5SENDB          EQU  H'0000'
U5BRKOVR         EQU  H'0001'
U5RXBIMD         EQU  H'0003'
U5WUE            EQU  H'0004'
U5ON             EQU  H'0007'


;----- T1CON Bits -----------------------------------------------------
ON_T1CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
TMR1ON           EQU  H'0000'
T1RD16           EQU  H'0001'
NOT_T1SYNC       EQU  H'0002'
T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'
CKPS0_T1CON      EQU  H'0004'
CKPS1_T1CON      EQU  H'0005'
RD161            EQU  H'0001'


;----- TMR1L Bits -----------------------------------------------------
TMR1L0           EQU  H'0000'
TMR1L1           EQU  H'0001'
TMR1L2           EQU  H'0002'
TMR1L3           EQU  H'0003'
TMR1L4           EQU  H'0004'
TMR1L5           EQU  H'0005'
TMR1L6           EQU  H'0006'
TMR1L7           EQU  H'0007'


;----- TMR1H Bits -----------------------------------------------------
TMR1H0           EQU  H'0000'
TMR1H1           EQU  H'0001'
TMR1H2           EQU  H'0002'
TMR1H3           EQU  H'0003'
TMR1H4           EQU  H'0004'
TMR1H5           EQU  H'0005'
TMR1H6           EQU  H'0006'
TMR1H7           EQU  H'0007'


;----- T1GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO              EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL_T1GCON      EQU  H'0006'
GE               EQU  H'0007'
T1GVAL           EQU  H'0002'
T1GGO            EQU  H'0003'
T1GSPM           EQU  H'0004'
T1GTM            EQU  H'0005'
T1GPOL           EQU  H'0006'
T1GE             EQU  H'0007'
NOT_DONE_T1GCON  EQU  H'0003'
NOT_T1DONE       EQU  H'0003'


;----- T1GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
T1GSS0           EQU  H'0000'
T1GSS1           EQU  H'0001'
T1GSS2           EQU  H'0002'
T1GSS3           EQU  H'0003'
T1GSS4           EQU  H'0004'


;----- SMT1CLK Bits -----------------------------------------------------
CSEL0            EQU  H'0000'
CSEL1            EQU  H'0001'
CSEL2            EQU  H'0002'
SMT1CSEL0        EQU  H'0000'
SMT1CSEL1        EQU  H'0001'
SMT1CSEL2        EQU  H'0002'


;----- T1CLK Bits -----------------------------------------------------
T1CS0            EQU  H'0000'
T1CS1            EQU  H'0001'
T1CS2            EQU  H'0002'
T1CS3            EQU  H'0003'
T1CS4            EQU  H'0004'
CS0_T1CLK        EQU  H'0000'
CS1_T1CLK        EQU  H'0001'
CS2_T1CLK        EQU  H'0002'
CS3              EQU  H'0003'
CS4              EQU  H'0004'


;----- CCP1CON Bits -----------------------------------------------------
FMT              EQU  H'0004'
OUT_CCP1CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP1FMT          EQU  H'0004'
CCP1OUT          EQU  H'0005'
CCP1EN           EQU  H'0007'
CCP1MODE0        EQU  H'0000'
CCP1MODE1        EQU  H'0001'
CCP1MODE2        EQU  H'0002'
CCP1MODE3        EQU  H'0003'


;----- CCP1CAP Bits -----------------------------------------------------
CTS0             EQU  H'0000'
CTS1             EQU  H'0001'
CTS2             EQU  H'0002'
CTS3             EQU  H'0003'
CCP1CTS0         EQU  H'0000'
CCP1CTS1         EQU  H'0001'
CCP1CTS2         EQU  H'0002'
CCP1CTS3         EQU  H'0003'


;----- CRCCON1 Bits -----------------------------------------------------
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
PLEN4            EQU  H'0004'


;----- IPR1 Bits -----------------------------------------------------
INT0IP           EQU  H'0000'
ZCDIP            EQU  H'0001'
ADIP             EQU  H'0002'
ACTIP            EQU  H'0003'
C1IP             EQU  H'0004'
SMT1IP           EQU  H'0005'
SMT1PRAIP        EQU  H'0006'
SMT1PWAIP        EQU  H'0007'


;----- IPR11 Bits -----------------------------------------------------
CCP3IP           EQU  H'0000'
CLC6IP           EQU  H'0001'
CWG3IP           EQU  H'0002'
TMR4IP           EQU  H'0003'
DMA4SCNTIP       EQU  H'0004'
DMA4DCNTIP       EQU  H'0005'
DMA4ORIP         EQU  H'0006'
DMA4AIP          EQU  H'0007'


;----- CWG1CLK Bits -----------------------------------------------------
CS_CWG1CLK       EQU  H'0000'
CWG1CS           EQU  H'0000'


;----- CWG1ISM Bits -----------------------------------------------------
CWG1ISM0         EQU  H'0000'
CWG1ISM1         EQU  H'0001'
CWG1ISM2         EQU  H'0002'
CWG1ISM3         EQU  H'0003'


;----- CWG1DBR Bits -----------------------------------------------------
DBR0             EQU  H'0000'
DBR1             EQU  H'0001'
DBR2             EQU  H'0002'
DBR3             EQU  H'0003'
DBR4             EQU  H'0004'
DBR5             EQU  H'0005'
CWG1DBR0         EQU  H'0000'
CWG1DBR1         EQU  H'0001'
CWG1DBR2         EQU  H'0002'
CWG1DBR3         EQU  H'0003'
CWG1DBR4         EQU  H'0004'
CWG1DBR5         EQU  H'0005'


;----- CWG1DBF Bits -----------------------------------------------------
DBF0             EQU  H'0000'
DBF1             EQU  H'0001'
DBF2             EQU  H'0002'
DBF3             EQU  H'0003'
DBF4             EQU  H'0004'
DBF5             EQU  H'0005'
CWG1DBF0         EQU  H'0000'
CWG1DBF1         EQU  H'0001'
CWG1DBF2         EQU  H'0002'
CWG1DBF3         EQU  H'0003'
CWG1DBF4         EQU  H'0004'
CWG1DBF5         EQU  H'0005'


;----- CWG1CON0 Bits -----------------------------------------------------
LD_CWG1CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G1EN             EQU  H'0007'
CWG1LD           EQU  H'0006'
CWG1EN           EQU  H'0007'
CWG1MODE0        EQU  H'0000'
CWG1MODE1        EQU  H'0001'
CWG1MODE2        EQU  H'0002'


;----- CWG1CON1 Bits -----------------------------------------------------
POLA             EQU  H'0000'
POLB             EQU  H'0001'
POLC             EQU  H'0002'
POLD             EQU  H'0003'
IN               EQU  H'0005'
CWG1POLA         EQU  H'0000'
CWG1POLB         EQU  H'0001'
CWG1POLC         EQU  H'0002'
CWG1POLD         EQU  H'0003'
CWG1IN           EQU  H'0005'


;----- CWG1AS0 Bits -----------------------------------------------------
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG1REN          EQU  H'0006'
CWG1SHUTDOWN     EQU  H'0007'
CWG1LSAC0        EQU  H'0002'
CWG1LSAC1        EQU  H'0003'
CWG1LSBD0        EQU  H'0004'
CWG1LSBD1        EQU  H'0005'


;----- CWG1AS1 Bits -----------------------------------------------------
AS0E             EQU  H'0000'
AS1E             EQU  H'0001'
AS2E             EQU  H'0002'
AS3E             EQU  H'0003'
AS4E             EQU  H'0004'
AS5E             EQU  H'0005'
AS6E             EQU  H'0006'
AS7E             EQU  H'0007'


;----- CWG1STR Bits -----------------------------------------------------
STRA             EQU  H'0000'
STRB             EQU  H'0001'
STRC             EQU  H'0002'
STRD             EQU  H'0003'
OVRA             EQU  H'0004'
OVRB             EQU  H'0005'
OVRC             EQU  H'0006'
OVRD             EQU  H'0007'
CWG1STRA         EQU  H'0000'
CWG1STRB         EQU  H'0001'
CWG1STRC         EQU  H'0002'
CWG1STRD         EQU  H'0003'
CWG1OVRA         EQU  H'0004'
CWG1OVRB         EQU  H'0005'
CWG1OVRC         EQU  H'0006'
CWG1OVRD         EQU  H'0007'


;----- ADCON1 Bits -----------------------------------------------------
DSEN             EQU  H'0000'
GPOL_ADCON1      EQU  H'0005'
IPEN_ADCON1      EQU  H'0006'
PPOL             EQU  H'0007'
ADDSEN           EQU  H'0000'
ADGPOL           EQU  H'0005'
ADIPEN           EQU  H'0006'
ADPPOL           EQU  H'0007'


;----- ADCSEL1 Bits -----------------------------------------------------
SSI              EQU  H'0006'
CHEN             EQU  H'0007'


;----- NCO1ACC Bits -----------------------------------------------------
NCO1ACC0         EQU  H'0000'
NCO1ACC1         EQU  H'0001'
NCO1ACC2         EQU  H'0002'
NCO1ACC3         EQU  H'0003'
NCO1ACC4         EQU  H'0004'
NCO1ACC5         EQU  H'0005'
NCO1ACC6         EQU  H'0006'
NCO1ACC7         EQU  H'0007'
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- NCO1ACCH Bits -----------------------------------------------------
NCO1ACC8         EQU  H'0000'
NCO1ACC9         EQU  H'0001'
NCO1ACC10        EQU  H'0002'
NCO1ACC11        EQU  H'0003'
NCO1ACC12        EQU  H'0004'
NCO1ACC13        EQU  H'0005'
NCO1ACC14        EQU  H'0006'
NCO1ACC15        EQU  H'0007'
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- NCO1ACCU Bits -----------------------------------------------------
NCO1ACC16        EQU  H'0000'
NCO1ACC17        EQU  H'0001'
NCO1ACC18        EQU  H'0002'
NCO1ACC19        EQU  H'0003'
NCO1ACC20        EQU  H'0004'
NCO1ACC21        EQU  H'0005'
NCO1ACC22        EQU  H'0006'
NCO1ACC23        EQU  H'0007'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
ACC20            EQU  H'0004'
ACC21            EQU  H'0005'
ACC22            EQU  H'0006'
ACC23            EQU  H'0007'


;----- NCO1INC Bits -----------------------------------------------------
NCO1INC0         EQU  H'0000'
NCO1INC1         EQU  H'0001'
NCO1INC2         EQU  H'0002'
NCO1INC3         EQU  H'0003'
NCO1INC4         EQU  H'0004'
NCO1INC5         EQU  H'0005'
NCO1INC6         EQU  H'0006'
NCO1INC7         EQU  H'0007'
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
INC4             EQU  H'0004'
INC5             EQU  H'0005'
INC6             EQU  H'0006'
INC7             EQU  H'0007'


;----- NCO1INCH Bits -----------------------------------------------------
NCO1INC8         EQU  H'0000'
NCO1INC9         EQU  H'0001'
NCO1INC10        EQU  H'0002'
NCO1INC11        EQU  H'0003'
NCO1INC12        EQU  H'0004'
NCO1INC13        EQU  H'0005'
NCO1INC14        EQU  H'0006'
NCO1INC15        EQU  H'0007'
INC8             EQU  H'0000'
INC9             EQU  H'0001'
INC10            EQU  H'0002'
INC11            EQU  H'0003'
INC12            EQU  H'0004'
INC13            EQU  H'0005'
INC14            EQU  H'0006'
INC15            EQU  H'0007'


;----- NCO1INCU Bits -----------------------------------------------------
NCO1INC16        EQU  H'0000'
NCO1INC17        EQU  H'0001'
NCO1INC18        EQU  H'0002'
NCO1INC19        EQU  H'0003'
NCO1INC20        EQU  H'0004'
NCO1INC21        EQU  H'0005'
NCO1INC22        EQU  H'0006'
NCO1INC23        EQU  H'0007'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
INC20            EQU  H'0004'
INC21            EQU  H'0005'
INC22            EQU  H'0006'
INC23            EQU  H'0007'


;----- NCO1CON Bits -----------------------------------------------------
PFM              EQU  H'0000'
POL_NCO1CON      EQU  H'0004'
OUT_NCO1CON      EQU  H'0005'
EN               EQU  H'0007'
NCO1PFM          EQU  H'0000'
NCO1POL          EQU  H'0004'
NCO1OUT          EQU  H'0005'
NCO1EN           EQU  H'0007'


;----- NCO1CLK Bits -----------------------------------------------------
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO1CKS0         EQU  H'0000'
NCO1CKS1         EQU  H'0001'
NCO1CKS2         EQU  H'0002'
NCO1CKS3         EQU  H'0003'
NCO1PWS0         EQU  H'0005'
NCO1PWS1         EQU  H'0006'
NCO1PWS2         EQU  H'0007'


;----- PWM1GIR Bits -----------------------------------------------------
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM1GIE Bits -----------------------------------------------------
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM1CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD_PWM1CON       EQU  H'0002'
EN               EQU  H'0007'


;----- PWM1S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PIE1 Bits -----------------------------------------------------
INT0IE           EQU  H'0000'
ZCDIE            EQU  H'0001'
ADIE             EQU  H'0002'
ACTIE            EQU  H'0003'
C1IE             EQU  H'0004'
SMT1IE           EQU  H'0005'
SMT1PRAIE        EQU  H'0006'
SMT1PWAIE        EQU  H'0007'


;----- PIE11 Bits -----------------------------------------------------
CCP3IE           EQU  H'0000'
CLC6IE           EQU  H'0001'
CWG3IE           EQU  H'0002'
TMR4IE           EQU  H'0003'
DMA4SCNTIE       EQU  H'0004'
DMA4DCNTIE       EQU  H'0005'
DMA4ORIE         EQU  H'0006'
DMA4AIE          EQU  H'0007'


;----- PIR1 Bits -----------------------------------------------------
INT0IF           EQU  H'0000'
ZCDIF            EQU  H'0001'
ADIF             EQU  H'0002'
ACTIF            EQU  H'0003'
C1IF             EQU  H'0004'
SMT1IF           EQU  H'0005'
SMT1PRAIF        EQU  H'0006'
SMT1PWAIF        EQU  H'0007'


;----- PIR11 Bits -----------------------------------------------------
CCP3IF           EQU  H'0000'
CLC6IF           EQU  H'0001'
CWG3IF           EQU  H'0002'
TMR4IF           EQU  H'0003'
DMA4SCNTIF       EQU  H'0004'
DMA4DCNTIF       EQU  H'0005'
DMA4ORIF         EQU  H'0006'
DMA4AIF          EQU  H'0007'


;----- BOOTREG Bits -----------------------------------------------------
BOOTDONE         EQU  H'0006'
BPOUT            EQU  H'0007'


;----- CLKRCON Bits -----------------------------------------------------
EN               EQU  H'0007'
CLKRDIV0         EQU  H'0000'
CLKRDIV1         EQU  H'0001'
CLKRDIV2         EQU  H'0002'
CLKRDC0          EQU  H'0003'
CLKRDC1          EQU  H'0004'
CLKREN           EQU  H'0007'
DIV0             EQU  H'0000'
DIV1             EQU  H'0001'
DIV2             EQU  H'0002'
DC0              EQU  H'0003'
DC1              EQU  H'0004'


;----- CLKRCLK Bits -----------------------------------------------------
CLKRCLK0         EQU  H'0000'
CLKRCLK1         EQU  H'0001'
CLKRCLK2         EQU  H'0002'
CLKRCLK3         EQU  H'0003'
CLKRCLK4         EQU  H'0004'
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'


;----- NVMCON0 Bits -----------------------------------------------------
GO_NVMCON0       EQU  H'0000'
NOT_DONE_NVMCON0 EQU  H'0000'
NDONE            EQU  H'0000'


;----- NVMCON1 Bits -----------------------------------------------------
WRERR            EQU  H'0007'


;----- NVMADR Bits -----------------------------------------------------
NVMADR0          EQU  H'0000'
NVMADR1          EQU  H'0001'
NVMADR2          EQU  H'0002'
NVMADR3          EQU  H'0003'
NVMADR4          EQU  H'0004'
NVMADR5          EQU  H'0005'
NVMADR6          EQU  H'0006'
NVMADR7          EQU  H'0007'


;----- NVMADRH Bits -----------------------------------------------------
NVMADR8          EQU  H'0000'
NVMADR9          EQU  H'0001'
NVMADR10         EQU  H'0002'
NVMADR11         EQU  H'0003'
NVMADR12         EQU  H'0004'
NVMADR13         EQU  H'0005'
NVMADR14         EQU  H'0006'
NVMADR15         EQU  H'0007'


;----- NVMADRU Bits -----------------------------------------------------
NVMADR16         EQU  H'0000'
NVMADR17         EQU  H'0001'
NVMADR18         EQU  H'0002'
NVMADR19         EQU  H'0003'
NVMADR20         EQU  H'0004'
NVMADR21         EQU  H'0005'


;----- NVMDAT Bits -----------------------------------------------------
NVMDAT0          EQU  H'0000'
NVMDAT1          EQU  H'0001'
NVMDAT2          EQU  H'0002'
NVMDAT3          EQU  H'0003'
NVMDAT4          EQU  H'0004'
NVMDAT5          EQU  H'0005'
NVMDAT6          EQU  H'0006'
NVMDAT7          EQU  H'0007'


;----- NVMDATH Bits -----------------------------------------------------
NVMDAT8          EQU  H'0000'
NVMDAT9          EQU  H'0001'
NVMDAT10         EQU  H'0002'
NVMDAT11         EQU  H'0003'
NVMDAT12         EQU  H'0004'
NVMDAT13         EQU  H'0005'
NVMDAT14         EQU  H'0006'
NVMDAT15         EQU  H'0007'


;----- VREGCON Bits -----------------------------------------------------
VREGPM0          EQU  H'0000'
VREGPM1          EQU  H'0001'
PMSYS0           EQU  H'0004'
PMSYS1           EQU  H'0005'


;----- BORCON Bits -----------------------------------------------------
BORRDY           EQU  H'0000'
SBOREN           EQU  H'0007'


;----- HLVDCON0 Bits -----------------------------------------------------
INTL             EQU  H'0000'
INTH             EQU  H'0001'
RDY_HLVDCON0     EQU  H'0004'
OUT_HLVDCON0     EQU  H'0005'
EN               EQU  H'0007'
HLVDINTL         EQU  H'0000'
HLVDINTH         EQU  H'0001'
HLVDRDY          EQU  H'0004'
HLVDOUT          EQU  H'0005'
HLVDEN           EQU  H'0007'


;----- HLVDCON1 Bits -----------------------------------------------------
SEL0             EQU  H'0000'
SEL1             EQU  H'0001'
SEL2             EQU  H'0002'
SEL3             EQU  H'0003'
HLVDSEL0         EQU  H'0000'
HLVDSEL1         EQU  H'0001'
HLVDSEL2         EQU  H'0002'
HLVDSEL3         EQU  H'0003'


;----- ZCDCON Bits -----------------------------------------------------
INTN_ZCDCON      EQU  H'0000'
INTP_ZCDCON      EQU  H'0001'
POL_ZCDCON       EQU  H'0004'
OUT_ZCDCON       EQU  H'0005'
SEN_ZCDCON       EQU  H'0007'
ZCDINTN          EQU  H'0000'
ZCDINTP          EQU  H'0001'
ZCDPOL           EQU  H'0004'
ZCDOUT           EQU  H'0005'
ZCDSEN           EQU  H'0007'


;----- PMD0 Bits -----------------------------------------------------
IOCMD            EQU  H'0000'
CLKRMD           EQU  H'0001'
SCANMD           EQU  H'0003'
CRCMD            EQU  H'0004'
HLVDMD           EQU  H'0005'
FVRMD            EQU  H'0006'
SYSCMD           EQU  H'0007'
LVDMD            EQU  H'0005'


;----- PMD2 Bits -----------------------------------------------------
TU16AMD          EQU  H'0000'
TU16BMD          EQU  H'0001'
CANMD            EQU  H'0007'
TU1MD            EQU  H'0000'
TU2MD            EQU  H'0001'


;----- PMD3 Bits -----------------------------------------------------
ZCDMD            EQU  H'0000'
CM1MD            EQU  H'0001'
CM2MD            EQU  H'0002'
ADCMD            EQU  H'0005'
DACMD            EQU  H'0006'
ACTMD            EQU  H'0007'


;----- PMD4 Bits -----------------------------------------------------
NCO1MD           EQU  H'0000'
NCO2MD           EQU  H'0001'
NCO3MD           EQU  H'0002'
DSM1MD           EQU  H'0003'
CWG1MD           EQU  H'0004'
CWG2MD           EQU  H'0005'
CWG3MD           EQU  H'0006'


;----- PMD5 Bits -----------------------------------------------------
CCP1MD           EQU  H'0000'
CCP2MD           EQU  H'0001'
CCP3MD           EQU  H'0002'
PWM1MD           EQU  H'0004'
PWM2MD           EQU  H'0005'
PWM3MD           EQU  H'0006'
PWM4MD           EQU  H'0007'


;----- PMD6 Bits -----------------------------------------------------
I2C1MD           EQU  H'0000'
SPI1MD_PMD6      EQU  H'0001'
SPI2MD           EQU  H'0002'
U1MD             EQU  H'0003'
U2MD             EQU  H'0004'
U3MD             EQU  H'0005'
U4MD             EQU  H'0006'
U5MD             EQU  H'0007'


;----- PMD7 Bits -----------------------------------------------------
CLC1MD           EQU  H'0000'
CLC2MD           EQU  H'0001'
CLC3MD           EQU  H'0002'
CLC4MD           EQU  H'0003'
CLC5MD           EQU  H'0004'
CLC6MD           EQU  H'0005'
CLC7MD           EQU  H'0006'
CLC8MD           EQU  H'0007'


;----- PMD8 Bits -----------------------------------------------------
DMA1MD           EQU  H'0000'
DMA2MD           EQU  H'0001'
DMA3MD           EQU  H'0002'
DMA4MD           EQU  H'0003'
DMA5MD           EQU  H'0004'
DMA6MD           EQU  H'0005'
DMA7MD           EQU  H'0006'
DMA8MD           EQU  H'0007'


;----- MD1CON0 Bits -----------------------------------------------------
BIT              EQU  H'0000'
OPOL             EQU  H'0004'
OUT_MD1CON0      EQU  H'0005'
EN               EQU  H'0007'
MD1BIT           EQU  H'0000'
MD1OPOL          EQU  H'0004'
MD1OUT           EQU  H'0005'
MD1EN            EQU  H'0007'


;----- MD1CON1 Bits -----------------------------------------------------
CLSYNC           EQU  H'0000'
CLPOL            EQU  H'0001'
CHSYNC           EQU  H'0004'
CHPOL            EQU  H'0005'
MD1CLSYNC        EQU  H'0000'
MD1CLPOL         EQU  H'0001'
MD1CHSYNC        EQU  H'0004'
MD1CHPOL         EQU  H'0005'


;----- MD1SRC Bits -----------------------------------------------------
MS0              EQU  H'0000'
MS1              EQU  H'0001'
MS2              EQU  H'0002'
MS3              EQU  H'0003'
MS4              EQU  H'0004'
MS5              EQU  H'0005'
MD1MS0           EQU  H'0000'
MD1MS1           EQU  H'0001'
MD1MS2           EQU  H'0002'
MD1MS3           EQU  H'0003'
MD1MS4           EQU  H'0004'
MD1MS5           EQU  H'0005'


;----- MD1CARL Bits -----------------------------------------------------
CL0              EQU  H'0000'
CL1              EQU  H'0001'
CL2              EQU  H'0002'
CL3              EQU  H'0003'
CL4              EQU  H'0004'
MD1CL0           EQU  H'0000'
MD1CL1           EQU  H'0001'
MD1CL2           EQU  H'0002'
MD1CL3           EQU  H'0003'
MD1CL4           EQU  H'0004'


;----- MD1CARH Bits -----------------------------------------------------
CH0              EQU  H'0000'
CH1              EQU  H'0001'
CH2              EQU  H'0002'
CH3              EQU  H'0003'
CH4              EQU  H'0004'
MD1CH0           EQU  H'0000'
MD1CH1           EQU  H'0001'
MD1CH2           EQU  H'0002'
MD1CH3           EQU  H'0003'
MD1CH4           EQU  H'0004'


;----- CMOUT Bits -----------------------------------------------------
MC1OUT           EQU  H'0000'
MC2OUT           EQU  H'0001'


;----- CM2CON0 Bits -----------------------------------------------------
SYNC             EQU  H'0000'
HYS              EQU  H'0001'
POL_CM2CON0      EQU  H'0004'
OUT_CM2CON0      EQU  H'0006'
EN               EQU  H'0007'
C2SYNC           EQU  H'0000'
C2HYS            EQU  H'0001'
C2POL            EQU  H'0004'
C2OUT_CM2CON0    EQU  H'0006'
C2EN             EQU  H'0007'


;----- CM2CON1 Bits -----------------------------------------------------
INTN_CM2CON1     EQU  H'0000'
INTP_CM2CON1     EQU  H'0001'
C2INTN           EQU  H'0000'
C2INTP           EQU  H'0001'


;----- CM2NCH Bits -----------------------------------------------------
NCH0_CM2NCH      EQU  H'0000'
NCH1_CM2NCH      EQU  H'0001'
NCH2_CM2NCH      EQU  H'0002'
C2NCH0           EQU  H'0000'
C2NCH1           EQU  H'0001'
C2NCH2           EQU  H'0002'


;----- CM2PCH Bits -----------------------------------------------------
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
C2PCH0           EQU  H'0000'
C2PCH1           EQU  H'0001'
C2PCH2           EQU  H'0002'


;----- WDTCON0 Bits -----------------------------------------------------
SEN_WDTCON0      EQU  H'0000'
SWDTEN           EQU  H'0000'
WDTSEN           EQU  H'0000'
WDTPS0           EQU  H'0001'
WDTPS1           EQU  H'0002'
WDTPS2           EQU  H'0003'
WDTPS3           EQU  H'0004'
WDTPS4           EQU  H'0005'


;----- WDTCON1 Bits -----------------------------------------------------
WINDOW0          EQU  H'0000'
WINDOW1          EQU  H'0001'
WINDOW2          EQU  H'0002'
WDTWINDOW0       EQU  H'0000'
WDTWINDOW1       EQU  H'0001'
WDTWINDOW2       EQU  H'0002'
WDTCS0           EQU  H'0004'
WDTCS1           EQU  H'0005'
WDTCS2           EQU  H'0006'


;----- WDTPSL Bits -----------------------------------------------------
PSCNT0           EQU  H'0000'
PSCNT1           EQU  H'0001'
PSCNT2           EQU  H'0002'
PSCNT3           EQU  H'0003'
PSCNT4           EQU  H'0004'
PSCNT5           EQU  H'0005'
PSCNT6           EQU  H'0006'
PSCNT7           EQU  H'0007'
WDTPSCNT0        EQU  H'0000'
WDTPSCNT1        EQU  H'0001'
WDTPSCNT2        EQU  H'0002'
WDTPSCNT3        EQU  H'0003'
WDTPSCNT4        EQU  H'0004'
WDTPSCNT5        EQU  H'0005'
WDTPSCNT6        EQU  H'0006'
WDTPSCNT7        EQU  H'0007'


;----- WDTPSH Bits -----------------------------------------------------
PSCNT8           EQU  H'0000'
PSCNT9           EQU  H'0001'
PSCNT10          EQU  H'0002'
PSCNT11          EQU  H'0003'
PSCNT12          EQU  H'0004'
PSCNT13          EQU  H'0005'
PSCNT14          EQU  H'0006'
PSCNT15          EQU  H'0007'
WDTPSCNT8        EQU  H'0000'
WDTPSCNT9        EQU  H'0001'
WDTPSCNT10       EQU  H'0002'
WDTPSCNT11       EQU  H'0003'
WDTPSCNT12       EQU  H'0004'
WDTPSCNT13       EQU  H'0005'
WDTPSCNT14       EQU  H'0006'
WDTPSCNT15       EQU  H'0007'


;----- WDTTMR Bits -----------------------------------------------------
PSCNT16          EQU  H'0000'
PSCNT17          EQU  H'0001'
STATE            EQU  H'0002'
WDTPSCNT16       EQU  H'0000'
WDTPSCNT17       EQU  H'0001'
WDTSTATE         EQU  H'0002'
WDTTMR0          EQU  H'0003'
WDTTMR1          EQU  H'0004'
WDTTMR2          EQU  H'0005'
WDTTMR3          EQU  H'0006'
WDTTMR4          EQU  H'0007'


;----- DAC1DAT Bits -----------------------------------------------------
DAC1R0           EQU  H'0000'
DAC1R1           EQU  H'0001'
DAC1R2           EQU  H'0002'
DAC1R3           EQU  H'0003'
DAC1R4           EQU  H'0004'
DAC1R5           EQU  H'0005'
DAC1R6           EQU  H'0006'
DAC1R7           EQU  H'0007'


;----- DAC1CON Bits -----------------------------------------------------
EN               EQU  H'0007'
DAC1PSS0         EQU  H'0002'
DAC1PSS1         EQU  H'0003'
DAC1EN           EQU  H'0007'
NSS0             EQU  H'0000'
PSS0_DAC1CON     EQU  H'0002'
PSS1_DAC1CON     EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'


;----- SPI2RXB Bits -----------------------------------------------------
RXB0             EQU  H'0000'
RXB1             EQU  H'0001'
RXB2             EQU  H'0002'
RXB3             EQU  H'0003'
RXB4             EQU  H'0004'
RXB5             EQU  H'0005'
RXB6             EQU  H'0006'
RXB7             EQU  H'0007'


;----- SPI2TXB Bits -----------------------------------------------------
TXB0             EQU  H'0000'
TXB1             EQU  H'0001'
TXB2             EQU  H'0002'
TXB3             EQU  H'0003'
TXB4             EQU  H'0004'
TXB5             EQU  H'0005'
TXB6             EQU  H'0006'
TXB7             EQU  H'0007'


;----- SPI2CON0 Bits -----------------------------------------------------
BMODE            EQU  H'0000'
MST              EQU  H'0001'
LSBF             EQU  H'0002'
EN               EQU  H'0007'
SPI2BMODE        EQU  H'0000'
SPI2MST          EQU  H'0001'
SPI2LSBF         EQU  H'0002'
SPI2SPIEN        EQU  H'0007'


;----- SPI2CON1 Bits -----------------------------------------------------
SDOP             EQU  H'0000'
SDIP             EQU  H'0001'
SSP              EQU  H'0002'
FST              EQU  H'0004'
CKP              EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'
SPI2SDOP         EQU  H'0000'
SPI2SDIP         EQU  H'0001'
SPI2SSP          EQU  H'0002'
SPI2FST          EQU  H'0004'
SPI2CKP          EQU  H'0005'
SPI2CKE          EQU  H'0006'
SPI2SMP          EQU  H'0007'


;----- SPI2CON2 Bits -----------------------------------------------------
RXR              EQU  H'0000'
TXR              EQU  H'0001'
SSET             EQU  H'0002'
SSFLT            EQU  H'0006'
BUSY_SPI2CON2    EQU  H'0007'
SPI2RXR          EQU  H'0000'
SPI2TXR          EQU  H'0001'
SPI2SSET         EQU  H'0002'
SPI2SSFLT        EQU  H'0006'
SPI2BUSY         EQU  H'0007'


;----- SPI2STATUS Bits -----------------------------------------------------
RXBF             EQU  H'0000'
CLRBF            EQU  H'0002'
RXRE             EQU  H'0003'
TXBE             EQU  H'0005'
TXWE             EQU  H'0007'
SPI2RXBF         EQU  H'0000'
SPI2CLRBF        EQU  H'0002'
SPI2RXRE         EQU  H'0003'
SPI2TXBE         EQU  H'0005'
SPI2TXWE         EQU  H'0007'


;----- SPI2TWIDTH Bits -----------------------------------------------------
TWIDTH0          EQU  H'0000'
TWIDTH1          EQU  H'0001'
TWIDTH2          EQU  H'0002'


;----- SPI2BAUD Bits -----------------------------------------------------
BAUD0            EQU  H'0000'
BAUD1            EQU  H'0001'
BAUD2            EQU  H'0002'
BAUD3            EQU  H'0003'
BAUD4            EQU  H'0004'
BAUD5            EQU  H'0005'
BAUD6            EQU  H'0006'
BAUD7            EQU  H'0007'


;----- SPI2INTF Bits -----------------------------------------------------
TXUIF            EQU  H'0001'
RXOIF            EQU  H'0002'
EOSIF            EQU  H'0004'
SOSIF            EQU  H'0005'
TCZIF            EQU  H'0006'
SRMTIF           EQU  H'0007'
SPI2TXUIF        EQU  H'0001'
SPI2RXOIF        EQU  H'0002'
SPI2EOSIF        EQU  H'0004'
SPI2SOSIF        EQU  H'0005'
SPI2TCZIF        EQU  H'0006'
SPI2SRMTIF       EQU  H'0007'


;----- SPI2INTE Bits -----------------------------------------------------
TXUIE            EQU  H'0001'
RXOIE            EQU  H'0002'
EOSIE            EQU  H'0004'
SOSIE            EQU  H'0005'
TCZIE            EQU  H'0006'
SRMTIE           EQU  H'0007'
SPI2TXUIE        EQU  H'0001'
SPI2RXOIE        EQU  H'0002'
SPI2EOSIE        EQU  H'0004'
SPI2SOSIE        EQU  H'0005'
SPI2TCZIE        EQU  H'0006'
SPI2SRMTIE       EQU  H'0007'


;----- SPI2CLK Bits -----------------------------------------------------
CLKSEL0          EQU  H'0000'
CLKSEL1          EQU  H'0001'
CLKSEL2          EQU  H'0002'
CLKSEL3          EQU  H'0003'
CLKSEL4          EQU  H'0004'
SPI2CLKSEL0      EQU  H'0000'
SPI2CLKSEL1      EQU  H'0001'
SPI2CLKSEL2      EQU  H'0002'
SPI2CLKSEL3      EQU  H'0003'
SPI2CLKSEL4      EQU  H'0004'


;----- ACTCON Bits -----------------------------------------------------
ACTORS           EQU  H'0001'
ACTLOCK          EQU  H'0003'
ACTUD            EQU  H'0006'
ACTEN            EQU  H'0007'


;----- OSCCON2 Bits -----------------------------------------------------
CDIV0            EQU  H'0000'
CDIV1            EQU  H'0001'
CDIV2            EQU  H'0002'
CDIV3            EQU  H'0003'
COSC0            EQU  H'0004'
COSC1            EQU  H'0005'
COSC2            EQU  H'0006'


;----- OSCCON3 Bits -----------------------------------------------------
NOSCR            EQU  H'0003'
ORDY             EQU  H'0004'
SOSCPWR          EQU  H'0006'
CSWHOLD          EQU  H'0007'


;----- OSCTUNE Bits -----------------------------------------------------
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'
TUN5             EQU  H'0005'


;----- OSCFRQ Bits -----------------------------------------------------
FRQ0             EQU  H'0000'
FRQ1             EQU  H'0001'
FRQ2             EQU  H'0002'
FRQ3             EQU  H'0003'


;----- OSCSTAT Bits -----------------------------------------------------
PLLR             EQU  H'0000'
ADOR             EQU  H'0002'
SOR              EQU  H'0003'
LFOR             EQU  H'0004'
MFOR             EQU  H'0005'
HFOR             EQU  H'0006'
EXTOR            EQU  H'0007'


;----- OSCEN Bits -----------------------------------------------------
PLLEN            EQU  H'0000'
ADOEN            EQU  H'0002'
SOSCEN           EQU  H'0003'
LFOEN            EQU  H'0004'
MFOEN            EQU  H'0005'
HFOEN            EQU  H'0006'
EXTOEN           EQU  H'0007'


;----- PRLOCK Bits -----------------------------------------------------
PRLOCKED         EQU  H'0000'


;----- SCANPR Bits -----------------------------------------------------
PR0_SCANPR       EQU  H'0000'
PR1_SCANPR       EQU  H'0001'
PR2_SCANPR       EQU  H'0002'
SCANPR0          EQU  H'0000'
SCANPR1          EQU  H'0001'
SCANPR2          EQU  H'0002'


;----- DMA2PR Bits -----------------------------------------------------
PR0_DMA2PR       EQU  H'0000'
PR1_DMA2PR       EQU  H'0001'
PR2_DMA2PR       EQU  H'0002'
DMA2PR0          EQU  H'0000'
DMA2PR1          EQU  H'0001'
DMA2PR2          EQU  H'0002'


;----- DMA3PR Bits -----------------------------------------------------
PR0_DMA3PR       EQU  H'0000'
PR1_DMA3PR       EQU  H'0001'
PR2_DMA3PR       EQU  H'0002'
DMA3PR0          EQU  H'0000'
DMA3PR1          EQU  H'0001'
DMA3PR2          EQU  H'0002'


;----- DMA4PR Bits -----------------------------------------------------
PR0_DMA4PR       EQU  H'0000'
PR1_DMA4PR       EQU  H'0001'
PR2_DMA4PR       EQU  H'0002'
DMA4PR0          EQU  H'0000'
DMA4PR1          EQU  H'0001'
DMA4PR2          EQU  H'0002'


;----- DMA5PR Bits -----------------------------------------------------
PR0_DMA5PR       EQU  H'0000'
PR1_DMA5PR       EQU  H'0001'
PR2_DMA5PR       EQU  H'0002'
DMA5PR0          EQU  H'0000'
DMA5PR1          EQU  H'0001'
DMA5PR2          EQU  H'0002'


;----- DMA6PR Bits -----------------------------------------------------
PR0_DMA6PR       EQU  H'0000'
PR1_DMA6PR       EQU  H'0001'
PR2_DMA6PR       EQU  H'0002'
DMA6PR0          EQU  H'0000'
DMA6PR1          EQU  H'0001'
DMA6PR2          EQU  H'0002'


;----- DMA7PR Bits -----------------------------------------------------
PR0_DMA7PR       EQU  H'0000'
PR1_DMA7PR       EQU  H'0001'
PR2_DMA7PR       EQU  H'0002'
DMA7PR0          EQU  H'0000'
DMA7PR1          EQU  H'0001'
DMA7PR2          EQU  H'0002'


;----- DMA8PR Bits -----------------------------------------------------
PR0_DMA8PR       EQU  H'0000'
PR1_DMA8PR       EQU  H'0001'
PR2_DMA8PR       EQU  H'0002'
DMA8PR0          EQU  H'0000'
DMA8PR1          EQU  H'0001'
DMA8PR2          EQU  H'0002'


;----- MAINPR Bits -----------------------------------------------------
PR0_MAINPR       EQU  H'0000'
PR1_MAINPR       EQU  H'0001'
PR2_MAINPR       EQU  H'0002'
MAINPR0          EQU  H'0000'
MAINPR1          EQU  H'0001'
MAINPR2          EQU  H'0002'


;----- ISRPR Bits -----------------------------------------------------
PR0_ISRPR        EQU  H'0000'
PR1_ISRPR        EQU  H'0001'
PR2_ISRPR        EQU  H'0002'
ISRPR0           EQU  H'0000'
ISRPR1           EQU  H'0001'
ISRPR2           EQU  H'0002'


;----- CLCDATA Bits -----------------------------------------------------
MCLC0            EQU  H'0000'
MCLC1            EQU  H'0001'
MCLC2            EQU  H'0002'
MCLC3            EQU  H'0003'
MCLC4            EQU  H'0004'
MCLC5            EQU  H'0005'
MCLC6            EQU  H'0006'
MCLC7            EQU  H'0007'
CLC1OUT          EQU  H'0000'
CLC2OUT          EQU  H'0001'
CLC3OUT          EQU  H'0002'
CLC4OUT          EQU  H'0003'
CLC5OUT          EQU  H'0004'
CLC6OUT          EQU  H'0005'
CLC7OUT          EQU  H'0006'
CLC8OUT          EQU  H'0007'


;----- CLCSELECT Bits -----------------------------------------------------
SLCT0            EQU  H'0000'
SLCT1            EQU  H'0001'
SLCT2            EQU  H'0002'


;----- CLCNCON Bits -----------------------------------------------------
INTN_CLCNCON     EQU  H'0003'
INTP_CLCNCON     EQU  H'0004'
OUT_CLCNCON      EQU  H'0005'
OE               EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- CLCNPOL Bits -----------------------------------------------------
G1POL            EQU  H'0000'
G2POL            EQU  H'0001'
G3POL            EQU  H'0002'
G4POL            EQU  H'0003'
POL_CLCNPOL      EQU  H'0007'


;----- CLCNSEL0 Bits -----------------------------------------------------
D1S0             EQU  H'0000'
D1S1             EQU  H'0001'
D1S2             EQU  H'0002'
D1S3             EQU  H'0003'
D1S4             EQU  H'0004'
D1S5             EQU  H'0005'
D1S6             EQU  H'0006'
D1S7             EQU  H'0007'


;----- CLCNSEL2 Bits -----------------------------------------------------
D3S0             EQU  H'0000'
D3S1             EQU  H'0001'
D3S2             EQU  H'0002'
D3S3             EQU  H'0003'
D3S4             EQU  H'0004'
D3S5             EQU  H'0005'
D3S6             EQU  H'0006'
D3S7             EQU  H'0007'


;----- CLCNSEL3 Bits -----------------------------------------------------
D4S0             EQU  H'0000'
D4S1             EQU  H'0001'
D4S2             EQU  H'0002'
D4S3             EQU  H'0003'
D4S4             EQU  H'0004'
D4S5             EQU  H'0005'
D4S6             EQU  H'0006'
D4S7             EQU  H'0007'


;----- CLCNGLS0 Bits -----------------------------------------------------
G1D1N            EQU  H'0000'
G1D1T            EQU  H'0001'
G1D2N            EQU  H'0002'
G1D2T            EQU  H'0003'
G1D3N            EQU  H'0004'
G1D3T            EQU  H'0005'
G1D4N            EQU  H'0006'
G1D4T            EQU  H'0007'


;----- CLCNGLS2 Bits -----------------------------------------------------
G3D1N            EQU  H'0000'
G3D1T            EQU  H'0001'
G3D2N            EQU  H'0002'
G3D2T            EQU  H'0003'
G3D3N            EQU  H'0004'
G3D3T            EQU  H'0005'
G3D4N            EQU  H'0006'
G3D4T            EQU  H'0007'


;----- CLCNGLS3 Bits -----------------------------------------------------
G4D1N            EQU  H'0000'
G4D1T            EQU  H'0001'
G4D2N            EQU  H'0002'
G4D2T            EQU  H'0003'
G4D3N            EQU  H'0004'
G4D3T            EQU  H'0005'
G4D4N            EQU  H'0006'
G4D4T            EQU  H'0007'


;----- DMASELECT Bits -----------------------------------------------------
SLCT0            EQU  H'0000'
SLCT1            EQU  H'0001'
SLCT2            EQU  H'0002'


;----- DMANBUF Bits -----------------------------------------------------
BUF0             EQU  H'0000'
BUF1             EQU  H'0001'
BUF2             EQU  H'0002'
BUF3             EQU  H'0003'
BUF4             EQU  H'0004'
BUF5             EQU  H'0005'
BUF6             EQU  H'0006'
BUF7             EQU  H'0007'


;----- DMANDCNT Bits -----------------------------------------------------
DCNT0            EQU  H'0000'
DCNT1            EQU  H'0001'
DCNT2            EQU  H'0002'
DCNT3            EQU  H'0003'
DCNT4            EQU  H'0004'
DCNT5            EQU  H'0005'
DCNT6            EQU  H'0006'
DCNT7            EQU  H'0007'


;----- DMANDCNTH Bits -----------------------------------------------------
DCNT8            EQU  H'0000'
DCNT9            EQU  H'0001'
DCNT10           EQU  H'0002'
DCNT11           EQU  H'0003'


;----- DMANDPTR Bits -----------------------------------------------------
DPTR0            EQU  H'0000'
DPTR1            EQU  H'0001'
DPTR2            EQU  H'0002'
DPTR3            EQU  H'0003'
DPTR4            EQU  H'0004'
DPTR5            EQU  H'0005'
DPTR6            EQU  H'0006'
DPTR7            EQU  H'0007'


;----- DMANDPTRH Bits -----------------------------------------------------
DPTR8            EQU  H'0000'
DPTR9            EQU  H'0001'
DPTR10           EQU  H'0002'
DPTR11           EQU  H'0003'
DPTR12           EQU  H'0004'
DPTR13           EQU  H'0005'
DPTR14           EQU  H'0006'
DPTR15           EQU  H'0007'


;----- DMANDSZ Bits -----------------------------------------------------
DSZ0             EQU  H'0000'
DSZ1             EQU  H'0001'
DSZ2             EQU  H'0002'
DSZ3             EQU  H'0003'
DSZ4             EQU  H'0004'
DSZ5             EQU  H'0005'
DSZ6             EQU  H'0006'
DSZ7             EQU  H'0007'


;----- DMANDSZH Bits -----------------------------------------------------
DSZ8             EQU  H'0000'
DSZ9             EQU  H'0001'
DSZ10            EQU  H'0002'
DSZ11            EQU  H'0003'


;----- DMANDSA Bits -----------------------------------------------------
DSA0             EQU  H'0000'
DSA1             EQU  H'0001'
DSA2             EQU  H'0002'
DSA3             EQU  H'0003'
DSA4             EQU  H'0004'
DSA5             EQU  H'0005'
DSA6             EQU  H'0006'
DSA7             EQU  H'0007'


;----- DMANDSAH Bits -----------------------------------------------------
DSA8             EQU  H'0000'
DSA9             EQU  H'0001'
DSA10            EQU  H'0002'
DSA11            EQU  H'0003'
DSA12            EQU  H'0004'
DSA13            EQU  H'0005'
DSA14            EQU  H'0006'
DSA15            EQU  H'0007'


;----- DMANSCNT Bits -----------------------------------------------------
SCNT0            EQU  H'0000'
SCNT1            EQU  H'0001'
SCNT2            EQU  H'0002'
SCNT3            EQU  H'0003'
SCNT4            EQU  H'0004'
SCNT5            EQU  H'0005'
SCNT6            EQU  H'0006'
SCNT7            EQU  H'0007'


;----- DMANSCNTH Bits -----------------------------------------------------
SCNT8            EQU  H'0000'
SCNT9            EQU  H'0001'
SCNT10           EQU  H'0002'
SCNT11           EQU  H'0003'


;----- DMANSPTR Bits -----------------------------------------------------
SPTR0            EQU  H'0000'
SPTR1            EQU  H'0001'
SPTR2            EQU  H'0002'
SPTR3            EQU  H'0003'
SPTR4            EQU  H'0004'
SPTR5            EQU  H'0005'
SPTR6            EQU  H'0006'
SPTR7            EQU  H'0007'


;----- DMANSPTRH Bits -----------------------------------------------------
SPTR8            EQU  H'0000'
SPTR9            EQU  H'0001'
SPTR10           EQU  H'0002'
SPTR11           EQU  H'0003'
SPTR12           EQU  H'0004'
SPTR13           EQU  H'0005'
SPTR14           EQU  H'0006'
SPTR15           EQU  H'0007'


;----- DMANSPTRU Bits -----------------------------------------------------
SPTR16           EQU  H'0000'
SPTR17           EQU  H'0001'
SPTR18           EQU  H'0002'
SPTR19           EQU  H'0003'
SPTR20           EQU  H'0004'
SPTR21           EQU  H'0005'


;----- DMANSSZ Bits -----------------------------------------------------
SSZ0             EQU  H'0000'
SSZ1             EQU  H'0001'
SSZ2             EQU  H'0002'
SSZ3             EQU  H'0003'
SSZ4             EQU  H'0004'
SSZ5             EQU  H'0005'
SSZ6             EQU  H'0006'
SSZ7             EQU  H'0007'


;----- DMANSSZH Bits -----------------------------------------------------
SSZ8             EQU  H'0000'
SSZ9             EQU  H'0001'
SSZ10            EQU  H'0002'
SSZ11            EQU  H'0003'


;----- DMANSSA Bits -----------------------------------------------------
SSA0             EQU  H'0000'
SSA1             EQU  H'0001'
SSA2             EQU  H'0002'
SSA3             EQU  H'0003'
SSA4             EQU  H'0004'
SSA5             EQU  H'0005'
SSA6             EQU  H'0006'
SSA7             EQU  H'0007'


;----- DMANSSAH Bits -----------------------------------------------------
SSA8             EQU  H'0000'
SSA9             EQU  H'0001'
SSA10            EQU  H'0002'
SSA11            EQU  H'0003'
SSA12            EQU  H'0004'
SSA13            EQU  H'0005'
SSA14            EQU  H'0006'
SSA15            EQU  H'0007'


;----- DMANSSAU Bits -----------------------------------------------------
SSA16            EQU  H'0000'
SSA17            EQU  H'0001'
SSA18            EQU  H'0002'
SSA19            EQU  H'0003'
SSA20            EQU  H'0004'
SSA21            EQU  H'0005'


;----- DMANCON0 Bits -----------------------------------------------------
XIP              EQU  H'0000'
AIRQEN           EQU  H'0002'
DGO              EQU  H'0005'
SIRQEN           EQU  H'0006'
EN               EQU  H'0007'


;----- DMANCON1 Bits -----------------------------------------------------
SSTP             EQU  H'0000'
DSTP             EQU  H'0005'


;----- DMANAIRQ Bits -----------------------------------------------------
AIRQ0            EQU  H'0000'
AIRQ1            EQU  H'0001'
AIRQ2            EQU  H'0002'
AIRQ3            EQU  H'0003'
AIRQ4            EQU  H'0004'
AIRQ5            EQU  H'0005'
AIRQ6            EQU  H'0006'
AIRQ7            EQU  H'0007'


;----- DMANSIRQ Bits -----------------------------------------------------
SIRQ0            EQU  H'0000'
SIRQ1            EQU  H'0001'
SIRQ2            EQU  H'0002'
SIRQ3            EQU  H'0003'
SIRQ4            EQU  H'0004'
SIRQ5            EQU  H'0005'
SIRQ6            EQU  H'0006'
SIRQ7            EQU  H'0007'


;----- C1CONL Bits -----------------------------------------------------
CLKSEL0          EQU  H'0007'
DNCNT0           EQU  H'0000'
DNCNT1           EQU  H'0001'
DNCNT2           EQU  H'0002'
DNCNT3           EQU  H'0003'
DNCNT4           EQU  H'0004'


;----- C1CONH Bits -----------------------------------------------------
WAKFIL           EQU  H'0000'
BUSY_C1CONH      EQU  H'0003'
SIDL             EQU  H'0005'
FRZ              EQU  H'0006'
ON_C1CONH        EQU  H'0007'
WFT0             EQU  H'0001'
WFT1             EQU  H'0002'


;----- C1CONU Bits -----------------------------------------------------
RTXAT            EQU  H'0000'
ESIGM            EQU  H'0001'
SERR2LOM         EQU  H'0002'
STEF             EQU  H'0003'
TXQEN            EQU  H'0004'
OPMOD0           EQU  H'0005'
OPMOD1           EQU  H'0006'
OPMOD2           EQU  H'0007'


;----- C1CONT Bits -----------------------------------------------------
ABAT             EQU  H'0003'
REQOP0           EQU  H'0000'
REQOP1           EQU  H'0001'
REQOP2           EQU  H'0002'
TXBWS0           EQU  H'0004'
TXBWS1           EQU  H'0005'
TXBWS2           EQU  H'0006'
TXBWS3           EQU  H'0007'


;----- C1NBTCFGL Bits -----------------------------------------------------
SJW0             EQU  H'0000'
SJW1             EQU  H'0001'
SJW2             EQU  H'0002'
SJW3             EQU  H'0003'
SJW4             EQU  H'0004'
SJW5             EQU  H'0005'
SJW6             EQU  H'0006'


;----- C1NBTCFGH Bits -----------------------------------------------------
TSEG20           EQU  H'0000'
TSEG21           EQU  H'0001'
TSEG22           EQU  H'0002'
TSEG23           EQU  H'0003'
TSEG24           EQU  H'0004'
TSEG25           EQU  H'0005'
TSEG26           EQU  H'0006'


;----- C1NBTCFGU Bits -----------------------------------------------------
TSEG10           EQU  H'0000'
TSEG11           EQU  H'0001'
TSEG12           EQU  H'0002'
TSEG13           EQU  H'0003'
TSEG14           EQU  H'0004'
TSEG15           EQU  H'0005'
TSEG16           EQU  H'0006'
TSEG17           EQU  H'0007'


;----- C1NBTCFGT Bits -----------------------------------------------------
BRP0             EQU  H'0000'
BRP1             EQU  H'0001'
BRP2             EQU  H'0002'
BRP3             EQU  H'0003'
BRP4             EQU  H'0004'
BRP5             EQU  H'0005'
BRP6             EQU  H'0006'
BRP7             EQU  H'0007'


;----- C1DBTCFGL Bits -----------------------------------------------------
SJW0             EQU  H'0000'
SJW1             EQU  H'0001'
SJW2             EQU  H'0002'
SJW3             EQU  H'0003'


;----- C1DBTCFGH Bits -----------------------------------------------------
TSEG20           EQU  H'0000'
TSEG21           EQU  H'0001'
TSEG22           EQU  H'0002'
TSEG23           EQU  H'0003'


;----- C1DBTCFGU Bits -----------------------------------------------------
TSEG10           EQU  H'0000'
TSEG11           EQU  H'0001'
TSEG12           EQU  H'0002'
TSEG13           EQU  H'0003'
TSEG14           EQU  H'0004'


;----- C1DBTCFGT Bits -----------------------------------------------------
BRP0             EQU  H'0000'
BRP1             EQU  H'0001'
BRP2             EQU  H'0002'
BRP3             EQU  H'0003'
BRP4             EQU  H'0004'
BRP5             EQU  H'0005'
BRP6             EQU  H'0006'
BRP7             EQU  H'0007'


;----- C1TDCL Bits -----------------------------------------------------
TDCV0            EQU  H'0000'
TDCV1            EQU  H'0001'
TDCV2            EQU  H'0002'
TDCV3            EQU  H'0003'
TDCV4            EQU  H'0004'
TDCV5            EQU  H'0005'


;----- C1TDCH Bits -----------------------------------------------------
TDCO0            EQU  H'0000'
TDCO1            EQU  H'0001'
TDCO2            EQU  H'0002'
TDCO3            EQU  H'0003'
TDCO4            EQU  H'0004'
TDCO5            EQU  H'0005'
TDCO6            EQU  H'0006'


;----- C1TDCU Bits -----------------------------------------------------
TDCMOD0          EQU  H'0000'
TDCMOD1          EQU  H'0001'


;----- C1TBC Bits -----------------------------------------------------
TBC0             EQU  H'0000'
TBC1             EQU  H'0001'
TBC2             EQU  H'0002'
TBC3             EQU  H'0003'
TBC4             EQU  H'0004'
TBC5             EQU  H'0005'
TBC6             EQU  H'0006'
TBC7             EQU  H'0007'


;----- C1TBCH Bits -----------------------------------------------------
TBC8             EQU  H'0000'
TBC9             EQU  H'0001'
TBC10            EQU  H'0002'
TBC11            EQU  H'0003'
TBC12            EQU  H'0004'
TBC13            EQU  H'0005'
TBC14            EQU  H'0006'
TBC15            EQU  H'0007'


;----- C1TBCU Bits -----------------------------------------------------
TBC16            EQU  H'0000'
TBC17            EQU  H'0001'
TBC18            EQU  H'0002'
TBC19            EQU  H'0003'
TBC20            EQU  H'0004'
TBC21            EQU  H'0005'
TBC22            EQU  H'0006'
TBC23            EQU  H'0007'


;----- C1TBCT Bits -----------------------------------------------------
TBC24            EQU  H'0000'
TBC25            EQU  H'0001'
TBC26            EQU  H'0002'
TBC27            EQU  H'0003'
TBC28            EQU  H'0004'
TBC29            EQU  H'0005'
TBC30            EQU  H'0006'
TBC31            EQU  H'0007'


;----- C1TSCONL Bits -----------------------------------------------------
TBCPRE0          EQU  H'0000'
TBCPRE1          EQU  H'0001'
TBCPRE2          EQU  H'0002'
TBCPRE3          EQU  H'0003'
TBCPRE4          EQU  H'0004'
TBCPRE5          EQU  H'0005'
TBCPRE6          EQU  H'0006'
TBCPRE7          EQU  H'0007'


;----- C1TSCONH Bits -----------------------------------------------------
TBCPRE8          EQU  H'0000'
TBCPRE9          EQU  H'0001'


;----- C1TSCONU Bits -----------------------------------------------------
TBCEN            EQU  H'0000'
TSEOF            EQU  H'0001'


;----- C1INTL Bits -----------------------------------------------------
TXIF             EQU  H'0000'
RXIF             EQU  H'0001'
TBCIF            EQU  H'0002'
MODIF            EQU  H'0003'
TEFIF            EQU  H'0004'


;----- C1INTH Bits -----------------------------------------------------
TXATIF           EQU  H'0002'
RXOVIF           EQU  H'0003'
SERRIF           EQU  H'0004'
CERRIF           EQU  H'0005'
WAKIF            EQU  H'0006'
IVMIF            EQU  H'0007'


;----- C1INTU Bits -----------------------------------------------------
TXIE             EQU  H'0000'
RXIE             EQU  H'0001'
TBCIE            EQU  H'0002'
MODIE            EQU  H'0003'
TEFIE            EQU  H'0004'


;----- C1INTT Bits -----------------------------------------------------
TXATIE           EQU  H'0002'
RXOVIE           EQU  H'0003'
SERRIE           EQU  H'0004'
CERRIE           EQU  H'0005'
WAKIE            EQU  H'0006'
IVMIE            EQU  H'0007'


;----- C1RXIF Bits -----------------------------------------------------
RFIF1            EQU  H'0001'
RFIF2            EQU  H'0002'
RFIF3            EQU  H'0003'
RFIF4            EQU  H'0004'
RFIF5            EQU  H'0005'
RFIF6            EQU  H'0006'
RFIF7            EQU  H'0007'


;----- C1RXIFH Bits -----------------------------------------------------
RFIF8            EQU  H'0000'
RFIF9            EQU  H'0001'
RFIF10           EQU  H'0002'
RFIF11           EQU  H'0003'
RFIF12           EQU  H'0004'
RFIF13           EQU  H'0005'
RFIF14           EQU  H'0006'
RFIF15           EQU  H'0007'


;----- C1RXIFU Bits -----------------------------------------------------
RFIF16           EQU  H'0000'
RFIF17           EQU  H'0001'
RFIF18           EQU  H'0002'
RFIF19           EQU  H'0003'
RFIF20           EQU  H'0004'
RFIF21           EQU  H'0005'
RFIF22           EQU  H'0006'
RFIF23           EQU  H'0007'


;----- C1RXIFT Bits -----------------------------------------------------
RFIF24           EQU  H'0000'
RFIF25           EQU  H'0001'
RFIF26           EQU  H'0002'
RFIF27           EQU  H'0003'
RFIF28           EQU  H'0004'
RFIF29           EQU  H'0005'
RFIF30           EQU  H'0006'
RFIF31           EQU  H'0007'


;----- C1TXIF Bits -----------------------------------------------------
TFIF0            EQU  H'0000'
TFIF1            EQU  H'0001'
TFIF2            EQU  H'0002'
TFIF3            EQU  H'0003'
TFIF4            EQU  H'0004'
TFIF5            EQU  H'0005'
TFIF6            EQU  H'0006'
TFIF7            EQU  H'0007'


;----- C1TXIFH Bits -----------------------------------------------------
TFIF8            EQU  H'0000'
TFIF9            EQU  H'0001'
TFIF10           EQU  H'0002'
TFIF11           EQU  H'0003'
TFIF12           EQU  H'0004'
TFIF13           EQU  H'0005'
TFIF14           EQU  H'0006'
TFIF15           EQU  H'0007'


;----- C1TXIFU Bits -----------------------------------------------------
TFIF16           EQU  H'0000'
TFIF17           EQU  H'0001'
TFIF18           EQU  H'0002'
TFIF19           EQU  H'0003'
TFIF20           EQU  H'0004'
TFIF21           EQU  H'0005'
TFIF22           EQU  H'0006'
TFIF23           EQU  H'0007'


;----- C1TXIFT Bits -----------------------------------------------------
TFIF24           EQU  H'0000'
TFIF25           EQU  H'0001'
TFIF26           EQU  H'0002'
TFIF27           EQU  H'0003'
TFIF28           EQU  H'0004'
TFIF29           EQU  H'0005'
TFIF30           EQU  H'0006'
TFIF31           EQU  H'0007'


;----- C1RXOVIF Bits -----------------------------------------------------
RFOVIF1          EQU  H'0001'
RFOVIF2          EQU  H'0002'
RFOVIF3          EQU  H'0003'
RFOVIF4          EQU  H'0004'
RFOVIF5          EQU  H'0005'
RFOVIF6          EQU  H'0006'
RFOVIF7          EQU  H'0007'


;----- C1RXOVIFH Bits -----------------------------------------------------
RFOVIF8          EQU  H'0000'
RFOVIF9          EQU  H'0001'
RFOVIF10         EQU  H'0002'
RFOVIF11         EQU  H'0003'
RFOVIF12         EQU  H'0004'
RFOVIF13         EQU  H'0005'
RFOVIF14         EQU  H'0006'
RFOVIF15         EQU  H'0007'


;----- C1RXOVIFU Bits -----------------------------------------------------
RFOVIF16         EQU  H'0000'
RFOVIF17         EQU  H'0001'
RFOVIF18         EQU  H'0002'
RFOVIF19         EQU  H'0003'
RFOVIF20         EQU  H'0004'
RFOVIF21         EQU  H'0005'
RFOVIF22         EQU  H'0006'
RFOVIF23         EQU  H'0007'


;----- C1RXOVIFT Bits -----------------------------------------------------
RFOVIF24         EQU  H'0000'
RFOVIF25         EQU  H'0001'
RFOVIF26         EQU  H'0002'
RFOVIF27         EQU  H'0003'
RFOVIF28         EQU  H'0004'
RFOVIF29         EQU  H'0005'
RFOVIF30         EQU  H'0006'
RFOVIF31         EQU  H'0007'


;----- C1TXATIF Bits -----------------------------------------------------
TFATIF0          EQU  H'0000'
TFATIF1          EQU  H'0001'
TFATIF2          EQU  H'0002'
TFATIF3          EQU  H'0003'
TFATIF4          EQU  H'0004'
TFATIF5          EQU  H'0005'
TFATIF6          EQU  H'0006'
TFATIF7          EQU  H'0007'


;----- C1TXATIFH Bits -----------------------------------------------------
TFATIF8          EQU  H'0000'
TFATIF9          EQU  H'0001'
TFATIF10         EQU  H'0002'
TFATIF11         EQU  H'0003'
TFATIF12         EQU  H'0004'
TFATIF13         EQU  H'0005'
TFATIF14         EQU  H'0006'
TFATIF15         EQU  H'0007'


;----- C1TXATIFU Bits -----------------------------------------------------
TFATIF16         EQU  H'0000'
TFATIF17         EQU  H'0001'
TFATIF18         EQU  H'0002'
TFATIF19         EQU  H'0003'
TFATIF20         EQU  H'0004'
TFATIF21         EQU  H'0005'
TFATIF22         EQU  H'0006'
TFATIF23         EQU  H'0007'


;----- C1TXATIFT Bits -----------------------------------------------------
TFATIF24         EQU  H'0000'
TFATIF25         EQU  H'0001'
TFATIF26         EQU  H'0002'
TFATIF27         EQU  H'0003'
TFATIF28         EQU  H'0004'
TFATIF29         EQU  H'0005'
TFATIF30         EQU  H'0006'
TFATIF31         EQU  H'0007'


;----- C1TXREQ Bits -----------------------------------------------------
TXREQ0           EQU  H'0000'
TXREQ1           EQU  H'0001'
TXREQ2           EQU  H'0002'
TXREQ3           EQU  H'0003'
TXREQ4           EQU  H'0004'
TXREQ5           EQU  H'0005'
TXREQ6           EQU  H'0006'
TXREQ7           EQU  H'0007'


;----- C1TXREQH Bits -----------------------------------------------------
TXREQ8           EQU  H'0000'
TXREQ9           EQU  H'0001'
TXREQ10          EQU  H'0002'
TXREQ11          EQU  H'0003'
TXREQ12          EQU  H'0004'
TXREQ13          EQU  H'0005'
TXREQ14          EQU  H'0006'
TXREQ15          EQU  H'0007'


;----- C1TXREQU Bits -----------------------------------------------------
TXREQ16          EQU  H'0000'
TXREQ17          EQU  H'0001'
TXREQ18          EQU  H'0002'
TXREQ19          EQU  H'0003'
TXREQ20          EQU  H'0004'
TXREQ21          EQU  H'0005'
TXREQ22          EQU  H'0006'
TXREQ23          EQU  H'0007'


;----- C1TXREQT Bits -----------------------------------------------------
TXREQ24          EQU  H'0000'
TXREQ25          EQU  H'0001'
TXREQ26          EQU  H'0002'
TXREQ27          EQU  H'0003'
TXREQ28          EQU  H'0004'
TXREQ29          EQU  H'0005'
TXREQ30          EQU  H'0006'
TXREQ31          EQU  H'0007'


;----- C1TRECL Bits -----------------------------------------------------
RERRCNT0         EQU  H'0000'
RERRCNT1         EQU  H'0001'
RERRCNT2         EQU  H'0002'
RERRCNT3         EQU  H'0003'
RERRCNT4         EQU  H'0004'
RERRCNT5         EQU  H'0005'
RERRCNT6         EQU  H'0006'
RERRCNT7         EQU  H'0007'


;----- C1TRECH Bits -----------------------------------------------------
TERRCNT0         EQU  H'0000'
TERRCNT1         EQU  H'0001'
TERRCNT2         EQU  H'0002'
TERRCNT3         EQU  H'0003'
TERRCNT4         EQU  H'0004'
TERRCNT5         EQU  H'0005'
TERRCNT6         EQU  H'0006'
TERRCNT7         EQU  H'0007'


;----- C1TRECU Bits -----------------------------------------------------
EWARN            EQU  H'0000'
RXWARN           EQU  H'0001'
TXWARN           EQU  H'0002'
RXBP             EQU  H'0003'
TXBP             EQU  H'0004'
TXBO             EQU  H'0005'


;----- C1BDIAG0L Bits -----------------------------------------------------
NRERRCNT0        EQU  H'0000'
NRERRCNT1        EQU  H'0001'
NRERRCNT2        EQU  H'0002'
NRERRCNT3        EQU  H'0003'
NRERRCNT4        EQU  H'0004'
NRERRCNT5        EQU  H'0005'
NRERRCNT6        EQU  H'0006'
NRERRCNT7        EQU  H'0007'


;----- C1BDIAG0H Bits -----------------------------------------------------
NTERRCNT0        EQU  H'0000'
NTERRCNT1        EQU  H'0001'
NTERRCNT2        EQU  H'0002'
NTERRCNT3        EQU  H'0003'
NTERRCNT4        EQU  H'0004'
NTERRCNT5        EQU  H'0005'
NTERRCNT6        EQU  H'0006'
NTERRCNT7        EQU  H'0007'


;----- C1BDIAG1L Bits -----------------------------------------------------
EFMSGCNT0        EQU  H'0000'
EFMSGCNT1        EQU  H'0001'
EFMSGCNT2        EQU  H'0002'
EFMSGCNT3        EQU  H'0003'
EFMSGCNT4        EQU  H'0004'
EFMSGCNT5        EQU  H'0005'
EFMSGCNT6        EQU  H'0006'
EFMSGCNT7        EQU  H'0007'


;----- C1BDIAG1H Bits -----------------------------------------------------
EFMSGCNT8        EQU  H'0000'
EFMSGCNT9        EQU  H'0001'
EFMSGCNT10       EQU  H'0002'
EFMSGCNT11       EQU  H'0003'
EFMSGCNT12       EQU  H'0004'
EFMSGCNT13       EQU  H'0005'
EFMSGCNT14       EQU  H'0006'
EFMSGCNT15       EQU  H'0007'


;----- C1BDIAG1U Bits -----------------------------------------------------
NBIT0ERR         EQU  H'0000'
NBIT1ERR         EQU  H'0001'
NACKERR          EQU  H'0002'
NFORMERR         EQU  H'0003'
NSTUFERR         EQU  H'0004'
NCRCERR          EQU  H'0005'
TXBOERR          EQU  H'0007'


;----- C1BDIAG1T Bits -----------------------------------------------------
DCRCERR          EQU  H'0005'
ESI              EQU  H'0006'


;----- C1TEFCONL Bits -----------------------------------------------------
TEFNEIE          EQU  H'0000'
TEFHIE           EQU  H'0001'
TEFFIE           EQU  H'0002'
TEFOVIE          EQU  H'0003'
TEFTSEN          EQU  H'0005'


;----- C1TEFCONH Bits -----------------------------------------------------
UINC             EQU  H'0000'
FRESET           EQU  H'0002'


;----- C1TEFCONT Bits -----------------------------------------------------
FSIZE0           EQU  H'0000'
FSIZE1           EQU  H'0001'
FSIZE2           EQU  H'0002'
FSIZE3           EQU  H'0003'
FSIZE4           EQU  H'0004'


;----- C1TEFSTAL Bits -----------------------------------------------------
TEFNEIF          EQU  H'0000'
TEFHIF           EQU  H'0001'
TEFFIF           EQU  H'0002'
TEFOVIF          EQU  H'0003'


;----- C1TEFUA Bits -----------------------------------------------------
TEFUA0           EQU  H'0000'
TEFUA1           EQU  H'0001'
TEFUA2           EQU  H'0002'
TEFUA3           EQU  H'0003'
TEFUA4           EQU  H'0004'
TEFUA5           EQU  H'0005'
TEFUA6           EQU  H'0006'
TEFUA7           EQU  H'0007'


;----- C1TEFUAH Bits -----------------------------------------------------
TEFUA8           EQU  H'0000'
TEFUA9           EQU  H'0001'
TEFUA10          EQU  H'0002'
TEFUA11          EQU  H'0003'
TEFUA12          EQU  H'0004'
TEFUA13          EQU  H'0005'
TEFUA14          EQU  H'0006'
TEFUA15          EQU  H'0007'


;----- C1TEFUAU Bits -----------------------------------------------------
TEFUA16          EQU  H'0000'
TEFUA17          EQU  H'0001'
TEFUA18          EQU  H'0002'
TEFUA19          EQU  H'0003'
TEFUA20          EQU  H'0004'
TEFUA21          EQU  H'0005'
TEFUA22          EQU  H'0006'
TEFUA23          EQU  H'0007'


;----- C1TEFUAT Bits -----------------------------------------------------
TEFUA24          EQU  H'0000'
TEFUA25          EQU  H'0001'
TEFUA26          EQU  H'0002'
TEFUA27          EQU  H'0003'
TEFUA28          EQU  H'0004'
TEFUA29          EQU  H'0005'
TEFUA30          EQU  H'0006'
TEFUA31          EQU  H'0007'


;----- C1FIFOBA Bits -----------------------------------------------------
FIFOBA0          EQU  H'0000'
FIFOBA1          EQU  H'0001'
FIFOBA2          EQU  H'0002'
FIFOBA3          EQU  H'0003'
FIFOBA4          EQU  H'0004'
FIFOBA5          EQU  H'0005'
FIFOBA6          EQU  H'0006'
FIFOBA7          EQU  H'0007'


;----- C1FIFOBAH Bits -----------------------------------------------------
FIFOBA8          EQU  H'0000'
FIFOBA9          EQU  H'0001'
FIFOBA10         EQU  H'0002'
FIFOBA11         EQU  H'0003'
FIFOBA12         EQU  H'0004'
FIFOBA13         EQU  H'0005'
FIFOBA14         EQU  H'0006'
FIFOBA15         EQU  H'0007'


;----- C1FIFOBAU Bits -----------------------------------------------------
FIFOBA16         EQU  H'0000'
FIFOBA17         EQU  H'0001'
FIFOBA18         EQU  H'0002'
FIFOBA19         EQU  H'0003'
FIFOBA20         EQU  H'0004'
FIFOBA21         EQU  H'0005'
FIFOBA22         EQU  H'0006'
FIFOBA23         EQU  H'0007'


;----- C1FIFOBAT Bits -----------------------------------------------------
FIFOBA24         EQU  H'0000'
FIFOBA25         EQU  H'0001'
FIFOBA26         EQU  H'0002'
FIFOBA27         EQU  H'0003'
FIFOBA28         EQU  H'0004'
FIFOBA29         EQU  H'0005'
FIFOBA30         EQU  H'0006'
FIFOBA31         EQU  H'0007'


;----- C1TXQCONL Bits -----------------------------------------------------
TXQNIE           EQU  H'0000'
TXQEIE           EQU  H'0002'
TXATIE           EQU  H'0004'
TXEN             EQU  H'0007'


;----- C1TXQCONH Bits -----------------------------------------------------
UINC             EQU  H'0000'
TXREQ            EQU  H'0001'
FRESET           EQU  H'0002'


;----- C1TXQCONU Bits -----------------------------------------------------
TXPRI0           EQU  H'0000'
TXPRI1           EQU  H'0001'
TXPRI2           EQU  H'0002'
TXPRI3           EQU  H'0003'
TXPRI4           EQU  H'0004'
TXAT0            EQU  H'0005'
TXAT1            EQU  H'0006'


;----- C1TXQCONT Bits -----------------------------------------------------
FSIZE0           EQU  H'0000'
FSIZE1           EQU  H'0001'
FSIZE2           EQU  H'0002'
FSIZE3           EQU  H'0003'
FSIZE4           EQU  H'0004'


;----- C1TXQSTAL Bits -----------------------------------------------------
TXQNIF           EQU  H'0000'
TXQEIF           EQU  H'0002'
TXATIF           EQU  H'0004'
TXERR            EQU  H'0005'
TXLARB           EQU  H'0006'
TXABT            EQU  H'0007'


;----- C1TXQSTAH Bits -----------------------------------------------------
TXQCI0           EQU  H'0000'
TXQCI1           EQU  H'0001'
TXQCI2           EQU  H'0002'
TXQCI3           EQU  H'0003'
TXQCI4           EQU  H'0004'


;----- C1TXQUA Bits -----------------------------------------------------
TXQUA0           EQU  H'0000'
TXQUA1           EQU  H'0001'
TXQUA2           EQU  H'0002'
TXQUA3           EQU  H'0003'
TXQUA4           EQU  H'0004'
TXQUA5           EQU  H'0005'
TXQUA6           EQU  H'0006'
TXQUA7           EQU  H'0007'


;----- C1TXQUAH Bits -----------------------------------------------------
TXQUA8           EQU  H'0000'
TXQUA9           EQU  H'0001'
TXQUA10          EQU  H'0002'
TXQUA11          EQU  H'0003'
TXQUA12          EQU  H'0004'
TXQUA13          EQU  H'0005'
TXQUA14          EQU  H'0006'
TXQUA15          EQU  H'0007'


;----- C1TXQUAU Bits -----------------------------------------------------
TXQUA16          EQU  H'0000'
TXQUA17          EQU  H'0001'
TXQUA18          EQU  H'0002'
TXQUA19          EQU  H'0003'
TXQUA20          EQU  H'0004'
TXQUA21          EQU  H'0005'
TXQUA22          EQU  H'0006'
TXQUA23          EQU  H'0007'


;----- C1TXQUAT Bits -----------------------------------------------------
TXQUA24          EQU  H'0000'
TXQUA25          EQU  H'0001'
TXQUA26          EQU  H'0002'
TXQUA27          EQU  H'0003'
TXQUA28          EQU  H'0004'
TXQUA29          EQU  H'0005'
TXQUA30          EQU  H'0006'
TXQUA31          EQU  H'0007'


;----- C1FIFOCON2 Bits -----------------------------------------------------
TFNRFNIE         EQU  H'0000'
TFHRFHIE         EQU  H'0001'
TFERFFIE         EQU  H'0002'
RXOVIE           EQU  H'0003'
TXATIE           EQU  H'0004'
RXTSEN           EQU  H'0005'
RTREN            EQU  H'0006'
TXEN             EQU  H'0007'


;----- C1FIFOCON2H Bits -----------------------------------------------------
UINC             EQU  H'0000'
TXREQ            EQU  H'0001'
FRESET           EQU  H'0002'


;----- C1FIFOCON2U Bits -----------------------------------------------------
TXPRI0           EQU  H'0000'
TXPRI1           EQU  H'0001'
TXPRI2           EQU  H'0002'
TXPRI3           EQU  H'0003'
TXPRI4           EQU  H'0004'
TXAT0            EQU  H'0005'
TXAT1            EQU  H'0006'


;----- C1FIFOCON2T Bits -----------------------------------------------------
FSIZE0           EQU  H'0000'
FSIZE1           EQU  H'0001'
FSIZE2           EQU  H'0002'
FSIZE3           EQU  H'0003'
FSIZE4           EQU  H'0004'


;----- C1FIFOSTA2 Bits -----------------------------------------------------
TFNRFNIF         EQU  H'0000'
TFHRFHIF         EQU  H'0001'
TFERFFIF         EQU  H'0002'
RXOVIF           EQU  H'0003'
TXATIF           EQU  H'0004'
TXERR            EQU  H'0005'
TXLARB           EQU  H'0006'
TXABT            EQU  H'0007'


;----- C1FIFOSTA2H Bits -----------------------------------------------------
FIFOCI0          EQU  H'0000'
FIFOCI1          EQU  H'0001'
FIFOCI2          EQU  H'0002'
FIFOCI3          EQU  H'0003'
FIFOCI4          EQU  H'0004'


;----- C1FIFOUA2 Bits -----------------------------------------------------
FIFOUA0          EQU  H'0000'
FIFOUA1          EQU  H'0001'
FIFOUA2          EQU  H'0002'
FIFOUA3          EQU  H'0003'
FIFOUA4          EQU  H'0004'
FIFOUA5          EQU  H'0005'
FIFOUA6          EQU  H'0006'
FIFOUA7          EQU  H'0007'


;----- C1FIFOUA2H Bits -----------------------------------------------------
FIFOUA8          EQU  H'0000'
FIFOUA9          EQU  H'0001'
FIFOUA10         EQU  H'0002'
FIFOUA11         EQU  H'0003'
FIFOUA12         EQU  H'0004'
FIFOUA13         EQU  H'0005'
FIFOUA14         EQU  H'0006'
FIFOUA15         EQU  H'0007'


;----- C1FIFOUA2U Bits -----------------------------------------------------
FIFOUA16         EQU  H'0000'
FIFOUA17         EQU  H'0001'
FIFOUA18         EQU  H'0002'
FIFOUA19         EQU  H'0003'
FIFOUA20         EQU  H'0004'
FIFOUA21         EQU  H'0005'
FIFOUA22         EQU  H'0006'
FIFOUA23         EQU  H'0007'


;----- C1FIFOUA2T Bits -----------------------------------------------------
FIFOUA24         EQU  H'0000'
FIFOUA25         EQU  H'0001'
FIFOUA26         EQU  H'0002'
FIFOUA27         EQU  H'0003'
FIFOUA28         EQU  H'0004'
FIFOUA29         EQU  H'0005'
FIFOUA30         EQU  H'0006'
FIFOUA31         EQU  H'0007'


;----- C1FIFOCON3 Bits -----------------------------------------------------
TFNRFNIE         EQU  H'0000'
TFHRFHIE         EQU  H'0001'
TFERFFIE         EQU  H'0002'
RXOVIE           EQU  H'0003'
TXATIE           EQU  H'0004'
RXTSEN           EQU  H'0005'
RTREN            EQU  H'0006'
TXEN             EQU  H'0007'


;----- C1FIFOCON3H Bits -----------------------------------------------------
UINC             EQU  H'0000'
TXREQ            EQU  H'0001'
FRESET           EQU  H'0002'


;----- C1FIFOCON3U Bits -----------------------------------------------------
TXPRI0           EQU  H'0000'
TXPRI1           EQU  H'0001'
TXPRI2           EQU  H'0002'
TXPRI3           EQU  H'0003'
TXPRI4           EQU  H'0004'
TXAT0            EQU  H'0005'
TXAT1            EQU  H'0006'


;----- C1FIFOCON3T Bits -----------------------------------------------------
FSIZE0           EQU  H'0000'
FSIZE1           EQU  H'0001'
FSIZE2           EQU  H'0002'
FSIZE3           EQU  H'0003'
FSIZE4           EQU  H'0004'


;----- C1FIFOSTA3 Bits -----------------------------------------------------
TFNRFNIF         EQU  H'0000'
TFHRFHIF         EQU  H'0001'
TFERFFIF         EQU  H'0002'
RXOVIF           EQU  H'0003'
TXATIF           EQU  H'0004'
TXERR            EQU  H'0005'
TXLARB           EQU  H'0006'
TXABT            EQU  H'0007'


;----- C1FIFOSTA3H Bits -----------------------------------------------------
FIFOCI0          EQU  H'0000'
FIFOCI1          EQU  H'0001'
FIFOCI2          EQU  H'0002'
FIFOCI3          EQU  H'0003'
FIFOCI4          EQU  H'0004'


;----- C1FIFOUA3 Bits -----------------------------------------------------
FIFOUA0          EQU  H'0000'
FIFOUA1          EQU  H'0001'
FIFOUA2          EQU  H'0002'
FIFOUA3          EQU  H'0003'
FIFOUA4          EQU  H'0004'
FIFOUA5          EQU  H'0005'
FIFOUA6          EQU  H'0006'
FIFOUA7          EQU  H'0007'


;----- C1FIFOUA3H Bits -----------------------------------------------------
FIFOUA8          EQU  H'0000'
FIFOUA9          EQU  H'0001'
FIFOUA10         EQU  H'0002'
FIFOUA11         EQU  H'0003'
FIFOUA12         EQU  H'0004'
FIFOUA13         EQU  H'0005'
FIFOUA14         EQU  H'0006'
FIFOUA15         EQU  H'0007'


;----- C1FIFOUA3U Bits -----------------------------------------------------
FIFOUA16         EQU  H'0000'
FIFOUA17         EQU  H'0001'
FIFOUA18         EQU  H'0002'
FIFOUA19         EQU  H'0003'
FIFOUA20         EQU  H'0004'
FIFOUA21         EQU  H'0005'
FIFOUA22         EQU  H'0006'
FIFOUA23         EQU  H'0007'


;----- C1FIFOUA3T Bits -----------------------------------------------------
FIFOUA24         EQU  H'0000'
FIFOUA25         EQU  H'0001'
FIFOUA26         EQU  H'0002'
FIFOUA27         EQU  H'0003'
FIFOUA28         EQU  H'0004'
FIFOUA29         EQU  H'0005'
FIFOUA30         EQU  H'0006'
FIFOUA31         EQU  H'0007'


;----- C1FLTCON0L Bits -----------------------------------------------------
FLTEN0           EQU  H'0007'
F0BP0            EQU  H'0000'
F0BP1            EQU  H'0001'
F0BP2            EQU  H'0002'
F0BP3            EQU  H'0003'
F0BP4            EQU  H'0004'


;----- C1FLTCON0H Bits -----------------------------------------------------
FLTEN1           EQU  H'0007'
F1BP0            EQU  H'0000'
F1BP1            EQU  H'0001'
F1BP2            EQU  H'0002'
F1BP3            EQU  H'0003'
F1BP4            EQU  H'0004'


;----- C1FLTCON0U Bits -----------------------------------------------------
FLTEN2           EQU  H'0007'
F2BP0            EQU  H'0000'
F2BP1            EQU  H'0001'
F2BP2            EQU  H'0002'
F2BP3            EQU  H'0003'
F2BP4            EQU  H'0004'


;----- C1FLTCON0T Bits -----------------------------------------------------
FLTEN3           EQU  H'0007'
F3BP0            EQU  H'0000'
F3BP1            EQU  H'0001'
F3BP2            EQU  H'0002'
F3BP3            EQU  H'0003'
F3BP4            EQU  H'0004'


;----- C1FLTCON2L Bits -----------------------------------------------------
FLTEN8           EQU  H'0007'
F8BP0            EQU  H'0000'
F8BP1            EQU  H'0001'
F8BP2            EQU  H'0002'
F8BP3            EQU  H'0003'
F8BP4            EQU  H'0004'


;----- C1FLTCON2H Bits -----------------------------------------------------
FLTEN9           EQU  H'0007'
F9BP0            EQU  H'0000'
F9BP1            EQU  H'0001'
F9BP2            EQU  H'0002'
F9BP3            EQU  H'0003'
F9BP4            EQU  H'0004'


;----- C1FLTCON2U Bits -----------------------------------------------------
FLTEN10          EQU  H'0007'
F10BP0           EQU  H'0000'
F10BP1           EQU  H'0001'
F10BP2           EQU  H'0002'
F10BP3           EQU  H'0003'
F10BP4           EQU  H'0004'


;----- C1FLTCON2T Bits -----------------------------------------------------
FLTEN11          EQU  H'0007'
F11BP0           EQU  H'0000'
F11BP1           EQU  H'0001'
F11BP2           EQU  H'0002'
F11BP3           EQU  H'0003'
F11BP4           EQU  H'0004'


;----- C1FLTOBJ0 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ0H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ0U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ0T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK0 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK0H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK0U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK0T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ2 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ2H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ2U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ2T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK2 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK2H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK2U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK2T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ3 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ3H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ3U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ3T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK3 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK3H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK3U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK3T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ4 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ4H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ4U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ4T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK4 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK4H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK4U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK4T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ5 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ5H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ5U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ5T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK5 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK5H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK5U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK5T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ6 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ6H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ6U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ6T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK6 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK6H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK6U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK6T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ7 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ7H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ7U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ7T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK7 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK7H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK7U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK7T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ8 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ8H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ8U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ8T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK8 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK8H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK8U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK8T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ9 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ9H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ9U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ9T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK9 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK9H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK9U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK9T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ10 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ10H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ10U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ10T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK10 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK10H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK10U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK10T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- C1FLTOBJ11 Bits -----------------------------------------------------
SID0             EQU  H'0000'
SID1             EQU  H'0001'
SID2             EQU  H'0002'
SID3             EQU  H'0003'
SID4             EQU  H'0004'
SID5             EQU  H'0005'
SID6             EQU  H'0006'
SID7             EQU  H'0007'


;----- C1FLTOBJ11H Bits -----------------------------------------------------
SID8             EQU  H'0000'
SID9             EQU  H'0001'
SID10            EQU  H'0002'
EID0             EQU  H'0003'
EID1             EQU  H'0004'
EID2             EQU  H'0005'
EID3             EQU  H'0006'
EID4             EQU  H'0007'


;----- C1FLTOBJ11U Bits -----------------------------------------------------
EID5             EQU  H'0000'
EID6             EQU  H'0001'
EID7             EQU  H'0002'
EID8             EQU  H'0003'
EID9             EQU  H'0004'
EID10            EQU  H'0005'
EID11            EQU  H'0006'
EID12            EQU  H'0007'


;----- C1FLTOBJ11T Bits -----------------------------------------------------
SID11            EQU  H'0005'
EXIDE            EQU  H'0006'
EID13            EQU  H'0000'
EID14            EQU  H'0001'
EID15            EQU  H'0002'
EID16            EQU  H'0003'
EID17            EQU  H'0004'


;----- C1MASK11 Bits -----------------------------------------------------
MSID0            EQU  H'0000'
MSID1            EQU  H'0001'
MSID2            EQU  H'0002'
MSID3            EQU  H'0003'
MSID4            EQU  H'0004'
MSID5            EQU  H'0005'
MSID6            EQU  H'0006'
MSID7            EQU  H'0007'


;----- C1MASK11H Bits -----------------------------------------------------
MSID8            EQU  H'0000'
MSID9            EQU  H'0001'
MSID10           EQU  H'0002'
MEID0            EQU  H'0003'
MEID1            EQU  H'0004'
MEID2            EQU  H'0005'
MEID3            EQU  H'0006'
MEID4            EQU  H'0007'


;----- C1MASK11U Bits -----------------------------------------------------
MEID5            EQU  H'0000'
MEID6            EQU  H'0001'
MEID7            EQU  H'0002'
MEID8            EQU  H'0003'
MEID9            EQU  H'0004'
MEID10           EQU  H'0005'
MEID11           EQU  H'0006'
MEID12           EQU  H'0007'


;----- C1MASK11T Bits -----------------------------------------------------
MSID11           EQU  H'0005'
MIDE             EQU  H'0006'
MEID13           EQU  H'0000'
MEID14           EQU  H'0001'
MEID15           EQU  H'0002'
MEID16           EQU  H'0003'
MEID17           EQU  H'0004'


;----- PPSLOCK Bits -----------------------------------------------------
PPSLOCKED        EQU  H'0000'


;----- RA0PPS Bits -----------------------------------------------------
RA0PPS0          EQU  H'0000'
RA0PPS1          EQU  H'0001'
RA0PPS2          EQU  H'0002'
RA0PPS3          EQU  H'0003'
RA0PPS4          EQU  H'0004'
RA0PPS5          EQU  H'0005'


;----- RA2PPS Bits -----------------------------------------------------
RA2PPS0          EQU  H'0000'
RA2PPS1          EQU  H'0001'
RA2PPS2          EQU  H'0002'
RA2PPS3          EQU  H'0003'
RA2PPS4          EQU  H'0004'
RA2PPS5          EQU  H'0005'


;----- RA3PPS Bits -----------------------------------------------------
RA3PPS0          EQU  H'0000'
RA3PPS1          EQU  H'0001'
RA3PPS2          EQU  H'0002'
RA3PPS3          EQU  H'0003'
RA3PPS4          EQU  H'0004'
RA3PPS5          EQU  H'0005'


;----- RA4PPS Bits -----------------------------------------------------
RA4PPS0          EQU  H'0000'
RA4PPS1          EQU  H'0001'
RA4PPS2          EQU  H'0002'
RA4PPS3          EQU  H'0003'
RA4PPS4          EQU  H'0004'
RA4PPS5          EQU  H'0005'


;----- RA5PPS Bits -----------------------------------------------------
RA5PPS0          EQU  H'0000'
RA5PPS1          EQU  H'0001'
RA5PPS2          EQU  H'0002'
RA5PPS3          EQU  H'0003'
RA5PPS4          EQU  H'0004'
RA5PPS5          EQU  H'0005'


;----- RA6PPS Bits -----------------------------------------------------
RA6PPS0          EQU  H'0000'
RA6PPS1          EQU  H'0001'
RA6PPS2          EQU  H'0002'
RA6PPS3          EQU  H'0003'
RA6PPS4          EQU  H'0004'
RA6PPS5          EQU  H'0005'


;----- RA7PPS Bits -----------------------------------------------------
RA7PPS0          EQU  H'0000'
RA7PPS1          EQU  H'0001'
RA7PPS2          EQU  H'0002'
RA7PPS3          EQU  H'0003'
RA7PPS4          EQU  H'0004'
RA7PPS5          EQU  H'0005'


;----- RB0PPS Bits -----------------------------------------------------
RB0PPS0          EQU  H'0000'
RB0PPS1          EQU  H'0001'
RB0PPS2          EQU  H'0002'
RB0PPS3          EQU  H'0003'
RB0PPS4          EQU  H'0004'
RB0PPS5          EQU  H'0005'


;----- RB2PPS Bits -----------------------------------------------------
RB2PPS0          EQU  H'0000'
RB2PPS1          EQU  H'0001'
RB2PPS2          EQU  H'0002'
RB2PPS3          EQU  H'0003'
RB2PPS4          EQU  H'0004'
RB2PPS5          EQU  H'0005'


;----- RB3PPS Bits -----------------------------------------------------
RB3PPS0          EQU  H'0000'
RB3PPS1          EQU  H'0001'
RB3PPS2          EQU  H'0002'
RB3PPS3          EQU  H'0003'
RB3PPS4          EQU  H'0004'
RB3PPS5          EQU  H'0005'


;----- RB4PPS Bits -----------------------------------------------------
RB4PPS0          EQU  H'0000'
RB4PPS1          EQU  H'0001'
RB4PPS2          EQU  H'0002'
RB4PPS3          EQU  H'0003'
RB4PPS4          EQU  H'0004'
RB4PPS5          EQU  H'0005'


;----- RB5PPS Bits -----------------------------------------------------
RB5PPS0          EQU  H'0000'
RB5PPS1          EQU  H'0001'
RB5PPS2          EQU  H'0002'
RB5PPS3          EQU  H'0003'
RB5PPS4          EQU  H'0004'
RB5PPS5          EQU  H'0005'


;----- RB6PPS Bits -----------------------------------------------------
RB6PPS0          EQU  H'0000'
RB6PPS1          EQU  H'0001'
RB6PPS2          EQU  H'0002'
RB6PPS3          EQU  H'0003'
RB6PPS4          EQU  H'0004'
RB6PPS5          EQU  H'0005'


;----- RB7PPS Bits -----------------------------------------------------
RB7PPS0          EQU  H'0000'
RB7PPS1          EQU  H'0001'
RB7PPS2          EQU  H'0002'
RB7PPS3          EQU  H'0003'
RB7PPS4          EQU  H'0004'
RB7PPS5          EQU  H'0005'


;----- RC0PPS Bits -----------------------------------------------------
RC0PPS0          EQU  H'0000'
RC0PPS1          EQU  H'0001'
RC0PPS2          EQU  H'0002'
RC0PPS3          EQU  H'0003'
RC0PPS4          EQU  H'0004'
RC0PPS5          EQU  H'0005'


;----- RC2PPS Bits -----------------------------------------------------
RC2PPS0          EQU  H'0000'
RC2PPS1          EQU  H'0001'
RC2PPS2          EQU  H'0002'
RC2PPS3          EQU  H'0003'
RC2PPS4          EQU  H'0004'
RC2PPS5          EQU  H'0005'


;----- RC3PPS Bits -----------------------------------------------------
RC3PPS0          EQU  H'0000'
RC3PPS1          EQU  H'0001'
RC3PPS2          EQU  H'0002'
RC3PPS3          EQU  H'0003'
RC3PPS4          EQU  H'0004'
RC3PPS5          EQU  H'0005'


;----- RC4PPS Bits -----------------------------------------------------
RC4PPS0          EQU  H'0000'
RC4PPS1          EQU  H'0001'
RC4PPS2          EQU  H'0002'
RC4PPS3          EQU  H'0003'
RC4PPS4          EQU  H'0004'
RC4PPS5          EQU  H'0005'


;----- RC5PPS Bits -----------------------------------------------------
RC5PPS0          EQU  H'0000'
RC5PPS1          EQU  H'0001'
RC5PPS2          EQU  H'0002'
RC5PPS3          EQU  H'0003'
RC5PPS4          EQU  H'0004'
RC5PPS5          EQU  H'0005'


;----- RC6PPS Bits -----------------------------------------------------
RC6PPS0          EQU  H'0000'
RC6PPS1          EQU  H'0001'
RC6PPS2          EQU  H'0002'
RC6PPS3          EQU  H'0003'
RC6PPS4          EQU  H'0004'
RC6PPS5          EQU  H'0005'


;----- RC7PPS Bits -----------------------------------------------------
RC7PPS0          EQU  H'0000'
RC7PPS1          EQU  H'0001'
RC7PPS2          EQU  H'0002'
RC7PPS3          EQU  H'0003'
RC7PPS4          EQU  H'0004'
RC7PPS5          EQU  H'0005'


;----- CANRXPPS Bits -----------------------------------------------------
CANRXPPS0        EQU  H'0000'
CANRXPPS1        EQU  H'0001'
CANRXPPS2        EQU  H'0002'
CANRXPPS3        EQU  H'0003'
CANRXPPS4        EQU  H'0004'


;----- INT0PPS Bits -----------------------------------------------------
PORT             EQU  H'0003'
INT0PPS0         EQU  H'0000'
INT0PPS1         EQU  H'0001'
INT0PPS2         EQU  H'0002'
INT0PPS3         EQU  H'0003'


;----- INT2PPS Bits -----------------------------------------------------
INT2PPS0         EQU  H'0000'
INT2PPS1         EQU  H'0001'
INT2PPS2         EQU  H'0002'
INT2PPS3         EQU  H'0003'
INT2PPS4         EQU  H'0004'
INT2PPS5         EQU  H'0005'


;----- T0CKIPPS Bits -----------------------------------------------------
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'
T0CKIPPS5        EQU  H'0005'


;----- T3CKIPPS Bits -----------------------------------------------------
T3CKIPPS0        EQU  H'0000'
T3CKIPPS1        EQU  H'0001'
T3CKIPPS2        EQU  H'0002'
T3CKIPPS3        EQU  H'0003'
T3CKIPPS4        EQU  H'0004'
T3CKIPPS5        EQU  H'0005'


;----- T3GPPS Bits -----------------------------------------------------
T3GPPS0          EQU  H'0000'
T3GPPS1          EQU  H'0001'
T3GPPS2          EQU  H'0002'
T3GPPS3          EQU  H'0003'
T3GPPS4          EQU  H'0004'


;----- T5CKIPPS Bits -----------------------------------------------------
T5CKIPPS0        EQU  H'0000'
T5CKIPPS1        EQU  H'0001'
T5CKIPPS2        EQU  H'0002'
T5CKIPPS3        EQU  H'0003'
T5CKIPPS4        EQU  H'0004'
T5CKIPPS5        EQU  H'0005'


;----- T5GPPS Bits -----------------------------------------------------
T5GPPS0          EQU  H'0000'
T5GPPS1          EQU  H'0001'
T5GPPS2          EQU  H'0002'
T5GPPS3          EQU  H'0003'
T5GPPS4          EQU  H'0004'


;----- T2INPPS Bits -----------------------------------------------------
T2INPPS0         EQU  H'0000'
T2INPPS1         EQU  H'0001'
T2INPPS2         EQU  H'0002'
T2INPPS3         EQU  H'0003'
T2INPPS4         EQU  H'0004'


;----- T4INPPS Bits -----------------------------------------------------
T4INPPS0         EQU  H'0000'
T4INPPS1         EQU  H'0001'
T4INPPS2         EQU  H'0002'
T4INPPS3         EQU  H'0003'
T4INPPS4         EQU  H'0004'


;----- T6INPPS Bits -----------------------------------------------------
T6INPPS0         EQU  H'0000'
T6INPPS1         EQU  H'0001'
T6INPPS2         EQU  H'0002'
T6INPPS3         EQU  H'0003'
T6INPPS4         EQU  H'0004'


;----- TUIN0PPS Bits -----------------------------------------------------
TUIN0PPS0        EQU  H'0000'
TUIN0PPS1        EQU  H'0001'
TUIN0PPS2        EQU  H'0002'
TUIN0PPS3        EQU  H'0003'
TUIN0PPS4        EQU  H'0004'
TUIN0PPS5        EQU  H'0005'


;----- TUIN2PPS Bits -----------------------------------------------------
TUIN2PPS0        EQU  H'0000'
TUIN2PPS1        EQU  H'0001'
TUIN2PPS2        EQU  H'0002'
TUIN2PPS3        EQU  H'0003'
TUIN2PPS4        EQU  H'0004'


;----- TUIN3PPS Bits -----------------------------------------------------
TUIN3PPS0        EQU  H'0000'
TUIN3PPS1        EQU  H'0001'
TUIN3PPS2        EQU  H'0002'
TUIN3PPS3        EQU  H'0003'
TUIN3PPS4        EQU  H'0004'


;----- CCP2PPS Bits -----------------------------------------------------
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'
CCP2PPS5         EQU  H'0005'


;----- CCP3PPS Bits -----------------------------------------------------
CCP3PPS0         EQU  H'0000'
CCP3PPS1         EQU  H'0001'
CCP3PPS2         EQU  H'0002'
CCP3PPS3         EQU  H'0003'
CCP3PPS4         EQU  H'0004'


;----- PWM2ERSPPS Bits -----------------------------------------------------
PWM2ERSPPS0      EQU  H'0000'
PWM2ERSPPS1      EQU  H'0001'
PWM2ERSPPS2      EQU  H'0002'
PWM2ERSPPS3      EQU  H'0003'
PWM2ERSPPS4      EQU  H'0004'
PWM2ERSPPS5      EQU  H'0005'


;----- PWM3ERSPPS Bits -----------------------------------------------------
PWM3ERSPPS0      EQU  H'0000'
PWM3ERSPPS1      EQU  H'0001'
PWM3ERSPPS2      EQU  H'0002'
PWM3ERSPPS3      EQU  H'0003'
PWM3ERSPPS4      EQU  H'0004'


;----- PWMIN0PPS Bits -----------------------------------------------------
PWMIN0PPS0       EQU  H'0000'
PWMIN0PPS1       EQU  H'0001'
PWMIN0PPS2       EQU  H'0002'
PWMIN0PPS3       EQU  H'0003'
PWMIN0PPS4       EQU  H'0004'
PWMIN0PPS5       EQU  H'0005'


;----- PWMIN1PPS Bits -----------------------------------------------------
PWMIN1PPS0       EQU  H'0000'
PWMIN1PPS1       EQU  H'0001'
PWMIN1PPS2       EQU  H'0002'
PWMIN1PPS3       EQU  H'0003'
PWMIN1PPS4       EQU  H'0004'
PWMIN1PPS5       EQU  H'0005'


;----- SMT1WINPPS Bits -----------------------------------------------------
SMT1WINPPS0      EQU  H'0000'
SMT1WINPPS1      EQU  H'0001'
SMT1WINPPS2      EQU  H'0002'
SMT1WINPPS3      EQU  H'0003'
SMT1WINPPS4      EQU  H'0004'
SMT1WINPPS5      EQU  H'0005'


;----- SMT1SIGPPS Bits -----------------------------------------------------
SMT1SIGPPS0      EQU  H'0000'
SMT1SIGPPS1      EQU  H'0001'
SMT1SIGPPS2      EQU  H'0002'
SMT1SIGPPS3      EQU  H'0003'
SMT1SIGPPS4      EQU  H'0004'
SMT1SIGPPS5      EQU  H'0005'


;----- CWG2PPS Bits -----------------------------------------------------
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'
CWG2INPPS0       EQU  H'0000'
CWG2INPPS1       EQU  H'0001'
CWG2INPPS2       EQU  H'0002'
CWG2INPPS3       EQU  H'0003'
CWG2INPPS4       EQU  H'0004'


;----- CWG3PPS Bits -----------------------------------------------------
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'
CWG3INPPS0       EQU  H'0000'
CWG3INPPS1       EQU  H'0001'
CWG3INPPS2       EQU  H'0002'
CWG3INPPS3       EQU  H'0003'
CWG3INPPS4       EQU  H'0004'


;----- MD1CARLPPS Bits -----------------------------------------------------
MDCARLPPS0       EQU  H'0000'
MDCARLPPS1       EQU  H'0001'
MDCARLPPS2       EQU  H'0002'
MDCARLPPS3       EQU  H'0003'
MDCARLPPS4       EQU  H'0004'


;----- MD1CARHPPS Bits -----------------------------------------------------
MDCARHPPS0       EQU  H'0000'
MDCARHPPS1       EQU  H'0001'
MDCARHPPS2       EQU  H'0002'
MDCARHPPS3       EQU  H'0003'
MDCARHPPS4       EQU  H'0004'


;----- MD1SRCPPS Bits -----------------------------------------------------
MDSRCPPS0        EQU  H'0000'
MDSRCPPS1        EQU  H'0001'
MDSRCPPS2        EQU  H'0002'
MDSRCPPS3        EQU  H'0003'
MDSRCPPS4        EQU  H'0004'


;----- CLCIN0PPS Bits -----------------------------------------------------
CLCIN0PPS0       EQU  H'0000'
CLCIN0PPS1       EQU  H'0001'
CLCIN0PPS2       EQU  H'0002'
CLCIN0PPS3       EQU  H'0003'
CLCIN0PPS4       EQU  H'0004'


;----- CLCIN2PPS Bits -----------------------------------------------------
CLCIN2PPS0       EQU  H'0000'
CLCIN2PPS1       EQU  H'0001'
CLCIN2PPS2       EQU  H'0002'
CLCIN2PPS3       EQU  H'0003'
CLCIN2PPS4       EQU  H'0004'


;----- CLCIN3PPS Bits -----------------------------------------------------
CLCIN3PPS0       EQU  H'0000'
CLCIN3PPS1       EQU  H'0001'
CLCIN3PPS2       EQU  H'0002'
CLCIN3PPS3       EQU  H'0003'
CLCIN3PPS4       EQU  H'0004'


;----- CLCIN4PPS Bits -----------------------------------------------------
CLCIN4PPS0       EQU  H'0000'
CLCIN4PPS1       EQU  H'0001'
CLCIN4PPS2       EQU  H'0002'
CLCIN4PPS3       EQU  H'0003'
CLCIN4PPS4       EQU  H'0004'


;----- CLCIN5PPS Bits -----------------------------------------------------
CLCIN5PPS0       EQU  H'0000'
CLCIN5PPS1       EQU  H'0001'
CLCIN5PPS2       EQU  H'0002'
CLCIN5PPS3       EQU  H'0003'
CLCIN5PPS4       EQU  H'0004'


;----- CLCIN6PPS Bits -----------------------------------------------------
CLCIN6PPS0       EQU  H'0000'
CLCIN6PPS1       EQU  H'0001'
CLCIN6PPS2       EQU  H'0002'
CLCIN6PPS3       EQU  H'0003'
CLCIN6PPS4       EQU  H'0004'


;----- CLCIN7PPS Bits -----------------------------------------------------
CLCIN7PPS0       EQU  H'0000'
CLCIN7PPS1       EQU  H'0001'
CLCIN7PPS2       EQU  H'0002'
CLCIN7PPS3       EQU  H'0003'
CLCIN7PPS4       EQU  H'0004'


;----- ADACTPPS Bits -----------------------------------------------------
ADACTPPS0        EQU  H'0000'
ADACTPPS1        EQU  H'0001'
ADACTPPS2        EQU  H'0002'
ADACTPPS3        EQU  H'0003'
ADACTPPS4        EQU  H'0004'


;----- SPI2SCKPPS Bits -----------------------------------------------------
SPI2SCKPPS0      EQU  H'0000'
SPI2SCKPPS1      EQU  H'0001'
SPI2SCKPPS2      EQU  H'0002'
SPI2SCKPPS3      EQU  H'0003'
SPI2SCKPPS4      EQU  H'0004'


;----- SPI2SDIPPS Bits -----------------------------------------------------
SPI2SDIPPS0      EQU  H'0000'
SPI2SDIPPS1      EQU  H'0001'
SPI2SDIPPS2      EQU  H'0002'
SPI2SDIPPS3      EQU  H'0003'
SPI2SDIPPS4      EQU  H'0004'


;----- SPI2SSPPS Bits -----------------------------------------------------
SPI2SSPPS0       EQU  H'0000'
SPI2SSPPS1       EQU  H'0001'
SPI2SSPPS2       EQU  H'0002'
SPI2SSPPS3       EQU  H'0003'
SPI2SSPPS4       EQU  H'0004'


;----- I2C1SDAPPS Bits -----------------------------------------------------
I2C1SDAPPS0      EQU  H'0000'
I2C1SDAPPS1      EQU  H'0001'
I2C1SDAPPS2      EQU  H'0002'
I2C1SDAPPS3      EQU  H'0003'
I2C1SDAPPS4      EQU  H'0004'


;----- I2C1SCLPPS Bits -----------------------------------------------------
I2C1SCLPPS0      EQU  H'0000'
I2C1SCLPPS1      EQU  H'0001'
I2C1SCLPPS2      EQU  H'0002'
I2C1SCLPPS3      EQU  H'0003'
I2C1SCLPPS4      EQU  H'0004'


;----- U2RXPPS Bits -----------------------------------------------------
U2RXPPS0         EQU  H'0000'
U2RXPPS1         EQU  H'0001'
U2RXPPS2         EQU  H'0002'
U2RXPPS3         EQU  H'0003'
U2RXPPS4         EQU  H'0004'


;----- U2CTSPPS Bits -----------------------------------------------------
U2CTSPPS0        EQU  H'0000'
U2CTSPPS1        EQU  H'0001'
U2CTSPPS2        EQU  H'0002'
U2CTSPPS3        EQU  H'0003'
U2CTSPPS4        EQU  H'0004'


;----- U3RXPPS Bits -----------------------------------------------------
U3RXPPS0         EQU  H'0000'
U3RXPPS1         EQU  H'0001'
U3RXPPS2         EQU  H'0002'
U3RXPPS3         EQU  H'0003'
U3RXPPS4         EQU  H'0004'
U3RXPPS5         EQU  H'0005'


;----- U3CTSPPS Bits -----------------------------------------------------
U3CTSPPS0        EQU  H'0000'
U3CTSPPS1        EQU  H'0001'
U3CTSPPS2        EQU  H'0002'
U3CTSPPS3        EQU  H'0003'
U3CTSPPS4        EQU  H'0004'
U3CTSPPS5        EQU  H'0005'


;----- U4RXPPS Bits -----------------------------------------------------
U4RXPPS0         EQU  H'0000'
U4RXPPS1         EQU  H'0001'
U4RXPPS2         EQU  H'0002'
U4RXPPS3         EQU  H'0003'
U4RXPPS4         EQU  H'0004'


;----- U4CTSPPS Bits -----------------------------------------------------
U4CTSPPS0        EQU  H'0000'
U4CTSPPS1        EQU  H'0001'
U4CTSPPS2        EQU  H'0002'
U4CTSPPS3        EQU  H'0003'
U4CTSPPS4        EQU  H'0004'


;----- U5RXPPS Bits -----------------------------------------------------
U5RXPPS0         EQU  H'0000'
U5RXPPS1         EQU  H'0001'
U5RXPPS2         EQU  H'0002'
U5RXPPS3         EQU  H'0003'
U5RXPPS4         EQU  H'0004'
U5RXPPS5         EQU  H'0005'


;----- U5CTSPPS Bits -----------------------------------------------------
U5CTSPPS0        EQU  H'0000'
U5CTSPPS1        EQU  H'0001'
U5CTSPPS2        EQU  H'0002'
U5CTSPPS3        EQU  H'0003'
U5CTSPPS4        EQU  H'0004'
U5CTSPPS5        EQU  H'0005'


;----- RC4I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- RC3I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- RB2I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- I2C1CNTL Bits -----------------------------------------------------
CNT0             EQU  H'0000'
CNT1             EQU  H'0001'
CNT2             EQU  H'0002'
CNT3             EQU  H'0003'
CNT4             EQU  H'0004'
CNT5             EQU  H'0005'
CNT6             EQU  H'0006'
CNT7             EQU  H'0007'


;----- I2C1CNTH Bits -----------------------------------------------------
CNT8             EQU  H'0000'
CNT9             EQU  H'0001'
CNT10            EQU  H'0002'
CNT11            EQU  H'0003'
CNT12            EQU  H'0004'
CNT13            EQU  H'0005'
CNT14            EQU  H'0006'
CNT15            EQU  H'0007'


;----- I2C1CON0 Bits -----------------------------------------------------
MDR              EQU  H'0003'
CSTR             EQU  H'0004'
S                EQU  H'0005'
RSEN             EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
I2CEN            EQU  H'0007'


;----- I2C1CON1 Bits -----------------------------------------------------
CSD              EQU  H'0000'
TXU              EQU  H'0001'
RXO              EQU  H'0002'
P                EQU  H'0003'
ACKT             EQU  H'0004'
ACKSTAT          EQU  H'0005'
ACKDT            EQU  H'0006'
ACKCNT           EQU  H'0007'


;----- I2C1CON2 Bits -----------------------------------------------------
ABD              EQU  H'0004'
FME              EQU  H'0005'
GCEN             EQU  H'0006'
ACNT             EQU  H'0007'
BFRET0           EQU  H'0000'
BFRET1           EQU  H'0001'
SDAHT0           EQU  H'0002'
SDAHT1           EQU  H'0003'


;----- I2C1ERR Bits -----------------------------------------------------
NACKIE           EQU  H'0000'
BCLIE            EQU  H'0001'
BTOIE            EQU  H'0002'
NACKIF           EQU  H'0004'
BCLIF            EQU  H'0005'
BTOIF            EQU  H'0006'
NACK1IE          EQU  H'0000'
BCL1IE           EQU  H'0001'
BTO1IE           EQU  H'0002'
NACK1IF          EQU  H'0004'
BCL1IF           EQU  H'0005'
BTO1IF           EQU  H'0006'


;----- I2C1STAT0 Bits -----------------------------------------------------
D                EQU  H'0003'
R                EQU  H'0004'
MMA              EQU  H'0005'
SMA              EQU  H'0006'
BFRE             EQU  H'0007'
; Register.bit not unique    DATA             EQU  H'0003'
READ             EQU  H'0004'
NOT_ADDRESS      EQU  H'0003'
NOT_WRITE        EQU  H'0004'
NOT_A            EQU  H'0003'
NOT_W            EQU  H'0004'


;----- I2C1STAT1 Bits -----------------------------------------------------
RXBF             EQU  H'0000'
CLRBF            EQU  H'0002'
RXRE             EQU  H'0003'
TXBE             EQU  H'0005'
TXWE             EQU  H'0007'


;----- I2C1PIR Bits -----------------------------------------------------
SCIF             EQU  H'0000'
RSCIF            EQU  H'0001'
PCIF             EQU  H'0002'
ADRIF            EQU  H'0003'
WRIF             EQU  H'0004'
ACKTIF           EQU  H'0006'
CNTIF            EQU  H'0007'
SC1IF            EQU  H'0000'
RSC1IF           EQU  H'0001'
PC1IF            EQU  H'0002'
ADR1IF           EQU  H'0003'
WR1IF            EQU  H'0004'
ACKT1IF          EQU  H'0006'
CNT1IF           EQU  H'0007'


;----- I2C1PIE Bits -----------------------------------------------------
SCIE             EQU  H'0000'
RSCIE            EQU  H'0001'
PCIE             EQU  H'0002'
ADRIE            EQU  H'0003'
WRIE             EQU  H'0004'
ACKTIE           EQU  H'0006'
CNTIE            EQU  H'0007'
SC1IE            EQU  H'0000'
RSC1IE           EQU  H'0001'
PC1IE            EQU  H'0002'
ADR1IE           EQU  H'0003'
WR1IE            EQU  H'0004'
ACKT1IE          EQU  H'0006'
CNT1IE           EQU  H'0007'


;----- I2C1BTO Bits -----------------------------------------------------
TOBY32           EQU  H'0006'
TOREC            EQU  H'0007'
TOTIME0          EQU  H'0000'
TOTIME1          EQU  H'0001'
TOTIME2          EQU  H'0002'
TOTIME3          EQU  H'0003'
TOTIME4          EQU  H'0004'
TOTIME5          EQU  H'0005'


;----- I2C1CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
I2CCLK0          EQU  H'0000'
I2CCLK1          EQU  H'0001'
I2CCLK2          EQU  H'0002'
I2CCLK3          EQU  H'0003'
I2CCLK4          EQU  H'0004'


;----- I2C1BTOC Bits -----------------------------------------------------
BTOC0            EQU  H'0000'
BTOC1            EQU  H'0001'
BTOC2            EQU  H'0002'
BTOC3            EQU  H'0003'
I2CBTOC0         EQU  H'0000'
I2CBTOC1         EQU  H'0001'
I2CBTOC2         EQU  H'0002'
I2CBTOC3         EQU  H'0003'


;----- U2P1H Bits -----------------------------------------------------
P1H              EQU  H'0000'


;----- U2P2H Bits -----------------------------------------------------
P2H              EQU  H'0000'


;----- U2P3H Bits -----------------------------------------------------
P3H              EQU  H'0000'


;----- U2CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
U2RXEN           EQU  H'0004'
U2TXEN           EQU  H'0005'
U2ABDEN          EQU  H'0006'
U2BRGS           EQU  H'0007'
U2MODE0          EQU  H'0000'
U2MODE1          EQU  H'0001'
U2MODE2          EQU  H'0002'
U2MODE3          EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


;----- U2CON1 Bits -----------------------------------------------------
SENDB            EQU  H'0000'
BRKOVR           EQU  H'0001'
RXBIMD           EQU  H'0003'
WUE              EQU  H'0004'
ON_U2CON1        EQU  H'0007'
U2SENDB          EQU  H'0000'
U2BRKOVR         EQU  H'0001'
U2RXBIMD         EQU  H'0003'
U2WUE            EQU  H'0004'
U2ON             EQU  H'0007'


;----- U2CON2 Bits -----------------------------------------------------
TXPOL            EQU  H'0002'
C0EN             EQU  H'0003'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U2TXPOL          EQU  H'0002'
U2C0EN           EQU  H'0003'
U2RXPOL          EQU  H'0006'
U2RUNOVF         EQU  H'0007'
U2FLO0           EQU  H'0000'
U2FLO1           EQU  H'0001'
U2STP0           EQU  H'0004'
U2STP1           EQU  H'0005'


;----- U2FIFO Bits -----------------------------------------------------
RXBF             EQU  H'0000'
RXBE             EQU  H'0001'
XON              EQU  H'0002'
RXIDL            EQU  H'0003'
TXBF             EQU  H'0004'
TXBE             EQU  H'0005'
STPMD            EQU  H'0006'
TXWRE            EQU  H'0007'
U2RXBF           EQU  H'0000'
U2RXBE           EQU  H'0001'
U2XON            EQU  H'0002'
U2RXIDL          EQU  H'0003'
U2TXBF           EQU  H'0004'
U2TXBE           EQU  H'0005'
U2STPMD          EQU  H'0006'
U2TXWRE          EQU  H'0007'
U2RCIDL          EQU  H'0003'
RCIDL            EQU  H'0003'


;----- U2UIR Bits -----------------------------------------------------
ABDIE            EQU  H'0002'
ABDIF            EQU  H'0006'
WUIF             EQU  H'0007'
U2ABDIE          EQU  H'0002'
U2ABDIF          EQU  H'0006'
U2WUIF           EQU  H'0007'


;----- U2ERRIR Bits -----------------------------------------------------
TXCIF            EQU  H'0000'
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
CERIF            EQU  H'0004'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U2TXCIF          EQU  H'0000'
U2RXFOIF         EQU  H'0001'
U2RXBKIF         EQU  H'0002'
U2FERIF          EQU  H'0003'
U2CERIF          EQU  H'0004'
U2ABDOVF         EQU  H'0005'
U2PERIF          EQU  H'0006'
U2TXMTIF         EQU  H'0007'


;----- U2ERRIE Bits -----------------------------------------------------
TXCIE            EQU  H'0000'
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
CERIE            EQU  H'0004'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U2TXCIE          EQU  H'0000'
U2RXFOIE         EQU  H'0001'
U2RXBKIE         EQU  H'0002'
U2FERIE          EQU  H'0003'
U2CERIE          EQU  H'0004'
U2ABDOVE         EQU  H'0005'
U2PERIE          EQU  H'0006'
U2TXMTIE         EQU  H'0007'


;----- U3CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U3RXEN           EQU  H'0004'
U3TXEN           EQU  H'0005'
U3ABDEN          EQU  H'0006'
U3BRGS           EQU  H'0007'
U3MODE0          EQU  H'0000'
U3MODE1          EQU  H'0001'
U3MODE2          EQU  H'0002'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


;----- U3CON2 Bits -----------------------------------------------------
TXPOL            EQU  H'0002'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U3TXPOL          EQU  H'0002'
U3RXPOL          EQU  H'0006'
U3RUNOVF         EQU  H'0007'
U3FLO0           EQU  H'0000'
U3FLO1           EQU  H'0001'
U3STP0           EQU  H'0004'
U3STP1           EQU  H'0005'


;----- U3FIFO Bits -----------------------------------------------------
RXBF             EQU  H'0000'
RXBE             EQU  H'0001'
XON              EQU  H'0002'
RXIDL            EQU  H'0003'
TXBF             EQU  H'0004'
TXBE             EQU  H'0005'
STPMD            EQU  H'0006'
TXWRE            EQU  H'0007'
U3RXBF           EQU  H'0000'
U3RXBE           EQU  H'0001'
U3XON            EQU  H'0002'
U3RXIDL          EQU  H'0003'
U3TXBF           EQU  H'0004'
U3TXBE           EQU  H'0005'
U3STPMD          EQU  H'0006'
U3TXWRE          EQU  H'0007'
U3RCIDL          EQU  H'0003'
RCIDL            EQU  H'0003'


;----- U3UIR Bits -----------------------------------------------------
ABDIE            EQU  H'0002'
ABDIF            EQU  H'0006'
WUIF             EQU  H'0007'
U3ABDIE          EQU  H'0002'
U3ABDIF          EQU  H'0006'
U3WUIF           EQU  H'0007'


;----- U3ERRIR Bits -----------------------------------------------------
TXCIF            EQU  H'0000'
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
CERIF            EQU  H'0004'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U3TXCIF          EQU  H'0000'
U3RXFOIF         EQU  H'0001'
U3RXBKIF         EQU  H'0002'
U3FERIF          EQU  H'0003'
U3CERIF          EQU  H'0004'
U3ABDOVF         EQU  H'0005'
U3PERIF          EQU  H'0006'
U3TXMTIF         EQU  H'0007'


;----- U3ERRIE Bits -----------------------------------------------------
TXCIE            EQU  H'0000'
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
CERIE            EQU  H'0004'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U3TXCIE          EQU  H'0000'
U3RXFOIE         EQU  H'0001'
U3RXBKIE         EQU  H'0002'
U3FERIE          EQU  H'0003'
U3CERIE          EQU  H'0004'
U3ABDOVE         EQU  H'0005'
U3PERIE          EQU  H'0006'
U3TXMTIE         EQU  H'0007'


;----- U4CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U4RXEN           EQU  H'0004'
U4TXEN           EQU  H'0005'
U4ABDEN          EQU  H'0006'
U4BRGS           EQU  H'0007'
U4MODE0          EQU  H'0000'
U4MODE1          EQU  H'0001'
U4MODE2          EQU  H'0002'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


;----- U4CON2 Bits -----------------------------------------------------
TXPOL            EQU  H'0002'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U4TXPOL          EQU  H'0002'
U4RXPOL          EQU  H'0006'
U4RUNOVF         EQU  H'0007'
U4FLO0           EQU  H'0000'
U4FLO1           EQU  H'0001'
U4STP0           EQU  H'0004'
U4STP1           EQU  H'0005'


;----- U4FIFO Bits -----------------------------------------------------
RXBF             EQU  H'0000'
RXBE             EQU  H'0001'
XON              EQU  H'0002'
RXIDL            EQU  H'0003'
TXBF             EQU  H'0004'
TXBE             EQU  H'0005'
STPMD            EQU  H'0006'
TXWRE            EQU  H'0007'
U4RXBF           EQU  H'0000'
U4RXBE           EQU  H'0001'
U4XON            EQU  H'0002'
U4RXIDL          EQU  H'0003'
U4TXBF           EQU  H'0004'
U4TXBE           EQU  H'0005'
U4STPMD          EQU  H'0006'
U4TXWRE          EQU  H'0007'
U4RCIDL          EQU  H'0003'
RCIDL            EQU  H'0003'


;----- U4UIR Bits -----------------------------------------------------
ABDIE            EQU  H'0002'
ABDIF            EQU  H'0006'
WUIF             EQU  H'0007'
U4ABDIE          EQU  H'0002'
U4ABDIF          EQU  H'0006'
U4WUIF           EQU  H'0007'


;----- U4ERRIR Bits -----------------------------------------------------
TXCIF            EQU  H'0000'
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
CERIF            EQU  H'0004'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U4TXCIF          EQU  H'0000'
U4RXFOIF         EQU  H'0001'
U4RXBKIF         EQU  H'0002'
U4FERIF          EQU  H'0003'
U4CERIF          EQU  H'0004'
U4ABDOVF         EQU  H'0005'
U4PERIF          EQU  H'0006'
U4TXMTIF         EQU  H'0007'


;----- U4ERRIE Bits -----------------------------------------------------
TXCIE            EQU  H'0000'
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
CERIE            EQU  H'0004'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U4TXCIE          EQU  H'0000'
U4RXFOIE         EQU  H'0001'
U4RXBKIE         EQU  H'0002'
U4FERIE          EQU  H'0003'
U4CERIE          EQU  H'0004'
U4ABDOVE         EQU  H'0005'
U4PERIE          EQU  H'0006'
U4TXMTIE         EQU  H'0007'


;----- U5CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U5RXEN           EQU  H'0004'
U5TXEN           EQU  H'0005'
U5ABDEN          EQU  H'0006'
U5BRGS           EQU  H'0007'
U5MODE0          EQU  H'0000'
U5MODE1          EQU  H'0001'
U5MODE2          EQU  H'0002'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


;----- U5CON2 Bits -----------------------------------------------------
TXPOL            EQU  H'0002'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U5TXPOL          EQU  H'0002'
U5RXPOL          EQU  H'0006'
U5RUNOVF         EQU  H'0007'
U5FLO0           EQU  H'0000'
U5FLO1           EQU  H'0001'
U5STP0           EQU  H'0004'
U5STP1           EQU  H'0005'


;----- U5FIFO Bits -----------------------------------------------------
RXBF             EQU  H'0000'
RXBE             EQU  H'0001'
XON              EQU  H'0002'
RXIDL            EQU  H'0003'
TXBF             EQU  H'0004'
TXBE             EQU  H'0005'
STPMD            EQU  H'0006'
TXWRE            EQU  H'0007'
U5RXBF           EQU  H'0000'
U5RXBE           EQU  H'0001'
U5XON            EQU  H'0002'
U5RXIDL          EQU  H'0003'
U5TXBF           EQU  H'0004'
U5TXBE           EQU  H'0005'
U5STPMD          EQU  H'0006'
U5TXWRE          EQU  H'0007'
U5RCIDL          EQU  H'0003'
RCIDL            EQU  H'0003'


;----- U5UIR Bits -----------------------------------------------------
ABDIE            EQU  H'0002'
ABDIF            EQU  H'0006'
WUIF             EQU  H'0007'
U5ABDIE          EQU  H'0002'
U5ABDIF          EQU  H'0006'
U5WUIF           EQU  H'0007'


;----- U5ERRIR Bits -----------------------------------------------------
TXCIF            EQU  H'0000'
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
CERIF            EQU  H'0004'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U5TXCIF          EQU  H'0000'
U5RXFOIF         EQU  H'0001'
U5RXBKIF         EQU  H'0002'
U5FERIF          EQU  H'0003'
U5CERIF          EQU  H'0004'
U5ABDOVF         EQU  H'0005'
U5PERIF          EQU  H'0006'
U5TXMTIF         EQU  H'0007'


;----- U5ERRIE Bits -----------------------------------------------------
TXCIE            EQU  H'0000'
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
CERIE            EQU  H'0004'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U5TXCIE          EQU  H'0000'
U5RXFOIE         EQU  H'0001'
U5RXBKIE         EQU  H'0002'
U5FERIE          EQU  H'0003'
U5CERIE          EQU  H'0004'
U5ABDOVE         EQU  H'0005'
U5PERIE          EQU  H'0006'
U5TXMTIE         EQU  H'0007'


;----- SMT1TMR Bits -----------------------------------------------------
TMR0             EQU  H'0000'
TMR1             EQU  H'0001'
TMR2             EQU  H'0002'
TMR3             EQU  H'0003'
TMR4             EQU  H'0004'
TMR5             EQU  H'0005'
TMR6             EQU  H'0006'
TMR7             EQU  H'0007'
SMT1TMR0         EQU  H'0000'
SMT1TMR1         EQU  H'0001'
SMT1TMR2         EQU  H'0002'
SMT1TMR3         EQU  H'0003'
SMT1TMR4         EQU  H'0004'
SMT1TMR5         EQU  H'0005'
SMT1TMR6         EQU  H'0006'
SMT1TMR7         EQU  H'0007'


;----- SMT1TMRH Bits -----------------------------------------------------
TMR8             EQU  H'0000'
TMR9             EQU  H'0001'
TMR10            EQU  H'0002'
TMR11            EQU  H'0003'
TMR12            EQU  H'0004'
TMR13            EQU  H'0005'
TMR14            EQU  H'0006'
TMR15            EQU  H'0007'
SMT1TMR8         EQU  H'0000'
SMT1TMR9         EQU  H'0001'
SMT1TMR10        EQU  H'0002'
SMT1TMR11        EQU  H'0003'
SMT1TMR12        EQU  H'0004'
SMT1TMR13        EQU  H'0005'
SMT1TMR14        EQU  H'0006'
SMT1TMR15        EQU  H'0007'


;----- SMT1TMRU Bits -----------------------------------------------------
TMR16            EQU  H'0000'
TMR17            EQU  H'0001'
TMR18            EQU  H'0002'
TMR19            EQU  H'0003'
TMR20            EQU  H'0004'
TMR21            EQU  H'0005'
TMR22            EQU  H'0006'
TMR23            EQU  H'0007'
SMT1TMR16        EQU  H'0000'
SMT1TMR17        EQU  H'0001'
SMT1TMR18        EQU  H'0002'
SMT1TMR19        EQU  H'0003'
SMT1TMR20        EQU  H'0004'
SMT1TMR21        EQU  H'0005'
SMT1TMR22        EQU  H'0006'
SMT1TMR23        EQU  H'0007'


;----- SMT1CPR Bits -----------------------------------------------------
CPR0             EQU  H'0000'
CPR1             EQU  H'0001'
CPR2             EQU  H'0002'
CPR3             EQU  H'0003'
CPR4             EQU  H'0004'
CPR5             EQU  H'0005'
CPR6             EQU  H'0006'
CPR7             EQU  H'0007'
SMT1CPR0         EQU  H'0000'
SMT1CPR1         EQU  H'0001'
SMT1CPR2         EQU  H'0002'
SMT1CPR3         EQU  H'0003'
SMT1CPR4         EQU  H'0004'
SMT1CPR5         EQU  H'0005'
SMT1CPR6         EQU  H'0006'
SMT1CPR7         EQU  H'0007'


;----- SMT1CPRH Bits -----------------------------------------------------
CPR8             EQU  H'0000'
CPR9             EQU  H'0001'
CPR10            EQU  H'0002'
CPR11            EQU  H'0003'
CPR12            EQU  H'0004'
CPR13            EQU  H'0005'
CPR14            EQU  H'0006'
CPR15            EQU  H'0007'
SMT1CPR8         EQU  H'0000'
SMT1CPR9         EQU  H'0001'
SMT1CPR10        EQU  H'0002'
SMT1CPR11        EQU  H'0003'
SMT1CPR12        EQU  H'0004'
SMT1CPR13        EQU  H'0005'
SMT1CPR14        EQU  H'0006'
SMT1CPR15        EQU  H'0007'


;----- SMT1CPRU Bits -----------------------------------------------------
CPR16            EQU  H'0000'
CPR17            EQU  H'0001'
CPR18            EQU  H'0002'
CPR19            EQU  H'0003'
CPR20            EQU  H'0004'
CPR21            EQU  H'0005'
CPR22            EQU  H'0006'
CPR23            EQU  H'0007'
SMT1CPR16        EQU  H'0000'
SMT1CPR17        EQU  H'0001'
SMT1CPR18        EQU  H'0002'
SMT1CPR19        EQU  H'0003'
SMT1CPR20        EQU  H'0004'
SMT1CPR21        EQU  H'0005'
SMT1CPR22        EQU  H'0006'
SMT1CPR23        EQU  H'0007'


;----- SMT1CPW Bits -----------------------------------------------------
CPW0             EQU  H'0000'
CPW1             EQU  H'0001'
CPW2             EQU  H'0002'
CPW3             EQU  H'0003'
CPW4             EQU  H'0004'
CPW5             EQU  H'0005'
CPW6             EQU  H'0006'
CPW7             EQU  H'0007'
SMT1CPW0         EQU  H'0000'
SMT1CPW1         EQU  H'0001'
SMT1CPW2         EQU  H'0002'
SMT1CPW3         EQU  H'0003'
SMT1CPW4         EQU  H'0004'
SMT1CPW5         EQU  H'0005'
SMT1CPW6         EQU  H'0006'
SMT1CPW7         EQU  H'0007'


;----- SMT1CPWH Bits -----------------------------------------------------
CPW8             EQU  H'0000'
CPW9             EQU  H'0001'
CPW10            EQU  H'0002'
CPW11            EQU  H'0003'
CPW12            EQU  H'0004'
CPW13            EQU  H'0005'
CPW14            EQU  H'0006'
CPW15            EQU  H'0007'
SMT1CPW8         EQU  H'0000'
SMT1CPW9         EQU  H'0001'
SMT1CPW10        EQU  H'0002'
SMT1CPW11        EQU  H'0003'
SMT1CPW12        EQU  H'0004'
SMT1CPW13        EQU  H'0005'
SMT1CPW14        EQU  H'0006'
SMT1CPW15        EQU  H'0007'


;----- SMT1CPWU Bits -----------------------------------------------------
CPW16            EQU  H'0000'
CPW17            EQU  H'0001'
CPW18            EQU  H'0002'
CPW19            EQU  H'0003'
CPW20            EQU  H'0004'
CPW21            EQU  H'0005'
CPW22            EQU  H'0006'
CPW23            EQU  H'0007'
SMT1CPW16        EQU  H'0000'
SMT1CPW17        EQU  H'0001'
SMT1CPW18        EQU  H'0002'
SMT1CPW19        EQU  H'0003'
SMT1CPW20        EQU  H'0004'
SMT1CPW21        EQU  H'0005'
SMT1CPW22        EQU  H'0006'
SMT1CPW23        EQU  H'0007'


;----- SMT1PR Bits -----------------------------------------------------
PR0_SMT1PR       EQU  H'0000'
PR1_SMT1PR       EQU  H'0001'
PR2_SMT1PR       EQU  H'0002'
PR3              EQU  H'0003'
PR4              EQU  H'0004'
PR5              EQU  H'0005'
PR6              EQU  H'0006'
PR7              EQU  H'0007'
SMT1PR0          EQU  H'0000'
SMT1PR1          EQU  H'0001'
SMT1PR2          EQU  H'0002'
SMT1PR3          EQU  H'0003'
SMT1PR4          EQU  H'0004'
SMT1PR5          EQU  H'0005'
SMT1PR6          EQU  H'0006'
SMT1PR7          EQU  H'0007'


;----- SMT1PRH Bits -----------------------------------------------------
PR8              EQU  H'0000'
PR9              EQU  H'0001'
PR10             EQU  H'0002'
PR11             EQU  H'0003'
PR12             EQU  H'0004'
PR13             EQU  H'0005'
PR14             EQU  H'0006'
PR15             EQU  H'0007'
SMT1PR8          EQU  H'0000'
SMT1PR9          EQU  H'0001'
SMT1PR10         EQU  H'0002'
SMT1PR11         EQU  H'0003'
SMT1PR12         EQU  H'0004'
SMT1PR13         EQU  H'0005'
SMT1PR14         EQU  H'0006'
SMT1PR15         EQU  H'0007'


;----- SMT1PRU Bits -----------------------------------------------------
PR16             EQU  H'0000'
PR17             EQU  H'0001'
PR18             EQU  H'0002'
PR19             EQU  H'0003'
PR20             EQU  H'0004'
PR21             EQU  H'0005'
PR22             EQU  H'0006'
PR23             EQU  H'0007'
SMT1PR16         EQU  H'0000'
SMT1PR17         EQU  H'0001'
SMT1PR18         EQU  H'0002'
SMT1PR19         EQU  H'0003'
SMT1PR20         EQU  H'0004'
SMT1PR21         EQU  H'0005'
SMT1PR22         EQU  H'0006'
SMT1PR23         EQU  H'0007'


;----- SMT1CON0 Bits -----------------------------------------------------
CPOL             EQU  H'0002'
SPOL             EQU  H'0003'
WPOL             EQU  H'0004'
STP              EQU  H'0005'
EN               EQU  H'0007'
SMT1CPOL         EQU  H'0002'
SMT1SPOL         EQU  H'0003'
SMT1WOL          EQU  H'0004'
SMT1STP          EQU  H'0005'
SMT1EN           EQU  H'0007'
PS0              EQU  H'0000'
PS1              EQU  H'0001'
SMT1PS0          EQU  H'0000'
SMT1PS1          EQU  H'0001'


;----- SMT1CON1 Bits -----------------------------------------------------
REPEAT           EQU  H'0006'
GO_SMT1CON1      EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
SMT1REPEAT       EQU  H'0006'
SMT1GO           EQU  H'0007'


;----- SMT1STAT Bits -----------------------------------------------------
AS               EQU  H'0000'
WS               EQU  H'0001'
TS               EQU  H'0002'
RST              EQU  H'0005'
CPWUP            EQU  H'0006'
CPRUP            EQU  H'0007'
SMT1AS           EQU  H'0000'
SMT1WS           EQU  H'0001'
SMT1TS           EQU  H'0002'
SMT1RESET        EQU  H'0005'
SMT1CPWUP        EQU  H'0006'
SMT1CPRUP        EQU  H'0007'
SMT1RST          EQU  H'0005'


;----- SMT1SIG Bits -----------------------------------------------------
SSEL0            EQU  H'0000'
SSEL1            EQU  H'0001'
SSEL2            EQU  H'0002'
SSEL3            EQU  H'0003'
SSEL4            EQU  H'0004'
SMT1SSEL0        EQU  H'0000'
SMT1SSEL1        EQU  H'0001'
SMT1SSEL2        EQU  H'0002'
SMT1SSEL3        EQU  H'0003'
SMT1SSEL4        EQU  H'0004'


;----- SMT1WIN Bits -----------------------------------------------------
WSEL0            EQU  H'0000'
WSEL1            EQU  H'0001'
WSEL2            EQU  H'0002'
WSEL3            EQU  H'0003'
WSEL4            EQU  H'0004'
SMT1WSEL0        EQU  H'0000'
SMT1WSEL1        EQU  H'0001'
SMT1WSEL2        EQU  H'0002'
SMT1WSEL3        EQU  H'0003'
SMT1WSEL4        EQU  H'0004'


;----- TMR0L Bits -----------------------------------------------------
TMR0L0           EQU  H'0000'
TMR0L1           EQU  H'0001'
TMR0L2           EQU  H'0002'
TMR0L3           EQU  H'0003'
TMR0L4           EQU  H'0004'
TMR0L5           EQU  H'0005'
TMR0L6           EQU  H'0006'
TMR0L7           EQU  H'0007'


;----- TMR0H Bits -----------------------------------------------------
TMR0H0           EQU  H'0000'
TMR0H1           EQU  H'0001'
TMR0H2           EQU  H'0002'
TMR0H3           EQU  H'0003'
TMR0H4           EQU  H'0004'
TMR0H5           EQU  H'0005'
TMR0H6           EQU  H'0006'
TMR0H7           EQU  H'0007'
T0PR0            EQU  H'0000'
T0PR1            EQU  H'0001'
T0PR2            EQU  H'0002'
T0PR3            EQU  H'0003'
T0PR4            EQU  H'0004'
T0PR5            EQU  H'0005'
T0PR6            EQU  H'0006'
T0PR7            EQU  H'0007'


;----- T0CON0 Bits -----------------------------------------------------
MD16             EQU  H'0004'
OUT_T0CON0       EQU  H'0005'
EN               EQU  H'0007'
T0MD16           EQU  H'0004'
T0OUT            EQU  H'0005'
T0EN             EQU  H'0007'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
T016BIT          EQU  H'0004'


;----- T0CON1 Bits -----------------------------------------------------
ASYNC            EQU  H'0004'
CKPS0_T0CON1     EQU  H'0000'
CKPS1_T0CON1     EQU  H'0001'
CKPS2_T0CON1     EQU  H'0002'
CKPS3            EQU  H'0003'
T0ASYNC          EQU  H'0004'
CS0_T0CON1       EQU  H'0005'
CS1_T0CON1       EQU  H'0006'
CS2_T0CON1       EQU  H'0007'
T0CKPS0          EQU  H'0000'
T0CKPS1          EQU  H'0001'
T0CKPS2          EQU  H'0002'
T0CKPS3          EQU  H'0003'
T0CS0            EQU  H'0005'
T0CS1            EQU  H'0006'
T0CS2            EQU  H'0007'


;----- T2CON Bits -----------------------------------------------------
ON_T2CON         EQU  H'0007'
T2ON             EQU  H'0007'
T2OUTPS0         EQU  H'0000'
T2OUTPS1         EQU  H'0001'
T2OUTPS2         EQU  H'0002'
T2OUTPS3         EQU  H'0003'
T2CKPS0          EQU  H'0004'
T2CKPS1          EQU  H'0005'
T2CKPS2          EQU  H'0006'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T2CON      EQU  H'0004'
CKPS1_T2CON      EQU  H'0005'
CKPS2_T2CON      EQU  H'0006'
TMR2ON           EQU  H'0007'


;----- T2HLT Bits -----------------------------------------------------
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T2CKSYNC         EQU  H'0005'
T2CKPOL          EQU  H'0006'
T2PSYNC          EQU  H'0007'
T2MODE0          EQU  H'0000'
T2MODE1          EQU  H'0001'
T2MODE2          EQU  H'0002'
T2MODE3          EQU  H'0003'
T2MODE4          EQU  H'0004'


;----- T2CLKCON Bits -----------------------------------------------------
CS0_T2CLKCON     EQU  H'0000'
CS1_T2CLKCON     EQU  H'0001'
CS2_T2CLKCON     EQU  H'0002'
CS3              EQU  H'0003'
T2CS0            EQU  H'0000'
T2CS1            EQU  H'0001'
T2CS2            EQU  H'0002'
T2CS3            EQU  H'0003'


;----- T2RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
T2RSEL0          EQU  H'0000'
T2RSEL1          EQU  H'0001'
T2RSEL2          EQU  H'0002'
T2RSEL3          EQU  H'0003'
T2RSEL4          EQU  H'0004'


;----- TMR3L Bits -----------------------------------------------------
TMR3L0           EQU  H'0000'
TMR3L1           EQU  H'0001'
TMR3L2           EQU  H'0002'
TMR3L3           EQU  H'0003'
TMR3L4           EQU  H'0004'
TMR3L5           EQU  H'0005'
TMR3L6           EQU  H'0006'
TMR3L7           EQU  H'0007'


;----- TMR3H Bits -----------------------------------------------------
TMR3H0           EQU  H'0000'
TMR3H1           EQU  H'0001'
TMR3H2           EQU  H'0002'
TMR3H3           EQU  H'0003'
TMR3H4           EQU  H'0004'
TMR3H5           EQU  H'0005'
TMR3H6           EQU  H'0006'
TMR3H7           EQU  H'0007'


;----- T3CON Bits -----------------------------------------------------
ON_T3CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
TMR3ON           EQU  H'0000'
T3RD16           EQU  H'0001'
NOT_T3SYNC       EQU  H'0002'
T3CKPS0          EQU  H'0004'
T3CKPS1          EQU  H'0005'
CKPS0_T3CON      EQU  H'0004'
CKPS1_T3CON      EQU  H'0005'
RD163            EQU  H'0001'


;----- T3GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO              EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL_T3GCON      EQU  H'0006'
GE               EQU  H'0007'
T3GVAL           EQU  H'0002'
T3GGO            EQU  H'0003'
T3GSPM           EQU  H'0004'
T3GTM            EQU  H'0005'
T3GPOL           EQU  H'0006'
T3GE             EQU  H'0007'
NOT_DONE_T3GCON  EQU  H'0003'
NOT_T3DONE       EQU  H'0003'


;----- T3GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
T3GSS0           EQU  H'0000'
T3GSS1           EQU  H'0001'
T3GSS2           EQU  H'0002'
T3GSS3           EQU  H'0003'
T3GSS4           EQU  H'0004'


;----- T3CLK Bits -----------------------------------------------------
T3CS0            EQU  H'0000'
T3CS1            EQU  H'0001'
T3CS2            EQU  H'0002'
T3CS3            EQU  H'0003'
T3CS4            EQU  H'0004'
CS0_T3CLK        EQU  H'0000'
CS1_T3CLK        EQU  H'0001'
CS2_T3CLK        EQU  H'0002'
CS3              EQU  H'0003'
CS4              EQU  H'0004'


;----- T4CON Bits -----------------------------------------------------
ON_T4CON         EQU  H'0007'
T4ON             EQU  H'0007'
T4OUTPS0         EQU  H'0000'
T4OUTPS1         EQU  H'0001'
T4OUTPS2         EQU  H'0002'
T4OUTPS3         EQU  H'0003'
T4CKPS0          EQU  H'0004'
T4CKPS1          EQU  H'0005'
T4CKPS2          EQU  H'0006'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T4CON      EQU  H'0004'
CKPS1_T4CON      EQU  H'0005'
CKPS2_T4CON      EQU  H'0006'
TMR4ON           EQU  H'0007'


;----- T4HLT Bits -----------------------------------------------------
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T4CKSYNC         EQU  H'0005'
T4CKPOL          EQU  H'0006'
T4PSYNC          EQU  H'0007'
T4MODE0          EQU  H'0000'
T4MODE1          EQU  H'0001'
T4MODE2          EQU  H'0002'
T4MODE3          EQU  H'0003'
T4MODE4          EQU  H'0004'


;----- T4CLKCON Bits -----------------------------------------------------
CS0_T4CLKCON     EQU  H'0000'
CS1_T4CLKCON     EQU  H'0001'
CS2_T4CLKCON     EQU  H'0002'
CS3              EQU  H'0003'
T4CS0            EQU  H'0000'
T4CS1            EQU  H'0001'
T4CS2            EQU  H'0002'
T4CS3            EQU  H'0003'


;----- T4RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
T4RSEL0          EQU  H'0000'
T4RSEL1          EQU  H'0001'
T4RSEL2          EQU  H'0002'
T4RSEL3          EQU  H'0003'
T4RSEL4          EQU  H'0004'


;----- TMR5L Bits -----------------------------------------------------
TMR5L0           EQU  H'0000'
TMR5L1           EQU  H'0001'
TMR5L2           EQU  H'0002'
TMR5L3           EQU  H'0003'
TMR5L4           EQU  H'0004'
TMR5L5           EQU  H'0005'
TMR5L6           EQU  H'0006'
TMR5L7           EQU  H'0007'


;----- TMR5H Bits -----------------------------------------------------
TMR5H0           EQU  H'0000'
TMR5H1           EQU  H'0001'
TMR5H2           EQU  H'0002'
TMR5H3           EQU  H'0003'
TMR5H4           EQU  H'0004'
TMR5H5           EQU  H'0005'
TMR5H6           EQU  H'0006'
TMR5H7           EQU  H'0007'


;----- T5CON Bits -----------------------------------------------------
ON_T5CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
TMR5ON           EQU  H'0000'
T5RD16           EQU  H'0001'
NOT_T5SYNC       EQU  H'0002'
T5CKPS0          EQU  H'0004'
T5CKPS1          EQU  H'0005'
CKPS0_T5CON      EQU  H'0004'
CKPS1_T5CON      EQU  H'0005'
RD165            EQU  H'0001'


;----- T5GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO              EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL_T5GCON      EQU  H'0006'
GE               EQU  H'0007'
T5GVAL           EQU  H'0002'
T5GGO            EQU  H'0003'
T5GSPM           EQU  H'0004'
T5GTM            EQU  H'0005'
T5GPOL           EQU  H'0006'
T5GE             EQU  H'0007'
NOT_DONE_T5GCON  EQU  H'0003'
NOT_T5DONE       EQU  H'0003'


;----- T5GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
T5GSS0           EQU  H'0000'
T5GSS1           EQU  H'0001'
T5GSS2           EQU  H'0002'
T5GSS3           EQU  H'0003'
T5GSS4           EQU  H'0004'


;----- T5CLK Bits -----------------------------------------------------
T5CS0            EQU  H'0000'
T5CS1            EQU  H'0001'
T5CS2            EQU  H'0002'
T5CS3            EQU  H'0003'
T5CS4            EQU  H'0004'
CS0_T5CLK        EQU  H'0000'
CS1_T5CLK        EQU  H'0001'
CS2_T5CLK        EQU  H'0002'
CS3              EQU  H'0003'
CS4              EQU  H'0004'


;----- T6CON Bits -----------------------------------------------------
ON_T6CON         EQU  H'0007'
T6ON             EQU  H'0007'
T6OUTPS0         EQU  H'0000'
T6OUTPS1         EQU  H'0001'
T6OUTPS2         EQU  H'0002'
T6OUTPS3         EQU  H'0003'
T6CKPS0          EQU  H'0004'
T6CKPS1          EQU  H'0005'
T6CKPS2          EQU  H'0006'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T6CON      EQU  H'0004'
CKPS1_T6CON      EQU  H'0005'
CKPS2_T6CON      EQU  H'0006'
TMR6ON           EQU  H'0007'


;----- T6HLT Bits -----------------------------------------------------
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T6CKSYNC         EQU  H'0005'
T6CKPOL          EQU  H'0006'
T6PSYNC          EQU  H'0007'
T6MODE0          EQU  H'0000'
T6MODE1          EQU  H'0001'
T6MODE2          EQU  H'0002'
T6MODE3          EQU  H'0003'
T6MODE4          EQU  H'0004'


;----- T6CLKCON Bits -----------------------------------------------------
CS0_T6CLKCON     EQU  H'0000'
CS1_T6CLKCON     EQU  H'0001'
CS2_T6CLKCON     EQU  H'0002'
CS3              EQU  H'0003'
T6CS0            EQU  H'0000'
T6CS1            EQU  H'0001'
T6CS2            EQU  H'0002'
T6CS3            EQU  H'0003'


;----- T6RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
T6RSEL0          EQU  H'0000'
T6RSEL1          EQU  H'0001'
T6RSEL2          EQU  H'0002'
T6RSEL3          EQU  H'0003'
T6RSEL4          EQU  H'0004'


;----- CCP2CON Bits -----------------------------------------------------
FMT              EQU  H'0004'
OUT_CCP2CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP2FMT          EQU  H'0004'
CCP2OUT          EQU  H'0005'
CCP2EN           EQU  H'0007'
CCP2MODE0        EQU  H'0000'
CCP2MODE1        EQU  H'0001'
CCP2MODE2        EQU  H'0002'
CCP2MODE3        EQU  H'0003'


;----- CCP2CAP Bits -----------------------------------------------------
CTS0             EQU  H'0000'
CTS1             EQU  H'0001'
CTS2             EQU  H'0002'
CTS3             EQU  H'0003'
CCP2CTS0         EQU  H'0000'
CCP2CTS1         EQU  H'0001'
CCP2CTS2         EQU  H'0002'
CCP2CTS3         EQU  H'0003'


;----- CCP3CON Bits -----------------------------------------------------
FMT              EQU  H'0004'
OUT_CCP3CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP3FMT          EQU  H'0004'
CCP3OUT          EQU  H'0005'
CCP3EN           EQU  H'0007'
CCP3MODE0        EQU  H'0000'
CCP3MODE1        EQU  H'0001'
CCP3MODE2        EQU  H'0002'
CCP3MODE3        EQU  H'0003'


;----- CCP3CAP Bits -----------------------------------------------------
CTS0             EQU  H'0000'
CTS1             EQU  H'0001'
CTS2             EQU  H'0002'
CTS3             EQU  H'0003'
CCP3CTS0         EQU  H'0000'
CCP3CTS1         EQU  H'0001'
CCP3CTS2         EQU  H'0002'
CCP3CTS3         EQU  H'0003'


;----- CCPTMRS0 Bits -----------------------------------------------------
C1TSEL0          EQU  H'0000'
C1TSEL1          EQU  H'0001'
C2TSEL0          EQU  H'0002'
C2TSEL1          EQU  H'0003'
C3TSEL0          EQU  H'0004'
C3TSEL1          EQU  H'0005'


;----- CRCDATA Bits -----------------------------------------------------
DATA0            EQU  H'0000'
DATA1            EQU  H'0001'
DATA2            EQU  H'0002'
DATA3            EQU  H'0003'
DATA4            EQU  H'0004'
DATA5            EQU  H'0005'
DATA6            EQU  H'0006'
DATA7            EQU  H'0007'


;----- CRCDATAH Bits -----------------------------------------------------
DATA8            EQU  H'0000'
DATA9            EQU  H'0001'
DATA10           EQU  H'0002'
DATA11           EQU  H'0003'
DATA12           EQU  H'0004'
DATA13           EQU  H'0005'
DATA14           EQU  H'0006'
DATA15           EQU  H'0007'


;----- CRCDATAU Bits -----------------------------------------------------
DATA16           EQU  H'0000'
DATA17           EQU  H'0001'
DATA18           EQU  H'0002'
DATA19           EQU  H'0003'
DATA20           EQU  H'0004'
DATA21           EQU  H'0005'
DATA22           EQU  H'0006'
DATA23           EQU  H'0007'


;----- CRCDATAT Bits -----------------------------------------------------
DATA24           EQU  H'0000'
DATA25           EQU  H'0001'
DATA26           EQU  H'0002'
DATA27           EQU  H'0003'
DATA28           EQU  H'0004'
DATA29           EQU  H'0005'
DATA30           EQU  H'0006'
DATA31           EQU  H'0007'


;----- CRCOUT Bits -----------------------------------------------------
OUT0             EQU  H'0000'
OUT1             EQU  H'0001'
OUT2             EQU  H'0002'
OUT3             EQU  H'0003'
OUT4             EQU  H'0004'
OUT5             EQU  H'0005'
OUT6             EQU  H'0006'
OUT7             EQU  H'0007'
SHIFT0           EQU  H'0000'
SHIFT1           EQU  H'0001'
SHIFT2           EQU  H'0002'
SHIFT3           EQU  H'0003'
SHIFT4           EQU  H'0004'
SHIFT5           EQU  H'0005'
SHIFT6           EQU  H'0006'
SHIFT7           EQU  H'0007'
XOR0             EQU  H'0000'
XOR1             EQU  H'0001'
XOR2             EQU  H'0002'
XOR3             EQU  H'0003'
XOR4             EQU  H'0004'
XOR5             EQU  H'0005'
XOR6             EQU  H'0006'
XOR7             EQU  H'0007'


;----- CRCOUTH Bits -----------------------------------------------------
OUT8             EQU  H'0000'
OUT9             EQU  H'0001'
OUT10            EQU  H'0002'
OUT11            EQU  H'0003'
OUT12            EQU  H'0004'
OUT13            EQU  H'0005'
OUT14            EQU  H'0006'
OUT15            EQU  H'0007'
SHIFT8           EQU  H'0000'
SHIFT9           EQU  H'0001'
SHIFT10          EQU  H'0002'
SHIFT11          EQU  H'0003'
SHIFT12          EQU  H'0004'
SHIFT13          EQU  H'0005'
SHIFT14          EQU  H'0006'
SHIFT15          EQU  H'0007'
XOR8             EQU  H'0000'
XOR9             EQU  H'0001'
XOR10            EQU  H'0002'
XOR11            EQU  H'0003'
XOR12            EQU  H'0004'
XOR13            EQU  H'0005'
XOR14            EQU  H'0006'
XOR15            EQU  H'0007'


;----- CRCOUTU Bits -----------------------------------------------------
OUT16            EQU  H'0000'
OUT17            EQU  H'0001'
OUT18            EQU  H'0002'
OUT19            EQU  H'0003'
OUT20            EQU  H'0004'
OUT21            EQU  H'0005'
OUT22            EQU  H'0006'
OUT23            EQU  H'0007'
SHIFT16          EQU  H'0000'
SHIFT17          EQU  H'0001'
SHIFT18          EQU  H'0002'
SHIFT19          EQU  H'0003'
SHIFT20          EQU  H'0004'
SHIFT21          EQU  H'0005'
SHIFT22          EQU  H'0006'
SHIFT23          EQU  H'0007'
XOR16            EQU  H'0000'
XOR17            EQU  H'0001'
XOR18            EQU  H'0002'
XOR19            EQU  H'0003'
XOR20            EQU  H'0004'
XOR21            EQU  H'0005'
XOR22            EQU  H'0006'
XOR23            EQU  H'0007'


;----- CRCOUTT Bits -----------------------------------------------------
OUT24            EQU  H'0000'
OUT25            EQU  H'0001'
OUT26            EQU  H'0002'
OUT27            EQU  H'0003'
OUT28            EQU  H'0004'
OUT29            EQU  H'0005'
OUT30            EQU  H'0006'
OUT31            EQU  H'0007'
SHIFT24          EQU  H'0000'
SHIFT25          EQU  H'0001'
SHIFT26          EQU  H'0002'
SHIFT27          EQU  H'0003'
SHIFT28          EQU  H'0004'
SHIFT29          EQU  H'0005'
SHIFT30          EQU  H'0006'
SHIFT31          EQU  H'0007'
XOR24            EQU  H'0000'
XOR25            EQU  H'0001'
XOR26            EQU  H'0002'
XOR27            EQU  H'0003'
XOR28            EQU  H'0004'
XOR29            EQU  H'0005'
XOR30            EQU  H'0006'
XOR31            EQU  H'0007'


;----- CRCCON0 Bits -----------------------------------------------------
FULL             EQU  H'0000'
LENDIAN          EQU  H'0001'
ACCMOD           EQU  H'0004'
CRCBUSY          EQU  H'0005'
CRCGO            EQU  H'0006'
CRCEN            EQU  H'0007'
SHIFTM           EQU  H'0001'
SETUP0           EQU  H'0002'
SETUP1           EQU  H'0003'
ACCM             EQU  H'0004'
BUSY_CRCCON0     EQU  H'0005'
GO_CRCCON0       EQU  H'0006'
EN               EQU  H'0007'


;----- CRCCON2 Bits -----------------------------------------------------
DLEN0            EQU  H'0000'
DLEN1            EQU  H'0001'
DLEN2            EQU  H'0002'
DLEN3            EQU  H'0003'
DLEN4            EQU  H'0004'


;----- SCANLADR Bits -----------------------------------------------------
LADR0            EQU  H'0000'
LADR1            EQU  H'0001'
LADR2            EQU  H'0002'
LADR3            EQU  H'0003'
LADR4            EQU  H'0004'
LADR5            EQU  H'0005'
LADR6            EQU  H'0006'
LADR7            EQU  H'0007'
SCANLADR0        EQU  H'0000'
SCANLADR1        EQU  H'0001'
SCANLADR2        EQU  H'0002'
SCANLADR3        EQU  H'0003'
SCANLADR4        EQU  H'0004'
SCANLADR5        EQU  H'0005'
SCANLADR6        EQU  H'0006'
SCANLADR7        EQU  H'0007'


;----- SCANLADRH Bits -----------------------------------------------------
LADR8            EQU  H'0000'
LADR9            EQU  H'0001'
LADR10           EQU  H'0002'
LADR11           EQU  H'0003'
LADR12           EQU  H'0004'
LADR13           EQU  H'0005'
LADR14           EQU  H'0006'
LADR15           EQU  H'0007'
SCANLADR8        EQU  H'0000'
SCANLADR9        EQU  H'0001'
SCANLADR10       EQU  H'0002'
SCANLADR11       EQU  H'0003'
SCANLADR12       EQU  H'0004'
SCANLADR13       EQU  H'0005'
SCANLADR14       EQU  H'0006'
SCANLADR15       EQU  H'0007'


;----- SCANLADRU Bits -----------------------------------------------------
LADR16           EQU  H'0000'
LADR17           EQU  H'0001'
LADR18           EQU  H'0002'
LADR19           EQU  H'0003'
LADR20           EQU  H'0004'
LADR21           EQU  H'0005'
SCANLADR16       EQU  H'0000'
SCANLADR17       EQU  H'0001'
SCANLADR18       EQU  H'0002'
SCANLADR19       EQU  H'0003'
SCANLADR20       EQU  H'0004'
SCANLADR21       EQU  H'0005'


;----- SCANHADR Bits -----------------------------------------------------
HADR0            EQU  H'0000'
HADR1            EQU  H'0001'
HADR2            EQU  H'0002'
HADR3            EQU  H'0003'
HADR4            EQU  H'0004'
HADR5            EQU  H'0005'
HADR6            EQU  H'0006'
HADR7            EQU  H'0007'
SCANHADR0        EQU  H'0000'
SCANHADR1        EQU  H'0001'
SCANHADR2        EQU  H'0002'
SCANHADR3        EQU  H'0003'
SCANHADR4        EQU  H'0004'
SCANHADR5        EQU  H'0005'
SCANHADR6        EQU  H'0006'
SCANHADR7        EQU  H'0007'


;----- SCANHADRH Bits -----------------------------------------------------
HADR8            EQU  H'0000'
HADR9            EQU  H'0001'
HADR10           EQU  H'0002'
HADR11           EQU  H'0003'
HADR12           EQU  H'0004'
HADR13           EQU  H'0005'
HADR14           EQU  H'0006'
HADR15           EQU  H'0007'
SCANHADR8        EQU  H'0000'
SCANHADR9        EQU  H'0001'
SCANHADR10       EQU  H'0002'
SCANHADR11       EQU  H'0003'
SCANHADR12       EQU  H'0004'
SCANHADR13       EQU  H'0005'
SCANHADR14       EQU  H'0006'
SCANHADR15       EQU  H'0007'


;----- SCANHADRU Bits -----------------------------------------------------
HADR16           EQU  H'0000'
HADR17           EQU  H'0001'
HADR18           EQU  H'0002'
HADR19           EQU  H'0003'
HADR20           EQU  H'0004'
HADR21           EQU  H'0005'
SCANHADR16       EQU  H'0000'
SCANHADR17       EQU  H'0001'
SCANHADR18       EQU  H'0002'
SCANHADR19       EQU  H'0003'
SCANHADR20       EQU  H'0004'
SCANHADR21       EQU  H'0005'


;----- SCANCON0 Bits -----------------------------------------------------
BUSY_SCANCON0    EQU  H'0000'
BURSTMD          EQU  H'0001'
MREG             EQU  H'0002'
SGO              EQU  H'0005'
TRIGEN           EQU  H'0006'
EN               EQU  H'0007'
SCANEN           EQU  H'0007'


;----- SCANTRIG Bits -----------------------------------------------------
TSEL0            EQU  H'0000'
TSEL1            EQU  H'0001'
TSEL2            EQU  H'0002'
TSEL3            EQU  H'0003'
TSEL4            EQU  H'0004'


;----- IPR0 Bits -----------------------------------------------------
SWIP             EQU  H'0000'
HLVDIP           EQU  H'0001'
OSFIP            EQU  H'0002'
CSWIP            EQU  H'0003'
TU16AIP          EQU  H'0004'
CLC1IP           EQU  H'0005'
CANIP            EQU  H'0006'
IOCIP            EQU  H'0007'


;----- IPR2 Bits -----------------------------------------------------
ADTIP            EQU  H'0000'
ADCH2IP          EQU  H'0001'
ADCH3IP          EQU  H'0002'
ADCH4IP          EQU  H'0003'
DMA1SCNTIP       EQU  H'0004'
DMA1DCNTIP       EQU  H'0005'
DMA1ORIP         EQU  H'0006'
DMA1AIP          EQU  H'0007'
ADCH1IP          EQU  H'0000'


;----- IPR3 Bits -----------------------------------------------------
SPI1RXIP         EQU  H'0000'
SPI1TXIP         EQU  H'0001'
SPI1IP           EQU  H'0002'
TMR2IP           EQU  H'0003'
TMR1IP           EQU  H'0004'
TMR1GIP          EQU  H'0005'
CCP1IP           EQU  H'0006'
TMR0IP           EQU  H'0007'


;----- IPR4 Bits -----------------------------------------------------
U1RXIP           EQU  H'0000'
U1TXIP           EQU  H'0001'
U1EIP            EQU  H'0002'
U1IP             EQU  H'0003'
CANRXIP          EQU  H'0004'
CANTXIP          EQU  H'0005'
PWM1PIP          EQU  H'0006'
PWM1IP           EQU  H'0007'


;----- IPR5 Bits -----------------------------------------------------
SPI2RXIP         EQU  H'0000'
SPI2TXIP         EQU  H'0001'
SPI2IP           EQU  H'0002'
TU16BIP          EQU  H'0003'
TMR3IP           EQU  H'0004'
TMR3GIP          EQU  H'0005'
PWM2PIP          EQU  H'0006'
PWM2IP           EQU  H'0007'


;----- IPR6 Bits -----------------------------------------------------
INT1IP           EQU  H'0000'
CLC2IP           EQU  H'0001'
CWG1IP           EQU  H'0002'
NCO1IP           EQU  H'0003'
DMA2SCNTIP       EQU  H'0004'
DMA2DCNTIP       EQU  H'0005'
DMA2ORIP         EQU  H'0006'
DMA2AIP          EQU  H'0007'


;----- IPR7 Bits -----------------------------------------------------
I2C1RXIP         EQU  H'0000'
I2C1TXIP         EQU  H'0001'
I2C1IP           EQU  H'0002'
I2C1EIP          EQU  H'0003'
CLC3IP           EQU  H'0005'
PWM3PIP          EQU  H'0006'
PWM3IP           EQU  H'0007'


;----- IPR8 Bits -----------------------------------------------------
U2RXIP           EQU  H'0000'
U2TXIP           EQU  H'0001'
U2EIP            EQU  H'0002'
U2IP             EQU  H'0003'
TMR5IP           EQU  H'0004'
TMR5GIP          EQU  H'0005'
CCP2IP           EQU  H'0006'
SCANIP           EQU  H'0007'


;----- IPR9 Bits -----------------------------------------------------
U3RXIP           EQU  H'0000'
U3TXIP           EQU  H'0001'
U3EIP            EQU  H'0002'
U3IP             EQU  H'0003'
CLC4IP           EQU  H'0005'
PWM4PIP          EQU  H'0006'
PWM4IP           EQU  H'0007'


;----- IPR10 Bits -----------------------------------------------------
INT2IP           EQU  H'0000'
CLC5IP           EQU  H'0001'
CWG2IP           EQU  H'0002'
NCO2IP           EQU  H'0003'
DMA3SCNTIP       EQU  H'0004'
DMA3DCNTIP       EQU  H'0005'
DMA3ORIP         EQU  H'0006'
DMA3AIP          EQU  H'0007'


;----- IPR12 Bits -----------------------------------------------------
U4RXIP           EQU  H'0000'
U4TXIP           EQU  H'0001'
U4EIP            EQU  H'0002'
U4IP             EQU  H'0003'
DMA5SCNTIP       EQU  H'0004'
DMA5DCNTIP       EQU  H'0005'
DMA5ORIP         EQU  H'0006'
DMA5AIP          EQU  H'0007'


;----- IPR13 Bits -----------------------------------------------------
U5RXIP           EQU  H'0000'
U5TXIP           EQU  H'0001'
U5EIP            EQU  H'0002'
U5IP             EQU  H'0003'
DMA6SCNTIP       EQU  H'0004'
DMA6DCNTIP       EQU  H'0005'
DMA6ORIP         EQU  H'0006'
DMA6AIP          EQU  H'0007'


;----- IPR14 Bits -----------------------------------------------------
CLC7IP           EQU  H'0001'
C2IP             EQU  H'0002'
NCO3IP           EQU  H'0003'
DMA7SCNTIP       EQU  H'0004'
DMA7DCNTIP       EQU  H'0005'
DMA7ORIP         EQU  H'0006'
DMA7AIP          EQU  H'0007'


;----- IPR15 Bits -----------------------------------------------------
NVMIP            EQU  H'0000'
CLC8IP           EQU  H'0001'
CRCIP            EQU  H'0002'
TMR6IP           EQU  H'0003'
DMA8SCNTIP       EQU  H'0004'
DMA8DCNTIP       EQU  H'0005'
DMA8ORIP         EQU  H'0006'
DMA8AIP          EQU  H'0007'


;----- STATUS_CSHAD Bits -----------------------------------------------------
NOT_PD           EQU  H'0005'
NOT_TO           EQU  H'0006'
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
OV_STATUS_CSHAD  EQU  H'0003'
N                EQU  H'0004'
NPD              EQU  H'0005'
NTO              EQU  H'0006'
PD               EQU  H'0005'
TO               EQU  H'0006'


;----- SHADCON Bits -----------------------------------------------------
SHADLO           EQU  H'0000'


;----- STATUS_SHAD Bits -----------------------------------------------------
NOT_PD           EQU  H'0005'
NOT_TO           EQU  H'0006'
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
OV_STATUS_SHAD   EQU  H'0003'
N                EQU  H'0004'
NPD              EQU  H'0005'
NTO              EQU  H'0006'
PD               EQU  H'0005'
TO               EQU  H'0006'


;----- TU16ACON0 Bits -----------------------------------------------------
CIE              EQU  H'0000'
ZIE              EQU  H'0001'
PRIE             EQU  H'0002'
RDSEL            EQU  H'0003'
OPOL             EQU  H'0004'
OM               EQU  H'0005'
CPOL             EQU  H'0006'
ON_TU16ACON0     EQU  H'0007'
TU16ACIE         EQU  H'0000'
TU16AZIE         EQU  H'0001'
TU16APRIE        EQU  H'0002'
TU16ARDSEL       EQU  H'0003'
TU16AOPOL        EQU  H'0004'
TU16AOM          EQU  H'0005'
TU16ACPOL        EQU  H'0006'
TU16AON          EQU  H'0007'


;----- TU16ACON1 Bits -----------------------------------------------------
CIF              EQU  H'0000'
ZIF              EQU  H'0001'
PRIF             EQU  H'0002'
CAPT             EQU  H'0003'
LIMIT            EQU  H'0004'
CLR              EQU  H'0005'
OSEN             EQU  H'0006'
RUN              EQU  H'0007'
TU16ACIF         EQU  H'0000'
TU16AZIF         EQU  H'0001'
TU16APRIF        EQU  H'0002'
TU16ACAPT        EQU  H'0003'
TU16ALIMIT       EQU  H'0004'
TU16ACLR         EQU  H'0005'
TMRAOSEN         EQU  H'0006'
TU16ARUN         EQU  H'0007'


;----- TU16AHLT Bits -----------------------------------------------------
CSYNC            EQU  H'0006'
EPOL             EQU  H'0007'
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
TU16ACSYNC       EQU  H'0006'
TU16AEPOL        EQU  H'0007'
TU16ASTOP0       EQU  H'0000'
TU16ASTOP1       EQU  H'0001'
TU16ARESET0      EQU  H'0002'
TU16ARESET1      EQU  H'0003'
TU16ASTART0      EQU  H'0004'
TU16ASTART1      EQU  H'0005'


;----- TU16APS Bits -----------------------------------------------------
PS0              EQU  H'0000'
PS1              EQU  H'0001'
PS2              EQU  H'0002'
PS3              EQU  H'0003'
PS4              EQU  H'0004'
PS5              EQU  H'0005'
PS6              EQU  H'0006'
PS7              EQU  H'0007'
TU16APS0         EQU  H'0000'
TU16APS1         EQU  H'0001'
TU16APS2         EQU  H'0002'
TU16APS3         EQU  H'0003'
TU16APS4         EQU  H'0004'
TU16APS5         EQU  H'0005'
TU16APS6         EQU  H'0006'
TU16APS7         EQU  H'0007'


;----- TU16ATMR Bits -----------------------------------------------------
TMRL0            EQU  H'0000'
TMRL1            EQU  H'0001'
TMRL2            EQU  H'0002'
TMRL3            EQU  H'0003'
TMRL4            EQU  H'0004'
TMRL5            EQU  H'0005'
TMRL6            EQU  H'0006'
TMRL7            EQU  H'0007'
TU16ATMRL0       EQU  H'0000'
TU16ATMRL1       EQU  H'0001'
TU16ATMRL2       EQU  H'0002'
TU16ATMRL3       EQU  H'0003'
TU16ATMRL4       EQU  H'0004'
TU16ATMRL5       EQU  H'0005'
TU16ATMRL6       EQU  H'0006'
TU16ATMRL7       EQU  H'0007'
TU16ATMR0        EQU  H'0000'
TU16ATMR1        EQU  H'0001'
TU16ATMR2        EQU  H'0002'
TU16ATMR3        EQU  H'0003'
TU16ATMR4        EQU  H'0004'
TU16ATMR5        EQU  H'0005'
TU16ATMR6        EQU  H'0006'
TU16ATMR7        EQU  H'0007'
CRL0             EQU  H'0000'
CRL1             EQU  H'0001'
CRL2             EQU  H'0002'
CRL3             EQU  H'0003'
CRL4             EQU  H'0004'
CRL5             EQU  H'0005'
CRL6             EQU  H'0006'
CRL7             EQU  H'0007'
TU16ACRL0        EQU  H'0000'
TU16ACRL1        EQU  H'0001'
TU16ACRL2        EQU  H'0002'
TU16ACRL3        EQU  H'0003'
TU16ACRL4        EQU  H'0004'
TU16ACRL5        EQU  H'0005'
TU16ACRL6        EQU  H'0006'
TU16ACRL7        EQU  H'0007'
TU16ACR0         EQU  H'0000'
TU16ACR1         EQU  H'0001'
TU16ACR2         EQU  H'0002'
TU16ACR3         EQU  H'0003'
TU16ACR4         EQU  H'0004'
TU16ACR5         EQU  H'0005'
TU16ACR6         EQU  H'0006'
TU16ACR7         EQU  H'0007'


;----- TU16ATMRH Bits -----------------------------------------------------
TMRH0            EQU  H'0000'
TMRH1            EQU  H'0001'
TMRH2            EQU  H'0002'
TMRH3            EQU  H'0003'
TMRH4            EQU  H'0004'
TMRH5            EQU  H'0005'
TMRH6            EQU  H'0006'
TMRH7            EQU  H'0007'
TU16ATMRH0       EQU  H'0000'
TU16ATMRH1       EQU  H'0001'
TU16ATMRH2       EQU  H'0002'
TU16ATMRH3       EQU  H'0003'
TU16ATMRH4       EQU  H'0004'
TU16ATMRH5       EQU  H'0005'
TU16ATMRH6       EQU  H'0006'
TU16ATMRH7       EQU  H'0007'
TU16ATMR8        EQU  H'0000'
TU16ATMR9        EQU  H'0001'
TU16ATMR10       EQU  H'0002'
TU16ATMR11       EQU  H'0003'
TU16ATMR12       EQU  H'0004'
TU16ATMR13       EQU  H'0005'
TU16ATMR14       EQU  H'0006'
TU16ATMR15       EQU  H'0007'
CRH0             EQU  H'0000'
CRH1             EQU  H'0001'
CRH2             EQU  H'0002'
CRH3             EQU  H'0003'
CRH4             EQU  H'0004'
CRH5             EQU  H'0005'
CRH6             EQU  H'0006'
CRH7             EQU  H'0007'
TU16ACRH0        EQU  H'0000'
TU16ACRH1        EQU  H'0001'
TU16ACRH2        EQU  H'0002'
TU16ACRH3        EQU  H'0003'
TU16ACRH4        EQU  H'0004'
TU16ACRH5        EQU  H'0005'
TU16ACRH6        EQU  H'0006'
TU16ACRH7        EQU  H'0007'
TU16ACR8         EQU  H'0000'
TU16ACR9         EQU  H'0001'
TU16ACR10        EQU  H'0002'
TU16ACR11        EQU  H'0003'
TU16ACR12        EQU  H'0004'
TU16ACR13        EQU  H'0005'
TU16ACR14        EQU  H'0006'
TU16ACR15        EQU  H'0007'


;----- TU16APR Bits -----------------------------------------------------
PRL0             EQU  H'0000'
PRL1             EQU  H'0001'
PRL2             EQU  H'0002'
PRL3             EQU  H'0003'
PRL4             EQU  H'0004'
PRL5             EQU  H'0005'
PRL6             EQU  H'0006'
PRL7             EQU  H'0007'
TU16APRL0        EQU  H'0000'
TU16APRL1        EQU  H'0001'
TU16APRL2        EQU  H'0002'
TU16APRL3        EQU  H'0003'
TU16APRL4        EQU  H'0004'
TU16APRL5        EQU  H'0005'
TU16APRL6        EQU  H'0006'
TU16APRL7        EQU  H'0007'
TU16APR0         EQU  H'0000'
TU16APR1         EQU  H'0001'
TU16APR2         EQU  H'0002'
TU16APR3         EQU  H'0003'
TU16APR4         EQU  H'0004'
TU16APR5         EQU  H'0005'
TU16APR6         EQU  H'0006'
TU16APR7         EQU  H'0007'


;----- TU16APRH Bits -----------------------------------------------------
PRH0             EQU  H'0000'
PRH1             EQU  H'0001'
PRH2             EQU  H'0002'
PRH3             EQU  H'0003'
PRH4             EQU  H'0004'
PRH5             EQU  H'0005'
PRH6             EQU  H'0006'
PRH7             EQU  H'0007'
TU16APRH0        EQU  H'0000'
TU16APRH1        EQU  H'0001'
TU16APRH2        EQU  H'0002'
TU16APRH3        EQU  H'0003'
TU16APRH4        EQU  H'0004'
TU16APRH5        EQU  H'0005'
TU16APRH6        EQU  H'0006'
TU16APRH7        EQU  H'0007'
TU16APR8         EQU  H'0000'
TU16APR9         EQU  H'0001'
TU16APR10        EQU  H'0002'
TU16APR11        EQU  H'0003'
TU16APR12        EQU  H'0004'
TU16APR13        EQU  H'0005'
TU16APR14        EQU  H'0006'
TU16APR15        EQU  H'0007'


;----- TU16ACLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
TU16ACLK0        EQU  H'0000'
TU16ACLK1        EQU  H'0001'
TU16ACLK2        EQU  H'0002'
TU16ACLK3        EQU  H'0003'
TU16ACLK4        EQU  H'0004'


;----- TU16AERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'
ERS4             EQU  H'0004'
ERS5             EQU  H'0005'
TU16AERS0        EQU  H'0000'
TU16AERS1        EQU  H'0001'
TU16AERS2        EQU  H'0002'
TU16AERS3        EQU  H'0003'
TU16AERS4        EQU  H'0004'
TU16AERS5        EQU  H'0005'


;----- TU16BCON0 Bits -----------------------------------------------------
CIE              EQU  H'0000'
ZIE              EQU  H'0001'
PRIE             EQU  H'0002'
RDSEL            EQU  H'0003'
OPOL             EQU  H'0004'
OM               EQU  H'0005'
CPOL             EQU  H'0006'
ON_TU16BCON0     EQU  H'0007'
TU16BCIE         EQU  H'0000'
TU16BZIE         EQU  H'0001'
TU16BPRIE        EQU  H'0002'
TU16BRDSEL       EQU  H'0003'
TU16BOPOL        EQU  H'0004'
TU16BOM          EQU  H'0005'
TU16BCPOL        EQU  H'0006'
TU16BON          EQU  H'0007'


;----- TU16BCON1 Bits -----------------------------------------------------
CIF              EQU  H'0000'
ZIF              EQU  H'0001'
PRIF             EQU  H'0002'
CAPT             EQU  H'0003'
LIMIT            EQU  H'0004'
CLR              EQU  H'0005'
OSEN             EQU  H'0006'
RUN              EQU  H'0007'
TU16BCIF         EQU  H'0000'
TU16BZIF         EQU  H'0001'
TU16BPRIF        EQU  H'0002'
TU16BCAPT        EQU  H'0003'
TU16BLIMIT       EQU  H'0004'
TU16BCLR         EQU  H'0005'
TMRBOSEN         EQU  H'0006'
TU16BRUN         EQU  H'0007'


;----- TU16BHLT Bits -----------------------------------------------------
CSYNC            EQU  H'0006'
EPOL             EQU  H'0007'
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
TU16BCSYNC       EQU  H'0006'
TU16BEPOL        EQU  H'0007'
TU16BSTOP0       EQU  H'0000'
TU16BSTOP1       EQU  H'0001'
TU16BRESET0      EQU  H'0002'
TU16BRESET1      EQU  H'0003'
TU16BSTART0      EQU  H'0004'
TU16BSTART1      EQU  H'0005'


;----- TU16BPS Bits -----------------------------------------------------
PS0              EQU  H'0000'
PS1              EQU  H'0001'
PS2              EQU  H'0002'
PS3              EQU  H'0003'
PS4              EQU  H'0004'
PS5              EQU  H'0005'
PS6              EQU  H'0006'
PS7              EQU  H'0007'
TU16BPS0         EQU  H'0000'
TU16BPS1         EQU  H'0001'
TU16BPS2         EQU  H'0002'
TU16BPS3         EQU  H'0003'
TU16BPS4         EQU  H'0004'
TU16BPS5         EQU  H'0005'
TU16BPS6         EQU  H'0006'
TU16BPS7         EQU  H'0007'


;----- TU16BTMR Bits -----------------------------------------------------
TMRL0            EQU  H'0000'
TMRL1            EQU  H'0001'
TMRL2            EQU  H'0002'
TMRL3            EQU  H'0003'
TMRL4            EQU  H'0004'
TMRL5            EQU  H'0005'
TMRL6            EQU  H'0006'
TMRL7            EQU  H'0007'
TU16BTMRL0       EQU  H'0000'
TU16BTMRL1       EQU  H'0001'
TU16BTMRL2       EQU  H'0002'
TU16BTMRL3       EQU  H'0003'
TU16BTMRL4       EQU  H'0004'
TU16BTMRL5       EQU  H'0005'
TU16BTMRL6       EQU  H'0006'
TU16BTMRL7       EQU  H'0007'
TU16BTMR0        EQU  H'0000'
TU16BTMR1        EQU  H'0001'
TU16BTMR2        EQU  H'0002'
TU16BTMR3        EQU  H'0003'
TU16BTMR4        EQU  H'0004'
TU16BTMR5        EQU  H'0005'
TU16BTMR6        EQU  H'0006'
TU16BTMR7        EQU  H'0007'
CRL0             EQU  H'0000'
CRL1             EQU  H'0001'
CRL2             EQU  H'0002'
CRL3             EQU  H'0003'
CRL4             EQU  H'0004'
CRL5             EQU  H'0005'
CRL6             EQU  H'0006'
CRL7             EQU  H'0007'
TU16BCRL0        EQU  H'0000'
TU16BCRL1        EQU  H'0001'
TU16BCRL2        EQU  H'0002'
TU16BCRL3        EQU  H'0003'
TU16BCRL4        EQU  H'0004'
TU16BCRL5        EQU  H'0005'
TU16BCRL6        EQU  H'0006'
TU16BCRL7        EQU  H'0007'
TU16BCR0         EQU  H'0000'
TU16BCR1         EQU  H'0001'
TU16BCR2         EQU  H'0002'
TU16BCR3         EQU  H'0003'
TU16BCR4         EQU  H'0004'
TU16BCR5         EQU  H'0005'
TU16BCR6         EQU  H'0006'
TU16BCR7         EQU  H'0007'


;----- TU16BTMRH Bits -----------------------------------------------------
TMRH0            EQU  H'0000'
TMRH1            EQU  H'0001'
TMRH2            EQU  H'0002'
TMRH3            EQU  H'0003'
TMRH4            EQU  H'0004'
TMRH5            EQU  H'0005'
TMRH6            EQU  H'0006'
TMRH7            EQU  H'0007'
TU16BTMRH0       EQU  H'0000'
TU16BTMRH1       EQU  H'0001'
TU16BTMRH2       EQU  H'0002'
TU16BTMRH3       EQU  H'0003'
TU16BTMRH4       EQU  H'0004'
TU16BTMRH5       EQU  H'0005'
TU16BTMRH6       EQU  H'0006'
TU16BTMRH7       EQU  H'0007'
TU16BTMR8        EQU  H'0000'
TU16BTMR9        EQU  H'0001'
TU16BTMR10       EQU  H'0002'
TU16BTMR11       EQU  H'0003'
TU16BTMR12       EQU  H'0004'
TU16BTMR13       EQU  H'0005'
TU16BTMR14       EQU  H'0006'
TU16BTMR15       EQU  H'0007'
CRH0             EQU  H'0000'
CRH1             EQU  H'0001'
CRH2             EQU  H'0002'
CRH3             EQU  H'0003'
CRH4             EQU  H'0004'
CRH5             EQU  H'0005'
CRH6             EQU  H'0006'
CRH7             EQU  H'0007'
TU16BCRH0        EQU  H'0000'
TU16BCRH1        EQU  H'0001'
TU16BCRH2        EQU  H'0002'
TU16BCRH3        EQU  H'0003'
TU16BCRH4        EQU  H'0004'
TU16BCRH5        EQU  H'0005'
TU16BCRH6        EQU  H'0006'
TU16BCRH7        EQU  H'0007'
TU16BCR8         EQU  H'0000'
TU16BCR9         EQU  H'0001'
TU16BCR10        EQU  H'0002'
TU16BCR11        EQU  H'0003'
TU16BCR12        EQU  H'0004'
TU16BCR13        EQU  H'0005'
TU16BCR14        EQU  H'0006'
TU16BCR15        EQU  H'0007'


;----- TU16BPR Bits -----------------------------------------------------
PRL0             EQU  H'0000'
PRL1             EQU  H'0001'
PRL2             EQU  H'0002'
PRL3             EQU  H'0003'
PRL4             EQU  H'0004'
PRL5             EQU  H'0005'
PRL6             EQU  H'0006'
PRL7             EQU  H'0007'
TU16BPRL0        EQU  H'0000'
TU16BPRL1        EQU  H'0001'
TU16BPRL2        EQU  H'0002'
TU16BPRL3        EQU  H'0003'
TU16BPRL4        EQU  H'0004'
TU16BPRL5        EQU  H'0005'
TU16BPRL6        EQU  H'0006'
TU16BPRL7        EQU  H'0007'
TU16BPR0         EQU  H'0000'
TU16BPR1         EQU  H'0001'
TU16BPR2         EQU  H'0002'
TU16BPR3         EQU  H'0003'
TU16BPR4         EQU  H'0004'
TU16BPR5         EQU  H'0005'
TU16BPR6         EQU  H'0006'
TU16BPR7         EQU  H'0007'


;----- TU16BPRH Bits -----------------------------------------------------
PRH0             EQU  H'0000'
PRH1             EQU  H'0001'
PRH2             EQU  H'0002'
PRH3             EQU  H'0003'
PRH4             EQU  H'0004'
PRH5             EQU  H'0005'
PRH6             EQU  H'0006'
PRH7             EQU  H'0007'
TU16BPRH0        EQU  H'0000'
TU16BPRH1        EQU  H'0001'
TU16BPRH2        EQU  H'0002'
TU16BPRH3        EQU  H'0003'
TU16BPRH4        EQU  H'0004'
TU16BPRH5        EQU  H'0005'
TU16BPRH6        EQU  H'0006'
TU16BPRH7        EQU  H'0007'
TU16BPR8         EQU  H'0000'
TU16BPR9         EQU  H'0001'
TU16BPR10        EQU  H'0002'
TU16BPR11        EQU  H'0003'
TU16BPR12        EQU  H'0004'
TU16BPR13        EQU  H'0005'
TU16BPR14        EQU  H'0006'
TU16BPR15        EQU  H'0007'


;----- TU16BCLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
TU16BCLK0        EQU  H'0000'
TU16BCLK1        EQU  H'0001'
TU16BCLK2        EQU  H'0002'
TU16BCLK3        EQU  H'0003'
TU16BCLK4        EQU  H'0004'


;----- TU16BERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'
ERS4             EQU  H'0004'
ERS5             EQU  H'0005'
TU16BERS0        EQU  H'0000'
TU16BERS1        EQU  H'0001'
TU16BERS2        EQU  H'0002'
TU16BERS3        EQU  H'0003'
TU16BERS4        EQU  H'0004'
TU16BERS5        EQU  H'0005'


;----- TUCHAIN Bits -----------------------------------------------------
CH16AB           EQU  H'0000'


;----- CWG2CLK Bits -----------------------------------------------------
CS_CWG2CLK       EQU  H'0000'
CWG2CS           EQU  H'0000'


;----- CWG2ISM Bits -----------------------------------------------------
CWG2ISM0         EQU  H'0000'
CWG2ISM1         EQU  H'0001'
CWG2ISM2         EQU  H'0002'
CWG2ISM3         EQU  H'0003'


;----- CWG2DBR Bits -----------------------------------------------------
DBR0             EQU  H'0000'
DBR1             EQU  H'0001'
DBR2             EQU  H'0002'
DBR3             EQU  H'0003'
DBR4             EQU  H'0004'
DBR5             EQU  H'0005'
CWG2DBR0         EQU  H'0000'
CWG2DBR1         EQU  H'0001'
CWG2DBR2         EQU  H'0002'
CWG2DBR3         EQU  H'0003'
CWG2DBR4         EQU  H'0004'
CWG2DBR5         EQU  H'0005'


;----- CWG2DBF Bits -----------------------------------------------------
DBF0             EQU  H'0000'
DBF1             EQU  H'0001'
DBF2             EQU  H'0002'
DBF3             EQU  H'0003'
DBF4             EQU  H'0004'
DBF5             EQU  H'0005'
CWG2DBF0         EQU  H'0000'
CWG2DBF1         EQU  H'0001'
CWG2DBF2         EQU  H'0002'
CWG2DBF3         EQU  H'0003'
CWG2DBF4         EQU  H'0004'
CWG2DBF5         EQU  H'0005'


;----- CWG2CON0 Bits -----------------------------------------------------
LD_CWG2CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G2EN             EQU  H'0007'
CWG2LD           EQU  H'0006'
CWG2EN           EQU  H'0007'
CWG2MODE0        EQU  H'0000'
CWG2MODE1        EQU  H'0001'
CWG2MODE2        EQU  H'0002'


;----- CWG2CON1 Bits -----------------------------------------------------
POLA             EQU  H'0000'
POLB             EQU  H'0001'
POLC             EQU  H'0002'
POLD             EQU  H'0003'
IN               EQU  H'0005'
CWG2POLA         EQU  H'0000'
CWG2POLB         EQU  H'0001'
CWG2POLC         EQU  H'0002'
CWG2POLD         EQU  H'0003'
CWG2IN           EQU  H'0005'


;----- CWG2AS0 Bits -----------------------------------------------------
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG2REN          EQU  H'0006'
CWG2SHUTDOWN     EQU  H'0007'
CWG2LSAC0        EQU  H'0002'
CWG2LSAC1        EQU  H'0003'
CWG2LSBD0        EQU  H'0004'
CWG2LSBD1        EQU  H'0005'


;----- CWG2AS1 Bits -----------------------------------------------------
AS0E             EQU  H'0000'
AS1E             EQU  H'0001'
AS2E             EQU  H'0002'
AS3E             EQU  H'0003'
AS4E             EQU  H'0004'
AS5E             EQU  H'0005'
AS6E             EQU  H'0006'
AS7E             EQU  H'0007'


;----- CWG2STR Bits -----------------------------------------------------
STRA             EQU  H'0000'
STRB             EQU  H'0001'
STRC             EQU  H'0002'
STRD             EQU  H'0003'
OVRA             EQU  H'0004'
OVRB             EQU  H'0005'
OVRC             EQU  H'0006'
OVRD             EQU  H'0007'
CWG2STRA         EQU  H'0000'
CWG2STRB         EQU  H'0001'
CWG2STRC         EQU  H'0002'
CWG2STRD         EQU  H'0003'
CWG2OVRA         EQU  H'0004'
CWG2OVRB         EQU  H'0005'
CWG2OVRC         EQU  H'0006'
CWG2OVRD         EQU  H'0007'


;----- CWG3CLK Bits -----------------------------------------------------
CS_CWG3CLK       EQU  H'0000'
CWG3CS           EQU  H'0000'


;----- CWG3ISM Bits -----------------------------------------------------
CWG3ISM0         EQU  H'0000'
CWG3ISM1         EQU  H'0001'
CWG3ISM2         EQU  H'0002'
CWG3ISM3         EQU  H'0003'


;----- CWG3DBR Bits -----------------------------------------------------
DBR0             EQU  H'0000'
DBR1             EQU  H'0001'
DBR2             EQU  H'0002'
DBR3             EQU  H'0003'
DBR4             EQU  H'0004'
DBR5             EQU  H'0005'
CWG3DBR0         EQU  H'0000'
CWG3DBR1         EQU  H'0001'
CWG3DBR2         EQU  H'0002'
CWG3DBR3         EQU  H'0003'
CWG3DBR4         EQU  H'0004'
CWG3DBR5         EQU  H'0005'


;----- CWG3DBF Bits -----------------------------------------------------
DBF0             EQU  H'0000'
DBF1             EQU  H'0001'
DBF2             EQU  H'0002'
DBF3             EQU  H'0003'
DBF4             EQU  H'0004'
DBF5             EQU  H'0005'
CWG3DBF0         EQU  H'0000'
CWG3DBF1         EQU  H'0001'
CWG3DBF2         EQU  H'0002'
CWG3DBF3         EQU  H'0003'
CWG3DBF4         EQU  H'0004'
CWG3DBF5         EQU  H'0005'


;----- CWG3CON0 Bits -----------------------------------------------------
LD_CWG3CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G3EN             EQU  H'0007'
CWG3LD           EQU  H'0006'
CWG3EN           EQU  H'0007'
CWG3MODE0        EQU  H'0000'
CWG3MODE1        EQU  H'0001'
CWG3MODE2        EQU  H'0002'


;----- CWG3CON1 Bits -----------------------------------------------------
POLA             EQU  H'0000'
POLB             EQU  H'0001'
POLC             EQU  H'0002'
POLD             EQU  H'0003'
IN               EQU  H'0005'
CWG3POLA         EQU  H'0000'
CWG3POLB         EQU  H'0001'
CWG3POLC         EQU  H'0002'
CWG3POLD         EQU  H'0003'
CWG3IN           EQU  H'0005'


;----- CWG3AS0 Bits -----------------------------------------------------
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG3REN          EQU  H'0006'
CWG3SHUTDOWN     EQU  H'0007'
CWG3LSAC0        EQU  H'0002'
CWG3LSAC1        EQU  H'0003'
CWG3LSBD0        EQU  H'0004'
CWG3LSBD1        EQU  H'0005'


;----- CWG3AS1 Bits -----------------------------------------------------
AS0E             EQU  H'0000'
AS1E             EQU  H'0001'
AS2E             EQU  H'0002'
AS3E             EQU  H'0003'
AS4E             EQU  H'0004'
AS5E             EQU  H'0005'
AS6E             EQU  H'0006'
AS7E             EQU  H'0007'


;----- CWG3STR Bits -----------------------------------------------------
STRA             EQU  H'0000'
STRB             EQU  H'0001'
STRC             EQU  H'0002'
STRD             EQU  H'0003'
OVRA             EQU  H'0004'
OVRB             EQU  H'0005'
OVRC             EQU  H'0006'
OVRD             EQU  H'0007'
CWG3STRA         EQU  H'0000'
CWG3STRB         EQU  H'0001'
CWG3STRC         EQU  H'0002'
CWG3STRD         EQU  H'0003'
CWG3OVRA         EQU  H'0004'
CWG3OVRB         EQU  H'0005'
CWG3OVRC         EQU  H'0006'
CWG3OVRD         EQU  H'0007'


;----- FVRCON Bits -----------------------------------------------------
TSRNG            EQU  H'0004'
TSEN             EQU  H'0005'
RDY_FVRCON       EQU  H'0006'
EN               EQU  H'0007'
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
CDAFVR0          EQU  H'0002'
CDAFVR1          EQU  H'0003'
FVRRDY           EQU  H'0006'
FVREN            EQU  H'0007'


;----- ADCPCON Bits -----------------------------------------------------
CPRDY            EQU  H'0000'
ON_ADCPCON       EQU  H'0007'
ADCPRDY          EQU  H'0000'
CPON_ADCPCON     EQU  H'0007'
ADCPON           EQU  H'0007'


;----- ADLTH Bits -----------------------------------------------------
ADLTH0           EQU  H'0000'
ADLTH1           EQU  H'0001'
ADLTH2           EQU  H'0002'
ADLTH3           EQU  H'0003'
ADLTH4           EQU  H'0004'
ADLTH5           EQU  H'0005'
ADLTH6           EQU  H'0006'
ADLTH7           EQU  H'0007'
LTH0             EQU  H'0000'
LTH1             EQU  H'0001'
LTH2             EQU  H'0002'
LTH3             EQU  H'0003'
LTH4             EQU  H'0004'
LTH5             EQU  H'0005'
LTH6             EQU  H'0006'
LTH7             EQU  H'0007'


;----- ADLTHH Bits -----------------------------------------------------
ADLTH8           EQU  H'0000'
ADLTH9           EQU  H'0001'
ADLTH10          EQU  H'0002'
ADLTH11          EQU  H'0003'
ADLTH12          EQU  H'0004'
ADLTH13          EQU  H'0005'
ADLTH14          EQU  H'0006'
ADLTH15          EQU  H'0007'
LTH8             EQU  H'0000'
LTH9             EQU  H'0001'
LTH10            EQU  H'0002'
LTH11            EQU  H'0003'
LTH12            EQU  H'0004'
LTH13            EQU  H'0005'
LTH14            EQU  H'0006'
LTH15            EQU  H'0007'


;----- ADUTH Bits -----------------------------------------------------
ADUTH0           EQU  H'0000'
ADUTH1           EQU  H'0001'
ADUTH2           EQU  H'0002'
ADUTH3           EQU  H'0003'
ADUTH4           EQU  H'0004'
ADUTH5           EQU  H'0005'
ADUTH6           EQU  H'0006'
ADUTH7           EQU  H'0007'
UTH0             EQU  H'0000'
UTH1             EQU  H'0001'
UTH2             EQU  H'0002'
UTH3             EQU  H'0003'
UTH4             EQU  H'0004'
UTH5             EQU  H'0005'
UTH6             EQU  H'0006'
UTH7             EQU  H'0007'


;----- ADUTHH Bits -----------------------------------------------------
ADUTH8           EQU  H'0000'
ADUTH9           EQU  H'0001'
ADUTH10          EQU  H'0002'
ADUTH11          EQU  H'0003'
ADUTH12          EQU  H'0004'
ADUTH13          EQU  H'0005'
ADUTH14          EQU  H'0006'
ADUTH15          EQU  H'0007'
UTH8             EQU  H'0000'
UTH9             EQU  H'0001'
UTH10            EQU  H'0002'
UTH11            EQU  H'0003'
UTH12            EQU  H'0004'
UTH13            EQU  H'0005'
UTH14            EQU  H'0006'
UTH15            EQU  H'0007'


;----- ADERR Bits -----------------------------------------------------
ADERR0           EQU  H'0000'
ADERR1           EQU  H'0001'
ADERR2           EQU  H'0002'
ADERR3           EQU  H'0003'
ADERR4           EQU  H'0004'
ADERR5           EQU  H'0005'
ADERR6           EQU  H'0006'
ADERR7           EQU  H'0007'
ERR0             EQU  H'0000'
ERR1             EQU  H'0001'
ERR2             EQU  H'0002'
ERR3             EQU  H'0003'
ERR4             EQU  H'0004'
ERR5             EQU  H'0005'
ERR6             EQU  H'0006'
ERR7             EQU  H'0007'


;----- ADERRH Bits -----------------------------------------------------
ADERR8           EQU  H'0000'
ADERR9           EQU  H'0001'
ADERR10          EQU  H'0002'
ADERR11          EQU  H'0003'
ADERR12          EQU  H'0004'
ADERR13          EQU  H'0005'
ADERR14          EQU  H'0006'
ADERR15          EQU  H'0007'
ERR8             EQU  H'0000'
ERR9             EQU  H'0001'
ERR10            EQU  H'0002'
ERR11            EQU  H'0003'
ERR12            EQU  H'0004'
ERR13            EQU  H'0005'
ERR14            EQU  H'0006'
ERR15            EQU  H'0007'


;----- ADSTPT Bits -----------------------------------------------------
ADSTPT0          EQU  H'0000'
ADSTPT1          EQU  H'0001'
ADSTPT2          EQU  H'0002'
ADSTPT3          EQU  H'0003'
ADSTPT4          EQU  H'0004'
ADSTPT5          EQU  H'0005'
ADSTPT6          EQU  H'0006'
ADSTPT7          EQU  H'0007'
STPT0            EQU  H'0000'
STPT1            EQU  H'0001'
STPT2            EQU  H'0002'
STPT3            EQU  H'0003'
STPT4            EQU  H'0004'
STPT5            EQU  H'0005'
STPT6            EQU  H'0006'
STPT7            EQU  H'0007'


;----- ADSTPTH Bits -----------------------------------------------------
ADSTPT8          EQU  H'0000'
ADSTPT9          EQU  H'0001'
ADSTPT10         EQU  H'0002'
ADSTPT11         EQU  H'0003'
ADSTPT12         EQU  H'0004'
ADSTPT13         EQU  H'0005'
ADSTPT14         EQU  H'0006'
ADSTPT15         EQU  H'0007'
STPT8            EQU  H'0000'
STPT9            EQU  H'0001'
STPT10           EQU  H'0002'
STPT11           EQU  H'0003'
STPT12           EQU  H'0004'
STPT13           EQU  H'0005'
STPT15           EQU  H'0006'
STPT16           EQU  H'0007'


;----- ADFLTR Bits -----------------------------------------------------
ADFLTR0          EQU  H'0000'
ADFLTR1          EQU  H'0001'
ADFLTR2          EQU  H'0002'
ADFLTR3          EQU  H'0003'
ADFLTR4          EQU  H'0004'
ADFLTR5          EQU  H'0005'
ADFLTR6          EQU  H'0006'
ADFLTR7          EQU  H'0007'
FLTR0            EQU  H'0000'
FLTR1            EQU  H'0001'
FLTR2            EQU  H'0002'
FLTR3            EQU  H'0003'
FLTR4            EQU  H'0004'
FLTR5            EQU  H'0005'
FLTR6            EQU  H'0006'
FLTR7            EQU  H'0007'


;----- ADFLTRH Bits -----------------------------------------------------
ADFLTR8          EQU  H'0000'
ADFLTR9          EQU  H'0001'
ADFLTR10         EQU  H'0002'
ADFLTR11         EQU  H'0003'
ADFLTR12         EQU  H'0004'
ADFLTR13         EQU  H'0005'
ADFLTR14         EQU  H'0006'
ADFLTR15         EQU  H'0007'
FLTR8            EQU  H'0000'
FLTR9            EQU  H'0001'
FLTR10           EQU  H'0002'
FLTR11           EQU  H'0003'
FLTR12           EQU  H'0004'
FLTR13           EQU  H'0005'
FLTR14           EQU  H'0006'
FLTR15           EQU  H'0007'


;----- ADACC Bits -----------------------------------------------------
ADACC0           EQU  H'0000'
ADACC1           EQU  H'0001'
ADACC2           EQU  H'0002'
ADACC3           EQU  H'0003'
ADACC4           EQU  H'0004'
ADACC5           EQU  H'0005'
ADACC6           EQU  H'0006'
ADACC7           EQU  H'0007'
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- ADACCH Bits -----------------------------------------------------
ADACC8           EQU  H'0000'
ADACC9           EQU  H'0001'
ADACC10          EQU  H'0002'
ADACC11          EQU  H'0003'
ADACC12          EQU  H'0004'
ADACC13          EQU  H'0005'
ADACC14          EQU  H'0006'
ADACC15          EQU  H'0007'
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- ADACCU Bits -----------------------------------------------------
ADACC16          EQU  H'0000'
ADACC17          EQU  H'0001'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'


;----- ADCNT Bits -----------------------------------------------------
ADCNT0           EQU  H'0000'
ADCNT1           EQU  H'0001'
ADCNT2           EQU  H'0002'
ADCNT3           EQU  H'0003'
ADCNT4           EQU  H'0004'
ADCNT5           EQU  H'0005'
ADCNT6           EQU  H'0006'
ADCNT7           EQU  H'0007'
CNT0             EQU  H'0000'
CNT1             EQU  H'0001'
CNT2             EQU  H'0002'
CNT3             EQU  H'0003'
CNT4             EQU  H'0004'
CNT5             EQU  H'0005'
CNT6             EQU  H'0006'
CNT7             EQU  H'0007'


;----- ADRPT Bits -----------------------------------------------------
ADRPT0           EQU  H'0000'
ADRPT1           EQU  H'0001'
ADRPT2           EQU  H'0002'
ADRPT3           EQU  H'0003'
ADRPT4           EQU  H'0004'
ADRPT5           EQU  H'0005'
ADRPT6           EQU  H'0006'
ADRPT7           EQU  H'0007'
RPT0             EQU  H'0000'
RPT1             EQU  H'0001'
RPT2             EQU  H'0002'
RPT3             EQU  H'0003'
RPT4             EQU  H'0004'
RPT5             EQU  H'0005'
RPT6             EQU  H'0006'
RPT7             EQU  H'0007'


;----- ADPREV Bits -----------------------------------------------------
ADPREV0          EQU  H'0000'
ADPREV1          EQU  H'0001'
ADPREV2          EQU  H'0002'
ADPREV3          EQU  H'0003'
ADPREV4          EQU  H'0004'
ADPREV5          EQU  H'0005'
ADPREV6          EQU  H'0006'
ADPREV7          EQU  H'0007'
PREV0            EQU  H'0000'
PREV1            EQU  H'0001'
PREV2            EQU  H'0002'
PREV3            EQU  H'0003'
PREV4            EQU  H'0004'
PREV5            EQU  H'0005'
PREV6            EQU  H'0006'
PREV7            EQU  H'0007'


;----- ADPREVH Bits -----------------------------------------------------
ADPREV8          EQU  H'0000'
ADPREV9          EQU  H'0001'
ADPREV10         EQU  H'0002'
ADPREV11         EQU  H'0003'
ADPREV12         EQU  H'0004'
ADPREV13         EQU  H'0005'
ADPREV14         EQU  H'0006'
ADPREV15         EQU  H'0007'
PREV8            EQU  H'0000'
PREV9            EQU  H'0001'
PREV10           EQU  H'0002'
PREV11           EQU  H'0003'
PREV12           EQU  H'0004'
PREV13           EQU  H'0005'
PREV14           EQU  H'0006'
PREV15           EQU  H'0007'


;----- ADRES Bits -----------------------------------------------------
ADRES0           EQU  H'0000'
ADRES1           EQU  H'0001'
ADRES2           EQU  H'0002'
ADRES3           EQU  H'0003'
ADRES4           EQU  H'0004'
ADRES5           EQU  H'0005'
ADRES6           EQU  H'0006'
ADRES7           EQU  H'0007'
RES0             EQU  H'0000'
RES1             EQU  H'0001'
RES2             EQU  H'0002'
RES3             EQU  H'0003'
RES4             EQU  H'0004'
RES5             EQU  H'0005'
RES6             EQU  H'0006'
RES7             EQU  H'0007'


;----- ADRESH Bits -----------------------------------------------------
ADRES8           EQU  H'0000'
ADRES9           EQU  H'0001'
ADRES10          EQU  H'0002'
ADRES11          EQU  H'0003'
ADRES12          EQU  H'0004'
ADRES13          EQU  H'0005'
ADRES14          EQU  H'0006'
ADRES15          EQU  H'0007'
RES8             EQU  H'0000'
RES9             EQU  H'0001'
RES10            EQU  H'0002'
RES11            EQU  H'0003'
RES12            EQU  H'0004'
RES13            EQU  H'0005'
RES14            EQU  H'0006'
RES15            EQU  H'0007'


;----- ADPCH Bits -----------------------------------------------------
ADPCH0           EQU  H'0000'
ADPCH1           EQU  H'0001'
ADPCH2           EQU  H'0002'
ADPCH3           EQU  H'0003'
ADPCH4           EQU  H'0004'
ADPCH5           EQU  H'0005'


;----- ADACQ Bits -----------------------------------------------------
ADACQ0           EQU  H'0000'
ADACQ1           EQU  H'0001'
ADACQ2           EQU  H'0002'
ADACQ3           EQU  H'0003'
ADACQ4           EQU  H'0004'
ADACQ5           EQU  H'0005'
ADACQ6           EQU  H'0006'
ADACQ7           EQU  H'0007'
ACQ0             EQU  H'0000'
ACQ1             EQU  H'0001'
ACQ2             EQU  H'0002'
ACQ3             EQU  H'0003'
ACQ4             EQU  H'0004'
ACQ5             EQU  H'0005'
ACQ6             EQU  H'0006'
ACQ7             EQU  H'0007'


;----- ADACQH Bits -----------------------------------------------------
ADACQ8           EQU  H'0000'
ADACQ9           EQU  H'0001'
ADACQ10          EQU  H'0002'
ADACQ11          EQU  H'0003'
ADACQ12          EQU  H'0004'
ACQ8             EQU  H'0000'
ACQ9             EQU  H'0001'
ACQ10            EQU  H'0002'
ACQ11            EQU  H'0003'
ACQ12            EQU  H'0004'


;----- ADCAP Bits -----------------------------------------------------
ADCAP0           EQU  H'0000'
ADCAP1           EQU  H'0001'
ADCAP2           EQU  H'0002'
ADCAP3           EQU  H'0003'
ADCAP4           EQU  H'0004'


;----- ADPRE Bits -----------------------------------------------------
PRE0             EQU  H'0000'
PRE1             EQU  H'0001'
PRE2             EQU  H'0002'
PRE3             EQU  H'0003'
PRE4             EQU  H'0004'
PRE5             EQU  H'0005'
PRE6             EQU  H'0006'
PRE7             EQU  H'0007'
ADPRE0           EQU  H'0000'
ADPRE1           EQU  H'0001'
ADPRE2           EQU  H'0002'
ADPRE3           EQU  H'0003'
ADPRE4           EQU  H'0004'
ADPRE5           EQU  H'0005'
ADPRE6           EQU  H'0006'
ADPRE7           EQU  H'0007'


;----- ADPREH Bits -----------------------------------------------------
PRE8             EQU  H'0000'
PRE9             EQU  H'0001'
PRE10            EQU  H'0002'
PRE11            EQU  H'0003'
PRE12            EQU  H'0004'
ADPRE8           EQU  H'0000'
ADPRE9           EQU  H'0001'
ADPRE10          EQU  H'0002'
ADPRE11          EQU  H'0003'
ADPRE12          EQU  H'0004'


;----- ADCON0 Bits -----------------------------------------------------
GO_ADCON0        EQU  H'0000'
CS_ADCON0        EQU  H'0004'
CSEN             EQU  H'0005'
CONT             EQU  H'0006'
ON_ADCON0        EQU  H'0007'
ADGO             EQU  H'0000'
ADCS             EQU  H'0004'
ADCSEN           EQU  H'0005'
ADCONT           EQU  H'0006'
ADON             EQU  H'0007'
DONE             EQU  H'0000'
FM0              EQU  H'0002'
GO_NOT_DONE      EQU  H'0000'
GO_NDONE         EQU  H'0000'
ADFM0            EQU  H'0002'


;----- ADCON2 Bits -----------------------------------------------------
ACLR             EQU  H'0003'
PSIS             EQU  H'0007'
ADMD0            EQU  H'0000'
ADMD1            EQU  H'0001'
ADMD2            EQU  H'0002'
ADACLR           EQU  H'0003'
ADCRS0           EQU  H'0004'
ADCRS1           EQU  H'0005'
ADCRS2           EQU  H'0006'
ADPSIS           EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
CRS0             EQU  H'0004'
CRS1             EQU  H'0005'
CRS2             EQU  H'0006'
MD0              EQU  H'0000'
MD1              EQU  H'0001'
MD2              EQU  H'0002'


;----- ADCON3 Bits -----------------------------------------------------
SOI              EQU  H'0003'
ADTMD0           EQU  H'0000'
ADTMD1           EQU  H'0001'
ADTMD2           EQU  H'0002'
ADSOI            EQU  H'0003'
ADCALC0          EQU  H'0004'
ADCALC1          EQU  H'0005'
ADCALC2          EQU  H'0006'
TMD0             EQU  H'0000'
TMD1             EQU  H'0001'
TMD2             EQU  H'0002'
CALC0            EQU  H'0004'
CALC1            EQU  H'0005'
CALC2            EQU  H'0006'


;----- ADSTAT Bits -----------------------------------------------------
MATH             EQU  H'0004'
LTHR             EQU  H'0005'
UTHR             EQU  H'0006'
OV_ADSTAT        EQU  H'0007'
ADSTAT0          EQU  H'0000'
ADSTAT1          EQU  H'0001'
ADSTAT2          EQU  H'0002'
ADMATH           EQU  H'0004'
ADLTHR           EQU  H'0005'
ADUTHR           EQU  H'0006'
ADAOV            EQU  H'0007'
ADOV             EQU  H'0007'
STAT0_ADSTAT     EQU  H'0000'
STAT1_ADSTAT     EQU  H'0001'
STAT2            EQU  H'0002'


;----- ADREF Bits -----------------------------------------------------
PREF0            EQU  H'0000'
PREF1            EQU  H'0001'
NREF0            EQU  H'0004'
ADPREF0          EQU  H'0000'
ADPREF1          EQU  H'0001'
ADNREF0          EQU  H'0004'


;----- ADACT Bits -----------------------------------------------------
ADACT0           EQU  H'0000'
ADACT1           EQU  H'0001'
ADACT2           EQU  H'0002'
ADACT3           EQU  H'0003'
ADACT4           EQU  H'0004'
ADACT5           EQU  H'0005'
ACT0             EQU  H'0000'
ACT1             EQU  H'0001'
ACT2             EQU  H'0002'
ACT3             EQU  H'0003'
ACT4             EQU  H'0004'
ACT5             EQU  H'0005'


;----- ADCLK Bits -----------------------------------------------------
ADCS0            EQU  H'0000'
ADCS1            EQU  H'0001'
ADCS2            EQU  H'0002'
ADCS3            EQU  H'0003'
ADCS4            EQU  H'0004'
ADCS5            EQU  H'0005'
CS0_ADCLK        EQU  H'0000'
CS1_ADCLK        EQU  H'0001'
CS2_ADCLK        EQU  H'0002'
CS3              EQU  H'0003'
CS4              EQU  H'0004'
CS5              EQU  H'0005'


;----- ADCTX Bits -----------------------------------------------------
CTXSW            EQU  H'0007'
CTX0             EQU  H'0000'
CTX1             EQU  H'0001'
ADCTXSW          EQU  H'0007'
ADCTX0           EQU  H'0000'
ADCTX1           EQU  H'0001'


;----- ADCSEL2 Bits -----------------------------------------------------
SSI              EQU  H'0006'
CHEN             EQU  H'0007'


;----- ADCSEL3 Bits -----------------------------------------------------
SSI              EQU  H'0006'
CHEN             EQU  H'0007'


;----- ADCSEL4 Bits -----------------------------------------------------
SSI              EQU  H'0006'
CHEN             EQU  H'0007'


;----- ANSELA Bits -----------------------------------------------------
ANSELA0          EQU  H'0000'
ANSELA1          EQU  H'0001'
ANSELA2          EQU  H'0002'
ANSELA3          EQU  H'0003'
ANSELA4          EQU  H'0004'
ANSELA5          EQU  H'0005'
ANSELA6          EQU  H'0006'
ANSELA7          EQU  H'0007'


;----- WPUA Bits -----------------------------------------------------
WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WPUA2            EQU  H'0002'
WPUA3            EQU  H'0003'
WPUA4            EQU  H'0004'
WPUA5            EQU  H'0005'
WPUA6            EQU  H'0006'
WPUA7            EQU  H'0007'


;----- ODCONA Bits -----------------------------------------------------
ODCA0            EQU  H'0000'
ODCA1            EQU  H'0001'
ODCA2            EQU  H'0002'
ODCA3            EQU  H'0003'
ODCA4            EQU  H'0004'
ODCA5            EQU  H'0005'
ODCA6            EQU  H'0006'
ODCA7            EQU  H'0007'


;----- SLRCONA Bits -----------------------------------------------------
SLRA0            EQU  H'0000'
SLRA1            EQU  H'0001'
SLRA2            EQU  H'0002'
SLRA3            EQU  H'0003'
SLRA4            EQU  H'0004'
SLRA5            EQU  H'0005'
SLRA6            EQU  H'0006'
SLRA7            EQU  H'0007'


;----- INLVLA Bits -----------------------------------------------------
INLVLA0          EQU  H'0000'
INLVLA1          EQU  H'0001'
INLVLA2          EQU  H'0002'
INLVLA3          EQU  H'0003'
INLVLA4          EQU  H'0004'
INLVLA5          EQU  H'0005'
INLVLA6          EQU  H'0006'
INLVLA7          EQU  H'0007'


;----- IOCAP Bits -----------------------------------------------------
IOCAP0           EQU  H'0000'
IOCAP1           EQU  H'0001'
IOCAP2           EQU  H'0002'
IOCAP3           EQU  H'0003'
IOCAP4           EQU  H'0004'
IOCAP5           EQU  H'0005'
IOCAP6           EQU  H'0006'
IOCAP7           EQU  H'0007'


;----- IOCAN Bits -----------------------------------------------------
IOCAN0           EQU  H'0000'
IOCAN1           EQU  H'0001'
IOCAN2           EQU  H'0002'
IOCAN3           EQU  H'0003'
IOCAN4           EQU  H'0004'
IOCAN5           EQU  H'0005'
IOCAN6           EQU  H'0006'
IOCAN7           EQU  H'0007'


;----- IOCAF Bits -----------------------------------------------------
IOCAF0           EQU  H'0000'
IOCAF1           EQU  H'0001'
IOCAF2           EQU  H'0002'
IOCAF3           EQU  H'0003'
IOCAF4           EQU  H'0004'
IOCAF5           EQU  H'0005'
IOCAF6           EQU  H'0006'
IOCAF7           EQU  H'0007'


;----- ANSELB Bits -----------------------------------------------------
ANSELB0          EQU  H'0000'
ANSELB1          EQU  H'0001'
ANSELB2          EQU  H'0002'
ANSELB3          EQU  H'0003'
ANSELB4          EQU  H'0004'
ANSELB5          EQU  H'0005'
ANSELB6          EQU  H'0006'
ANSELB7          EQU  H'0007'


;----- WPUB Bits -----------------------------------------------------
WPUB0            EQU  H'0000'
WPUB1            EQU  H'0001'
WPUB2            EQU  H'0002'
WPUB3            EQU  H'0003'
WPUB4            EQU  H'0004'
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'
WPUB7            EQU  H'0007'


;----- ODCONB Bits -----------------------------------------------------
ODCB0            EQU  H'0000'
ODCB1            EQU  H'0001'
ODCB2            EQU  H'0002'
ODCB3            EQU  H'0003'
ODCB4            EQU  H'0004'
ODCB5            EQU  H'0005'
ODCB6            EQU  H'0006'
ODCB7            EQU  H'0007'


;----- SLRCONB Bits -----------------------------------------------------
SLRB0            EQU  H'0000'
SLRB1            EQU  H'0001'
SLRB2            EQU  H'0002'
SLRB3            EQU  H'0003'
SLRB4            EQU  H'0004'
SLRB5            EQU  H'0005'
SLRB6            EQU  H'0006'
SLRB7            EQU  H'0007'


;----- INLVLB Bits -----------------------------------------------------
INLVLB0          EQU  H'0000'
INLVLB1          EQU  H'0001'
INLVLB2          EQU  H'0002'
INLVLB3          EQU  H'0003'
INLVLB4          EQU  H'0004'
INLVLB5          EQU  H'0005'
INLVLB6          EQU  H'0006'
INLVLB7          EQU  H'0007'


;----- IOCBP Bits -----------------------------------------------------
IOCBP0           EQU  H'0000'
IOCBP1           EQU  H'0001'
IOCBP2           EQU  H'0002'
IOCBP3           EQU  H'0003'
IOCBP4           EQU  H'0004'
IOCBP5           EQU  H'0005'
IOCBP6           EQU  H'0006'
IOCBP7           EQU  H'0007'


;----- IOCBN Bits -----------------------------------------------------
IOCBN0           EQU  H'0000'
IOCBN1           EQU  H'0001'
IOCBN2           EQU  H'0002'
IOCBN3           EQU  H'0003'
IOCBN4           EQU  H'0004'
IOCBN5           EQU  H'0005'
IOCBN6           EQU  H'0006'
IOCBN7           EQU  H'0007'


;----- IOCBF Bits -----------------------------------------------------
IOCBF0           EQU  H'0000'
IOCBF1           EQU  H'0001'
IOCBF2           EQU  H'0002'
IOCBF3           EQU  H'0003'
IOCBF4           EQU  H'0004'
IOCBF5           EQU  H'0005'
IOCBF6           EQU  H'0006'
IOCBF7           EQU  H'0007'


;----- ANSELC Bits -----------------------------------------------------
ANSELC0          EQU  H'0000'
ANSELC1          EQU  H'0001'
ANSELC2          EQU  H'0002'
ANSELC3          EQU  H'0003'
ANSELC4          EQU  H'0004'
ANSELC5          EQU  H'0005'
ANSELC6          EQU  H'0006'
ANSELC7          EQU  H'0007'


;----- WPUC Bits -----------------------------------------------------
WPUC0            EQU  H'0000'
WPUC1            EQU  H'0001'
WPUC2            EQU  H'0002'
WPUC3            EQU  H'0003'
WPUC4            EQU  H'0004'
WPUC5            EQU  H'0005'
WPUC6            EQU  H'0006'
WPUC7            EQU  H'0007'


;----- ODCONC Bits -----------------------------------------------------
ODCC0            EQU  H'0000'
ODCC1            EQU  H'0001'
ODCC2            EQU  H'0002'
ODCC3            EQU  H'0003'
ODCC4            EQU  H'0004'
ODCC5            EQU  H'0005'
ODCC6            EQU  H'0006'
ODCC7            EQU  H'0007'


;----- SLRCONC Bits -----------------------------------------------------
SLRC0            EQU  H'0000'
SLRC1            EQU  H'0001'
SLRC2            EQU  H'0002'
SLRC3            EQU  H'0003'
SLRC4            EQU  H'0004'
SLRC5            EQU  H'0005'
SLRC6            EQU  H'0006'
SLRC7            EQU  H'0007'


;----- INLVLC Bits -----------------------------------------------------
INLVLC0          EQU  H'0000'
INLVLC1          EQU  H'0001'
INLVLC2          EQU  H'0002'
INLVLC3          EQU  H'0003'
INLVLC4          EQU  H'0004'
INLVLC5          EQU  H'0005'
INLVLC6          EQU  H'0006'
INLVLC7          EQU  H'0007'


;----- IOCCP Bits -----------------------------------------------------
IOCCP0           EQU  H'0000'
IOCCP1           EQU  H'0001'
IOCCP2           EQU  H'0002'
IOCCP3           EQU  H'0003'
IOCCP4           EQU  H'0004'
IOCCP5           EQU  H'0005'
IOCCP6           EQU  H'0006'
IOCCP7           EQU  H'0007'


;----- IOCCN Bits -----------------------------------------------------
IOCCN0           EQU  H'0000'
IOCCN1           EQU  H'0001'
IOCCN2           EQU  H'0002'
IOCCN3           EQU  H'0003'
IOCCN4           EQU  H'0004'
IOCCN5           EQU  H'0005'
IOCCN6           EQU  H'0006'
IOCCN7           EQU  H'0007'


;----- IOCCF Bits -----------------------------------------------------
IOCCF0           EQU  H'0000'
IOCCF1           EQU  H'0001'
IOCCF2           EQU  H'0002'
IOCCF3           EQU  H'0003'
IOCCF4           EQU  H'0004'
IOCCF5           EQU  H'0005'
IOCCF6           EQU  H'0006'
IOCCF7           EQU  H'0007'


;----- WPUE Bits -----------------------------------------------------
WPUE3            EQU  H'0003'


;----- INLVLE Bits -----------------------------------------------------
INLVLE3          EQU  H'0003'


;----- IOCEP Bits -----------------------------------------------------
IOCEP3           EQU  H'0003'


;----- IOCEN Bits -----------------------------------------------------
IOCEN3           EQU  H'0003'


;----- IOCEF Bits -----------------------------------------------------
IOCEF3           EQU  H'0003'


;----- NCO2ACC Bits -----------------------------------------------------
NCO2ACC0         EQU  H'0000'
NCO2ACC1         EQU  H'0001'
NCO2ACC2         EQU  H'0002'
NCO2ACC3         EQU  H'0003'
NCO2ACC4         EQU  H'0004'
NCO2ACC5         EQU  H'0005'
NCO2ACC6         EQU  H'0006'
NCO2ACC7         EQU  H'0007'
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- NCO2ACCH Bits -----------------------------------------------------
NCO2ACC8         EQU  H'0000'
NCO2ACC9         EQU  H'0001'
NCO2ACC10        EQU  H'0002'
NCO2ACC11        EQU  H'0003'
NCO2ACC12        EQU  H'0004'
NCO2ACC13        EQU  H'0005'
NCO2ACC14        EQU  H'0006'
NCO2ACC15        EQU  H'0007'
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- NCO2ACCU Bits -----------------------------------------------------
NCO2ACC16        EQU  H'0000'
NCO2ACC17        EQU  H'0001'
NCO2ACC18        EQU  H'0002'
NCO2ACC19        EQU  H'0003'
NCO2ACC20        EQU  H'0004'
NCO2ACC21        EQU  H'0005'
NCO2ACC22        EQU  H'0006'
NCO2ACC23        EQU  H'0007'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
ACC20            EQU  H'0004'
ACC21            EQU  H'0005'
ACC22            EQU  H'0006'
ACC23            EQU  H'0007'


;----- NCO2INC Bits -----------------------------------------------------
NCO2INC0         EQU  H'0000'
NCO2INC1         EQU  H'0001'
NCO2INC2         EQU  H'0002'
NCO2INC3         EQU  H'0003'
NCO2INC4         EQU  H'0004'
NCO2INC5         EQU  H'0005'
NCO2INC6         EQU  H'0006'
NCO2INC7         EQU  H'0007'
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
INC4             EQU  H'0004'
INC5             EQU  H'0005'
INC6             EQU  H'0006'
INC7             EQU  H'0007'


;----- NCO2INCH Bits -----------------------------------------------------
NCO2INC8         EQU  H'0000'
NCO2INC9         EQU  H'0001'
NCO2INC10        EQU  H'0002'
NCO2INC11        EQU  H'0003'
NCO2INC12        EQU  H'0004'
NCO2INC13        EQU  H'0005'
NCO2INC14        EQU  H'0006'
NCO2INC15        EQU  H'0007'
INC8             EQU  H'0000'
INC9             EQU  H'0001'
INC10            EQU  H'0002'
INC11            EQU  H'0003'
INC12            EQU  H'0004'
INC13            EQU  H'0005'
INC14            EQU  H'0006'
INC15            EQU  H'0007'


;----- NCO2INCU Bits -----------------------------------------------------
NCO2INC16        EQU  H'0000'
NCO2INC17        EQU  H'0001'
NCO2INC18        EQU  H'0002'
NCO2INC19        EQU  H'0003'
NCO2INC20        EQU  H'0004'
NCO2INC21        EQU  H'0005'
NCO2INC22        EQU  H'0006'
NCO2INC23        EQU  H'0007'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
INC20            EQU  H'0004'
INC21            EQU  H'0005'
INC22            EQU  H'0006'
INC23            EQU  H'0007'


;----- NCO2CON Bits -----------------------------------------------------
PFM              EQU  H'0000'
POL_NCO2CON      EQU  H'0004'
OUT_NCO2CON      EQU  H'0005'
EN               EQU  H'0007'
NCO2PFM          EQU  H'0000'
NCO2POL          EQU  H'0004'
NCO2OUT          EQU  H'0005'
NCO2EN           EQU  H'0007'


;----- NCO2CLK Bits -----------------------------------------------------
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO2CKS0         EQU  H'0000'
NCO2CKS1         EQU  H'0001'
NCO2CKS2         EQU  H'0002'
NCO2CKS3         EQU  H'0003'
NCO2PWS0         EQU  H'0005'
NCO2PWS1         EQU  H'0006'
NCO2PWS2         EQU  H'0007'


;----- NCO3ACC Bits -----------------------------------------------------
NCO3ACC0         EQU  H'0000'
NCO3ACC1         EQU  H'0001'
NCO3ACC2         EQU  H'0002'
NCO3ACC3         EQU  H'0003'
NCO3ACC4         EQU  H'0004'
NCO3ACC5         EQU  H'0005'
NCO3ACC6         EQU  H'0006'
NCO3ACC7         EQU  H'0007'
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- NCO3ACCH Bits -----------------------------------------------------
NCO3ACC8         EQU  H'0000'
NCO3ACC9         EQU  H'0001'
NCO3ACC10        EQU  H'0002'
NCO3ACC11        EQU  H'0003'
NCO3ACC12        EQU  H'0004'
NCO3ACC13        EQU  H'0005'
NCO3ACC14        EQU  H'0006'
NCO3ACC15        EQU  H'0007'
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- NCO3ACCU Bits -----------------------------------------------------
NCO3ACC16        EQU  H'0000'
NCO3ACC17        EQU  H'0001'
NCO3ACC18        EQU  H'0002'
NCO3ACC19        EQU  H'0003'
NCO3ACC20        EQU  H'0004'
NCO3ACC21        EQU  H'0005'
NCO3ACC22        EQU  H'0006'
NCO3ACC23        EQU  H'0007'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
ACC20            EQU  H'0004'
ACC21            EQU  H'0005'
ACC22            EQU  H'0006'
ACC23            EQU  H'0007'


;----- NCO3INC Bits -----------------------------------------------------
NCO3INC0         EQU  H'0000'
NCO3INC1         EQU  H'0001'
NCO3INC2         EQU  H'0002'
NCO3INC3         EQU  H'0003'
NCO3INC4         EQU  H'0004'
NCO3INC5         EQU  H'0005'
NCO3INC6         EQU  H'0006'
NCO3INC7         EQU  H'0007'
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
INC4             EQU  H'0004'
INC5             EQU  H'0005'
INC6             EQU  H'0006'
INC7             EQU  H'0007'


;----- NCO3INCH Bits -----------------------------------------------------
NCO3INC8         EQU  H'0000'
NCO3INC9         EQU  H'0001'
NCO3INC10        EQU  H'0002'
NCO3INC11        EQU  H'0003'
NCO3INC12        EQU  H'0004'
NCO3INC13        EQU  H'0005'
NCO3INC14        EQU  H'0006'
NCO3INC15        EQU  H'0007'
INC8             EQU  H'0000'
INC9             EQU  H'0001'
INC10            EQU  H'0002'
INC11            EQU  H'0003'
INC12            EQU  H'0004'
INC13            EQU  H'0005'
INC14            EQU  H'0006'
INC15            EQU  H'0007'


;----- NCO3INCU Bits -----------------------------------------------------
NCO3INC16        EQU  H'0000'
NCO3INC17        EQU  H'0001'
NCO3INC18        EQU  H'0002'
NCO3INC19        EQU  H'0003'
NCO3INC20        EQU  H'0004'
NCO3INC21        EQU  H'0005'
NCO3INC22        EQU  H'0006'
NCO3INC23        EQU  H'0007'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
INC20            EQU  H'0004'
INC21            EQU  H'0005'
INC22            EQU  H'0006'
INC23            EQU  H'0007'


;----- NCO3CON Bits -----------------------------------------------------
PFM              EQU  H'0000'
POL_NCO3CON      EQU  H'0004'
OUT_NCO3CON      EQU  H'0005'
EN               EQU  H'0007'
NCO3PFM          EQU  H'0000'
NCO3POL          EQU  H'0004'
NCO3OUT          EQU  H'0005'
NCO3EN           EQU  H'0007'


;----- NCO3CLK Bits -----------------------------------------------------
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO3CKS0         EQU  H'0000'
NCO3CKS1         EQU  H'0001'
NCO3CKS2         EQU  H'0002'
NCO3CKS3         EQU  H'0003'
NCO3PWS0         EQU  H'0005'
NCO3PWS1         EQU  H'0006'
NCO3PWS2         EQU  H'0007'


;----- FSCMCON Bits -----------------------------------------------------
FSCMFEV          EQU  H'0000'
FSCMFFI          EQU  H'0001'
FSCMPEV          EQU  H'0002'
FSCMPFI          EQU  H'0003'
FSCMSEV          EQU  H'0004'
FSCMSFI          EQU  H'0005'


;----- IVTLOCK Bits -----------------------------------------------------
IVTLOCKED        EQU  H'0000'


;----- IVTAD Bits -----------------------------------------------------
AD0              EQU  H'0000'
AD1              EQU  H'0001'
AD2              EQU  H'0002'
AD3              EQU  H'0003'
AD4              EQU  H'0004'
AD5              EQU  H'0005'
AD6              EQU  H'0006'
AD7              EQU  H'0007'


;----- IVTADH Bits -----------------------------------------------------
AD8              EQU  H'0000'
AD9              EQU  H'0001'
AD10             EQU  H'0002'
AD11             EQU  H'0003'
AD12             EQU  H'0004'
AD13             EQU  H'0005'
AD14             EQU  H'0006'
AD15             EQU  H'0007'


;----- IVTADU Bits -----------------------------------------------------
AD16             EQU  H'0000'
AD17             EQU  H'0001'
AD18             EQU  H'0002'
AD19             EQU  H'0003'
AD20             EQU  H'0004'


;----- IVTBASE Bits -----------------------------------------------------
BASE0            EQU  H'0000'
BASE1            EQU  H'0001'
BASE2            EQU  H'0002'
BASE3            EQU  H'0003'
BASE4            EQU  H'0004'
BASE5            EQU  H'0005'
BASE6            EQU  H'0006'
BASE7            EQU  H'0007'


;----- IVTBASEH Bits -----------------------------------------------------
BASE8            EQU  H'0000'
BASE9            EQU  H'0001'
BASE10           EQU  H'0002'
BASE11           EQU  H'0003'
BASE12           EQU  H'0004'
BASE13           EQU  H'0005'
BASE14           EQU  H'0006'
BASE15           EQU  H'0007'


;----- IVTBASEU Bits -----------------------------------------------------
BASE16           EQU  H'0000'
BASE17           EQU  H'0001'
BASE18           EQU  H'0002'
BASE19           EQU  H'0003'
BASE20           EQU  H'0004'


;----- PWM2GIR Bits -----------------------------------------------------
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM2GIE Bits -----------------------------------------------------
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM2CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD_PWM2CON       EQU  H'0002'
EN               EQU  H'0007'


;----- PWM2S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWM3GIR Bits -----------------------------------------------------
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM3GIE Bits -----------------------------------------------------
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM3CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD_PWM3CON       EQU  H'0002'
EN               EQU  H'0007'


;----- PWM3S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWM4GIR Bits -----------------------------------------------------
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM4GIE Bits -----------------------------------------------------
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM4CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD_PWM4CON       EQU  H'0002'
EN               EQU  H'0007'


;----- PWM4S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWMLOAD Bits -----------------------------------------------------
PWM1LD           EQU  H'0000'
PWM2LD           EQU  H'0001'
PWM3LD           EQU  H'0002'
PWM4LD           EQU  H'0003'


;----- PWMEN Bits -----------------------------------------------------
PWM1EN           EQU  H'0000'
PWM2EN           EQU  H'0001'
PWM3EN           EQU  H'0002'
PWM4EN           EQU  H'0003'


;----- PIE0 Bits -----------------------------------------------------
SWIE             EQU  H'0000'
HLVDIE           EQU  H'0001'
OSFIE            EQU  H'0002'
CSWIE            EQU  H'0003'
TU16AIE          EQU  H'0004'
CLC1IE           EQU  H'0005'
CANIE            EQU  H'0006'
IOCIE            EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
ADTIE            EQU  H'0000'
ADCH2IE          EQU  H'0001'
ADCH3IE          EQU  H'0002'
ADCH4IE          EQU  H'0003'
DMA1SCNTIE       EQU  H'0004'
DMA1DCNTIE       EQU  H'0005'
DMA1ORIE         EQU  H'0006'
DMA1AIE          EQU  H'0007'
ADCH1IE          EQU  H'0000'


;----- PIE3 Bits -----------------------------------------------------
SPI1RXIE         EQU  H'0000'
SPI1TXIE         EQU  H'0001'
SPI1IE           EQU  H'0002'
TMR2IE           EQU  H'0003'
TMR1IE           EQU  H'0004'
TMR1GIE          EQU  H'0005'
CCP1IE           EQU  H'0006'
TMR0IE           EQU  H'0007'


;----- PIE4 Bits -----------------------------------------------------
U1RXIE           EQU  H'0000'
U1TXIE           EQU  H'0001'
U1EIE            EQU  H'0002'
U1IE             EQU  H'0003'
CANRXIE          EQU  H'0004'
CANTXIE          EQU  H'0005'
PWM1PIE          EQU  H'0006'
PWM1IE           EQU  H'0007'


;----- PIE5 Bits -----------------------------------------------------
SPI2RXIE         EQU  H'0000'
SPI2TXIE         EQU  H'0001'
SPI2IE           EQU  H'0002'
TU16BIE          EQU  H'0003'
TMR3IE           EQU  H'0004'
TMR3GIE          EQU  H'0005'
PWM2PIE          EQU  H'0006'
PWM2IE           EQU  H'0007'


;----- PIE6 Bits -----------------------------------------------------
INT1IE           EQU  H'0000'
CLC2IE           EQU  H'0001'
CWG1IE           EQU  H'0002'
NCO1IE           EQU  H'0003'
DMA2SCNTIE       EQU  H'0004'
DMA2DCNTIE       EQU  H'0005'
DMA2ORIE         EQU  H'0006'
DMA2AIE          EQU  H'0007'


;----- PIE7 Bits -----------------------------------------------------
I2C1RXIE         EQU  H'0000'
I2C1TXIE         EQU  H'0001'
I2C1IE           EQU  H'0002'
I2C1EIE          EQU  H'0003'
CLC3IE           EQU  H'0005'
PWM3PIE          EQU  H'0006'
PWM3IE           EQU  H'0007'


;----- PIE8 Bits -----------------------------------------------------
U2RXIE           EQU  H'0000'
U2TXIE           EQU  H'0001'
U2EIE            EQU  H'0002'
U2IE             EQU  H'0003'
TMR5IE           EQU  H'0004'
TMR5GIE          EQU  H'0005'
CCP2IE           EQU  H'0006'
SCANIE           EQU  H'0007'


;----- PIE9 Bits -----------------------------------------------------
U3RXIE           EQU  H'0000'
U3TXIE           EQU  H'0001'
U3EIE            EQU  H'0002'
U3IE             EQU  H'0003'
CLC4IE           EQU  H'0005'
PWM4PIE          EQU  H'0006'
PWM4IE           EQU  H'0007'


;----- PIE10 Bits -----------------------------------------------------
INT2IE           EQU  H'0000'
CLC5IE           EQU  H'0001'
CWG2IE           EQU  H'0002'
NCO2IE           EQU  H'0003'
DMA3SCNTIE       EQU  H'0004'
DMA3DCNTIE       EQU  H'0005'
DMA3ORIE         EQU  H'0006'
DMA3AIE          EQU  H'0007'


;----- PIE12 Bits -----------------------------------------------------
U4RXIE           EQU  H'0000'
U4TXIE           EQU  H'0001'
U4EIE            EQU  H'0002'
U4IE             EQU  H'0003'
DMA5SCNTIE       EQU  H'0004'
DMA5DCNTIE       EQU  H'0005'
DMA5ORIE         EQU  H'0006'
DMA5AIE          EQU  H'0007'


;----- PIE13 Bits -----------------------------------------------------
U5RXIE           EQU  H'0000'
U5TXIE           EQU  H'0001'
U5EIE            EQU  H'0002'
U5IE             EQU  H'0003'
DMA6SCNTIE       EQU  H'0004'
DMA6DCNTIE       EQU  H'0005'
DMA6ORIE         EQU  H'0006'
DMA6AIE          EQU  H'0007'


;----- PIE14 Bits -----------------------------------------------------
CLC7IE           EQU  H'0001'
C2IE             EQU  H'0002'
NCO3IE           EQU  H'0003'
DMA7SCNTIE       EQU  H'0004'
DMA7DCNTIE       EQU  H'0005'
DMA7ORIE         EQU  H'0006'
DMA7AIE          EQU  H'0007'


;----- PIE15 Bits -----------------------------------------------------
NVMIE            EQU  H'0000'
CLC8IE           EQU  H'0001'
CRCIE            EQU  H'0002'
TMR6IE           EQU  H'0003'
DMA8SCNTIE       EQU  H'0004'
DMA8DCNTIE       EQU  H'0005'
DMA8ORIE         EQU  H'0006'
DMA8AIE          EQU  H'0007'


;----- PIR0 Bits -----------------------------------------------------
SWIF             EQU  H'0000'
HLVDIF           EQU  H'0001'
OSFIF            EQU  H'0002'
CSWIF            EQU  H'0003'
TU16AIF          EQU  H'0004'
CLC1IF           EQU  H'0005'
CANIF            EQU  H'0006'
IOCIF            EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
ADTIF            EQU  H'0000'
ADCH2IF          EQU  H'0001'
ADCH3IF          EQU  H'0002'
ADCH4IF          EQU  H'0003'
DMA1SCNTIF       EQU  H'0004'
DMA1DCNTIF       EQU  H'0005'
DMA1ORIF         EQU  H'0006'
DMA1AIF          EQU  H'0007'
ADCH1IF          EQU  H'0000'


;----- PIR3 Bits -----------------------------------------------------
SPI1RXIF         EQU  H'0000'
SPI1TXIF         EQU  H'0001'
SPI1IF           EQU  H'0002'
TMR2IF           EQU  H'0003'
TMR1IF           EQU  H'0004'
TMR1GIF          EQU  H'0005'
CCP1IF           EQU  H'0006'
TMR0IF           EQU  H'0007'


;----- PIR4 Bits -----------------------------------------------------
U1RXIF           EQU  H'0000'
U1TXIF           EQU  H'0001'
U1EIF            EQU  H'0002'
U1IF             EQU  H'0003'
CANRXIF          EQU  H'0004'
CANTXIF          EQU  H'0005'
PWM1PIF          EQU  H'0006'
PWM1IF           EQU  H'0007'


;----- PIR5 Bits -----------------------------------------------------
SPI2RXIF         EQU  H'0000'
SPI2TXIF         EQU  H'0001'
SPI2IF           EQU  H'0002'
TU16BIF          EQU  H'0003'
TMR3IF           EQU  H'0004'
TMR3GIF          EQU  H'0005'
PWM2PIF          EQU  H'0006'
PWM2IF           EQU  H'0007'


;----- PIR6 Bits -----------------------------------------------------
INT1IF           EQU  H'0000'
CLC2IF           EQU  H'0001'
CWG1IF           EQU  H'0002'
NCO1IF           EQU  H'0003'
DMA2SCNTIF       EQU  H'0004'
DMA2DCNTIF       EQU  H'0005'
DMA2ORIF         EQU  H'0006'
DMA2AIF          EQU  H'0007'


;----- PIR7 Bits -----------------------------------------------------
I2C1RXIF         EQU  H'0000'
I2C1TXIF         EQU  H'0001'
I2C1IF           EQU  H'0002'
I2C1EIF          EQU  H'0003'
CLC3IF           EQU  H'0005'
PWM3PIF          EQU  H'0006'
PWM3IF           EQU  H'0007'


;----- PIR8 Bits -----------------------------------------------------
U2RXIF           EQU  H'0000'
U2TXIF           EQU  H'0001'
U2EIF            EQU  H'0002'
U2IF             EQU  H'0003'
TMR5IF           EQU  H'0004'
TMR5GIF          EQU  H'0005'
CCP2IF           EQU  H'0006'
SCANIF           EQU  H'0007'


;----- PIR9 Bits -----------------------------------------------------
U3RXIF           EQU  H'0000'
U3TXIF           EQU  H'0001'
U3EIF            EQU  H'0002'
U3IF             EQU  H'0003'
CLC4IF           EQU  H'0005'
PWM4PIF          EQU  H'0006'
PWM4IF           EQU  H'0007'


;----- PIR10 Bits -----------------------------------------------------
INT2IF           EQU  H'0000'
CLC5IF           EQU  H'0001'
CWG2IF           EQU  H'0002'
NCO2IF           EQU  H'0003'
DMA3SCNTIF       EQU  H'0004'
DMA3DCNTIF       EQU  H'0005'
DMA3ORIF         EQU  H'0006'
DMA3AIF          EQU  H'0007'


;----- PIR12 Bits -----------------------------------------------------
U4RXIF           EQU  H'0000'
U4TXIF           EQU  H'0001'
U4EIF            EQU  H'0002'
U4IF             EQU  H'0003'
DMA5SCNTIF       EQU  H'0004'
DMA5DCNTIF       EQU  H'0005'
DMA5ORIF         EQU  H'0006'
DMA5AIF          EQU  H'0007'


;----- PIR13 Bits -----------------------------------------------------
U5RXIF           EQU  H'0000'
U5TXIF           EQU  H'0001'
U5EIF            EQU  H'0002'
U5IF             EQU  H'0003'
DMA6SCNTIF       EQU  H'0004'
DMA6DCNTIF       EQU  H'0005'
DMA6ORIF         EQU  H'0006'
DMA6AIF          EQU  H'0007'


;----- PIR14 Bits -----------------------------------------------------
CLC7IF           EQU  H'0001'
C2IF             EQU  H'0002'
NCO3IF           EQU  H'0003'
DMA7SCNTIF       EQU  H'0004'
DMA7DCNTIF       EQU  H'0005'
DMA7ORIF         EQU  H'0006'
DMA7AIF          EQU  H'0007'


;----- PIR15 Bits -----------------------------------------------------
NVMIF            EQU  H'0000'
CLC8IF           EQU  H'0001'
CRCIF            EQU  H'0002'
TMR6IF           EQU  H'0003'
DMA8SCNTIF       EQU  H'0004'
DMA8DCNTIF       EQU  H'0005'
DMA8ORIF         EQU  H'0006'
DMA8AIF          EQU  H'0007'


;----- LATA Bits -----------------------------------------------------
LATA0            EQU  H'0000'
LATA1            EQU  H'0001'
LATA2            EQU  H'0002'
LATA3            EQU  H'0003'
LATA4            EQU  H'0004'
LATA5            EQU  H'0005'
LATA6            EQU  H'0006'
LATA7            EQU  H'0007'


;----- LATB Bits -----------------------------------------------------
LATB0            EQU  H'0000'
LATB1            EQU  H'0001'
LATB2            EQU  H'0002'
LATB3            EQU  H'0003'
LATB4            EQU  H'0004'
LATB5            EQU  H'0005'
LATB6            EQU  H'0006'
LATB7            EQU  H'0007'


;----- LATC Bits -----------------------------------------------------
LATC0            EQU  H'0000'
LATC1            EQU  H'0001'
LATC2            EQU  H'0002'
LATC3            EQU  H'0003'
LATC4            EQU  H'0004'
LATC5            EQU  H'0005'
LATC6            EQU  H'0006'
LATC7            EQU  H'0007'


;----- TRISA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA3           EQU  H'0003'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'
TRISA6           EQU  H'0006'
TRISA7           EQU  H'0007'


;----- TRISB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB2           EQU  H'0002'
TRISB3           EQU  H'0003'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'
TRISB7           EQU  H'0007'


;----- TRISC Bits -----------------------------------------------------
TRISC0           EQU  H'0000'
TRISC1           EQU  H'0001'
TRISC2           EQU  H'0002'
TRISC3           EQU  H'0003'
TRISC4           EQU  H'0004'
TRISC5           EQU  H'0005'
TRISC6           EQU  H'0006'
TRISC7           EQU  H'0007'


;----- TRISE Bits -----------------------------------------------------
TRISE3           EQU  H'0003'


;----- PORTA Bits -----------------------------------------------------
RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'
RA4              EQU  H'0004'
RA5              EQU  H'0005'
RA6              EQU  H'0006'
RA7              EQU  H'0007'


;----- PORTB Bits -----------------------------------------------------
RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB3              EQU  H'0003'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PORTC Bits -----------------------------------------------------
RC0              EQU  H'0000'
RC1              EQU  H'0001'
RC2              EQU  H'0002'
RC3              EQU  H'0003'
RC4              EQU  H'0004'
RC5              EQU  H'0005'
RC6              EQU  H'0006'
RC7              EQU  H'0007'


;----- PORTE Bits -----------------------------------------------------
RE3              EQU  H'0003'


;----- INTCON0 Bits -----------------------------------------------------
INT0EDG          EQU  H'0000'
INT1EDG          EQU  H'0001'
INT2EDG          EQU  H'0002'
IPEN_INTCON0     EQU  H'0005'
GIEL             EQU  H'0006'
GIE              EQU  H'0007'
GIEH             EQU  H'0007'


;----- INTCON1 Bits -----------------------------------------------------
STAT0_INTCON1    EQU  H'0006'
STAT1_INTCON1    EQU  H'0007'


;----- STATUS Bits -----------------------------------------------------
NOT_PD           EQU  H'0005'
NOT_TO           EQU  H'0006'
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
OV_STATUS        EQU  H'0003'
N                EQU  H'0004'
NPD              EQU  H'0005'
NTO              EQU  H'0006'
PD               EQU  H'0005'
TO               EQU  H'0006'


;----- PCON0 Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'
NOT_RI           EQU  H'0002'
NOT_RMCLR        EQU  H'0003'
NOT_RWDT         EQU  H'0004'
NOT_WDTWV        EQU  H'0005'
NBOR             EQU  H'0000'
NPOR             EQU  H'0001'
NRI              EQU  H'0002'
NRMCLR           EQU  H'0003'
NRWDT            EQU  H'0004'
NWDTWV           EQU  H'0005'
STKUNF           EQU  H'0006'
STKOVF           EQU  H'0007'
BOR              EQU  H'0000'
POR              EQU  H'0001'
RI               EQU  H'0002'
RMCLR            EQU  H'0003'
RWDT             EQU  H'0004'
WDTWV            EQU  H'0005'


;----- PCON1 Bits -----------------------------------------------------
NOT_MEMV         EQU  H'0001'
NMEMV            EQU  H'0001'
MEMV             EQU  H'0001'


;----- CPUDOZE Bits -----------------------------------------------------
DOE              EQU  H'0004'
ROI              EQU  H'0005'
DOZEN            EQU  H'0006'
IDLEN            EQU  H'0007'
DOZE0            EQU  H'0000'
DOZE1            EQU  H'0001'
DOZE2            EQU  H'0002'


;----- STKPTR Bits -----------------------------------------------------
STKPTR0          EQU  H'0000'
STKPTR1          EQU  H'0001'
STKPTR2          EQU  H'0002'
STKPTR3          EQU  H'0003'
STKPTR4          EQU  H'0004'
STKPTR5          EQU  H'0005'
SP0              EQU  H'0000'
SP1              EQU  H'0001'
SP2              EQU  H'0002'
SP3              EQU  H'0003'
SP4              EQU  H'0004'
SP5              EQU  H'0005'

;==========================================================================
;
;       RAM Definitions
;
;==========================================================================

       __MAXRAM  H'24FF'
       __BADRAM  H'0000'-H'0037'
       __BADRAM  H'003B'-H'003F'
       __BADRAM  H'004E'-H'0060'
       __BADRAM  H'0069'
       __BADRAM  H'009A'-H'00AB'
       __BADRAM  H'00C0'-H'00D3'
       __BADRAM  H'00E0'-H'00E7'
       __BADRAM  H'010F'
       __BADRAM  H'013A'
       __BADRAM  H'013B'
       __BADRAM  H'0219'-H'023C'
       __BADRAM  H'0252'
       __BADRAM  H'027C'-H'0285'
       __BADRAM  H'02A0'
       __BADRAM  H'02C8'
       __BADRAM  H'02CA'
       __BADRAM  H'02EB'
       __BADRAM  H'02EE'
       __BADRAM  H'02EE'
       __BADRAM  H'02F0'
       __BADRAM  H'0312'-H'0317'
       __BADRAM  H'034E'-H'034F'
       __BADRAM  H'0372'
       __BADRAM  H'0384'-H'0386'
       __BADRAM  H'0391'-H'0392'
       __BADRAM  H'039E'-H'03BB'
       __BADRAM  H'03EE'
       __BADRAM  H'0418'-H'041F'
       __BADRAM  H'0420'
       __BADRAM  H'0422'
       __BADRAM  H'0423'
       __BADRAM  H'0428'-H'043F'
       __BADRAM  H'04C1'-H'04C5'
       __BADRAM  H'04C9'
       __BADRAM  H'04CB'-H'04CD'
       __BADRAM  H'04E1'
       __BADRAM  H'04E3'-H'04E5'


;==========================================================================
; The following is an assignment of address values for all of the
; configuration registers for the purpose of table reads
_CONFIG1      EQU  H'300000'
_CONFIG2      EQU  H'300001'
_CONFIG3      EQU  H'300002'
_CONFIG4      EQU  H'300003'
_CONFIG5      EQU  H'300004'
_CONFIG6      EQU  H'300005'
_CONFIG7      EQU  H'300006'
_CONFIG8      EQU  H'300007'
_CONFIG9      EQU  H'300008'
_CONFIG10     EQU  H'300009'
_CONFIG11     EQU  H'30000A'
_CONFIG12     EQU  H'30000B'
_CONFIG13     EQU  H'30000C'
_CONFIG14     EQU  H'30000D'
_CONFIG15     EQU  H'30000E'
_CONFIG16     EQU  H'30000F'
_CONFIG17     EQU  H'300010'
_CONFIG18     EQU  H'300011'
_CONFIG19     EQU  H'300012'
_CONFIG20     EQU  H'300013'
_CONFIG21     EQU  H'300014'
_CONFIG22     EQU  H'300015'
_CONFIG23     EQU  H'300016'
_CONFIG24     EQU  H'300017'
_CONFIG25     EQU  H'300018'
_CONFIG26     EQU  H'300019'
_CONFIG27     EQU  H'30001A'
_CONFIG28     EQU  H'30001B'
_CONFIG29     EQU  H'30001C'
_CONFIG30     EQU  H'30001D'
_CONFIG31     EQU  H'30001E'
_CONFIG32     EQU  H'30001F'
_CONFIG33     EQU  H'300020'
_CONFIG34     EQU  H'300021'
_CONFIG35     EQU  H'300022'

;----- CONFIG1 Options --------------------------------------------------
_FEXTOSC_LP_1        EQU  H'F8'; LP (crystal oscillator) optimized for 32.768 kHz
_FEXTOSC_XT_1        EQU  H'F9'; XT (crystal oscillator) above 100 kHz, below 8 MHz
_FEXTOSC_HS_1        EQU  H'FA'; HS (crystal oscillator) above 8 MHz
_FEXTOSC_OFF_1       EQU  H'FC'; Oscillator not enabled
_FEXTOSC_ECL_1       EQU  H'FD'; EC (external clock) below 100 kHz
_FEXTOSC_ECM_1       EQU  H'FE'; EC (external clock) for 500 kHz to 8 MHz
_FEXTOSC_ECH_1       EQU  H'FF'; EC (external clock) above 8 MHz
_RSTOSC_HFINTOSC_64MHZ_1 EQU  H'8F'; HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1
_RSTOSC_EXTOSC_4PLL_1 EQU  H'AF'; EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
_RSTOSC_SOSC_1       EQU  H'CF'; Secondary Oscillator
_RSTOSC_LFINTOSC_1   EQU  H'DF'; Low-Frequency Oscillator
_RSTOSC_HFINTOSC_1MHZ_1 EQU  H'EF'; HFINTOSC with HFFRQ = 4 MHz and CDIV = 4:1
_RSTOSC_EXTOSC_1     EQU  H'FF'; EXTOSC operating per FEXTOSC bits (device manufacturing default)

;----- CONFIG2 Options --------------------------------------------------
_CLKOUTEN_OFF_2      EQU  H'FF'; CLKOUT function is disabled
_CLKOUTEN_ON_2       EQU  H'FE'; CLKOUT function is enabled
_PR1WAY_ON_2         EQU  H'FF'; PRLOCKED bit can be cleared and set only once
_PR1WAY_OFF_2        EQU  H'FD'; PRLOCKED bit can be set and cleared repeatedly
_CSWEN_OFF_2         EQU  H'F7'; The NOSC and NDIV bits cannot be changed by user software
_CSWEN_ON_2          EQU  H'FF'; Writing to NOSC and NDIV is allowed
_JTAGEN_ON_2         EQU  H'FF'; Enable JTAG Boundary Scan mode and pins
_JTAGEN_OFF_2        EQU  H'EF'; Disable JTAG Boundary Scan mode, JTAG pins revert to user functions
_FCMEN_OFF_2         EQU  H'DF'; Fail-Safe Clock Monitor disabled
_FCMEN_ON_2          EQU  H'FF'; Fail-Safe Clock Monitor enabled
_FCMENP_OFF_2        EQU  H'BF'; FSCM timer will not set FSCMP bit or OSFIF interrupt on Primary XTAL failure
_FCMENP_ON_2         EQU  H'FF'; FSCM timer will set FSCMP bit and OSFIF interrupt on Primary XTAL failure
_FCMENS_OFF_2        EQU  H'7F'; FSCM timer will not set FSCMS bit or OSFIF interrupt on Secondary XTAL failure
_FCMENS_ON_2         EQU  H'FF'; FSCM timer will set FSCMS bit and OSFIF interrupt on Secondary XTAL failure

;----- CONFIG3 Options --------------------------------------------------
_MCLRE_EXTMCLR_3     EQU  H'FF'; If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR
_MCLRE_INTMCLR_3     EQU  H'FE'; If LVP = 0, MCLR pin function is port defined function; If LVP =1, RE3 pin fuction is MCLR
_PWRTS_PWRT_OFF_3    EQU  H'FF'; PWRT is disabled
_PWRTS_PWRT_64_3     EQU  H'FD'; PWRT set at 64ms
_PWRTS_PWRT_16_3     EQU  H'FB'; PWRT set at 16ms
_PWRTS_PWRT_1_3      EQU  H'F9'; PWRT set at 1ms
_MVECEN_ON_3         EQU  H'FF'; Multi-vector enabled, Vector table used for interrupts
_MVECEN_OFF_3        EQU  H'F7'; Interrupt contoller does not use vector table to prioritze interrupts
_IVT1WAY_ON_3        EQU  H'FF'; IVTLOCKED bit can be cleared and set only once
_IVT1WAY_OFF_3       EQU  H'EF'; IVTLOCKED bit can be cleared and set repeatedly
_LPBOREN_OFF_3       EQU  H'FF'; Low-Power BOR disabled
_LPBOREN_ON_3        EQU  H'DF'; Low-Power BOR enabled
_BOREN_SBORDIS_3     EQU  H'FF'; Brown-out Reset enabled , SBOREN bit is ignored
_BOREN_NOSLP_3       EQU  H'BF'; Brown-out Reset enabled while running, disabled in Sleep; SBOREN is ignored
_BOREN_ON_3          EQU  H'7F'; Brown-out Reset enabled according to SBOREN
_BOREN_OFF_3         EQU  H'3F'; Brown-out Reset disabled

;----- CONFIG4 Options --------------------------------------------------
_BORV_VBOR_1P9_4     EQU  H'FF'; Brown-out Reset Voltage (VBOR) set to 1.9V
_BORV_VBOR_2P45_4    EQU  H'FE'; Brown-out Reset Voltage (VBOR) set to 2.45V
_BORV_VBOR_2P7_4     EQU  H'FD'; Brown-out Reset Voltage (VBOR) set to 2.7V
_BORV_VBOR_2P85_4    EQU  H'FC'; Brown-out Reset Voltage (VBOR) set to 2.8V
_ZCD_OFF_4           EQU  H'FF'; ZCD module is disabled. ZCD can be enabled by setting the ZCDSEN bit of ZCDCON
_ZCD_ON_4            EQU  H'FB'; ZCD module is always enabled; ZCDMD and ZCDSEN bits are ignored
_PPS1WAY_ON_4        EQU  H'FF'; PPSLOCKED bit can be cleared and set only once; PPS registers remain locked after one clear/set cycle
_PPS1WAY_OFF_4       EQU  H'F7'; PPSLOCKED bit can be set and cleared repeatedly (subject to the unlock sequence)
_STVREN_ON_4         EQU  H'FF'; Stack full/underflow will cause Reset
_STVREN_OFF_4        EQU  H'EF'; Stack full/underflow will not cause Reset
_LVP_ON_4            EQU  H'FF'; Low voltage programming enabled. MCLR/VPP pin function is MCLR. MCLRE configuration bit is ignored
_LVP_OFF_4           EQU  H'DF'; HV on MCLR/VPP must be used for programming
_XINST_OFF_4         EQU  H'FF'; Extended Instruction Set and Indexed Addressing Mode disabled
_XINST_ON_4          EQU  H'7F'; Extended Instruction Set and Indexed Addressing Mode enabled

;----- CONFIG5 Options --------------------------------------------------
_WDTCPS_WDTCPS_0_5   EQU  H'E0'; Divider ratio 1:32
_WDTCPS_WDTCPS_1_5   EQU  H'E1'; Divider ratio 1:64
_WDTCPS_WDTCPS_2_5   EQU  H'E2'; Divider ratio 1:128
_WDTCPS_WDTCPS_3_5   EQU  H'E3'; Divider ratio 1:256
_WDTCPS_WDTCPS_4_5   EQU  H'E4'; Divider ratio 1:512
_WDTCPS_WDTCPS_5_5   EQU  H'E5'; Divider ratio 1:1024
_WDTCPS_WDTCPS_6_5   EQU  H'E6'; Divider ratio 1:2048
_WDTCPS_WDTCPS_7_5   EQU  H'E7'; Divider ratio 1:4096
_WDTCPS_WDTCPS_8_5   EQU  H'E8'; Divider ratio 1:8192
_WDTCPS_WDTCPS_9_5   EQU  H'E9'; Divider ratio 1:16384
_WDTCPS_WDTCPS_10_5  EQU  H'EA'; Divider ratio 1:32768
_WDTCPS_WDTCPS_11_5  EQU  H'EB'; Divider ratio 1:65536
_WDTCPS_WDTCPS_12_5  EQU  H'EC'; Divider ratio 1:131072
_WDTCPS_WDTCPS_13_5  EQU  H'ED'; Divider ratio 1:262144
_WDTCPS_WDTCPS_14_5  EQU  H'EE'; Divider ratio 1:524299
_WDTCPS_WDTCPS_15_5  EQU  H'EF'; Divider ratio 1:1048576
_WDTCPS_WDTCPS_16_5  EQU  H'F0'; Divider ratio 1:2097152
_WDTCPS_WDTCPS_17_5  EQU  H'F1'; Divider ratio 1:4194304
_WDTCPS_WDTCPS_18_5  EQU  H'F2'; Divider ratio 1:8388608
_WDTCPS_WDTCPS_19_5  EQU  H'F3'; Divider ratio 1:32
_WDTCPS_WDTCPS_20_5  EQU  H'F4'; Divider ratio 1:32
_WDTCPS_WDTCPS_21_5  EQU  H'F5'; Divider ratio 1:32
_WDTCPS_WDTCPS_22_5  EQU  H'F6'; Divider ratio 1:32
_WDTCPS_WDTCPS_23_5  EQU  H'F7'; Divider ratio 1:32
_WDTCPS_WDTCPS_24_5  EQU  H'F8'; Divider ratio 1:32
_WDTCPS_WDTCPS_25_5  EQU  H'F9'; Divider ratio 1:32
_WDTCPS_WDTCPS_26_5  EQU  H'FA'; Divider ratio 1:32
_WDTCPS_WDTCPS_27_5  EQU  H'FB'; Divider ratio 1:32
_WDTCPS_WDTCPS_28_5  EQU  H'FC'; Divider ratio 1:32
_WDTCPS_WDTCPS_29_5  EQU  H'FD'; Divider ratio 1:32
_WDTCPS_WDTCPS_30_5  EQU  H'FE'; Divider ratio 1:32
_WDTCPS_WDTCPS_31_5  EQU  H'FF'; Divider ratio 1:65536; software control of WDTPS
_WDTE_OFF_5          EQU  H'9F'; WDT Disabled; SWDTEN is ignored
_WDTE_SWDTEN_5       EQU  H'BF'; WDT enabled/disabled by SWDTEN bit
_WDTE_NSLEEP_5       EQU  H'DF'; WDT enabled while sleep=0, suspended when sleep=1; SWDTEN ignored
_WDTE_ON_5           EQU  H'FF'; WDT enabled regardless of sleep; SWDTEN is ignored

;----- CONFIG6 Options --------------------------------------------------
_WDTCWS_WDTCWS_0_6   EQU  H'F8'; window delay = 87.5; no software control; keyed access required
_WDTCWS_WDTCWS_1_6   EQU  H'F9'; window delay = 75 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_2_6   EQU  H'FA'; window delay = 62.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_3_6   EQU  H'FB'; window delay = 50 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_4_6   EQU  H'FC'; window delay = 37.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_5_6   EQU  H'FD'; window delay = 25 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_6_6   EQU  H'FE'; window always open (100%); no software control; keyed access required
_WDTCWS_WDTCWS_7_6   EQU  H'FF'; window always open (100%); software control; keyed access not required
_WDTCCS_LFINTOSC_6   EQU  H'C7'; WDT reference clock is the 31.0 kHz LFINTOSC
_WDTCCS_MFINTOSC_6   EQU  H'CF'; WDT reference clock is the 32kHz MFINTOSC output
_WDTCCS_SOSC_6       EQU  H'D7'; WDT reference clock is SOSC
_WDTCCS_SC_6         EQU  H'FF'; Software Control

;----- CONFIG7 Options --------------------------------------------------
_BBSIZE_BBSIZE_512_7 EQU  H'FF'; Boot Block size is 512 words
_BBSIZE_BBSIZE_1024_7 EQU  H'FE'; Boot Block size is 1024 words
_BBSIZE_BBSIZE_2048_7 EQU  H'FD'; Boot Block size is 2048 words
_BBSIZE_BBSIZE_4096_7 EQU  H'FC'; Boot Block size is 4096 words
_BBSIZE_BBSIZE_8192_7 EQU  H'FB'; Boot Block size is 8192 words
_BBSIZE_BBSIZE_16384_7 EQU  H'FA'; Boot Block size is 16384 words
_BBSIZE_BBSIZE_32768_7 EQU  H'F9'; Boot Block size is 32768 words
_BBEN_OFF_7          EQU  H'FF'; Boot block disabled
_BBEN_ON_7           EQU  H'F7'; Boot block enabled
_SAFEN_OFF_7         EQU  H'FF'; SAF disabled
_SAFEN_ON_7          EQU  H'EF'; SAF enabled

;----- CONFIG8 Options --------------------------------------------------
_WRTB_OFF_8          EQU  H'FF'; Boot Block not Write protected
_WRTB_ON_8           EQU  H'FE'; Boot Block Write protected
_WRTC_OFF_8          EQU  H'FF'; Configuration registers not Write protected
_WRTC_ON_8           EQU  H'FD'; Configuration registers Write protected
_WRTD_OFF_8          EQU  H'FF'; Data EEPROM not Write protected
_WRTD_ON_8           EQU  H'FB'; Data EEPROM Write protected
_WRTSAF_OFF_8        EQU  H'FF'; SAF not Write Protected
_WRTSAF_ON_8         EQU  H'F7'; SAF Write Protected
_WRTAPP_OFF_8        EQU  H'FF'; Application Block not write protected
_WRTAPP_ON_8         EQU  H'7F'; Application Block write protected

;----- CONFIG9 Options --------------------------------------------------
_BOOTPINSEL_RA4_9    EQU  H'FC'; CRC on boot output pin is RA4
_BOOTPINSEL_RA2_9    EQU  H'FD'; CRC on boot output pin is RA2
_BOOTPINSEL_RC4_9    EQU  H'FE'; CRC on boot output pin is RC4
_BOOTPINSEL_RC5_9    EQU  H'FF'; CRC on boot output pin is RC5
_BPEN_OFF_9          EQU  H'FF'; CRC on boot output pin disabled
_BPEN_ON_9           EQU  H'EF'; CRC on boot output pin enabled, selected by BOOTPINSEL<1:0>
_ODCON_OFF_9         EQU  H'FF'; Pin drives both high-going and low-going signals
_ODCON_ON_9          EQU  H'DF'; Pin drives only low-going signals

;----- CONFIG10 Options --------------------------------------------------
_CP_OFF_10           EQU  H'FF'; PFM and Data EEPROM code protection disabled
_CP_ON_10            EQU  H'FE'; PFM and Data EEPROM code protection enabled

;----- Dumping Configs 11 and higher --------------------------------------------------


;----- End of dumping Configs --------------------------------------------------


;----- DEVID Equates --------------------------------------------------
_IDLOC0            EQU  H'200002'
_IDLOC1            EQU  H'200003'
_IDLOC2            EQU  H'200004'
_IDLOC3            EQU  H'200005'
_IDLOC4            EQU  H'200006'
_IDLOC5            EQU  H'200007'
_IDLOC6            EQU  H'200008'
_IDLOC7            EQU  H'200009'
_IDLOC8            EQU  H'20000A'
_IDLOC9            EQU  H'20000B'
_IDLOC10           EQU  H'20000C'
_IDLOC11           EQU  H'20000D'
_IDLOC12           EQU  H'20000E'
_IDLOC13           EQU  H'20000F'
_IDLOC14           EQU  H'200010'
_IDLOC15           EQU  H'200011'
_IDLOC16           EQU  H'200012'
_IDLOC17           EQU  H'200013'
_IDLOC18           EQU  H'200014'
_IDLOC19           EQU  H'200015'
_IDLOC20           EQU  H'200016'
_IDLOC21           EQU  H'200017'
_IDLOC22           EQU  H'200018'
_IDLOC23           EQU  H'200019'
_IDLOC24           EQU  H'20001A'
_IDLOC25           EQU  H'20001B'
_IDLOC26           EQU  H'20001C'
_IDLOC27           EQU  H'20001D'
_IDLOC28           EQU  H'20001E'
_IDLOC29           EQU  H'20001F'

        LIST
