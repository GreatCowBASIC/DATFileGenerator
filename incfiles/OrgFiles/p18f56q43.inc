        LIST
;==========================================================================
;
;  Built by GCBASIC convertor : 18F_PIC_to_INCv6_15a
;  MPASM processor include for the chip shown below
;
; C:\Program Files\Microchip\MPLABX\v6.15\packs\index.idx
; C:\Program Files\Microchip\MPLABX\v6.15\packs\Microchip\PIC18F-Q_DFP\1.20.405\xc8\pic\dat\ini\18F56Q43.ini
; C:\Program Files\Microchip\MPLABX\v6.15\packs\Microchip\PIC18F-Q_DFP\1.20.405\edc\18F56Q43.PIC
; C:\Program Files\Microchip\MPLABX\v6.15\packs\Microchip\PIC18F-Q_DFP\1.20.405\xc8\pic\dat\cfgdata\18F56Q43.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __PIC18F56Q43
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
WDTPS            EQU  H'007A'
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
RD0PPS           EQU  H'0219'
RD1PPS           EQU  H'021A'
RD2PPS           EQU  H'021B'
RD3PPS           EQU  H'021C'
RD4PPS           EQU  H'021D'
RD5PPS           EQU  H'021E'
RD6PPS           EQU  H'021F'
RD7PPS           EQU  H'0220'
RE0PPS           EQU  H'0221'
RE1PPS           EQU  H'0222'
RE2PPS           EQU  H'0223'
RF0PPS           EQU  H'0229'
RF1PPS           EQU  H'022A'
RF2PPS           EQU  H'022B'
RF3PPS           EQU  H'022C'
RF4PPS           EQU  H'022D'
RF5PPS           EQU  H'022E'
RF6PPS           EQU  H'022F'
RF7PPS           EQU  H'0230'
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
CCP1PPS          EQU  H'024F'
CCP2PPS          EQU  H'0250'
CCP3PPS          EQU  H'0251'
PWM1ERSPPS       EQU  H'0253'
PWM2ERSPPS       EQU  H'0254'
PWM3ERSPPS       EQU  H'0255'
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
I2C1RXB          EQU  H'028B'
I2C1TXB          EQU  H'028C'
I2C1CNT          EQU  H'028D'
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
I2C1CLK          EQU  H'029C'
I2C1BTO          EQU  H'029D'
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
U2TXB            EQU  H'02B6'
U2TXBL           EQU  H'02B6'
U2P1             EQU  H'02B8'
U2P1L            EQU  H'02B8'
U2P2             EQU  H'02BA'
U2P2L            EQU  H'02BA'
U2P3             EQU  H'02BC'
U2P3L            EQU  H'02BC'
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
TMR1             EQU  H'031C'
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
PR1              EQU  H'0321'
T2TMR            EQU  H'0322'
TMR2             EQU  H'0322'
T2PR             EQU  H'0323'
PR2              EQU  H'0323'
T2CON            EQU  H'0324'
T2HLT            EQU  H'0325'
T2CLKCON         EQU  H'0326'
T2CLK            EQU  H'0326'
T2RST            EQU  H'0327'
TMR3             EQU  H'0328'
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
PR3              EQU  H'032D'
T4TMR            EQU  H'032E'
TMR4             EQU  H'032E'
T4PR             EQU  H'032F'
PR4              EQU  H'032F'
T4CON            EQU  H'0330'
T4HLT            EQU  H'0331'
T4CLKCON         EQU  H'0332'
T4CLK            EQU  H'0332'
T4RST            EQU  H'0333'
TMR5             EQU  H'0334'
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
PR5              EQU  H'0339'
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
CRCDATL          EQU  H'034F'
CRCDATH          EQU  H'0350'
CRCACC           EQU  H'0351'
CRCACCL          EQU  H'0351'
CRCACCH          EQU  H'0352'
CRCSHFT          EQU  H'0353'
CRCSHIFTL        EQU  H'0353'
CRCSHIFTH        EQU  H'0354'
CRCXOR           EQU  H'0355'
CRCXORL          EQU  H'0355'
CRCXORH          EQU  H'0356'
CRCCON0          EQU  H'0357'
CRCCON1          EQU  H'0358'
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
CWG1CLK          EQU  H'03BC'
CWG1CLKCON       EQU  H'03BC'
CWG1ISM          EQU  H'03BD'
CWG1DAT          EQU  H'03BD'
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
CWG2DAT          EQU  H'03C6'
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
CWG3DAT          EQU  H'03CF'
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
ANSELD           EQU  H'0418'
WPUD             EQU  H'0419'
ODCOND           EQU  H'041A'
SLRCOND          EQU  H'041B'
INLVLD           EQU  H'041C'
ANSELE           EQU  H'0420'
WPUE             EQU  H'0421'
ODCONE           EQU  H'0422'
SLRCONE          EQU  H'0423'
INLVLE           EQU  H'0424'
IOCEP            EQU  H'0425'
IOCEN            EQU  H'0426'
IOCEF            EQU  H'0427'
ANSELF           EQU  H'0428'
WPUF             EQU  H'0429'
ODCONF           EQU  H'042A'
SLRCONF          EQU  H'042B'
INLVLF           EQU  H'042C'
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
LATD             EQU  H'04C1'
LATE             EQU  H'04C2'
LATF             EQU  H'04C3'
TRISA            EQU  H'04C6'
TRISB            EQU  H'04C7'
TRISC            EQU  H'04C8'
TRISD            EQU  H'04C9'
TRISE            EQU  H'04CA'
TRISF            EQU  H'04CB'
PORTA            EQU  H'04CE'
PORTB            EQU  H'04CF'
PORTC            EQU  H'04D0'
PORTD            EQU  H'04D1'
PORTE            EQU  H'04D2'
PORTF            EQU  H'04D3'
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


;----- CLKRCON Bits -----------------------------------------------------
DIV0             EQU  H'0000'
DIV1             EQU  H'0001'
DIV2             EQU  H'0002'
DC0              EQU  H'0003'
DC1              EQU  H'0004'
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
NVMGO            EQU  H'0000'


;----- NVMCON1 Bits -----------------------------------------------------
CMD0             EQU  H'0000'
CMD1             EQU  H'0001'
CMD2             EQU  H'0002'
WRERR            EQU  H'0007'
NVMCMD0          EQU  H'0000'
NVMCMD1          EQU  H'0001'
NVMCMD2          EQU  H'0002'


;----- NVMLOCK Bits -----------------------------------------------------


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
NVMADRU0         EQU  H'0000'
NVMADRU1         EQU  H'0001'
NVMADRU2         EQU  H'0002'
NVMADRU3         EQU  H'0003'
NVMADRU4         EQU  H'0004'
NVMADRU5         EQU  H'0005'
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


;----- PMD1 Bits -----------------------------------------------------
TMR0MD           EQU  H'0000'
TMR1MD           EQU  H'0001'
TMR2MD           EQU  H'0002'
TMR3MD           EQU  H'0003'
TMR4MD           EQU  H'0004'
TMR5MD           EQU  H'0005'
TMR6MD           EQU  H'0006'
SMT1MD           EQU  H'0007'


;----- PMD3 Bits -----------------------------------------------------
ZCDMD            EQU  H'0000'
CM1MD            EQU  H'0001'
CM2MD            EQU  H'0002'
ADCMD            EQU  H'0005'
DAC1MD           EQU  H'0006'
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
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
C1PCH0           EQU  H'0000'
C1PCH1           EQU  H'0001'
C1PCH2           EQU  H'0002'


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
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
C2PCH0           EQU  H'0000'
C2PCH1           EQU  H'0001'
C2PCH2           EQU  H'0002'


;----- WDTCON0 Bits -----------------------------------------------------
SEN_WDTCON0      EQU  H'0000'
PS0              EQU  H'0001'
PS1              EQU  H'0002'
PS2              EQU  H'0003'
PS3              EQU  H'0004'
PS4              EQU  H'0005'
SWDTEN           EQU  H'0000'
WDTPS0           EQU  H'0001'
WDTPS1           EQU  H'0002'
WDTPS2           EQU  H'0003'
WDTPS3           EQU  H'0004'
WDTPS4           EQU  H'0005'
WDTSEN           EQU  H'0000'
PS0              EQU  H'0001'
PS1              EQU  H'0002'
PS2              EQU  H'0003'
PS3              EQU  H'0004'
PS4              EQU  H'0005'
WDTPS0           EQU  H'0001'
WDTPS1           EQU  H'0002'
WDTPS2           EQU  H'0003'
WDTPS3           EQU  H'0004'
WDTPS4           EQU  H'0005'


;----- WDTCON1 Bits -----------------------------------------------------
WINDOW0          EQU  H'0000'
WINDOW1          EQU  H'0001'
WINDOW2          EQU  H'0002'
CS0_WDTCON1      EQU  H'0004'
CS1_WDTCON1      EQU  H'0005'
CS2_WDTCON1      EQU  H'0006'
WINDOW0          EQU  H'0000'
WINDOW1          EQU  H'0001'
WINDOW2          EQU  H'0002'
WDTCS0           EQU  H'0004'
WDTCS1           EQU  H'0005'
WDTCS2           EQU  H'0006'
WDTWINDOW0       EQU  H'0000'
WDTWINDOW1       EQU  H'0001'
WDTWINDOW2       EQU  H'0002'
CS0_WDTCON1      EQU  H'0004'
CS1_WDTCON1      EQU  H'0005'
CS2_WDTCON1      EQU  H'0006'
WDTWINDOW0       EQU  H'0000'
WDTWINDOW1       EQU  H'0001'
WDTWINDOW2       EQU  H'0002'
WDTCS0           EQU  H'0004'
WDTCS1           EQU  H'0005'
WDTCS2           EQU  H'0006'


;----- WDTPS Bits -----------------------------------------------------
PSCNT0           EQU  H'0000'
PSCNT1           EQU  H'0001'
PSCNT2           EQU  H'0002'
PSCNT3           EQU  H'0003'
PSCNT4           EQU  H'0004'
PSCNT5           EQU  H'0005'
PSCNT6           EQU  H'0006'
PSCNT7           EQU  H'0007'


;----- WDTPSH Bits -----------------------------------------------------
PSCNT8           EQU  H'0000'
PSCNT9           EQU  H'0001'
PSCNT10          EQU  H'0002'
PSCNT11          EQU  H'0003'
PSCNT12          EQU  H'0004'
PSCNT13          EQU  H'0005'
PSCNT14          EQU  H'0006'
PSCNT15          EQU  H'0007'


;----- WDTTMR Bits -----------------------------------------------------
PSCNT16          EQU  H'0000'
PSCNT17          EQU  H'0001'
STATE            EQU  H'0002'
TMR0             EQU  H'0003'
TMR1             EQU  H'0004'
TMR2             EQU  H'0005'
TMR3             EQU  H'0006'
TMR4             EQU  H'0007'
WDTPSCNT16       EQU  H'0000'
WDTPSCNT17       EQU  H'0001'
WDTSTATE         EQU  H'0002'
WDTTMR0          EQU  H'0003'
WDTTMR1          EQU  H'0004'
WDTTMR2          EQU  H'0005'
WDTTMR3          EQU  H'0006'
WDTTMR4          EQU  H'0007'
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
NSS0             EQU  H'0000'
NSS1             EQU  H'0001'
PSS0_DAC1CON     EQU  H'0002'
PSS1_DAC1CON     EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'
OE2              EQU  H'0006'
EN               EQU  H'0007'
DAC1NSS0         EQU  H'0000'
DAC1NSS1         EQU  H'0001'
DAC1PSS0         EQU  H'0002'
DAC1PSS1         EQU  H'0003'
DAC1OE0          EQU  H'0004'
DAC1OE1          EQU  H'0005'
DAC1OE2          EQU  H'0006'
DAC1EN           EQU  H'0007'
NSS0             EQU  H'0000'
PSS0_DAC1CON     EQU  H'0002'
PSS1_DAC1CON     EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'


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


;----- SPI1TCNT Bits -----------------------------------------------------


;----- SPI1TCNTH Bits -----------------------------------------------------
TCNTH0           EQU  H'0000'
TCNTH1           EQU  H'0001'
TCNTH2           EQU  H'0002'


;----- SPI1CON0 Bits -----------------------------------------------------
BMODE            EQU  H'0000'
MST              EQU  H'0001'
LSBF             EQU  H'0002'
EN               EQU  H'0007'
SPI1BMODE        EQU  H'0000'
SPI1MST          EQU  H'0001'
SPI1LSBF         EQU  H'0002'
SPI1SPIEN        EQU  H'0007'


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


;----- SPI2TCNT Bits -----------------------------------------------------


;----- SPI2TCNTH Bits -----------------------------------------------------
TCNTH0           EQU  H'0000'
TCNTH1           EQU  H'0001'
TCNTH2           EQU  H'0002'


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
ORS              EQU  H'0001'
LOCK             EQU  H'0003'
UD               EQU  H'0006'
EN               EQU  H'0007'
ACTORS           EQU  H'0001'
ACTLOCK          EQU  H'0003'
ACTUD            EQU  H'0006'
ACTEN            EQU  H'0007'


;----- OSCCON1 Bits -----------------------------------------------------
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'
NOSC2            EQU  H'0006'
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'
NOSC2            EQU  H'0006'


;----- OSCCON2 Bits -----------------------------------------------------
CDIV0            EQU  H'0000'
CDIV1            EQU  H'0001'
CDIV2            EQU  H'0002'
CDIV3            EQU  H'0003'
COSC0            EQU  H'0004'
COSC1            EQU  H'0005'
COSC2            EQU  H'0006'
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
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'
TUN5             EQU  H'0005'


;----- OSCFRQ Bits -----------------------------------------------------
HFFRQ0           EQU  H'0000'
HFFRQ1           EQU  H'0001'
HFFRQ2           EQU  H'0002'
HFFRQ3           EQU  H'0003'
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
PR0_SCANPR       EQU  H'0000'
PR1_SCANPR       EQU  H'0001'
PR2_SCANPR       EQU  H'0002'
SCANPR0          EQU  H'0000'
SCANPR1          EQU  H'0001'
SCANPR2          EQU  H'0002'
SCANPR0          EQU  H'0000'
SCANPR1          EQU  H'0001'
SCANPR2          EQU  H'0002'


;----- DMA1PR Bits -----------------------------------------------------
PR0_DMA1PR       EQU  H'0000'
PR1_DMA1PR       EQU  H'0001'
PR2_DMA1PR       EQU  H'0002'
PR0_DMA1PR       EQU  H'0000'
PR1_DMA1PR       EQU  H'0001'
PR2_DMA1PR       EQU  H'0002'
DMA1PR0          EQU  H'0000'
DMA1PR1          EQU  H'0001'
DMA1PR2          EQU  H'0002'
DMA1PR0          EQU  H'0000'
DMA1PR1          EQU  H'0001'
DMA1PR2          EQU  H'0002'


;----- DMA2PR Bits -----------------------------------------------------
PR0_DMA2PR       EQU  H'0000'
PR1_DMA2PR       EQU  H'0001'
PR2_DMA2PR       EQU  H'0002'
PR0_DMA2PR       EQU  H'0000'
PR1_DMA2PR       EQU  H'0001'
PR2_DMA2PR       EQU  H'0002'
DMA2PR0          EQU  H'0000'
DMA2PR1          EQU  H'0001'
DMA2PR2          EQU  H'0002'
DMA2PR0          EQU  H'0000'
DMA2PR1          EQU  H'0001'
DMA2PR2          EQU  H'0002'


;----- DMA3PR Bits -----------------------------------------------------
PR0_DMA3PR       EQU  H'0000'
PR1_DMA3PR       EQU  H'0001'
PR2_DMA3PR       EQU  H'0002'
PR0_DMA3PR       EQU  H'0000'
PR1_DMA3PR       EQU  H'0001'
PR2_DMA3PR       EQU  H'0002'
DMA3PR0          EQU  H'0000'
DMA3PR1          EQU  H'0001'
DMA3PR2          EQU  H'0002'
DMA3PR0          EQU  H'0000'
DMA3PR1          EQU  H'0001'
DMA3PR2          EQU  H'0002'


;----- DMA4PR Bits -----------------------------------------------------
PR0_DMA4PR       EQU  H'0000'
PR1_DMA4PR       EQU  H'0001'
PR2_DMA4PR       EQU  H'0002'
PR0_DMA4PR       EQU  H'0000'
PR1_DMA4PR       EQU  H'0001'
PR2_DMA4PR       EQU  H'0002'
DMA4PR0          EQU  H'0000'
DMA4PR1          EQU  H'0001'
DMA4PR2          EQU  H'0002'
DMA4PR0          EQU  H'0000'
DMA4PR1          EQU  H'0001'
DMA4PR2          EQU  H'0002'


;----- DMA5PR Bits -----------------------------------------------------
PR0_DMA5PR       EQU  H'0000'
PR1_DMA5PR       EQU  H'0001'
PR2_DMA5PR       EQU  H'0002'
PR0_DMA5PR       EQU  H'0000'
PR1_DMA5PR       EQU  H'0001'
PR2_DMA5PR       EQU  H'0002'
DMA5PR0          EQU  H'0000'
DMA5PR1          EQU  H'0001'
DMA5PR2          EQU  H'0002'
DMA5PR0          EQU  H'0000'
DMA5PR1          EQU  H'0001'
DMA5PR2          EQU  H'0002'


;----- DMA6PR Bits -----------------------------------------------------
PR0_DMA6PR       EQU  H'0000'
PR1_DMA6PR       EQU  H'0001'
PR2_DMA6PR       EQU  H'0002'
PR0_DMA6PR       EQU  H'0000'
PR1_DMA6PR       EQU  H'0001'
PR2_DMA6PR       EQU  H'0002'
DMA6PR0          EQU  H'0000'
DMA6PR1          EQU  H'0001'
DMA6PR2          EQU  H'0002'
DMA6PR0          EQU  H'0000'
DMA6PR1          EQU  H'0001'
DMA6PR2          EQU  H'0002'


;----- MAINPR Bits -----------------------------------------------------
PR0_MAINPR       EQU  H'0000'
PR1_MAINPR       EQU  H'0001'
PR2_MAINPR       EQU  H'0002'
PR0_MAINPR       EQU  H'0000'
PR1_MAINPR       EQU  H'0001'
PR2_MAINPR       EQU  H'0002'
MAINPR0          EQU  H'0000'
MAINPR1          EQU  H'0001'
MAINPR2          EQU  H'0002'
MAINPR0          EQU  H'0000'
MAINPR1          EQU  H'0001'
MAINPR2          EQU  H'0002'


;----- ISRPR Bits -----------------------------------------------------
PR0_ISRPR        EQU  H'0000'
PR1_ISRPR        EQU  H'0001'
PR2_ISRPR        EQU  H'0002'
PR0_ISRPR        EQU  H'0000'
PR1_ISRPR        EQU  H'0001'
PR2_ISRPR        EQU  H'0002'
ISRPR0           EQU  H'0000'
ISRPR1           EQU  H'0001'
ISRPR2           EQU  H'0002'
ISRPR0           EQU  H'0000'
ISRPR1           EQU  H'0001'
ISRPR2           EQU  H'0002'


;----- CLCDATA Bits -----------------------------------------------------
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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
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


;----- CLCNSEL1 Bits -----------------------------------------------------
D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'
D2S6             EQU  H'0006'
D2S7             EQU  H'0007'


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


;----- CLCNGLS1 Bits -----------------------------------------------------
G2D1N            EQU  H'0000'
G2D1T            EQU  H'0001'
G2D2N            EQU  H'0002'
G2D2T            EQU  H'0003'
G2D3N            EQU  H'0004'
G2D3T            EQU  H'0005'
G2D4N            EQU  H'0006'
G2D4T            EQU  H'0007'


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
SMODE0           EQU  H'0001'
SMODE1           EQU  H'0002'
SMR0             EQU  H'0003'
SMR1             EQU  H'0004'
DSTP             EQU  H'0005'
DMODE0           EQU  H'0006'
DMODE1           EQU  H'0007'


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


;----- PPSLOCK Bits -----------------------------------------------------
PPSLOCKED        EQU  H'0000'


;----- RA0PPS Bits -----------------------------------------------------
RA0PPS0          EQU  H'0000'
RA0PPS1          EQU  H'0001'
RA0PPS2          EQU  H'0002'
RA0PPS3          EQU  H'0003'
RA0PPS4          EQU  H'0004'
RA0PPS5          EQU  H'0005'
RA0PPS6          EQU  H'0006'


;----- RA1PPS Bits -----------------------------------------------------
RA1PPS0          EQU  H'0000'
RA1PPS1          EQU  H'0001'
RA1PPS2          EQU  H'0002'
RA1PPS3          EQU  H'0003'
RA1PPS4          EQU  H'0004'
RA1PPS5          EQU  H'0005'
RA1PPS6          EQU  H'0006'


;----- RA2PPS Bits -----------------------------------------------------
RA2PPS0          EQU  H'0000'
RA2PPS1          EQU  H'0001'
RA2PPS2          EQU  H'0002'
RA2PPS3          EQU  H'0003'
RA2PPS4          EQU  H'0004'
RA2PPS5          EQU  H'0005'
RA2PPS6          EQU  H'0006'


;----- RA3PPS Bits -----------------------------------------------------
RA3PPS0          EQU  H'0000'
RA3PPS1          EQU  H'0001'
RA3PPS2          EQU  H'0002'
RA3PPS3          EQU  H'0003'
RA3PPS4          EQU  H'0004'
RA3PPS5          EQU  H'0005'
RA3PPS6          EQU  H'0006'


;----- RA4PPS Bits -----------------------------------------------------
RA4PPS0          EQU  H'0000'
RA4PPS1          EQU  H'0001'
RA4PPS2          EQU  H'0002'
RA4PPS3          EQU  H'0003'
RA4PPS4          EQU  H'0004'
RA4PPS5          EQU  H'0005'
RA4PPS6          EQU  H'0006'


;----- RA5PPS Bits -----------------------------------------------------
RA5PPS0          EQU  H'0000'
RA5PPS1          EQU  H'0001'
RA5PPS2          EQU  H'0002'
RA5PPS3          EQU  H'0003'
RA5PPS4          EQU  H'0004'
RA5PPS5          EQU  H'0005'
RA5PPS6          EQU  H'0006'


;----- RA6PPS Bits -----------------------------------------------------
RA6PPS0          EQU  H'0000'
RA6PPS1          EQU  H'0001'
RA6PPS2          EQU  H'0002'
RA6PPS3          EQU  H'0003'
RA6PPS4          EQU  H'0004'
RA6PPS5          EQU  H'0005'
RA6PPS6          EQU  H'0006'


;----- RA7PPS Bits -----------------------------------------------------
RA7PPS0          EQU  H'0000'
RA7PPS1          EQU  H'0001'
RA7PPS2          EQU  H'0002'
RA7PPS3          EQU  H'0003'
RA7PPS4          EQU  H'0004'
RA7PPS5          EQU  H'0005'
RA7PPS6          EQU  H'0006'


;----- RB0PPS Bits -----------------------------------------------------
RB0PPS0          EQU  H'0000'
RB0PPS1          EQU  H'0001'
RB0PPS2          EQU  H'0002'
RB0PPS3          EQU  H'0003'
RB0PPS4          EQU  H'0004'
RB0PPS5          EQU  H'0005'
RB0PPS6          EQU  H'0006'


;----- RB1PPS Bits -----------------------------------------------------
RB1PPS0          EQU  H'0000'
RB1PPS1          EQU  H'0001'
RB1PPS2          EQU  H'0002'
RB1PPS3          EQU  H'0003'
RB1PPS4          EQU  H'0004'
RB1PPS5          EQU  H'0005'
RB1PPS6          EQU  H'0006'


;----- RB2PPS Bits -----------------------------------------------------
RB2PPS0          EQU  H'0000'
RB2PPS1          EQU  H'0001'
RB2PPS2          EQU  H'0002'
RB2PPS3          EQU  H'0003'
RB2PPS4          EQU  H'0004'
RB2PPS5          EQU  H'0005'
RB2PPS6          EQU  H'0006'


;----- RB3PPS Bits -----------------------------------------------------
RB3PPS0          EQU  H'0000'
RB3PPS1          EQU  H'0001'
RB3PPS2          EQU  H'0002'
RB3PPS3          EQU  H'0003'
RB3PPS4          EQU  H'0004'
RB3PPS5          EQU  H'0005'
RB3PPS6          EQU  H'0006'


;----- RB4PPS Bits -----------------------------------------------------
RB4PPS0          EQU  H'0000'
RB4PPS1          EQU  H'0001'
RB4PPS2          EQU  H'0002'
RB4PPS3          EQU  H'0003'
RB4PPS4          EQU  H'0004'
RB4PPS5          EQU  H'0005'
RB4PPS6          EQU  H'0006'


;----- RB5PPS Bits -----------------------------------------------------
RB5PPS0          EQU  H'0000'
RB5PPS1          EQU  H'0001'
RB5PPS2          EQU  H'0002'
RB5PPS3          EQU  H'0003'
RB5PPS4          EQU  H'0004'
RB5PPS5          EQU  H'0005'
RB5PPS6          EQU  H'0006'


;----- RB6PPS Bits -----------------------------------------------------
RB6PPS0          EQU  H'0000'
RB6PPS1          EQU  H'0001'
RB6PPS2          EQU  H'0002'
RB6PPS3          EQU  H'0003'
RB6PPS4          EQU  H'0004'
RB6PPS5          EQU  H'0005'
RB6PPS6          EQU  H'0006'


;----- RB7PPS Bits -----------------------------------------------------
RB7PPS0          EQU  H'0000'
RB7PPS1          EQU  H'0001'
RB7PPS2          EQU  H'0002'
RB7PPS3          EQU  H'0003'
RB7PPS4          EQU  H'0004'
RB7PPS5          EQU  H'0005'
RB7PPS6          EQU  H'0006'


;----- RC0PPS Bits -----------------------------------------------------
RC0PPS0          EQU  H'0000'
RC0PPS1          EQU  H'0001'
RC0PPS2          EQU  H'0002'
RC0PPS3          EQU  H'0003'
RC0PPS4          EQU  H'0004'
RC0PPS5          EQU  H'0005'
RC0PPS6          EQU  H'0006'


;----- RC1PPS Bits -----------------------------------------------------
RC1PPS0          EQU  H'0000'
RC1PPS1          EQU  H'0001'
RC1PPS2          EQU  H'0002'
RC1PPS3          EQU  H'0003'
RC1PPS4          EQU  H'0004'
RC1PPS5          EQU  H'0005'
RC1PPS6          EQU  H'0006'


;----- RC2PPS Bits -----------------------------------------------------
RC2PPS0          EQU  H'0000'
RC2PPS1          EQU  H'0001'
RC2PPS2          EQU  H'0002'
RC2PPS3          EQU  H'0003'
RC2PPS4          EQU  H'0004'
RC2PPS5          EQU  H'0005'
RC2PPS6          EQU  H'0006'


;----- RC3PPS Bits -----------------------------------------------------
RC3PPS0          EQU  H'0000'
RC3PPS1          EQU  H'0001'
RC3PPS2          EQU  H'0002'
RC3PPS3          EQU  H'0003'
RC3PPS4          EQU  H'0004'
RC3PPS5          EQU  H'0005'
RC3PPS6          EQU  H'0006'


;----- RC4PPS Bits -----------------------------------------------------
RC4PPS0          EQU  H'0000'
RC4PPS1          EQU  H'0001'
RC4PPS2          EQU  H'0002'
RC4PPS3          EQU  H'0003'
RC4PPS4          EQU  H'0004'
RC4PPS5          EQU  H'0005'
RC4PPS6          EQU  H'0006'


;----- RC5PPS Bits -----------------------------------------------------
RC5PPS0          EQU  H'0000'
RC5PPS1          EQU  H'0001'
RC5PPS2          EQU  H'0002'
RC5PPS3          EQU  H'0003'
RC5PPS4          EQU  H'0004'
RC5PPS5          EQU  H'0005'
RC5PPS6          EQU  H'0006'


;----- RC6PPS Bits -----------------------------------------------------
RC6PPS0          EQU  H'0000'
RC6PPS1          EQU  H'0001'
RC6PPS2          EQU  H'0002'
RC6PPS3          EQU  H'0003'
RC6PPS4          EQU  H'0004'
RC6PPS5          EQU  H'0005'
RC6PPS6          EQU  H'0006'


;----- RC7PPS Bits -----------------------------------------------------
RC7PPS0          EQU  H'0000'
RC7PPS1          EQU  H'0001'
RC7PPS2          EQU  H'0002'
RC7PPS3          EQU  H'0003'
RC7PPS4          EQU  H'0004'
RC7PPS5          EQU  H'0005'
RC7PPS6          EQU  H'0006'


;----- RD0PPS Bits -----------------------------------------------------
RD0PPS0          EQU  H'0000'
RD0PPS1          EQU  H'0001'
RD0PPS2          EQU  H'0002'
RD0PPS3          EQU  H'0003'
RD0PPS4          EQU  H'0004'
RD0PPS5          EQU  H'0005'
RD0PPS6          EQU  H'0006'


;----- RD1PPS Bits -----------------------------------------------------
RD1PPS0          EQU  H'0000'
RD1PPS1          EQU  H'0001'
RD1PPS2          EQU  H'0002'
RD1PPS3          EQU  H'0003'
RD1PPS4          EQU  H'0004'
RD1PPS5          EQU  H'0005'
RD1PPS6          EQU  H'0006'


;----- RD2PPS Bits -----------------------------------------------------
RD2PPS0          EQU  H'0000'
RD2PPS1          EQU  H'0001'
RD2PPS2          EQU  H'0002'
RD2PPS3          EQU  H'0003'
RD2PPS4          EQU  H'0004'
RD2PPS5          EQU  H'0005'
RD2PPS6          EQU  H'0006'


;----- RD3PPS Bits -----------------------------------------------------
RD3PPS0          EQU  H'0000'
RD3PPS1          EQU  H'0001'
RD3PPS2          EQU  H'0002'
RD3PPS3          EQU  H'0003'
RD3PPS4          EQU  H'0004'
RD3PPS5          EQU  H'0005'
RD3PPS6          EQU  H'0006'


;----- RD4PPS Bits -----------------------------------------------------
RD4PPS0          EQU  H'0000'
RD4PPS1          EQU  H'0001'
RD4PPS2          EQU  H'0002'
RD4PPS3          EQU  H'0003'
RD4PPS4          EQU  H'0004'
RD4PPS5          EQU  H'0005'
RD4PPS6          EQU  H'0006'


;----- RD5PPS Bits -----------------------------------------------------
RD5PPS0          EQU  H'0000'
RD5PPS1          EQU  H'0001'
RD5PPS2          EQU  H'0002'
RD5PPS3          EQU  H'0003'
RD5PPS4          EQU  H'0004'
RD5PPS5          EQU  H'0005'
RD5PPS6          EQU  H'0006'


;----- RD6PPS Bits -----------------------------------------------------
RD6PPS0          EQU  H'0000'
RD6PPS1          EQU  H'0001'
RD6PPS2          EQU  H'0002'
RD6PPS3          EQU  H'0003'
RD6PPS4          EQU  H'0004'
RD6PPS5          EQU  H'0005'
RD6PPS6          EQU  H'0006'


;----- RD7PPS Bits -----------------------------------------------------
RD7PPS0          EQU  H'0000'
RD7PPS1          EQU  H'0001'
RD7PPS2          EQU  H'0002'
RD7PPS3          EQU  H'0003'
RD7PPS4          EQU  H'0004'
RD7PPS5          EQU  H'0005'
RD7PPS6          EQU  H'0006'


;----- RE0PPS Bits -----------------------------------------------------
RE0PPS0          EQU  H'0000'
RE0PPS1          EQU  H'0001'
RE0PPS2          EQU  H'0002'
RE0PPS3          EQU  H'0003'
RE0PPS4          EQU  H'0004'
RE0PPS5          EQU  H'0005'
RE0PPS6          EQU  H'0006'


;----- RE1PPS Bits -----------------------------------------------------
RE1PPS0          EQU  H'0000'
RE1PPS1          EQU  H'0001'
RE1PPS2          EQU  H'0002'
RE1PPS3          EQU  H'0003'
RE1PPS4          EQU  H'0004'
RE1PPS5          EQU  H'0005'
RE1PPS6          EQU  H'0006'


;----- RE2PPS Bits -----------------------------------------------------
RE2PPS0          EQU  H'0000'
RE2PPS1          EQU  H'0001'
RE2PPS2          EQU  H'0002'
RE2PPS3          EQU  H'0003'
RE2PPS4          EQU  H'0004'
RE2PPS5          EQU  H'0005'
RE2PPS6          EQU  H'0006'


;----- RF0PPS Bits -----------------------------------------------------
RF0PPS0          EQU  H'0000'
RF0PPS1          EQU  H'0001'
RF0PPS2          EQU  H'0002'
RF0PPS3          EQU  H'0003'
RF0PPS4          EQU  H'0004'
RF0PPS5          EQU  H'0005'
RF0PPS6          EQU  H'0006'


;----- RF1PPS Bits -----------------------------------------------------
RF1PPS0          EQU  H'0000'
RF1PPS1          EQU  H'0001'
RF1PPS2          EQU  H'0002'
RF1PPS3          EQU  H'0003'
RF1PPS4          EQU  H'0004'
RF1PPS5          EQU  H'0005'
RF1PPS6          EQU  H'0006'


;----- RF2PPS Bits -----------------------------------------------------
RF2PPS0          EQU  H'0000'
RF2PPS1          EQU  H'0001'
RF2PPS2          EQU  H'0002'
RF2PPS3          EQU  H'0003'
RF2PPS4          EQU  H'0004'
RF2PPS5          EQU  H'0005'
RF2PPS6          EQU  H'0006'


;----- RF3PPS Bits -----------------------------------------------------
RF3PPS0          EQU  H'0000'
RF3PPS1          EQU  H'0001'
RF3PPS2          EQU  H'0002'
RF3PPS3          EQU  H'0003'
RF3PPS4          EQU  H'0004'
RF3PPS5          EQU  H'0005'
RF3PPS6          EQU  H'0006'


;----- RF4PPS Bits -----------------------------------------------------
RF4PPS0          EQU  H'0000'
RF4PPS1          EQU  H'0001'
RF4PPS2          EQU  H'0002'
RF4PPS3          EQU  H'0003'
RF4PPS4          EQU  H'0004'
RF4PPS5          EQU  H'0005'
RF4PPS6          EQU  H'0006'


;----- RF5PPS Bits -----------------------------------------------------
RF5PPS0          EQU  H'0000'
RF5PPS1          EQU  H'0001'
RF5PPS2          EQU  H'0002'
RF5PPS3          EQU  H'0003'
RF5PPS4          EQU  H'0004'
RF5PPS5          EQU  H'0005'
RF5PPS6          EQU  H'0006'


;----- RF6PPS Bits -----------------------------------------------------
RF6PPS0          EQU  H'0000'
RF6PPS1          EQU  H'0001'
RF6PPS2          EQU  H'0002'
RF6PPS3          EQU  H'0003'
RF6PPS4          EQU  H'0004'
RF6PPS5          EQU  H'0005'
RF6PPS6          EQU  H'0006'


;----- RF7PPS Bits -----------------------------------------------------
RF7PPS0          EQU  H'0000'
RF7PPS1          EQU  H'0001'
RF7PPS2          EQU  H'0002'
RF7PPS3          EQU  H'0003'
RF7PPS4          EQU  H'0004'
RF7PPS5          EQU  H'0005'
RF7PPS6          EQU  H'0006'


;----- INT0PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT             EQU  H'0003'
INT0PPS0         EQU  H'0000'
INT0PPS1         EQU  H'0001'
INT0PPS2         EQU  H'0002'
INT0PPS3         EQU  H'0003'
INT0PPS0         EQU  H'0000'
INT0PPS1         EQU  H'0001'
INT0PPS2         EQU  H'0002'
INT0PPS3         EQU  H'0003'


;----- INT1PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
INT1PPS0         EQU  H'0000'
INT1PPS1         EQU  H'0001'
INT1PPS2         EQU  H'0002'
INT1PPS3         EQU  H'0003'
INT1PPS4         EQU  H'0004'
INT1PPS0         EQU  H'0000'
INT1PPS1         EQU  H'0001'
INT1PPS2         EQU  H'0002'
INT1PPS3         EQU  H'0003'
INT1PPS4         EQU  H'0004'


;----- INT2PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
INT2PPS0         EQU  H'0000'
INT2PPS1         EQU  H'0001'
INT2PPS2         EQU  H'0002'
INT2PPS3         EQU  H'0003'
INT2PPS4         EQU  H'0004'
INT2PPS5         EQU  H'0005'
INT2PPS0         EQU  H'0000'
INT2PPS1         EQU  H'0001'
INT2PPS2         EQU  H'0002'
INT2PPS3         EQU  H'0003'
INT2PPS4         EQU  H'0004'
INT2PPS5         EQU  H'0005'


;----- T0CKIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'
T0CKIPPS5        EQU  H'0005'
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'
T0CKIPPS5        EQU  H'0005'


;----- T1CKIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
T1CKIPPS0        EQU  H'0000'
T1CKIPPS1        EQU  H'0001'
T1CKIPPS2        EQU  H'0002'
T1CKIPPS3        EQU  H'0003'
T1CKIPPS4        EQU  H'0004'
T1CKIPPS5        EQU  H'0005'
T1CKIPPS0        EQU  H'0000'
T1CKIPPS1        EQU  H'0001'
T1CKIPPS2        EQU  H'0002'
T1CKIPPS3        EQU  H'0003'
T1CKIPPS4        EQU  H'0004'
T1CKIPPS5        EQU  H'0005'


;----- T1GPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T1GPPS0          EQU  H'0000'
T1GPPS1          EQU  H'0001'
T1GPPS2          EQU  H'0002'
T1GPPS3          EQU  H'0003'
T1GPPS4          EQU  H'0004'
T1GPPS0          EQU  H'0000'
T1GPPS1          EQU  H'0001'
T1GPPS2          EQU  H'0002'
T1GPPS3          EQU  H'0003'
T1GPPS4          EQU  H'0004'


;----- T3CKIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
T3CKIPPS0        EQU  H'0000'
T3CKIPPS1        EQU  H'0001'
T3CKIPPS2        EQU  H'0002'
T3CKIPPS3        EQU  H'0003'
T3CKIPPS4        EQU  H'0004'
T3CKIPPS5        EQU  H'0005'
T3CKIPPS0        EQU  H'0000'
T3CKIPPS1        EQU  H'0001'
T3CKIPPS2        EQU  H'0002'
T3CKIPPS3        EQU  H'0003'
T3CKIPPS4        EQU  H'0004'
T3CKIPPS5        EQU  H'0005'


;----- T3GPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T3GPPS0          EQU  H'0000'
T3GPPS1          EQU  H'0001'
T3GPPS2          EQU  H'0002'
T3GPPS3          EQU  H'0003'
T3GPPS4          EQU  H'0004'
T3GPPS0          EQU  H'0000'
T3GPPS1          EQU  H'0001'
T3GPPS2          EQU  H'0002'
T3GPPS3          EQU  H'0003'
T3GPPS4          EQU  H'0004'


;----- T5CKIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
T5CKIPPS0        EQU  H'0000'
T5CKIPPS1        EQU  H'0001'
T5CKIPPS2        EQU  H'0002'
T5CKIPPS3        EQU  H'0003'
T5CKIPPS4        EQU  H'0004'
T5CKIPPS5        EQU  H'0005'
T5CKIPPS0        EQU  H'0000'
T5CKIPPS1        EQU  H'0001'
T5CKIPPS2        EQU  H'0002'
T5CKIPPS3        EQU  H'0003'
T5CKIPPS4        EQU  H'0004'
T5CKIPPS5        EQU  H'0005'


;----- T5GPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T5GPPS0          EQU  H'0000'
T5GPPS1          EQU  H'0001'
T5GPPS2          EQU  H'0002'
T5GPPS3          EQU  H'0003'
T5GPPS4          EQU  H'0004'
T5GPPS0          EQU  H'0000'
T5GPPS1          EQU  H'0001'
T5GPPS2          EQU  H'0002'
T5GPPS3          EQU  H'0003'
T5GPPS4          EQU  H'0004'


;----- T2INPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T2INPPS0         EQU  H'0000'
T2INPPS1         EQU  H'0001'
T2INPPS2         EQU  H'0002'
T2INPPS3         EQU  H'0003'
T2INPPS4         EQU  H'0004'
T2INPPS0         EQU  H'0000'
T2INPPS1         EQU  H'0001'
T2INPPS2         EQU  H'0002'
T2INPPS3         EQU  H'0003'
T2INPPS4         EQU  H'0004'


;----- T4INPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T4INPPS0         EQU  H'0000'
T4INPPS1         EQU  H'0001'
T4INPPS2         EQU  H'0002'
T4INPPS3         EQU  H'0003'
T4INPPS4         EQU  H'0004'
T4INPPS0         EQU  H'0000'
T4INPPS1         EQU  H'0001'
T4INPPS2         EQU  H'0002'
T4INPPS3         EQU  H'0003'
T4INPPS4         EQU  H'0004'


;----- T6INPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
T6INPPS0         EQU  H'0000'
T6INPPS1         EQU  H'0001'
T6INPPS2         EQU  H'0002'
T6INPPS3         EQU  H'0003'
T6INPPS4         EQU  H'0004'
T6INPPS0         EQU  H'0000'
T6INPPS1         EQU  H'0001'
T6INPPS2         EQU  H'0002'
T6INPPS3         EQU  H'0003'
T6INPPS4         EQU  H'0004'


;----- CCP1PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'
CCP1PPS5         EQU  H'0005'
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'
CCP1PPS5         EQU  H'0005'


;----- CCP2PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'
CCP2PPS5         EQU  H'0005'
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'
CCP2PPS5         EQU  H'0005'


;----- CCP3PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CCP3PPS0         EQU  H'0000'
CCP3PPS1         EQU  H'0001'
CCP3PPS2         EQU  H'0002'
CCP3PPS3         EQU  H'0003'
CCP3PPS4         EQU  H'0004'
CCP3PPS0         EQU  H'0000'
CCP3PPS1         EQU  H'0001'
CCP3PPS2         EQU  H'0002'
CCP3PPS3         EQU  H'0003'
CCP3PPS4         EQU  H'0004'


;----- PWM1ERSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PWM1ERSPPS0      EQU  H'0000'
PWM1ERSPPS1      EQU  H'0001'
PWM1ERSPPS2      EQU  H'0002'
PWM1ERSPPS3      EQU  H'0003'
PWM1ERSPPS4      EQU  H'0004'
PWM1ERSPPS0      EQU  H'0000'
PWM1ERSPPS1      EQU  H'0001'
PWM1ERSPPS2      EQU  H'0002'
PWM1ERSPPS3      EQU  H'0003'
PWM1ERSPPS4      EQU  H'0004'


;----- PWM2ERSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
PWM2ERSPPS0      EQU  H'0000'
PWM2ERSPPS1      EQU  H'0001'
PWM2ERSPPS2      EQU  H'0002'
PWM2ERSPPS3      EQU  H'0003'
PWM2ERSPPS4      EQU  H'0004'
PWM2ERSPPS5      EQU  H'0005'
PWM2ERSPPS0      EQU  H'0000'
PWM2ERSPPS1      EQU  H'0001'
PWM2ERSPPS2      EQU  H'0002'
PWM2ERSPPS3      EQU  H'0003'
PWM2ERSPPS4      EQU  H'0004'
PWM2ERSPPS5      EQU  H'0005'


;----- PWM3ERSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PWM3ERSPPS0      EQU  H'0000'
PWM3ERSPPS1      EQU  H'0001'
PWM3ERSPPS2      EQU  H'0002'
PWM3ERSPPS3      EQU  H'0003'
PWM3ERSPPS4      EQU  H'0004'
PWM3ERSPPS0      EQU  H'0000'
PWM3ERSPPS1      EQU  H'0001'
PWM3ERSPPS2      EQU  H'0002'
PWM3ERSPPS3      EQU  H'0003'
PWM3ERSPPS4      EQU  H'0004'


;----- PWMIN0PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
PWMIN0PPS0       EQU  H'0000'
PWMIN0PPS1       EQU  H'0001'
PWMIN0PPS2       EQU  H'0002'
PWMIN0PPS3       EQU  H'0003'
PWMIN0PPS4       EQU  H'0004'
PWMIN0PPS5       EQU  H'0005'
PWMIN0PPS0       EQU  H'0000'
PWMIN0PPS1       EQU  H'0001'
PWMIN0PPS2       EQU  H'0002'
PWMIN0PPS3       EQU  H'0003'
PWMIN0PPS4       EQU  H'0004'
PWMIN0PPS5       EQU  H'0005'


;----- PWMIN1PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
PWMIN1PPS0       EQU  H'0000'
PWMIN1PPS1       EQU  H'0001'
PWMIN1PPS2       EQU  H'0002'
PWMIN1PPS3       EQU  H'0003'
PWMIN1PPS4       EQU  H'0004'
PWMIN1PPS5       EQU  H'0005'
PWMIN1PPS0       EQU  H'0000'
PWMIN1PPS1       EQU  H'0001'
PWMIN1PPS2       EQU  H'0002'
PWMIN1PPS3       EQU  H'0003'
PWMIN1PPS4       EQU  H'0004'
PWMIN1PPS5       EQU  H'0005'


;----- SMT1WINPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
SMT1WINPPS0      EQU  H'0000'
SMT1WINPPS1      EQU  H'0001'
SMT1WINPPS2      EQU  H'0002'
SMT1WINPPS3      EQU  H'0003'
SMT1WINPPS4      EQU  H'0004'
SMT1WINPPS5      EQU  H'0005'
SMT1WINPPS0      EQU  H'0000'
SMT1WINPPS1      EQU  H'0001'
SMT1WINPPS2      EQU  H'0002'
SMT1WINPPS3      EQU  H'0003'
SMT1WINPPS4      EQU  H'0004'
SMT1WINPPS5      EQU  H'0005'


;----- SMT1SIGPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
SMT1SIGPPS0      EQU  H'0000'
SMT1SIGPPS1      EQU  H'0001'
SMT1SIGPPS2      EQU  H'0002'
SMT1SIGPPS3      EQU  H'0003'
SMT1SIGPPS4      EQU  H'0004'
SMT1SIGPPS5      EQU  H'0005'
SMT1SIGPPS0      EQU  H'0000'
SMT1SIGPPS1      EQU  H'0001'
SMT1SIGPPS2      EQU  H'0002'
SMT1SIGPPS3      EQU  H'0003'
SMT1SIGPPS4      EQU  H'0004'
SMT1SIGPPS5      EQU  H'0005'


;----- CWG1PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
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
CWG1INPPS0       EQU  H'0000'
CWG1INPPS1       EQU  H'0001'
CWG1INPPS2       EQU  H'0002'
CWG1INPPS3       EQU  H'0003'
CWG1INPPS4       EQU  H'0004'
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'


;----- CWG2PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
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
CWG2INPPS0       EQU  H'0000'
CWG2INPPS1       EQU  H'0001'
CWG2INPPS2       EQU  H'0002'
CWG2INPPS3       EQU  H'0003'
CWG2INPPS4       EQU  H'0004'
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'


;----- CWG3PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
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
CWG3INPPS0       EQU  H'0000'
CWG3INPPS1       EQU  H'0001'
CWG3INPPS2       EQU  H'0002'
CWG3INPPS3       EQU  H'0003'
CWG3INPPS4       EQU  H'0004'
CWGINPPS0        EQU  H'0000'
CWGINPPS1        EQU  H'0001'
CWGINPPS2        EQU  H'0002'
CWGINPPS3        EQU  H'0003'
CWGINPPS4        EQU  H'0004'


;----- MD1CARLPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
MDCARLPPS0       EQU  H'0000'
MDCARLPPS1       EQU  H'0001'
MDCARLPPS2       EQU  H'0002'
MDCARLPPS3       EQU  H'0003'
MDCARLPPS4       EQU  H'0004'
MDCARLPPS0       EQU  H'0000'
MDCARLPPS1       EQU  H'0001'
MDCARLPPS2       EQU  H'0002'
MDCARLPPS3       EQU  H'0003'
MDCARLPPS4       EQU  H'0004'


;----- MD1CARHPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
MDCARHPPS0       EQU  H'0000'
MDCARHPPS1       EQU  H'0001'
MDCARHPPS2       EQU  H'0002'
MDCARHPPS3       EQU  H'0003'
MDCARHPPS4       EQU  H'0004'
MDCARHPPS0       EQU  H'0000'
MDCARHPPS1       EQU  H'0001'
MDCARHPPS2       EQU  H'0002'
MDCARHPPS3       EQU  H'0003'
MDCARHPPS4       EQU  H'0004'


;----- MD1SRCPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
MDSRCPPS0        EQU  H'0000'
MDSRCPPS1        EQU  H'0001'
MDSRCPPS2        EQU  H'0002'
MDSRCPPS3        EQU  H'0003'
MDSRCPPS4        EQU  H'0004'
MDSRCPPS0        EQU  H'0000'
MDSRCPPS1        EQU  H'0001'
MDSRCPPS2        EQU  H'0002'
MDSRCPPS3        EQU  H'0003'
MDSRCPPS4        EQU  H'0004'


;----- CLCIN0PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN0PPS0       EQU  H'0000'
CLCIN0PPS1       EQU  H'0001'
CLCIN0PPS2       EQU  H'0002'
CLCIN0PPS3       EQU  H'0003'
CLCIN0PPS4       EQU  H'0004'
CLCIN0PPS0       EQU  H'0000'
CLCIN0PPS1       EQU  H'0001'
CLCIN0PPS2       EQU  H'0002'
CLCIN0PPS3       EQU  H'0003'
CLCIN0PPS4       EQU  H'0004'


;----- CLCIN1PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN1PPS0       EQU  H'0000'
CLCIN1PPS1       EQU  H'0001'
CLCIN1PPS2       EQU  H'0002'
CLCIN1PPS3       EQU  H'0003'
CLCIN1PPS4       EQU  H'0004'
CLCIN1PPS0       EQU  H'0000'
CLCIN1PPS1       EQU  H'0001'
CLCIN1PPS2       EQU  H'0002'
CLCIN1PPS3       EQU  H'0003'
CLCIN1PPS4       EQU  H'0004'


;----- CLCIN2PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN2PPS0       EQU  H'0000'
CLCIN2PPS1       EQU  H'0001'
CLCIN2PPS2       EQU  H'0002'
CLCIN2PPS3       EQU  H'0003'
CLCIN2PPS4       EQU  H'0004'
CLCIN2PPS0       EQU  H'0000'
CLCIN2PPS1       EQU  H'0001'
CLCIN2PPS2       EQU  H'0002'
CLCIN2PPS3       EQU  H'0003'
CLCIN2PPS4       EQU  H'0004'


;----- CLCIN3PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN3PPS0       EQU  H'0000'
CLCIN3PPS1       EQU  H'0001'
CLCIN3PPS2       EQU  H'0002'
CLCIN3PPS3       EQU  H'0003'
CLCIN3PPS4       EQU  H'0004'
CLCIN3PPS0       EQU  H'0000'
CLCIN3PPS1       EQU  H'0001'
CLCIN3PPS2       EQU  H'0002'
CLCIN3PPS3       EQU  H'0003'
CLCIN3PPS4       EQU  H'0004'


;----- CLCIN4PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN4PPS0       EQU  H'0000'
CLCIN4PPS1       EQU  H'0001'
CLCIN4PPS2       EQU  H'0002'
CLCIN4PPS3       EQU  H'0003'
CLCIN4PPS4       EQU  H'0004'
CLCIN4PPS0       EQU  H'0000'
CLCIN4PPS1       EQU  H'0001'
CLCIN4PPS2       EQU  H'0002'
CLCIN4PPS3       EQU  H'0003'
CLCIN4PPS4       EQU  H'0004'


;----- CLCIN5PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN5PPS0       EQU  H'0000'
CLCIN5PPS1       EQU  H'0001'
CLCIN5PPS2       EQU  H'0002'
CLCIN5PPS3       EQU  H'0003'
CLCIN5PPS4       EQU  H'0004'
CLCIN5PPS0       EQU  H'0000'
CLCIN5PPS1       EQU  H'0001'
CLCIN5PPS2       EQU  H'0002'
CLCIN5PPS3       EQU  H'0003'
CLCIN5PPS4       EQU  H'0004'


;----- CLCIN6PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN6PPS0       EQU  H'0000'
CLCIN6PPS1       EQU  H'0001'
CLCIN6PPS2       EQU  H'0002'
CLCIN6PPS3       EQU  H'0003'
CLCIN6PPS4       EQU  H'0004'
CLCIN6PPS0       EQU  H'0000'
CLCIN6PPS1       EQU  H'0001'
CLCIN6PPS2       EQU  H'0002'
CLCIN6PPS3       EQU  H'0003'
CLCIN6PPS4       EQU  H'0004'


;----- CLCIN7PPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
CLCIN7PPS0       EQU  H'0000'
CLCIN7PPS1       EQU  H'0001'
CLCIN7PPS2       EQU  H'0002'
CLCIN7PPS3       EQU  H'0003'
CLCIN7PPS4       EQU  H'0004'
CLCIN7PPS0       EQU  H'0000'
CLCIN7PPS1       EQU  H'0001'
CLCIN7PPS2       EQU  H'0002'
CLCIN7PPS3       EQU  H'0003'
CLCIN7PPS4       EQU  H'0004'


;----- ADACTPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
ADACTPPS0        EQU  H'0000'
ADACTPPS1        EQU  H'0001'
ADACTPPS2        EQU  H'0002'
ADACTPPS3        EQU  H'0003'
ADACTPPS4        EQU  H'0004'
ADACTPPS0        EQU  H'0000'
ADACTPPS1        EQU  H'0001'
ADACTPPS2        EQU  H'0002'
ADACTPPS3        EQU  H'0003'
ADACTPPS4        EQU  H'0004'


;----- SPI1SCKPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI1SCKPPS0      EQU  H'0000'
SPI1SCKPPS1      EQU  H'0001'
SPI1SCKPPS2      EQU  H'0002'
SPI1SCKPPS3      EQU  H'0003'
SPI1SCKPPS4      EQU  H'0004'
SPI1SCKPPS0      EQU  H'0000'
SPI1SCKPPS1      EQU  H'0001'
SPI1SCKPPS2      EQU  H'0002'
SPI1SCKPPS3      EQU  H'0003'
SPI1SCKPPS4      EQU  H'0004'


;----- SPI1SDIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI1SDIPPS0      EQU  H'0000'
SPI1SDIPPS1      EQU  H'0001'
SPI1SDIPPS2      EQU  H'0002'
SPI1SDIPPS3      EQU  H'0003'
SPI1SDIPPS4      EQU  H'0004'
SPI1SDIPPS0      EQU  H'0000'
SPI1SDIPPS1      EQU  H'0001'
SPI1SDIPPS2      EQU  H'0002'
SPI1SDIPPS3      EQU  H'0003'
SPI1SDIPPS4      EQU  H'0004'


;----- SPI1SSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI1SSPPS0       EQU  H'0000'
SPI1SSPPS1       EQU  H'0001'
SPI1SSPPS2       EQU  H'0002'
SPI1SSPPS3       EQU  H'0003'
SPI1SSPPS4       EQU  H'0004'
SPI1SSPPS0       EQU  H'0000'
SPI1SSPPS1       EQU  H'0001'
SPI1SSPPS2       EQU  H'0002'
SPI1SSPPS3       EQU  H'0003'
SPI1SSPPS4       EQU  H'0004'


;----- SPI2SCKPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI2SCKPPS0      EQU  H'0000'
SPI2SCKPPS1      EQU  H'0001'
SPI2SCKPPS2      EQU  H'0002'
SPI2SCKPPS3      EQU  H'0003'
SPI2SCKPPS4      EQU  H'0004'
SPI2SCKPPS0      EQU  H'0000'
SPI2SCKPPS1      EQU  H'0001'
SPI2SCKPPS2      EQU  H'0002'
SPI2SCKPPS3      EQU  H'0003'
SPI2SCKPPS4      EQU  H'0004'


;----- SPI2SDIPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI2SDIPPS0      EQU  H'0000'
SPI2SDIPPS1      EQU  H'0001'
SPI2SDIPPS2      EQU  H'0002'
SPI2SDIPPS3      EQU  H'0003'
SPI2SDIPPS4      EQU  H'0004'
SPI2SDIPPS0      EQU  H'0000'
SPI2SDIPPS1      EQU  H'0001'
SPI2SDIPPS2      EQU  H'0002'
SPI2SDIPPS3      EQU  H'0003'
SPI2SDIPPS4      EQU  H'0004'


;----- SPI2SSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
SPI2SSPPS0       EQU  H'0000'
SPI2SSPPS1       EQU  H'0001'
SPI2SSPPS2       EQU  H'0002'
SPI2SSPPS3       EQU  H'0003'
SPI2SSPPS4       EQU  H'0004'
SPI2SSPPS0       EQU  H'0000'
SPI2SSPPS1       EQU  H'0001'
SPI2SSPPS2       EQU  H'0002'
SPI2SSPPS3       EQU  H'0003'
SPI2SSPPS4       EQU  H'0004'


;----- I2C1SDAPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
I2C1SDAPPS0      EQU  H'0000'
I2C1SDAPPS1      EQU  H'0001'
I2C1SDAPPS2      EQU  H'0002'
I2C1SDAPPS3      EQU  H'0003'
I2C1SDAPPS4      EQU  H'0004'
I2C1SDAPPS0      EQU  H'0000'
I2C1SDAPPS1      EQU  H'0001'
I2C1SDAPPS2      EQU  H'0002'
I2C1SDAPPS3      EQU  H'0003'
I2C1SDAPPS4      EQU  H'0004'


;----- I2C1SCLPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
I2C1SCLPPS0      EQU  H'0000'
I2C1SCLPPS1      EQU  H'0001'
I2C1SCLPPS2      EQU  H'0002'
I2C1SCLPPS3      EQU  H'0003'
I2C1SCLPPS4      EQU  H'0004'
I2C1SCLPPS0      EQU  H'0000'
I2C1SCLPPS1      EQU  H'0001'
I2C1SCLPPS2      EQU  H'0002'
I2C1SCLPPS3      EQU  H'0003'
I2C1SCLPPS4      EQU  H'0004'


;----- U1RXPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U1RXPPS0         EQU  H'0000'
U1RXPPS1         EQU  H'0001'
U1RXPPS2         EQU  H'0002'
U1RXPPS3         EQU  H'0003'
U1RXPPS4         EQU  H'0004'
U1RXPPS5         EQU  H'0005'
U1RXPPS0         EQU  H'0000'
U1RXPPS1         EQU  H'0001'
U1RXPPS2         EQU  H'0002'
U1RXPPS3         EQU  H'0003'
U1RXPPS4         EQU  H'0004'
U1RXPPS5         EQU  H'0005'


;----- U1CTSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U1CTSPPS0        EQU  H'0000'
U1CTSPPS1        EQU  H'0001'
U1CTSPPS2        EQU  H'0002'
U1CTSPPS3        EQU  H'0003'
U1CTSPPS4        EQU  H'0004'
U1CTSPPS5        EQU  H'0005'
U1CTSPPS0        EQU  H'0000'
U1CTSPPS1        EQU  H'0001'
U1CTSPPS2        EQU  H'0002'
U1CTSPPS3        EQU  H'0003'
U1CTSPPS4        EQU  H'0004'
U1CTSPPS5        EQU  H'0005'


;----- U2RXPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
U2RXPPS0         EQU  H'0000'
U2RXPPS1         EQU  H'0001'
U2RXPPS2         EQU  H'0002'
U2RXPPS3         EQU  H'0003'
U2RXPPS4         EQU  H'0004'
U2RXPPS0         EQU  H'0000'
U2RXPPS1         EQU  H'0001'
U2RXPPS2         EQU  H'0002'
U2RXPPS3         EQU  H'0003'
U2RXPPS4         EQU  H'0004'


;----- U2CTSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
U2CTSPPS0        EQU  H'0000'
U2CTSPPS1        EQU  H'0001'
U2CTSPPS2        EQU  H'0002'
U2CTSPPS3        EQU  H'0003'
U2CTSPPS4        EQU  H'0004'
U2CTSPPS0        EQU  H'0000'
U2CTSPPS1        EQU  H'0001'
U2CTSPPS2        EQU  H'0002'
U2CTSPPS3        EQU  H'0003'
U2CTSPPS4        EQU  H'0004'


;----- U3RXPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U3RXPPS0         EQU  H'0000'
U3RXPPS1         EQU  H'0001'
U3RXPPS2         EQU  H'0002'
U3RXPPS3         EQU  H'0003'
U3RXPPS4         EQU  H'0004'
U3RXPPS5         EQU  H'0005'
U3RXPPS0         EQU  H'0000'
U3RXPPS1         EQU  H'0001'
U3RXPPS2         EQU  H'0002'
U3RXPPS3         EQU  H'0003'
U3RXPPS4         EQU  H'0004'
U3RXPPS5         EQU  H'0005'


;----- U3CTSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U3CTSPPS0        EQU  H'0000'
U3CTSPPS1        EQU  H'0001'
U3CTSPPS2        EQU  H'0002'
U3CTSPPS3        EQU  H'0003'
U3CTSPPS4        EQU  H'0004'
U3CTSPPS5        EQU  H'0005'
U3CTSPPS0        EQU  H'0000'
U3CTSPPS1        EQU  H'0001'
U3CTSPPS2        EQU  H'0002'
U3CTSPPS3        EQU  H'0003'
U3CTSPPS4        EQU  H'0004'
U3CTSPPS5        EQU  H'0005'


;----- U4RXPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
U4RXPPS0         EQU  H'0000'
U4RXPPS1         EQU  H'0001'
U4RXPPS2         EQU  H'0002'
U4RXPPS3         EQU  H'0003'
U4RXPPS4         EQU  H'0004'
U4RXPPS0         EQU  H'0000'
U4RXPPS1         EQU  H'0001'
U4RXPPS2         EQU  H'0002'
U4RXPPS3         EQU  H'0003'
U4RXPPS4         EQU  H'0004'


;----- U4CTSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
U4CTSPPS0        EQU  H'0000'
U4CTSPPS1        EQU  H'0001'
U4CTSPPS2        EQU  H'0002'
U4CTSPPS3        EQU  H'0003'
U4CTSPPS4        EQU  H'0004'
U4CTSPPS0        EQU  H'0000'
U4CTSPPS1        EQU  H'0001'
U4CTSPPS2        EQU  H'0002'
U4CTSPPS3        EQU  H'0003'
U4CTSPPS4        EQU  H'0004'


;----- U5RXPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U5RXPPS0         EQU  H'0000'
U5RXPPS1         EQU  H'0001'
U5RXPPS2         EQU  H'0002'
U5RXPPS3         EQU  H'0003'
U5RXPPS4         EQU  H'0004'
U5RXPPS5         EQU  H'0005'
U5RXPPS0         EQU  H'0000'
U5RXPPS1         EQU  H'0001'
U5RXPPS2         EQU  H'0002'
U5RXPPS3         EQU  H'0003'
U5RXPPS4         EQU  H'0004'
U5RXPPS5         EQU  H'0005'


;----- U5CTSPPS Bits -----------------------------------------------------
PIN0             EQU  H'0000'
PIN1             EQU  H'0001'
PIN2             EQU  H'0002'
PORT0            EQU  H'0003'
PORT1            EQU  H'0004'
PORT2            EQU  H'0005'
U5CTSPPS0        EQU  H'0000'
U5CTSPPS1        EQU  H'0001'
U5CTSPPS2        EQU  H'0002'
U5CTSPPS3        EQU  H'0003'
U5CTSPPS4        EQU  H'0004'
U5CTSPPS5        EQU  H'0005'
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
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- RB1I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'
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
I2CTH0           EQU  H'0000'
I2CTH1           EQU  H'0001'
I2CPU0           EQU  H'0004'
I2CPU1           EQU  H'0005'
I2CSLEW0         EQU  H'0006'
I2CSLEW1         EQU  H'0007'


;----- I2C1RXB Bits -----------------------------------------------------


;----- I2C1TXB Bits -----------------------------------------------------


;----- I2C1CNT Bits -----------------------------------------------------
CNT0             EQU  H'0000'
CNT1             EQU  H'0001'
CNT2             EQU  H'0002'
CNT3             EQU  H'0003'
CNT4             EQU  H'0004'
CNT5             EQU  H'0005'
CNT6             EQU  H'0006'
CNT7             EQU  H'0007'


;----- I2C1ADB0 Bits -----------------------------------------------------


;----- I2C1ADB1 Bits -----------------------------------------------------


;----- I2C1ADR0 Bits -----------------------------------------------------


;----- I2C1ADR1 Bits -----------------------------------------------------
ADR0             EQU  H'0001'
ADR1             EQU  H'0002'
ADR2             EQU  H'0003'
ADR3             EQU  H'0004'
ADR4             EQU  H'0005'
ADR5             EQU  H'0006'
ADR6             EQU  H'0007'


;----- I2C1ADR2 Bits -----------------------------------------------------


;----- I2C1ADR3 Bits -----------------------------------------------------
ADR0             EQU  H'0001'
ADR1             EQU  H'0002'
ADR2             EQU  H'0003'
ADR3             EQU  H'0004'
ADR4             EQU  H'0005'
ADR5             EQU  H'0006'
ADR6             EQU  H'0007'


;----- I2C1CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
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
ACKT             EQU  H'0004'
ACKSTAT          EQU  H'0005'
ACKDT            EQU  H'0006'
ACKCNT           EQU  H'0007'


;----- I2C1CON2 Bits -----------------------------------------------------
BFRET0           EQU  H'0000'
BFRET1           EQU  H'0001'
SDAHT0           EQU  H'0002'
SDAHT1           EQU  H'0003'
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


;----- I2C1BTO Bits -----------------------------------------------------
BTO0             EQU  H'0000'
BTO1             EQU  H'0001'
BTO2             EQU  H'0002'
BTO3             EQU  H'0003'
I2CBTO0          EQU  H'0000'
I2CBTO1          EQU  H'0001'
I2CBTO2          EQU  H'0002'
I2CBTO3          EQU  H'0003'


;----- U1RXB Bits -----------------------------------------------------


;----- U1RXCHK Bits -----------------------------------------------------


;----- U1TXB Bits -----------------------------------------------------


;----- U1TXCHK Bits -----------------------------------------------------


;----- U1P1 Bits -----------------------------------------------------


;----- U1P1H Bits -----------------------------------------------------
P1H              EQU  H'0000'


;----- U1P2 Bits -----------------------------------------------------


;----- U1P2H Bits -----------------------------------------------------
P2H              EQU  H'0000'


;----- U1P3 Bits -----------------------------------------------------


;----- U1P3H Bits -----------------------------------------------------
P3H              EQU  H'0000'


;----- U1CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
U1MODE0          EQU  H'0000'
U1MODE1          EQU  H'0001'
U1MODE2          EQU  H'0002'
U1MODE3          EQU  H'0003'
U1RXEN           EQU  H'0004'
U1TXEN           EQU  H'0005'
U1ABDEN          EQU  H'0006'
U1BRGS           EQU  H'0007'
U1MODE0          EQU  H'0000'
U1MODE1          EQU  H'0001'
U1MODE2          EQU  H'0002'
U1MODE3          EQU  H'0003'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


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


;----- U1CON2 Bits -----------------------------------------------------
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
TXPOL            EQU  H'0002'
C0EN             EQU  H'0003'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U1FLO0           EQU  H'0000'
U1FLO1           EQU  H'0001'
U1TXPOL          EQU  H'0002'
U1C0EN           EQU  H'0003'
U1STP0           EQU  H'0004'
U1STP1           EQU  H'0005'
U1RXPOL          EQU  H'0006'
U1RUNOVF         EQU  H'0007'
U1FLO0           EQU  H'0000'
U1FLO1           EQU  H'0001'
U1STP0           EQU  H'0004'
U1STP1           EQU  H'0005'


;----- U1BRG Bits -----------------------------------------------------


;----- U1BRGH Bits -----------------------------------------------------


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


;----- U2RXB Bits -----------------------------------------------------


;----- U2TXB Bits -----------------------------------------------------


;----- U2P1 Bits -----------------------------------------------------


;----- U2P2 Bits -----------------------------------------------------


;----- U2P3 Bits -----------------------------------------------------


;----- U2CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U2MODE0          EQU  H'0000'
U2MODE1          EQU  H'0001'
U2MODE2          EQU  H'0002'
U2MODE3          EQU  H'0003'
U2RXEN           EQU  H'0004'
U2TXEN           EQU  H'0005'
U2ABDEN          EQU  H'0006'
U2BRGS           EQU  H'0007'
U2MODE0          EQU  H'0000'
U2MODE1          EQU  H'0001'
U2MODE2          EQU  H'0002'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
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
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
TXPOL            EQU  H'0002'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U2FLO0           EQU  H'0000'
U2FLO1           EQU  H'0001'
U2TXPOL          EQU  H'0002'
U2STP0           EQU  H'0004'
U2STP1           EQU  H'0005'
U2RXPOL          EQU  H'0006'
U2RUNOVF         EQU  H'0007'
U2FLO0           EQU  H'0000'
U2FLO1           EQU  H'0001'
U2STP0           EQU  H'0004'
U2STP1           EQU  H'0005'


;----- U2BRG Bits -----------------------------------------------------


;----- U2BRGH Bits -----------------------------------------------------


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


;----- U3RXB Bits -----------------------------------------------------


;----- U3TXB Bits -----------------------------------------------------


;----- U3P1 Bits -----------------------------------------------------


;----- U3P2 Bits -----------------------------------------------------


;----- U3P3 Bits -----------------------------------------------------


;----- U3CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U3MODE0          EQU  H'0000'
U3MODE1          EQU  H'0001'
U3MODE2          EQU  H'0002'
U3MODE3          EQU  H'0003'
U3RXEN           EQU  H'0004'
U3TXEN           EQU  H'0005'
U3ABDEN          EQU  H'0006'
U3BRGS           EQU  H'0007'
U3MODE0          EQU  H'0000'
U3MODE1          EQU  H'0001'
U3MODE2          EQU  H'0002'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


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


;----- U3CON2 Bits -----------------------------------------------------
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
TXPOL            EQU  H'0002'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U3FLO0           EQU  H'0000'
U3FLO1           EQU  H'0001'
U3TXPOL          EQU  H'0002'
U3STP0           EQU  H'0004'
U3STP1           EQU  H'0005'
U3RXPOL          EQU  H'0006'
U3RUNOVF         EQU  H'0007'
U3FLO0           EQU  H'0000'
U3FLO1           EQU  H'0001'
U3STP0           EQU  H'0004'
U3STP1           EQU  H'0005'


;----- U3BRG Bits -----------------------------------------------------


;----- U3BRGH Bits -----------------------------------------------------


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


;----- U4RXB Bits -----------------------------------------------------


;----- U4TXB Bits -----------------------------------------------------


;----- U4P1 Bits -----------------------------------------------------


;----- U4P2 Bits -----------------------------------------------------


;----- U4P3 Bits -----------------------------------------------------


;----- U4CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U4MODE0          EQU  H'0000'
U4MODE1          EQU  H'0001'
U4MODE2          EQU  H'0002'
U4MODE3          EQU  H'0003'
U4RXEN           EQU  H'0004'
U4TXEN           EQU  H'0005'
U4ABDEN          EQU  H'0006'
U4BRGS           EQU  H'0007'
U4MODE0          EQU  H'0000'
U4MODE1          EQU  H'0001'
U4MODE2          EQU  H'0002'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


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


;----- U4CON2 Bits -----------------------------------------------------
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
TXPOL            EQU  H'0002'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U4FLO0           EQU  H'0000'
U4FLO1           EQU  H'0001'
U4TXPOL          EQU  H'0002'
U4STP0           EQU  H'0004'
U4STP1           EQU  H'0005'
U4RXPOL          EQU  H'0006'
U4RUNOVF         EQU  H'0007'
U4FLO0           EQU  H'0000'
U4FLO1           EQU  H'0001'
U4STP0           EQU  H'0004'
U4STP1           EQU  H'0005'


;----- U4BRG Bits -----------------------------------------------------


;----- U4BRGH Bits -----------------------------------------------------


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


;----- U5RXB Bits -----------------------------------------------------


;----- U5TXB Bits -----------------------------------------------------


;----- U5P1 Bits -----------------------------------------------------


;----- U5P2 Bits -----------------------------------------------------


;----- U5P3 Bits -----------------------------------------------------


;----- U5CON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
U5MODE0          EQU  H'0000'
U5MODE1          EQU  H'0001'
U5MODE2          EQU  H'0002'
U5MODE3          EQU  H'0003'
U5RXEN           EQU  H'0004'
U5TXEN           EQU  H'0005'
U5ABDEN          EQU  H'0006'
U5BRGS           EQU  H'0007'
U5MODE0          EQU  H'0000'
U5MODE1          EQU  H'0001'
U5MODE2          EQU  H'0002'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
RXEN             EQU  H'0004'
TXEN             EQU  H'0005'
ABDEN            EQU  H'0006'
BRGS             EQU  H'0007'


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


;----- U5CON2 Bits -----------------------------------------------------
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
TXPOL            EQU  H'0002'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
RXPOL            EQU  H'0006'
RUNOVF           EQU  H'0007'
FLO0             EQU  H'0000'
FLO1             EQU  H'0001'
STP0             EQU  H'0004'
STP1             EQU  H'0005'
U5FLO0           EQU  H'0000'
U5FLO1           EQU  H'0001'
U5TXPOL          EQU  H'0002'
U5STP0           EQU  H'0004'
U5STP1           EQU  H'0005'
U5RXPOL          EQU  H'0006'
U5RUNOVF         EQU  H'0007'
U5FLO0           EQU  H'0000'
U5FLO1           EQU  H'0001'
U5STP0           EQU  H'0004'
U5STP1           EQU  H'0005'


;----- U5BRG Bits -----------------------------------------------------


;----- U5BRGH Bits -----------------------------------------------------


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
SMT1TMR0         EQU  H'0000'
SMT1TMR1         EQU  H'0001'
SMT1TMR2         EQU  H'0002'
SMT1TMR3         EQU  H'0003'
SMT1TMR4         EQU  H'0004'
SMT1TMR5         EQU  H'0005'
SMT1TMR6         EQU  H'0006'
SMT1TMR7         EQU  H'0007'


;----- SMT1TMRH Bits -----------------------------------------------------
SMT1TMR8         EQU  H'0000'
SMT1TMR9         EQU  H'0001'
SMT1TMR10        EQU  H'0002'
SMT1TMR11        EQU  H'0003'
SMT1TMR12        EQU  H'0004'
SMT1TMR13        EQU  H'0005'
SMT1TMR14        EQU  H'0006'
SMT1TMR15        EQU  H'0007'


;----- SMT1TMRU Bits -----------------------------------------------------
SMT1TMR16        EQU  H'0000'
SMT1TMR17        EQU  H'0001'
SMT1TMR18        EQU  H'0002'
SMT1TMR19        EQU  H'0003'
SMT1TMR20        EQU  H'0004'
SMT1TMR21        EQU  H'0005'
SMT1TMR22        EQU  H'0006'
SMT1TMR23        EQU  H'0007'


;----- SMT1CPR Bits -----------------------------------------------------
SMT1CPR0         EQU  H'0000'
SMT1CPR1         EQU  H'0001'
SMT1CPR2         EQU  H'0002'
SMT1CPR3         EQU  H'0003'
SMT1CPR4         EQU  H'0004'
SMT1CPR5         EQU  H'0005'
SMT1CPR6         EQU  H'0006'
SMT1CPR7         EQU  H'0007'


;----- SMT1CPRH Bits -----------------------------------------------------
SMT1CPR8         EQU  H'0000'
SMT1CPR9         EQU  H'0001'
SMT1CPR10        EQU  H'0002'
SMT1CPR11        EQU  H'0003'
SMT1CPR12        EQU  H'0004'
SMT1CPR13        EQU  H'0005'
SMT1CPR14        EQU  H'0006'
SMT1CPR15        EQU  H'0007'


;----- SMT1CPRU Bits -----------------------------------------------------
SMT1CPR16        EQU  H'0000'
SMT1CPR17        EQU  H'0001'
SMT1CPR18        EQU  H'0002'
SMT1CPR19        EQU  H'0003'
SMT1CPR20        EQU  H'0004'
SMT1CPR21        EQU  H'0005'
SMT1CPR22        EQU  H'0006'
SMT1CPR23        EQU  H'0007'


;----- SMT1CPW Bits -----------------------------------------------------
SMT1CPW0         EQU  H'0000'
SMT1CPW1         EQU  H'0001'
SMT1CPW2         EQU  H'0002'
SMT1CPW3         EQU  H'0003'
SMT1CPW4         EQU  H'0004'
SMT1CPW5         EQU  H'0005'
SMT1CPW6         EQU  H'0006'
SMT1CPW7         EQU  H'0007'


;----- SMT1CPWH Bits -----------------------------------------------------
SMT1CPW8         EQU  H'0000'
SMT1CPW9         EQU  H'0001'
SMT1CPW10        EQU  H'0002'
SMT1CPW11        EQU  H'0003'
SMT1CPW12        EQU  H'0004'
SMT1CPW13        EQU  H'0005'
SMT1CPW14        EQU  H'0006'
SMT1CPW15        EQU  H'0007'


;----- SMT1CPWU Bits -----------------------------------------------------
SMT1CPW16        EQU  H'0000'
SMT1CPW17        EQU  H'0001'
SMT1CPW18        EQU  H'0002'
SMT1CPW19        EQU  H'0003'
SMT1CPW20        EQU  H'0004'
SMT1CPW21        EQU  H'0005'
SMT1CPW22        EQU  H'0006'
SMT1CPW23        EQU  H'0007'


;----- SMT1PR Bits -----------------------------------------------------
SMT1PR0          EQU  H'0000'
SMT1PR1          EQU  H'0001'
SMT1PR2          EQU  H'0002'
SMT1PR3          EQU  H'0003'
SMT1PR4          EQU  H'0004'
SMT1PR5          EQU  H'0005'
SMT1PR6          EQU  H'0006'
SMT1PR7          EQU  H'0007'


;----- SMT1PRH Bits -----------------------------------------------------
SMT1PR8          EQU  H'0000'
SMT1PR9          EQU  H'0001'
SMT1PR10         EQU  H'0002'
SMT1PR11         EQU  H'0003'
SMT1PR12         EQU  H'0004'
SMT1PR13         EQU  H'0005'
SMT1PR14         EQU  H'0006'
SMT1PR15         EQU  H'0007'


;----- SMT1PRU Bits -----------------------------------------------------
SMT1PR16         EQU  H'0000'
SMT1PR17         EQU  H'0001'
SMT1PR18         EQU  H'0002'
SMT1PR19         EQU  H'0003'
SMT1PR20         EQU  H'0004'
SMT1PR21         EQU  H'0005'
SMT1PR22         EQU  H'0006'
SMT1PR23         EQU  H'0007'


;----- SMT1CON0 Bits -----------------------------------------------------
SMT1PS0          EQU  H'0000'
SMT1PS1          EQU  H'0001'
CPOL             EQU  H'0002'
SPOL             EQU  H'0003'
WPOL             EQU  H'0004'
STP              EQU  H'0005'
EN               EQU  H'0007'
SMT1PS0          EQU  H'0000'
SMT1PS1          EQU  H'0001'


;----- SMT1CON1 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
REPEAT           EQU  H'0006'
SMT1GO           EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
SMT1MODE0        EQU  H'0000'
SMT1MODE1        EQU  H'0001'
SMT1MODE2        EQU  H'0002'
SMT1MODE3        EQU  H'0003'
SMT1REPEAT       EQU  H'0006'
SMT1MODE0        EQU  H'0000'
SMT1MODE1        EQU  H'0001'
SMT1MODE2        EQU  H'0002'
SMT1MODE3        EQU  H'0003'


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


;----- SMT1CLK Bits -----------------------------------------------------
CSEL0            EQU  H'0000'
CSEL1            EQU  H'0001'
CSEL2            EQU  H'0002'
CSEL3            EQU  H'0003'
SMT1CSEL0        EQU  H'0000'
SMT1CSEL1        EQU  H'0001'
SMT1CSEL2        EQU  H'0002'
SMT1CSEL3        EQU  H'0003'


;----- SMT1SIG Bits -----------------------------------------------------
SSEL0            EQU  H'0000'
SSEL1            EQU  H'0001'
SSEL2            EQU  H'0002'
SSEL3            EQU  H'0003'
SSEL4            EQU  H'0004'
SSEL5            EQU  H'0005'
SMT1SSEL0        EQU  H'0000'
SMT1SSEL1        EQU  H'0001'
SMT1SSEL2        EQU  H'0002'
SMT1SSEL3        EQU  H'0003'
SMT1SSEL4        EQU  H'0004'
SMT1SSEL5        EQU  H'0005'


;----- SMT1WIN Bits -----------------------------------------------------
WSEL0            EQU  H'0000'
WSEL1            EQU  H'0001'
WSEL2            EQU  H'0002'
WSEL3            EQU  H'0003'
WSEL4            EQU  H'0004'
WSEL5            EQU  H'0005'
SMT1WSEL0        EQU  H'0000'
SMT1WSEL1        EQU  H'0001'
SMT1WSEL2        EQU  H'0002'
SMT1WSEL3        EQU  H'0003'
SMT1WSEL4        EQU  H'0004'
SMT1WSEL5        EQU  H'0005'


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
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
MD16             EQU  H'0004'
OUT_T0CON0       EQU  H'0005'
EN               EQU  H'0007'
T0OUTPS0         EQU  H'0000'
T0OUTPS1         EQU  H'0001'
T0OUTPS2         EQU  H'0002'
T0OUTPS3         EQU  H'0003'
T0MD16           EQU  H'0004'
T0OUT            EQU  H'0005'
T0EN             EQU  H'0007'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
T016BIT          EQU  H'0004'
T0OUTPS0         EQU  H'0000'
T0OUTPS1         EQU  H'0001'
T0OUTPS2         EQU  H'0002'
T0OUTPS3         EQU  H'0003'


;----- T0CON1 Bits -----------------------------------------------------
CKPS0_T0CON1     EQU  H'0000'
CKPS1_T0CON1     EQU  H'0001'
CKPS2_T0CON1     EQU  H'0002'
CKPS3            EQU  H'0003'
ASYNC            EQU  H'0004'
CS0_T0CON1       EQU  H'0005'
CS1_T0CON1       EQU  H'0006'
CS2_T0CON1       EQU  H'0007'
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
T0CKPS0          EQU  H'0000'
T0CKPS1          EQU  H'0001'
T0CKPS2          EQU  H'0002'
T0CKPS3          EQU  H'0003'
T0CS0            EQU  H'0005'
T0CS1            EQU  H'0006'
T0CS2            EQU  H'0007'


;----- TMR1 Bits -----------------------------------------------------
TMR1L0           EQU  H'0000'
TMR1L1           EQU  H'0001'
TMR1L2           EQU  H'0002'
TMR1L3           EQU  H'0003'
TMR1L4           EQU  H'0004'
TMR1L5           EQU  H'0005'
TMR1L6           EQU  H'0006'
TMR1L7           EQU  H'0007'
TMR10            EQU  H'0000'
TMR11            EQU  H'0001'
TMR12            EQU  H'0002'
TMR13            EQU  H'0003'
TMR14            EQU  H'0004'
TMR15            EQU  H'0005'
TMR16            EQU  H'0006'
TMR17            EQU  H'0007'


;----- TMR1H Bits -----------------------------------------------------
TMR1H0           EQU  H'0000'
TMR1H1           EQU  H'0001'
TMR1H2           EQU  H'0002'
TMR1H3           EQU  H'0003'
TMR1H4           EQU  H'0004'
TMR1H5           EQU  H'0005'
TMR1H6           EQU  H'0006'
TMR1H7           EQU  H'0007'
TMR18            EQU  H'0000'
TMR19            EQU  H'0001'
TMR110           EQU  H'0002'
TMR111           EQU  H'0003'
TMR112           EQU  H'0004'
TMR113           EQU  H'0005'
TMR114           EQU  H'0006'
TMR115           EQU  H'0007'


;----- T1CON Bits -----------------------------------------------------
ON_T1CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
CKPS0_T1CON      EQU  H'0004'
CKPS1_T1CON      EQU  H'0005'
TMR1ON           EQU  H'0000'
T1RD16           EQU  H'0001'
NOT_T1SYNC       EQU  H'0002'
T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'
NSYNC            EQU  H'0002'
CKPS0_T1CON      EQU  H'0004'
CKPS1_T1CON      EQU  H'0005'
RD161            EQU  H'0001'
NT1SYNC          EQU  H'0002'


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
GGO_NOT_DONE     EQU  H'0003'
GGO_NDONE        EQU  H'0003'
T1GGO_NOT_DONE   EQU  H'0003'
T1GGO_NDONE      EQU  H'0003'


;----- T1GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
GSS5             EQU  H'0005'
T1GSS0           EQU  H'0000'
T1GSS1           EQU  H'0001'
T1GSS2           EQU  H'0002'
T1GSS3           EQU  H'0003'
T1GSS4           EQU  H'0004'
T1GSS5           EQU  H'0005'


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


;----- T2TMR Bits -----------------------------------------------------


;----- T2PR Bits -----------------------------------------------------


;----- T2CON Bits -----------------------------------------------------
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T2CON      EQU  H'0004'
CKPS1_T2CON      EQU  H'0005'
CKPS2_T2CON      EQU  H'0006'
ON_T2CON         EQU  H'0007'
T2OUTPS0         EQU  H'0000'
T2OUTPS1         EQU  H'0001'
T2OUTPS2         EQU  H'0002'
T2OUTPS3         EQU  H'0003'
T2CKPS0          EQU  H'0004'
T2CKPS1          EQU  H'0005'
T2CKPS2          EQU  H'0006'
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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T2MODE0          EQU  H'0000'
T2MODE1          EQU  H'0001'
T2MODE2          EQU  H'0002'
T2MODE3          EQU  H'0003'
T2MODE4          EQU  H'0004'
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
CS4              EQU  H'0004'
CS5              EQU  H'0005'
T2CS0            EQU  H'0000'
T2CS1            EQU  H'0001'
T2CS2            EQU  H'0002'
T2CS3            EQU  H'0003'
T2CS4            EQU  H'0004'
T2CS5            EQU  H'0005'


;----- T2RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
RSEL5            EQU  H'0005'
RSEL6            EQU  H'0006'
T2RSEL0          EQU  H'0000'
T2RSEL1          EQU  H'0001'
T2RSEL2          EQU  H'0002'
T2RSEL3          EQU  H'0003'
T2RSEL4          EQU  H'0004'
T2RSEL5          EQU  H'0005'
T2RSEL6          EQU  H'0006'


;----- TMR3 Bits -----------------------------------------------------
TMR3L0           EQU  H'0000'
TMR3L1           EQU  H'0001'
TMR3L2           EQU  H'0002'
TMR3L3           EQU  H'0003'
TMR3L4           EQU  H'0004'
TMR3L5           EQU  H'0005'
TMR3L6           EQU  H'0006'
TMR3L7           EQU  H'0007'
TMR30            EQU  H'0000'
TMR31            EQU  H'0001'
TMR32            EQU  H'0002'
TMR33            EQU  H'0003'
TMR34            EQU  H'0004'
TMR35            EQU  H'0005'
TMR36            EQU  H'0006'
TMR37            EQU  H'0007'


;----- TMR3H Bits -----------------------------------------------------
TMR3H0           EQU  H'0000'
TMR3H1           EQU  H'0001'
TMR3H2           EQU  H'0002'
TMR3H3           EQU  H'0003'
TMR3H4           EQU  H'0004'
TMR3H5           EQU  H'0005'
TMR3H6           EQU  H'0006'
TMR3H7           EQU  H'0007'
TMR38            EQU  H'0000'
TMR39            EQU  H'0001'
TMR310           EQU  H'0002'
TMR311           EQU  H'0003'
TMR312           EQU  H'0004'
TMR313           EQU  H'0005'
TMR314           EQU  H'0006'
TMR315           EQU  H'0007'


;----- T3CON Bits -----------------------------------------------------
ON_T3CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
CKPS0_T3CON      EQU  H'0004'
CKPS1_T3CON      EQU  H'0005'
TMR3ON           EQU  H'0000'
T3RD16           EQU  H'0001'
NOT_T3SYNC       EQU  H'0002'
T3CKPS0          EQU  H'0004'
T3CKPS1          EQU  H'0005'
NSYNC            EQU  H'0002'
CKPS0_T3CON      EQU  H'0004'
CKPS1_T3CON      EQU  H'0005'
RD163            EQU  H'0001'
NT3SYNC          EQU  H'0002'


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
GGO_NOT_DONE     EQU  H'0003'
GGO_NDONE        EQU  H'0003'
T3GGO_NOT_DONE   EQU  H'0003'
T3GGO_NDONE      EQU  H'0003'


;----- T3GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
GSS5             EQU  H'0005'
T3GSS0           EQU  H'0000'
T3GSS1           EQU  H'0001'
T3GSS2           EQU  H'0002'
T3GSS3           EQU  H'0003'
T3GSS4           EQU  H'0004'
T3GSS5           EQU  H'0005'


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


;----- T4TMR Bits -----------------------------------------------------


;----- T4PR Bits -----------------------------------------------------


;----- T4CON Bits -----------------------------------------------------
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T4CON      EQU  H'0004'
CKPS1_T4CON      EQU  H'0005'
CKPS2_T4CON      EQU  H'0006'
ON_T4CON         EQU  H'0007'
T4OUTPS0         EQU  H'0000'
T4OUTPS1         EQU  H'0001'
T4OUTPS2         EQU  H'0002'
T4OUTPS3         EQU  H'0003'
T4CKPS0          EQU  H'0004'
T4CKPS1          EQU  H'0005'
T4CKPS2          EQU  H'0006'
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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T4MODE0          EQU  H'0000'
T4MODE1          EQU  H'0001'
T4MODE2          EQU  H'0002'
T4MODE3          EQU  H'0003'
T4MODE4          EQU  H'0004'
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
CS4              EQU  H'0004'
CS5              EQU  H'0005'
T4CS0            EQU  H'0000'
T4CS1            EQU  H'0001'
T4CS2            EQU  H'0002'
T4CS3            EQU  H'0003'
T4CS4            EQU  H'0004'
T4CS5            EQU  H'0005'


;----- T4RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
RSEL5            EQU  H'0005'
RSEL6            EQU  H'0006'
T4RSEL0          EQU  H'0000'
T4RSEL1          EQU  H'0001'
T4RSEL2          EQU  H'0002'
T4RSEL3          EQU  H'0003'
T4RSEL4          EQU  H'0004'
T4RSEL5          EQU  H'0005'
T4RSEL6          EQU  H'0006'


;----- TMR5 Bits -----------------------------------------------------
TMR5L0           EQU  H'0000'
TMR5L1           EQU  H'0001'
TMR5L2           EQU  H'0002'
TMR5L3           EQU  H'0003'
TMR5L4           EQU  H'0004'
TMR5L5           EQU  H'0005'
TMR5L6           EQU  H'0006'
TMR5L7           EQU  H'0007'
TMR50            EQU  H'0000'
TMR51            EQU  H'0001'
TMR52            EQU  H'0002'
TMR53            EQU  H'0003'
TMR54            EQU  H'0004'
TMR55            EQU  H'0005'
TMR56            EQU  H'0006'
TMR57            EQU  H'0007'


;----- TMR5H Bits -----------------------------------------------------
TMR5H0           EQU  H'0000'
TMR5H1           EQU  H'0001'
TMR5H2           EQU  H'0002'
TMR5H3           EQU  H'0003'
TMR5H4           EQU  H'0004'
TMR5H5           EQU  H'0005'
TMR5H6           EQU  H'0006'
TMR5H7           EQU  H'0007'
TMR58            EQU  H'0000'
TMR59            EQU  H'0001'
TMR510           EQU  H'0002'
TMR511           EQU  H'0003'
TMR512           EQU  H'0004'
TMR513           EQU  H'0005'
TMR514           EQU  H'0006'
TMR515           EQU  H'0007'


;----- T5CON Bits -----------------------------------------------------
ON_T5CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'
CKPS0_T5CON      EQU  H'0004'
CKPS1_T5CON      EQU  H'0005'
TMR5ON           EQU  H'0000'
T5RD16           EQU  H'0001'
NOT_T5SYNC       EQU  H'0002'
T5CKPS0          EQU  H'0004'
T5CKPS1          EQU  H'0005'
NSYNC            EQU  H'0002'
CKPS0_T5CON      EQU  H'0004'
CKPS1_T5CON      EQU  H'0005'
RD165            EQU  H'0001'
NT5SYNC          EQU  H'0002'


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
GGO_NOT_DONE     EQU  H'0003'
GGO_NDONE        EQU  H'0003'
T5GGO_NOT_DONE   EQU  H'0003'
T5GGO_NDONE      EQU  H'0003'


;----- T5GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'
GSS4             EQU  H'0004'
GSS5             EQU  H'0005'
T5GSS0           EQU  H'0000'
T5GSS1           EQU  H'0001'
T5GSS2           EQU  H'0002'
T5GSS3           EQU  H'0003'
T5GSS4           EQU  H'0004'
T5GSS5           EQU  H'0005'


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


;----- T6TMR Bits -----------------------------------------------------


;----- T6PR Bits -----------------------------------------------------


;----- T6CON Bits -----------------------------------------------------
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T6CON      EQU  H'0004'
CKPS1_T6CON      EQU  H'0005'
CKPS2_T6CON      EQU  H'0006'
ON_T6CON         EQU  H'0007'
T6OUTPS0         EQU  H'0000'
T6OUTPS1         EQU  H'0001'
T6OUTPS2         EQU  H'0002'
T6OUTPS3         EQU  H'0003'
T6CKPS0          EQU  H'0004'
T6CKPS1          EQU  H'0005'
T6CKPS2          EQU  H'0006'
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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
T6MODE0          EQU  H'0000'
T6MODE1          EQU  H'0001'
T6MODE2          EQU  H'0002'
T6MODE3          EQU  H'0003'
T6MODE4          EQU  H'0004'
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
CS4              EQU  H'0004'
CS5              EQU  H'0005'
T6CS0            EQU  H'0000'
T6CS1            EQU  H'0001'
T6CS2            EQU  H'0002'
T6CS3            EQU  H'0003'
T6CS4            EQU  H'0004'
T6CS5            EQU  H'0005'


;----- T6RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
RSEL4            EQU  H'0004'
RSEL5            EQU  H'0005'
RSEL6            EQU  H'0006'
T6RSEL0          EQU  H'0000'
T6RSEL1          EQU  H'0001'
T6RSEL2          EQU  H'0002'
T6RSEL3          EQU  H'0003'
T6RSEL4          EQU  H'0004'
T6RSEL5          EQU  H'0005'
T6RSEL6          EQU  H'0006'


;----- CCPR1 Bits -----------------------------------------------------


;----- CCPR1H Bits -----------------------------------------------------


;----- CCP1CON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
FMT              EQU  H'0004'
OUT_CCP1CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP1MODE0        EQU  H'0000'
CCP1MODE1        EQU  H'0001'
CCP1MODE2        EQU  H'0002'
CCP1MODE3        EQU  H'0003'
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


;----- CCPR2 Bits -----------------------------------------------------


;----- CCPR2H Bits -----------------------------------------------------


;----- CCP2CON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
FMT              EQU  H'0004'
OUT_CCP2CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP2MODE0        EQU  H'0000'
CCP2MODE1        EQU  H'0001'
CCP2MODE2        EQU  H'0002'
CCP2MODE3        EQU  H'0003'
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


;----- CCPR3 Bits -----------------------------------------------------


;----- CCPR3H Bits -----------------------------------------------------


;----- CCP3CON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
FMT              EQU  H'0004'
OUT_CCP3CON      EQU  H'0005'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
CCP3MODE0        EQU  H'0000'
CCP3MODE1        EQU  H'0001'
CCP3MODE2        EQU  H'0002'
CCP3MODE3        EQU  H'0003'
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


;----- CRCDATH Bits -----------------------------------------------------
DATA8            EQU  H'0000'
DATA9            EQU  H'0001'
DATA10           EQU  H'0002'
DATA11           EQU  H'0003'
DATA12           EQU  H'0004'
DATA13           EQU  H'0005'
DATA14           EQU  H'0006'
DATA15           EQU  H'0007'


;----- CRCACC Bits -----------------------------------------------------
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- CRCACCH Bits -----------------------------------------------------
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- CRCSHFT Bits -----------------------------------------------------
SHFT0            EQU  H'0000'
SHFT1            EQU  H'0001'
SHFT2            EQU  H'0002'
SHFT3            EQU  H'0003'
SHFT4            EQU  H'0004'
SHFT5            EQU  H'0005'
SHFT6            EQU  H'0006'
SHFT7            EQU  H'0007'


;----- CRCSHIFTH Bits -----------------------------------------------------
SHFT8            EQU  H'0000'
SHFT9            EQU  H'0001'
SHFT10           EQU  H'0002'
SHFT11           EQU  H'0003'
SHFT12           EQU  H'0004'
SHFT13           EQU  H'0005'
SHFT14           EQU  H'0006'
SHFT15           EQU  H'0007'


;----- CRCXOR Bits -----------------------------------------------------
X1               EQU  H'0001'
X2               EQU  H'0002'
X3               EQU  H'0003'
X4               EQU  H'0004'
X5               EQU  H'0005'
X6               EQU  H'0006'
X7               EQU  H'0007'


;----- CRCXORH Bits -----------------------------------------------------
X8               EQU  H'0000'
X9               EQU  H'0001'
X10              EQU  H'0002'
X11              EQU  H'0003'
X12              EQU  H'0004'
X13              EQU  H'0005'
X14              EQU  H'0006'
X15              EQU  H'0007'


;----- CRCCON0 Bits -----------------------------------------------------
FULL             EQU  H'0000'
SHIFTM           EQU  H'0001'
ACCM             EQU  H'0004'
BUSY_CRCCON0     EQU  H'0005'
CRCGO            EQU  H'0006'
EN               EQU  H'0007'
CRCEN            EQU  H'0007'


;----- CRCCON1 Bits -----------------------------------------------------
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
DLEN0            EQU  H'0004'
DLEN1            EQU  H'0005'
DLEN2            EQU  H'0006'
DLEN3            EQU  H'0007'
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
DLEN0            EQU  H'0004'
DLEN1            EQU  H'0005'
DLEN2            EQU  H'0006'
DLEN3            EQU  H'0007'


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
LADR0            EQU  H'0000'
LADR1            EQU  H'0001'
LADR2            EQU  H'0002'
LADR3            EQU  H'0003'
LADR4            EQU  H'0004'
LADR5            EQU  H'0005'
LADR16           EQU  H'0000'
LADR17           EQU  H'0001'
LADR18           EQU  H'0002'
LADR19           EQU  H'0003'
LADR20           EQU  H'0004'
LADR21           EQU  H'0005'
SCANLADR0        EQU  H'0000'
SCANLADR1        EQU  H'0001'
SCANLADR2        EQU  H'0002'
SCANLADR3        EQU  H'0003'
SCANLADR4        EQU  H'0004'
SCANLADR5        EQU  H'0005'
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
HADR0            EQU  H'0000'
HADR1            EQU  H'0001'
HADR2            EQU  H'0002'
HADR3            EQU  H'0003'
HADR4            EQU  H'0004'
HADR5            EQU  H'0005'
HADR16           EQU  H'0000'
HADR17           EQU  H'0001'
HADR18           EQU  H'0002'
HADR19           EQU  H'0003'
HADR20           EQU  H'0004'
HADR21           EQU  H'0005'
SCANHADR0        EQU  H'0000'
SCANHADR1        EQU  H'0001'
SCANHADR2        EQU  H'0002'
SCANHADR3        EQU  H'0003'
SCANHADR4        EQU  H'0004'
SCANHADR5        EQU  H'0005'
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
CLC1IP           EQU  H'0005'
IOCIP            EQU  H'0007'


;----- IPR1 Bits -----------------------------------------------------
INT0IP           EQU  H'0000'
ZCDIP            EQU  H'0001'
ADIP             EQU  H'0002'
ACTIP            EQU  H'0003'
C1IP             EQU  H'0004'
SMT1IP           EQU  H'0005'
SMT1PRAIP        EQU  H'0006'
SMT1PWAIP        EQU  H'0007'


;----- IPR2 Bits -----------------------------------------------------
ADTIP            EQU  H'0000'
DMA1SCNTIP       EQU  H'0004'
DMA1DCNTIP       EQU  H'0005'
DMA1ORIP         EQU  H'0006'
DMA1AIP          EQU  H'0007'


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
PWM1PIP          EQU  H'0006'
PWM1IP           EQU  H'0007'


;----- IPR5 Bits -----------------------------------------------------
SPI2RXIP         EQU  H'0000'
SPI2TXIP         EQU  H'0001'
SPI2IP           EQU  H'0002'
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


;----- IPR10 Bits -----------------------------------------------------
INT2IP           EQU  H'0000'
CLC5IP           EQU  H'0001'
CWG2IP           EQU  H'0002'
NCO2IP           EQU  H'0003'
DMA3SCNTIP       EQU  H'0004'
DMA3DCNTIP       EQU  H'0005'
DMA3ORIP         EQU  H'0006'
DMA3AIP          EQU  H'0007'


;----- IPR11 Bits -----------------------------------------------------
CCP3IP           EQU  H'0000'
CLC6IP           EQU  H'0001'
CWG3IP           EQU  H'0002'
TMR4IP           EQU  H'0003'
DMA4SCNTIP       EQU  H'0004'
DMA4DCNTIP       EQU  H'0005'
DMA4ORIP         EQU  H'0006'
DMA4AIP          EQU  H'0007'


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


;----- IPR15 Bits -----------------------------------------------------
NVMIP            EQU  H'0000'
CLC8IP           EQU  H'0001'
CRCIP            EQU  H'0002'
TMR6IP           EQU  H'0003'


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


;----- WREG_CSHAD Bits -----------------------------------------------------


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


;----- WREG_SHAD Bits -----------------------------------------------------


;----- PCLATH_SHAD Bits -----------------------------------------------------


;----- PCLATU_SHAD Bits -----------------------------------------------------
PCU0             EQU  H'0000'
PCU1             EQU  H'0001'
PCU2             EQU  H'0002'
PCU3             EQU  H'0003'
PCU4             EQU  H'0004'


;----- FSR0SH Bits -----------------------------------------------------
FSR0SH0          EQU  H'0000'
FSR0SH1          EQU  H'0001'
FSR0SH2          EQU  H'0002'
FSR0SH3          EQU  H'0003'
FSR0SH4          EQU  H'0004'
FSR0SH5          EQU  H'0005'
FSR0SH6          EQU  H'0006'
FSR0SH7          EQU  H'0007'
FSR0SH8          EQU  H'0008'
FSR0SH9          EQU  H'0009'
FSR0SH10         EQU  H'0010'
FSR0SH11         EQU  H'0011'
FSR0SH12         EQU  H'0012'
FSR0SH13         EQU  H'0013'


;----- FSR0H_SHAD Bits -----------------------------------------------------
FSR0H0           EQU  H'0000'
FSR0H1           EQU  H'0001'
FSR0H2           EQU  H'0002'
FSR0H3           EQU  H'0003'
FSR0H4           EQU  H'0004'
FSR0H5           EQU  H'0005'


;----- FSR1SH Bits -----------------------------------------------------
FSR1SH0          EQU  H'0000'
FSR1SH1          EQU  H'0001'
FSR1SH2          EQU  H'0002'
FSR1SH3          EQU  H'0003'
FSR1SH4          EQU  H'0004'
FSR1SH5          EQU  H'0005'
FSR1SH6          EQU  H'0006'
FSR1SH7          EQU  H'0007'
FSR1SH8          EQU  H'0008'
FSR1SH9          EQU  H'0009'
FSR1SH10         EQU  H'0010'
FSR1SH11         EQU  H'0011'
FSR1SH12         EQU  H'0012'
FSR1SH13         EQU  H'0013'


;----- FSR1H_SHAD Bits -----------------------------------------------------
FSR1H0           EQU  H'0000'
FSR1H1           EQU  H'0001'
FSR1H2           EQU  H'0002'
FSR1H3           EQU  H'0003'
FSR1H4           EQU  H'0004'
FSR1H5           EQU  H'0005'


;----- FSR2SH Bits -----------------------------------------------------
FSR2SH0          EQU  H'0000'
FSR2SH1          EQU  H'0001'
FSR2SH2          EQU  H'0002'
FSR2SH3          EQU  H'0003'
FSR2SH4          EQU  H'0004'
FSR2SH5          EQU  H'0005'
FSR2SH6          EQU  H'0006'
FSR2SH7          EQU  H'0007'
FSR2SH8          EQU  H'0008'
FSR2SH9          EQU  H'0009'
FSR2SH10         EQU  H'0010'
FSR2SH11         EQU  H'0011'
FSR2SH12         EQU  H'0012'
FSR2SH13         EQU  H'0013'


;----- FSR2H_SHAD Bits -----------------------------------------------------
FSR2H0           EQU  H'0000'
FSR2H1           EQU  H'0001'
FSR2H2           EQU  H'0002'
FSR2H3           EQU  H'0003'
FSR2H4           EQU  H'0004'
FSR2H5           EQU  H'0005'


;----- PRODSH Bits -----------------------------------------------------
PROD0            EQU  H'0000'
PROD1            EQU  H'0001'
PROD2            EQU  H'0002'
PROD3            EQU  H'0003'
PROD4            EQU  H'0004'
PROD5            EQU  H'0005'
PROD6            EQU  H'0006'
PROD7            EQU  H'0007'
PROD8            EQU  H'0008'
PROD9            EQU  H'0009'
PROD10           EQU  H'0010'
PROD11           EQU  H'0011'
PROD12           EQU  H'0012'
PROD13           EQU  H'0013'
PROD14           EQU  H'0014'
PROD15           EQU  H'0015'


;----- PRODH_SHAD Bits -----------------------------------------------------


;----- CWG1CLK Bits -----------------------------------------------------
CS_CWG1CLK       EQU  H'0000'
CWG1CS           EQU  H'0000'


;----- CWG1ISM Bits -----------------------------------------------------
ISM0             EQU  H'0000'
ISM1             EQU  H'0001'
ISM2             EQU  H'0002'
ISM3             EQU  H'0003'
ISM4             EQU  H'0004'


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
LD_CWG1CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G1EN             EQU  H'0007'
CWG1MODE0        EQU  H'0000'
CWG1MODE1        EQU  H'0001'
CWG1MODE2        EQU  H'0002'
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
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSDBD0           EQU  H'0004'
LSDBD1           EQU  H'0005'
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSCA1            EQU  H'0003'
LSDB0            EQU  H'0004'
LSDB1            EQU  H'0005'
CWG1LSAC0        EQU  H'0002'
CWG1LSAC1        EQU  H'0003'
CWG1LSDB0        EQU  H'0004'
CWG1LSDB1        EQU  H'0005'
CWG1LSAC0        EQU  H'0002'
CWG1LSAC1        EQU  H'0003'
CWG1LSDB0        EQU  H'0004'
CWG1LSDB1        EQU  H'0005'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG1LSBD0        EQU  H'0004'
CWG1LSBD1        EQU  H'0005'
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


;----- CWG2CLK Bits -----------------------------------------------------
CS_CWG2CLK       EQU  H'0000'
CWG2CS           EQU  H'0000'


;----- CWG2ISM Bits -----------------------------------------------------
ISM0             EQU  H'0000'
ISM1             EQU  H'0001'
ISM2             EQU  H'0002'
ISM3             EQU  H'0003'
ISM4             EQU  H'0004'


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
LD_CWG2CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G2EN             EQU  H'0007'
CWG2MODE0        EQU  H'0000'
CWG2MODE1        EQU  H'0001'
CWG2MODE2        EQU  H'0002'
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
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSDBD0           EQU  H'0004'
LSDBD1           EQU  H'0005'
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSCA1            EQU  H'0003'
LSDB0            EQU  H'0004'
LSDB1            EQU  H'0005'
CWG2LSAC0        EQU  H'0002'
CWG2LSAC1        EQU  H'0003'
CWG2LSDB0        EQU  H'0004'
CWG2LSDB1        EQU  H'0005'
CWG2LSAC0        EQU  H'0002'
CWG2LSAC1        EQU  H'0003'
CWG2LSDB0        EQU  H'0004'
CWG2LSDB1        EQU  H'0005'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG2LSBD0        EQU  H'0004'
CWG2LSBD1        EQU  H'0005'
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
ISM0             EQU  H'0000'
ISM1             EQU  H'0001'
ISM2             EQU  H'0002'
ISM3             EQU  H'0003'
ISM4             EQU  H'0004'


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
LD_CWG3CON0      EQU  H'0006'
EN               EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
G3EN             EQU  H'0007'
CWG3MODE0        EQU  H'0000'
CWG3MODE1        EQU  H'0001'
CWG3MODE2        EQU  H'0002'
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
LSAC0            EQU  H'0002'
LSAC1            EQU  H'0003'
LSDBD0           EQU  H'0004'
LSDBD1           EQU  H'0005'
REN              EQU  H'0006'
SHUTDOWN         EQU  H'0007'
LSAC0            EQU  H'0002'
LSCA1            EQU  H'0003'
LSDB0            EQU  H'0004'
LSDB1            EQU  H'0005'
CWG3LSAC0        EQU  H'0002'
CWG3LSAC1        EQU  H'0003'
CWG3LSDB0        EQU  H'0004'
CWG3LSDB1        EQU  H'0005'
CWG3LSAC0        EQU  H'0002'
CWG3LSAC1        EQU  H'0003'
CWG3LSDB0        EQU  H'0004'
CWG3LSDB1        EQU  H'0005'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
LSAC1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
CWG3LSBD0        EQU  H'0004'
CWG3LSBD1        EQU  H'0005'
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
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
CDAFVR0          EQU  H'0002'
CDAFVR1          EQU  H'0003'
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
ADACC18          EQU  H'0002'
ADACC19          EQU  H'0003'
ADACC20          EQU  H'0004'
ADACC21          EQU  H'0005'
ADACC22          EQU  H'0006'
ADACC23          EQU  H'0007'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
ACC20            EQU  H'0004'
ACC21            EQU  H'0005'
ACC22            EQU  H'0006'
ACC23            EQU  H'0007'


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
ACQ0             EQU  H'0000'
ACQ1             EQU  H'0001'
ACQ2             EQU  H'0002'
ACQ3             EQU  H'0003'
ACQ4             EQU  H'0004'
ADACQ8           EQU  H'0000'
ADACQ9           EQU  H'0001'
ADACQ10          EQU  H'0002'
ADACQ11          EQU  H'0003'
ADACQ12          EQU  H'0004'
ADACQ0           EQU  H'0000'
ADACQ1           EQU  H'0001'
ADACQ2           EQU  H'0002'
ADACQ3           EQU  H'0003'
ADACQ4           EQU  H'0004'
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
PRE0             EQU  H'0000'
PRE1             EQU  H'0001'
PRE2             EQU  H'0002'
PRE3             EQU  H'0003'
PRE4             EQU  H'0004'
PRE8             EQU  H'0000'
PRE9             EQU  H'0001'
PRE10            EQU  H'0002'
PRE11            EQU  H'0003'
PRE12            EQU  H'0004'
ADPRE0           EQU  H'0000'
ADPRE1           EQU  H'0001'
ADPRE2           EQU  H'0002'
ADPRE3           EQU  H'0003'
ADPRE4           EQU  H'0004'
ADPRE8           EQU  H'0000'
ADPRE9           EQU  H'0001'
ADPRE10          EQU  H'0002'
ADPRE11          EQU  H'0003'
ADPRE12          EQU  H'0004'


;----- ADCON0 Bits -----------------------------------------------------
GO_ADCON0        EQU  H'0000'
FM0              EQU  H'0002'
FM1              EQU  H'0003'
CS_ADCON0        EQU  H'0004'
CONT             EQU  H'0006'
ON_ADCON0        EQU  H'0007'
ADGO             EQU  H'0000'
ADFM0            EQU  H'0002'
ADFM1            EQU  H'0003'
ADCS             EQU  H'0004'
ADCONT           EQU  H'0006'
ADON             EQU  H'0007'
DONE             EQU  H'0000'
FM0              EQU  H'0002'
GO_NOT_DONE      EQU  H'0000'
GO_NDONE         EQU  H'0000'
ADFM0            EQU  H'0002'


;----- ADCON1 Bits -----------------------------------------------------
DSEN             EQU  H'0000'
GPOL_ADCON1      EQU  H'0005'
IPEN_ADCON1      EQU  H'0006'
PPOL             EQU  H'0007'
ADDSEN           EQU  H'0000'
ADGPOL           EQU  H'0005'
ADIPEN           EQU  H'0006'
ADPPOL           EQU  H'0007'


;----- ADCON2 Bits -----------------------------------------------------
MD0              EQU  H'0000'
MD1              EQU  H'0001'
MD2              EQU  H'0002'
ACLR             EQU  H'0003'
CRS0             EQU  H'0004'
CRS1             EQU  H'0005'
CRS2             EQU  H'0006'
PSIS             EQU  H'0007'
ADMD0            EQU  H'0000'
ADMD1            EQU  H'0001'
ADMD2            EQU  H'0002'
ADACLR           EQU  H'0003'
ADCRS0           EQU  H'0004'
ADCRS1           EQU  H'0005'
ADCRS2           EQU  H'0006'
ADPSIS           EQU  H'0007'
ADMD0            EQU  H'0000'
ADMD1            EQU  H'0001'
ADMD2            EQU  H'0002'
ADCRS0           EQU  H'0004'
ADCRS1           EQU  H'0005'
ADCRS2           EQU  H'0006'
MD0              EQU  H'0000'
MD1              EQU  H'0001'
MD2              EQU  H'0002'
CRS0             EQU  H'0004'
CRS1             EQU  H'0005'
CRS2             EQU  H'0006'


;----- ADCON3 Bits -----------------------------------------------------
TMD0             EQU  H'0000'
TMD1             EQU  H'0001'
TMD2             EQU  H'0002'
SOI              EQU  H'0003'
CALC0            EQU  H'0004'
CALC1            EQU  H'0005'
CALC2            EQU  H'0006'
ADTMD0           EQU  H'0000'
ADTMD1           EQU  H'0001'
ADTMD2           EQU  H'0002'
ADSOI            EQU  H'0003'
ADCALC0          EQU  H'0004'
ADCALC1          EQU  H'0005'
ADCALC2          EQU  H'0006'
ADTMD0           EQU  H'0000'
ADTMD1           EQU  H'0001'
ADTMD2           EQU  H'0002'
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
STAT0_ADSTAT     EQU  H'0000'
STAT1_ADSTAT     EQU  H'0001'
STAT2            EQU  H'0002'
MATH             EQU  H'0004'
LTHR             EQU  H'0005'
UTHR             EQU  H'0006'
AOV              EQU  H'0007'
ADSTAT0          EQU  H'0000'
ADSTAT1          EQU  H'0001'
ADSTAT2          EQU  H'0002'
ADMATH           EQU  H'0004'
ADLTHR           EQU  H'0005'
ADUTHR           EQU  H'0006'
ADAOV            EQU  H'0007'
ADSTAT0          EQU  H'0000'
ADSTAT1          EQU  H'0001'
ADSTAT2          EQU  H'0002'
ADOV             EQU  H'0007'
STAT0_ADSTAT     EQU  H'0000'
STAT1_ADSTAT     EQU  H'0001'
STAT2            EQU  H'0002'
OV_ADSTAT        EQU  H'0007'


;----- ADREF Bits -----------------------------------------------------
PREF0            EQU  H'0000'
PREF1            EQU  H'0001'
PREF2            EQU  H'0002'
PREF3            EQU  H'0003'
NREF0            EQU  H'0004'
NREF1            EQU  H'0005'
NREF2            EQU  H'0006'
NREF3            EQU  H'0007'
ADPREF0          EQU  H'0000'
ADPREF1          EQU  H'0001'
ADPREF2          EQU  H'0002'
ADPREF3          EQU  H'0003'
ADNREF0          EQU  H'0004'
ADNREF1          EQU  H'0005'
ADNREF2          EQU  H'0006'
ADNREF3          EQU  H'0007'
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


;----- ANSELD Bits -----------------------------------------------------
ANSELD0          EQU  H'0000'
ANSELD1          EQU  H'0001'
ANSELD2          EQU  H'0002'
ANSELD3          EQU  H'0003'
ANSELD4          EQU  H'0004'
ANSELD5          EQU  H'0005'
ANSELD6          EQU  H'0006'
ANSELD7          EQU  H'0007'


;----- WPUD Bits -----------------------------------------------------
WPUD0            EQU  H'0000'
WPUD1            EQU  H'0001'
WPUD2            EQU  H'0002'
WPUD3            EQU  H'0003'
WPUD4            EQU  H'0004'
WPUD5            EQU  H'0005'
WPUD6            EQU  H'0006'
WPUD7            EQU  H'0007'


;----- ODCOND Bits -----------------------------------------------------
ODCD0            EQU  H'0000'
ODCD1            EQU  H'0001'
ODCD2            EQU  H'0002'
ODCD3            EQU  H'0003'
ODCD4            EQU  H'0004'
ODCD5            EQU  H'0005'
ODCD6            EQU  H'0006'
ODCD7            EQU  H'0007'


;----- SLRCOND Bits -----------------------------------------------------
SLRD0            EQU  H'0000'
SLRD1            EQU  H'0001'
SLRD2            EQU  H'0002'
SLRD3            EQU  H'0003'
SLRD4            EQU  H'0004'
SLRD5            EQU  H'0005'
SLRD6            EQU  H'0006'
SLRD7            EQU  H'0007'


;----- INLVLD Bits -----------------------------------------------------
INLVLD0          EQU  H'0000'
INLVLD1          EQU  H'0001'
INLVLD2          EQU  H'0002'
INLVLD3          EQU  H'0003'
INLVLD4          EQU  H'0004'
INLVLD5          EQU  H'0005'
INLVLD6          EQU  H'0006'
INLVLD7          EQU  H'0007'


;----- ANSELE Bits -----------------------------------------------------
ANSELE0          EQU  H'0000'
ANSELE1          EQU  H'0001'
ANSELE2          EQU  H'0002'


;----- WPUE Bits -----------------------------------------------------
WPUE0            EQU  H'0000'
WPUE1            EQU  H'0001'
WPUE2            EQU  H'0002'
WPUE3            EQU  H'0003'


;----- ODCONE Bits -----------------------------------------------------
ODCE0            EQU  H'0000'
ODCE1            EQU  H'0001'
ODCE2            EQU  H'0002'


;----- SLRCONE Bits -----------------------------------------------------
SLRE0            EQU  H'0000'
SLRE1            EQU  H'0001'
SLRE2            EQU  H'0002'


;----- INLVLE Bits -----------------------------------------------------
INLVLE0          EQU  H'0000'
INLVLE1          EQU  H'0001'
INLVLE2          EQU  H'0002'
INLVLE3          EQU  H'0003'


;----- IOCEP Bits -----------------------------------------------------
IOCEP3           EQU  H'0003'


;----- IOCEN Bits -----------------------------------------------------
IOCEN3           EQU  H'0003'


;----- IOCEF Bits -----------------------------------------------------
IOCEF3           EQU  H'0003'


;----- ANSELF Bits -----------------------------------------------------
ANSELF0          EQU  H'0000'
ANSELF1          EQU  H'0001'
ANSELF2          EQU  H'0002'
ANSELF3          EQU  H'0003'
ANSELF4          EQU  H'0004'
ANSELF5          EQU  H'0005'
ANSELF6          EQU  H'0006'
ANSELF7          EQU  H'0007'


;----- WPUF Bits -----------------------------------------------------
WPUF0            EQU  H'0000'
WPUF1            EQU  H'0001'
WPUF2            EQU  H'0002'
WPUF3            EQU  H'0003'
WPUF4            EQU  H'0004'
WPUF5            EQU  H'0005'
WPUF6            EQU  H'0006'
WPUF7            EQU  H'0007'


;----- ODCONF Bits -----------------------------------------------------
ODCF0            EQU  H'0000'
ODCF1            EQU  H'0001'
ODCF2            EQU  H'0002'
ODCF3            EQU  H'0003'
ODCF4            EQU  H'0004'
ODCF5            EQU  H'0005'
ODCF6            EQU  H'0006'
ODCF7            EQU  H'0007'


;----- SLRCONF Bits -----------------------------------------------------
SLRF0            EQU  H'0000'
SLRF1            EQU  H'0001'
SLRF2            EQU  H'0002'
SLRF3            EQU  H'0003'
SLRF4            EQU  H'0004'
SLRF5            EQU  H'0005'
SLRF6            EQU  H'0006'
SLRF7            EQU  H'0007'


;----- INLVLF Bits -----------------------------------------------------
INLVLF0          EQU  H'0000'
INLVLF1          EQU  H'0001'
INLVLF2          EQU  H'0002'
INLVLF3          EQU  H'0003'
INLVLF4          EQU  H'0004'
INLVLF5          EQU  H'0005'
INLVLF6          EQU  H'0006'
INLVLF7          EQU  H'0007'


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
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
NCO1ACC16        EQU  H'0000'
NCO1ACC17        EQU  H'0001'
NCO1ACC18        EQU  H'0002'
NCO1ACC19        EQU  H'0003'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
NCO1ACC0         EQU  H'0000'
NCO1ACC1         EQU  H'0001'
NCO1ACC2         EQU  H'0002'
NCO1ACC3         EQU  H'0003'


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
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
NCO1INC16        EQU  H'0000'
NCO1INC17        EQU  H'0001'
NCO1INC18        EQU  H'0002'
NCO1INC19        EQU  H'0003'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
NCO1INC0         EQU  H'0000'
NCO1INC1         EQU  H'0001'
NCO1INC2         EQU  H'0002'
NCO1INC3         EQU  H'0003'


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
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO1CKS0         EQU  H'0000'
NCO1CKS1         EQU  H'0001'
NCO1CKS2         EQU  H'0002'
NCO1CKS3         EQU  H'0003'
NCO1CKS4         EQU  H'0004'
NCO1PWS0         EQU  H'0005'
NCO1PWS1         EQU  H'0006'
NCO1PWS2         EQU  H'0007'
NCO1CKS0         EQU  H'0000'
NCO1CKS1         EQU  H'0001'
NCO1CKS2         EQU  H'0002'
NCO1CKS3         EQU  H'0003'
NCO1CKS4         EQU  H'0004'
NCO1PWS0         EQU  H'0005'
NCO1PWS1         EQU  H'0006'
NCO1PWS2         EQU  H'0007'


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
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
NCO2ACC16        EQU  H'0000'
NCO2ACC17        EQU  H'0001'
NCO2ACC18        EQU  H'0002'
NCO2ACC19        EQU  H'0003'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
NCO2ACC0         EQU  H'0000'
NCO2ACC1         EQU  H'0001'
NCO2ACC2         EQU  H'0002'
NCO2ACC3         EQU  H'0003'


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
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
NCO2INC16        EQU  H'0000'
NCO2INC17        EQU  H'0001'
NCO2INC18        EQU  H'0002'
NCO2INC19        EQU  H'0003'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
NCO2INC0         EQU  H'0000'
NCO2INC1         EQU  H'0001'
NCO2INC2         EQU  H'0002'
NCO2INC3         EQU  H'0003'


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
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO2CKS0         EQU  H'0000'
NCO2CKS1         EQU  H'0001'
NCO2CKS2         EQU  H'0002'
NCO2CKS3         EQU  H'0003'
NCO2CKS4         EQU  H'0004'
NCO2PWS0         EQU  H'0005'
NCO2PWS1         EQU  H'0006'
NCO2PWS2         EQU  H'0007'
NCO2CKS0         EQU  H'0000'
NCO2CKS1         EQU  H'0001'
NCO2CKS2         EQU  H'0002'
NCO2CKS3         EQU  H'0003'
NCO2CKS4         EQU  H'0004'
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
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
NCO3ACC16        EQU  H'0000'
NCO3ACC17        EQU  H'0001'
NCO3ACC18        EQU  H'0002'
NCO3ACC19        EQU  H'0003'
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'
NCO3ACC0         EQU  H'0000'
NCO3ACC1         EQU  H'0001'
NCO3ACC2         EQU  H'0002'
NCO3ACC3         EQU  H'0003'


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
INC0             EQU  H'0000'
INC1             EQU  H'0001'
INC2             EQU  H'0002'
INC3             EQU  H'0003'
NCO3INC16        EQU  H'0000'
NCO3INC17        EQU  H'0001'
NCO3INC18        EQU  H'0002'
NCO3INC19        EQU  H'0003'
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'
NCO3INC0         EQU  H'0000'
NCO3INC1         EQU  H'0001'
NCO3INC2         EQU  H'0002'
NCO3INC3         EQU  H'0003'


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
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
CKS0             EQU  H'0000'
CKS1             EQU  H'0001'
CKS2             EQU  H'0002'
CKS3             EQU  H'0003'
CKS4             EQU  H'0004'
PWS0             EQU  H'0005'
PWS1             EQU  H'0006'
PWS2             EQU  H'0007'
NCO3CKS0         EQU  H'0000'
NCO3CKS1         EQU  H'0001'
NCO3CKS2         EQU  H'0002'
NCO3CKS3         EQU  H'0003'
NCO3CKS4         EQU  H'0004'
NCO3PWS0         EQU  H'0005'
NCO3PWS1         EQU  H'0006'
NCO3PWS2         EQU  H'0007'
NCO3CKS0         EQU  H'0000'
NCO3CKS1         EQU  H'0001'
NCO3CKS2         EQU  H'0002'
NCO3CKS3         EQU  H'0003'
NCO3CKS4         EQU  H'0004'
NCO3PWS0         EQU  H'0005'
NCO3PWS1         EQU  H'0006'
NCO3PWS2         EQU  H'0007'


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


;----- PWM1ERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'
ERS4             EQU  H'0004'
ERS5             EQU  H'0005'
ERS6             EQU  H'0006'
ERS7             EQU  H'0007'


;----- PWM1CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
CLK5             EQU  H'0005'
CLK6             EQU  H'0006'
CLK7             EQU  H'0007'


;----- PWM1LDS Bits -----------------------------------------------------
LDS0             EQU  H'0000'
LDS1             EQU  H'0001'
LDS2             EQU  H'0002'
LDS3             EQU  H'0003'
LDS4             EQU  H'0004'
LDS5             EQU  H'0005'
LDS6             EQU  H'0006'
LDS7             EQU  H'0007'


;----- PWM1PR Bits -----------------------------------------------------


;----- PWM1PRH Bits -----------------------------------------------------


;----- PWM1CPRE Bits -----------------------------------------------------


;----- PWM1PIPOS Bits -----------------------------------------------------


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWM1S1P1 Bits -----------------------------------------------------


;----- PWM1S1P1H Bits -----------------------------------------------------


;----- PWM1S1P2 Bits -----------------------------------------------------


;----- PWM1S1P2H Bits -----------------------------------------------------


;----- PWM2ERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'
ERS4             EQU  H'0004'
ERS5             EQU  H'0005'
ERS6             EQU  H'0006'
ERS7             EQU  H'0007'


;----- PWM2CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
CLK5             EQU  H'0005'
CLK6             EQU  H'0006'
CLK7             EQU  H'0007'


;----- PWM2LDS Bits -----------------------------------------------------
LDS0             EQU  H'0000'
LDS1             EQU  H'0001'
LDS2             EQU  H'0002'
LDS3             EQU  H'0003'
LDS4             EQU  H'0004'
LDS5             EQU  H'0005'
LDS6             EQU  H'0006'
LDS7             EQU  H'0007'


;----- PWM2PR Bits -----------------------------------------------------


;----- PWM2PRH Bits -----------------------------------------------------


;----- PWM2CPRE Bits -----------------------------------------------------


;----- PWM2PIPOS Bits -----------------------------------------------------


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWM2S1P1 Bits -----------------------------------------------------


;----- PWM2S1P1H Bits -----------------------------------------------------


;----- PWM2S1P2 Bits -----------------------------------------------------


;----- PWM2S1P2H Bits -----------------------------------------------------


;----- PWM3ERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'
ERS4             EQU  H'0004'
ERS5             EQU  H'0005'
ERS6             EQU  H'0006'
ERS7             EQU  H'0007'


;----- PWM3CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'
CLK5             EQU  H'0005'
CLK6             EQU  H'0006'
CLK7             EQU  H'0007'


;----- PWM3LDS Bits -----------------------------------------------------
LDS0             EQU  H'0000'
LDS1             EQU  H'0001'
LDS2             EQU  H'0002'
LDS3             EQU  H'0003'
LDS4             EQU  H'0004'
LDS5             EQU  H'0005'
LDS6             EQU  H'0006'
LDS7             EQU  H'0007'


;----- PWM3PR Bits -----------------------------------------------------


;----- PWM3PRH Bits -----------------------------------------------------


;----- PWM3CPRE Bits -----------------------------------------------------


;----- PWM3PIPOS Bits -----------------------------------------------------


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
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- PWM3S1P1 Bits -----------------------------------------------------


;----- PWM3S1P1H Bits -----------------------------------------------------


;----- PWM3S1P2 Bits -----------------------------------------------------


;----- PWM3S1P2H Bits -----------------------------------------------------


;----- PWMLOAD Bits -----------------------------------------------------
MPWM1LD          EQU  H'0000'
MPWM2LD          EQU  H'0001'
MPWM3LD          EQU  H'0002'


;----- PWMEN Bits -----------------------------------------------------
MPWM1EN          EQU  H'0000'
MPWM2EN          EQU  H'0001'
MPWM3EN          EQU  H'0002'


;----- PIE0 Bits -----------------------------------------------------
SWIE             EQU  H'0000'
HLVDIE           EQU  H'0001'
OSFIE            EQU  H'0002'
CSWIE            EQU  H'0003'
CLC1IE           EQU  H'0005'
IOCIE            EQU  H'0007'


;----- PIE1 Bits -----------------------------------------------------
INT0IE           EQU  H'0000'
ZCDIE            EQU  H'0001'
ADIE             EQU  H'0002'
ACTIE            EQU  H'0003'
C1IE             EQU  H'0004'
SMT1IE           EQU  H'0005'
SMT1PRAIE        EQU  H'0006'
SMT1PWAIE        EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
ADTIE            EQU  H'0000'
DMA1SCNTIE       EQU  H'0004'
DMA1DCNTIE       EQU  H'0005'
DMA1ORIE         EQU  H'0006'
DMA1AIE          EQU  H'0007'


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
PWM1PIE          EQU  H'0006'
PWM1IE           EQU  H'0007'


;----- PIE5 Bits -----------------------------------------------------
SPI2RXIE         EQU  H'0000'
SPI2TXIE         EQU  H'0001'
SPI2IE           EQU  H'0002'
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


;----- PIE10 Bits -----------------------------------------------------
INT2IE           EQU  H'0000'
CLC5IE           EQU  H'0001'
CWG2IE           EQU  H'0002'
NCO2IE           EQU  H'0003'
DMA3SCNTIE       EQU  H'0004'
DMA3DCNTIE       EQU  H'0005'
DMA3ORIE         EQU  H'0006'
DMA3AIE          EQU  H'0007'


;----- PIE11 Bits -----------------------------------------------------
CCP3IE           EQU  H'0000'
CLC6IE           EQU  H'0001'
CWG3IE           EQU  H'0002'
TMR4IE           EQU  H'0003'
DMA4SCNTIE       EQU  H'0004'
DMA4DCNTIE       EQU  H'0005'
DMA4ORIE         EQU  H'0006'
DMA4AIE          EQU  H'0007'


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


;----- PIE15 Bits -----------------------------------------------------
NVMIE            EQU  H'0000'
CLC8IE           EQU  H'0001'
CRCIE            EQU  H'0002'
TMR6IE           EQU  H'0003'


;----- PIR0 Bits -----------------------------------------------------
SWIF             EQU  H'0000'
HLVDIF           EQU  H'0001'
OSFIF            EQU  H'0002'
CSWIF            EQU  H'0003'
CLC1IF           EQU  H'0005'
IOCIF            EQU  H'0007'


;----- PIR1 Bits -----------------------------------------------------
INT0IF           EQU  H'0000'
ZCDIF            EQU  H'0001'
ADIF             EQU  H'0002'
ACTIF            EQU  H'0003'
C1IF             EQU  H'0004'
SMT1IF           EQU  H'0005'
SMT1PRAIF        EQU  H'0006'
SMT1PWAIF        EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
ADTIF            EQU  H'0000'
DMA1SCNTIF       EQU  H'0004'
DMA1DCNTIF       EQU  H'0005'
DMA1ORIF         EQU  H'0006'
DMA1AIF          EQU  H'0007'


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
PWM1PIF          EQU  H'0006'
PWM1IF           EQU  H'0007'


;----- PIR5 Bits -----------------------------------------------------
SPI2RXIF         EQU  H'0000'
SPI2TXIF         EQU  H'0001'
SPI2IF           EQU  H'0002'
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


;----- PIR10 Bits -----------------------------------------------------
INT2IF           EQU  H'0000'
CLC5IF           EQU  H'0001'
CWG2IF           EQU  H'0002'
NCO2IF           EQU  H'0003'
DMA3SCNTIF       EQU  H'0004'
DMA3DCNTIF       EQU  H'0005'
DMA3ORIF         EQU  H'0006'
DMA3AIF          EQU  H'0007'


;----- PIR11 Bits -----------------------------------------------------
CCP3IF           EQU  H'0000'
CLC6IF           EQU  H'0001'
CWG3IF           EQU  H'0002'
TMR4IF           EQU  H'0003'
DMA4SCNTIF       EQU  H'0004'
DMA4DCNTIF       EQU  H'0005'
DMA4ORIF         EQU  H'0006'
DMA4AIF          EQU  H'0007'


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


;----- PIR15 Bits -----------------------------------------------------
NVMIF            EQU  H'0000'
CLC8IF           EQU  H'0001'
CRCIF            EQU  H'0002'
TMR6IF           EQU  H'0003'


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


;----- LATD Bits -----------------------------------------------------
LATD0            EQU  H'0000'
LATD1            EQU  H'0001'
LATD2            EQU  H'0002'
LATD3            EQU  H'0003'
LATD4            EQU  H'0004'
LATD5            EQU  H'0005'
LATD6            EQU  H'0006'
LATD7            EQU  H'0007'


;----- LATE Bits -----------------------------------------------------
LATE0            EQU  H'0000'
LATE1            EQU  H'0001'
LATE2            EQU  H'0002'


;----- LATF Bits -----------------------------------------------------
LATF0            EQU  H'0000'
LATF1            EQU  H'0001'
LATF2            EQU  H'0002'
LATF3            EQU  H'0003'
LATF4            EQU  H'0004'
LATF5            EQU  H'0005'
LATF6            EQU  H'0006'
LATF7            EQU  H'0007'


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


;----- TRISD Bits -----------------------------------------------------
TRISD0           EQU  H'0000'
TRISD1           EQU  H'0001'
TRISD2           EQU  H'0002'
TRISD3           EQU  H'0003'
TRISD4           EQU  H'0004'
TRISD5           EQU  H'0005'
TRISD6           EQU  H'0006'
TRISD7           EQU  H'0007'


;----- TRISE Bits -----------------------------------------------------
TRISE0           EQU  H'0000'
TRISE1           EQU  H'0001'
TRISE2           EQU  H'0002'


;----- TRISF Bits -----------------------------------------------------
TRISF0           EQU  H'0000'
TRISF1           EQU  H'0001'
TRISF2           EQU  H'0002'
TRISF3           EQU  H'0003'
TRISF4           EQU  H'0004'
TRISF5           EQU  H'0005'
TRISF6           EQU  H'0006'
TRISF7           EQU  H'0007'


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


;----- PORTD Bits -----------------------------------------------------
RD0              EQU  H'0000'
RD1              EQU  H'0001'
RD2              EQU  H'0002'
RD3              EQU  H'0003'
RD4              EQU  H'0004'
RD5              EQU  H'0005'
RD6              EQU  H'0006'
RD7              EQU  H'0007'


;----- PORTE Bits -----------------------------------------------------
RE0              EQU  H'0000'
RE1              EQU  H'0001'
RE2              EQU  H'0002'
RE3              EQU  H'0003'


;----- PORTF Bits -----------------------------------------------------
RF0              EQU  H'0000'
RF1              EQU  H'0001'
RF2              EQU  H'0002'
RF3              EQU  H'0003'
RF4              EQU  H'0004'
RF5              EQU  H'0005'
RF6              EQU  H'0006'
RF7              EQU  H'0007'


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


;----- FSR2L Bits -----------------------------------------------------
FSR20            EQU  H'0000'
FSR21            EQU  H'0001'
FSR22            EQU  H'0002'
FSR23            EQU  H'0003'
FSR24            EQU  H'0004'
FSR25            EQU  H'0005'
FSR26            EQU  H'0006'
FSR27            EQU  H'0007'
FSR28            EQU  H'0008'
FSR29            EQU  H'0009'
FSR210           EQU  H'0010'
FSR211           EQU  H'0011'


;----- PLUSW2 Bits -----------------------------------------------------


;----- PREINC2 Bits -----------------------------------------------------


;----- POSTDEC2 Bits -----------------------------------------------------


;----- POSTINC2 Bits -----------------------------------------------------


;----- INDF2 Bits -----------------------------------------------------


;----- FSR1L Bits -----------------------------------------------------
FSR10            EQU  H'0000'
FSR11            EQU  H'0001'
FSR12            EQU  H'0002'
FSR13            EQU  H'0003'
FSR14            EQU  H'0004'
FSR15            EQU  H'0005'
FSR16            EQU  H'0006'
FSR17            EQU  H'0007'
FSR18            EQU  H'0008'
FSR19            EQU  H'0009'
FSR110           EQU  H'0010'
FSR111           EQU  H'0011'


;----- PLUSW1 Bits -----------------------------------------------------


;----- PREINC1 Bits -----------------------------------------------------


;----- POSTDEC1 Bits -----------------------------------------------------


;----- POSTINC1 Bits -----------------------------------------------------


;----- INDF1 Bits -----------------------------------------------------


;----- WREG Bits -----------------------------------------------------


;----- FSR0L Bits -----------------------------------------------------
FSR00            EQU  H'0000'
FSR01            EQU  H'0001'
FSR02            EQU  H'0002'
FSR03            EQU  H'0003'
FSR04            EQU  H'0004'
FSR05            EQU  H'0005'
FSR06            EQU  H'0006'
FSR07            EQU  H'0007'
FSR08            EQU  H'0008'
FSR09            EQU  H'0009'
FSR010           EQU  H'0010'
FSR011           EQU  H'0011'


;----- PLUSW0 Bits -----------------------------------------------------


;----- PREINC0 Bits -----------------------------------------------------


;----- POSTDEC0 Bits -----------------------------------------------------


;----- POSTINC0 Bits -----------------------------------------------------


;----- INDF0 Bits -----------------------------------------------------


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
NOT_RCM          EQU  H'0000'
NOT_MEMV         EQU  H'0001'
NOT_RVREG        EQU  H'0002'
NRCM             EQU  H'0000'
NMEMV            EQU  H'0001'
NRVREG           EQU  H'0002'
MEMV             EQU  H'0001'


;----- CPUDOZE Bits -----------------------------------------------------
DOZE0            EQU  H'0000'
DOZE1            EQU  H'0001'
DOZE2            EQU  H'0002'
DOE              EQU  H'0004'
ROI              EQU  H'0005'
DOZEN            EQU  H'0006'
IDLEN            EQU  H'0007'
DOZE0            EQU  H'0000'
DOZE1            EQU  H'0001'
DOZE2            EQU  H'0002'


;----- PROD Bits -----------------------------------------------------
PROD0            EQU  H'0000'
PROD1            EQU  H'0001'
PROD2            EQU  H'0002'
PROD3            EQU  H'0003'
PROD4            EQU  H'0004'
PROD5            EQU  H'0005'
PROD6            EQU  H'0006'
PROD7            EQU  H'0007'
PROD8            EQU  H'0008'
PROD9            EQU  H'0009'
PROD10           EQU  H'0010'
PROD11           EQU  H'0011'
PROD12           EQU  H'0012'
PROD13           EQU  H'0013'
PROD14           EQU  H'0014'
PROD15           EQU  H'0015'


;----- PRODH Bits -----------------------------------------------------


;----- TABLAT Bits -----------------------------------------------------


;----- PCLAT Bits -----------------------------------------------------
PCLAT0           EQU  H'0000'
PCLAT1           EQU  H'0001'
PCLAT2           EQU  H'0002'
PCLAT3           EQU  H'0003'
PCLAT4           EQU  H'0004'
PCLAT5           EQU  H'0005'
PCLAT6           EQU  H'0006'
PCLAT7           EQU  H'0007'
PCLAT8           EQU  H'0008'
PCLAT9           EQU  H'0009'
PCLAT10          EQU  H'0010'
PCLAT11          EQU  H'0011'
PCLAT12          EQU  H'0012'
PCLAT13          EQU  H'0013'
PCLAT14          EQU  H'0014'
PCLAT15          EQU  H'0015'
PCLAT16          EQU  H'0016'
PCLAT17          EQU  H'0017'
PCLAT18          EQU  H'0018'
PCLAT19          EQU  H'0019'
PCLAT20          EQU  H'0020'


;----- PCLATH Bits -----------------------------------------------------


;----- PCLATU Bits -----------------------------------------------------
PCU0             EQU  H'0000'
PCU1             EQU  H'0001'
PCU2             EQU  H'0002'
PCU3             EQU  H'0003'
PCU4             EQU  H'0004'


;----- STKPTR Bits -----------------------------------------------------
STKPTR0          EQU  H'0000'
STKPTR1          EQU  H'0001'
STKPTR2          EQU  H'0002'
STKPTR3          EQU  H'0003'
STKPTR4          EQU  H'0004'
STKPTR5          EQU  H'0005'
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


;----- TOS Bits -----------------------------------------------------
TOS0             EQU  H'0000'
TOS1             EQU  H'0001'
TOS2             EQU  H'0002'
TOS3             EQU  H'0003'
TOS4             EQU  H'0004'
TOS5             EQU  H'0005'
TOS6             EQU  H'0006'
TOS7             EQU  H'0007'
TOS8             EQU  H'0008'
TOS9             EQU  H'0009'
TOS10            EQU  H'0010'
TOS11            EQU  H'0011'
TOS12            EQU  H'0012'
TOS13            EQU  H'0013'
TOS14            EQU  H'0014'
TOS15            EQU  H'0015'
TOS16            EQU  H'0016'
TOS17            EQU  H'0017'
TOS18            EQU  H'0018'
TOS19            EQU  H'0019'
TOS20            EQU  H'0020'


;----- TOSH Bits -----------------------------------------------------

;==========================================================================
;
;       RAM Definitions
;
;==========================================================================

__MAXRAM  H'15FF'
__BADRAM  H'0000'-H'0038'
__BADRAM  H'003B'-H'003F'
__BADRAM  H'004E'-H'0060'
__BADRAM  H'0062'
__BADRAM  H'0069'
__BADRAM  H'009A'-H'00AB'
__BADRAM  H'00BC'-H'00BD'
__BADRAM  H'00C0'-H'00D3'
__BADRAM  H'00E0'-H'00E7'
__BADRAM  H'0224'-H'0228'
__BADRAM  H'0231'-H'023E'
__BADRAM  H'024B'-H'024E'
__BADRAM  H'0252'
__BADRAM  H'0256'
__BADRAM  H'027C'-H'0285'
__BADRAM  H'028A'
__BADRAM  H'029E'-H'02A0'
__BADRAM  H'02B5'
__BADRAM  H'02B7'
__BADRAM  H'02C8'
__BADRAM  H'02CA'
__BADRAM  H'02EB'
__BADRAM  H'02EE'
__BADRAM  H'02EE'
__BADRAM  H'02F0'
__BADRAM  H'0312'-H'0317'
__BADRAM  H'034E'-H'034F'
__BADRAM  H'0359'
__BADRAM  H'0372'
__BADRAM  H'0384'-H'03BB'
__BADRAM  H'03EE'
__BADRAM  H'03FB'-H'03FF'
__BADRAM  H'041E'-H'0420'
__BADRAM  H'042E'-H'0440'
__BADRAM  H'0458'
__BADRAM  H'048E'-H'049C'
__BADRAM  H'04C4'-H'04C5'
__BADRAM  H'04CC'-H'04CD'
__BADRAM  H'04E4'-H'04E5'


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
_CONFIG10     EQU  H'300009'

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
_FCMEN_OFF_2         EQU  H'DF'; Fail-Safe Clock Monitor disabled
_FCMEN_ON_2          EQU  H'FF'; Fail-Safe Clock Monitor enabled

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
_BBEN_OFF_7          EQU  H'FF'; Boot block disabled
_BBEN_ON_7           EQU  H'F7'; Boot block enabled
_SAFEN_OFF_7         EQU  H'FF'; SAF disabled
_SAFEN_ON_7          EQU  H'EF'; SAF enabled
_DEBUG_OFF_7         EQU  H'FF'; Background Debugger disabled
_DEBUG_ON_7          EQU  H'DF'; Background Debugger enabled

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

;----- CONFIG10 Options --------------------------------------------------
_CP_OFF_10           EQU  H'FF'; PFM and Data EEPROM code protection disabled
_CP_ON_10            EQU  H'FE'; PFM and Data EEPROM code protection enabled

;----- DEVID Equates --------------------------------------------------
_IDLOC0            EQU  H'200000'
_IDLOC1            EQU  H'200001'
_IDLOC2            EQU  H'200002'
_IDLOC3            EQU  H'200003'
_IDLOC4            EQU  H'200004'
_IDLOC5            EQU  H'200005'
_IDLOC6            EQU  H'200006'
_IDLOC7            EQU  H'200007'
_IDLOC8            EQU  H'200008'
_IDLOC9            EQU  H'200009'
_IDLOC10           EQU  H'20000A'
_IDLOC11           EQU  H'20000B'
_IDLOC12           EQU  H'20000C'
_IDLOC13           EQU  H'20000D'
_IDLOC14           EQU  H'20000E'
_IDLOC15           EQU  H'20000F'
_IDLOC16           EQU  H'200010'
_IDLOC17           EQU  H'200011'
_IDLOC18           EQU  H'200012'
_IDLOC19           EQU  H'200013'
_IDLOC20           EQU  H'200014'
_IDLOC21           EQU  H'200015'
_IDLOC22           EQU  H'200016'
_IDLOC23           EQU  H'200017'
_IDLOC24           EQU  H'200018'
_IDLOC25           EQU  H'200019'
_IDLOC26           EQU  H'20001A'
_IDLOC27           EQU  H'20001B'
_IDLOC28           EQU  H'20001C'
_IDLOC29           EQU  H'20001D'
_IDLOC30           EQU  H'20001E'
_IDLOC31           EQU  H'20001F'
_IDLOC32           EQU  H'200020'
_IDLOC33           EQU  H'200021'
_IDLOC34           EQU  H'200022'
_IDLOC35           EQU  H'200023'
_IDLOC36           EQU  H'200024'
_IDLOC37           EQU  H'200025'
_IDLOC38           EQU  H'200026'
_IDLOC39           EQU  H'200027'
_IDLOC40           EQU  H'200028'
_IDLOC41           EQU  H'200029'
_IDLOC42           EQU  H'20002A'
_IDLOC43           EQU  H'20002B'
_IDLOC44           EQU  H'20002C'
_IDLOC45           EQU  H'20002D'
_IDLOC46           EQU  H'20002E'
_IDLOC47           EQU  H'20002F'
_IDLOC48           EQU  H'200030'
_IDLOC49           EQU  H'200031'
_IDLOC50           EQU  H'200032'
_IDLOC51           EQU  H'200033'
_IDLOC52           EQU  H'200034'
_IDLOC53           EQU  H'200035'
_IDLOC54           EQU  H'200036'
_IDLOC55           EQU  H'200037'
_IDLOC56           EQU  H'200038'
_IDLOC57           EQU  H'200039'
_IDLOC58           EQU  H'20003A'
_IDLOC59           EQU  H'20003B'
_IDLOC60           EQU  H'20003C'
_IDLOC61           EQU  H'20003D'
_IDLOC62           EQU  H'20003E'
_IDLOC63           EQU  H'20003F'

        LIST
