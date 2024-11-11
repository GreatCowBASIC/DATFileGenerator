        LIST
;==========================================================================
;
;  Built by GCBASIC convertor : 18F_PIC_to_INCv6_20
;  MPASM processor include for the chip shown below
;
; C:\Users\admin\.mchp_packs\index.idx
; C:\Users\admin\.mchp_packs\Microchip\PIC18F-Q_DFP\1.25.433\xc8\pic\dat\ini\18F14Q20.ini
; C:\Users\admin\.mchp_packs\Microchip\PIC18F-Q_DFP\1.25.433\edc\18F14Q20.PIC
; C:\Users\admin\.mchp_packs\Microchip\PIC18F-Q_DFP\1.25.433\xc8\pic\dat\cfgdata\18F14Q20.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __PIC18F14Q20
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
PRLOCK           EQU  H'0038'
MAINPR           EQU  H'0039'
ISRPR            EQU  H'003A'
DMA1PR           EQU  H'003B'
DMA2PR           EQU  H'003C'
DMA3PR           EQU  H'003D'
DMA4PR           EQU  H'003E'
SCANPR           EQU  H'003F'
DMASELECT        EQU  H'0040'
DMANBUF          EQU  H'0041'
DMANDCNT         EQU  H'0042'
DMANDCNTL        EQU  H'0042'
DMANDCNTH        EQU  H'0043'
DMANDPTR         EQU  H'0044'
DMANDPTRL        EQU  H'0044'
DMANDPTRH        EQU  H'0045'
DMANDSZ          EQU  H'0046'
DMANDSZL         EQU  H'0046'
DMANDSZH         EQU  H'0047'
DMANDSA          EQU  H'0048'
DMANDSAL         EQU  H'0048'
DMANDSAH         EQU  H'0049'
DMANSCNT         EQU  H'004A'
DMANSCNTL        EQU  H'004A'
DMANSCNTH        EQU  H'004B'
DMANSPTR         EQU  H'004C'
DMANSPTRL        EQU  H'004C'
DMANSPTRH        EQU  H'004D'
DMANSPTRU        EQU  H'004E'
DMANSSZ          EQU  H'004F'
DMANSSZL         EQU  H'004F'
DMANSSZH         EQU  H'0050'
DMANSSA          EQU  H'0051'
DMANSSAL         EQU  H'0051'
DMANSSAH         EQU  H'0052'
DMANSSAU         EQU  H'0053'
DMANCON0         EQU  H'0054'
DMANCON1         EQU  H'0055'
DMANAIRQ         EQU  H'0056'
DMANSIRQ         EQU  H'0057'
NVMCON0          EQU  H'0058'
NVMCON1          EQU  H'0059'
NVMLOCK          EQU  H'005A'
NVMADR           EQU  H'005B'
NVMADRL          EQU  H'005B'
NVMADRH          EQU  H'005C'
NVMADRU          EQU  H'005D'
NVMDAT           EQU  H'005E'
NVMDATL          EQU  H'005E'
NVMDATH          EQU  H'005F'
CRCDATA          EQU  H'0060'
CRCDATAL         EQU  H'0060'
CRCDATAH         EQU  H'0061'
CRCDATAU         EQU  H'0062'
CRCDATAT         EQU  H'0063'
CRCOUT           EQU  H'0064'
CRCSHFT          EQU  H'0064'
CRCXOR           EQU  H'0064'
CRCOUTL          EQU  H'0064'
CRCSHFTL         EQU  H'0064'
CRCSHIFTL        EQU  H'0064'
CRCXORL          EQU  H'0064'
CRCOUTH          EQU  H'0065'
CRCSHFTH         EQU  H'0065'
CRCSHIFTH        EQU  H'0065'
CRCXORH          EQU  H'0065'
CRCOUTU          EQU  H'0066'
CRCSHFTU         EQU  H'0066'
CRCSHIFTU        EQU  H'0066'
CRCXORU          EQU  H'0066'
CRCOUTT          EQU  H'0067'
CRCSHFTT         EQU  H'0067'
CRCSHIFTT        EQU  H'0067'
CRCXORT          EQU  H'0067'
CRCCON0          EQU  H'0068'
CRCCON1          EQU  H'0069'
CRCCON2          EQU  H'006A'
SCANLADR         EQU  H'006B'
SCANLADRL        EQU  H'006B'
SCANLADRH        EQU  H'006C'
SCANLADRU        EQU  H'006D'
SCANHADR         EQU  H'006E'
SCANHADRL        EQU  H'006E'
SCANHADRH        EQU  H'006F'
SCANHADRU        EQU  H'0070'
SCANCON0         EQU  H'0071'
SCANTRIG         EQU  H'0072'
BORCON           EQU  H'0073'
WDTCON0          EQU  H'0074'
WDTCON1          EQU  H'0075'
WDTPS            EQU  H'0076'
WDTPSL           EQU  H'0076'
WDTPSH           EQU  H'0077'
WDTTMR           EQU  H'0078'
VREGCON          EQU  H'0079'
OSCCON1          EQU  H'007A'
OSCCON2          EQU  H'007B'
OSCCON3          EQU  H'007C'
OSCSTAT          EQU  H'007D'
OSCSTAT1         EQU  H'007D'
OSCEN            EQU  H'007E'
OSCTUNE          EQU  H'007F'
OSCFRQ           EQU  H'0080'
OSCFREQ          EQU  H'0080'
ACTCON           EQU  H'0081'
FSCMCON          EQU  H'0082'
I3C1CON0         EQU  H'0083'
I3C1CON1         EQU  H'0084'
I3C1RXB          EQU  H'0085'
I3C1TXB          EQU  H'0086'
I3C1STAT0        EQU  H'0087'
I3C1STAT1        EQU  H'0088'
I3C1BSTAT        EQU  H'0089'
I3C1PIR0         EQU  H'008A'
I3C1PIR1         EQU  H'008B'
I3C1ERRIR0       EQU  H'008C'
I3C1ERRIR1       EQU  H'008D'
I3C1PIE0         EQU  H'008E'
I3C1PIE1         EQU  H'008F'
I3C1ERRIE0       EQU  H'0090'
I3C1ERRIE1       EQU  H'0091'
I3C1BIDL         EQU  H'0092'
I3C1BIDLL        EQU  H'0092'
I3C1BIDLH        EQU  H'0093'
I3C1BAVL         EQU  H'0094'
I3C1BTO          EQU  H'0095'
I3C1BTOL         EQU  H'0095'
I3C1BTOH         EQU  H'0096'
I3C1IBIMDB       EQU  H'0097'
I3C1RETRY        EQU  H'0098'
I3C1FEAT         EQU  H'0099'
I3C1SADR         EQU  H'009A'
I3C1DADR         EQU  H'009B'
I3C1EC           EQU  H'009C'
I3C1MWL          EQU  H'009D'
I3C1MWLL         EQU  H'009D'
I3C1MWLH         EQU  H'009E'
I3C1MRL          EQU  H'009F'
I3C1MRLL         EQU  H'009F'
I3C1MRLH         EQU  H'00A0'
I3C1IBIPSZ       EQU  H'00A1'
I3C1PID0         EQU  H'00A2'
I3C1PID1         EQU  H'00A3'
I3C1PID2         EQU  H'00A4'
I3C1PID3         EQU  H'00A5'
I3C1PID4         EQU  H'00A6'
I3C1PID5         EQU  H'00A7'
I3C1BCR          EQU  H'00A8'
I3C1DCR          EQU  H'00A9'
I3C1DSTAT0       EQU  H'00AA'
I3C1DSTAT1       EQU  H'00AB'
I3C1MWS          EQU  H'00AC'
I3C1MRS          EQU  H'00AD'
I3C1MRT          EQU  H'00AE'
I3C1MRTL         EQU  H'00AE'
I3C1MRTH         EQU  H'00AF'
I3C1MRTU         EQU  H'00B0'
I3C1RSTACT       EQU  H'00B1'
I3C1BUSCXT       EQU  H'00B2'
I3C1CCC          EQU  H'00B3'
I3C1I2CCON       EQU  H'00B4'
I3C1CLK          EQU  H'00B5'
I3C2CON0         EQU  H'00B6'
I3C2CON1         EQU  H'00B7'
I3C2RXB          EQU  H'00B8'
I3C2TXB          EQU  H'00B9'
I3C2STAT0        EQU  H'00BA'
I3C2STAT1        EQU  H'00BB'
I3C2BSTAT        EQU  H'00BC'
I3C2PIR0         EQU  H'00BD'
I3C2PIR1         EQU  H'00BE'
I3C2ERRIR0       EQU  H'00BF'
I3C2ERRIR1       EQU  H'00C0'
I3C2PIE0         EQU  H'00C1'
I3C2PIE1         EQU  H'00C2'
I3C2ERRIE0       EQU  H'00C3'
I3C2ERRIE1       EQU  H'00C4'
I3C2BIDL         EQU  H'00C5'
I3C2BIDLL        EQU  H'00C5'
I3C2BIDLH        EQU  H'00C6'
I3C2BAVL         EQU  H'00C7'
I3C2BTO          EQU  H'00C8'
I3C2BTOL         EQU  H'00C8'
I3C2BTOH         EQU  H'00C9'
I3C2IBIMDB       EQU  H'00CA'
I3C2RETRY        EQU  H'00CB'
I3C2FEAT         EQU  H'00CC'
I3C2SADR         EQU  H'00CD'
I3C2DADR         EQU  H'00CE'
I3C2EC           EQU  H'00CF'
I3C2MWL          EQU  H'00D0'
I3C2MWLL         EQU  H'00D0'
I3C2MWLH         EQU  H'00D1'
I3C2MRL          EQU  H'00D2'
I3C2MRLL         EQU  H'00D2'
I3C2MRLH         EQU  H'00D3'
I3C2IBIPSZ       EQU  H'00D4'
I3C2PID0         EQU  H'00D5'
I3C2PID1         EQU  H'00D6'
I3C2PID2         EQU  H'00D7'
I3C2PID3         EQU  H'00D8'
I3C2PID4         EQU  H'00D9'
I3C2PID5         EQU  H'00DA'
I3C2BCR          EQU  H'00DB'
I3C2DCR          EQU  H'00DC'
I3C2DSTAT0       EQU  H'00DD'
I3C2DSTAT1       EQU  H'00DE'
I3C2MWS          EQU  H'00DF'
I3C2MRS          EQU  H'00E0'
I3C2MRT          EQU  H'00E1'
I3C2MRTL         EQU  H'00E1'
I3C2MRTH         EQU  H'00E2'
I3C2MRTU         EQU  H'00E3'
I3C2RSTACT       EQU  H'00E4'
I3C2BUSCXT       EQU  H'00E5'
I3C2CCC          EQU  H'00E6'
I3C2I2CCON       EQU  H'00E7'
I3C2CLK          EQU  H'00E8'
CLKRCON          EQU  H'0100'
CLKRCLK          EQU  H'0101'
TMR0L            EQU  H'0103'
TMR0             EQU  H'0103'
TMR0H            EQU  H'0104'
PR0              EQU  H'0104'
T0CON0           EQU  H'0105'
T0CON1           EQU  H'0106'
TMR1             EQU  H'0107'
TMR1L            EQU  H'0107'
TMR1H            EQU  H'0108'
T1CON            EQU  H'0109'
TMR1CON          EQU  H'0109'
T1GCON           EQU  H'010A'
TMR1GCON         EQU  H'010A'
T1GATE           EQU  H'010B'
TMR1GATE         EQU  H'010B'
T1CLK            EQU  H'010C'
TMR1CLK          EQU  H'010C'
PR1              EQU  H'010C'
T2TMR            EQU  H'0119'
TMR2             EQU  H'0119'
T2PR             EQU  H'011A'
PR2              EQU  H'011A'
T2CON            EQU  H'011B'
T2HLT            EQU  H'011C'
T2CLKCON         EQU  H'011D'
T2CLK            EQU  H'011D'
T2RST            EQU  H'011E'
T4TMR            EQU  H'011F'
TMR4             EQU  H'011F'
T4PR             EQU  H'0120'
PR4              EQU  H'0120'
T4CON            EQU  H'0121'
T4HLT            EQU  H'0122'
T4CLKCON         EQU  H'0123'
T4CLK            EQU  H'0123'
T4RST            EQU  H'0124'
TUCHAIN          EQU  H'012B'
TU16ACON0        EQU  H'012C'
TU16ACON1        EQU  H'012D'
TU16AHLT         EQU  H'012E'
TU16APS          EQU  H'012F'
TU16ATMR         EQU  H'0130'
TU16ACR          EQU  H'0130'
TU16ATMRL        EQU  H'0130'
TU16ACRL         EQU  H'0130'
TU16ATMRH        EQU  H'0131'
TU16ACRH         EQU  H'0131'
TU16ATMRT        EQU  H'0133'
TU16ACRT         EQU  H'0133'
TU16APR          EQU  H'0134'
TU16APRL         EQU  H'0134'
TU16APRH         EQU  H'0135'
TU16APRT         EQU  H'0137'
TU16ACLK         EQU  H'0138'
TU16AERS         EQU  H'0139'
TU16BCON0        EQU  H'013A'
TU16BCON1        EQU  H'013B'
TU16BHLT         EQU  H'013C'
TU16BPS          EQU  H'013D'
TU16BTMR         EQU  H'013E'
TU16BCR          EQU  H'013E'
TU16BTMRL        EQU  H'013E'
TU16BCRL         EQU  H'013E'
TU16BTMRH        EQU  H'013F'
TU16BCRH         EQU  H'013F'
TU16BTMRT        EQU  H'0141'
TU16BCRT         EQU  H'0141'
TU16BPR          EQU  H'0142'
TU16BPRL         EQU  H'0142'
TU16BPRH         EQU  H'0143'
TU16BPRT         EQU  H'0145'
TU16BCLK         EQU  H'0146'
TU16BERS         EQU  H'0147'
CCPTMRS0         EQU  H'0148'
CCPR1            EQU  H'0149'
CCPR1L           EQU  H'0149'
CCPR1H           EQU  H'014A'
CCP1CON          EQU  H'014B'
CCP1CAP          EQU  H'014C'
CCPR2            EQU  H'014D'
CCPR2L           EQU  H'014D'
CCPR2H           EQU  H'014E'
CCP2CON          EQU  H'014F'
CCP2CAP          EQU  H'0150'
PWMLOAD          EQU  H'0151'
PWMEN            EQU  H'0152'
PWM1ERS          EQU  H'0153'
PWM1CLK          EQU  H'0154'
PWM1LDS          EQU  H'0155'
PWM1PR           EQU  H'0156'
PWM1PRL          EQU  H'0156'
PWM1PRH          EQU  H'0157'
PWM1CPRE         EQU  H'0158'
PWM1PIPOS        EQU  H'0159'
PWM1GIR          EQU  H'015A'
PWM1GIE          EQU  H'015B'
PWM1CON          EQU  H'015C'
PWM1S1CFG        EQU  H'015D'
PWM1S1P1         EQU  H'015E'
PWM1S1P1L        EQU  H'015E'
PWM1S1P1H        EQU  H'015F'
PWM1S1P2         EQU  H'0160'
PWM1S1P2L        EQU  H'0160'
PWM1S1P2H        EQU  H'0161'
PWM2ERS          EQU  H'0162'
PWM2CLK          EQU  H'0163'
PWM2LDS          EQU  H'0164'
PWM2PR           EQU  H'0165'
PWM2PRL          EQU  H'0165'
PWM2PRH          EQU  H'0166'
PWM2CPRE         EQU  H'0167'
PWM2PIPOS        EQU  H'0168'
PWM2GIR          EQU  H'0169'
PWM2GIE          EQU  H'016A'
PWM2CON          EQU  H'016B'
PWM2S1CFG        EQU  H'016C'
PWM2S1P1         EQU  H'016D'
PWM2S1P1L        EQU  H'016D'
PWM2S1P1H        EQU  H'016E'
PWM2S1P2         EQU  H'016F'
PWM2S1P2L        EQU  H'016F'
PWM2S1P2H        EQU  H'0170'
CWG1CLK          EQU  H'0199'
CWG1CLKCON       EQU  H'0199'
CWG1ISM          EQU  H'019A'
CWG1DAT          EQU  H'019A'
CWG1DBR          EQU  H'019B'
CWG1DBF          EQU  H'019C'
CWG1CON0         EQU  H'019D'
CWG1CON1         EQU  H'019E'
CWG1AS0          EQU  H'019F'
CWG1AS1          EQU  H'01A0'
CWG1STR          EQU  H'01A1'
CLCDATA          EQU  H'01A3'
CLCSELECT        EQU  H'01A4'
CLCNCON          EQU  H'01A5'
CLCNPOL          EQU  H'01A6'
CLCNSEL0         EQU  H'01A7'
CLCNSEL1         EQU  H'01A8'
CLCNSEL2         EQU  H'01A9'
CLCNSEL3         EQU  H'01AA'
CLCNGLS0         EQU  H'01AB'
CLCNGLS1         EQU  H'01AC'
CLCNGLS2         EQU  H'01AD'
CLCNGLS3         EQU  H'01AE'
U1RXB            EQU  H'01AF'
U1RXBL           EQU  H'01AF'
U1RXCHK          EQU  H'01B0'
U1TXB            EQU  H'01B1'
U1TXBL           EQU  H'01B1'
U1TXCHK          EQU  H'01B2'
U1P1             EQU  H'01B3'
U1P1L            EQU  H'01B3'
U1P1H            EQU  H'01B4'
U1P2             EQU  H'01B5'
U1P2L            EQU  H'01B5'
U1P2H            EQU  H'01B6'
U1P3             EQU  H'01B7'
U1P3L            EQU  H'01B7'
U1P3H            EQU  H'01B8'
U1CON0           EQU  H'01B9'
U1CON1           EQU  H'01BA'
U1CON2           EQU  H'01BB'
U1BRG            EQU  H'01BC'
U1BRGL           EQU  H'01BC'
U1BRGH           EQU  H'01BD'
U1FIFO           EQU  H'01BE'
U1UIR            EQU  H'01C0'
U1ERRIR          EQU  H'01C1'
U1ERRIE          EQU  H'01C2'
U2RXB            EQU  H'01C3'
U2RXBL           EQU  H'01C3'
U2TXB            EQU  H'01C5'
U2TXBL           EQU  H'01C5'
U2P1             EQU  H'01C7'
U2P1L            EQU  H'01C7'
U2P2             EQU  H'01C9'
U2P2L            EQU  H'01C9'
U2P3             EQU  H'01CB'
U2P3L            EQU  H'01CB'
U2CON0           EQU  H'01CD'
U2CON1           EQU  H'01CE'
U2CON2           EQU  H'01CF'
U2BRG            EQU  H'01D0'
U2BRGL           EQU  H'01D0'
U2BRGH           EQU  H'01D1'
U2FIFO           EQU  H'01D2'
U2UIR            EQU  H'01D4'
U2ERRIR          EQU  H'01D5'
U2ERRIE          EQU  H'01D6'
SPI1RXB          EQU  H'01D7'
SPI1TXB          EQU  H'01D8'
SPI1TCNT         EQU  H'01D9'
SPI1TCNTL        EQU  H'01D9'
SPI1TCNTH        EQU  H'01DA'
SPI1CON0         EQU  H'01DB'
SPI1CON1         EQU  H'01DC'
SPI1CON2         EQU  H'01DD'
SPI1STATUS       EQU  H'01DE'
SPI1TWIDTH       EQU  H'01DF'
SPI1BAUD         EQU  H'01E0'
SPI1INTF         EQU  H'01E1'
SPI1INTE         EQU  H'01E2'
SPI1CLK          EQU  H'01E3'
I2C1STAT0        EQU  H'01E4'
I2C1STAT1        EQU  H'01E5'
I2C1CON0         EQU  H'01E6'
I2C1CON1         EQU  H'01E7'
I2C1CON2         EQU  H'01E8'
I2C1CON3         EQU  H'01E9'
I2C1PIR          EQU  H'01EA'
I2C1PIE          EQU  H'01EB'
I2C1ERR          EQU  H'01EC'
I2C1CNT          EQU  H'01ED'
I2C1CNTL         EQU  H'01ED'
I2C1CNTH         EQU  H'01EE'
I2C1RXB          EQU  H'01EF'
I2C1TXB          EQU  H'01F0'
I2C1ADB0         EQU  H'01F1'
I2C1ADB1         EQU  H'01F2'
I2C1ADR0         EQU  H'01F3'
I2C1ADR1         EQU  H'01F4'
I2C1ADR2         EQU  H'01F5'
I2C1ADR3         EQU  H'01F6'
I2C1BTO          EQU  H'01F7'
I2C1BAUD         EQU  H'01F8'
I2C1CLK          EQU  H'01F9'
I2C1BTOC         EQU  H'01FA'
FVRCON           EQU  H'0201'
HLVDCON0         EQU  H'0202'
HLVDCON1         EQU  H'0203'
ADLTH            EQU  H'0215'
ADLTHL           EQU  H'0215'
ADLTHH           EQU  H'0216'
ADUTH            EQU  H'0217'
ADUTHL           EQU  H'0217'
ADUTHH           EQU  H'0218'
ADERR            EQU  H'0219'
ADERRL           EQU  H'0219'
ADERRH           EQU  H'021A'
ADSTPT           EQU  H'021B'
ADSTPTL          EQU  H'021B'
ADSTPTH          EQU  H'021C'
ADFLTR           EQU  H'021D'
ADFLTRL          EQU  H'021D'
ADFLTRH          EQU  H'021E'
ADACC            EQU  H'021F'
ADACCL           EQU  H'021F'
ADACCH           EQU  H'0220'
ADCNT            EQU  H'0222'
ADRPT            EQU  H'0223'
ADPREV           EQU  H'0224'
ADPREVL          EQU  H'0224'
ADPREVH          EQU  H'0225'
ADRES            EQU  H'0226'
ADRESL           EQU  H'0226'
ADRESH           EQU  H'0227'
ADPCH            EQU  H'0228'
ADNCH            EQU  H'0229'
ADACQ            EQU  H'022A'
ADACQL           EQU  H'022A'
ADACQH           EQU  H'022B'
ADCAP            EQU  H'022C'
ADPRE            EQU  H'022D'
ADPREL           EQU  H'022D'
ADPREH           EQU  H'022E'
ADCON0           EQU  H'022F'
ADCON1           EQU  H'0230'
ADCON2           EQU  H'0231'
ADCON3           EQU  H'0232'
ADSTAT           EQU  H'0233'
ADREF            EQU  H'0234'
ADACT            EQU  H'0235'
ADCLK            EQU  H'0236'
ADCG1A           EQU  H'0237'
ADCG1B           EQU  H'0238'
ADCG1C           EQU  H'0239'
PMD0             EQU  H'0300'
PMD1             EQU  H'0301'
PMD2             EQU  H'0302'
PMD3             EQU  H'0303'
PMD4             EQU  H'0304'
RB5FEAT          EQU  H'030F'
RB6FEAT          EQU  H'0310'
RC0FEAT          EQU  H'0312'
RC1FEAT          EQU  H'0313'
RC4FEAT          EQU  H'0315'
RC5FEAT          EQU  H'0316'
RA0PPS           EQU  H'0319'
RA1PPS           EQU  H'031A'
RA2PPS           EQU  H'031B'
RA4PPS           EQU  H'031D'
RA5PPS           EQU  H'031E'
RB5PPS           EQU  H'0326'
RB6PPS           EQU  H'0327'
RB7PPS           EQU  H'0328'
RC0PPS           EQU  H'0329'
RC1PPS           EQU  H'032A'
RC3PPS           EQU  H'032C'
RC4PPS           EQU  H'032D'
RC5PPS           EQU  H'032E'
RC6PPS           EQU  H'032F'
RC7PPS           EQU  H'0330'
PPSLOCK          EQU  H'0351'
INT0PPS          EQU  H'0352'
INT1PPS          EQU  H'0353'
INT2PPS          EQU  H'0354'
T0CKIPPS         EQU  H'0355'
T1CKIPPS         EQU  H'0356'
T1GPPS           EQU  H'0357'
T2INPPS          EQU  H'0360'
T4INPPS          EQU  H'0361'
TUIN0PPS         EQU  H'0365'
TUIN1PPS         EQU  H'0366'
CCP1PPS          EQU  H'0369'
CCP2PPS          EQU  H'036A'
STATUS_CSHAD     EQU  H'0373'
WREG_CSHAD       EQU  H'0374'
BSR_CSHAD        EQU  H'0375'
SHADCON          EQU  H'0376'
STATUS_SHAD      EQU  H'0377'
WREG_SHAD        EQU  H'0378'
BSR_SHAD         EQU  H'0379'
PCLAT_SHAD       EQU  H'037A'
PCLATH_SHAD      EQU  H'037A'
PCLATU_SHAD      EQU  H'037B'
FSR0_SHAD        EQU  H'037C'
FSR0L_SHAD       EQU  H'037C'
FSR0H_SHAD       EQU  H'037D'
FSR1_SHAD        EQU  H'037E'
FSR1L_SHAD       EQU  H'037E'
FSR1H_SHAD       EQU  H'037F'
FSR2_SHAD        EQU  H'0380'
FSR2L_SHAD       EQU  H'0380'
FSR2H_SHAD       EQU  H'0381'
PROD_SHAD        EQU  H'0382'
PRODL_SHAD       EQU  H'0382'
PRODH_SHAD       EQU  H'0383'
PWMIN0PPS        EQU  H'0384'
PWMIN1PPS        EQU  H'0385'
PWM1ERSPPS       EQU  H'0386'
PWM2ERSPPS       EQU  H'0387'
CWG1PPS          EQU  H'0399'
CWG1INPPS        EQU  H'0399'
CLCIN0PPS        EQU  H'039D'
CLCIN1PPS        EQU  H'039E'
CLCIN2PPS        EQU  H'039F'
CLCIN3PPS        EQU  H'03A0'
U1CTSPPS         EQU  H'03A1'
U1RXPPS          EQU  H'03A2'
U2CTSPPS         EQU  H'03A3'
U2RXPPS          EQU  H'03A4'
SPI1SCKPPS       EQU  H'03A7'
SPI1SDIPPS       EQU  H'03A8'
SPI1SSPPS        EQU  H'03A9'
I2C1SCLPPS       EQU  H'03AD'
I2C1SDAPPS       EQU  H'03AE'
ADACTPPS         EQU  H'03B1'
ANSELA           EQU  H'0416'
WPUA             EQU  H'0417'
ODCONA           EQU  H'0418'
SLRCONA          EQU  H'0419'
INLVLA           EQU  H'041A'
IOCAP            EQU  H'041B'
IOCAN            EQU  H'041C'
IOCAF            EQU  H'041D'
ANSELB           EQU  H'0420'
WPUB             EQU  H'0421'
ODCONB           EQU  H'0422'
SLRCONB          EQU  H'0423'
INLVLB           EQU  H'0424'
IOCBP            EQU  H'0425'
IOCBN            EQU  H'0426'
IOCBF            EQU  H'0427'
ANSELC           EQU  H'042A'
WPUC             EQU  H'042B'
ODCONC           EQU  H'042C'
SLRCONC          EQU  H'042D'
INLVLC           EQU  H'042E'
IOCCP            EQU  H'042F'
IOCCN            EQU  H'0430'
IOCCF            EQU  H'0431'
IOCWP            EQU  H'0457'
IOCWN            EQU  H'0458'
IOCWF            EQU  H'0459'
IVTLOCK          EQU  H'0460'
INTCON0          EQU  H'0461'
INTCON1          EQU  H'0462'
IVTAD            EQU  H'0463'
IVTADL           EQU  H'0463'
IVTADH           EQU  H'0464'
IVTADU           EQU  H'0465'
IVTBASE          EQU  H'0466'
IVTBASEL         EQU  H'0466'
IVTBASEH         EQU  H'0467'
IVTBASEU         EQU  H'0468'
PIR0             EQU  H'0469'
PIR1             EQU  H'046A'
PIR2             EQU  H'046B'
PIR3             EQU  H'046C'
PIR4             EQU  H'046D'
PIR5             EQU  H'046E'
PIR6             EQU  H'046F'
PIR7             EQU  H'0470'
PIR8             EQU  H'0471'
PIR9             EQU  H'0472'
PIE0             EQU  H'0473'
PIE1             EQU  H'0474'
PIE2             EQU  H'0475'
PIE3             EQU  H'0476'
PIE4             EQU  H'0477'
PIE5             EQU  H'0478'
PIE6             EQU  H'0479'
PIE7             EQU  H'047A'
PIE8             EQU  H'047B'
PIE9             EQU  H'047C'
IPR0             EQU  H'047D'
IPR1             EQU  H'047E'
IPR2             EQU  H'047F'
IPR3             EQU  H'0480'
IPR4             EQU  H'0481'
IPR5             EQU  H'0482'
IPR6             EQU  H'0483'
IPR7             EQU  H'0484'
IPR8             EQU  H'0485'
IPR9             EQU  H'0486'
PORTA            EQU  H'0487'
PORTB            EQU  H'0488'
PORTC            EQU  H'0489'
TRISA            EQU  H'048D'
TRISB            EQU  H'048E'
TRISC            EQU  H'048F'
LATA             EQU  H'0493'
LATB             EQU  H'0494'
LATC             EQU  H'0495'
PORTW            EQU  H'0499'
LATW             EQU  H'049A'
PORTWIN0         EQU  H'049B'
PORTWIN1         EQU  H'049C'
PORTWIN2         EQU  H'049D'
PORTWIN3         EQU  H'049E'
PORTWIN4         EQU  H'049F'
PORTWIN5         EQU  H'04A0'
PORTWIN6         EQU  H'04A1'
PORTWIN7         EQU  H'04A2'
PORTWCLK         EQU  H'04A3'
PORTWDF          EQU  H'04A4'
PORTWCON         EQU  H'04A5'
MVIOSTAT         EQU  H'04A6'
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


;----- CRCCON1 Bits -----------------------------------------------------
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
PLEN4            EQU  H'0004'
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
PLEN4            EQU  H'0004'


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


;----- I3C1PID1 Bits -----------------------------------------------------
PID8             EQU  H'0000'
PID9             EQU  H'0001'
PID10            EQU  H'0002'
PID11            EQU  H'0003'
PID12            EQU  H'0004'
PID13            EQU  H'0005'
PID14            EQU  H'0006'
PID15            EQU  H'0007'
PID_11_80        EQU  H'0000'
PID_11_81        EQU  H'0001'
PID_11_82        EQU  H'0002'
PID_11_83        EQU  H'0003'
PID_15_120       EQU  H'0004'
PID_15_121       EQU  H'0005'
PID_15_122       EQU  H'0006'
PID_15_123       EQU  H'0007'


;----- I3C1CON0 Bits -----------------------------------------------------
IBIREQ           EQU  H'0000'
HJREQ            EQU  H'0001'
ACKP             EQU  H'0002'
CLRRXB           EQU  H'0003'
CLRTXB           EQU  H'0004'
RST              EQU  H'0005'
BTOEN            EQU  H'0006'
EN               EQU  H'0007'


;----- I3C1CON1 Bits -----------------------------------------------------
ACKPOS           EQU  H'0000'
SASDRMD          EQU  H'0001'
FHDRE            EQU  H'0002'
BERRDET          EQU  H'0003'


;----- I3C1RXB Bits -----------------------------------------------------


;----- I3C1TXB Bits -----------------------------------------------------


;----- I3C1STAT0 Bits -----------------------------------------------------
RNW0             EQU  H'0000'
RNW1             EQU  H'0001'
RXBF             EQU  H'0002'
TXBE             EQU  H'0003'
RSTDET           EQU  H'0004'
OPMD0            EQU  H'0005'
OPMD1            EQU  H'0006'
BFREE            EQU  H'0007'


;----- I3C1STAT1 Bits -----------------------------------------------------
TXFNE            EQU  H'0000'


;----- I3C1BSTAT Bits -----------------------------------------------------
TE0ERR           EQU  H'0000'
TE1ERR           EQU  H'0001'
TE2ERR           EQU  H'0002'
TE3ERR           EQU  H'0003'
TE4ERR           EQU  H'0004'
TE5ERR           EQU  H'0005'
TE6ERR           EQU  H'0006'


;----- I3C1PIR0 Bits -----------------------------------------------------
SCCCIF           EQU  H'0000'
BTFIF            EQU  H'0001'
DADRIF           EQU  H'0002'
SADRIF           EQU  H'0003'
I2CACKIF         EQU  H'0004'
RSCIF            EQU  H'0005'
PCIF             EQU  H'0006'
SCIF             EQU  H'0007'


;----- I3C1PIR1 Bits -----------------------------------------------------
IBIDONEIF        EQU  H'0005'
DACHIF           EQU  H'0006'
TCOMPIF          EQU  H'0007'


;----- I3C1ERRIR0 Bits -----------------------------------------------------
UCCCIF           EQU  H'0000'
BTOIF            EQU  H'0001'
BUSEIF           EQU  H'0002'
IBIEIF           EQU  H'0003'
HJEIF            EQU  H'0004'
RXOIF            EQU  H'0005'
TXUIF            EQU  H'0006'
I2CNACKIF        EQU  H'0007'


;----- I3C1ERRIR1 Bits -----------------------------------------------------
RXREIF           EQU  H'0000'
TXWEIF           EQU  H'0001'
MWLOEIF          EQU  H'0002'
ABEIF            EQU  H'0003'


;----- I3C1PIE0 Bits -----------------------------------------------------
SCCCIE           EQU  H'0000'
BTFIE            EQU  H'0001'
DADRIE           EQU  H'0002'
SADRIE           EQU  H'0003'
I2CACKIE         EQU  H'0004'
RSCIE            EQU  H'0005'
PCIE             EQU  H'0006'
SCIE             EQU  H'0007'


;----- I3C1PIE1 Bits -----------------------------------------------------
IBIDONEIE        EQU  H'0005'
DACHIE           EQU  H'0006'
TCOMPIE          EQU  H'0007'


;----- I3C1ERRIE0 Bits -----------------------------------------------------
UCCCIE           EQU  H'0000'
BTOIE            EQU  H'0001'
BUSEIE           EQU  H'0002'
IBIEIE           EQU  H'0003'
HJEIE            EQU  H'0004'
RXOIE            EQU  H'0005'
TXUIE            EQU  H'0006'
I2CNACKIE        EQU  H'0007'


;----- I3C1ERRIE1 Bits -----------------------------------------------------
RXREIE           EQU  H'0000'
TXWEIE           EQU  H'0001'
MWLOEIE          EQU  H'0002'
ABEIE            EQU  H'0003'


;----- I3C1BIDL Bits -----------------------------------------------------


;----- I3C1BIDLH Bits -----------------------------------------------------


;----- I3C1BAVL Bits -----------------------------------------------------


;----- I3C1BTO Bits -----------------------------------------------------


;----- I3C1BTOH Bits -----------------------------------------------------


;----- I3C1IBIMDB Bits -----------------------------------------------------
IBIMDB_4_00      EQU  H'0000'
IBIMDB_4_01      EQU  H'0001'
IBIMDB_4_02      EQU  H'0002'
IBIMDB_4_03      EQU  H'0003'
IBIMDB_4_04      EQU  H'0004'
IBIMDB_7_50      EQU  H'0005'
IBIMDB_7_51      EQU  H'0006'
IBIMDB_7_52      EQU  H'0007'


;----- I3C1RETRY Bits -----------------------------------------------------


;----- I3C1FEAT Bits -----------------------------------------------------
HJCAP            EQU  H'0000'
HDRCAP           EQU  H'0001'


;----- I3C1SADR Bits -----------------------------------------------------
SADR0            EQU  H'0000'
SADR1            EQU  H'0001'
SADR2            EQU  H'0002'
SADR3            EQU  H'0003'
SADR4            EQU  H'0004'
SADR5            EQU  H'0005'
SADR6            EQU  H'0006'


;----- I3C1DADR Bits -----------------------------------------------------
DADR0            EQU  H'0000'
DADR1            EQU  H'0001'
DADR2            EQU  H'0002'
DADR3            EQU  H'0003'
DADR4            EQU  H'0004'
DADR5            EQU  H'0005'
DADR6            EQU  H'0006'


;----- I3C1EC Bits -----------------------------------------------------
IBIEN            EQU  H'0000'
CREN             EQU  H'0001'
EC2              EQU  H'0002'
HJEN             EQU  H'0003'
EC_7_40          EQU  H'0004'
EC_7_41          EQU  H'0005'
EC_7_42          EQU  H'0006'
EC_7_43          EQU  H'0007'


;----- I3C1MWL Bits -----------------------------------------------------


;----- I3C1MWLH Bits -----------------------------------------------------


;----- I3C1MRL Bits -----------------------------------------------------


;----- I3C1MRLH Bits -----------------------------------------------------


;----- I3C1IBIPSZ Bits -----------------------------------------------------


;----- I3C1PID0 Bits -----------------------------------------------------
PID0             EQU  H'0000'
PID1             EQU  H'0001'
PID2             EQU  H'0002'
PID3             EQU  H'0003'
PID4             EQU  H'0004'
PID5             EQU  H'0005'
PID6             EQU  H'0006'
PID7             EQU  H'0007'


;----- I3C1PID2 Bits -----------------------------------------------------
PID16            EQU  H'0000'
PID17            EQU  H'0001'
PID18            EQU  H'0002'
PID19            EQU  H'0003'
PID20            EQU  H'0004'
PID21            EQU  H'0005'
PID22            EQU  H'0006'
PID23            EQU  H'0007'


;----- I3C1PID3 Bits -----------------------------------------------------
PID24            EQU  H'0000'
PID25            EQU  H'0001'
PID26            EQU  H'0002'
PID27            EQU  H'0003'
PID28            EQU  H'0004'
PID29            EQU  H'0005'
PID30            EQU  H'0006'
PID31            EQU  H'0007'


;----- I3C1PID4 Bits -----------------------------------------------------
PID32            EQU  H'0000'
PID33            EQU  H'0001'
PID34            EQU  H'0002'
PID35            EQU  H'0003'
PID36            EQU  H'0004'
PID37            EQU  H'0005'
PID38            EQU  H'0006'
PID39            EQU  H'0007'
PID_39_330       EQU  H'0001'
PID_39_331       EQU  H'0002'
PID_39_332       EQU  H'0003'
PID_39_333       EQU  H'0004'
PID_39_334       EQU  H'0005'
PID_39_335       EQU  H'0006'
PID_39_336       EQU  H'0007'


;----- I3C1PID5 Bits -----------------------------------------------------
PID40            EQU  H'0000'
PID41            EQU  H'0001'
PID42            EQU  H'0002'
PID43            EQU  H'0003'
PID44            EQU  H'0004'
PID45            EQU  H'0005'
PID46            EQU  H'0006'
PID47            EQU  H'0007'


;----- I3C1BCR Bits -----------------------------------------------------
BCR0             EQU  H'0000'
BCR1             EQU  H'0001'
BCR2             EQU  H'0002'
BCR3             EQU  H'0003'
BCR4             EQU  H'0004'
BCR5             EQU  H'0005'
BCR6             EQU  H'0006'
BCR7             EQU  H'0007'
BCR_7_60         EQU  H'0006'
BCR_7_61         EQU  H'0007'


;----- I3C1DCR Bits -----------------------------------------------------


;----- I3C1DSTAT0 Bits -----------------------------------------------------
INTPEND0         EQU  H'0000'
INTPEND1         EQU  H'0001'
INTPEND2         EQU  H'0002'
INTPEND3         EQU  H'0003'
PERR             EQU  H'0005'
ACTMODE0         EQU  H'0006'
ACTMODE1         EQU  H'0007'
INPEND0          EQU  H'0000'
INTPEND1         EQU  H'0001'
INTPEND2         EQU  H'0002'
INTPEND3         EQU  H'0003'
ACTMODE0         EQU  H'0006'
ACTMODE1         EQU  H'0007'


;----- I3C1DSTAT1 Bits -----------------------------------------------------
VRSV0            EQU  H'0000'
VRSV1            EQU  H'0001'
VRSV2            EQU  H'0002'
VRSV3            EQU  H'0003'
VRSV4            EQU  H'0004'
VRSV5            EQU  H'0005'
VRSV6            EQU  H'0006'
VRSV7            EQU  H'0007'


;----- I3C1MWS Bits -----------------------------------------------------
MWS0             EQU  H'0000'
MWS1             EQU  H'0001'
MWS2             EQU  H'0002'
MWS3             EQU  H'0003'
MWS4             EQU  H'0004'
MWS5             EQU  H'0005'
MWS6             EQU  H'0006'
MWS7             EQU  H'0007'
MWS_2_00         EQU  H'0000'
MWS_2_01         EQU  H'0001'
MWS_2_02         EQU  H'0002'
MWS_7_40         EQU  H'0004'
MWS_7_41         EQU  H'0005'
MWS_7_42         EQU  H'0006'
MWS_7_43         EQU  H'0007'


;----- I3C1MRS Bits -----------------------------------------------------
MRS0             EQU  H'0000'
MRS1             EQU  H'0001'
MRS2             EQU  H'0002'
MRS3             EQU  H'0003'
MRS4             EQU  H'0004'
MRS5             EQU  H'0005'
MRS6             EQU  H'0006'
MRS7             EQU  H'0007'
MRS_2_00         EQU  H'0000'
MRS_2_01         EQU  H'0001'
MRS_2_02         EQU  H'0002'
MRS_5_30         EQU  H'0003'
MRS_5_31         EQU  H'0004'
MRS_5_32         EQU  H'0005'


;----- I3C1MRT Bits -----------------------------------------------------


;----- I3C1MRTH Bits -----------------------------------------------------


;----- I3C1MRTU Bits -----------------------------------------------------


;----- I3C1RSTACT Bits -----------------------------------------------------


;----- I3C1BUSCXT Bits -----------------------------------------------------


;----- I3C1CCC Bits -----------------------------------------------------


;----- I3C1I2CCON Bits -----------------------------------------------------
SDAHT0           EQU  H'0000'
SDAHT1           EQU  H'0001'
FLTEN            EQU  H'0002'


;----- I3C1CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


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
CCP1CTS0         EQU  H'0000'
CCP1CTS1         EQU  H'0001'
CCP1CTS2         EQU  H'0002'


;----- PWM1S1P1 Bits -----------------------------------------------------


;----- PWM1S1P1H Bits -----------------------------------------------------


;----- PWM1ERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'


;----- PWM1CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


;----- PWM1LDS Bits -----------------------------------------------------
LDS0             EQU  H'0000'
LDS1             EQU  H'0001'
LDS2             EQU  H'0002'
LDS3             EQU  H'0003'


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


;----- PWM1S1P2 Bits -----------------------------------------------------


;----- PWM1S1P2H Bits -----------------------------------------------------


;----- CLCNSEL1 Bits -----------------------------------------------------
D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'


;----- CLCNGLS1 Bits -----------------------------------------------------
G2D1N            EQU  H'0000'
G2D1T            EQU  H'0001'
G2D2N            EQU  H'0002'
G2D2T            EQU  H'0003'
G2D3N            EQU  H'0004'
G2D3T            EQU  H'0005'
G2D4N            EQU  H'0006'
G2D4T            EQU  H'0007'


;----- U1P1 Bits -----------------------------------------------------


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


;----- U1RXB Bits -----------------------------------------------------


;----- U1TXB Bits -----------------------------------------------------


;----- U1P2 Bits -----------------------------------------------------


;----- U1P3 Bits -----------------------------------------------------


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


;----- ADCON1 Bits -----------------------------------------------------
DSEN             EQU  H'0000'
PCSC             EQU  H'0001'
GPOL_ADCON1      EQU  H'0005'
IPEN_ADCON1      EQU  H'0006'
PPOL             EQU  H'0007'
ADDSEN           EQU  H'0000'
ADPCSC           EQU  H'0001'
ADGPOL           EQU  H'0005'
ADIPEN           EQU  H'0006'
ADPPOL           EQU  H'0007'


;----- PMD1 Bits -----------------------------------------------------
ACTMD            EQU  H'0000'
PORTWMD          EQU  H'0001'
IOCMD            EQU  H'0002'
CLKRMD           EQU  H'0003'
TMR0MD           EQU  H'0004'
TMR1MD           EQU  H'0005'
TMR2MD           EQU  H'0006'
TMR4MD           EQU  H'0007'


;----- RC1FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SYSBUF2          EQU  H'0002'
I3CBUF0          EQU  H'0003'
I3CBUF1          EQU  H'0004'
I3CBUF2          EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RA1PPS Bits -----------------------------------------------------
RA1PPS0          EQU  H'0000'
RA1PPS1          EQU  H'0001'
RA1PPS2          EQU  H'0002'
RA1PPS3          EQU  H'0003'
RA1PPS4          EQU  H'0004'


;----- RC1PPS Bits -----------------------------------------------------
RC1PPS0          EQU  H'0000'
RC1PPS1          EQU  H'0001'
RC1PPS2          EQU  H'0002'
RC1PPS3          EQU  H'0003'
RC1PPS4          EQU  H'0004'


;----- INT1PPS Bits -----------------------------------------------------
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


;----- CCP1PPS Bits -----------------------------------------------------
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'


;----- FSR1_SHAD Bits -----------------------------------------------------
FSR1_SHAD0       EQU  H'0000'
FSR1_SHAD1       EQU  H'0001'
FSR1_SHAD2       EQU  H'0002'
FSR1_SHAD3       EQU  H'0003'
FSR1_SHAD4       EQU  H'0004'
FSR1_SHAD5       EQU  H'0005'
FSR1_SHAD6       EQU  H'0006'
FSR1_SHAD7       EQU  H'0007'
FSR1_SHAD8       EQU  H'0008'
FSR1_SHAD9       EQU  H'0009'
FSR1_SHAD10      EQU  H'0010'
FSR1_SHAD11      EQU  H'0011'
FSR1_SHAD12      EQU  H'0012'
FSR1_SHAD13      EQU  H'0013'


;----- FSR1H_SHAD Bits -----------------------------------------------------
FSR1H0           EQU  H'0000'
FSR1H1           EQU  H'0001'
FSR1H2           EQU  H'0002'
FSR1H3           EQU  H'0003'
FSR1H4           EQU  H'0004'
FSR1H5           EQU  H'0005'


;----- PWM1ERSPPS Bits -----------------------------------------------------
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


;----- CLCIN1PPS Bits -----------------------------------------------------
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


;----- U1CTSPPS Bits -----------------------------------------------------
U1CTSPPS0        EQU  H'0000'
U1CTSPPS1        EQU  H'0001'
U1CTSPPS2        EQU  H'0002'
U1CTSPPS3        EQU  H'0003'
U1CTSPPS4        EQU  H'0004'
U1CTSPPS0        EQU  H'0000'
U1CTSPPS1        EQU  H'0001'
U1CTSPPS2        EQU  H'0002'
U1CTSPPS3        EQU  H'0003'
U1CTSPPS4        EQU  H'0004'


;----- U1RXPPS Bits -----------------------------------------------------
U1RXPPS0         EQU  H'0000'
U1RXPPS1         EQU  H'0001'
U1RXPPS2         EQU  H'0002'
U1RXPPS3         EQU  H'0003'
U1RXPPS4         EQU  H'0004'
U1RXPPS0         EQU  H'0000'
U1RXPPS1         EQU  H'0001'
U1RXPPS2         EQU  H'0002'
U1RXPPS3         EQU  H'0003'
U1RXPPS4         EQU  H'0004'


;----- I3C2PIR1 Bits -----------------------------------------------------
IBIDONEIF        EQU  H'0005'
DACHIF           EQU  H'0006'
TCOMPIF          EQU  H'0007'


;----- PIR1 Bits -----------------------------------------------------
DMA2SCNTIF       EQU  H'0000'
DMA2DCNTIF       EQU  H'0001'
DMA2ORIF         EQU  H'0002'
DMA2AIF          EQU  H'0003'
DMA3SCNTIF       EQU  H'0004'
DMA3DCNTIF       EQU  H'0005'
DMA3ORIF         EQU  H'0006'
DMA3AIF          EQU  H'0007'


;----- I3C2PIE1 Bits -----------------------------------------------------
IBIDONEIE        EQU  H'0005'
DACHIE           EQU  H'0006'
TCOMPIE          EQU  H'0007'


;----- PIE1 Bits -----------------------------------------------------
DMA2SCNTIE       EQU  H'0000'
DMA2DCNTIE       EQU  H'0001'
DMA2ORIE         EQU  H'0002'
DMA2AIE          EQU  H'0003'
DMA3SCNTIE       EQU  H'0004'
DMA3DCNTIE       EQU  H'0005'
DMA3ORIE         EQU  H'0006'
DMA3AIE          EQU  H'0007'


;----- IPR1 Bits -----------------------------------------------------
DMA2SCNTIP       EQU  H'0000'
DMA2DCNTIP       EQU  H'0001'
DMA2ORIP         EQU  H'0002'
DMA2AIP          EQU  H'0003'
DMA3SCNTIP       EQU  H'0004'
DMA3DCNTIP       EQU  H'0005'
DMA3ORIP         EQU  H'0006'
DMA3AIP          EQU  H'0007'


;----- PORTWIN1 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


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
FSR112           EQU  H'0012'
FSR113           EQU  H'0013'


;----- FSR1H Bits -----------------------------------------------------
FSR1H0           EQU  H'0000'
FSR1H1           EQU  H'0001'
FSR1H2           EQU  H'0002'
FSR1H3           EQU  H'0003'
FSR1H4           EQU  H'0004'
FSR1H5           EQU  H'0005'


;----- PLUSW1 Bits -----------------------------------------------------


;----- PREINC1 Bits -----------------------------------------------------


;----- POSTDEC1 Bits -----------------------------------------------------


;----- POSTINC1 Bits -----------------------------------------------------


;----- INDF1 Bits -----------------------------------------------------


;----- PRLOCK Bits -----------------------------------------------------
PRLOCKED         EQU  H'0000'


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
SHIFTM           EQU  H'0001'
SETUP0           EQU  H'0002'
SETUP1           EQU  H'0003'
ACCMOD           EQU  H'0004'
CRCBUSY          EQU  H'0005'
CRCGO            EQU  H'0006'
CRCEN            EQU  H'0007'
LENDIAN          EQU  H'0001'
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


;----- BORCON Bits -----------------------------------------------------
BORRDY           EQU  H'0000'
SBOREN           EQU  H'0007'


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


;----- VREGCON Bits -----------------------------------------------------
VREGPM0          EQU  H'0000'
VREGPM1          EQU  H'0001'
PMSYS0           EQU  H'0004'
PMSYS1           EQU  H'0005'
VREGPM0          EQU  H'0000'
VREGPM1          EQU  H'0001'
PMSYS0           EQU  H'0004'
PMSYS1           EQU  H'0005'


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


;----- OSCSTAT Bits -----------------------------------------------------
PLLR             EQU  H'0000'
SFOR             EQU  H'0001'
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
FRQ0             EQU  H'0000'
FRQ1             EQU  H'0001'
FRQ2             EQU  H'0002'
FRQ3             EQU  H'0003'
FRQ0             EQU  H'0000'
FRQ1             EQU  H'0001'
FRQ2             EQU  H'0002'
FRQ3             EQU  H'0003'


;----- ACTCON Bits -----------------------------------------------------
ORS              EQU  H'0001'
LOCK             EQU  H'0003'
UD               EQU  H'0006'
EN               EQU  H'0007'
ACTORS           EQU  H'0001'
ACTLOCK          EQU  H'0003'
ACTUD            EQU  H'0006'
ACTEN            EQU  H'0007'


;----- FSCMCON Bits -----------------------------------------------------
FSCMFEV          EQU  H'0000'
FSCMFFI          EQU  H'0001'
FSCMPEV          EQU  H'0002'
FSCMPFI          EQU  H'0003'
FSCMSEV          EQU  H'0004'
FSCMSFI          EQU  H'0005'


;----- I3C2CON0 Bits -----------------------------------------------------
IBIREQ           EQU  H'0000'
HJREQ            EQU  H'0001'
ACKP             EQU  H'0002'
CLRRXB           EQU  H'0003'
CLRTXB           EQU  H'0004'
RST              EQU  H'0005'
BTOEN            EQU  H'0006'
EN               EQU  H'0007'


;----- I3C2CON1 Bits -----------------------------------------------------
ACKPOS           EQU  H'0000'
SASDRMD          EQU  H'0001'
FHDRE            EQU  H'0002'
BERRDET          EQU  H'0003'


;----- I3C2RXB Bits -----------------------------------------------------


;----- I3C2TXB Bits -----------------------------------------------------


;----- I3C2STAT0 Bits -----------------------------------------------------
RNW0             EQU  H'0000'
RNW1             EQU  H'0001'
RXBF             EQU  H'0002'
TXBE             EQU  H'0003'
RSTDET           EQU  H'0004'
OPMD0            EQU  H'0005'
OPMD1            EQU  H'0006'
BFREE            EQU  H'0007'


;----- I3C2STAT1 Bits -----------------------------------------------------
TXFNE            EQU  H'0000'


;----- I3C2BSTAT Bits -----------------------------------------------------
TE0ERR           EQU  H'0000'
TE1ERR           EQU  H'0001'
TE2ERR           EQU  H'0002'
TE3ERR           EQU  H'0003'
TE4ERR           EQU  H'0004'
TE5ERR           EQU  H'0005'
TE6ERR           EQU  H'0006'


;----- I3C2PIR0 Bits -----------------------------------------------------
SCCCIF           EQU  H'0000'
BTFIF            EQU  H'0001'
DADRIF           EQU  H'0002'
SADRIF           EQU  H'0003'
I2CACKIF         EQU  H'0004'
RSCIF            EQU  H'0005'
PCIF             EQU  H'0006'
SCIF             EQU  H'0007'


;----- I3C2ERRIR0 Bits -----------------------------------------------------
UCCCIF           EQU  H'0000'
BTOIF            EQU  H'0001'
BUSEIF           EQU  H'0002'
IBIEIF           EQU  H'0003'
HJEIF            EQU  H'0004'
RXOIF            EQU  H'0005'
TXUIF            EQU  H'0006'
I2CNACKIF        EQU  H'0007'


;----- I3C2ERRIR1 Bits -----------------------------------------------------
RXREIF           EQU  H'0000'
TXWEIF           EQU  H'0001'
MWLOEIF          EQU  H'0002'
ABEIF            EQU  H'0003'


;----- I3C2PIE0 Bits -----------------------------------------------------
SCCCIE           EQU  H'0000'
BTFIE            EQU  H'0001'
DADRIE           EQU  H'0002'
SADRIE           EQU  H'0003'
I2CACKIE         EQU  H'0004'
RSCIE            EQU  H'0005'
PCIE             EQU  H'0006'
SCIE             EQU  H'0007'


;----- I3C2ERRIE0 Bits -----------------------------------------------------
UCCCIE           EQU  H'0000'
BTOIE            EQU  H'0001'
BUSEIE           EQU  H'0002'
IBIEIE           EQU  H'0003'
HJEIE            EQU  H'0004'
RXOIE            EQU  H'0005'
TXUIE            EQU  H'0006'
I2CNACKIE        EQU  H'0007'


;----- I3C2ERRIE1 Bits -----------------------------------------------------
RXREIE           EQU  H'0000'
TXWEIE           EQU  H'0001'
MWLOEIE          EQU  H'0002'
ABEIE            EQU  H'0003'


;----- I3C2BIDL Bits -----------------------------------------------------


;----- I3C2BIDLH Bits -----------------------------------------------------


;----- I3C2BAVL Bits -----------------------------------------------------


;----- I3C2BTO Bits -----------------------------------------------------


;----- I3C2BTOH Bits -----------------------------------------------------


;----- I3C2IBIMDB Bits -----------------------------------------------------
IBIMDB_4_00      EQU  H'0000'
IBIMDB_4_01      EQU  H'0001'
IBIMDB_4_02      EQU  H'0002'
IBIMDB_4_03      EQU  H'0003'
IBIMDB_4_04      EQU  H'0004'
IBIMDB_7_50      EQU  H'0005'
IBIMDB_7_51      EQU  H'0006'
IBIMDB_7_52      EQU  H'0007'


;----- I3C2RETRY Bits -----------------------------------------------------


;----- I3C2FEAT Bits -----------------------------------------------------
HJCAP            EQU  H'0000'
HDRCAP           EQU  H'0001'


;----- I3C2SADR Bits -----------------------------------------------------
SADR0            EQU  H'0000'
SADR1            EQU  H'0001'
SADR2            EQU  H'0002'
SADR3            EQU  H'0003'
SADR4            EQU  H'0004'
SADR5            EQU  H'0005'
SADR6            EQU  H'0006'


;----- I3C2DADR Bits -----------------------------------------------------
DADR0            EQU  H'0000'
DADR1            EQU  H'0001'
DADR2            EQU  H'0002'
DADR3            EQU  H'0003'
DADR4            EQU  H'0004'
DADR5            EQU  H'0005'
DADR6            EQU  H'0006'


;----- I3C2EC Bits -----------------------------------------------------
IBIEN            EQU  H'0000'
CREN             EQU  H'0001'
EC2              EQU  H'0002'
HJEN             EQU  H'0003'
EC_7_40          EQU  H'0004'
EC_7_41          EQU  H'0005'
EC_7_42          EQU  H'0006'
EC_7_43          EQU  H'0007'


;----- I3C2MWL Bits -----------------------------------------------------


;----- I3C2MWLH Bits -----------------------------------------------------


;----- I3C2MRL Bits -----------------------------------------------------


;----- I3C2MRLH Bits -----------------------------------------------------


;----- I3C2IBIPSZ Bits -----------------------------------------------------


;----- I3C2PID0 Bits -----------------------------------------------------
PID0             EQU  H'0000'
PID1             EQU  H'0001'
PID2             EQU  H'0002'
PID3             EQU  H'0003'
PID4             EQU  H'0004'
PID5             EQU  H'0005'
PID6             EQU  H'0006'
PID7             EQU  H'0007'


;----- I3C2PID1 Bits -----------------------------------------------------
PID8             EQU  H'0000'
PID9             EQU  H'0001'
PID10            EQU  H'0002'
PID11            EQU  H'0003'
PID12            EQU  H'0004'
PID13            EQU  H'0005'
PID14            EQU  H'0006'
PID15            EQU  H'0007'
PID_11_80        EQU  H'0000'
PID_11_81        EQU  H'0001'
PID_11_82        EQU  H'0002'
PID_11_83        EQU  H'0003'
PID_15_120       EQU  H'0004'
PID_15_121       EQU  H'0005'
PID_15_122       EQU  H'0006'
PID_15_123       EQU  H'0007'


;----- I3C2PID2 Bits -----------------------------------------------------
PID16            EQU  H'0000'
PID17            EQU  H'0001'
PID18            EQU  H'0002'
PID19            EQU  H'0003'
PID20            EQU  H'0004'
PID21            EQU  H'0005'
PID22            EQU  H'0006'
PID23            EQU  H'0007'


;----- I3C2PID3 Bits -----------------------------------------------------
PID24            EQU  H'0000'
PID25            EQU  H'0001'
PID26            EQU  H'0002'
PID27            EQU  H'0003'
PID28            EQU  H'0004'
PID29            EQU  H'0005'
PID30            EQU  H'0006'
PID31            EQU  H'0007'


;----- I3C2PID4 Bits -----------------------------------------------------
PID32            EQU  H'0000'
PID33            EQU  H'0001'
PID34            EQU  H'0002'
PID35            EQU  H'0003'
PID36            EQU  H'0004'
PID37            EQU  H'0005'
PID38            EQU  H'0006'
PID39            EQU  H'0007'
PID_39_330       EQU  H'0001'
PID_39_331       EQU  H'0002'
PID_39_332       EQU  H'0003'
PID_39_333       EQU  H'0004'
PID_39_334       EQU  H'0005'
PID_39_335       EQU  H'0006'
PID_39_336       EQU  H'0007'


;----- I3C2PID5 Bits -----------------------------------------------------
PID40            EQU  H'0000'
PID41            EQU  H'0001'
PID42            EQU  H'0002'
PID43            EQU  H'0003'
PID44            EQU  H'0004'
PID45            EQU  H'0005'
PID46            EQU  H'0006'
PID47            EQU  H'0007'


;----- I3C2BCR Bits -----------------------------------------------------
BCR0             EQU  H'0000'
BCR1             EQU  H'0001'
BCR2             EQU  H'0002'
BCR3             EQU  H'0003'
BCR4             EQU  H'0004'
BCR5             EQU  H'0005'
BCR6             EQU  H'0006'
BCR7             EQU  H'0007'
BCR_7_60         EQU  H'0006'
BCR_7_61         EQU  H'0007'


;----- I3C2DCR Bits -----------------------------------------------------


;----- I3C2DSTAT0 Bits -----------------------------------------------------
INTPEND0         EQU  H'0000'
INTPEND1         EQU  H'0001'
INTPEND2         EQU  H'0002'
INTPEND3         EQU  H'0003'
PERR             EQU  H'0005'
ACTMODE0         EQU  H'0006'
ACTMODE1         EQU  H'0007'
INPEND0          EQU  H'0000'
INTPEND1         EQU  H'0001'
INTPEND2         EQU  H'0002'
INTPEND3         EQU  H'0003'
ACTMODE0         EQU  H'0006'
ACTMODE1         EQU  H'0007'


;----- I3C2DSTAT1 Bits -----------------------------------------------------
VRSV0            EQU  H'0000'
VRSV1            EQU  H'0001'
VRSV2            EQU  H'0002'
VRSV3            EQU  H'0003'
VRSV4            EQU  H'0004'
VRSV5            EQU  H'0005'
VRSV6            EQU  H'0006'
VRSV7            EQU  H'0007'


;----- I3C2MWS Bits -----------------------------------------------------
MWS0             EQU  H'0000'
MWS1             EQU  H'0001'
MWS2             EQU  H'0002'
MWS3             EQU  H'0003'
MWS4             EQU  H'0004'
MWS5             EQU  H'0005'
MWS6             EQU  H'0006'
MWS7             EQU  H'0007'
MWS_2_00         EQU  H'0000'
MWS_2_01         EQU  H'0001'
MWS_2_02         EQU  H'0002'
MWS_7_40         EQU  H'0004'
MWS_7_41         EQU  H'0005'
MWS_7_42         EQU  H'0006'
MWS_7_43         EQU  H'0007'


;----- I3C2MRS Bits -----------------------------------------------------
MRS0             EQU  H'0000'
MRS1             EQU  H'0001'
MRS2             EQU  H'0002'
MRS3             EQU  H'0003'
MRS4             EQU  H'0004'
MRS5             EQU  H'0005'
MRS6             EQU  H'0006'
MRS7             EQU  H'0007'
MRS_2_00         EQU  H'0000'
MRS_2_01         EQU  H'0001'
MRS_2_02         EQU  H'0002'
MRS_5_30         EQU  H'0003'
MRS_5_31         EQU  H'0004'
MRS_5_32         EQU  H'0005'


;----- I3C2MRT Bits -----------------------------------------------------


;----- I3C2MRTH Bits -----------------------------------------------------


;----- I3C2MRTU Bits -----------------------------------------------------


;----- I3C2RSTACT Bits -----------------------------------------------------


;----- I3C2BUSCXT Bits -----------------------------------------------------


;----- I3C2CCC Bits -----------------------------------------------------


;----- I3C2I2CCON Bits -----------------------------------------------------
SDAHT0           EQU  H'0000'
SDAHT1           EQU  H'0001'
FLTEN            EQU  H'0002'


;----- I3C2CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


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
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


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
T1GSS0           EQU  H'0000'
T1GSS1           EQU  H'0001'
T1GSS2           EQU  H'0002'
T1GSS3           EQU  H'0003'


;----- T1CLK Bits -----------------------------------------------------
T1CS0            EQU  H'0000'
T1CS1            EQU  H'0001'
T1CS2            EQU  H'0002'
T1CS3            EQU  H'0003'
CS0_T1CLK        EQU  H'0000'
CS1_T1CLK        EQU  H'0001'
CS2_T1CLK        EQU  H'0002'
CS3              EQU  H'0003'


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
T4CS0            EQU  H'0000'
T4CS1            EQU  H'0001'
T4CS2            EQU  H'0002'


;----- T4RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'
RSEL3            EQU  H'0003'
T4RSEL0          EQU  H'0000'
T4RSEL1          EQU  H'0001'
T4RSEL2          EQU  H'0002'
T4RSEL3          EQU  H'0003'


;----- TUCHAIN Bits -----------------------------------------------------
CH16AB           EQU  H'0000'


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
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
CSYNC            EQU  H'0006'
EPOL             EQU  H'0007'
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
TU16ASTOP0       EQU  H'0000'
TU16ASTOP1       EQU  H'0001'
TU16ARESET0      EQU  H'0002'
TU16ARESET1      EQU  H'0003'
TU16ASTART0      EQU  H'0004'
TU16ASTART1      EQU  H'0005'
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


;----- TU16ATMRT Bits -----------------------------------------------------
TMRT0            EQU  H'0000'
TMRT1            EQU  H'0001'
TMRT2            EQU  H'0002'
TMRT3            EQU  H'0003'
TMRT4            EQU  H'0004'
TMRT5            EQU  H'0005'
TMRT6            EQU  H'0006'
TMRT7            EQU  H'0007'
TU16ATMRT0       EQU  H'0000'
TU16ATMRT1       EQU  H'0001'
TU16ATMRT2       EQU  H'0002'
TU16ATMRT3       EQU  H'0003'
TU16ATMRT4       EQU  H'0004'
TU16ATMRT5       EQU  H'0005'
TU16ATMRT6       EQU  H'0006'
TU16ATMRT7       EQU  H'0007'
TU16ATMR24       EQU  H'0000'
TU16ATMR25       EQU  H'0001'
TU16ATMR26       EQU  H'0002'
TU16ATMR27       EQU  H'0003'
TU16ATMR28       EQU  H'0004'
TU16ATMR29       EQU  H'0005'
TU16ATMR30       EQU  H'0006'
TU16ATMR31       EQU  H'0007'
CRT0             EQU  H'0000'
CRT1             EQU  H'0001'
CRT2             EQU  H'0002'
CRT3             EQU  H'0003'
CRT4             EQU  H'0004'
CRT5             EQU  H'0005'
CRT6             EQU  H'0006'
CRT7             EQU  H'0007'
TU16ACRT0        EQU  H'0000'
TU16ACRT1        EQU  H'0001'
TU16ACRT2        EQU  H'0002'
TU16ACRT3        EQU  H'0003'
TU16ACRT4        EQU  H'0004'
TU16ACRT5        EQU  H'0005'
TU16ACRT6        EQU  H'0006'
TU16ACRT7        EQU  H'0007'
TU16ACR24        EQU  H'0000'
TU16ACR25        EQU  H'0001'
TU16ACR26        EQU  H'0002'
TU16ACR27        EQU  H'0003'
TU16ACR28        EQU  H'0004'
TU16ACR29        EQU  H'0005'
TU16ACR30        EQU  H'0006'
TU16ACR31        EQU  H'0007'


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


;----- TU16APRT Bits -----------------------------------------------------
PRT0             EQU  H'0000'
PRT1             EQU  H'0001'
PRT2             EQU  H'0002'
PRT3             EQU  H'0003'
PRT4             EQU  H'0004'
PRT5             EQU  H'0005'
PRT6             EQU  H'0006'
PRT7             EQU  H'0007'
TU16APRT0        EQU  H'0000'
TU16APRT1        EQU  H'0001'
TU16APRT2        EQU  H'0002'
TU16APRT3        EQU  H'0003'
TU16APRT4        EQU  H'0004'
TU16APRT5        EQU  H'0005'
TU16APRT6        EQU  H'0006'
TU16APRT7        EQU  H'0007'
TU16APR24        EQU  H'0000'
TU16APR25        EQU  H'0001'
TU16APR26        EQU  H'0002'
TU16APR27        EQU  H'0003'
TU16APR28        EQU  H'0004'
TU16APR29        EQU  H'0005'
TU16APR30        EQU  H'0006'
TU16APR31        EQU  H'0007'


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
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
CSYNC            EQU  H'0006'
EPOL             EQU  H'0007'
STOP0            EQU  H'0000'
STOP1            EQU  H'0001'
RESET0           EQU  H'0002'
RESET1           EQU  H'0003'
START0           EQU  H'0004'
START1           EQU  H'0005'
TU16BSTOP0       EQU  H'0000'
TU16BSTOP1       EQU  H'0001'
TU16BRESET0      EQU  H'0002'
TU16BRESET1      EQU  H'0003'
TU16BSTART0      EQU  H'0004'
TU16BSTART1      EQU  H'0005'
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


;----- TU16BTMRT Bits -----------------------------------------------------
TMRT0            EQU  H'0000'
TMRT1            EQU  H'0001'
TMRT2            EQU  H'0002'
TMRT3            EQU  H'0003'
TMRT4            EQU  H'0004'
TMRT5            EQU  H'0005'
TMRT6            EQU  H'0006'
TMRT7            EQU  H'0007'
TU16BTMRT0       EQU  H'0000'
TU16BTMRT1       EQU  H'0001'
TU16BTMRT2       EQU  H'0002'
TU16BTMRT3       EQU  H'0003'
TU16BTMRT4       EQU  H'0004'
TU16BTMRT5       EQU  H'0005'
TU16BTMRT6       EQU  H'0006'
TU16BTMRT7       EQU  H'0007'
TU16BTMR24       EQU  H'0000'
TU16BTMR25       EQU  H'0001'
TU16BTMR26       EQU  H'0002'
TU16BTMR27       EQU  H'0003'
TU16BTMR28       EQU  H'0004'
TU16BTMR29       EQU  H'0005'
TU16BTMR30       EQU  H'0006'
TU16BTMR31       EQU  H'0007'
CRT0             EQU  H'0000'
CRT1             EQU  H'0001'
CRT2             EQU  H'0002'
CRT3             EQU  H'0003'
CRT4             EQU  H'0004'
CRT5             EQU  H'0005'
CRT6             EQU  H'0006'
CRT7             EQU  H'0007'
TU16BCRT0        EQU  H'0000'
TU16BCRT1        EQU  H'0001'
TU16BCRT2        EQU  H'0002'
TU16BCRT3        EQU  H'0003'
TU16BCRT4        EQU  H'0004'
TU16BCRT5        EQU  H'0005'
TU16BCRT6        EQU  H'0006'
TU16BCRT7        EQU  H'0007'
TU16BCR24        EQU  H'0000'
TU16BCR25        EQU  H'0001'
TU16BCR26        EQU  H'0002'
TU16BCR27        EQU  H'0003'
TU16BCR28        EQU  H'0004'
TU16BCR29        EQU  H'0005'
TU16BCR30        EQU  H'0006'
TU16BCR31        EQU  H'0007'


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


;----- TU16BPRT Bits -----------------------------------------------------
PRT0             EQU  H'0000'
PRT1             EQU  H'0001'
PRT2             EQU  H'0002'
PRT3             EQU  H'0003'
PRT4             EQU  H'0004'
PRT5             EQU  H'0005'
PRT6             EQU  H'0006'
PRT7             EQU  H'0007'
TU16BPRT0        EQU  H'0000'
TU16BPRT1        EQU  H'0001'
TU16BPRT2        EQU  H'0002'
TU16BPRT3        EQU  H'0003'
TU16BPRT4        EQU  H'0004'
TU16BPRT5        EQU  H'0005'
TU16BPRT6        EQU  H'0006'
TU16BPRT7        EQU  H'0007'
TU16BPR24        EQU  H'0000'
TU16BPR25        EQU  H'0001'
TU16BPR26        EQU  H'0002'
TU16BPR27        EQU  H'0003'
TU16BPR28        EQU  H'0004'
TU16BPR29        EQU  H'0005'
TU16BPR30        EQU  H'0006'
TU16BPR31        EQU  H'0007'


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


;----- CCPTMRS0 Bits -----------------------------------------------------
C1TSEL0          EQU  H'0000'
C1TSEL1          EQU  H'0001'
C2TSEL0          EQU  H'0002'
C2TSEL1          EQU  H'0003'
C1TSEL0          EQU  H'0000'
C1TSEL1          EQU  H'0001'
C2TSEL2          EQU  H'0002'
C2TSEL3          EQU  H'0003'


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
CCP2CTS0         EQU  H'0000'
CCP2CTS1         EQU  H'0001'
CCP2CTS2         EQU  H'0002'


;----- PWMLOAD Bits -----------------------------------------------------
MPWM1LD          EQU  H'0000'
MPWM2LD          EQU  H'0001'


;----- PWMEN Bits -----------------------------------------------------
MPWM1EN          EQU  H'0000'
MPWM2EN          EQU  H'0001'


;----- PWM2ERS Bits -----------------------------------------------------
ERS0             EQU  H'0000'
ERS1             EQU  H'0001'
ERS2             EQU  H'0002'
ERS3             EQU  H'0003'


;----- PWM2CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


;----- PWM2LDS Bits -----------------------------------------------------
LDS0             EQU  H'0000'
LDS1             EQU  H'0001'
LDS2             EQU  H'0002'
LDS3             EQU  H'0003'


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


;----- CLCDATA Bits -----------------------------------------------------
CLC1OUT          EQU  H'0000'
CLC2OUT          EQU  H'0001'
CLC3OUT          EQU  H'0002'
CLC4OUT          EQU  H'0003'


;----- CLCSELECT Bits -----------------------------------------------------
SLCT0            EQU  H'0000'
SLCT1            EQU  H'0001'


;----- CLCNCON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
INTN_CLCNCON     EQU  H'0003'
INTP_CLCNCON     EQU  H'0004'
OUT_CLCNCON      EQU  H'0005'
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


;----- CLCNSEL2 Bits -----------------------------------------------------
D3S0             EQU  H'0000'
D3S1             EQU  H'0001'
D3S2             EQU  H'0002'
D3S3             EQU  H'0003'
D3S4             EQU  H'0004'
D3S5             EQU  H'0005'


;----- CLCNSEL3 Bits -----------------------------------------------------
D4S0             EQU  H'0000'
D4S1             EQU  H'0001'
D4S2             EQU  H'0002'
D4S3             EQU  H'0003'
D4S4             EQU  H'0004'
D4S5             EQU  H'0005'


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


;----- U1RXCHK Bits -----------------------------------------------------


;----- U1TXCHK Bits -----------------------------------------------------


;----- U1P2H Bits -----------------------------------------------------
P2H              EQU  H'0000'


;----- U1P3H Bits -----------------------------------------------------
P3H              EQU  H'0000'


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
RXFOIF           EQU  H'0001'
RXBKIF           EQU  H'0002'
FERIF            EQU  H'0003'
ABDOVF           EQU  H'0005'
PERIF            EQU  H'0006'
TXMTIF           EQU  H'0007'
U2RXFOIF         EQU  H'0001'
U2RXBKIF         EQU  H'0002'
U2FERIF          EQU  H'0003'
U2ABDOVF         EQU  H'0005'
U2PERIF          EQU  H'0006'
U2TXMTIF         EQU  H'0007'


;----- U2ERRIE Bits -----------------------------------------------------
RXFOIE           EQU  H'0001'
RXBKIE           EQU  H'0002'
FERIE            EQU  H'0003'
ABDOVE           EQU  H'0005'
PERIE            EQU  H'0006'
TXMTIE           EQU  H'0007'
U2RXFOIE         EQU  H'0001'
U2RXBKIE         EQU  H'0002'
U2FERIE          EQU  H'0003'
U2ABDOVE         EQU  H'0005'
U2PERIE          EQU  H'0006'
U2TXMTIE         EQU  H'0007'


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
SPI1CLKSEL0      EQU  H'0000'
SPI1CLKSEL1      EQU  H'0001'
SPI1CLKSEL2      EQU  H'0002'
SPI1CLKSEL3      EQU  H'0003'


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
P                EQU  H'0003'
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
GCEN             EQU  H'0006'
ACNT             EQU  H'0007'
BFRET0           EQU  H'0000'
BFRET1           EQU  H'0001'
SDAHT0           EQU  H'0002'
SDAHT1           EQU  H'0003'


;----- I2C1CON3 Bits -----------------------------------------------------
ACNTMD0          EQU  H'0000'
ACNTMD1          EQU  H'0001'
FME0             EQU  H'0002'
FME1             EQU  H'0003'
BFREDR           EQU  H'0004'
ACNTMD0          EQU  H'0000'
ACNTMD1          EQU  H'0001'
FME0             EQU  H'0002'
FME1             EQU  H'0003'


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


;----- I2C1CNT Bits -----------------------------------------------------
CNT0             EQU  H'0000'
CNT1             EQU  H'0001'
CNT2             EQU  H'0002'
CNT3             EQU  H'0003'
CNT4             EQU  H'0004'
CNT5             EQU  H'0005'
CNT6             EQU  H'0006'
CNT7             EQU  H'0007'
I2C1CNT0         EQU  H'0000'
I2C1CNT1         EQU  H'0001'
I2C1CNT2         EQU  H'0002'
I2C1CNT3         EQU  H'0003'
I2C1CNT4         EQU  H'0004'
I2C1CNT5         EQU  H'0005'
I2C1CNT6         EQU  H'0006'
I2C1CNT7         EQU  H'0007'


;----- I2C1CNTH Bits -----------------------------------------------------
CNT8             EQU  H'0000'
CNT9             EQU  H'0001'
CNT10            EQU  H'0002'
CNT11            EQU  H'0003'
CNT12            EQU  H'0004'
CNT13            EQU  H'0005'
CNT14            EQU  H'0006'
CNT15            EQU  H'0007'
I2C1CNT8         EQU  H'0000'
I2C1CNT9         EQU  H'0001'
I2C1CNT10        EQU  H'0002'
I2C1CNT11        EQU  H'0003'
I2C1CNT12        EQU  H'0004'
I2C1CNT13        EQU  H'0005'
I2C1CNT14        EQU  H'0006'
I2C1CNT15        EQU  H'0007'


;----- I2C1RXB Bits -----------------------------------------------------


;----- I2C1TXB Bits -----------------------------------------------------


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


;----- I2C1BTO Bits -----------------------------------------------------
TOTIME0          EQU  H'0000'
TOTIME1          EQU  H'0001'
TOTIME2          EQU  H'0002'
TOTIME3          EQU  H'0003'
TOTIME4          EQU  H'0004'
TOTIME5          EQU  H'0005'
TOBY32           EQU  H'0006'
TOREC            EQU  H'0007'
TOTIME0          EQU  H'0000'
TOTIME1          EQU  H'0001'
TOTIME2          EQU  H'0002'
TOTIME3          EQU  H'0003'
TOTIME4          EQU  H'0004'
TOTIME5          EQU  H'0005'


;----- I2C1BAUD Bits -----------------------------------------------------


;----- I2C1CLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
I2CCLK0          EQU  H'0000'
I2CCLK1          EQU  H'0001'
I2CCLK2          EQU  H'0002'
I2CCLK3          EQU  H'0003'


;----- I2C1BTOC Bits -----------------------------------------------------
BTOC0            EQU  H'0000'
BTOC1            EQU  H'0001'
BTOC2            EQU  H'0002'
I2CBTOC0         EQU  H'0000'
I2CBTOC1         EQU  H'0001'
I2CBTOC2         EQU  H'0002'


;----- FVRCON Bits -----------------------------------------------------
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
TSRNG            EQU  H'0004'
TSEN             EQU  H'0005'
RDY_FVRCON       EQU  H'0006'
EN               EQU  H'0007'
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
FVRRDY           EQU  H'0006'
FVREN            EQU  H'0007'


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


;----- ADNCH Bits -----------------------------------------------------
ADNCH0           EQU  H'0000'
ADNCH1           EQU  H'0001'
ADNCH2           EQU  H'0002'
ADNCH3           EQU  H'0003'
ADNCH4           EQU  H'0004'
ADNCH5           EQU  H'0005'


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


;----- ADCG1A Bits -----------------------------------------------------
CGA0             EQU  H'0000'
CGA1             EQU  H'0001'
CGA2             EQU  H'0002'
CGA4             EQU  H'0004'
CGA5             EQU  H'0005'


;----- ADCG1B Bits -----------------------------------------------------
CGB5             EQU  H'0005'
CGB6             EQU  H'0006'
CGB7             EQU  H'0007'


;----- ADCG1C Bits -----------------------------------------------------
CGC0             EQU  H'0000'
CGC1             EQU  H'0001'
CGC3             EQU  H'0003'
CGC4             EQU  H'0004'
CGC5             EQU  H'0005'
CGC6             EQU  H'0006'
CGC7             EQU  H'0007'


;----- PMD0 Bits -----------------------------------------------------
DMA1MD           EQU  H'0000'
DMA2MD           EQU  H'0001'
DMA3MD           EQU  H'0002'
DMA4MD           EQU  H'0003'
CRCMD            EQU  H'0005'
SCANMD           EQU  H'0006'
SYSCMD           EQU  H'0007'


;----- PMD2 Bits -----------------------------------------------------
TU16AMD          EQU  H'0000'
TU16BMD          EQU  H'0001'
CCP1MD           EQU  H'0002'
CCP2MD           EQU  H'0003'
PWM1MD           EQU  H'0004'
PWM2MD           EQU  H'0005'
CWG1MD           EQU  H'0006'
CLC1MD           EQU  H'0007'


;----- PMD3 Bits -----------------------------------------------------
CLC2MD           EQU  H'0000'
CLC3MD           EQU  H'0001'
CLC4MD           EQU  H'0002'
U1MD             EQU  H'0003'
U2MD             EQU  H'0004'
SPI1MD_PMD3      EQU  H'0005'
I2C1MD           EQU  H'0006'
I3C1MD           EQU  H'0007'


;----- PMD4 Bits -----------------------------------------------------
I3C2MD           EQU  H'0000'
FVRMD            EQU  H'0001'
HLVDMD           EQU  H'0002'
ADCMD            EQU  H'0003'


;----- RB5FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SYSBUF2          EQU  H'0002'
I3CBUF0          EQU  H'0003'
I3CBUF1          EQU  H'0004'
I3CBUF2          EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RB6FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SYSBUF2          EQU  H'0002'
I3CBUF0          EQU  H'0003'
I3CBUF1          EQU  H'0004'
I3CBUF2          EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC0FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SYSBUF2          EQU  H'0002'
I3CBUF0          EQU  H'0003'
I3CBUF1          EQU  H'0004'
I3CBUF2          EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC4FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC5FEAT Bits -----------------------------------------------------
SYSBUF0          EQU  H'0000'
SYSBUF1          EQU  H'0001'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RA0PPS Bits -----------------------------------------------------
RA0PPS0          EQU  H'0000'
RA0PPS1          EQU  H'0001'
RA0PPS2          EQU  H'0002'
RA0PPS3          EQU  H'0003'
RA0PPS4          EQU  H'0004'


;----- RA2PPS Bits -----------------------------------------------------
RA2PPS0          EQU  H'0000'
RA2PPS1          EQU  H'0001'
RA2PPS2          EQU  H'0002'
RA2PPS3          EQU  H'0003'
RA2PPS4          EQU  H'0004'


;----- RA4PPS Bits -----------------------------------------------------
RA4PPS0          EQU  H'0000'
RA4PPS1          EQU  H'0001'
RA4PPS2          EQU  H'0002'
RA4PPS3          EQU  H'0003'
RA4PPS4          EQU  H'0004'


;----- RA5PPS Bits -----------------------------------------------------
RA5PPS0          EQU  H'0000'
RA5PPS1          EQU  H'0001'
RA5PPS2          EQU  H'0002'
RA5PPS3          EQU  H'0003'
RA5PPS4          EQU  H'0004'


;----- RB5PPS Bits -----------------------------------------------------
RB5PPS0          EQU  H'0000'
RB5PPS1          EQU  H'0001'
RB5PPS2          EQU  H'0002'
RB5PPS3          EQU  H'0003'
RB5PPS4          EQU  H'0004'


;----- RB6PPS Bits -----------------------------------------------------
RB6PPS0          EQU  H'0000'
RB6PPS1          EQU  H'0001'
RB6PPS2          EQU  H'0002'
RB6PPS3          EQU  H'0003'
RB6PPS4          EQU  H'0004'


;----- RB7PPS Bits -----------------------------------------------------
RB7PPS0          EQU  H'0000'
RB7PPS1          EQU  H'0001'
RB7PPS2          EQU  H'0002'
RB7PPS3          EQU  H'0003'
RB7PPS4          EQU  H'0004'


;----- RC0PPS Bits -----------------------------------------------------
RC0PPS0          EQU  H'0000'
RC0PPS1          EQU  H'0001'
RC0PPS2          EQU  H'0002'
RC0PPS3          EQU  H'0003'
RC0PPS4          EQU  H'0004'


;----- RC3PPS Bits -----------------------------------------------------
RC3PPS0          EQU  H'0000'
RC3PPS1          EQU  H'0001'
RC3PPS2          EQU  H'0002'
RC3PPS3          EQU  H'0003'
RC3PPS4          EQU  H'0004'


;----- RC4PPS Bits -----------------------------------------------------
RC4PPS0          EQU  H'0000'
RC4PPS1          EQU  H'0001'
RC4PPS2          EQU  H'0002'
RC4PPS3          EQU  H'0003'
RC4PPS4          EQU  H'0004'


;----- RC5PPS Bits -----------------------------------------------------
RC5PPS0          EQU  H'0000'
RC5PPS1          EQU  H'0001'
RC5PPS2          EQU  H'0002'
RC5PPS3          EQU  H'0003'
RC5PPS4          EQU  H'0004'


;----- RC6PPS Bits -----------------------------------------------------
RC6PPS0          EQU  H'0000'
RC6PPS1          EQU  H'0001'
RC6PPS2          EQU  H'0002'
RC6PPS3          EQU  H'0003'
RC6PPS4          EQU  H'0004'


;----- RC7PPS Bits -----------------------------------------------------
RC7PPS0          EQU  H'0000'
RC7PPS1          EQU  H'0001'
RC7PPS2          EQU  H'0002'
RC7PPS3          EQU  H'0003'
RC7PPS4          EQU  H'0004'


;----- PPSLOCK Bits -----------------------------------------------------
PPSLOCKED        EQU  H'0000'


;----- INT0PPS Bits -----------------------------------------------------
INT0PPS0         EQU  H'0000'
INT0PPS1         EQU  H'0001'
INT0PPS2         EQU  H'0002'
INT0PPS3         EQU  H'0003'
INT0PPS4         EQU  H'0004'
INT0PPS0         EQU  H'0000'
INT0PPS1         EQU  H'0001'
INT0PPS2         EQU  H'0002'
INT0PPS3         EQU  H'0003'
INT0PPS4         EQU  H'0004'


;----- INT2PPS Bits -----------------------------------------------------
INT2PPS0         EQU  H'0000'
INT2PPS1         EQU  H'0001'
INT2PPS2         EQU  H'0002'
INT2PPS3         EQU  H'0003'
INT2PPS4         EQU  H'0004'
INT2PPS0         EQU  H'0000'
INT2PPS1         EQU  H'0001'
INT2PPS2         EQU  H'0002'
INT2PPS3         EQU  H'0003'
INT2PPS4         EQU  H'0004'


;----- T0CKIPPS Bits -----------------------------------------------------
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'


;----- T1CKIPPS Bits -----------------------------------------------------
T1CKIPPS0        EQU  H'0000'
T1CKIPPS1        EQU  H'0001'
T1CKIPPS2        EQU  H'0002'
T1CKIPPS3        EQU  H'0003'
T1CKIPPS4        EQU  H'0004'
T1CKIPPS0        EQU  H'0000'
T1CKIPPS1        EQU  H'0001'
T1CKIPPS2        EQU  H'0002'
T1CKIPPS3        EQU  H'0003'
T1CKIPPS4        EQU  H'0004'


;----- T1GPPS Bits -----------------------------------------------------
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


;----- T2INPPS Bits -----------------------------------------------------
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


;----- TUIN0PPS Bits -----------------------------------------------------
TUIN0PPS0        EQU  H'0000'
TUIN0PPS1        EQU  H'0001'
TUIN0PPS2        EQU  H'0002'
TUIN0PPS3        EQU  H'0003'
TUIN0PPS4        EQU  H'0004'
TUIN0PPS0        EQU  H'0000'
TUIN0PPS1        EQU  H'0001'
TUIN0PPS2        EQU  H'0002'
TUIN0PPS3        EQU  H'0003'
TUIN0PPS4        EQU  H'0004'


;----- TUIN1PPS Bits -----------------------------------------------------
TUIN1PPS0        EQU  H'0000'
TUIN1PPS1        EQU  H'0001'
TUIN1PPS2        EQU  H'0002'
TUIN1PPS3        EQU  H'0003'
TUIN1PPS4        EQU  H'0004'
TUIN1PPS0        EQU  H'0000'
TUIN1PPS1        EQU  H'0001'
TUIN1PPS2        EQU  H'0002'
TUIN1PPS3        EQU  H'0003'
TUIN1PPS4        EQU  H'0004'


;----- CCP2PPS Bits -----------------------------------------------------
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'


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


;----- PCLAT_SHAD Bits -----------------------------------------------------
PCLAT_SHAD0      EQU  H'0000'
PCLAT_SHAD1      EQU  H'0001'
PCLAT_SHAD2      EQU  H'0002'
PCLAT_SHAD3      EQU  H'0003'
PCLAT_SHAD4      EQU  H'0004'
PCLAT_SHAD5      EQU  H'0005'
PCLAT_SHAD6      EQU  H'0006'
PCLAT_SHAD7      EQU  H'0007'
PCLAT_SHAD8      EQU  H'0008'
PCLAT_SHAD9      EQU  H'0009'
PCLAT_SHAD10     EQU  H'0010'
PCLAT_SHAD11     EQU  H'0011'
PCLAT_SHAD12     EQU  H'0012'


;----- PCLATU_SHAD Bits -----------------------------------------------------
PCU0             EQU  H'0000'
PCU1             EQU  H'0001'
PCU2             EQU  H'0002'
PCU3             EQU  H'0003'
PCU4             EQU  H'0004'


;----- FSR0_SHAD Bits -----------------------------------------------------
FSR0_SHAD0       EQU  H'0000'
FSR0_SHAD1       EQU  H'0001'
FSR0_SHAD2       EQU  H'0002'
FSR0_SHAD3       EQU  H'0003'
FSR0_SHAD4       EQU  H'0004'
FSR0_SHAD5       EQU  H'0005'
FSR0_SHAD6       EQU  H'0006'
FSR0_SHAD7       EQU  H'0007'
FSR0_SHAD8       EQU  H'0008'
FSR0_SHAD9       EQU  H'0009'
FSR0_SHAD10      EQU  H'0010'
FSR0_SHAD11      EQU  H'0011'
FSR0_SHAD12      EQU  H'0012'
FSR0_SHAD13      EQU  H'0013'


;----- FSR0H_SHAD Bits -----------------------------------------------------
FSR0H0           EQU  H'0000'
FSR0H1           EQU  H'0001'
FSR0H2           EQU  H'0002'
FSR0H3           EQU  H'0003'
FSR0H4           EQU  H'0004'
FSR0H5           EQU  H'0005'


;----- FSR2_SHAD Bits -----------------------------------------------------
FSR2_SHAD0       EQU  H'0000'
FSR2_SHAD1       EQU  H'0001'
FSR2_SHAD2       EQU  H'0002'
FSR2_SHAD3       EQU  H'0003'
FSR2_SHAD4       EQU  H'0004'
FSR2_SHAD5       EQU  H'0005'
FSR2_SHAD6       EQU  H'0006'
FSR2_SHAD7       EQU  H'0007'
FSR2_SHAD8       EQU  H'0008'
FSR2_SHAD9       EQU  H'0009'
FSR2_SHAD10      EQU  H'0010'
FSR2_SHAD11      EQU  H'0011'
FSR2_SHAD12      EQU  H'0012'
FSR2_SHAD13      EQU  H'0013'


;----- FSR2H_SHAD Bits -----------------------------------------------------
FSR2H0           EQU  H'0000'
FSR2H1           EQU  H'0001'
FSR2H2           EQU  H'0002'
FSR2H3           EQU  H'0003'
FSR2H4           EQU  H'0004'
FSR2H5           EQU  H'0005'


;----- PROD_SHAD Bits -----------------------------------------------------
PROD_SHAD0       EQU  H'0000'
PROD_SHAD1       EQU  H'0001'
PROD_SHAD2       EQU  H'0002'
PROD_SHAD3       EQU  H'0003'
PROD_SHAD4       EQU  H'0004'
PROD_SHAD5       EQU  H'0005'
PROD_SHAD6       EQU  H'0006'
PROD_SHAD7       EQU  H'0007'
PROD_SHAD8       EQU  H'0008'
PROD_SHAD9       EQU  H'0009'
PROD_SHAD10      EQU  H'0010'
PROD_SHAD11      EQU  H'0011'
PROD_SHAD12      EQU  H'0012'
PROD_SHAD13      EQU  H'0013'
PROD_SHAD14      EQU  H'0014'
PROD_SHAD15      EQU  H'0015'


;----- PRODH_SHAD Bits -----------------------------------------------------


;----- PWMIN0PPS Bits -----------------------------------------------------
PWMIN0PPS0       EQU  H'0000'
PWMIN0PPS1       EQU  H'0001'
PWMIN0PPS2       EQU  H'0002'
PWMIN0PPS3       EQU  H'0003'
PWMIN0PPS4       EQU  H'0004'
PWMIN0PPS0       EQU  H'0000'
PWMIN0PPS1       EQU  H'0001'
PWMIN0PPS2       EQU  H'0002'
PWMIN0PPS3       EQU  H'0003'
PWMIN0PPS4       EQU  H'0004'


;----- PWMIN1PPS Bits -----------------------------------------------------
PWMIN1PPS0       EQU  H'0000'
PWMIN1PPS1       EQU  H'0001'
PWMIN1PPS2       EQU  H'0002'
PWMIN1PPS3       EQU  H'0003'
PWMIN1PPS4       EQU  H'0004'
PWMIN1PPS0       EQU  H'0000'
PWMIN1PPS1       EQU  H'0001'
PWMIN1PPS2       EQU  H'0002'
PWMIN1PPS3       EQU  H'0003'
PWMIN1PPS4       EQU  H'0004'


;----- PWM2ERSPPS Bits -----------------------------------------------------
PWM2ERSPPS0      EQU  H'0000'
PWM2ERSPPS1      EQU  H'0001'
PWM2ERSPPS2      EQU  H'0002'
PWM2ERSPPS3      EQU  H'0003'
PWM2ERSPPS4      EQU  H'0004'
PWM2ERSPPS0      EQU  H'0000'
PWM2ERSPPS1      EQU  H'0001'
PWM2ERSPPS2      EQU  H'0002'
PWM2ERSPPS3      EQU  H'0003'
PWM2ERSPPS4      EQU  H'0004'


;----- CWG1PPS Bits -----------------------------------------------------
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


;----- CLCIN0PPS Bits -----------------------------------------------------
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


;----- CLCIN2PPS Bits -----------------------------------------------------
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


;----- U2CTSPPS Bits -----------------------------------------------------
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


;----- U2RXPPS Bits -----------------------------------------------------
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


;----- SPI1SCKPPS Bits -----------------------------------------------------
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


;----- I2C1SCLPPS Bits -----------------------------------------------------
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


;----- I2C1SDAPPS Bits -----------------------------------------------------
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


;----- ADACTPPS Bits -----------------------------------------------------
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


;----- ANSELA Bits -----------------------------------------------------
ANSELA0          EQU  H'0000'
ANSELA1          EQU  H'0001'
ANSELA2          EQU  H'0002'
ANSELA4          EQU  H'0004'
ANSELA5          EQU  H'0005'


;----- WPUA Bits -----------------------------------------------------
WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WPUA2            EQU  H'0002'
WPUA3            EQU  H'0003'
WPUA4            EQU  H'0004'
WPUA5            EQU  H'0005'


;----- ODCONA Bits -----------------------------------------------------
ODCA0            EQU  H'0000'
ODCA1            EQU  H'0001'
ODCA2            EQU  H'0002'
ODCA4            EQU  H'0004'
ODCA5            EQU  H'0005'


;----- SLRCONA Bits -----------------------------------------------------
SLRA0            EQU  H'0000'
SLRA1            EQU  H'0001'
SLRA2            EQU  H'0002'
SLRA4            EQU  H'0004'
SLRA5            EQU  H'0005'


;----- INLVLA Bits -----------------------------------------------------
INLVLA0          EQU  H'0000'
INLVLA1          EQU  H'0001'
INLVLA2          EQU  H'0002'
INLVLA3          EQU  H'0003'
INLVLA4          EQU  H'0004'
INLVLA5          EQU  H'0005'


;----- IOCAP Bits -----------------------------------------------------
IOCAP0           EQU  H'0000'
IOCAP1           EQU  H'0001'
IOCAP2           EQU  H'0002'
IOCAP3           EQU  H'0003'
IOCAP4           EQU  H'0004'
IOCAP5           EQU  H'0005'


;----- IOCAN Bits -----------------------------------------------------
IOCAN0           EQU  H'0000'
IOCAN1           EQU  H'0001'
IOCAN2           EQU  H'0002'
IOCAN3           EQU  H'0003'
IOCAN4           EQU  H'0004'
IOCAN5           EQU  H'0005'


;----- IOCAF Bits -----------------------------------------------------
IOCAF0           EQU  H'0000'
IOCAF1           EQU  H'0001'
IOCAF2           EQU  H'0002'
IOCAF3           EQU  H'0003'
IOCAF4           EQU  H'0004'
IOCAF5           EQU  H'0005'


;----- ANSELB Bits -----------------------------------------------------
ANSELB7          EQU  H'0007'


;----- WPUB Bits -----------------------------------------------------
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'
WPUB7            EQU  H'0007'


;----- ODCONB Bits -----------------------------------------------------
ODCB5            EQU  H'0005'
ODCB6            EQU  H'0006'
ODCB7            EQU  H'0007'


;----- SLRCONB Bits -----------------------------------------------------
SLRB5            EQU  H'0005'
SLRB6            EQU  H'0006'
SLRB7            EQU  H'0007'


;----- INLVLB Bits -----------------------------------------------------
INLVLB5          EQU  H'0005'
INLVLB6          EQU  H'0006'
INLVLB7          EQU  H'0007'


;----- IOCBP Bits -----------------------------------------------------
IOCBP5           EQU  H'0005'
IOCBP6           EQU  H'0006'
IOCBP7           EQU  H'0007'


;----- IOCBN Bits -----------------------------------------------------
IOCBN5           EQU  H'0005'
IOCBN6           EQU  H'0006'
IOCBN7           EQU  H'0007'


;----- IOCBF Bits -----------------------------------------------------
IOCBF5           EQU  H'0005'
IOCBF6           EQU  H'0006'
IOCBF7           EQU  H'0007'


;----- ANSELC Bits -----------------------------------------------------
ANSELC3          EQU  H'0003'
ANSELC4          EQU  H'0004'
ANSELC5          EQU  H'0005'
ANSELC6          EQU  H'0006'
ANSELC7          EQU  H'0007'


;----- WPUC Bits -----------------------------------------------------
WPUC0            EQU  H'0000'
WPUC1            EQU  H'0001'
WPUC3            EQU  H'0003'
WPUC4            EQU  H'0004'
WPUC5            EQU  H'0005'
WPUC6            EQU  H'0006'
WPUC7            EQU  H'0007'


;----- ODCONC Bits -----------------------------------------------------
ODCC0            EQU  H'0000'
ODCC1            EQU  H'0001'
ODCC3            EQU  H'0003'
ODCC4            EQU  H'0004'
ODCC5            EQU  H'0005'
ODCC6            EQU  H'0006'
ODCC7            EQU  H'0007'


;----- SLRCONC Bits -----------------------------------------------------
SLRC0            EQU  H'0000'
SLRC1            EQU  H'0001'
SLRC3            EQU  H'0003'
SLRC4            EQU  H'0004'
SLRC5            EQU  H'0005'
SLRC6            EQU  H'0006'
SLRC7            EQU  H'0007'


;----- INLVLC Bits -----------------------------------------------------
INLVLC0          EQU  H'0000'
INLVLC1          EQU  H'0001'
INLVLC3          EQU  H'0003'
INLVLC4          EQU  H'0004'
INLVLC5          EQU  H'0005'
INLVLC6          EQU  H'0006'
INLVLC7          EQU  H'0007'


;----- IOCCP Bits -----------------------------------------------------
IOCCP0           EQU  H'0000'
IOCCP1           EQU  H'0001'
IOCCP3           EQU  H'0003'
IOCCP4           EQU  H'0004'
IOCCP5           EQU  H'0005'
IOCCP6           EQU  H'0006'
IOCCP7           EQU  H'0007'


;----- IOCCN Bits -----------------------------------------------------
IOCCN0           EQU  H'0000'
IOCCN1           EQU  H'0001'
IOCCN3           EQU  H'0003'
IOCCN4           EQU  H'0004'
IOCCN5           EQU  H'0005'
IOCCN6           EQU  H'0006'
IOCCN7           EQU  H'0007'


;----- IOCCF Bits -----------------------------------------------------
IOCCF0           EQU  H'0000'
IOCCF1           EQU  H'0001'
IOCCF3           EQU  H'0003'
IOCCF4           EQU  H'0004'
IOCCF5           EQU  H'0005'
IOCCF6           EQU  H'0006'
IOCCF7           EQU  H'0007'


;----- IOCWP Bits -----------------------------------------------------
IOCWP0           EQU  H'0000'
IOCWP1           EQU  H'0001'
IOCWP2           EQU  H'0002'
IOCWP3           EQU  H'0003'
IOCWP4           EQU  H'0004'
IOCWP5           EQU  H'0005'
IOCWP6           EQU  H'0006'
IOCWP7           EQU  H'0007'


;----- IOCWN Bits -----------------------------------------------------
IOCWN0           EQU  H'0000'
IOCWN1           EQU  H'0001'
IOCWN2           EQU  H'0002'
IOCWN3           EQU  H'0003'
IOCWN4           EQU  H'0004'
IOCWN5           EQU  H'0005'
IOCWN6           EQU  H'0006'
IOCWN7           EQU  H'0007'


;----- IOCWF Bits -----------------------------------------------------
IOCWF0           EQU  H'0000'
IOCWF1           EQU  H'0001'
IOCWF2           EQU  H'0002'
IOCWF3           EQU  H'0003'
IOCWF4           EQU  H'0004'
IOCWF5           EQU  H'0005'
IOCWF6           EQU  H'0006'
IOCWF7           EQU  H'0007'


;----- IVTLOCK Bits -----------------------------------------------------
IVTLOCKED        EQU  H'0000'


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


;----- PIR0 Bits -----------------------------------------------------
SWIF             EQU  H'0000'
INT0IF           EQU  H'0001'
INT1IF           EQU  H'0002'
INT2IF           EQU  H'0003'
DMA1SCNTIF       EQU  H'0004'
DMA1DCNTIF       EQU  H'0005'
DMA1ORIF         EQU  H'0006'
DMA1AIF          EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
DMA4SCNTIF       EQU  H'0000'
DMA4DCNTIF       EQU  H'0001'
DMA4ORIF         EQU  H'0002'
DMA4AIF          EQU  H'0003'
NVMIF            EQU  H'0004'
CRCIF            EQU  H'0005'
SCANIF           EQU  H'0006'
ACTIF            EQU  H'0007'


;----- PIR3 Bits -----------------------------------------------------
CSWIF            EQU  H'0000'
OSFIF            EQU  H'0001'
VDDIO2IF         EQU  H'0002'
VDDIO3IF         EQU  H'0003'
IOCIF            EQU  H'0004'
TMR0IF           EQU  H'0005'
TMR1IF           EQU  H'0006'
TMR1GIF          EQU  H'0007'


;----- PIR4 Bits -----------------------------------------------------
TMR2IF           EQU  H'0000'
TMR4IF           EQU  H'0001'
TU16AIF          EQU  H'0002'
TU16BIF          EQU  H'0003'
CCP1IF           EQU  H'0004'
CCP2IF           EQU  H'0005'
PWM1PIF          EQU  H'0006'
PWM1IF           EQU  H'0007'


;----- PIR5 Bits -----------------------------------------------------
PWM2PIF          EQU  H'0000'
PWM2IF           EQU  H'0001'
CWG1IF           EQU  H'0002'
CLC1IF           EQU  H'0003'
CLC2IF           EQU  H'0004'
CLC3IF           EQU  H'0005'
CLC4IF           EQU  H'0006'
IOCSRIF          EQU  H'0007'


;----- PIR6 Bits -----------------------------------------------------
U1RXIF           EQU  H'0000'
U1TXIF           EQU  H'0001'
U1IF             EQU  H'0002'
U1EIF            EQU  H'0003'
U2RXIF           EQU  H'0004'
U2TXIF           EQU  H'0005'
U2IF             EQU  H'0006'
U2EIF            EQU  H'0007'


;----- PIR7 Bits -----------------------------------------------------
SPI1RXIF         EQU  H'0000'
SPI1TXIF         EQU  H'0001'
SPI1IF           EQU  H'0002'
I2C1RXIF         EQU  H'0003'
I2C1TXIF         EQU  H'0004'
I2C1IF           EQU  H'0005'
I2C1EIF          EQU  H'0006'


;----- PIR8 Bits -----------------------------------------------------
I3C1RXIF         EQU  H'0000'
I3C1TXIF         EQU  H'0001'
I3C1IF           EQU  H'0002'
I3C1EIF          EQU  H'0003'
I3C1RIF          EQU  H'0004'


;----- PIR9 Bits -----------------------------------------------------
I3C2RXIF         EQU  H'0000'
I3C2TXIF         EQU  H'0001'
I3C2IF           EQU  H'0002'
I3C2EIF          EQU  H'0003'
I3C2RIF          EQU  H'0004'
HLVDIF           EQU  H'0005'
ADIF             EQU  H'0006'
ADTIF            EQU  H'0007'


;----- PIE0 Bits -----------------------------------------------------
SWIE             EQU  H'0000'
INT0IE           EQU  H'0001'
INT1IE           EQU  H'0002'
INT2IE           EQU  H'0003'
DMA1SCNTIE       EQU  H'0004'
DMA1DCNTIE       EQU  H'0005'
DMA1ORIE         EQU  H'0006'
DMA1AIE          EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
DMA4SCNTIE       EQU  H'0000'
DMA4DCNTIE       EQU  H'0001'
DMA4ORIE         EQU  H'0002'
DMA4AIE          EQU  H'0003'
NVMIE            EQU  H'0004'
CRCIE            EQU  H'0005'
SCANIE           EQU  H'0006'
ACTIE            EQU  H'0007'


;----- PIE3 Bits -----------------------------------------------------
CSWIE            EQU  H'0000'
OSFIE            EQU  H'0001'
VDDIO2IE         EQU  H'0002'
VDDIO3IE         EQU  H'0003'
IOCIE            EQU  H'0004'
TMR0IE           EQU  H'0005'
TMR1IE           EQU  H'0006'
TMR1GIE          EQU  H'0007'


;----- PIE4 Bits -----------------------------------------------------
TMR2IE           EQU  H'0000'
TMR4IE           EQU  H'0001'
TU16AIE          EQU  H'0002'
TU16BIE          EQU  H'0003'
CCP1IE           EQU  H'0004'
CCP2IE           EQU  H'0005'
PWM1PIE          EQU  H'0006'
PWM1IE           EQU  H'0007'


;----- PIE5 Bits -----------------------------------------------------
PWM2PIE          EQU  H'0000'
PWM2IE           EQU  H'0001'
CWG1IE           EQU  H'0002'
CLC1IE           EQU  H'0003'
CLC2IE           EQU  H'0004'
CLC3IE           EQU  H'0005'
CLC4IE           EQU  H'0006'
IOCSRIE          EQU  H'0007'


;----- PIE6 Bits -----------------------------------------------------
U1RXIE           EQU  H'0000'
U1TXIE           EQU  H'0001'
U1IE             EQU  H'0002'
U1EIE            EQU  H'0003'
U2RXIE           EQU  H'0004'
U2TXIE           EQU  H'0005'
U2IE             EQU  H'0006'
U2EIE            EQU  H'0007'


;----- PIE7 Bits -----------------------------------------------------
SPI1RXIE         EQU  H'0000'
SPI1TXIE         EQU  H'0001'
SPI1IE           EQU  H'0002'
I2C1RXIE         EQU  H'0003'
I2C1TXIE         EQU  H'0004'
I2C1IE           EQU  H'0005'
I2C1EIE          EQU  H'0006'


;----- PIE8 Bits -----------------------------------------------------
I3C1RXIE         EQU  H'0000'
I3C1TXIE         EQU  H'0001'
I3C1IE           EQU  H'0002'
I3C1EIE          EQU  H'0003'
I3C1RIE          EQU  H'0004'


;----- PIE9 Bits -----------------------------------------------------
I3C2RXIE         EQU  H'0000'
I3C2TXIE         EQU  H'0001'
I3C2IE           EQU  H'0002'
I3C2EIE          EQU  H'0003'
I3C2RIE          EQU  H'0004'
HLVDIE           EQU  H'0005'
ADIE             EQU  H'0006'
ADTIE            EQU  H'0007'


;----- IPR0 Bits -----------------------------------------------------
SWIP             EQU  H'0000'
INT0IP           EQU  H'0001'
INT1IP           EQU  H'0002'
INT2IP           EQU  H'0003'
DMA1SCNTIP       EQU  H'0004'
DMA1DCNTIP       EQU  H'0005'
DMA1ORIP         EQU  H'0006'
DMA1AIP          EQU  H'0007'


;----- IPR2 Bits -----------------------------------------------------
DMA4SCNTIP       EQU  H'0000'
DMA4DCNTIP       EQU  H'0001'
DMA4ORIP         EQU  H'0002'
DMA4AIP          EQU  H'0003'
NVMIP            EQU  H'0004'
CRCIP            EQU  H'0005'
SCANIP           EQU  H'0006'
ACTIP            EQU  H'0007'


;----- IPR3 Bits -----------------------------------------------------
CSWIP            EQU  H'0000'
OSFIP            EQU  H'0001'
VDDIO2IP         EQU  H'0002'
VDDIO3IP         EQU  H'0003'
IOCIP            EQU  H'0004'
TMR0IP           EQU  H'0005'
TMR1IP           EQU  H'0006'
TMR1GIP          EQU  H'0007'


;----- IPR4 Bits -----------------------------------------------------
TMR2IP           EQU  H'0000'
TMR4IP           EQU  H'0001'
TU16AIP          EQU  H'0002'
TU16BIP          EQU  H'0003'
CCP1IP           EQU  H'0004'
CCP2IP           EQU  H'0005'
PWM1PIP          EQU  H'0006'
PWM1IP           EQU  H'0007'


;----- IPR5 Bits -----------------------------------------------------
PWM2PIP          EQU  H'0000'
PWM2IP           EQU  H'0001'
CWG1IP           EQU  H'0002'
CLC1IP           EQU  H'0003'
CLC2IP           EQU  H'0004'
CLC3IP           EQU  H'0005'
CLC4IP           EQU  H'0006'
IOCSRIP          EQU  H'0007'


;----- IPR6 Bits -----------------------------------------------------
U1RXIP           EQU  H'0000'
U1TXIP           EQU  H'0001'
U1IP             EQU  H'0002'
U1EIP            EQU  H'0003'
U2RXIP           EQU  H'0004'
U2TXIP           EQU  H'0005'
U2IP             EQU  H'0006'
U2EIP            EQU  H'0007'


;----- IPR7 Bits -----------------------------------------------------
SPI1RXIP         EQU  H'0000'
SPI1TXIP         EQU  H'0001'
SPI1IP           EQU  H'0002'
I2C1RXIP         EQU  H'0003'
I2C1TXIP         EQU  H'0004'
I2C1IP           EQU  H'0005'
I2C1EIP          EQU  H'0006'


;----- IPR8 Bits -----------------------------------------------------
I3C1RXIP         EQU  H'0000'
I3C1TXIP         EQU  H'0001'
I3C1IP           EQU  H'0002'
I3C1EIP          EQU  H'0003'
I3C1RIP          EQU  H'0004'


;----- IPR9 Bits -----------------------------------------------------
I3C2RXIP         EQU  H'0000'
I3C2TXIP         EQU  H'0001'
I3C2IP           EQU  H'0002'
I3C2EIP          EQU  H'0003'
I3C2RIP          EQU  H'0004'
HLVDIP           EQU  H'0005'
ADIP             EQU  H'0006'
ADTIP            EQU  H'0007'


;----- PORTA Bits -----------------------------------------------------
RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'
RA4              EQU  H'0004'
RA5              EQU  H'0005'


;----- PORTB Bits -----------------------------------------------------
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PORTC Bits -----------------------------------------------------
RC0              EQU  H'0000'
RC1              EQU  H'0001'
RC3              EQU  H'0003'
RC4              EQU  H'0004'
RC5              EQU  H'0005'
RC6              EQU  H'0006'
RC7              EQU  H'0007'


;----- TRISA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'


;----- TRISB Bits -----------------------------------------------------
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'
TRISB7           EQU  H'0007'


;----- TRISC Bits -----------------------------------------------------
TRISC0           EQU  H'0000'
TRISC1           EQU  H'0001'
TRISC3           EQU  H'0003'
TRISC4           EQU  H'0004'
TRISC5           EQU  H'0005'
TRISC6           EQU  H'0006'
TRISC7           EQU  H'0007'


;----- LATA Bits -----------------------------------------------------
LATA0            EQU  H'0000'
LATA1            EQU  H'0001'
LATA2            EQU  H'0002'
LATA4            EQU  H'0004'
LATA5            EQU  H'0005'


;----- LATB Bits -----------------------------------------------------
LATB5            EQU  H'0005'
LATB6            EQU  H'0006'
LATB7            EQU  H'0007'


;----- LATC Bits -----------------------------------------------------
LATC0            EQU  H'0000'
LATC1            EQU  H'0001'
LATC3            EQU  H'0003'
LATC4            EQU  H'0004'
LATC5            EQU  H'0005'
LATC6            EQU  H'0006'
LATC7            EQU  H'0007'


;----- PORTW Bits -----------------------------------------------------
RW0              EQU  H'0000'
RW1              EQU  H'0001'
RW2              EQU  H'0002'
RW3              EQU  H'0003'
RW4              EQU  H'0004'
RW5              EQU  H'0005'
RW6              EQU  H'0006'
RW7              EQU  H'0007'


;----- LATW Bits -----------------------------------------------------
LATW0            EQU  H'0000'
LATW1            EQU  H'0001'
LATW2            EQU  H'0002'
LATW3            EQU  H'0003'
LATW4            EQU  H'0004'
LATW5            EQU  H'0005'
LATW6            EQU  H'0006'
LATW7            EQU  H'0007'


;----- PORTWIN0 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN2 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN3 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN4 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN5 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN6 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWIN7 Bits -----------------------------------------------------
IN0              EQU  H'0000'
IN1              EQU  H'0001'
IN2              EQU  H'0002'


;----- PORTWCLK Bits -----------------------------------------------------
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'
CLK4             EQU  H'0004'


;----- PORTWDF Bits -----------------------------------------------------
DF0              EQU  H'0000'
DF1              EQU  H'0001'
DF2              EQU  H'0002'
DF3              EQU  H'0003'
DF4              EQU  H'0004'
DF5              EQU  H'0005'
DF6              EQU  H'0006'
DF7              EQU  H'0007'
PWDF0            EQU  H'0000'
PWDF1            EQU  H'0001'
PWDF2            EQU  H'0002'
PWDF3            EQU  H'0003'
PWDF4            EQU  H'0004'
PWDF5            EQU  H'0005'
PWDF6            EQU  H'0006'
PWDF7            EQU  H'0007'


;----- PORTWCON Bits -----------------------------------------------------
CLKEN            EQU  H'0000'


;----- MVIOSTAT Bits -----------------------------------------------------
VDDIO2RDY        EQU  H'0000'
VDDIO3RDY        EQU  H'0001'


;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
OV_STATUS        EQU  H'0003'
N                EQU  H'0004'
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
FSR212           EQU  H'0012'
FSR213           EQU  H'0013'


;----- FSR2H Bits -----------------------------------------------------
FSR2H0           EQU  H'0000'
FSR2H1           EQU  H'0001'
FSR2H2           EQU  H'0002'
FSR2H3           EQU  H'0003'
FSR2H4           EQU  H'0004'
FSR2H5           EQU  H'0005'


;----- PLUSW2 Bits -----------------------------------------------------


;----- PREINC2 Bits -----------------------------------------------------


;----- POSTDEC2 Bits -----------------------------------------------------


;----- POSTINC2 Bits -----------------------------------------------------


;----- INDF2 Bits -----------------------------------------------------


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
FSR012           EQU  H'0012'
FSR013           EQU  H'0013'


;----- FSR0H Bits -----------------------------------------------------
FSR0H0           EQU  H'0000'
FSR0H1           EQU  H'0001'
FSR0H2           EQU  H'0002'
FSR0H3           EQU  H'0003'
FSR0H4           EQU  H'0004'
FSR0H5           EQU  H'0005'


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
NOT_PORVDDIO2    EQU  H'0003'
NOT_PORVDDIO3    EQU  H'0004'
NRCM             EQU  H'0000'
NMEMV            EQU  H'0001'
NRVREG           EQU  H'0002'
NPORVDDIO2       EQU  H'0003'
NPORVDDIO3       EQU  H'0004'
RCM              EQU  H'0000'
MEMV             EQU  H'0001'
RVREG            EQU  H'0002'
PORVDDIO2        EQU  H'0003'
PORVDDIO3        EQU  H'0004'


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
STKPTR6          EQU  H'0006'


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

__MAXRAM  H'08FF'
__BADRAM  H'0000'-H'0037'
__BADRAM  H'0102'
__BADRAM  H'010E'-H'0119'
__BADRAM  H'0125'-H'012A'
__BADRAM  H'0171'-H'0198'
__BADRAM  H'01A2'
__BADRAM  H'01BF'
__BADRAM  H'01C4'
__BADRAM  H'01C6'
__BADRAM  H'01E3'
__BADRAM  H'0200'
__BADRAM  H'0204'-H'0214'
__BADRAM  H'023A'
__BADRAM  H'023B'
__BADRAM  H'023C'
__BADRAM  H'0305'-H'030E'
__BADRAM  H'0311'
__BADRAM  H'0314'
__BADRAM  H'0317'-H'0318'
__BADRAM  H'031C'
__BADRAM  H'031F'
__BADRAM  H'0320'
__BADRAM  H'0321'
__BADRAM  H'0322'
__BADRAM  H'0323'
__BADRAM  H'0324'
__BADRAM  H'0325'
__BADRAM  H'032B'
__BADRAM  H'0331'-H'0350'
__BADRAM  H'0358'-H'035F'
__BADRAM  H'0362'-H'0364'
__BADRAM  H'0367'-H'0368'
__BADRAM  H'036B'-H'0372'
__BADRAM  H'0388'-H'0398'
__BADRAM  H'039A'-H'039C'
__BADRAM  H'03A5'-H'03A6'
__BADRAM  H'03AA'-H'03AC'
__BADRAM  H'03AF'-H'03B0'
__BADRAM  H'03B2'
__BADRAM  H'0400'-H'0415'
__BADRAM  H'041E'-H'041F'
__BADRAM  H'0428'-H'0429'
__BADRAM  H'0432'-H'0456'
__BADRAM  H'048A'
__BADRAM  H'048B'
__BADRAM  H'048C'
__BADRAM  H'0490'
__BADRAM  H'0491'
__BADRAM  H'0492'
__BADRAM  H'0496'
__BADRAM  H'0497'
__BADRAM  H'0498'
__BADRAM  H'04A7'-H'04D7'


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
_CONFIG10     EQU  H'300008'
_CONFIG11     EQU  H'300009'
_CONFIG12     EQU  H'30000A'
_CONFIG14     EQU  H'300018'
_CONFIG9      EQU  H'300019'

;----- CONFIG1 Options --------------------------------------------------
_FEXTOSC_HS_8MHZ_1   EQU  H'F8'; HS (crystal oscillator) up to 8 MHz
_FEXTOSC_HS_16MHZ_1  EQU  H'F9'; HS (crystal oscillator) up to 16 MHz
_FEXTOSC_HS_24MHZ_1  EQU  H'FA'; HS (crystal oscillator) up to 24 MHz
_FEXTOSC_HS_32MHZ_1  EQU  H'FB'; HS (crystal oscillator) up to 32 MHz
_FEXTOSC_OFF_1       EQU  H'FC'; Oscillator not enabled
_FEXTOSC_ECL_1       EQU  H'FD'; EC (external clock) below 500 kHz
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
_BBEN_OFF_2          EQU  H'FF'; User PFM code protection disabled
_BBEN_ON_2           EQU  H'FB'; User PFM code protection enabled
_CSWEN_OFF_2         EQU  H'F7'; The NOSC and NDIV bits cannot be changed by user software
_CSWEN_ON_2          EQU  H'FF'; Writing to NOSC and NDIV is allowed
_FCMEN_OFF_2         EQU  H'DF'; Fail-Safe Clock Monitor disabled
_FCMEN_ON_2          EQU  H'FF'; Fail-Safe Clock Monitor enabled
_FCMENP_OFF_2        EQU  H'BF'; Fail-Safe Clock Monitor disabled
_FCMENP_ON_2         EQU  H'FF'; Fail-Safe Clock Monitor enabled; timer will flag FSCMP bit and OSFIF interrupt on EXTOSC failure.
_FCMENS_OFF_2        EQU  H'7F'; Fail-Safe Clock Monitor Disabled
_FCMENS_ON_2         EQU  H'FF'; Fail-Safe Clock Monitor enabled; timer will flag FSCMS bit and OSFIF interrupt on SOSC failure.

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
_BOREN_ON_3          EQU  H'FF'; Brown-out Reset enabled , SBOREN bit is ignored
_BOREN_NOSLEEP_3     EQU  H'BF'; Brown-out Reset enabled while running, disabled in Sleep; SBOREN is ignored
_BOREN_SBOREN_3      EQU  H'7F'; Brown-out Reset enabled according to SBOREN
_BOREN_OFF_3         EQU  H'3F'; Brown-out Reset disabled

;----- CONFIG4 Options --------------------------------------------------
_BORV_VBOR_1P9_4     EQU  H'FF'; Brown-out Reset Voltage (VBOR) set to 1.9V
_BORV_VBOR_2P45_4    EQU  H'FE'; Brown-out Reset Voltage (VBOR) set to 2.45V
_BORV_VBOR_2P7_4     EQU  H'FD'; Brown-out Reset Voltage (VBOR) set to 2.7V
_BORV_VBOR_2P85_4    EQU  H'FC'; Brown-out Reset Voltage (VBOR) set to 2.8V
_PPS1WAY_ON_4        EQU  H'FF'; PPSLOCKED bit can be cleared and set only once; PPS registers remain locked after one clear/set cycle
_PPS1WAY_OFF_4       EQU  H'F7'; PPSLOCKED bit can be set and cleared repeatedly (subject to the unlock sequence)
_STVREN_ON_4         EQU  H'FF'; Stack full/underflow will cause Reset
_STVREN_OFF_4        EQU  H'EF'; Stack full/underflow will not cause Reset
_LVP_ON_4            EQU  H'FF'; Low voltage programming enabled. MCLR/VPP pin function is MCLR. MCLRE configuration bit is ignored
_LVP_OFF_4           EQU  H'DF'; HV on MCLR/VPP must be used for programming
_DEBUG_OFF_4         EQU  H'FF'; Backround debugger disabled
_DEBUG_ON_4          EQU  H'BF'; Background debugger enabled
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
_WDTE_OFF_5          EQU  H'9F'; WDT Disabled; SEN is ignored
_WDTE_SEN_5          EQU  H'BF'; WDT enabled/disabled by SEN bit
_WDTE_NSLEEP_5       EQU  H'DF'; WDT enabled while sleep=0, suspended when sleep=1; SEN ignored
_WDTE_ON_5           EQU  H'FF'; WDT enabled regardless of sleep; SEN is ignored

;----- CONFIG6 Options --------------------------------------------------
_WDTCWS_WDTCWS_0_6   EQU  H'F8'; window delay = 87.5; no software control; keyed access required
_WDTCWS_WDTCWS_1_6   EQU  H'F9'; window delay = 75 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_2_6   EQU  H'FA'; window delay = 62.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_3_6   EQU  H'FB'; window delay = 50 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_4_6   EQU  H'FC'; window delay = 37.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_5_6   EQU  H'FD'; window delay = 25 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_6_6   EQU  H'FE'; window always open (100%); no software control; keyed access required
_WDTCWS_WDTCWS_7_6   EQU  H'FF'; window always open (100%); software control; keyed access not required
_WDTCCS_LFINTOSC_6   EQU  H'C7'; WDT reference clock is the 31.0 kHz LFINTOSC (when WDTE != 0b00)
_WDTCCS_MFINTOSC_6   EQU  H'CF'; WDT reference clock is the 31.25kHz MFINTOSC output (when WDTE != 0b00)
_WDTCCS_SOSC_6       EQU  H'D7'; WDT reference clock is SOSC (when WDTE != 0b00)
_WDTCCS_SC_6         EQU  H'FF'; Software Control (when WDTE != 0b00)

;----- CONFIG7 Options --------------------------------------------------
_VDDIO2MD_STANDARD_RANGE_7 EQU  H'FF'; Standard Operating Range (VDDIO2 >= 1.62V)
_VDDIO2MD_LOW_VOLTAGE_RANGE_7 EQU  H'FE'; Low-voltage Operating Range (VDDIO2 < 1.62V)
_VDDIO3MD_STANDARD_RANGE_7 EQU  H'FF'; Standard Operating Range (VDDIO3 >= 1.62V)
_VDDIO3MD_LOW_VOLTAGE_RANGE_7 EQU  H'FD'; Low-voltage Operating Range (VDDIO3 < 1.62V)

;----- CONFIG8 Options --------------------------------------------------
_BBSIZE_BBSIZE_4096_8 EQU  H'1F'; Boot Block size is 4096 words
_BBSIZE_BBSIZE_3968_8 EQU  H'1E'; Boot Block size is 3968 words
_BBSIZE_BBSIZE_3840_8 EQU  H'1D'; Boot Block size is 3840 words
_BBSIZE_BBSIZE_3712_8 EQU  H'1C'; Boot Block size is 3712 words
_BBSIZE_BBSIZE_3584_8 EQU  H'1B'; Boot Block size is 3584 words
_BBSIZE_BBSIZE_3456_8 EQU  H'1A'; Boot Block size is 3456 words
_BBSIZE_BBSIZE_3328_8 EQU  H'19'; Boot Block size is 3328 words
_BBSIZE_BBSIZE_3200_8 EQU  H'18'; Boot Block size is 3200 words
_BBSIZE_BBSIZE_3072_8 EQU  H'17'; Boot Block size is 3072 words
_BBSIZE_BBSIZE_2944_8 EQU  H'16'; Boot Block size is 2944 words
_BBSIZE_BBSIZE_2816_8 EQU  H'15'; Boot Block size is 2816 words
_BBSIZE_BBSIZE_2688_8 EQU  H'14'; Boot Block size is 2688 words
_BBSIZE_BBSIZE_2560_8 EQU  H'13'; Boot Block size is 2560 words
_BBSIZE_BBSIZE_2432_8 EQU  H'12'; Boot Block size is 2432 words
_BBSIZE_BBSIZE_2304_8 EQU  H'11'; Boot Block size is 2304 words
_BBSIZE_BBSIZE_2176_8 EQU  H'10'; Boot Block size is 2176 words
_BBSIZE_BBSIZE_2048_8 EQU  H'F0'; Boot Block size is 2048 words
_BBSIZE_BBSIZE_1920_8 EQU  H'E0'; Boot Block size is 1920 words
_BBSIZE_BBSIZE_1792_8 EQU  H'D0'; Boot Block size is 1792 words
_BBSIZE_BBSIZE_1664_8 EQU  H'C0'; Boot Block size is 1664 words
_BBSIZE_BBSIZE_1536_8 EQU  H'B0'; Boot Block size is 1536 words
_BBSIZE_BBSIZE_1408_8 EQU  H'A0'; Boot Block size is 1408 words
_BBSIZE_BBSIZE_1280_8 EQU  H'90'; Boot Block size is 1280 words
_BBSIZE_BBSIZE_1152_8 EQU  H'80'; Boot Block size is 1152 words
_BBSIZE_BBSIZE_1024_8 EQU  H'70'; Boot Block size is 1024 words
_BBSIZE_BBSIZE_896_8 EQU  H'60'; Boot Block size is 896 words
_BBSIZE_BBSIZE_768_8 EQU  H'50'; Boot Block size is 768 words
_BBSIZE_BBSIZE_640_8 EQU  H'40'; Boot Block size is 640 words
_BBSIZE_BBSIZE_512_8 EQU  H'30'; Boot Block size is 512 words
_BBSIZE_BBSIZE_384_8 EQU  H'20'; Boot Block size is 384 words
_BBSIZE_BBSIZE_256_8 EQU  H'10'; Boot Block size is 256 words
_BBSIZE_BBSIZE_128_8 EQU  H'00'; Boot Block size is 128 words

;----- CONFIG10 Options --------------------------------------------------
_WRTB_OFF_10         EQU  H'FF'; Boot Block is not write-protected
_WRTB_ON_10          EQU  H'FE'; Boot Block is write-protected
_WRTC_OFF_10         EQU  H'FF'; Configuration registers are not write-protected
_WRTC_ON_10          EQU  H'FD'; Configuration registers are write-protected
_WRTD_OFF_10         EQU  H'FF'; Data EEPROM is not write-protected
_WRTD_ON_10          EQU  H'FB'; Data EEPROM is write-protected
_WRTSAF_OFF_10       EQU  H'FF'; SAF is not write-protected
_WRTSAF_ON_10        EQU  H'F7'; SAF is write-protected
_WRTAPP_OFF_10       EQU  H'FF'; Application Block is not write-protected
_WRTAPP_ON_10        EQU  H'7F'; Application Block is write-protected

;----- CONFIG11 Options --------------------------------------------------
_CP_OFF_11           EQU  H'FF'; User PFM code protection is disabled
_CP_ON_11            EQU  H'FE'; User PFM code protection is enabled

;----- CONFIG12 Options --------------------------------------------------
_CPD_OFF_12          EQU  H'FF'; Data EEPROM code protection is disabled
_CPD_ON_12           EQU  H'FE'; Data EEPROM code protection is enabled

;----- CONFIG14 Options --------------------------------------------------
_SAFLOCK_OFF_14      EQU  H'FF'; SAF Lock disabled
_SAFLOCK_ON_14       EQU  H'FE'; SAF Lock enabled, SAF areas are locked and SAFSZ bits cannot be erased

;----- CONFIG9 Options --------------------------------------------------
_SAFSZ_SAFSZ_NONE_9  EQU  H'FF'; NONE
_SAFSZ_SAFSZ_128_9   EQU  H'FE'; Last 128 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_256_9   EQU  H'FC'; Last 256 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_384_9   EQU  H'F8'; Last 384 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_512_9   EQU  H'F0'; Last 512 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_640_9   EQU  H'E0'; Last 640 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_768_9   EQU  H'C0'; Last 768 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_896_9   EQU  H'80'; Last 896 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits
_SAFSZ_SAFSZ_1024_9  EQU  H'00'; Last 1024 words of PFM; self write operation depends on WRTSAF and SAFLOCK bits

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
