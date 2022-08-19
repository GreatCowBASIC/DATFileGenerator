        LIST
;==========================================================================
;
;  Built by Great Cow BASIC convertor
;  MPASM processor include for the chip shown below
;
; C:\Users\admin\.mchp_packs\index.idx
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\ini\16F17126.ini
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\edc\PIC16F17126.PIC
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\cfgdata\16F17126.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __16F17126
           MESSG "Processor-header file mismatch.  Verify selected processor."
        ENDIF
;==========================================================================
;
;       Register Definitions
;
;==========================================================================

;----- Register Files -----------------------------------------------------

;-----Bank0------------------
INDF0            EQU  H'0000'
INDF1            EQU  H'0001'
PCL              EQU  H'0002'
STATUS           EQU  H'0003'
FSR0             EQU  H'0004'
FSR0L            EQU  H'0004'
FSR0H            EQU  H'0005'
FSR1             EQU  H'0006'
FSR1L            EQU  H'0006'
FSR1H            EQU  H'0007'
BSR              EQU  H'0008'
WREG             EQU  H'0009'
PCLATH           EQU  H'000A'
INTCON           EQU  H'000B'
PORTA            EQU  H'000C'
PORTC            EQU  H'000E'
TRISA            EQU  H'0012'
TRISC            EQU  H'0014'
LATA             EQU  H'0018'
LATC             EQU  H'001A'

;-----Bank1------------------
PIR0             EQU  H'008C'
PIR1             EQU  H'008D'
PIR2             EQU  H'008E'
PIR3             EQU  H'008F'
PIR4             EQU  H'0090'
PIR5             EQU  H'0091'
PIR6             EQU  H'0092'
PIE0             EQU  H'0096'
PIE1             EQU  H'0097'
PIE2             EQU  H'0098'
PIE3             EQU  H'0099'
PIE4             EQU  H'009A'
PIE5             EQU  H'009B'
PIE6             EQU  H'009C'

;-----Bank2------------------
PMD0             EQU  H'010C'
PMD1             EQU  H'010D'
PMD2             EQU  H'010E'
PMD3             EQU  H'010F'
PMD4             EQU  H'0110'

;-----Bank3------------------
WDTCON0          EQU  H'018C'
WDTCON1          EQU  H'018D'
WDTPS            EQU  H'018E'
WDTPSL           EQU  H'018E'
WDTPSH           EQU  H'018F'
WDTTMR           EQU  H'0190'
BORCON           EQU  H'0191'
PCON0            EQU  H'0192'
PCON1            EQU  H'0193'
TMR0L            EQU  H'019C'
TMR0             EQU  H'019C'
TMR0H            EQU  H'019D'
PR0              EQU  H'019D'
T0CON0           EQU  H'019E'
T0CON1           EQU  H'019F'

;-----Bank4------------------
FVRCON           EQU  H'020C'
CPCON            EQU  H'020D'
ZCDCON           EQU  H'021F'

;-----Bank5------------------
CPUDOZE          EQU  H'028C'
OSCCON1          EQU  H'028D'
OSCCON2          EQU  H'028E'
OSCCON3          EQU  H'028F'
OSCSTAT          EQU  H'0290'
OSCEN            EQU  H'0291'
OSCTUNE          EQU  H'0292'
OSCFRQ           EQU  H'0293'
ACTCON           EQU  H'0294'
CLKRCON          EQU  H'0296'
CLKRCLK          EQU  H'0297'

;-----Bank6------------------
TMR1             EQU  H'030C'
TMR1L            EQU  H'030C'
TMR1H            EQU  H'030D'
T1CON            EQU  H'030E'
T1GCON           EQU  H'030F'
PR1              EQU  H'030F'
T1GATE           EQU  H'0310'
TMR1GATE         EQU  H'0310'
T1CLK            EQU  H'0311'
TMR1CLK          EQU  H'0311'
TMR3             EQU  H'0312'
TMR3L            EQU  H'0312'
TMR3H            EQU  H'0313'
T3CON            EQU  H'0314'
T3GCON           EQU  H'0315'
PR3              EQU  H'0315'
T3GATE           EQU  H'0316'
TMR3GATE         EQU  H'0316'
T3CLK            EQU  H'0317'
TMR3CLK          EQU  H'0317'

;-----Bank7------------------
T2TMR            EQU  H'038C'
TMR2             EQU  H'038C'
T2PR             EQU  H'038D'
PR2              EQU  H'038D'
T2CON            EQU  H'038E'
T2HLT            EQU  H'038F'
T2CLKCON         EQU  H'0390'
T2CLK            EQU  H'0390'
T2RST            EQU  H'0391'
T4TMR            EQU  H'0392'
TMR4             EQU  H'0392'
T4PR             EQU  H'0393'
PR4              EQU  H'0393'
T4CON            EQU  H'0394'
T4HLT            EQU  H'0395'
T4CLKCON         EQU  H'0396'
T4CLK            EQU  H'0396'
T4RST            EQU  H'0397'

;-----Bank8------------------
CCPR1            EQU  H'040C'
CCPR1L           EQU  H'040C'
CCPR1H           EQU  H'040D'
CCP1CON          EQU  H'040E'
CCP1CAP          EQU  H'040F'
CCPR2            EQU  H'0410'
CCPR2L           EQU  H'0410'
CCPR2H           EQU  H'0411'
CCP2CON          EQU  H'0412'
CCP2CAP          EQU  H'0413'
CCPTMRS0         EQU  H'041F'

;-----Bank9------------------
PWM1ERS          EQU  H'048C'
PWM1CLK          EQU  H'048D'
PWM1LDS          EQU  H'048E'
PWM1PR           EQU  H'048F'
PWM1PRL          EQU  H'048F'
PWM1PRH          EQU  H'0490'
PWM1CPRE         EQU  H'0491'
PWM1PIPOS        EQU  H'0492'
PWM1GIR          EQU  H'0493'
PWM1GIE          EQU  H'0494'
PWM1CON          EQU  H'0495'
PWM1S1CFG        EQU  H'0496'
PWM1S1P1         EQU  H'0497'
PWM1S1P1L        EQU  H'0497'
PWM1S1P1H        EQU  H'0498'
PWM1S1P2         EQU  H'0499'
PWM1S1P2L        EQU  H'0499'
PWM1S1P2H        EQU  H'049A'
PWMLOAD          EQU  H'049E'
PWMEN            EQU  H'049F'

;-----Bank10------------------
PWM2ERS          EQU  H'050C'
PWM2CLK          EQU  H'050D'
PWM2LDS          EQU  H'050E'
PWM2PR           EQU  H'050F'
PWM2PRL          EQU  H'050F'
PWM2PRH          EQU  H'0510'
PWM2CPRE         EQU  H'0511'
PWM2PIPOS        EQU  H'0512'
PWM2GIR          EQU  H'0513'
PWM2GIE          EQU  H'0514'
PWM2CON          EQU  H'0515'
PWM2S1CFG        EQU  H'0516'
PWM2S1P1         EQU  H'0517'
PWM2S1P1L        EQU  H'0517'
PWM2S1P1H        EQU  H'0518'
PWM2S1P2         EQU  H'0519'
PWM2S1P2L        EQU  H'0519'
PWM2S1P2H        EQU  H'051A'

;-----Bank11------------------
NCO1ACC          EQU  H'058C'
NCO1ACCL         EQU  H'058C'
NCO1ACCH         EQU  H'058D'
NCO1ACCU         EQU  H'058E'
NCO1INC          EQU  H'058F'
NCO1INCL         EQU  H'058F'
NCO1INCH         EQU  H'0590'
NCO1INCU         EQU  H'0591'
NCO1CON          EQU  H'0592'
NCO1CLK          EQU  H'0593'

;-----Bank12------------------
CWG1CLK          EQU  H'060C'
CWG1CLKCON       EQU  H'060C'
CWG1ISM          EQU  H'060D'
CWG1DAT          EQU  H'060D'
CWG1DBR          EQU  H'060E'
CWG1DBF          EQU  H'060F'
CWG1CON0         EQU  H'0610'
CWG1CON1         EQU  H'0611'
CWG1AS0          EQU  H'0612'
CWG1AS1          EQU  H'0613'
CWG1STR          EQU  H'0614'

;-----Bank13------------------
CLCNCON          EQU  H'068C'
CLCNPOL          EQU  H'068D'
CLCNSEL0         EQU  H'068E'
CLCNSEL1         EQU  H'068F'
CLCNSEL2         EQU  H'0690'
CLCNSEL3         EQU  H'0691'
CLCNGLS0         EQU  H'0692'
CLCNGLS1         EQU  H'0693'
CLCNGLS2         EQU  H'0694'
CLCNGLS3         EQU  H'0695'
CLCSELECT        EQU  H'0696'
CLCDATA          EQU  H'0697'

;-----Bank14------------------
RC1REG           EQU  H'070C'
TX1REG           EQU  H'070D'
SP1BRG           EQU  H'070E'
SP1BRGL          EQU  H'070E'
SP1BRGH          EQU  H'070F'
RC1STA           EQU  H'0710'
TX1STA           EQU  H'0711'
BAUD1CON         EQU  H'0712'
RC2REG           EQU  H'0716'
TX2REG           EQU  H'0717'
SP2BRG           EQU  H'0718'
SP2BRGL          EQU  H'0718'
SP2BRGH          EQU  H'0719'
RC2STA           EQU  H'071A'
TX2STA           EQU  H'071B'
BAUD2CON         EQU  H'071C'

;-----Bank15------------------
SSP1BUF          EQU  H'078C'
SSP1ADD          EQU  H'078D'
SSP1MSK          EQU  H'078E'
SSP1STAT         EQU  H'078F'
SSP1CON1         EQU  H'0790'
SSP1CON2         EQU  H'0791'
SSP1CON3         EQU  H'0792'
SSP2BUF          EQU  H'0796'
SSP2ADD          EQU  H'0797'
SSP2MSK          EQU  H'0798'
SSP2STAT         EQU  H'0799'
SSP2CON1         EQU  H'079A'
SSP2CON2         EQU  H'079B'
SSP2CON3         EQU  H'079C'

;-----Bank16------------------
CM1CON0          EQU  H'080C'
CM1CON1          EQU  H'080D'
CM1NCH           EQU  H'080E'
CM1PCH           EQU  H'080F'
CM2CON0          EQU  H'0810'
CM2CON1          EQU  H'0811'
CM2NCH           EQU  H'0812'
CM2PCH           EQU  H'0813'
CMOUT            EQU  H'081F'
CMSTAT           EQU  H'081F'

;-----Bank17------------------
DAC1CON          EQU  H'088C'
DAC1DATL         EQU  H'088D'
DAC2CON          EQU  H'0890'
DAC2DATL         EQU  H'0891'

;-----Bank18------------------
OPA1CON0         EQU  H'090C'
OPA1CON1         EQU  H'090D'
OPA1CON2         EQU  H'090E'
OPA1CON3         EQU  H'090F'
OPA1HWC          EQU  H'0910'
OPA1OFFSET       EQU  H'0911'
OPA1ORS          EQU  H'0912'

;-----Bank57------------------
NVMADR           EQU  H'1C8C'
NVMADRL          EQU  H'1C8C'
NVMADRH          EQU  H'1C8D'
NVMDAT           EQU  H'1C8E'
NVMDATL          EQU  H'1C8E'
NVMDATH          EQU  H'1C8F'
NVMCON1          EQU  H'1C90'
NVMCON2          EQU  H'1C91'
SCANLADR         EQU  H'1C92'
SCANLADRL        EQU  H'1C92'
SCANLADRH        EQU  H'1C93'
SCANHADR         EQU  H'1C95'
SCANHADRL        EQU  H'1C95'
SCANHADRH        EQU  H'1C96'
SCANCON0         EQU  H'1C98'
SCANTRIG         EQU  H'1C99'
CRCDATA          EQU  H'1C9A'
CRCDATAL         EQU  H'1C9A'
CRCDATAH         EQU  H'1C9B'
CRCDATAU         EQU  H'1C9C'
CRCDATAT         EQU  H'1C9D'
CRCOUT           EQU  H'1C9E'
CRCSHFT          EQU  H'1C9E'
CRCXOR           EQU  H'1C9E'
CRCOUTL          EQU  H'1C9E'
CRCSHFTL         EQU  H'1C9E'
CRCSHIFTL        EQU  H'1C9E'
CRCXORL          EQU  H'1C9E'
CRCOUTH          EQU  H'1C9F'
CRCSHFTH         EQU  H'1C9F'
CRCSHIFTH        EQU  H'1C9F'
CRCXORH          EQU  H'1C9F'
CRCOUTU          EQU  H'1CA0'
CRCSHFTU         EQU  H'1CA0'
CRCSHIFTU        EQU  H'1CA0'
CRCXORU          EQU  H'1CA0'
CRCOUTT          EQU  H'1CA1'
CRCSHFTT         EQU  H'1CA1'
CRCSHIFTT        EQU  H'1CA1'
CRCXORT          EQU  H'1CA1'
CRCCON0          EQU  H'1CA2'
CRCCON1          EQU  H'1CA3'
CRCCON2          EQU  H'1CA4'

;-----Bank58------------------
ADLTH            EQU  H'1D0C'
ADLTHL           EQU  H'1D0C'
ADLTHH           EQU  H'1D0D'
ADUTH            EQU  H'1D0E'
ADUTHL           EQU  H'1D0E'
ADUTHH           EQU  H'1D0F'
ADERR            EQU  H'1D10'
ADERRL           EQU  H'1D10'
ADERRH           EQU  H'1D11'
ADSTPT           EQU  H'1D12'
ADSTPTL          EQU  H'1D12'
ADSTPTH          EQU  H'1D13'
ADFLTR           EQU  H'1D14'
ADFLTRL          EQU  H'1D14'
ADFLTRH          EQU  H'1D15'
ADACC            EQU  H'1D16'
ADACCL           EQU  H'1D16'
ADACCH           EQU  H'1D17'
ADACCU           EQU  H'1D18'
ADCNT            EQU  H'1D19'
ADRPT            EQU  H'1D1A'
ADPREV           EQU  H'1D1B'
ADPREVL          EQU  H'1D1B'
ADPREVH          EQU  H'1D1C'
ADRES            EQU  H'1D1D'
ADRESL           EQU  H'1D1D'
ADRESH           EQU  H'1D1E'
ADPCH            EQU  H'1D1F'
ADNCH            EQU  H'1D20'
ADACQ            EQU  H'1D21'
ADACQL           EQU  H'1D21'
ADACQH           EQU  H'1D22'
ADCAP            EQU  H'1D23'
ADPRE            EQU  H'1D24'
ADPREL           EQU  H'1D24'
ADPREH           EQU  H'1D25'
ADCON0           EQU  H'1D26'
ADCON1           EQU  H'1D27'
ADCON2           EQU  H'1D28'
ADCON3           EQU  H'1D29'
ADSTAT           EQU  H'1D2A'
ADREF            EQU  H'1D2B'
ADACT            EQU  H'1D2C'
ADCLK            EQU  H'1D2D'
ADCG1A           EQU  H'1D2E'
ADCG1C           EQU  H'1D30'

;-----Bank59------------------
RA0PPS           EQU  H'1D8C'
RA1PPS           EQU  H'1D8D'
RA2PPS           EQU  H'1D8E'
RA4PPS           EQU  H'1D90'
RA5PPS           EQU  H'1D91'
RC0PPS           EQU  H'1D9C'
RC1PPS           EQU  H'1D9D'
RC2PPS           EQU  H'1D9E'
RC3PPS           EQU  H'1D9F'
RC4PPS           EQU  H'1DA0'
RC5PPS           EQU  H'1DA1'

;-----Bank60------------------
PPSLOCK          EQU  H'1E0C'
INTPPS           EQU  H'1E0D'
T0CKIPPS         EQU  H'1E0E'
T1CKIPPS         EQU  H'1E0F'
T1GPPS           EQU  H'1E10'
T3CKIPPS         EQU  H'1E11'
T3GPPS           EQU  H'1E12'
T2INPPS          EQU  H'1E19'
T4INPPS          EQU  H'1E1A'
CCP1PPS          EQU  H'1E1E'
CCP2PPS          EQU  H'1E1F'
PWMIN0PPS        EQU  H'1E24'
PWMIN1PPS        EQU  H'1E25'
PWM1ERSPPS       EQU  H'1E26'
PWM2ERSPPS       EQU  H'1E27'
CWG1PPS          EQU  H'1E39'
CLCIN0PPS        EQU  H'1E3D'
CLCIN1PPS        EQU  H'1E3E'
CLCIN2PPS        EQU  H'1E3F'
CLCIN3PPS        EQU  H'1E40'
CK1PPS           EQU  H'1E41'
TX1CKPPS         EQU  H'1E41'
RX1PPS           EQU  H'1E42'
RX1DTPPS         EQU  H'1E42'

;-----Bank61------------------
CK2PPS           EQU  H'1E43'
TX2CKPPS         EQU  H'1E43'
RX2PPS           EQU  H'1E44'
RX2DTPPS         EQU  H'1E44'
SSP1CLKPPS       EQU  H'1E47'
SSP1DATPPS       EQU  H'1E48'
SSP1SSPPS        EQU  H'1E49'
SSP2CLKPPS       EQU  H'1E4A'
SSP2DATPPS       EQU  H'1E4B'
SSP2SSPPS        EQU  H'1E4C'
ADACTPPS         EQU  H'1E50'
OPA1PPS          EQU  H'1E53'
ANSELA           EQU  H'1E8C'
WPUA             EQU  H'1E8D'
ODCONA           EQU  H'1E8E'
SLRCONA          EQU  H'1E8F'
INLVLA           EQU  H'1E90'
IOCAP            EQU  H'1E91'
IOCAN            EQU  H'1E92'
IOCAF            EQU  H'1E93'
ANSELC           EQU  H'1EA0'
WPUC             EQU  H'1EA1'
ODCONC           EQU  H'1EA2'
SLRCONC          EQU  H'1EA3'
INLVLC           EQU  H'1EA4'
IOCCP            EQU  H'1EA5'
IOCCN            EQU  H'1EA6'
IOCCF            EQU  H'1EA7'

;-----Bank62------------------
RC0I2C           EQU  H'1EE9'
RC1I2C           EQU  H'1EEA'
RC4I2C           EQU  H'1EEC'
RC5I2C           EQU  H'1EED'
UMTOAP           EQU  H'1F0C'
UMTOAL           EQU  H'1F0D'
UMTOAH           EQU  H'1F0E'

;-----Bank64------------------
STATUS_SHAD      EQU  H'1FE4'
WREG_SHAD        EQU  H'1FE5'
BSR_SHAD         EQU  H'1FE6'
PCLATH_SHAD      EQU  H'1FE7'
FSR0_SHAD        EQU  H'1FE8'
FSR0L_SHAD       EQU  H'1FE8'
FSR0H_SHAD       EQU  H'1FE9'
FSR1_SHAD        EQU  H'1FEA'
FSR1L_SHAD       EQU  H'1FEA'
FSR1H_SHAD       EQU  H'1FEB'
STKPTR           EQU  H'1FED'
TOSL             EQU  H'1FEE'
TOSH             EQU  H'1FEF'


;----- PIR1 Bits -----------------------------------------------------
NVMIF            EQU  H'0000'
CRCIF            EQU  H'0001'
SCANIF           EQU  H'0002'
ACTIF            EQU  H'0003'
CSWIF            EQU  H'0004'
OSFIF            EQU  H'0005'
TMR1IF           EQU  H'0006'
TMR1GIF          EQU  H'0007'


;----- PIE1 Bits -----------------------------------------------------
NVMIE            EQU  H'0000'
CRCIE            EQU  H'0001'
SCANIE           EQU  H'0002'
ACTIE            EQU  H'0003'
CSWIE            EQU  H'0004'
OSFIE            EQU  H'0005'
TMR1IE           EQU  H'0006'
TMR1GIE          EQU  H'0007'


;----- PMD1 Bits -----------------------------------------------------
TMR1MD           EQU  H'0000'
TMR3MD           EQU  H'0001'
TMR2MD           EQU  H'0002'
TMR4MD           EQU  H'0003'
CCP1MD           EQU  H'0005'
CCP2MD           EQU  H'0006'
PWM1MD           EQU  H'0007'


;----- OSCCON1 Bits -----------------------------------------------------
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'
NOSC2            EQU  H'0006'


;----- TMR1 Bits -----------------------------------------------------
TMR1L0           EQU  H'0000'
TMR1L1           EQU  H'0001'
TMR1L2           EQU  H'0002'
TMR1L3           EQU  H'0003'
TMR1L4           EQU  H'0004'
TMR1L5           EQU  H'0005'
TMR1L6           EQU  H'0006'
TMR1L7           EQU  H'0007'
TMR10_TMR1       EQU  H'0000'
TMR11_TMR1       EQU  H'0001'
TMR12_TMR1       EQU  H'0002'
TMR13_TMR1       EQU  H'0003'
TMR14_TMR1       EQU  H'0004'
TMR15_TMR1       EQU  H'0005'
TMR16_TMR1       EQU  H'0006'
TMR17_TMR1       EQU  H'0007'
CAL01            EQU  H'0000'
CAL11            EQU  H'0001'
CAL21            EQU  H'0002'
CAL31            EQU  H'0003'
CAL41            EQU  H'0004'
CAL51            EQU  H'0005'
CAL61            EQU  H'0006'
CAL71            EQU  H'0007'


;----- TMR1H Bits -----------------------------------------------------
TMR1H0           EQU  H'0000'
TMR1H1           EQU  H'0001'
TMR1H2           EQU  H'0002'
TMR1H3           EQU  H'0003'
TMR1H4           EQU  H'0004'
TMR1H5           EQU  H'0005'
TMR1H6           EQU  H'0006'
TMR1H7           EQU  H'0007'
TMR18_TMR1H      EQU  H'0000'
TMR19_TMR1H      EQU  H'0001'
TMR110           EQU  H'0002'
TMR111           EQU  H'0003'
TMR112           EQU  H'0004'
TMR113           EQU  H'0005'
TMR114           EQU  H'0006'
TMR115           EQU  H'0007'


;----- T1CON Bits -----------------------------------------------------
ON_T1CON         EQU  H'0000'
RD16             EQU  H'0001'
NSYNC            EQU  H'0002'
TMR1ON           EQU  H'0000'
T1RD16           EQU  H'0001'
NT1SYNC          EQU  H'0002'
T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'
CKPS0_T1CON      EQU  H'0004'
CKPS1_T1CON      EQU  H'0005'
RD161            EQU  H'0001'


;----- T1GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO_NDONE        EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL             EQU  H'0006'
GE               EQU  H'0007'
T1GVAL           EQU  H'0002'
T1GGO_NDONE      EQU  H'0003'
T1GSPM           EQU  H'0004'
T1GTM            EQU  H'0005'
T1GPOL           EQU  H'0006'
T1GE             EQU  H'0007'
T1GGO            EQU  H'0003'


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
P1M1             EQU  H'0007'


;----- CCP1CAP Bits -----------------------------------------------------
CTS0             EQU  H'0000'
CTS1             EQU  H'0001'
CTS2             EQU  H'0002'
CTS3             EQU  H'0003'
CCP1CTS0         EQU  H'0000'
CCP1CTS1         EQU  H'0001'
CCP1CTS2         EQU  H'0002'
CCP1CTS3         EQU  H'0003'


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


;----- PWM1GIR Bits -----------------------------------------------------
S1P1             EQU  H'0000'
S1P2             EQU  H'0001'
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM1GIE Bits -----------------------------------------------------
S1P1             EQU  H'0000'
S1P2             EQU  H'0001'
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM1CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD               EQU  H'0002'
EN               EQU  H'0007'


;----- PWM1S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- CLCNSEL1 Bits -----------------------------------------------------
D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'
D2S6             EQU  H'0006'


;----- CLCNGLS1 Bits -----------------------------------------------------
G2D1N            EQU  H'0000'
G2D1T            EQU  H'0001'
G2D2N            EQU  H'0002'
G2D2T            EQU  H'0003'
G2D3N            EQU  H'0004'
G2D3T            EQU  H'0005'
G2D4N            EQU  H'0006'
G2D4T            EQU  H'0007'


;----- RC1STA Bits -----------------------------------------------------
RX9D             EQU  H'0000'
OERR             EQU  H'0001'
FERR             EQU  H'0002'
ADDEN            EQU  H'0003'
CREN             EQU  H'0004'
SREN             EQU  H'0005'
RX9              EQU  H'0006'
SPEN             EQU  H'0007'


;----- TX1STA Bits -----------------------------------------------------
TX9D             EQU  H'0000'
TRMT             EQU  H'0001'
BRGH             EQU  H'0002'
SENDB            EQU  H'0003'
SYNC_TX1STA      EQU  H'0004'
TXEN             EQU  H'0005'
TX9              EQU  H'0006'
CSRC             EQU  H'0007'


;----- BAUD1CON Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- SSP1CON1 Bits -----------------------------------------------------
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'
SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'
SSPM01           EQU  H'0000'
SSPM11           EQU  H'0001'
SSPM21           EQU  H'0002'
SSPM31           EQU  H'0003'
CKP1             EQU  H'0004'
SSPEN1           EQU  H'0005'
SSPOV1           EQU  H'0006'
WCOL1            EQU  H'0007'


;----- SSP1ADD Bits -----------------------------------------------------
MSK0             EQU  H'0000'
MSK1             EQU  H'0001'
MSK2             EQU  H'0002'
MSK3             EQU  H'0003'
MSK4             EQU  H'0004'
MSK5             EQU  H'0005'
MSK6             EQU  H'0006'
MSK7             EQU  H'0007'
MSK01            EQU  H'0000'
MSK11            EQU  H'0001'
MSK21            EQU  H'0002'
MSK31            EQU  H'0003'
MSK41            EQU  H'0004'
MSK51            EQU  H'0005'
MSK61            EQU  H'0006'
MSK71            EQU  H'0007'


;----- SSP1MSK Bits -----------------------------------------------------
MSK0             EQU  H'0000'
MSK1             EQU  H'0001'
MSK2             EQU  H'0002'
MSK3             EQU  H'0003'
MSK4             EQU  H'0004'
MSK5             EQU  H'0005'
MSK6             EQU  H'0006'
MSK7             EQU  H'0007'


;----- SSP1STAT Bits -----------------------------------------------------
BF               EQU  H'0000'
UA               EQU  H'0001'
R_NW             EQU  H'0002'
S                EQU  H'0003'
P                EQU  H'0004'
D_NA             EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'
R_W              EQU  H'0002'
D_A              EQU  H'0005'
NW               EQU  H'0002'
NA               EQU  H'0005'
NWRITE           EQU  H'0002'
NADDRESS         EQU  H'0005'
READ_WRITE       EQU  H'0002'
DATA_ADDRESS     EQU  H'0005'
I2C_READ         EQU  H'0002'
I2C_START        EQU  H'0003'
I2C_STOP         EQU  H'0004'
I2C_DAT          EQU  H'0005'
BF1              EQU  H'0000'
UA1              EQU  H'0001'
R                EQU  H'0002'
START            EQU  H'0003'
STOP             EQU  H'0004'
D                EQU  H'0005'
CKE1             EQU  H'0006'
SMP1             EQU  H'0007'
RW               EQU  H'0002'
START1           EQU  H'0003'
STOP1            EQU  H'0004'
DA               EQU  H'0005'
RW1              EQU  H'0002'
I2C_START1       EQU  H'0003'
I2C_STOP2        EQU  H'0004'
DA1              EQU  H'0005'
I2C_READ1        EQU  H'0002'
S2               EQU  H'0003'
P2               EQU  H'0004'
DATA_ADDRESS1    EQU  H'0005'
READ_WRITE1      EQU  H'0002'
D_A1             EQU  H'0005'
R_W1             EQU  H'0002'
D_NA1            EQU  H'0005'
R_NW1            EQU  H'0002'
I2C_DAT1         EQU  H'0005'
NW2              EQU  H'0002'
NA2              EQU  H'0005'
NWRITE1          EQU  H'0002'
NADDRESS1        EQU  H'0005'


;----- SSP1CON2 Bits -----------------------------------------------------
SEN              EQU  H'0000'
RSEN             EQU  H'0001'
PEN              EQU  H'0002'
RCEN             EQU  H'0003'
ACKEN            EQU  H'0004'
ACKDT            EQU  H'0005'
ACKSTAT          EQU  H'0006'
GCEN             EQU  H'0007'
ADMSK1           EQU  H'0001'
ADMSK2           EQU  H'0002'
ADMSK3           EQU  H'0003'
ADMSK4           EQU  H'0004'
ADMSK5           EQU  H'0005'
SEN1             EQU  H'0000'
ADMSK11          EQU  H'0001'
ADMSK21          EQU  H'0002'
ADMSK31          EQU  H'0003'
ACKEN1           EQU  H'0004'
ACKDT1           EQU  H'0005'
ACKSTAT1         EQU  H'0006'
GCEN1            EQU  H'0007'
RSEN1            EQU  H'0001'
PEN1             EQU  H'0002'
RCEN1            EQU  H'0003'
ADMSK41          EQU  H'0004'
ADMSK51          EQU  H'0005'


;----- SSP1CON3 Bits -----------------------------------------------------
DHEN             EQU  H'0000'
AHEN             EQU  H'0001'
SBCDE            EQU  H'0002'
SDAHT            EQU  H'0003'
BOEN             EQU  H'0004'
SCIE             EQU  H'0005'
PCIE             EQU  H'0006'
ACKTIM           EQU  H'0007'


;----- CM1CON0 Bits -----------------------------------------------------
SYNC_CM1CON0     EQU  H'0000'
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
NCH0             EQU  H'0000'
NCH1             EQU  H'0001'
NCH2             EQU  H'0002'
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


;----- DAC1CON Bits -----------------------------------------------------
NSS              EQU  H'0000'
REFRNG           EQU  H'0006'
EN               EQU  H'0007'
PSS0             EQU  H'0002'
PSS1             EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'


;----- OPA1CON1 Bits -----------------------------------------------------
RESON            EQU  H'0003'
NSS0             EQU  H'0000'
NSS1             EQU  H'0001'
NSS2             EQU  H'0002'
GSEL0            EQU  H'0004'
GSEL1            EQU  H'0005'
GSEL2            EQU  H'0006'
OPA1RESON        EQU  H'0003'
OPA1NSS0         EQU  H'0000'
OPA1NSS1         EQU  H'0001'
OPA1NSS2         EQU  H'0002'
OPA1GSEL0        EQU  H'0004'
OPA1GSEL1        EQU  H'0005'
OPA1GSEL2        EQU  H'0006'


;----- CRCCON1 Bits -----------------------------------------------------
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
PLEN4            EQU  H'0004'


;----- ADCON1 Bits -----------------------------------------------------
DSEN             EQU  H'0000'
PCSC             EQU  H'0001'
GPOL             EQU  H'0005'
IPEN             EQU  H'0006'
PPOL             EQU  H'0007'
ADDSEN           EQU  H'0000'
ADPCSC           EQU  H'0001'
ADGPOL           EQU  H'0005'
ADIPEN           EQU  H'0006'
ADPPOL           EQU  H'0007'


;----- CCP1PPS Bits -----------------------------------------------------
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'
CCP1PPS5         EQU  H'0005'


;----- CLCIN1PPS Bits -----------------------------------------------------
CLCIN1PPS0       EQU  H'0000'
CLCIN1PPS1       EQU  H'0001'
CLCIN1PPS2       EQU  H'0002'
CLCIN1PPS3       EQU  H'0003'
CLCIN1PPS4       EQU  H'0004'
CLCIN1PPS5       EQU  H'0005'


;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
NPD              EQU  H'0003'
NTO              EQU  H'0004'
CARRY            EQU  H'0000'
ZERO             EQU  H'0002'


;----- BSR Bits -----------------------------------------------------
BSR0             EQU  H'0000'
BSR1             EQU  H'0001'
BSR2             EQU  H'0002'
BSR3             EQU  H'0003'
BSR4             EQU  H'0004'
BSR5             EQU  H'0005'


;----- INTCON Bits -----------------------------------------------------
INTEDG           EQU  H'0000'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- PORTA Bits -----------------------------------------------------
RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'
RA4              EQU  H'0004'
RA5              EQU  H'0005'


;----- PORTC Bits -----------------------------------------------------
RC0              EQU  H'0000'
RC1              EQU  H'0001'
RC2              EQU  H'0002'
RC3              EQU  H'0003'
RC4              EQU  H'0004'
RC5              EQU  H'0005'


;----- TRISA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'


;----- TRISC Bits -----------------------------------------------------
TRISC0           EQU  H'0000'
TRISC1           EQU  H'0001'
TRISC2           EQU  H'0002'
TRISC3           EQU  H'0003'
TRISC4           EQU  H'0004'
TRISC5           EQU  H'0005'


;----- LATA Bits -----------------------------------------------------
LATA0            EQU  H'0000'
LATA1            EQU  H'0001'
LATA2            EQU  H'0002'
LATA4            EQU  H'0004'
LATA5            EQU  H'0005'


;----- LATC Bits -----------------------------------------------------
LATC0            EQU  H'0000'
LATC1            EQU  H'0001'
LATC2            EQU  H'0002'
LATC3            EQU  H'0003'
LATC4            EQU  H'0004'
LATC5            EQU  H'0005'


;----- PIR0 Bits -----------------------------------------------------
INTF             EQU  H'0000'
IOCIF            EQU  H'0004'
TMR0IF           EQU  H'0005'


;----- PIR2 Bits -----------------------------------------------------
TMR3IF           EQU  H'0000'
TMR3GIF          EQU  H'0001'
TMR2IF           EQU  H'0002'
TMR4IF           EQU  H'0003'
CCP1IF           EQU  H'0005'
CCP2IF           EQU  H'0006'


;----- PIR3 Bits -----------------------------------------------------
PWM1PIF          EQU  H'0000'
PWM1IF           EQU  H'0001'
PWM2PIF          EQU  H'0002'
PWM2IF           EQU  H'0003'


;----- PIR4 Bits -----------------------------------------------------
NCO1IF           EQU  H'0000'
CWG1IF           EQU  H'0001'
CLC1IF           EQU  H'0002'
CLC2IF           EQU  H'0003'
CLC3IF           EQU  H'0004'
CLC4IF           EQU  H'0005'
TX1IF            EQU  H'0006'
RC1IF            EQU  H'0007'


;----- PIR5 Bits -----------------------------------------------------
TX2IF            EQU  H'0000'
RC2IF            EQU  H'0001'
SSP1IF           EQU  H'0002'
BCL1IF           EQU  H'0003'
SSP2IF           EQU  H'0004'
BCL2IF           EQU  H'0005'
CM1IF            EQU  H'0006'
CM2IF            EQU  H'0007'


;----- PIR6 Bits -----------------------------------------------------
ADIF             EQU  H'0000'
ADTIF            EQU  H'0001'
ZCDIF            EQU  H'0002'


;----- PIE0 Bits -----------------------------------------------------
INTE             EQU  H'0000'
IOCIE            EQU  H'0004'
TMR0IE           EQU  H'0005'


;----- PIE2 Bits -----------------------------------------------------
TMR3IE           EQU  H'0000'
TMR3GIE          EQU  H'0001'
TMR2IE           EQU  H'0002'
TMR4IE           EQU  H'0003'
CCP1IE           EQU  H'0005'
CCP2IE           EQU  H'0006'


;----- PIE3 Bits -----------------------------------------------------
PWM1PIE          EQU  H'0000'
PWM1IE           EQU  H'0001'
PWM2PIE          EQU  H'0002'
PWM2IE           EQU  H'0003'


;----- PIE4 Bits -----------------------------------------------------
NCO1IE           EQU  H'0000'
CWG1IE           EQU  H'0001'
CLC1IE           EQU  H'0002'
CLC2IE           EQU  H'0003'
CLC3IE           EQU  H'0004'
CLC4IE           EQU  H'0005'
TX1IE            EQU  H'0006'
RC1IE            EQU  H'0007'


;----- PIE5 Bits -----------------------------------------------------
TX2IE            EQU  H'0000'
RC2IE            EQU  H'0001'
SSP1IE           EQU  H'0002'
BCL1IE           EQU  H'0003'
SSP2IE           EQU  H'0004'
BCL2IE           EQU  H'0005'
CM1IE            EQU  H'0006'
CM2IE            EQU  H'0007'


;----- PIE6 Bits -----------------------------------------------------
ADIE             EQU  H'0000'
ADTIE            EQU  H'0001'
ZCDIE            EQU  H'0002'


;----- PMD0 Bits -----------------------------------------------------
NVMMD            EQU  H'0000'
CRCMD            EQU  H'0001'
SCANMD           EQU  H'0002'
SYSCMD           EQU  H'0003'
ACTMD            EQU  H'0004'
IOCMD            EQU  H'0005'
CLKRMD           EQU  H'0006'
TMR0MD           EQU  H'0007'


;----- PMD2 Bits -----------------------------------------------------
PWM2MD           EQU  H'0000'
NCO1MD           EQU  H'0003'
CWG1MD           EQU  H'0004'
CLC1MD           EQU  H'0005'
CLC2MD           EQU  H'0006'
CLC3MD           EQU  H'0007'


;----- PMD3 Bits -----------------------------------------------------
CLC4MD           EQU  H'0000'
UART1MD          EQU  H'0001'
UART2MD          EQU  H'0002'
MSSP1MD          EQU  H'0003'
MSSP2MD          EQU  H'0004'
FVRMD            EQU  H'0005'
CM1MD            EQU  H'0006'
CM2MD            EQU  H'0007'


;----- PMD4 Bits -----------------------------------------------------
ADCMD            EQU  H'0000'
DAC1MD           EQU  H'0001'
DAC2MD           EQU  H'0002'
OPA1MD           EQU  H'0003'
ZCDMD            EQU  H'0004'


;----- WDTCON0 Bits -----------------------------------------------------
SEN              EQU  H'0000'
SWDTEN           EQU  H'0000'
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
WDTPSCNT16       EQU  H'0000'
WDTPSCNT17       EQU  H'0001'
WDTSTATE         EQU  H'0002'
WDTTMR0          EQU  H'0003'
WDTTMR1          EQU  H'0004'
WDTTMR2          EQU  H'0005'
WDTTMR3          EQU  H'0006'
WDTTMR4          EQU  H'0007'


;----- BORCON Bits -----------------------------------------------------
BORRDY           EQU  H'0000'
SBOREN           EQU  H'0007'


;----- PCON0 Bits -----------------------------------------------------
NBOR             EQU  H'0000'
NPOR             EQU  H'0001'
NRI              EQU  H'0002'
NRMCLR           EQU  H'0003'
NRWDT            EQU  H'0004'
NWDTWV           EQU  H'0005'
STKUNF           EQU  H'0006'
STKOVF           EQU  H'0007'


;----- PCON1 Bits -----------------------------------------------------
NMEMV            EQU  H'0001'


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


;----- FVRCON Bits -----------------------------------------------------
TSRNG            EQU  H'0004'
TSEN             EQU  H'0005'
RDY              EQU  H'0006'
EN               EQU  H'0007'
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
CDAFVR0          EQU  H'0002'
CDAFVR1          EQU  H'0003'
FVRRDY           EQU  H'0006'
FVREN            EQU  H'0007'


;----- CPCON Bits -----------------------------------------------------
CPRDY            EQU  H'0000'
CPT              EQU  H'0001'
CPREQ            EQU  H'0002'
CPOS             EQU  H'0005'


;----- ZCDCON Bits -----------------------------------------------------
INTN_ZCDCON      EQU  H'0000'
INTP_ZCDCON      EQU  H'0001'
POL_ZCDCON       EQU  H'0004'
OUT_ZCDCON       EQU  H'0005'
SEN              EQU  H'0007'
ZCDINTN          EQU  H'0000'
ZCDINTP          EQU  H'0001'
ZCDPOL           EQU  H'0004'
ZCDOUT           EQU  H'0005'
ZCDSEN           EQU  H'0007'


;----- CPUDOZE Bits -----------------------------------------------------
DOE              EQU  H'0004'
ROI              EQU  H'0005'
DOZEN            EQU  H'0006'
IDLEN            EQU  H'0007'
DOZE0            EQU  H'0000'
DOZE1            EQU  H'0001'
DOZE2            EQU  H'0002'


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


;----- OSCFRQ Bits -----------------------------------------------------
FRQ0             EQU  H'0000'
FRQ1             EQU  H'0001'
FRQ2             EQU  H'0002'


;----- ACTCON Bits -----------------------------------------------------
ACTORS           EQU  H'0001'
ACTLOCK          EQU  H'0003'
ACTUD            EQU  H'0006'
ACTEN            EQU  H'0007'


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
CLK0             EQU  H'0000'
CLK1             EQU  H'0001'
CLK2             EQU  H'0002'
CLK3             EQU  H'0003'


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
CAL03            EQU  H'0000'
CAL13            EQU  H'0001'
CAL23            EQU  H'0002'
CAL33            EQU  H'0003'
CAL43            EQU  H'0004'
CAL53            EQU  H'0005'
CAL63            EQU  H'0006'
CAL73            EQU  H'0007'


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
NSYNC            EQU  H'0002'
TMR3ON           EQU  H'0000'
T3RD16           EQU  H'0001'
NT3SYNC          EQU  H'0002'
T3CKPS0          EQU  H'0004'
T3CKPS1          EQU  H'0005'
CKPS0_T3CON      EQU  H'0004'
CKPS1_T3CON      EQU  H'0005'
RD163            EQU  H'0001'


;----- T3GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO_NDONE        EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL             EQU  H'0006'
GE               EQU  H'0007'
T3GVAL           EQU  H'0002'
T3GGO_NDONE      EQU  H'0003'
T3GSPM           EQU  H'0004'
T3GTM            EQU  H'0005'
T3GPOL           EQU  H'0006'
T3GE             EQU  H'0007'
T3GGO            EQU  H'0003'


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
CSYNC            EQU  H'0005'
CPOL             EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
T2CSYNC          EQU  H'0005'
T2CPOL           EQU  H'0006'
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
CSYNC            EQU  H'0005'
CPOL             EQU  H'0006'
PSYNC            EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
MODE4            EQU  H'0004'
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
T4CSYNC          EQU  H'0005'
T4CPOL           EQU  H'0006'
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
P2M1             EQU  H'0007'


;----- CCP2CAP Bits -----------------------------------------------------
CTS0             EQU  H'0000'
CTS1             EQU  H'0001'
CTS2             EQU  H'0002'
CTS3             EQU  H'0003'
CCP2CTS0         EQU  H'0000'
CCP2CTS1         EQU  H'0001'
CCP2CTS2         EQU  H'0002'
CCP2CTS3         EQU  H'0003'


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


;----- PWM2GIR Bits -----------------------------------------------------
S1P1             EQU  H'0000'
S1P2             EQU  H'0001'
S1P1IF           EQU  H'0000'
S1P2IF           EQU  H'0001'


;----- PWM2GIE Bits -----------------------------------------------------
S1P1             EQU  H'0000'
S1P2             EQU  H'0001'
S1P1IE           EQU  H'0000'
S1P2IE           EQU  H'0001'


;----- PWM2CON Bits -----------------------------------------------------
ERSNOW           EQU  H'0000'
ERSPOL           EQU  H'0001'
LD               EQU  H'0002'
EN               EQU  H'0007'


;----- PWM2S1CFG Bits -----------------------------------------------------
PPEN             EQU  H'0003'
POL1             EQU  H'0006'
POL2             EQU  H'0007'
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


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
ACC16            EQU  H'0000'
ACC17            EQU  H'0001'
ACC18            EQU  H'0002'
ACC19            EQU  H'0003'


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
INC16            EQU  H'0000'
INC17            EQU  H'0001'
INC18            EQU  H'0002'
INC19            EQU  H'0003'


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


;----- CWG1CLK Bits -----------------------------------------------------
CS               EQU  H'0000'
CWG1CS           EQU  H'0000'


;----- CWG1ISM Bits -----------------------------------------------------
ISM0             EQU  H'0000'
ISM1             EQU  H'0001'
ISM2             EQU  H'0002'
ISM3             EQU  H'0003'
ISM4             EQU  H'0004'
DAT0             EQU  H'0000'
DAT1             EQU  H'0001'
DAT2             EQU  H'0002'
DAT3             EQU  H'0003'
DAT4             EQU  H'0004'


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
LD               EQU  H'0006'
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
LSCA1            EQU  H'0003'
LSBD0            EQU  H'0004'
LSBD1            EQU  H'0005'
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


;----- CLCNCON Bits -----------------------------------------------------
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
D1S6             EQU  H'0006'


;----- CLCNSEL2 Bits -----------------------------------------------------
D3S0             EQU  H'0000'
D3S1             EQU  H'0001'
D3S2             EQU  H'0002'
D3S3             EQU  H'0003'
D3S4             EQU  H'0004'
D3S5             EQU  H'0005'
D3S6             EQU  H'0006'


;----- CLCNSEL3 Bits -----------------------------------------------------
D4S0             EQU  H'0000'
D4S1             EQU  H'0001'
D4S2             EQU  H'0002'
D4S3             EQU  H'0003'
D4S4             EQU  H'0004'
D4S5             EQU  H'0005'
D4S6             EQU  H'0006'


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


;----- CLCSELECT Bits -----------------------------------------------------
SLCT0            EQU  H'0000'
SLCT1            EQU  H'0001'
SLCT2            EQU  H'0002'
SLCT3            EQU  H'0003'


;----- CLCDATA Bits -----------------------------------------------------
CLC1OUT          EQU  H'0000'
CLC2OUT          EQU  H'0001'
CLC3OUT          EQU  H'0002'
CLC4OUT          EQU  H'0003'


;----- RC2STA Bits -----------------------------------------------------
RX9D             EQU  H'0000'
OERR             EQU  H'0001'
FERR             EQU  H'0002'
ADDEN            EQU  H'0003'
CREN             EQU  H'0004'
SREN             EQU  H'0005'
RX9              EQU  H'0006'
SPEN             EQU  H'0007'


;----- TX2STA Bits -----------------------------------------------------
TX9D             EQU  H'0000'
TRMT             EQU  H'0001'
BRGH             EQU  H'0002'
SENDB            EQU  H'0003'
SYNC_TX2STA      EQU  H'0004'
TXEN             EQU  H'0005'
TX9              EQU  H'0006'
CSRC             EQU  H'0007'


;----- BAUD2CON Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- SSP2ADD Bits -----------------------------------------------------
MSK0             EQU  H'0000'
MSK1             EQU  H'0001'
MSK2             EQU  H'0002'
MSK3             EQU  H'0003'
MSK4             EQU  H'0004'
MSK5             EQU  H'0005'
MSK6             EQU  H'0006'
MSK7             EQU  H'0007'
MSK02            EQU  H'0000'
MSK12            EQU  H'0001'
MSK22            EQU  H'0002'
MSK32            EQU  H'0003'
MSK42            EQU  H'0004'
MSK52            EQU  H'0005'
MSK62            EQU  H'0006'
MSK72            EQU  H'0007'


;----- SSP2MSK Bits -----------------------------------------------------
MSK0             EQU  H'0000'
MSK1             EQU  H'0001'
MSK2             EQU  H'0002'
MSK3             EQU  H'0003'
MSK4             EQU  H'0004'
MSK5             EQU  H'0005'
MSK6             EQU  H'0006'
MSK7             EQU  H'0007'


;----- SSP2STAT Bits -----------------------------------------------------
BF               EQU  H'0000'
UA               EQU  H'0001'
R_NW             EQU  H'0002'
S                EQU  H'0003'
P                EQU  H'0004'
D_NA             EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'
R_W              EQU  H'0002'
D_A              EQU  H'0005'
NW               EQU  H'0002'
NA               EQU  H'0005'
NWRITE           EQU  H'0002'
NADDRESS         EQU  H'0005'
READ_WRITE       EQU  H'0002'
DATA_ADDRESS     EQU  H'0005'
I2C_READ         EQU  H'0002'
I2C_START        EQU  H'0003'
I2C_STOP         EQU  H'0004'
I2C_DAT          EQU  H'0005'
BF2              EQU  H'0000'
UA2              EQU  H'0001'
R                EQU  H'0002'
START            EQU  H'0003'
STOP             EQU  H'0004'
D                EQU  H'0005'
CKE2             EQU  H'0006'
SMP2             EQU  H'0007'
RW               EQU  H'0002'
START2           EQU  H'0003'
STOP2            EQU  H'0004'
DA               EQU  H'0005'
RW2              EQU  H'0002'
I2C_START2       EQU  H'0003'
I2C_STOP2        EQU  H'0004'
DA2              EQU  H'0005'
I2C_READ2        EQU  H'0002'
S2               EQU  H'0003'
P2               EQU  H'0004'
DATA_ADDRESS2    EQU  H'0005'
READ_WRITE2      EQU  H'0002'
D_A2             EQU  H'0005'
R_W2             EQU  H'0002'
D_NA2            EQU  H'0005'
R_NW2            EQU  H'0002'
I2C_DAT2         EQU  H'0005'
NW2              EQU  H'0002'
NA2              EQU  H'0005'
NWRITE2          EQU  H'0002'
NADDRESS2        EQU  H'0005'


;----- SSP2CON1 Bits -----------------------------------------------------
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'
SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'
SSPM02           EQU  H'0000'
SSPM12           EQU  H'0001'
SSPM22           EQU  H'0002'
SSPM32           EQU  H'0003'
CKP2             EQU  H'0004'
SSPEN2           EQU  H'0005'
SSPOV2           EQU  H'0006'
WCOL2            EQU  H'0007'


;----- SSP2CON2 Bits -----------------------------------------------------
SEN              EQU  H'0000'
RSEN             EQU  H'0001'
PEN              EQU  H'0002'
RCEN             EQU  H'0003'
ACKEN            EQU  H'0004'
ACKDT            EQU  H'0005'
ACKSTAT          EQU  H'0006'
GCEN             EQU  H'0007'
ADMSK1           EQU  H'0001'
ADMSK2           EQU  H'0002'
ADMSK3           EQU  H'0003'
ADMSK4           EQU  H'0004'
ADMSK5           EQU  H'0005'
SEN2             EQU  H'0000'
ADMSK12          EQU  H'0001'
ADMSK22          EQU  H'0002'
ADMSK32          EQU  H'0003'
ACKEN2           EQU  H'0004'
ACKDT2           EQU  H'0005'
ACKSTAT2         EQU  H'0006'
GCEN2            EQU  H'0007'
RSEN2            EQU  H'0001'
PEN2             EQU  H'0002'
RCEN2            EQU  H'0003'
ADMSK42          EQU  H'0004'
ADMSK52          EQU  H'0005'


;----- SSP2CON3 Bits -----------------------------------------------------
DHEN             EQU  H'0000'
AHEN             EQU  H'0001'
SBCDE            EQU  H'0002'
SDAHT            EQU  H'0003'
BOEN             EQU  H'0004'
SCIE             EQU  H'0005'
PCIE             EQU  H'0006'
ACKTIM           EQU  H'0007'


;----- CM2CON0 Bits -----------------------------------------------------
SYNC_CM2CON0     EQU  H'0000'
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
NCH0             EQU  H'0000'
NCH1             EQU  H'0001'
NCH2             EQU  H'0002'
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


;----- CMOUT Bits -----------------------------------------------------
C1OUT_CMOUT      EQU  H'0000'
C2OUT_CMOUT      EQU  H'0001'


;----- DAC2CON Bits -----------------------------------------------------
NSS              EQU  H'0000'
REFRNG           EQU  H'0006'
EN               EQU  H'0007'
PSS0             EQU  H'0002'
PSS1             EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'


;----- OPA1CON0 Bits -----------------------------------------------------
UG               EQU  H'0003'
CPON             EQU  H'0005'
EN               EQU  H'0007'
SOC0             EQU  H'0000'
SOC1             EQU  H'0001'
SOC2             EQU  H'0002'
UG0              EQU  H'0003'
OPA1UG           EQU  H'0003'
OPA1CPON         EQU  H'0005'
OPA1EN           EQU  H'0007'
OPA1SOC0         EQU  H'0000'
OPA1SOC1         EQU  H'0001'
OPA1SOC2         EQU  H'0002'


;----- OPA1CON2 Bits -----------------------------------------------------
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
NCH0             EQU  H'0004'
NCH1             EQU  H'0005'
NCH2             EQU  H'0006'
OPA1PCH0         EQU  H'0000'
OPA1PCH1         EQU  H'0001'
OPA1PCH2         EQU  H'0002'
OPA1NCH0         EQU  H'0004'
OPA1NCH1         EQU  H'0005'
OPA1NCH2         EQU  H'0006'


;----- OPA1CON3 Bits -----------------------------------------------------
PSS0             EQU  H'0000'
PSS1             EQU  H'0001'
FMS0             EQU  H'0006'
FMS1             EQU  H'0007'
OPA1PSS0         EQU  H'0000'
OPA1PSS1         EQU  H'0001'
OPA1FMS0         EQU  H'0006'
OPA1FMS1         EQU  H'0007'


;----- OPA1HWC Bits -----------------------------------------------------
ORPOL            EQU  H'0003'
OREN             EQU  H'0007'
HWCL0            EQU  H'0000'
HWCL1            EQU  H'0001'
HWCL2            EQU  H'0002'
HWCH0            EQU  H'0004'
HWCH1            EQU  H'0005'
HWCH2            EQU  H'0006'
OPA1ORPOL        EQU  H'0003'
OPA1OREN         EQU  H'0007'
OPA1HWCL0        EQU  H'0000'
OPA1HWCL1        EQU  H'0001'
OPA1HWCL2        EQU  H'0002'
OPA1HWCH0        EQU  H'0004'
OPA1HWCH1        EQU  H'0005'
OPA1HWCH2        EQU  H'0006'


;----- OPA1OFFSET Bits -----------------------------------------------------
OFFSET0          EQU  H'0000'
OFFSET1          EQU  H'0001'
OFFSET2          EQU  H'0002'
OFFSET3          EQU  H'0003'
OFFSET4          EQU  H'0004'
OFFSET5          EQU  H'0005'
OFFSET6          EQU  H'0006'
OFFSET7          EQU  H'0007'
OPA1OFFSET0      EQU  H'0000'
OPA1OFFSET1      EQU  H'0001'
OPA1OFFSET2      EQU  H'0002'
OPA1OFFSET3      EQU  H'0003'
OPA1OFFSET4      EQU  H'0004'
OPA1OFFSET5      EQU  H'0005'
OPA1OFFSET6      EQU  H'0006'
OPA1OFFSET7      EQU  H'0007'


;----- OPA1ORS Bits -----------------------------------------------------
ORS0             EQU  H'0000'
ORS1             EQU  H'0001'
ORS2             EQU  H'0002'
ORS3             EQU  H'0003'
ORS4             EQU  H'0004'
OPA1ORS0         EQU  H'0000'
OPA1ORS1         EQU  H'0001'
OPA1ORS2         EQU  H'0002'
OPA1ORS3         EQU  H'0003'
OPA1ORS4         EQU  H'0004'


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


;----- NVMCON1 Bits -----------------------------------------------------
RD               EQU  H'0000'
WR               EQU  H'0001'
WREN             EQU  H'0002'
WRERR            EQU  H'0003'
FREE             EQU  H'0004'
LWLO             EQU  H'0005'
NVMREGS          EQU  H'0006'


;----- SCANCON0 Bits -----------------------------------------------------
INTM             EQU  H'0003'
DABORT           EQU  H'0004'
BUSY             EQU  H'0005'
SGO              EQU  H'0006'
EN               EQU  H'0007'


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
BUSY             EQU  H'0005'
GO_CRCCON0       EQU  H'0006'
EN               EQU  H'0007'


;----- CRCCON2 Bits -----------------------------------------------------
DLEN0            EQU  H'0000'
DLEN1            EQU  H'0001'
DLEN2            EQU  H'0002'
DLEN3            EQU  H'0003'
DLEN4            EQU  H'0004'


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
PCH0             EQU  H'0000'
PCH1             EQU  H'0001'
PCH2             EQU  H'0002'
PCH3             EQU  H'0003'
PCH4             EQU  H'0004'
PCH5             EQU  H'0005'


;----- ADNCH Bits -----------------------------------------------------
NCH0             EQU  H'0000'
NCH1             EQU  H'0001'
NCH2             EQU  H'0002'
NCH3             EQU  H'0003'
NCH4             EQU  H'0004'
NCH5             EQU  H'0005'


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
IC               EQU  H'0001'
CS               EQU  H'0004'
CONT             EQU  H'0006'
ON_ADCON0        EQU  H'0007'
ADGO             EQU  H'0000'
ADIC             EQU  H'0001'
ADCS             EQU  H'0004'
ADCONT           EQU  H'0006'
ADON             EQU  H'0007'
DONE             EQU  H'0000'
FM0              EQU  H'0002'
FM1              EQU  H'0003'
GO_NDONE         EQU  H'0000'
ADFM0            EQU  H'0002'
ADFM1            EQU  H'0003'


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
AOV              EQU  H'0007'
ADSTAT0          EQU  H'0000'
ADSTAT1          EQU  H'0001'
ADSTAT2          EQU  H'0002'
ADMATH           EQU  H'0004'
ADLTHR           EQU  H'0005'
ADUTHR           EQU  H'0006'
ADAOV            EQU  H'0007'
ADOV             EQU  H'0007'
STAT0            EQU  H'0000'
STAT1            EQU  H'0001'
STAT2            EQU  H'0002'


;----- ADREF Bits -----------------------------------------------------
PREF0            EQU  H'0000'
PREF1            EQU  H'0001'
ADPREF0          EQU  H'0000'
ADPREF1          EQU  H'0001'


;----- ADACT Bits -----------------------------------------------------
ADACT0           EQU  H'0000'
ADACT1           EQU  H'0001'
ADACT2           EQU  H'0002'
ADACT3           EQU  H'0003'
ADACT4           EQU  H'0004'
ACT0             EQU  H'0000'
ACT1             EQU  H'0001'
ACT2             EQU  H'0002'
ACT3             EQU  H'0003'
ACT4             EQU  H'0004'


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


;----- ADCG1C Bits -----------------------------------------------------
CGC0             EQU  H'0000'
CGC1             EQU  H'0001'
CGC2             EQU  H'0002'
CGC3             EQU  H'0003'
CGC4             EQU  H'0004'
CGC5             EQU  H'0005'


;----- PPSLOCK Bits -----------------------------------------------------
PPSLOCKED        EQU  H'0000'


;----- INTPPS Bits -----------------------------------------------------
INTPPS0          EQU  H'0000'
INTPPS1          EQU  H'0001'
INTPPS2          EQU  H'0002'
INTPPS3          EQU  H'0003'
INTPPS4          EQU  H'0004'
INTPPS5          EQU  H'0005'


;----- T0CKIPPS Bits -----------------------------------------------------
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'
T0CKIPPS4        EQU  H'0004'
T0CKIPPS5        EQU  H'0005'


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
T1GPPS5          EQU  H'0005'


;----- T2INPPS Bits -----------------------------------------------------
T2INPPS0         EQU  H'0000'
T2INPPS1         EQU  H'0001'
T2INPPS2         EQU  H'0002'
T2INPPS3         EQU  H'0003'
T2INPPS4         EQU  H'0004'
T2INPPS5         EQU  H'0005'


;----- CCP2PPS Bits -----------------------------------------------------
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'
CCP2PPS5         EQU  H'0005'


;----- CWG1PPS Bits -----------------------------------------------------
CWG1PPS0         EQU  H'0000'
CWG1PPS1         EQU  H'0001'
CWG1PPS2         EQU  H'0002'
CWG1PPS3         EQU  H'0003'
CWG1PPS4         EQU  H'0004'
CWG1PPS5         EQU  H'0005'


;----- CLCIN0PPS Bits -----------------------------------------------------
CLCIN0PPS0       EQU  H'0000'
CLCIN0PPS1       EQU  H'0001'
CLCIN0PPS2       EQU  H'0002'
CLCIN0PPS3       EQU  H'0003'
CLCIN0PPS4       EQU  H'0004'
CLCIN0PPS5       EQU  H'0005'


;----- CLCIN2PPS Bits -----------------------------------------------------
CLCIN2PPS0       EQU  H'0000'
CLCIN2PPS1       EQU  H'0001'
CLCIN2PPS2       EQU  H'0002'
CLCIN2PPS3       EQU  H'0003'
CLCIN2PPS4       EQU  H'0004'
CLCIN2PPS5       EQU  H'0005'


;----- CLCIN3PPS Bits -----------------------------------------------------
CLCIN3PPS0       EQU  H'0000'
CLCIN3PPS1       EQU  H'0001'
CLCIN3PPS2       EQU  H'0002'
CLCIN3PPS3       EQU  H'0003'
CLCIN3PPS4       EQU  H'0004'
CLCIN3PPS5       EQU  H'0005'


;----- SSP1CLKPPS Bits -----------------------------------------------------
SSP1CLKPPS0      EQU  H'0000'
SSP1CLKPPS1      EQU  H'0001'
SSP1CLKPPS2      EQU  H'0002'
SSP1CLKPPS3      EQU  H'0003'
SSP1CLKPPS4      EQU  H'0004'
SSP1CLKPPS5      EQU  H'0005'


;----- SSP1DATPPS Bits -----------------------------------------------------
SSP1DATPPS0      EQU  H'0000'
SSP1DATPPS1      EQU  H'0001'
SSP1DATPPS2      EQU  H'0002'
SSP1DATPPS3      EQU  H'0003'
SSP1DATPPS4      EQU  H'0004'
SSP1DATPPS5      EQU  H'0005'


;----- SSP1SSPPS Bits -----------------------------------------------------
SSP1SSPPS0       EQU  H'0000'
SSP1SSPPS1       EQU  H'0001'
SSP1SSPPS2       EQU  H'0002'
SSP1SSPPS3       EQU  H'0003'
SSP1SSPPS4       EQU  H'0004'
SSP1SSPPS5       EQU  H'0005'


;----- ANSELA Bits -----------------------------------------------------
ANSELA0          EQU  H'0000'
ANSELA1          EQU  H'0001'
ANSELA2          EQU  H'0002'
ANSELA4          EQU  H'0004'
ANSELA5          EQU  H'0005'
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA4            EQU  H'0004'
ANSA5            EQU  H'0005'


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


;----- ANSELC Bits -----------------------------------------------------
ANSELC0          EQU  H'0000'
ANSELC1          EQU  H'0001'
ANSELC2          EQU  H'0002'
ANSELC3          EQU  H'0003'
ANSELC4          EQU  H'0004'
ANSELC5          EQU  H'0005'
ANSC0            EQU  H'0000'
ANSC1            EQU  H'0001'
ANSC2            EQU  H'0002'
ANSC3            EQU  H'0003'
ANSC4            EQU  H'0004'
ANSC5            EQU  H'0005'


;----- WPUC Bits -----------------------------------------------------
WPUC0            EQU  H'0000'
WPUC1            EQU  H'0001'
WPUC2            EQU  H'0002'
WPUC3            EQU  H'0003'
WPUC4            EQU  H'0004'
WPUC5            EQU  H'0005'


;----- ODCONC Bits -----------------------------------------------------
ODCC0            EQU  H'0000'
ODCC1            EQU  H'0001'
ODCC2            EQU  H'0002'
ODCC3            EQU  H'0003'
ODCC4            EQU  H'0004'
ODCC5            EQU  H'0005'


;----- SLRCONC Bits -----------------------------------------------------
SLRC0            EQU  H'0000'
SLRC1            EQU  H'0001'
SLRC2            EQU  H'0002'
SLRC3            EQU  H'0003'
SLRC4            EQU  H'0004'
SLRC5            EQU  H'0005'


;----- INLVLC Bits -----------------------------------------------------
INLVLC0          EQU  H'0000'
INLVLC1          EQU  H'0001'
INLVLC2          EQU  H'0002'
INLVLC3          EQU  H'0003'
INLVLC4          EQU  H'0004'
INLVLC5          EQU  H'0005'


;----- IOCCP Bits -----------------------------------------------------
IOCCP0           EQU  H'0000'
IOCCP1           EQU  H'0001'
IOCCP2           EQU  H'0002'
IOCCP3           EQU  H'0003'
IOCCP4           EQU  H'0004'
IOCCP5           EQU  H'0005'


;----- IOCCN Bits -----------------------------------------------------
IOCCN0           EQU  H'0000'
IOCCN1           EQU  H'0001'
IOCCN2           EQU  H'0002'
IOCCN3           EQU  H'0003'
IOCCN4           EQU  H'0004'
IOCCN5           EQU  H'0005'


;----- IOCCF Bits -----------------------------------------------------
IOCCF0           EQU  H'0000'
IOCCF1           EQU  H'0001'
IOCCF2           EQU  H'0002'
IOCCF3           EQU  H'0003'
IOCCF4           EQU  H'0004'
IOCCF5           EQU  H'0005'


;----- RC0I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC1I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC4I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- RC5I2C Bits -----------------------------------------------------
TH0              EQU  H'0000'
TH1              EQU  H'0001'
PU0              EQU  H'0004'
PU1              EQU  H'0005'
SLEW0            EQU  H'0006'
SLEW1            EQU  H'0007'


;----- STATUS_SHAD Bits -----------------------------------------------------
STATUS_SHAD0     EQU  H'0000'
STATUS_SHAD1     EQU  H'0001'
STATUS_SHAD2     EQU  H'0002'


;----- STKPTR Bits -----------------------------------------------------
STKPTR0          EQU  H'0000'
STKPTR1          EQU  H'0001'
STKPTR2          EQU  H'0002'
STKPTR3          EQU  H'0003'
STKPTR4          EQU  H'0004'


;----- TOSL Bits -----------------------------------------------------
TOSL0            EQU  H'0000'
TOSL1            EQU  H'0001'
TOSL2            EQU  H'0002'
TOSL3            EQU  H'0003'
TOSL4            EQU  H'0004'
TOSL5            EQU  H'0005'
TOSL6            EQU  H'0006'
TOSL7            EQU  H'0007'


;----- TOSH Bits -----------------------------------------------------
TOSH0            EQU  H'0000'
TOSH1            EQU  H'0001'
TOSH2            EQU  H'0002'
TOSH3            EQU  H'0003'
TOSH4            EQU  H'0004'
TOSH5            EQU  H'0005'
TOSH6            EQU  H'0006'
TOSH7            EQU  H'0007'

;==========================================================================
;
;       RAM Definitions
;
;==========================================================================

       __MAXRAM  H'3FFF'
       __BADRAM  H'000E'
       __BADRAM  H'000F'-H'0011'
       __BADRAM  H'0013'
       __BADRAM  H'0015'-H'0017'
       __BADRAM  H'0019'
       __BADRAM  H'001B'-H'001F'
       __BADRAM  H'0093'-H'0095'
       __BADRAM  H'009E'-H'00A0'
       __BADRAM  H'0111'-H'011F'
       __BADRAM  H'0194'-H'019B'
       __BADRAM  H'020E'-H'021E'
       __BADRAM  H'0295'
       __BADRAM  H'0298'-H'029F'
       __BADRAM  H'0318'-H'031F'
       __BADRAM  H'0398'-H'039F'
       __BADRAM  H'0414'-H'041E'
       __BADRAM  H'049B'-H'049D'
       __BADRAM  H'051B'-H'051F'
       __BADRAM  H'0594'-H'059F'
       __BADRAM  H'0615'-H'061F'
       __BADRAM  H'0698'-H'069F'
       __BADRAM  H'0713'-H'0715'
       __BADRAM  H'071E'-H'0720'
       __BADRAM  H'0793'-H'0795'
       __BADRAM  H'079E'-H'07A0'
       __BADRAM  H'0814'-H'081E'
       __BADRAM  H'088E'-H'088F'
       __BADRAM  H'0892'-H'089F'
       __BADRAM  H'0913'-H'0920'
       __BADRAM  H'098C'-H'099F'
       __BADRAM  H'0A0C'-H'0A1F'
       __BADRAM  H'0A8C'-H'0A9F'
       __BADRAM  H'0B0C'-H'0B1F'
       __BADRAM  H'0B8C'-H'0B9F'
       __BADRAM  H'0C0C'-H'0C1F'
       __BADRAM  H'0C8C'-H'0C9F'
       __BADRAM  H'0E0C'-H'0E1F'
       __BADRAM  H'0E8C'-H'0E9F'
       __BADRAM  H'0E0C'-H'0E1F'
       __BADRAM  H'0E8C'-H'0E9F'
       __BADRAM  H'0F0C'-H'0F1F'
       __BADRAM  H'0F8C'-H'0F9F'
       __BADRAM  H'100C'-H'101F'
       __BADRAM  H'108C'-H'109F'
       __BADRAM  H'110C'-H'111F'
       __BADRAM  H'118C'-H'119F'
       __BADRAM  H'120C'-H'121F'
       __BADRAM  H'128C'-H'129F'
       __BADRAM  H'130C'-H'131F'
       __BADRAM  H'138C'-H'139F'
       __BADRAM  H'140C'-H'141F'
       __BADRAM  H'148C'-H'149F'
       __BADRAM  H'150C'-H'151F'
       __BADRAM  H'158C'-H'159F'
       __BADRAM  H'160C'-H'161F'
       __BADRAM  H'168C'-H'169F'
       __BADRAM  H'170C'-H'171F'
       __BADRAM  H'178C'-H'179F'
       __BADRAM  H'180C'-H'181F'
       __BADRAM  H'188C'-H'189F'
       __BADRAM  H'190C'-H'191F'
       __BADRAM  H'198C'-H'199F'
       __BADRAM  H'1A0C'-H'1A1F'
       __BADRAM  H'1A8C'-H'1A9F'
       __BADRAM  H'1B0C'-H'1B1F'
       __BADRAM  H'1B8C'-H'1B9F'
       __BADRAM  H'1C0C'-H'1C1F'
       __BADRAM  H'1C94'
       __BADRAM  H'1C97'
       __BADRAM  H'1E2F'
       __BADRAM  H'1E31'-H'1E6F'
       __BADRAM  H'1E8F'
       __BADRAM  H'1E92'-H'1E9B'
       __BADRAM  H'1EA2'-H'1EEF'
       __BADRAM  H'1E13'-H'1E18'
       __BADRAM  H'1E1B'-H'1E1D'
       __BADRAM  H'1E20'-H'1E23'
       __BADRAM  H'1E28'-H'1E38'
       __BADRAM  H'1E3A'-H'1E3C'
       __BADRAM  H'1E45'-H'1E46'
       __BADRAM  H'1E4E'-H'1E50'
       __BADRAM  H'1E51'-H'1E52'
       __BADRAM  H'1E54'-H'1E6F'
       __BADRAM  H'1E94'-H'1E9F'
       __BADRAM  H'1EA8'-H'1EE8'
       __BADRAM  H'1EEB'
       __BADRAM  H'1EEE'-H'1EEF'
       __BADRAM  H'1F0F'-H'1F6F'
       __BADRAM  H'1F8C'-H'1FE3'
       __BADRAM  H'1FEC'


;==========================================================================
; The following is an assignment of address values for all of the
; configuration registers for the purpose of table reads
_CONFIG1      EQU  H'8007'
_CONFIG2      EQU  H'8008'
_CONFIG3      EQU  H'8009'
_CONFIG4      EQU  H'800A'
_CONFIG5      EQU  H'800B'
;==========================================================================
             ; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\cfgdata\16F17126.cfgdata
;==========================================================================

;----- CONFIG1 Options --------------------------------------------------
_FEXTOSC_ECH         EQU  H'3FFF'; EC (external clock) above 16 MHz
_FEXTOSC_ECL         EQU  H'3FFD'; EC (external clock) below 16 MHz
_FEXTOSC_OFF         EQU  H'3FFC'; Oscillator not enabled
_FEXTOSC_HS          EQU  H'3FFA'; HS (crystal oscillator) above 4MHz
_FEXTOSC_XT          EQU  H'3FF9'; XT (crystal oscillator) above 100kHz, below 4MHz
_FEXTOSC_LP          EQU  H'3FF8'; LP (crystal oscillator) optimized for 32.768kHz
_RSTOSC_EXTOSC       EQU  H'3FFF'; EXTOSC operating per FEXTOSC bits
_RSTOSC_HFINTOSC_1MHz EQU  H'3FEF'; HFINTOSC (1MHz)
_RSTOSC_LFINTOSC     EQU  H'3FDF'; LFINTOSC
_RSTOSC_SOSC         EQU  H'3FCF'; SOSC
_RSTOSC_EXTOSC_4PLL EQU  H'3FAF'; EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
_RSTOSC_HFINTOSC_16MHz EQU  H'3F9F'; HFINTOSC (16 MHz)
_RSTOSC_HFINTOSC_32MHz EQU  H'3F8F'; HFINTOSC (32 MHz)
_CLKOUTEN_ON         EQU  H'3EFF'; CLKOUT function is enabled; FOSC/4 clock appears at OSC2
_CLKOUTEN_OFF        EQU  H'3FFF'; CLKOUT function is disabled; i/o or oscillator function on OSC2
_CSWEN_ON            EQU  H'3FFF'; Writing to NOSC and NDIV is allowed
_CSWEN_OFF           EQU  H'37FF'; The NOSC and NDIV bits cannot be changed by user software
_VDDAR_HI            EQU  H'3FFF'; Internal analog systems are calibrated for operation between VDD = 2.3 - 5.5V
_VDDAR_LO            EQU  H'2FFF'; Internal analog systems are calibrated for operation between VDD = 1.8 - 3.6V
_FCMEN_ON            EQU  H'3FFF'; Fail-Safe Clock Monitor enabled
_FCMEN_OFF           EQU  H'1FFF'; Fail-Safe Clock Monitor disabled

;----- CONFIG2 Options --------------------------------------------------
_MCLRE_EXTMCLR       EQU  H'3FFF'; If LVP = 0, MCLR pin is MCLR; If LVP = 1, RA3 pin function is MCLR
_MCLRE_INTMCLR       EQU  H'3FFE'; If LVP = 0, MCLR is port-defined function; If LVP = 1, RA3 pin function is MCLR
_PWRTS_PWRT_OFF      EQU  H'3FFF'; PWRT is disabled
_PWRTS_PWRT_64       EQU  H'3FFD'; PWRT set at 64 ms
_PWRTS_PWRT_16       EQU  H'3FFB'; PWRT set at 16 ms
_PWRTS_PWRT_1        EQU  H'3FF9'; PWRT set at 1 ms
_LPBOREN_OFF         EQU  H'3FFF'; ULPBOR disabled
_LPBOREN_ON          EQU  H'3FDF'; ULPBOR enabled
_BOREN_ON            EQU  H'3FFF'; Brown-out Reset enabled, SBOREN bit is ignored
_BOREN_NSLEEP        EQU  H'3FBF'; Brown-out Reset enabled while running, disabled in sleep; SBOREN is ignored
_BOREN_SBOREN        EQU  H'3F7F'; Brown-out reset enabled according to SBOREN bit
_BOREN_OFF           EQU  H'3F3F'; Brown-out reset disabled
_DACAUTOEN_OFF       EQU  H'3FFF'; DAC Buffer reference range is determined by the REFRNG bit
_DACAUTOEN_ON        EQU  H'3EFF'; DAC Buffer reference range is automatically determined by module hardware
_BORV_LO             EQU  H'3FFF'; Brown-out Reset Voltage (VBOR) set to 1.9V
_BORV_HI             EQU  H'3DFF'; Brown-out Reset Voltage (VBOR) is set to 2.85V
_ZCD_OFF             EQU  H'3FFF'; ZCD module is disabled; ZCD can be enabled by setting the ZCDSEN bit of ZCDCON
_ZCD_ON              EQU  H'3BFF'; ZCD module is always enabled; ZCDMD and ZCDSEN bits are ignored
_PPS1WAY_ON          EQU  H'3FFF'; The PPSLOCKED bit can be cleared and set only once after an unlocking sequence is executed; once PPSLOCKED is set, all future changes to PPS registers are prevented
_PPS1WAY_OFF         EQU  H'37FF'; The PPSLOCKED bit can be set and cleared as needed (unlocking sequence is required)
_STVREN_ON           EQU  H'3FFF'; Stack Overflow or Underflow will cause a reset
_STVREN_OFF          EQU  H'2FFF'; Stack Overflow or Underflow will not cause a reset

;----- CONFIG3 Options --------------------------------------------------
_WDTCPS_WDTCPS_0     EQU  H'3FE0'; Divider ratio 1:32
_WDTCPS_WDTCPS_1     EQU  H'3FE1'; Divider ratio 1:64
_WDTCPS_WDTCPS_2     EQU  H'3FE2'; Divider ratio 1:128
_WDTCPS_WDTCPS_3     EQU  H'3FE3'; Divider ratio 1:256
_WDTCPS_WDTCPS_4     EQU  H'3FE4'; Divider ratio 1:512
_WDTCPS_WDTCPS_5     EQU  H'3FE5'; Divider ratio 1:1024
_WDTCPS_WDTCPS_6     EQU  H'3FE6'; Divider ratio 1:2048
_WDTCPS_WDTCPS_7     EQU  H'3FE7'; Divider ratio 1:4096
_WDTCPS_WDTCPS_8     EQU  H'3FE8'; Divider ratio 1:8192
_WDTCPS_WDTCPS_9     EQU  H'3FE9'; Divider ratio 1:16384
_WDTCPS_WDTCPS_10    EQU  H'3FEA'; Divider ratio 1:32768
_WDTCPS_WDTCPS_11    EQU  H'3FEB'; Divider ratio 1:65536
_WDTCPS_WDTCPS_12    EQU  H'3FEC'; Divider ratio 1:131072
_WDTCPS_WDTCPS_13    EQU  H'3FED'; Divider ratio 1:262144
_WDTCPS_WDTCPS_14    EQU  H'3FEE'; Divider ratio 1:524299
_WDTCPS_WDTCPS_15    EQU  H'3FEF'; Divider ratio 1:1048576
_WDTCPS_WDTCPS_16    EQU  H'3FF0'; Divider ratio 1:2097152
_WDTCPS_WDTCPS_17    EQU  H'3FF1'; Divider ratio 1:4194304
_WDTCPS_WDTCPS_18    EQU  H'3FF2'; Divider ratio 1:8388608
_WDTCPS_WDTCPS_19    EQU  H'3FF3'; Divider ratio 1:32
_WDTCPS_WDTCPS_20    EQU  H'3FF4'; Divider ratio 1:32
_WDTCPS_WDTCPS_21    EQU  H'3FF5'; Divider ratio 1:32
_WDTCPS_WDTCPS_22    EQU  H'3FF6'; Divider ratio 1:32
_WDTCPS_WDTCPS_23    EQU  H'3FF7'; Divider ratio 1:32
_WDTCPS_WDTCPS_24    EQU  H'3FF8'; Divider ratio 1:32
_WDTCPS_WDTCPS_25    EQU  H'3FF9'; Divider ratio 1:32
_WDTCPS_WDTCPS_26    EQU  H'3FFA'; Divider ratio 1:32
_WDTCPS_WDTCPS_27    EQU  H'3FFB'; Divider ratio 1:32
_WDTCPS_WDTCPS_28    EQU  H'3FFC'; Divider ratio 1:32
_WDTCPS_WDTCPS_29    EQU  H'3FFD'; Divider ratio 1:32
_WDTCPS_WDTCPS_30    EQU  H'3FFE'; Divider ratio 1:32
_WDTCPS_WDTCPS_31    EQU  H'3FFF'; Divider ratio 1:65536; software control of WDTPS
_WDTE_OFF            EQU  H'3F9F'; WDT Disabled, SEN is ignored
_WDTE_SWDTEN         EQU  H'3FBF'; WDT enabled/disabled by SEN bit
_WDTE_NSLEEP         EQU  H'3FDF'; WDT enabled while Sleep = 0, suspended when Sleep = 1; SEN is ignored
_WDTE_ON             EQU  H'3FFF'; WDT enabled regardless of Sleep; SEN bit is ignored
_WDTCWS_WDTCWS_0     EQU  H'38FF'; window delay = 87.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_1     EQU  H'39FF'; window delay = 75 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_2     EQU  H'3AFF'; window delay = 62.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_3     EQU  H'3BFF'; window delay = 50 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_4     EQU  H'3CFF'; window delay = 37.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_5     EQU  H'3DFF'; window delay = 25 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_6     EQU  H'3EFF'; window always open (100%); no software control; keyed access required
_WDTCWS_WDTCWS_7     EQU  H'3FFF'; window always open (100%); software control; keyed access not required
_WDTCCS_LFINTOSC     EQU  H'07FF'; WDT reference clock is the 31.0 kHz LFINTOSC
_WDTCCS_MFINTOSC     EQU  H'0FFF'; WDT reference clock is the 32 kHz MFINTOSC
_WDTCCS_SOSC         EQU  H'17FF'; WDT reference clock is the 32kHz secondary oscillator
_WDTCCS_SC           EQU  H'3FFF'; Software Control

;----- CONFIG4 Options --------------------------------------------------
_BBSIZE_BB512        EQU  H'3FFF'; 512 words boot block size
_BBSIZE_BB1K         EQU  H'3FFE'; 1024 words boot block size
_BBSIZE_BB2K         EQU  H'3FFD'; 2048 words boot block size
_BBSIZE_BB4K         EQU  H'3FFC'; 4096 word boot block size
_BBSIZE_BB8K         EQU  H'3FFB'; 8192 word boot block size
_BBSIZE_BB16K        EQU  H'3FFA'; 16384 word boot block size
_BBSIZE_BB32K        EQU  H'3FF9'; * half of user program memory
_BBSIZE_BB64K        EQU  H'3FF8'; * half of user program memory
_BBEN_OFF            EQU  H'3FFF'; Boot Block disabled
_BBEN_ON             EQU  H'3FF7'; Boot Block enabled
_SAFEN_OFF           EQU  H'3FFF'; SAF disabled
_SAFEN_ON            EQU  H'3FEF'; SAF enabled
_WRTAPP_OFF          EQU  H'3FFF'; Application Block is NOT write protected
_WRTAPP_ON           EQU  H'3F7F'; Application Block is write protected
_WRTB_OFF            EQU  H'3FFF'; Boot Block is NOT write protected
_WRTB_ON             EQU  H'3EFF'; Boot Block is write protected
_WRTC_OFF            EQU  H'3FFF'; Configuration Register is NOT write protected
_WRTC_ON             EQU  H'3DFF'; Configuration Register is write protected
_WRTD_OFF            EQU  H'3FFF'; Data EEPROM is NOT write protected
_WRTD_ON             EQU  H'3BFF'; Data EEPROM is write protected
_WRTSAF_OFF          EQU  H'3FFF'; SAF is NOT write protected
_WRTSAF_ON           EQU  H'37FF'; SAF is write protected
_LVP_ON              EQU  H'3FFF'; Low Voltage programming enabled. MCLR/Vpp pin function is MCLR. MCLRE Configuration bit is ignored
_LVP_OFF             EQU  H'1FFF'; High Voltage on MCLR/Vpp must be used for programming

;----- CONFIG5 Options --------------------------------------------------
_CP_OFF              EQU  H'3FFF'; Program Flash Memory code protection is disabled
_CP_ON               EQU  H'3FFE'; Program Flash Memory code protection is enabled
_CPD_OFF             EQU  H'3FFF'; Data EEPROM code protection is disabled
_CPD_ON              EQU  H'3FFD'; Data EEPROM code protection is enabled

;----- DEVID Equates --------------------------------------------------
_IDLOC0            EQU  H'8000'
_IDLOC1            EQU  H'8001'
_IDLOC2            EQU  H'8001'
_IDLOC3            EQU  H'8002'
_IDLOC4            EQU  H'8002'
_IDLOC5            EQU  H'8003'
_IDLOC6            EQU  H'8003'
_IDLOC7            EQU  H'8004'

        LIST
