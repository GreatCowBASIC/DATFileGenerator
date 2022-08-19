        LIST
;==========================================================================
;
;  Built by Great Cow BASIC convertor
;  MPASM processor include for the chip shown below
;
; C:\Users\admin\.mchp_packs\index.idx
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\ini\16F18054.ini
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\edc\PIC16F18054.PIC
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\cfgdata\16F18054.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __16F18054
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
PORTB            EQU  H'000D'
PORTC            EQU  H'000E'
PORTE            EQU  H'0010'
TRISA            EQU  H'0012'
TRISB            EQU  H'0013'
TRISC            EQU  H'0014'
TRISE            EQU  H'0016'
LATA             EQU  H'0018'
LATB             EQU  H'0019'
LATC             EQU  H'001A'
LATE             EQU  H'001C'

;-----Bank1------------------
PIR0             EQU  H'008C'
PIR1             EQU  H'008D'
PIR2             EQU  H'008E'
PIR3             EQU  H'008F'
PIR4             EQU  H'0090'
PIE0             EQU  H'0096'
PIE1             EQU  H'0097'
PIE2             EQU  H'0098'
PIE3             EQU  H'0099'
PIE4             EQU  H'009A'

;-----Bank3------------------
WDTCON           EQU  H'018C'
BORCON           EQU  H'018D'
PCON0            EQU  H'018E'
PCON1            EQU  H'018F'
TMR0L            EQU  H'019C'
TMR0             EQU  H'019C'
TMR0H            EQU  H'019D'
PR0              EQU  H'019D'
T0CON0           EQU  H'019E'
T0CON1           EQU  H'019F'

;-----Bank4------------------
OSCCON2          EQU  H'020E'
OSCCON3          EQU  H'020F'
OSCSTAT          EQU  H'0210'
OSCEN            EQU  H'0211'
OSCTUNE          EQU  H'0212'
OSCFRQ           EQU  H'0213'
ACTCON           EQU  H'0214'

;-----Bank5------------------
TMR1             EQU  H'028C'
TMR1L            EQU  H'028C'
TMR1H            EQU  H'028D'
T1CON            EQU  H'028E'
T1GCON           EQU  H'028F'
PR1              EQU  H'028F'
T1GATE           EQU  H'0290'
TMR1GATE         EQU  H'0290'
T1CLK            EQU  H'0291'
TMR1CLK          EQU  H'0291'
TMR3             EQU  H'0292'
TMR3L            EQU  H'0292'
TMR3H            EQU  H'0293'
T3CON            EQU  H'0294'
T3GCON           EQU  H'0295'
PR3              EQU  H'0295'
T3GATE           EQU  H'0296'
TMR3GATE         EQU  H'0296'
T3CLK            EQU  H'0297'
TMR3CLK          EQU  H'0297'

;-----Bank6------------------
T2TMR            EQU  H'030C'
TMR2             EQU  H'030C'
T2PR             EQU  H'030D'
PR2              EQU  H'030D'
T2CON            EQU  H'030E'
T2HLT            EQU  H'030F'
T2CLKCON         EQU  H'0310'
T2CLK            EQU  H'0310'
T2RST            EQU  H'0311'
T4TMR            EQU  H'0312'
TMR4             EQU  H'0312'
T4PR             EQU  H'0313'
PR4              EQU  H'0313'
T4CON            EQU  H'0314'
T4HLT            EQU  H'0315'
T4CLKCON         EQU  H'0316'
T4CLK            EQU  H'0316'
T4RST            EQU  H'0317'
T6TMR            EQU  H'0318'
TMR6             EQU  H'0318'
T6PR             EQU  H'0319'
PR6              EQU  H'0319'
T6CON            EQU  H'031A'
T6HLT            EQU  H'031B'
T6CLKCON         EQU  H'031C'
T6CLK            EQU  H'031C'
T6RST            EQU  H'031D'

;-----Bank7------------------
CCPR1            EQU  H'038C'
CCPR1L           EQU  H'038C'
CCPR1H           EQU  H'038D'
CCP1CON          EQU  H'038E'
CCP1CAP          EQU  H'038F'
CCPR2            EQU  H'0390'
CCPR2L           EQU  H'0390'
CCPR2H           EQU  H'0391'
CCP2CON          EQU  H'0392'
CCP2CAP          EQU  H'0393'
CCPTMRS0         EQU  H'039F'

;-----Bank8------------------
PWM3DC           EQU  H'040C'
PWM3DCL          EQU  H'040C'
PWM3DCH          EQU  H'040D'
PWM3CON          EQU  H'040E'
PWM4DC           EQU  H'040F'
PWM4DCL          EQU  H'040F'
PWM4DCH          EQU  H'0410'
PWM4CON          EQU  H'0411'
PWM5DC           EQU  H'0412'
PWM5DCL          EQU  H'0412'
PWM5DCH          EQU  H'0413'
PWM5CON          EQU  H'0414'
PWMTMRS0         EQU  H'041F'

;-----Bank9------------------
NCO1ACC          EQU  H'048C'
NCO1ACCL         EQU  H'048C'
NCO1ACCH         EQU  H'048D'
NCO1ACCU         EQU  H'048E'
NCO1INC          EQU  H'048F'
NCO1INCL         EQU  H'048F'
NCO1INCH         EQU  H'0490'
NCO1INCU         EQU  H'0491'
NCO1CON          EQU  H'0492'
NCO1CLK          EQU  H'0493'

;-----Bank10------------------
CWG1CLK          EQU  H'050C'
CWG1CLKCON       EQU  H'050C'
CWG1ISM          EQU  H'050D'
CWG1DAT          EQU  H'050D'
CWG1DBR          EQU  H'050E'
CWG1DBF          EQU  H'050F'
CWG1CON0         EQU  H'0510'
CWG1CON1         EQU  H'0511'
CWG1AS0          EQU  H'0512'
CWG1AS1          EQU  H'0513'
CWG1STR          EQU  H'0514'

;-----Bank11------------------
CLCNCON          EQU  H'058C'
CLCNPOL          EQU  H'058D'
CLCNSEL0         EQU  H'058E'
CLCNSEL1         EQU  H'058F'
CLCNSEL2         EQU  H'0590'
CLCNSEL3         EQU  H'0591'
CLCNGLS0         EQU  H'0592'
CLCNGLS1         EQU  H'0593'
CLCNGLS2         EQU  H'0594'
CLCNGLS3         EQU  H'0595'
CLCSELECT        EQU  H'0596'
CLCDATA          EQU  H'0597'

;-----Bank12------------------
RC1REG           EQU  H'060C'
TX1REG           EQU  H'060D'
SP1BRG           EQU  H'060E'
SP1BRGL          EQU  H'060E'
SP1BRGH          EQU  H'060F'
RC1STA           EQU  H'0610'
TX1STA           EQU  H'0611'
BAUD1CON         EQU  H'0612'
RC2REG           EQU  H'0616'
TX2REG           EQU  H'0617'
SP2BRG           EQU  H'0618'
SP2BRGL          EQU  H'0618'
SP2BRGH          EQU  H'0619'
RC2STA           EQU  H'061A'
TX2STA           EQU  H'061B'
BAUD2CON         EQU  H'061C'

;-----Bank13------------------
SSP1BUF          EQU  H'068C'
SSP1ADD          EQU  H'068D'
SSP1MSK          EQU  H'068E'
SSP1STAT         EQU  H'068F'
SSP1CON1         EQU  H'0690'
SSP1CON2         EQU  H'0691'
SSP1CON3         EQU  H'0692'
SSP2BUF          EQU  H'0696'
SSP2ADD          EQU  H'0697'
SSP2MSK          EQU  H'0698'
SSP2STAT         EQU  H'0699'
SSP2CON1         EQU  H'069A'
SSP2CON2         EQU  H'069B'
SSP2CON3         EQU  H'069C'

;-----Bank32------------------
FVRCON           EQU  H'100C'
CPCON            EQU  H'100D'
ZCDCON           EQU  H'101F'

;-----Bank33------------------
CM1CON0          EQU  H'108C'
CM1CON1          EQU  H'108D'
CM1NCH           EQU  H'108E'
CM1PCH           EQU  H'108F'
CMOUT            EQU  H'109F'
CMSTAT           EQU  H'109F'

;-----Bank34------------------
DAC1CON          EQU  H'110C'
DAC1DATL         EQU  H'110D'

;-----Bank57------------------
NVMADR           EQU  H'1C8C'
NVMADRL          EQU  H'1C8C'
NVMADRH          EQU  H'1C8D'
NVMDAT           EQU  H'1C8E'
NVMDATL          EQU  H'1C8E'
NVMDATH          EQU  H'1C8F'
NVMCON1          EQU  H'1C90'
NVMCON2          EQU  H'1C91'

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
ADCG1B           EQU  H'1D2F'
ADCG1C           EQU  H'1D30'

;-----Bank59------------------
RA0PPS           EQU  H'1D8C'
RA1PPS           EQU  H'1D8D'
RA2PPS           EQU  H'1D8E'
RA3PPS           EQU  H'1D8F'
RA4PPS           EQU  H'1D90'
RA5PPS           EQU  H'1D91'
RA6PPS           EQU  H'1D92'
RA7PPS           EQU  H'1D93'
RB0PPS           EQU  H'1D94'
RB1PPS           EQU  H'1D95'
RB2PPS           EQU  H'1D96'
RB3PPS           EQU  H'1D97'
RB4PPS           EQU  H'1D98'
RB5PPS           EQU  H'1D99'
RB6PPS           EQU  H'1D9A'
RB7PPS           EQU  H'1D9B'
RC0PPS           EQU  H'1D9C'
RC1PPS           EQU  H'1D9D'
RC2PPS           EQU  H'1D9E'
RC3PPS           EQU  H'1D9F'
RC4PPS           EQU  H'1DA0'
RC5PPS           EQU  H'1DA1'
RC6PPS           EQU  H'1DA2'
RC7PPS           EQU  H'1DA3'

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
T6INPPS          EQU  H'1E1B'
CCP1PPS          EQU  H'1E1E'
CCP2PPS          EQU  H'1E1F'
CWG1PPS          EQU  H'1E39'
CLCIN0PPS        EQU  H'1E3D'
CLCIN1PPS        EQU  H'1E3E'
CLCIN2PPS        EQU  H'1E3F'
CLCIN3PPS        EQU  H'1E40'
RX1PPS           EQU  H'1E41'
TX1CKPPS         EQU  H'1E41'
CK1PPS           EQU  H'1E42'
RX1DTPPS         EQU  H'1E42'

;-----Bank61------------------
RX2PPS           EQU  H'1E43'
TX2CKPPS         EQU  H'1E43'
CK2PPS           EQU  H'1E44'
RX2DTPPS         EQU  H'1E44'
SSP1CLKPPS       EQU  H'1E47'
SSP1DATPPS       EQU  H'1E48'
SSP1SSPPS        EQU  H'1E49'
SSP2CLKPPS       EQU  H'1E4A'
SSP2DATPPS       EQU  H'1E4B'
SSP2SSPPS        EQU  H'1E4C'
ADACTPPS         EQU  H'1E50'
ANSELA           EQU  H'1E8C'
WPUA             EQU  H'1E8D'
ODCONA           EQU  H'1E8E'
SLRCONA          EQU  H'1E8F'
INLVLA           EQU  H'1E90'
IOCAP            EQU  H'1E91'
IOCAN            EQU  H'1E92'
IOCAF            EQU  H'1E93'
ANSELB           EQU  H'1E96'
WPUB             EQU  H'1E97'
ODCONB           EQU  H'1E98'
SLRCONB          EQU  H'1E99'
INLVLB           EQU  H'1E9A'
IOCBP            EQU  H'1E9B'
IOCBN            EQU  H'1E9C'
IOCBF            EQU  H'1E9D'
ANSELC           EQU  H'1EA0'
WPUC             EQU  H'1EA1'
ODCONC           EQU  H'1EA2'
SLRCONC          EQU  H'1EA3'
INLVLC           EQU  H'1EA4'
IOCCP            EQU  H'1EA5'
IOCCN            EQU  H'1EA6'
IOCCF            EQU  H'1EA7'
WPUE             EQU  H'1EB5'
INLVLE           EQU  H'1EB8'
IOCEP            EQU  H'1EB9'
IOCEN            EQU  H'1EBA'
IOCEF            EQU  H'1EBB'

;-----Bank62------------------
RB1I2C           EQU  H'1EE3'
RB2I2C           EQU  H'1EE4'
RC3I2C           EQU  H'1EEB'
RC4I2C           EQU  H'1EEC'
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
ACTIF            EQU  H'0002'
TMR1IF           EQU  H'0003'
TMR1GIF          EQU  H'0004'
TMR3IF           EQU  H'0005'
TMR3GIF          EQU  H'0006'
TMR2IF           EQU  H'0007'


;----- PIE1 Bits -----------------------------------------------------
NVMIE            EQU  H'0000'
ACTIE            EQU  H'0002'
TMR1IE           EQU  H'0003'
TMR1GIE          EQU  H'0004'
TMR3IE           EQU  H'0005'
TMR3GIE          EQU  H'0006'
TMR2IE           EQU  H'0007'


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


;----- ADCON1 Bits -----------------------------------------------------
DSEN             EQU  H'0000'
PCSC             EQU  H'0001'
GPOL             EQU  H'0005'
IPEN             EQU  H'0006'
PPOL             EQU  H'0007'
ADDSEN           EQU  H'0000'
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


;----- LATE Bits -----------------------------------------------------
LATE3            EQU  H'0003'


;----- PIR0 Bits -----------------------------------------------------
INTF             EQU  H'0000'
IOCIF            EQU  H'0004'
TMR0IF           EQU  H'0005'


;----- PIR2 Bits -----------------------------------------------------
TMR4IF           EQU  H'0000'
TMR6IF           EQU  H'0001'
CCP1IF           EQU  H'0002'
CCP2IF           EQU  H'0003'
NCO1IF           EQU  H'0004'
CWG1IF           EQU  H'0005'
CLC1IF           EQU  H'0006'
CLC2IF           EQU  H'0007'


;----- PIR3 Bits -----------------------------------------------------
CLC3IF           EQU  H'0000'
CLC4IF           EQU  H'0001'
TX1IF            EQU  H'0002'
RC1IF            EQU  H'0003'
TX2IF            EQU  H'0004'
RC2IF            EQU  H'0005'
SSP1IF           EQU  H'0006'
BCL1IF           EQU  H'0007'


;----- PIR4 Bits -----------------------------------------------------
SSP2IF           EQU  H'0000'
BCL2IF           EQU  H'0001'
CM1IF            EQU  H'0002'
ADIF             EQU  H'0003'
ADTIF            EQU  H'0004'
ZCDIF            EQU  H'0005'


;----- PIE0 Bits -----------------------------------------------------
INTE             EQU  H'0000'
IOCIE            EQU  H'0004'
TMR0IE           EQU  H'0005'


;----- PIE2 Bits -----------------------------------------------------
TMR4IE           EQU  H'0000'
TMR6IE           EQU  H'0001'
CCP1IE           EQU  H'0002'
CCP2IE           EQU  H'0003'
NCO1IE           EQU  H'0004'
CWG1IE           EQU  H'0005'
CLC1IE           EQU  H'0006'
CLC2IE           EQU  H'0007'


;----- PIE3 Bits -----------------------------------------------------
CLC3IE           EQU  H'0000'
CLC4IE           EQU  H'0001'
TX1IE            EQU  H'0002'
RC1IE            EQU  H'0003'
TX2IE            EQU  H'0004'
RC2IE            EQU  H'0005'
SSP1IE           EQU  H'0006'
BCL1IE           EQU  H'0007'


;----- PIE4 Bits -----------------------------------------------------
SSP2IE           EQU  H'0000'
BCL2IE           EQU  H'0001'
CM1IE            EQU  H'0002'
ADIE             EQU  H'0003'
ADTIE            EQU  H'0004'
ZCDIE            EQU  H'0005'


;----- WDTCON Bits -----------------------------------------------------
SEN              EQU  H'0000'
CS               EQU  H'0007'
WDTPS0           EQU  H'0001'
WDTPS1           EQU  H'0002'
WDTPS2           EQU  H'0003'
WDTPS3           EQU  H'0004'
WDTPS4           EQU  H'0005'
SWDTEN           EQU  H'0000'
WDTCS            EQU  H'0007'


;----- BORCON Bits -----------------------------------------------------
BORRDY           EQU  H'0000'
SBOREN           EQU  H'0007'


;----- PCON0 Bits -----------------------------------------------------
NBOR             EQU  H'0000'
NPOR             EQU  H'0001'
NRI              EQU  H'0002'
NRMCLR           EQU  H'0003'
NRWDT            EQU  H'0004'
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


;----- OSCCON2 Bits -----------------------------------------------------
COSC0            EQU  H'0004'
COSC1            EQU  H'0005'
COSC2            EQU  H'0006'


;----- OSCCON3 Bits -----------------------------------------------------
SOSCPWR          EQU  H'0006'


;----- OSCSTAT Bits -----------------------------------------------------
SFOR             EQU  H'0001'
ADOR             EQU  H'0002'
SOR              EQU  H'0003'
LFOR             EQU  H'0004'
MFOR             EQU  H'0005'
HFOR             EQU  H'0006'


;----- OSCEN Bits -----------------------------------------------------
ADOEN            EQU  H'0002'
SOSCEN           EQU  H'0003'
LFOEN            EQU  H'0004'
MFOEN            EQU  H'0005'
HFOEN            EQU  H'0006'


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
T2RSEL0          EQU  H'0000'
T2RSEL1          EQU  H'0001'
T2RSEL2          EQU  H'0002'
T2RSEL3          EQU  H'0003'


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
T4RSEL0          EQU  H'0000'
T4RSEL1          EQU  H'0001'
T4RSEL2          EQU  H'0002'
T4RSEL3          EQU  H'0003'


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
T6CSYNC          EQU  H'0005'
T6CPOL           EQU  H'0006'
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
T6RSEL0          EQU  H'0000'
T6RSEL1          EQU  H'0001'
T6RSEL2          EQU  H'0002'
T6RSEL3          EQU  H'0003'


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


;----- PWM3CON Bits -----------------------------------------------------
POL_PWM3CON      EQU  H'0004'
OUT_PWM3CON      EQU  H'0005'
EN               EQU  H'0007'
PWM3POL          EQU  H'0004'
PWM3OUT          EQU  H'0005'
PWM3EN           EQU  H'0007'


;----- PWM4CON Bits -----------------------------------------------------
POL_PWM4CON      EQU  H'0004'
OUT_PWM4CON      EQU  H'0005'
EN               EQU  H'0007'
PWM4POL          EQU  H'0004'
PWM4OUT          EQU  H'0005'
PWM4EN           EQU  H'0007'


;----- PWM5CON Bits -----------------------------------------------------
POL_PWM5CON      EQU  H'0004'
OUT_PWM5CON      EQU  H'0005'
EN               EQU  H'0007'
PWM5POL          EQU  H'0004'
PWM5OUT          EQU  H'0005'
PWM5EN           EQU  H'0007'


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
DAT0             EQU  H'0000'
DAT1             EQU  H'0001'
DAT2             EQU  H'0002'
DAT3             EQU  H'0003'


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


;----- CLCSELECT Bits -----------------------------------------------------
SLCT0            EQU  H'0000'
SLCT1            EQU  H'0001'
SLCT2            EQU  H'0002'


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


;----- CMOUT Bits -----------------------------------------------------
C1OUT_CMOUT      EQU  H'0000'


;----- DAC1CON Bits -----------------------------------------------------
NSS              EQU  H'0000'
REFRNG           EQU  H'0006'
EN               EQU  H'0007'
PSS0             EQU  H'0002'
PSS1             EQU  H'0003'
OE0              EQU  H'0004'
OE1              EQU  H'0005'


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
CS               EQU  H'0004'
CONT             EQU  H'0006'
ON_ADCON0        EQU  H'0007'
ADGO             EQU  H'0000'
ADCS             EQU  H'0004'
ADCONT           EQU  H'0006'
ADON             EQU  H'0007'
DONE             EQU  H'0000'
FM0              EQU  H'0002'
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
MD0              EQU  H'0000'
MD1              EQU  H'0001'
MD2              EQU  H'0002'
CRS0             EQU  H'0004'
CRS1             EQU  H'0005'
CRS2             EQU  H'0006'


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
OV               EQU  H'0007'


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
CGA3             EQU  H'0003'
CGA4             EQU  H'0004'
CGA5             EQU  H'0005'
CGA6             EQU  H'0006'
CGA7             EQU  H'0007'


;----- ADCG1B Bits -----------------------------------------------------
CGB0             EQU  H'0000'
CGB1             EQU  H'0001'
CGB2             EQU  H'0002'
CGB3             EQU  H'0003'
CGB4             EQU  H'0004'
CGB5             EQU  H'0005'
CGB6             EQU  H'0006'
CGB7             EQU  H'0007'


;----- ADCG1C Bits -----------------------------------------------------
CGC0             EQU  H'0000'
CGC1             EQU  H'0001'
CGC2             EQU  H'0002'
CGC3             EQU  H'0003'
CGC4             EQU  H'0004'
CGC5             EQU  H'0005'
CGC6             EQU  H'0006'
CGC7             EQU  H'0007'


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
ANSELA3          EQU  H'0003'
ANSELA4          EQU  H'0004'
ANSELA5          EQU  H'0005'
ANSELA6          EQU  H'0006'
ANSELA7          EQU  H'0007'
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA3            EQU  H'0003'
ANSA4            EQU  H'0004'
ANSA5            EQU  H'0005'
ANSA6            EQU  H'0006'
ANSA7            EQU  H'0007'


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
ANSB0            EQU  H'0000'
ANSB1            EQU  H'0001'
ANSB2            EQU  H'0002'
ANSB3            EQU  H'0003'
ANSB4            EQU  H'0004'
ANSB5            EQU  H'0005'
ANSB6            EQU  H'0006'
ANSB7            EQU  H'0007'


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
ANSC0            EQU  H'0000'
ANSC1            EQU  H'0001'
ANSC2            EQU  H'0002'
ANSC3            EQU  H'0003'
ANSC4            EQU  H'0004'
ANSC5            EQU  H'0005'
ANSC6            EQU  H'0006'
ANSC7            EQU  H'0007'


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

       __MAXRAM  H'0FFF'
       __BADRAM  H'000F'
       __BADRAM  H'0011'
       __BADRAM  H'0015'
       __BADRAM  H'0017'
       __BADRAM  H'001B'
       __BADRAM  H'001E'-H'0020'
       __BADRAM  H'0091'-H'0095'
       __BADRAM  H'009B'-H'009D'
       __BADRAM  H'010C'-H'011F'
       __BADRAM  H'0190'-H'019B'
       __BADRAM  H'020C'-H'020D'
       __BADRAM  H'0215'-H'021F'
       __BADRAM  H'0298'-H'029F'
       __BADRAM  H'031E'-H'031F'
       __BADRAM  H'0394'-H'039E'
       __BADRAM  H'0415'-H'041E'
       __BADRAM  H'0494'-H'049F'
       __BADRAM  H'0515'-H'051F'
       __BADRAM  H'0598'-H'05A3'
       __BADRAM  H'0613'-H'0615'
       __BADRAM  H'061E'-H'0620'
       __BADRAM  H'0693'-H'0695'
       __BADRAM  H'069E'-H'06A0'
       __BADRAM  H'070C'-H'071F'
       __BADRAM  H'078C'-H'079F'
       __BADRAM  H'080C'-H'081F'
       __BADRAM  H'088C'-H'089F'
       __BADRAM  H'090C'-H'091F'
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
       __BADRAM  H'100E'-H'101E'
       __BADRAM  H'1090'-H'109E'
       __BADRAM  H'110E'-H'111F'
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
       __BADRAM  H'1C92'-H'1CEF'
       __BADRAM  H'1E20'
       __BADRAM  H'1E31'-H'1E6F'
       __BADRAM  H'1EA4'-H'1EEF'
       __BADRAM  H'1E13'-H'1E18'
       __BADRAM  H'1E1C'-H'1E1D'
       __BADRAM  H'1E20'-H'1E38'
       __BADRAM  H'1E3A'-H'1E3C'
       __BADRAM  H'1E45'-H'1E46'
       __BADRAM  H'1E4E'-H'1E50'
       __BADRAM  H'1E51'-H'1E6F'
       __BADRAM  H'1E94'-H'1E95'
       __BADRAM  H'1E9E'-H'1E9F'
       __BADRAM  H'1EA8'-H'1EB5'
       __BADRAM  H'1EB6'-H'1EB7'
       __BADRAM  H'1EBC'-H'1EE2'
       __BADRAM  H'1EE5'-H'1EEA'
       __BADRAM  H'1EEE'-H'1EF0'
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
             ; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.15.191\xc8\pic\dat\cfgdata\16F18054.cfgdata
;==========================================================================

;----- CONFIG1 Options --------------------------------------------------
_FEXTOSC_ECH         EQU  H'3FFF'; EC (external clock) above 16 MHz
_FEXTOSC_ECL         EQU  H'3FFD'; EC (external clock) below 16 MHz
_FEXTOSC_OFF         EQU  H'3FFC'; Oscillator not enabled
_RSTOSC_EXTOSC       EQU  H'3FFF'; EXTOSC operating per FEXTOSC bits
_RSTOSC_HFINTOSC_1MHz EQU  H'3FEF'; HFINTOSC (1MHz)
_RSTOSC_LFINTOSC     EQU  H'3FDF'; LFINTOSC
_RSTOSC_SOSC         EQU  H'3FCF'; SOSC
_RSTOSC_HFINTOSC_32MHz EQU  H'3F8F'; HFINTOSC (32 MHz)
_CLKOUTEN_ON         EQU  H'3EFF'; CLKOUT function is enabled; FOSC/4 clock appears at OSC2
_CLKOUTEN_OFF        EQU  H'3FFF'; CLKOUT function is disabled; i/o or oscillator function on OSC2
_VDDAR_HI            EQU  H'3FFF'; Internal analog systems are calibrated for operation between VDD = 2.3 - 5.5V
_VDDAR_LO            EQU  H'2FFF'; Internal analog systems are calibrated for operation between VDD = 1.8 - 3.6V

;----- CONFIG2 Options --------------------------------------------------
_MCLRE_EXTMCLR       EQU  H'3FFF'; If LVP = 0, MCLR pin is MCLR; If LVP = 1, RA3 pin function is MCLR
_MCLRE_INTMCLR       EQU  H'3FFE'; If LVP = 0, MCLR is port-defined function; If LVP = 1, RA3 pin function is MCLR
_PWRTS_PWRT_OFF      EQU  H'3FFF'; PWRT is disabled
_PWRTS_PWRT_64       EQU  H'3FFD'; PWRT set at 64 ms
_PWRTS_PWRT_16       EQU  H'3FFB'; PWRT set at 16 ms
_PWRTS_PWRT_1        EQU  H'3FF9'; PWRT set at 1 ms
_WDTE_ON             EQU  H'3FFF'; WDT enabled regardless of Sleep; SEN bit is ignored
_WDTE_NSLEEP         EQU  H'3FF7'; WDT enabled when sleep = 0, suspended when Sleep = 1; SEN is ignored
_WDTE_SWDTEN         EQU  H'3FEF'; WDT enabled/disabled by SEN bit
_WDTE_OFF            EQU  H'3FE7'; WDT disabled; SEN is ignored
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
_WRTAPP_OFF          EQU  H'3FFF'; Application Block is NOT write-protected
_WRTAPP_ON           EQU  H'3F7F'; Application Block is write-protected
_WRTB_OFF            EQU  H'3FFF'; Boot Block is NOT write-protected
_WRTB_ON             EQU  H'3EFF'; Boot Block is write-protected
_WRTC_OFF            EQU  H'3FFF'; Configuration Register is NOT write-protected
_WRTC_ON             EQU  H'3DFF'; Configuration Register is write-protected
_WRTD_OFF            EQU  H'3FFF'; Data EEPROM is NOT write-protected
_WRTD_ON             EQU  H'3BFF'; Data EEPROM is write-protected
_WRTSAF_OFF          EQU  H'3FFF'; SAF is NOT write-protected
_WRTSAF_ON           EQU  H'37FF'; SAF is write-protected
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
