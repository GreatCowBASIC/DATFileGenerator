        LIST
;==========================================================================
;
;  Built by Great Cow BASIC convertor
;  MPASM processor include for the chip shown below
;
; C:\Users\admin\.mchp_packs\index.idx
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.8.149\xc8\pic\dat\ini\16F15275.ini
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.8.149\edc\PIC16F15275.PIC
; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.8.149\xc8\pic\dat\cfgdata\16F15275.cfgdata
;
;==========================================================================
        NOLIST
        IFNDEF __16F15275
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
PORTD            EQU  H'000F'
PORTE            EQU  H'0010'
TRISA            EQU  H'0012'
TRISB            EQU  H'0013'
TRISC            EQU  H'0014'
TRISD            EQU  H'0015'
TRISE            EQU  H'0016'
LATA             EQU  H'0018'
LATB             EQU  H'0019'
LATC             EQU  H'001A'
LATD             EQU  H'001B'
LATE             EQU  H'001C'

;-----Bank1------------------
CPCON            EQU  H'009A'
ADRES            EQU  H'009B'
ADRESL           EQU  H'009B'
ADRESH           EQU  H'009C'
ADCON0           EQU  H'009D'
ADCON1           EQU  H'009E'
ADACT            EQU  H'009F'

;-----Bank2------------------
RB1I2C           EQU  H'010C'
RB2I2C           EQU  H'010D'
RC3I2C           EQU  H'010E'
RC4I2C           EQU  H'010F'
RC1REG           EQU  H'0119'
TX1REG           EQU  H'011A'
SP1BRG           EQU  H'011B'
SP1BRGL          EQU  H'011B'
SP1BRGH          EQU  H'011C'
RC1STA           EQU  H'011D'
TX1STA           EQU  H'011E'
BAUD1CON         EQU  H'011F'

;-----Bank3------------------
SSP1BUF          EQU  H'018C'
SSP1ADD          EQU  H'018D'
SSP1MSK          EQU  H'018E'
SSP1STAT         EQU  H'018F'
SSP1CON1         EQU  H'0190'
SSP1CON2         EQU  H'0191'
SSP1CON3         EQU  H'0192'

;-----Bank4------------------
TMR1L            EQU  H'020C'
TMR1H            EQU  H'020D'
T1CON            EQU  H'020E'
TMR1CON          EQU  H'020E'
T1GCON           EQU  H'020F'
TMR1GCON         EQU  H'020F'
T1GATE           EQU  H'0210'
TMR1GATE         EQU  H'0210'
T1CLK            EQU  H'0211'
TMR1CLK          EQU  H'0211'

;-----Bank5------------------
T2TMR            EQU  H'028C'
TMR2             EQU  H'028C'
T2PR             EQU  H'028D'
PR2              EQU  H'028D'
T2CON            EQU  H'028E'
T2HLT            EQU  H'028F'
T2CLKCON         EQU  H'0290'
T2RST            EQU  H'0291'

;-----Bank6------------------
CCPR1            EQU  H'030C'
CCPR1L           EQU  H'030C'
CCPR1H           EQU  H'030D'
CCP1CON          EQU  H'030E'
CCP1CAP          EQU  H'030F'
CCPR2            EQU  H'0310'
CCPR2L           EQU  H'0310'
CCPR2H           EQU  H'0311'
CCP2CON          EQU  H'0312'
CCP2CAP          EQU  H'0313'
PWM3DC           EQU  H'0314'
PWM3DCL          EQU  H'0314'
PWM3DCH          EQU  H'0315'
PWM3CON          EQU  H'0316'
PWM4DC           EQU  H'0318'
PWM4DCL          EQU  H'0318'
PWM4DCH          EQU  H'0319'
PWM4CON          EQU  H'031A'

;-----Bank11------------------
TMR0L            EQU  H'059C'
TMR0             EQU  H'059C'
TMR0H            EQU  H'059D'
PR0              EQU  H'059D'
T0CON0           EQU  H'059E'
T0CON1           EQU  H'059F'

;-----Bank14------------------
PIR0             EQU  H'070C'
PIR1             EQU  H'070D'
PIR2             EQU  H'070E'
PIE0             EQU  H'0716'
PIE1             EQU  H'0717'
PIE2             EQU  H'0718'

;-----Bank16------------------
WDTCON           EQU  H'080C'
BORCON           EQU  H'0811'
PCON0            EQU  H'0813'
PCON1            EQU  H'0814'
NVMADR           EQU  H'081A'
NVMADRL          EQU  H'081A'
NVMADRH          EQU  H'081B'
NVMDAT           EQU  H'081C'
NVMDATL          EQU  H'081C'
NVMDATH          EQU  H'081D'
NVMCON1          EQU  H'081E'
NVMCON2          EQU  H'081F'

;-----Bank17------------------
OSCCON           EQU  H'088E'
OSCSTAT          EQU  H'0890'
OSCEN            EQU  H'0891'
OSCTUNE          EQU  H'0892'
OSCFRQ           EQU  H'0893'

;-----Bank18------------------
FVRCON           EQU  H'090C'

;-----Bank61------------------
PPSLOCK          EQU  H'1E8F'
INTPPS           EQU  H'1E90'
T0CKIPPS         EQU  H'1E91'
T1CKIPPS         EQU  H'1E92'
T1GPPS           EQU  H'1E93'
T2INPPS          EQU  H'1E9C'
CCP1PPS          EQU  H'1EA1'
CCP2PPS          EQU  H'1EA2'

;-----Bank62------------------
ADACTPPS         EQU  H'1EC3'
SSP1CLKPPS       EQU  H'1EC5'
SSP1DATPPS       EQU  H'1EC6'
SSP1SSPPS        EQU  H'1EC7'
RX1PPS           EQU  H'1ECB'
RX1DTPPS         EQU  H'1ECB'
RXPPS            EQU  H'1ECB'
CK1PPS           EQU  H'1ECC'
TX1CKPPS         EQU  H'1ECC'
CKPPS            EQU  H'1ECC'
RA0PPS           EQU  H'1F10'
RA1PPS           EQU  H'1F11'
RA2PPS           EQU  H'1F12'
RA3PPS           EQU  H'1F13'
RA4PPS           EQU  H'1F14'
RA5PPS           EQU  H'1F15'
RA6PPS           EQU  H'1F16'
RA7PPS           EQU  H'1F17'
RB0PPS           EQU  H'1F18'
RB1PPS           EQU  H'1F19'
RB2PPS           EQU  H'1F1A'
RB3PPS           EQU  H'1F1B'
RB4PPS           EQU  H'1F1C'
RB5PPS           EQU  H'1F1D'
RB6PPS           EQU  H'1F1E'
RB7PPS           EQU  H'1F1F'
RC0PPS           EQU  H'1F20'
RC1PPS           EQU  H'1F21'
RC2PPS           EQU  H'1F22'
RC3PPS           EQU  H'1F23'
RC4PPS           EQU  H'1F24'
RC5PPS           EQU  H'1F25'
RC6PPS           EQU  H'1F26'
RC7PPS           EQU  H'1F27'
RD0PPS           EQU  H'1F28'
RD1PPS           EQU  H'1F29'
RD2PPS           EQU  H'1F2A'
RD3PPS           EQU  H'1F2B'
RD4PPS           EQU  H'1F2C'
RD5PPS           EQU  H'1F2D'
RD6PPS           EQU  H'1F2E'
RD7PPS           EQU  H'1F2F'
RE0PPS           EQU  H'1F30'
RE1PPS           EQU  H'1F31'
RE2PPS           EQU  H'1F32'
ANSELA           EQU  H'1F38'
WPUA             EQU  H'1F39'
ODCONA           EQU  H'1F3A'
SLRCONA          EQU  H'1F3B'
INLVLA           EQU  H'1F3C'
IOCAP            EQU  H'1F3D'
IOCAN            EQU  H'1F3E'
IOCAF            EQU  H'1F3F'

;-----Bank63------------------
ANSELB           EQU  H'1F43'
WPUB             EQU  H'1F44'
ODCONB           EQU  H'1F45'
SLRCONB          EQU  H'1F46'
INLVLB           EQU  H'1F47'
IOCBP            EQU  H'1F48'
IOCBN            EQU  H'1F49'
IOCBF            EQU  H'1F4A'
ANSELC           EQU  H'1F4E'
WPUC             EQU  H'1F4F'
ODCONC           EQU  H'1F50'
SLRCONC          EQU  H'1F51'
INLVLC           EQU  H'1F52'
IOCCP            EQU  H'1F53'
IOCCN            EQU  H'1F54'
IOCCF            EQU  H'1F55'
ANSELD           EQU  H'1F59'
WPUD             EQU  H'1F5A'
ODCOND           EQU  H'1F5B'
SLRCOND          EQU  H'1F5C'
INLVLD           EQU  H'1F5D'
IOCDP            EQU  H'1F5E'
IOCDN            EQU  H'1F5F'
IOCDF            EQU  H'1F60'
ANSELE           EQU  H'1F64'
WPUE             EQU  H'1F65'
ODCONE           EQU  H'1F66'
SLRCONE          EQU  H'1F67'
INLVLE           EQU  H'1F68'
IOCEP            EQU  H'1F69'
IOCEN            EQU  H'1F6A'
IOCEF            EQU  H'1F6B'

;-----Bank64------------------
STATUS_SHAD      EQU  H'1FE4'
WREG_SHAD        EQU  H'1FE5'
BSR_SHAD         EQU  H'1FE6'
PCLATH_SHAD      EQU  H'1FE7'
FSR0_SHAD        EQU  H'1FE8'
FSR0L_SHAD       EQU  H'1FE8'
FSR0H_SHAD       EQU  H'1FE9'
FSR1L_SHAD       EQU  H'1FEA'
FSR1H_SHAD       EQU  H'1FEB'
STKPTR           EQU  H'1FED'
TOSL             EQU  H'1FEE'
TOSH             EQU  H'1FEF'


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


;----- CPCON Bits -----------------------------------------------------
CPRDY            EQU  H'0000'
CPT              EQU  H'0001'


;----- ADCON0 Bits -----------------------------------------------------
ON_ADCON0        EQU  H'0000'
GO_ADCON0        EQU  H'0001'
ADON             EQU  H'0000'
GO_NDONE         EQU  H'0001'
ADGO             EQU  H'0001'


;----- ADCON1 Bits -----------------------------------------------------
FM               EQU  H'0007'
ADFM             EQU  H'0007'


;----- RB1I2C Bits -----------------------------------------------------
SLEW             EQU  H'0006'


;----- RB2I2C Bits -----------------------------------------------------
SLEW             EQU  H'0006'


;----- RC3I2C Bits -----------------------------------------------------
SLEW             EQU  H'0006'


;----- RC4I2C Bits -----------------------------------------------------
SLEW             EQU  H'0006'


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


;----- T1GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO              EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL             EQU  H'0006'
GE               EQU  H'0007'
T1GVAL           EQU  H'0002'
T1GGO            EQU  H'0003'
T1GSPM           EQU  H'0004'
T1GTM            EQU  H'0005'
T1GPOL           EQU  H'0006'
T1GE             EQU  H'0007'
NOT_DONE         EQU  H'0003'
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


;----- T1CLK Bits -----------------------------------------------------
T1CS0            EQU  H'0000'
T1CS1            EQU  H'0001'
T1CS2            EQU  H'0002'
T1CS3            EQU  H'0003'
CS0_T1CLK        EQU  H'0000'
CS1_T1CLK        EQU  H'0001'
CS2_T1CLK        EQU  H'0002'
CS3              EQU  H'0003'


;----- T2CON Bits -----------------------------------------------------
ON_T2CON         EQU  H'0007'
OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0_T2CON      EQU  H'0004'
CKPS1_T2CON      EQU  H'0005'
CKPS2_T2CON      EQU  H'0006'
T2ON             EQU  H'0007'
T2OUTPS0         EQU  H'0000'
T2OUTPS1         EQU  H'0001'
T2OUTPS2         EQU  H'0002'
T2OUTPS3         EQU  H'0003'
T2CKPS0          EQU  H'0004'
T2CKPS1          EQU  H'0005'
T2CKPS2          EQU  H'0006'
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
T2RSEL0          EQU  H'0000'
T2RSEL1          EQU  H'0001'
T2RSEL2          EQU  H'0002'
T2RSEL3          EQU  H'0003'


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
CCP1CTS0         EQU  H'0000'
CCP1CTS1         EQU  H'0001'


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
CCP2CTS0         EQU  H'0000'
CCP2CTS1         EQU  H'0001'


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


;----- PIR0 Bits -----------------------------------------------------
INTF             EQU  H'0000'
IOCIF            EQU  H'0004'
TMR0IF           EQU  H'0005'


;----- PIR1 Bits -----------------------------------------------------
ADIF             EQU  H'0000'
SSP1IF           EQU  H'0001'
BCL1IF           EQU  H'0002'
TX1IF            EQU  H'0003'
RC1IF            EQU  H'0004'
TMR1IF           EQU  H'0005'
TMR2IF           EQU  H'0006'
CCP1IF           EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
TMR1GIF          EQU  H'0005'
NVMIF            EQU  H'0006'
CCP2IF           EQU  H'0007'


;----- PIE0 Bits -----------------------------------------------------
INTE             EQU  H'0000'
IOCIE            EQU  H'0004'
TMR0IE           EQU  H'0005'


;----- PIE1 Bits -----------------------------------------------------
ADIE             EQU  H'0000'
SSP1IE           EQU  H'0001'
BCL1IE           EQU  H'0002'
TX1IE            EQU  H'0003'
RC1IE            EQU  H'0004'
TMR1IE           EQU  H'0005'
TMR2IE           EQU  H'0006'
CCP1IE           EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
TMR1GIE          EQU  H'0005'
NVMIE            EQU  H'0006'
CCP2IE           EQU  H'0007'


;----- WDTCON Bits -----------------------------------------------------
SEN              EQU  H'0000'
CS               EQU  H'0007'
WDTSEN           EQU  H'0000'
WDTCS            EQU  H'0007'
SWDTEN           EQU  H'0000'


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


;----- NVMCON1 Bits -----------------------------------------------------
RD               EQU  H'0000'
WR               EQU  H'0001'
WREN             EQU  H'0002'
WRERR            EQU  H'0003'
FREE             EQU  H'0004'
LWLO             EQU  H'0005'
NVMREGS          EQU  H'0006'


;----- OSCCON Bits -----------------------------------------------------
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'
NOSC2            EQU  H'0006'


;----- OSCSTAT Bits -----------------------------------------------------
SFOR             EQU  H'0001'
ADOR             EQU  H'0002'
LFOR             EQU  H'0004'
MFOR             EQU  H'0005'
HFOR             EQU  H'0006'


;----- OSCEN Bits -----------------------------------------------------
ADOEN            EQU  H'0002'
LFOEN            EQU  H'0004'
MFOEN            EQU  H'0005'
HFOEN            EQU  H'0006'


;----- OSCTUNE Bits -----------------------------------------------------
HFTUN0           EQU  H'0000'
HFTUN1           EQU  H'0001'
HFTUN2           EQU  H'0002'
HFTUN3           EQU  H'0003'
HFTUN4           EQU  H'0004'
HFTUN5           EQU  H'0005'


;----- OSCFRQ Bits -----------------------------------------------------
HFFRQ0           EQU  H'0000'
HFFRQ1           EQU  H'0001'
HFFRQ2           EQU  H'0002'


;----- FVRCON Bits -----------------------------------------------------
FVRRDY           EQU  H'0006'
FVREN            EQU  H'0007'
ADFVR0           EQU  H'0000'
ADFVR1           EQU  H'0001'
CDAFVR0          EQU  H'0002'
CDAFVR1          EQU  H'0003'


;----- PPSLOCK Bits -----------------------------------------------------
PPSLOCKED        EQU  H'0000'


;----- INTPPS Bits -----------------------------------------------------
INTPPS0          EQU  H'0000'
INTPPS1          EQU  H'0001'
INTPPS2          EQU  H'0002'
INTPPS3          EQU  H'0003'


;----- T0CKIPPS Bits -----------------------------------------------------
T0CKIPPS0        EQU  H'0000'
T0CKIPPS1        EQU  H'0001'
T0CKIPPS2        EQU  H'0002'
T0CKIPPS3        EQU  H'0003'


;----- T1CKIPPS Bits -----------------------------------------------------
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


;----- T2INPPS Bits -----------------------------------------------------
T2INPPS0         EQU  H'0000'
T2INPPS1         EQU  H'0001'
T2INPPS2         EQU  H'0002'
T2INPPS3         EQU  H'0003'
T2INPPS4         EQU  H'0004'


;----- CCP1PPS Bits -----------------------------------------------------
CCP1PPS0         EQU  H'0000'
CCP1PPS1         EQU  H'0001'
CCP1PPS2         EQU  H'0002'
CCP1PPS3         EQU  H'0003'
CCP1PPS4         EQU  H'0004'


;----- CCP2PPS Bits -----------------------------------------------------
CCP2PPS0         EQU  H'0000'
CCP2PPS1         EQU  H'0001'
CCP2PPS2         EQU  H'0002'
CCP2PPS3         EQU  H'0003'
CCP2PPS4         EQU  H'0004'


;----- ADACTPPS Bits -----------------------------------------------------
ADACTPPS0        EQU  H'0000'
ADACTPPS1        EQU  H'0001'
ADACTPPS2        EQU  H'0002'
ADACTPPS3        EQU  H'0003'
ADACTPPS4        EQU  H'0004'


;----- SSP1CLKPPS Bits -----------------------------------------------------
SSP1CLKPPS0      EQU  H'0000'
SSP1CLKPPS1      EQU  H'0001'
SSP1CLKPPS2      EQU  H'0002'
SSP1CLKPPS3      EQU  H'0003'
SSP1CLKPPS4      EQU  H'0004'


;----- SSP1DATPPS Bits -----------------------------------------------------
SSP1DATPPS0      EQU  H'0000'
SSP1DATPPS1      EQU  H'0001'
SSP1DATPPS2      EQU  H'0002'
SSP1DATPPS3      EQU  H'0003'
SSP1DATPPS4      EQU  H'0004'


;----- SSP1SSPPS Bits -----------------------------------------------------
SSP1SSPPS0       EQU  H'0000'
SSP1SSPPS1       EQU  H'0001'
SSP1SSPPS2       EQU  H'0002'
SSP1SSPPS3       EQU  H'0003'
SSP1SSPPS4       EQU  H'0004'


;----- RX1PPS Bits -----------------------------------------------------
RX1DTPPS0        EQU  H'0000'
RX1DTPPS1        EQU  H'0001'
RX1DTPPS2        EQU  H'0002'
RX1DTPPS3        EQU  H'0003'
RX1DTPPS4        EQU  H'0004'


;----- CK1PPS Bits -----------------------------------------------------
TX1CKPPS0        EQU  H'0000'
TX1CKPPS1        EQU  H'0001'
TX1CKPPS2        EQU  H'0002'
TX1CKPPS3        EQU  H'0003'
TX1CKPPS4        EQU  H'0004'


;----- RA0PPS Bits -----------------------------------------------------
RA0PPS0          EQU  H'0000'
RA0PPS1          EQU  H'0001'
RA0PPS2          EQU  H'0002'
RA0PPS3          EQU  H'0003'
RA0PPS4          EQU  H'0004'
RA0PPS5          EQU  H'0005'


;----- RA1PPS Bits -----------------------------------------------------
RA1PPS0          EQU  H'0000'
RA1PPS1          EQU  H'0001'
RA1PPS2          EQU  H'0002'
RA1PPS3          EQU  H'0003'
RA1PPS4          EQU  H'0004'
RA1PPS5          EQU  H'0005'


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


;----- RB1PPS Bits -----------------------------------------------------
RB1PPS0          EQU  H'0000'
RB1PPS1          EQU  H'0001'
RB1PPS2          EQU  H'0002'
RB1PPS3          EQU  H'0003'
RB1PPS4          EQU  H'0004'
RB1PPS5          EQU  H'0005'


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


;----- RC1PPS Bits -----------------------------------------------------
RC1PPS0          EQU  H'0000'
RC1PPS1          EQU  H'0001'
RC1PPS2          EQU  H'0002'
RC1PPS3          EQU  H'0003'
RC1PPS4          EQU  H'0004'
RC1PPS5          EQU  H'0005'


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


;----- RD0PPS Bits -----------------------------------------------------
RD0PPS0          EQU  H'0000'
RD0PPS1          EQU  H'0001'
RD0PPS2          EQU  H'0002'
RD0PPS3          EQU  H'0003'
RD0PPS4          EQU  H'0004'
RD0PPS5          EQU  H'0005'


;----- RD1PPS Bits -----------------------------------------------------
RD1PPS0          EQU  H'0000'
RD1PPS1          EQU  H'0001'
RD1PPS2          EQU  H'0002'
RD1PPS3          EQU  H'0003'
RD1PPS4          EQU  H'0004'
RD1PPS5          EQU  H'0005'


;----- RD2PPS Bits -----------------------------------------------------
RD2PPS0          EQU  H'0000'
RD2PPS1          EQU  H'0001'
RD2PPS2          EQU  H'0002'
RD2PPS3          EQU  H'0003'
RD2PPS4          EQU  H'0004'
RD2PPS5          EQU  H'0005'


;----- RD3PPS Bits -----------------------------------------------------
RD3PPS0          EQU  H'0000'
RD3PPS1          EQU  H'0001'
RD3PPS2          EQU  H'0002'
RD3PPS3          EQU  H'0003'
RD3PPS4          EQU  H'0004'
RD3PPS5          EQU  H'0005'


;----- RD4PPS Bits -----------------------------------------------------
RD4PPS0          EQU  H'0000'
RD4PPS1          EQU  H'0001'
RD4PPS2          EQU  H'0002'
RD4PPS3          EQU  H'0003'
RD4PPS4          EQU  H'0004'
RD4PPS5          EQU  H'0005'


;----- RD5PPS Bits -----------------------------------------------------
RD5PPS0          EQU  H'0000'
RD5PPS1          EQU  H'0001'
RD5PPS2          EQU  H'0002'
RD5PPS3          EQU  H'0003'
RD5PPS4          EQU  H'0004'
RD5PPS5          EQU  H'0005'


;----- RD6PPS Bits -----------------------------------------------------
RD6PPS0          EQU  H'0000'
RD6PPS1          EQU  H'0001'
RD6PPS2          EQU  H'0002'
RD6PPS3          EQU  H'0003'
RD6PPS4          EQU  H'0004'
RD6PPS5          EQU  H'0005'


;----- RD7PPS Bits -----------------------------------------------------
RD7PPS0          EQU  H'0000'
RD7PPS1          EQU  H'0001'
RD7PPS2          EQU  H'0002'
RD7PPS3          EQU  H'0003'
RD7PPS4          EQU  H'0004'
RD7PPS5          EQU  H'0005'


;----- RE0PPS Bits -----------------------------------------------------
RE0PPS0          EQU  H'0000'
RE0PPS1          EQU  H'0001'
RE0PPS2          EQU  H'0002'
RE0PPS3          EQU  H'0003'
RE0PPS4          EQU  H'0004'
RE0PPS5          EQU  H'0005'


;----- RE1PPS Bits -----------------------------------------------------
RE1PPS0          EQU  H'0000'
RE1PPS1          EQU  H'0001'
RE1PPS2          EQU  H'0002'
RE1PPS3          EQU  H'0003'
RE1PPS4          EQU  H'0004'
RE1PPS5          EQU  H'0005'


;----- RE2PPS Bits -----------------------------------------------------
RE2PPS0          EQU  H'0000'
RE2PPS1          EQU  H'0001'
RE2PPS2          EQU  H'0002'
RE2PPS3          EQU  H'0003'
RE2PPS4          EQU  H'0004'
RE2PPS5          EQU  H'0005'


;----- ANSELA Bits -----------------------------------------------------
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


;----- ANSELD Bits -----------------------------------------------------
ANSD0            EQU  H'0000'
ANSD1            EQU  H'0001'
ANSD2            EQU  H'0002'
ANSD3            EQU  H'0003'
ANSD4            EQU  H'0004'
ANSD5            EQU  H'0005'
ANSD6            EQU  H'0006'
ANSD7            EQU  H'0007'


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


;----- IOCDP Bits -----------------------------------------------------
IOCDP0           EQU  H'0000'
IOCDP1           EQU  H'0001'
IOCDP2           EQU  H'0002'
IOCDP3           EQU  H'0003'
IOCDP4           EQU  H'0004'
IOCDP5           EQU  H'0005'
IOCDP6           EQU  H'0006'
IOCDP7           EQU  H'0007'


;----- IOCDN Bits -----------------------------------------------------
IOCDN0           EQU  H'0000'
IOCDN1           EQU  H'0001'
IOCDN2           EQU  H'0002'
IOCDN3           EQU  H'0003'
IOCDN4           EQU  H'0004'
IOCDN5           EQU  H'0005'
IOCDN6           EQU  H'0006'
IOCDN7           EQU  H'0007'


;----- IOCDF Bits -----------------------------------------------------
IOCDF0           EQU  H'0000'
IOCDF1           EQU  H'0001'
IOCDF2           EQU  H'0002'
IOCDF3           EQU  H'0003'
IOCDF4           EQU  H'0004'
IOCDF5           EQU  H'0005'
IOCDF6           EQU  H'0006'
IOCDF7           EQU  H'0007'


;----- ANSELE Bits -----------------------------------------------------
ANSE0            EQU  H'0000'
ANSE1            EQU  H'0001'
ANSE2            EQU  H'0002'


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
IOCEP0           EQU  H'0000'
IOCEP1           EQU  H'0001'
IOCEP2           EQU  H'0002'
IOCEP3           EQU  H'0003'


;----- IOCEN Bits -----------------------------------------------------
IOCEN0           EQU  H'0000'
IOCEN1           EQU  H'0001'
IOCEN2           EQU  H'0002'
IOCEN3           EQU  H'0003'


;----- IOCEF Bits -----------------------------------------------------
IOCEF0           EQU  H'0000'
IOCEF1           EQU  H'0001'
IOCEF2           EQU  H'0002'
IOCEF3           EQU  H'0003'


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

       __MAXRAM  H'1FFF'
       __BADRAM  H'0011'
       __BADRAM  H'0017'
       __BADRAM  H'001E'-H'0020'
       __BADRAM  H'008C'-H'0099'
       __BADRAM  H'0110'-H'0118'
       __BADRAM  H'0193'-H'019F'
       __BADRAM  H'0212'-H'021F'
       __BADRAM  H'0292'-H'029F'
       __BADRAM  H'0317'
       __BADRAM  H'031B'-H'031F'
       __BADRAM  H'038C'-H'039F'
       __BADRAM  H'040C'-H'041F'
       __BADRAM  H'048C'-H'049F'
       __BADRAM  H'050C'-H'051F'
       __BADRAM  H'058C'-H'059B'
       __BADRAM  H'060C'-H'061F'
       __BADRAM  H'068C'-H'069F'
       __BADRAM  H'070F'-H'0715'
       __BADRAM  H'0719'-H'071F'
       __BADRAM  H'078C'-H'079F'
       __BADRAM  H'080E'-H'0811'
       __BADRAM  H'0812'
       __BADRAM  H'0815'-H'0819'
       __BADRAM  H'088C'-H'088D'
       __BADRAM  H'088F'
       __BADRAM  H'0894'-H'089F'
       __BADRAM  H'090E'-H'0920'
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
       __BADRAM  H'1C8C'-H'1C9F'
       __BADRAM  H'1E0C'-H'1E1F'
       __BADRAM  H'1E8C'-H'1EEF'
       __BADRAM  H'1E0C'-H'1E6F'
       __BADRAM  H'1E8C'-H'1E8E'
       __BADRAM  H'1E94'-H'1E9B'
       __BADRAM  H'1E9E'-H'1EA1'
       __BADRAM  H'1EA3'-H'1EC2'
       __BADRAM  H'1EC4'
       __BADRAM  H'1EC8'-H'1ECA'
       __BADRAM  H'1ECE'-H'1EF0'
       __BADRAM  H'1F0C'-H'1F0F'
       __BADRAM  H'1F33'-H'1F37'
       __BADRAM  H'1F40'-H'1F42'
       __BADRAM  H'1F4B'-H'1F4D'
       __BADRAM  H'1F56'-H'1F58'
       __BADRAM  H'1F61'-H'1F63'
       __BADRAM  H'1F6C'-H'1F6F'
       __BADRAM  H'1F8C'-H'1FB7'
       __BADRAM  H'1FB8'-H'1FE3'
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
             ; C:\Users\admin\.mchp_packs\Microchip\PIC16F1xxxx_DFP\1.8.149\xc8\pic\dat\cfgdata\16F15275.cfgdata
;==========================================================================

;----- CONFIG1 Options --------------------------------------------------
_FEXTOSC_ECH         EQU  H'3FFF'; EC (external clock) 16 MHz and above
_FEXTOSC_ECL         EQU  H'3FFE'; EC (external clock) below 16MHz
_FEXTOSC_OFF         EQU  H'3FFD'; Oscillator not enabled
_RSTOSC_EXTOSC       EQU  H'3FFF'; EXTOSC operating per FEXTOSC bits
_RSTOSC_HFINTOSC_1MHZ EQU  H'3FEF'; HFINTOSC (1 MHz)
_RSTOSC_LFINTOSC     EQU  H'3FDF'; LFINTOSC
_RSTOSC_HFINTOSC_32MHZ EQU  H'3FCF'; HFINTOSC (32 MHz)
_CLKOUTEN_ON         EQU  H'3EFF'; CLKOUT function is enabled; FOSC/4 clock appears on RA6
_CLKOUTEN_OFF        EQU  H'3FFF'; CLKOUT function is disabled; I/O function on RA6
_VDDAR_HI            EQU  H'3FFF'; Internal analog systems are calibrated for operation between VDD = 2.3V - 5.5V
_VDDAR_LO            EQU  H'2FFF'; Internal analog systems are calibrated for operation between VDD = 1.8V - 3.6V

;----- CONFIG2 Options --------------------------------------------------
_MCLRE_EXTMCLR       EQU  H'3FFF'; If LVP = 0, MCLR pin is MCLR; If LVP = 1, RE3 pin function is MCLR
_MCLRE_INTMCLR       EQU  H'3FFE'; If LVP = 0, MCLR pin is port defined function; If LVP = 1, RE3 pin function is MCLR
_PWRTS_PWRT_OFF      EQU  H'3FFF'; PWRT is disabled
_PWRTS_PWRT_64       EQU  H'3FFD'; PWRT set at 64 ms
_PWRTS_PWRT_16       EQU  H'3FFB'; PWRT set at 16 ms
_PWRTS_PWRT_1        EQU  H'3FF9'; PWRT set at 1 ms
_WDTE_ON             EQU  H'3FFF'; WDT enabled regardless of Sleep; SEN bit is ignored
_WDTE_NSLEEP         EQU  H'3FF7'; WDT enabled while Sleep = 0, suspended when Sleep = 1; SEN is ignored
_WDTE_SWDTEN         EQU  H'3FEF'; WDT enabled/disabled by SEN bit
_WDTE_OFF            EQU  H'3FE7'; WDT disabled; SEN is ignored
_BOREN_ON            EQU  H'3FFF'; Brown-out Reset Enabled, SBOREN bit is ignored
_BOREN_NSLEEP        EQU  H'3FBF'; Brown-out Reset enabled while running, disabled in Sleep; SBOREN is ignored
_BOREN_SBOREN        EQU  H'3F7F'; Brown-out Reset enabled according to SBOREN bit
_BOREN_OFF           EQU  H'3F3F'; Brown-out Reset disabled
_BORV_LO             EQU  H'3FFF'; Brown-out Reset Voltage (VBOR) set to 1.9V
_BORV_HI             EQU  H'3DFF'; Brown-out Reset Voltage (VBOR) is set to 2.85V
_PPS1WAY_ON          EQU  H'3FFF'; The PPSLOCKED bit can be set once after an unlocking sequence is executed; once PPSLOCKED is set, all future changes to PPS registers are prevented
_PPS1WAY_OFF         EQU  H'37FF'; The PPSLOCKED bit can be set and cleared as needed (unlocking sequence is required)
_STVREN_ON           EQU  H'3FFF'; Stack Overflow or Underflow will cause a reset
_STVREN_OFF          EQU  H'2FFF'; Stack Overflow or Underflow will not cause a reset

;----- CONFIG3 Options --------------------------------------------------

;----- CONFIG4 Options --------------------------------------------------
_BBSIZE_BB512        EQU  H'3FFF'; 512 words boot block size
_BBSIZE_BB1K         EQU  H'3FFE'; 1024 words boot block size
_BBSIZE_BB2K         EQU  H'3FFD'; 2048 words boot block size
_BBSIZE_BB4K         EQU  H'3FFC'; 4096 words boot block size
_BBSIZE_BB8K         EQU  H'3FFB'; * half of user program memory
_BBSIZE_BB16K        EQU  H'3FFA'; * half of user program memory
_BBSIZE_BB32K        EQU  H'3FF9'; * half of user program memory
_BBSIZE_BB64K        EQU  H'3FF8'; * half of user program memory
_BBEN_OFF            EQU  H'3FFF'; Boot Block is disabled
_BBEN_ON             EQU  H'3FF7'; Boot Block is enabled
_SAFEN_OFF           EQU  H'3FFF'; SAF is disabled
_SAFEN_ON            EQU  H'3FEF'; SAF is enabled
_WRTAPP_OFF          EQU  H'3FFF'; Application Block is not write-protected
_WRTAPP_ON           EQU  H'3F7F'; Application Block is write-protected
_WRTB_OFF            EQU  H'3FFF'; Boot Block is not write-protected
_WRTB_ON             EQU  H'3EFF'; Boot Block is write-protected
_WRTC_OFF            EQU  H'3FFF'; Configuration Registers are not write-protected
_WRTC_ON             EQU  H'3DFF'; Configuration Registers are write-protected
_WRTSAF_OFF          EQU  H'3FFF'; SAF is not write-protected
_WRTSAF_ON           EQU  H'37FF'; SAF is write-protected
_LVP_ON              EQU  H'3FFF'; Low Voltage programming enabled. MCLR/Vpp pin function is MCLR. MCLRE Configuration bit is ignored.
_LVP_OFF             EQU  H'1FFF'; High Voltage on MCLR/Vpp must be used for programming

;----- CONFIG5 Options --------------------------------------------------
_CP_OFF              EQU  H'3FFF'; User Program Flash Memory code protection is disabled
_CP_ON               EQU  H'3FFE'; User Program Flash Memory code protection is enabled

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
