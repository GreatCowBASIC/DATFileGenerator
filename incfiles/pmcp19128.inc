        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP19128 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP19128 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP19128
;       2. LIST directive in the source file
;               LIST   P=MCP19128
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP19128
           MESSG "Processor-header file mismatch.  Verify selected processor."
        ENDIF



;==========================================================================
;
;       Register Definitions
;
;==========================================================================

W                EQU  H'0000'
F                EQU  H'0001'

;----- Register Files -----------------------------------------------------

;-----Bank0------------------
INDF             EQU  H'0000'
TMR0             EQU  H'0001'
PCL              EQU  H'0002'
STATUS           EQU  H'0003'
FSR              EQU  H'0004'
PORTA            EQU  H'0005'
PORTGPA          EQU  H'0005'
PORTB            EQU  H'0006'
PORTGPB          EQU  H'0006'
PIR1             EQU  H'0007'
PIR2             EQU  H'0008'
PIR3             EQU  H'0009'
PCLATH           EQU  H'000A'
INTCON           EQU  H'000B'
TMR1             EQU  H'000C'
TMR1L            EQU  H'000C'
TMR1H            EQU  H'000D'
T1GCON           EQU  H'000E'
TMR2             EQU  H'000F'
PR2              EQU  H'0010'
TMR4             EQU  H'0011'
PR4              EQU  H'0012'
TCON             EQU  H'0013'
PCON             EQU  H'0014'
PWM0PH           EQU  H'0015'
PWM0R            EQU  H'0016'
PWM1R            EQU  H'0017'
PWM2R            EQU  H'0018'
PWM3R            EQU  H'0019'
PWMCON           EQU  H'001A'
OSCCON           EQU  H'001B'
ADRESL           EQU  H'001C'
ADRESH           EQU  H'001D'
ADCON0           EQU  H'001E'
ADCON1           EQU  H'001F'

;-----Bank1------------------
OPTION_REG       EQU  H'0081'
TRISA            EQU  H'0085'
TRISGPA          EQU  H'0085'
TRISGPB          EQU  H'0086'
PIE1             EQU  H'0087'
PIE2             EQU  H'0088'
PIE3             EQU  H'0089'
INTEDGE1         EQU  H'008C'
INTEDGE2         EQU  H'008D'
MODECON1         EQU  H'008E'
MODECON2         EQU  H'008F'
EACON            EQU  H'0090'
SENSECON         EQU  H'0091'
DEADCON1         EQU  H'0092'
DEADCON2         EQU  H'0093'
REFCON1          EQU  H'0094'
REFCON2          EQU  H'0095'
REFCON3          EQU  H'0096'
REFCON4          EQU  H'0097'
V1REF            EQU  H'0098'
V2REF            EQU  H'0099'
I1REF            EQU  H'009A'
I2REF            EQU  H'009B'
SLPCRCON         EQU  H'009C'
SSCON            EQU  H'009D'
ABECON           EQU  H'009E'
OVCON            EQU  H'009F'

;-----Bank2------------------
WPUGPA           EQU  H'0105'
WPUGPB           EQU  H'0106'
BAUDCON          EQU  H'0107'
SPBRGL           EQU  H'0108'
SPBRGH           EQU  H'0109'
RCREG            EQU  H'010C'
TXREG            EQU  H'010D'
TXSTA            EQU  H'010E'
RCSTA            EQU  H'010F'
SSPADD           EQU  H'0110'
SSPBUF           EQU  H'0111'
SSPCON1          EQU  H'0112'
SSPCON2          EQU  H'0113'
SSPCON3          EQU  H'0114'
SSPMSK           EQU  H'0115'
SSPSTAT          EQU  H'0116'
SSPADD2          EQU  H'0117'
SSPMSK2          EQU  H'0118'
CC1RL            EQU  H'0119'
CC1RH            EQU  H'011A'
CC2RL            EQU  H'011B'
CC2RH            EQU  H'011C'
CCDCON           EQU  H'011D'

;-----Bank3------------------
IOCA             EQU  H'0185'
IOCB             EQU  H'0186'
ANSEL            EQU  H'0187'
TMUXCON0         EQU  H'0188'
TMUXCON1         EQU  H'0189'
PORTICD          EQU  H'018C'
TRISICD          EQU  H'018D'
ICKBUG           EQU  H'018E'
BIGBUG           EQU  H'018F'
PMCON1           EQU  H'0190'
PMCON2           EQU  H'0191'
PMADR            EQU  H'0192'
PMADRL           EQU  H'0192'
PMADRH           EQU  H'0193'
PMADATL          EQU  H'0194'
PMDAT            EQU  H'0194'
PMADATH          EQU  H'0195'
CALCON1          EQU  H'0196'
CALCON2          EQU  H'0197'
CALADR           EQU  H'0198'
CALDATWR         EQU  H'0199'
CALDATRD         EQU  H'019A'
ATSTCON0         EQU  H'019B'
ATSTCON1         EQU  H'019C'

;----- PIR1 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
TIMR4IF          EQU  H'0002'
TIMR1GIF         EQU  H'0003'
SSPIF            EQU  H'0004'
BCLIF            EQU  H'0005'
ADIF             EQU  H'0006'
OTIF             EQU  H'0007'


;----- PIE1 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
TMR4IE           EQU  H'0002'
TMR1GIE          EQU  H'0003'
SSPIE            EQU  H'0004'
BCLIE            EQU  H'0005'
ADIE             EQU  H'0006'
OTIE             EQU  H'0007'


;----- INTEDGE1 Bits -----------------------------------------------------
VINUVINTN        EQU  H'0000'
VINUVINTP        EQU  H'0001'
LDINTN           EQU  H'0002'
LDINTP           EQU  H'0003'
DRUVINTN         EQU  H'0004'
DRUVINTP         EQU  H'0005'


;----- MODECON1 Bits -----------------------------------------------------
PDIR             EQU  H'0000'
CCLAMP           EQU  H'0001'
TOPO0            EQU  H'0002'
TOPO1            EQU  H'0003'
MSSYNC           EQU  H'0004'
MSCLK            EQU  H'0005'
MSSEL            EQU  H'0006'
BYPASS           EQU  H'0007'



;----- REFCON1 Bits -----------------------------------------------------
I1REFEN          EQU  H'0000'
I2REFEN          EQU  H'0001'
V1REFEN          EQU  H'0002'
V2REFEN          EQU  H'0003'
V1UVEN           EQU  H'0004'
V1OVEN           EQU  H'0005'
V2UVEN           EQU  H'0006'
V2OVEN           EQU  H'0007'


;----- SSPCON1 Bits -----------------------------------------------------
SSPM3            EQU  H'0003'
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'


;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
NOT_PD           EQU  H'0003'
NOT_TO           EQU  H'0004'
IRP              EQU  H'0007'

RP0              EQU  H'0005'
RP1              EQU  H'0006'


;----- PORTA Bits -----------------------------------------------------
GPA0             EQU  H'0000'
GPA1             EQU  H'0001'
GPA2             EQU  H'0002'
GPA3             EQU  H'0003'
GPA4             EQU  H'0004'
GPA5             EQU  H'0005'
GPA6             EQU  H'0006'
GPA7             EQU  H'0007'

GPIO0            EQU  H'0000'
GPIO1            EQU  H'0001'
GPIO2            EQU  H'0002'
GPIO3            EQU  H'0003'
GPIO4            EQU  H'0004'
GPIO5            EQU  H'0005'
GPIO6            EQU  H'0006'
GPIO7            EQU  H'0007'

RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'
RA4              EQU  H'0004'
RA5              EQU  H'0005'
RA6              EQU  H'0006'
RA7              EQU  H'0007'


;----- PORTGPA Bits -----------------------------------------------------
GPA0             EQU  H'0000'
GPA1             EQU  H'0001'
GPA2             EQU  H'0002'
GPA3             EQU  H'0003'
GPA4             EQU  H'0004'
GPA5             EQU  H'0005'
GPA6             EQU  H'0006'
GPA7             EQU  H'0007'

GPIO0            EQU  H'0000'
GPIO1            EQU  H'0001'
GPIO2            EQU  H'0002'
GPIO3            EQU  H'0003'
GPIO4            EQU  H'0004'
GPIO5            EQU  H'0005'
GPIO6            EQU  H'0006'
GPIO7            EQU  H'0007'

RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'
RA4              EQU  H'0004'
RA5              EQU  H'0005'
RA6              EQU  H'0006'
RA7              EQU  H'0007'


;----- PORTB Bits -----------------------------------------------------
GPB0             EQU  H'0000'
GPB1             EQU  H'0001'
GPB2             EQU  H'0002'
GPB3             EQU  H'0003'
GPB4             EQU  H'0004'
GPB5             EQU  H'0005'
GPB6             EQU  H'0006'

RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PORTGPB Bits -----------------------------------------------------
GPB0             EQU  H'0000'
GPB1             EQU  H'0001'
GPB2             EQU  H'0002'
GPB3             EQU  H'0003'
GPB4             EQU  H'0004'
GPB5             EQU  H'0005'
GPB6             EQU  H'0006'

RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
V1UVIF           EQU  H'0000'
V1OVIF           EQU  H'0001'
V2UVIF           EQU  H'0002'
V2OVIF           EQU  H'0003'
LDIF             EQU  H'0004'
BTUVIF           EQU  H'0005'
OCIF             EQU  H'0006'


;----- PIR3 Bits -----------------------------------------------------
TXIF             EQU  H'0000'
RCIF             EQU  H'0001'
CC1IF            EQU  H'0002'
CC2IF            EQU  H'0003'
SSIF             EQU  H'0004'
VINUVIF          EQU  H'0005'
VDDUIF           EQU  H'0006'
DRVUVIF          EQU  H'0007'


;----- INTCON Bits -----------------------------------------------------
IOCF             EQU  H'0000'
INTF             EQU  H'0001'
T0IF             EQU  H'0002'
IOCE             EQU  H'0003'
INTE             EQU  H'0004'
T0IE             EQU  H'0005'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- T1GCON Bits -----------------------------------------------------
T1GSS0           EQU  H'0000'
T1GSS1           EQU  H'0001'
T1GVAL           EQU  H'0002'
T1GO_DONE        EQU  H'0003'
T1GSPM           EQU  H'0004'
T1GTM            EQU  H'0005'
T1GPOL           EQU  H'0006'
TMR1GE           EQU  H'0007'



;----- TCON Bits -----------------------------------------------------
TMR1ON           EQU  H'0000'
TMR2ON           EQU  H'0001'
TMR4ON           EQU  H'0002'
TMR1CS           EQU  H'0003'
T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'
T4CKPS0          EQU  H'0006'
T4CKPS1          EQU  H'0007'



;----- PCON Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'
LDO_LP           EQU  H'0004'
LDO_LV           EQU  H'0005'


;----- PWMCON Bits -----------------------------------------------------
PWM1EN           EQU  H'0000'
PWM2EN           EQU  H'0001'
PWM3EN           EQU  H'0002'


;----- OSCCON Bits -----------------------------------------------------
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'
OSCSEL0          EQU  H'0005'
OSCSEL1          EQU  H'0006'
OSCSTB           EQU  H'0007'



;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'
CHS0             EQU  H'0002'
CHS1             EQU  H'0003'
CHS2             EQU  H'0004'
CHS3             EQU  H'0005'
CHS4             EQU  H'0006'
CHS5             EQU  H'0007'



;----- ADCON1 Bits -----------------------------------------------------
VCFG0            EQU  H'0000'
VCFG1            EQU  H'0001'
ADFM             EQU  H'0002'
ADCS0            EQU  H'0004'
ADCS1            EQU  H'0005'
ADCS2            EQU  H'0006'



;----- OPTION_REG Bits -----------------------------------------------------
PSA              EQU  H'0003'
T0SE             EQU  H'0004'
T0CS             EQU  H'0005'
INTEDG           EQU  H'0006'
NOT_RAPU         EQU  H'0007'

PS0              EQU  H'0000'
PS1              EQU  H'0001'
PS2              EQU  H'0002'


;----- TRISA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA3           EQU  H'0003'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'
TRISA6           EQU  H'0006'
TRISA7           EQU  H'0007'


;----- TRISGPA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA3           EQU  H'0003'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'
TRISA6           EQU  H'0006'
TRISA7           EQU  H'0007'


;----- TRISGPB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB2           EQU  H'0002'
TRISB3           EQU  H'0003'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'


;----- PIE2 Bits -----------------------------------------------------
V1UVIE           EQU  H'0000'
V1OVIE           EQU  H'0001'
V2UVIE           EQU  H'0002'
V2OVIE           EQU  H'0003'
LDIE             EQU  H'0004'
BTUVIE           EQU  H'0005'
OCIE             EQU  H'0006'


;----- PIE3 Bits -----------------------------------------------------
TXIE             EQU  H'0000'
RCIE             EQU  H'0001'
CC1IE            EQU  H'0002'
CC2IE            EQU  H'0003'
SSIE             EQU  H'0004'
VINUVIE          EQU  H'0005'
VDDUVIE          EQU  H'0006'
DRUVIE           EQU  H'0007'


;----- INTEDGE2 Bits -----------------------------------------------------
V1UVINTN         EQU  H'0000'
V1UVINTP         EQU  H'0001'
V1OVINTN         EQU  H'0002'
V1OVINTP         EQU  H'0003'
V2UVINTN         EQU  H'0004'
V2UVINTP         EQU  H'0005'
V2OVINTN         EQU  H'0006'
V2OVINTP         EQU  H'0007'


;----- MODECON2 Bits -----------------------------------------------------
BBRO             EQU  H'0005'
BUKDMBY          EQU  H'0006'
SKIP             EQU  H'0007'


;----- EACON Bits -----------------------------------------------------
I1EAF            EQU  H'0000'
I1EAR            EQU  H'0001'
I2EAF            EQU  H'0002'
I2EAR            EQU  H'0003'
V1EAF            EQU  H'0004'
V1EAR            EQU  H'0005'
V2EAF            EQU  H'0006'
V2EAR            EQU  H'0007'


;----- REFCON2 Bits -----------------------------------------------------
VINUVEN          EQU  H'0007'


;----- SLPCRCON Bits -----------------------------------------------------
SLPBY            EQU  H'0006'


;----- SSCON Bits -----------------------------------------------------
SSEN             EQU  H'0007'


;----- ABECON Bits -----------------------------------------------------
LD1EN            EQU  H'0000'
HD1EN            EQU  H'0001'
LD2EN            EQU  H'0002'
HD2EN            EQU  H'0003'
DEL1EN           EQU  H'0004'
DEH1EN           EQU  H'0005'
DEL2EN           EQU  H'0006'
DEH2EN           EQU  H'0007'


;----- WPUGPA Bits -----------------------------------------------------
WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WCS0             EQU  H'0002'
WCS1             EQU  H'0003'
WPUA5            EQU  H'0005'
WPUA6            EQU  H'0006'


;----- WPUGPB Bits -----------------------------------------------------
WPUB1            EQU  H'0001'
WPUB2            EQU  H'0002'
WPUB3            EQU  H'0003'
WPUB4            EQU  H'0004'
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'


;----- BAUDCON Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- TXSTA Bits -----------------------------------------------------
TX9D             EQU  H'0000'
TRMT             EQU  H'0001'
BRGH             EQU  H'0002'
SENDB            EQU  H'0003'
SYNC             EQU  H'0004'
TXEN             EQU  H'0005'
TX9              EQU  H'0006'
CSRC             EQU  H'0007'


;----- RCSTA Bits -----------------------------------------------------
RX9D             EQU  H'0000'
OERR             EQU  H'0001'
FERR             EQU  H'0002'
ADDEN            EQU  H'0003'
CREN             EQU  H'0004'
SREN             EQU  H'0005'
RX9              EQU  H'0006'
SPEN             EQU  H'0007'


;----- SSPCON2 Bits -----------------------------------------------------
SEN              EQU  H'0000'
RSEN             EQU  H'0001'
PEN              EQU  H'0002'
RCEN             EQU  H'0003'
ACKEN            EQU  H'0004'
ACKDT            EQU  H'0005'
ACKSTAT          EQU  H'0006'
GCEN             EQU  H'0007'


;----- SSPCON3 Bits -----------------------------------------------------
DHEN             EQU  H'0000'
AHEN             EQU  H'0001'
SBCDE            EQU  H'0002'
SDAHT            EQU  H'0003'
BOEN             EQU  H'0004'
SCIE             EQU  H'0005'
PCIE             EQU  H'0006'
ACKTIM           EQU  H'0007'


;----- SSPSTAT Bits -----------------------------------------------------
BF               EQU  H'0000'
UA               EQU  H'0001'
R_NOT_W          EQU  H'0002'
S                EQU  H'0003'
P                EQU  H'0004'
D_NOT_A          EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'


;----- IOCA Bits -----------------------------------------------------
IOCA0            EQU  H'0000'
IOCA1            EQU  H'0001'
IOCA2            EQU  H'0002'
IOCA3            EQU  H'0003'
IOCA4            EQU  H'0004'
IOCA5            EQU  H'0005'
IOCA6            EQU  H'0006'
IOCA7            EQU  H'0007'


;----- IOCB Bits -----------------------------------------------------
IOCB0            EQU  H'0000'
IOCB1            EQU  H'0001'
IOCB2            EQU  H'0002'
IOCB3            EQU  H'0003'
IOCB4            EQU  H'0004'
IOCB5            EQU  H'0005'
IOCB6            EQU  H'0006'


;----- ANSEL Bits -----------------------------------------------------
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA3            EQU  H'0003'
ANSB3            EQU  H'0004'
ANSB4            EQU  H'0005'
ANSB5            EQU  H'0006'
ANSB6            EQU  H'0007'


;----- TMUXCON0 Bits -----------------------------------------------------
GCTRL            EQU  H'0007'


;----- TMUXCON1 Bits -----------------------------------------------------
DMUX0OUT         EQU  H'0005'
DMUX1OUT         EQU  H'0006'


;----- PMCON1 Bits -----------------------------------------------------
RD               EQU  H'0000'
WR               EQU  H'0001'
WREN             EQU  H'0002'
CALSEL           EQU  H'0006'


;----- CALCON1 Bits -----------------------------------------------------
WR               EQU  H'0001'
WREN             EQU  H'0002'


;----- ATSTCON0 Bits -----------------------------------------------------
TMPTBY           EQU  H'0000'
DRUVBY           EQU  H'0001'
BGCHOP           EQU  H'0002'
HSIAHIGHGAIN     EQU  H'0003'
SWFRQOR          EQU  H'0004'
TSTOT            EQU  H'0005'
BRGBY            EQU  H'0006'
TSTGM            EQU  H'0007'


;----- ATSTCON1 Bits -----------------------------------------------------
DEADTBY          EQU  H'0002'




;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'01FF'
       __BADRAM  H'011E'
       __BADRAM  H'011F'
       __BADRAM  H'019D'-H'019F'
       __BADRAM  H'01A0'-H'01EF'

;==========================================================================
;
;       Configuration Bits
;
;   NAME            Address
;   CONFIG            2007h
;
;==========================================================================

; The following is an assignment of address values for all of the
; configuration registers for the purpose of table reads
_CONFIG         EQU  H'2007'

;----- CONFIG Options --------------------------------------------------
_WDTE_OFF            EQU  H'3FF7'; WDT disabled and can be enabled by SWDTEN bit of the WDTCON register
_WDT_OFF             EQU  H'3FF7'; WDT disabled and can be enabled by SWDTEN bit of the WDTCON register
_WDTE_ON             EQU  H'3FFF'; WDT enabled
_WDT_ON              EQU  H'3FFF'; WDT enabled

_PWRTE_ON            EQU  H'3FEF'; PWRT enabled
_PWRTE_OFF           EQU  H'3FFF'; PWRT disabled

_MCLRE_OFF           EQU  H'3FDF'; MCLR pin is alternate function, MCLR function is internally disabled
_MCLRE_ON            EQU  H'3FFF'; MCLR pin is MCLR function and weak internal pull-up is enabled

_CP_ON               EQU  H'3FBF'; Program memory is external read and write protected
_CP_OFF              EQU  H'3FFF'; Program memory is not code protected

_BOREN_OFF           EQU  H'3EFF'; BOR disabled
_BOREN_ON            EQU  H'3FFF'; BOR disabled during sleep and enabled during operation

_WRT_ALL             EQU  H'33FF'; 0000h to 1FFFh write protected, entire program memory is write protected.
_WRT_HALF            EQU  H'37FF'; 000h to 7FFh write protected, 800h to FFFh may be modified by PMCON1 control
_WRT_BOOT            EQU  H'3BFF'; 000h to 3FFh write protected, 400h to FFFh may be modified by PMCON1 control
_WRT_OFF             EQU  H'3FFF'; Write protection off

;----- DEVID Equates --------------------------------------------------
_DEVID1          EQU  H'2006'

;----- IDLOC Equates --------------------------------------------------
_IDLOC0          EQU  H'2000'
_IDLOC1          EQU  H'2001'
_IDLOC2          EQU  H'2002'
_IDLOC3          EQU  H'2003'

        LIST
