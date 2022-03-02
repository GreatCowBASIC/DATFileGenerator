        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP19123 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP19123 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP19123
;       2. LIST directive in the source file
;               LIST   P=MCP19123
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP19123
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
PCON             EQU  H'0009'
PCLATH           EQU  H'000A'
INTCON           EQU  H'000B'
TMR1             EQU  H'000C'
TMR1L            EQU  H'000C'
TMR1H            EQU  H'000D'
T1CON            EQU  H'000E'
TMR2             EQU  H'000F'
T2CON            EQU  H'0010'
PR2              EQU  H'0011'
T1GCON           EQU  H'0012'
PWMPHL           EQU  H'0013'
PWMPHH           EQU  H'0014'
PWMRL            EQU  H'0015'
PWMRH            EQU  H'0016'
CC1RL            EQU  H'0017'
CC1RH            EQU  H'0018'
CC2RL            EQU  H'0019'
CC2RH            EQU  H'001A'
CCDCON           EQU  H'001B'
ADRES            EQU  H'001C'
ADRESL           EQU  H'001C'
ADRESH           EQU  H'001D'
ADCON0           EQU  H'001E'
ADCON1           EQU  H'001F'

;-----Bank1------------------
OPTION_REG       EQU  H'0081'
TRISA            EQU  H'0085'
TRISGPA          EQU  H'0085'
TRISB            EQU  H'0086'
TRISGPB          EQU  H'0086'
PIE1             EQU  H'0087'
PIE2             EQU  H'0088'
VINUVLO          EQU  H'0090'
VINOVLO          EQU  H'0091'
VINCON           EQU  H'0092'
DAGCON           EQU  H'0093'
VOUT             EQU  H'0094'
VOUTL            EQU  H'0094'
VOUTH            EQU  H'0095'
OSCTUNE          EQU  H'0096'
CMPZCON          EQU  H'0098'
VOTUVLO          EQU  H'0099'
VOTOVLO          EQU  H'009A'
DEADCON          EQU  H'009B'
RAMPCON          EQU  H'009C'
OCCON            EQU  H'009D'
CSGSCON          EQU  H'009E'
RELEFF           EQU  H'009F'

;-----Bank2------------------
WPUGPA           EQU  H'0105'
WPUGPB           EQU  H'0106'
PE1              EQU  H'0107'
MODECON          EQU  H'0108'
SSPADD           EQU  H'0110'
SSPBUF           EQU  H'0111'
SSPCON1          EQU  H'0112'
SSPCON2          EQU  H'0113'
SSPCON3          EQU  H'0114'
SSPMSK           EQU  H'0115'
SSPSTAT          EQU  H'0116'
SSPADD2          EQU  H'0117'
SSPMSK2          EQU  H'0118'
VREFCAL          EQU  H'0119'
VRFSCAL          EQU  H'011A'
RAMPCAL          EQU  H'011B'
CSRCAL           EQU  H'011C'
OVUVCAL          EQU  H'011D'
DEMCAL           EQU  H'011E'
HCSOVCAL         EQU  H'011F'

;-----Bank3------------------
IOCA             EQU  H'0185'
IOCB             EQU  H'0186'
ANSELA           EQU  H'0187'
ANSELB           EQU  H'0188'
PORTICD          EQU  H'018C'
TRISICD          EQU  H'018D'
ICKBUG           EQU  H'018E'
BIGBUG           EQU  H'018F'
PMCON1           EQU  H'0190'
PMCON2           EQU  H'0191'
PMADR            EQU  H'0192'
PMADRL           EQU  H'0192'
PMADRH           EQU  H'0193'
PMDAT            EQU  H'0194'
PMDATL           EQU  H'0194'
PMDATH           EQU  H'0195'
TTACAL           EQU  H'0196'
OSCCAL           EQU  H'0197'
BGTCAL           EQU  H'0198'
BGRCAL           EQU  H'0199'
AVDDCAL          EQU  H'019A'
VOURCAL          EQU  H'019B'
DOVCAL           EQU  H'019C'
VEAOCAL          EQU  H'019D'
BUFFCON          EQU  H'019E'
ATSTCON          EQU  H'019F'

;----- PIR1 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
CC1IF            EQU  H'0002'
CC2IF            EQU  H'0003'
SSPIF            EQU  H'0004'
BCLIF            EQU  H'0005'
ADIF             EQU  H'0006'
TMR1GIF          EQU  H'0007'


;----- T1CON Bits -----------------------------------------------------
TMR1ON           EQU  H'0000'
TMR1CS           EQU  H'0001'

T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'


;----- PIE1 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
CC1IE            EQU  H'0002'
CC2IE            EQU  H'0003'
SSPIE            EQU  H'0004'
BCLIE            EQU  H'0005'
ADIE             EQU  H'0006'
TMR1GIE          EQU  H'0007'


;----- SSPCON1 Bits -----------------------------------------------------
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'

SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'


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
GPB7             EQU  H'0007'

RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB3              EQU  H'0003'
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
GPB7             EQU  H'0007'

RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB3              EQU  H'0003'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
UVLOIF           EQU  H'0000'
OVLOIF           EQU  H'0001'
OVIF             EQU  H'0004'
OCIF             EQU  H'0005'
OTIF             EQU  H'0006'
UVIF             EQU  H'0007'


;----- PCON Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'


;----- INTCON Bits -----------------------------------------------------
IOCF             EQU  H'0000'
INTF             EQU  H'0001'
T0IF             EQU  H'0002'
IOCE             EQU  H'0003'
INTE             EQU  H'0004'
T0IE             EQU  H'0005'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- T2CON Bits -----------------------------------------------------
TMR2ON           EQU  H'0002'


;----- T1GCON Bits -----------------------------------------------------
T1GVAL           EQU  H'0002'
T1GGO_NOT_DONE   EQU  H'0003'
T1GSPM           EQU  H'0004'
T1GTM            EQU  H'0005'
T1GPOL           EQU  H'0006'
TMR1GE           EQU  H'0007'

T1GSS0           EQU  H'0000'
T1GSS1           EQU  H'0001'
T1GGO            EQU  H'0003'


;----- CCDCON Bits -----------------------------------------------------
CC1M0            EQU  H'0000'
CC1M1            EQU  H'0001'
CC1M2            EQU  H'0002'
CC1M3            EQU  H'0003'
CC2M0            EQU  H'0004'
CC2M1            EQU  H'0005'
CC2M2            EQU  H'0006'
CC2M3            EQU  H'0007'


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
ADFM             EQU  H'0002'

VCFG0            EQU  H'0000'
VCFG1            EQU  H'0001'
ADCS0            EQU  H'0004'
ADCS1            EQU  H'0005'
ADCS2            EQU  H'0006'


;----- OPTION_REG Bits -----------------------------------------------------
PSA              EQU  H'0003'
T0SE             EQU  H'0004'
T0CS             EQU  H'0005'
INTEDG           EQU  H'0006'
NOT_RAPUI        EQU  H'0007'

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


;----- TRISB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB2           EQU  H'0002'
TRISB3           EQU  H'0003'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'
TRISB7           EQU  H'0007'


;----- TRISGPB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB2           EQU  H'0002'
TRISB3           EQU  H'0003'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'
TRISB7           EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
UVLOIE           EQU  H'0000'
OVLOIE           EQU  H'0001'
OVIE             EQU  H'0004'
OCIE             EQU  H'0005'
OTIE             EQU  H'0006'
UVIE             EQU  H'0007'


;----- VINUVLO Bits -----------------------------------------------------
UVLO0            EQU  H'0000'
UVLO1            EQU  H'0001'
UVLO2            EQU  H'0002'
UVLO3            EQU  H'0003'


;----- VINOVLO Bits -----------------------------------------------------
OVLO0            EQU  H'0000'
OVLO1            EQU  H'0001'
OVLO2            EQU  H'0002'
OVLO3            EQU  H'0003'


;----- VINCON Bits -----------------------------------------------------
OVLOINTN         EQU  H'0000'
OVLOINTP         EQU  H'0001'
OVLOOUT          EQU  H'0002'
OVLOEN           EQU  H'0003'
UVLOINTN         EQU  H'0004'
UVLOINTP         EQU  H'0005'
UVLOOUT          EQU  H'0006'
UVLOEN           EQU  H'0007'


;----- DAGCON Bits -----------------------------------------------------
DAG0             EQU  H'0000'
DAG1             EQU  H'0001'
DAG2             EQU  H'0002'


;----- VOUTL Bits -----------------------------------------------------
VOUT0            EQU  H'0000'
VOUT1            EQU  H'0001'
VOUT2            EQU  H'0002'
VOUT3            EQU  H'0003'
VOUT4            EQU  H'0004'
VOUT5            EQU  H'0005'
VOUT6            EQU  H'0006'
VOUT7            EQU  H'0007'


;----- VOUTH Bits -----------------------------------------------------
VOUT8            EQU  H'0000'
VOUT9            EQU  H'0001'


;----- OSCTUNE Bits -----------------------------------------------------
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'


;----- CMPZCON Bits -----------------------------------------------------
CMPZG0           EQU  H'0000'
CMPZG1           EQU  H'0001'
CMPZG2           EQU  H'0002'
CMPZG3           EQU  H'0003'
CMPZF0           EQU  H'0004'
CMPZF1           EQU  H'0005'
CMPZF2           EQU  H'0006'
CMPZF3           EQU  H'0007'


;----- VOTUVLO Bits -----------------------------------------------------
OUV0             EQU  H'0000'
OUV1             EQU  H'0001'
OUV2             EQU  H'0002'
OUV3             EQU  H'0003'


;----- VOTOVLO Bits -----------------------------------------------------
OOV0             EQU  H'0000'
OOV1             EQU  H'0001'
OOV2             EQU  H'0002'
OOV3             EQU  H'0003'


;----- DEADCON Bits -----------------------------------------------------
LDLY0            EQU  H'0000'
LDLY1            EQU  H'0001'
LDLY2            EQU  H'0002'
LDLY3            EQU  H'0003'
HDLY0            EQU  H'0004'
HDLY1            EQU  H'0005'
HDLY2            EQU  H'0006'
HDLY3            EQU  H'0007'


;----- RAMPCON Bits -----------------------------------------------------
RMPEN            EQU  H'0007'

RMP0             EQU  H'0000'
RMP1             EQU  H'0001'
RMP2             EQU  H'0002'
RMP3             EQU  H'0003'
RMP4             EQU  H'0004'


;----- OCCON Bits -----------------------------------------------------
OCEN             EQU  H'0007'

OOC0             EQU  H'0000'
OOC1             EQU  H'0001'
OOC2             EQU  H'0002'
OOC3             EQU  H'0003'
OOC4             EQU  H'0004'
OCLEB0           EQU  H'0005'
OCLEB1           EQU  H'0006'


;----- RELEFF Bits -----------------------------------------------------
MSDONE           EQU  H'0007'


;----- WPUGPA Bits -----------------------------------------------------
WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WCS0             EQU  H'0002'
WCS1             EQU  H'0003'
WPUA5            EQU  H'0005'

WPUGPA0          EQU  H'0000'
WPUGPA1          EQU  H'0001'
WPUGPA2          EQU  H'0002'
WPUGPA3          EQU  H'0003'
WPUGPA5          EQU  H'0005'


;----- WPUGPB Bits -----------------------------------------------------
WPUB1            EQU  H'0001'
WPUB2            EQU  H'0002'
WPUB3            EQU  H'0003'
WPUB4            EQU  H'0004'
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'
WPUB7            EQU  H'0007'

WPUGPB1          EQU  H'0001'
WPUGPB2          EQU  H'0002'
WPUGPB3          EQU  H'0003'
WPUGPB4          EQU  H'0004'
WPUGPB5          EQU  H'0005'
WPUGPB6          EQU  H'0006'
WPUGPB7          EQU  H'0007'


;----- PE1 Bits -----------------------------------------------------
OVTEE            EQU  H'0000'
UVTEE            EQU  H'0001'
SPAN             EQU  H'0002'
MEASEN           EQU  H'0003'
LODIS            EQU  H'0004'
HIDIS            EQU  H'0005'
TOPO             EQU  H'0006'
DECON            EQU  H'0007'


;----- MODECON Bits -----------------------------------------------------
EACLMP           EQU  H'0003'
CNSG             EQU  H'0004'
VDDEN            EQU  H'0005'
VGNDEN           EQU  H'0006'
CLMPSEL          EQU  H'0007'

MSC0             EQU  H'0000'
MSC1             EQU  H'0001'
MSC2             EQU  H'0002'


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


;----- VREFCAL Bits -----------------------------------------------------
VREF0            EQU  H'0000'
VREF1            EQU  H'0001'
VREF2            EQU  H'0002'
VREF3            EQU  H'0003'
VREF4            EQU  H'0004'


;----- VRFSCAL Bits -----------------------------------------------------
VRFS0            EQU  H'0000'
VRFS1            EQU  H'0001'
VRFS2            EQU  H'0002'
VRFS3            EQU  H'0003'
VRFS4            EQU  H'0004'


;----- RAMPCAL Bits -----------------------------------------------------
RAMP0            EQU  H'0000'
RAMP1            EQU  H'0001'
RAMP2            EQU  H'0002'
RAMP3            EQU  H'0003'
RAMP4            EQU  H'0004'


;----- CSRCAL Bits -----------------------------------------------------
CSR0             EQU  H'0000'
CSR1             EQU  H'0001'
CSR2             EQU  H'0002'
CSR3             EQU  H'0003'
CSR4             EQU  H'0004'


;----- OVUVCAL Bits -----------------------------------------------------
OVCO0            EQU  H'0000'
OVCO1            EQU  H'0001'
OVCO2            EQU  H'0002'
OVCO3            EQU  H'0003'
UVCO0            EQU  H'0004'
UVCO1            EQU  H'0005'
UVCO2            EQU  H'0006'
UVCO3            EQU  H'0007'


;----- DEMCAL Bits -----------------------------------------------------
DEMOV0           EQU  H'0000'
DEMOV1           EQU  H'0001'
DEMOV2           EQU  H'0002'
DEMOV3           EQU  H'0003'
DEMOV4           EQU  H'0004'


;----- HCSOVCAL Bits -----------------------------------------------------
HCSOV0           EQU  H'0000'
HCSOV1           EQU  H'0001'
HCSOV2           EQU  H'0002'
HCSOV3           EQU  H'0003'
HCSOV4           EQU  H'0004'
HCSOV5           EQU  H'0005'
HCSOV6           EQU  H'0006'


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
IOCB7            EQU  H'0007'


;----- ANSELA Bits -----------------------------------------------------
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA3            EQU  H'0003'


;----- ANSELB Bits -----------------------------------------------------
ANSB1            EQU  H'0001'
ANSB2            EQU  H'0002'
ANSB4            EQU  H'0004'
ANSB5            EQU  H'0005'


;----- PMCON1 Bits -----------------------------------------------------
RD               EQU  H'0000'
WR               EQU  H'0001'
WREN             EQU  H'0002'
CALSEL           EQU  H'0006'


;----- PMADRL Bits -----------------------------------------------------
PMADRL0          EQU  H'0000'
PMADRL1          EQU  H'0001'
PMADRL2          EQU  H'0002'
PMADRL3          EQU  H'0003'
PMADRL4          EQU  H'0004'
PMADRL5          EQU  H'0005'
PMADRL6          EQU  H'0006'
PMADRL7          EQU  H'0007'


;----- PMADRH Bits -----------------------------------------------------
PMADRH0          EQU  H'0000'
PMADRH1          EQU  H'0001'
PMADRH2          EQU  H'0002'
PMADRH3          EQU  H'0003'


;----- PMDATL Bits -----------------------------------------------------
PMDATL0          EQU  H'0000'
PMDATL1          EQU  H'0001'
PMDATL2          EQU  H'0002'
PMDATL3          EQU  H'0003'
PMDATL4          EQU  H'0004'
PMDATL5          EQU  H'0005'
PMDATL6          EQU  H'0006'
PMDATL7          EQU  H'0007'


;----- PMDATH Bits -----------------------------------------------------
PMDATH0          EQU  H'0000'
PMDATH1          EQU  H'0001'
PMDATH2          EQU  H'0002'
PMDATH3          EQU  H'0003'
PMDATH4          EQU  H'0004'
PMDATH5          EQU  H'0005'
PMDATH6          EQU  H'0006'
PMDATH7          EQU  H'0007'


;----- TTACAL Bits -----------------------------------------------------
TTA0             EQU  H'0000'
TTA1             EQU  H'0001'
TTA2             EQU  H'0002'
TTA3             EQU  H'0003'


;----- OSCCAL Bits -----------------------------------------------------
FCAL0            EQU  H'0000'
FCAL1            EQU  H'0001'
FCAL2            EQU  H'0002'
FCAL3            EQU  H'0003'
FCAL4            EQU  H'0004'
FCAL5            EQU  H'0005'
FCAL6            EQU  H'0006'


;----- BGTCAL Bits -----------------------------------------------------
BGT0             EQU  H'0000'
BGT1             EQU  H'0001'
BGT2             EQU  H'0002'
BGT3             EQU  H'0003'


;----- BGRCAL Bits -----------------------------------------------------
BGR0             EQU  H'0000'
BGR1             EQU  H'0001'
BGR2             EQU  H'0002'
BGR3             EQU  H'0003'
BGR4             EQU  H'0004'
BGR5             EQU  H'0005'


;----- AVDDCAL Bits -----------------------------------------------------
AVDD0            EQU  H'0000'
AVDD1            EQU  H'0001'
AVDD2            EQU  H'0002'
AVDD3            EQU  H'0003'


;----- VOURCAL Bits -----------------------------------------------------
VOUR0            EQU  H'0000'
VOUR1            EQU  H'0001'
VOUR2            EQU  H'0002'
VOUR3            EQU  H'0003'
VOUR4            EQU  H'0004'


;----- DOVCAL Bits -----------------------------------------------------
DOV0             EQU  H'0000'
DOV1             EQU  H'0001'
DOV2             EQU  H'0002'
DOV3             EQU  H'0003'
DOV4             EQU  H'0004'


;----- VEAOCAL Bits -----------------------------------------------------
VEAO0            EQU  H'0000'
VEAO1            EQU  H'0001'
VEAO2            EQU  H'0002'
VEAO3            EQU  H'0003'
VEAO4            EQU  H'0004'


;----- BUFFCON Bits -----------------------------------------------------
DIGOEN           EQU  H'0006'
BNCHEN           EQU  H'0007'

DSEL0            EQU  H'0000'
DSEL1            EQU  H'0001'
DSEL2            EQU  H'0002'
DSEL3            EQU  H'0003'
DSEL4            EQU  H'0004'


;----- ATSTCON Bits -----------------------------------------------------
SWFRQEN          EQU  H'0000'
TMPTBY           EQU  H'0001'
EAFBEN           EQU  H'0002'
AVDDTSEN         EQU  H'0004'
DTYCLEN          EQU  H'0006'
EACLKEN          EQU  H'0007'




;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'01FF'
       __BADRAM  H'0089'
       __BADRAM  H'008C'-H'008F'
       __BADRAM  H'0097'
       __BADRAM  H'0109'
       __BADRAM  H'010C'-H'010F'
       __BADRAM  H'0189'
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
_WDTE_OFF            EQU  H'3FF7'; WDT disabled
_WDT_OFF             EQU  H'3FF7'; WDT disabled
_WDTE_ON             EQU  H'3FFF'; WDT enabled
_WDT_ON              EQU  H'3FFF'; WDT enabled

_PWRTE_ON            EQU  H'3FEF'; PWRT enabled
_PWRTE_OFF           EQU  H'3FFF'; PWRT disabled

_MCLRE_OFF           EQU  H'3FDF'; MCLR pin is alternate function, MCLR function is internally disabled
_MCLRE_ON            EQU  H'3FFF'; MCLR pin is MCLR function and weak internal pull-up is enabled

_CP_ON               EQU  H'3FBF'; Program memory is external read and write protected
_CP_OFF              EQU  H'3FFF'; Program memory is not code protected

_BOREN_OFF           EQU  H'3EFF'; BOR Disabled
_BOREN_ON            EQU  H'3FFF'; BOR enabled during operation and disabled in Sleep

_WRT_ALL             EQU  H'33FF'; 000h to FFFh write protected, entire program memory is write protected.
_WRT_HALF            EQU  H'37FF'; 000h to 1FFh write protected, 200h to 3FFh may be modified by PMCON1 control
_WRT_BOOT            EQU  H'3BFF'; 000h to FFh write protected, 100h to 3FFh may be modified by PMCON1 control
_WRT_OFF             EQU  H'3FFF'; Write protection off

;----- DEVID Equates --------------------------------------------------
_DEVID1          EQU  H'2006'

;----- IDLOC Equates --------------------------------------------------
_IDLOC0          EQU  H'2000'
_IDLOC1          EQU  H'2001'
_IDLOC2          EQU  H'2002'
_IDLOC3          EQU  H'2003'

        LIST
