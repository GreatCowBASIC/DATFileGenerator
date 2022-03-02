        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP19126 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP19126 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP19126
;       2. LIST directive in the source file
;               LIST   P=MCP19126
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP19126
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
PWMPHL           EQU  H'0013'
PWMPHH           EQU  H'0014'
PWMRL            EQU  H'0015'
PWMRH            EQU  H'0016'
OVREFCON         EQU  H'0017'
CCD1OVRFCON      EQU  H'0018'
CREFCON          EQU  H'0019'
VREF2CON         EQU  H'001A'
OSCTUNE          EQU  H'001B'
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
PMCON1           EQU  H'008C'
PMCON2           EQU  H'008D'
PMADR            EQU  H'008E'
PMADRL           EQU  H'008E'
PMADRH           EQU  H'008F'
PMDAT            EQU  H'0090'
PMDATL           EQU  H'0090'
PMDATH           EQU  H'0091'
VINUVLO          EQU  H'0092'
VINOVLO          EQU  H'0093'
VINCON           EQU  H'0094'
CC1RL            EQU  H'0095'
CC1RH            EQU  H'0096'
CC2RL            EQU  H'0097'
CC2RH            EQU  H'0098'
CCDCON           EQU  H'0099'
CCD1DIMCON       EQU  H'009A'
OVCON            EQU  H'009B'
DEADCON          EQU  H'009D'
SLPCRCON         EQU  H'009E'
DESATCON         EQU  H'009F'

;-----Bank2------------------
WPUGPA           EQU  H'0105'
WPUGPB           EQU  H'0106'
PE1              EQU  H'0107'
MODECON          EQU  H'0108'
ABECON           EQU  H'0109'
ATSTCON          EQU  H'010C'
ICOACON          EQU  H'010D'
ICLEBCON         EQU  H'010E'
SSPADD           EQU  H'0110'
SSPBUF           EQU  H'0111'
SSPCON1          EQU  H'0112'
SSPCON2          EQU  H'0113'
SSPCON3          EQU  H'0114'
SSPMSK           EQU  H'0115'
SSPSTAT          EQU  H'0116'
SSPADD2          EQU  H'0117'
SSPMSK2          EQU  H'0118'
BAUDCON          EQU  H'0119'
SPBRGL           EQU  H'011A'
SPBRGH           EQU  H'011B'
RCREG            EQU  H'011C'
TXREG            EQU  H'011D'
TXSTA            EQU  H'011E'
RCSTA            EQU  H'011F'

;-----Bank3------------------
IOCA             EQU  H'0185'
IOCB             EQU  H'0186'
ANSELA           EQU  H'0187'
ANSELB           EQU  H'0188'
PORTICD          EQU  H'018C'
TRISICD          EQU  H'018D'
ICKBUG           EQU  H'018E'
BIGBUG           EQU  H'018F'
CRSPCAL          EQU  H'0190'
VR2SPCAL         EQU  H'0191'
OVRSPCAL         EQU  H'0192'
DSTCAL           EQU  H'0193'
OSCCAL           EQU  H'0194'
AVDDCAL          EQU  H'0195'
TTACAL           EQU  H'0196'
BGRCAL           EQU  H'0197'
CGMVGMCAL        EQU  H'0198'
A2GCAL           EQU  H'0199'
TMPSNSCAL        EQU  H'019A'

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
GPB7             EQU  H'0007'

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
GPB7             EQU  H'0007'

RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'


;----- PIR1 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
CC1IF            EQU  H'0002'
CC2IF            EQU  H'0003'
SSPIF            EQU  H'0004'
BCLIF            EQU  H'0005'
RCIF             EQU  H'0006'
TXIF             EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
UVLOIF           EQU  H'0000'
OVLOIF           EQU  H'0001'
DRUVIF           EQU  H'0002'
OVIF             EQU  H'0003'
OTIF             EQU  H'0004'
ADIF             EQU  H'0006'
CDSIF            EQU  H'0007'


;----- PCON Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'
VDDOK            EQU  H'0002'
NOT_VDD_FLAG     EQU  H'0003'
ADC_REFR         EQU  H'0007'


;----- INTCON Bits -----------------------------------------------------
IOCF             EQU  H'0000'
INTF             EQU  H'0001'
T0IF             EQU  H'0002'
IOCE             EQU  H'0003'
INTE             EQU  H'0004'
T0IE             EQU  H'0005'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- T1CON Bits -----------------------------------------------------
TMR1ON           EQU  H'0000'
TMR1CS           EQU  H'0001'

T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'


;----- T2CON Bits -----------------------------------------------------
TMR2ON           EQU  H'0002'


;----- OVREFCON Bits -----------------------------------------------------
OOV0             EQU  H'0000'
OOV1             EQU  H'0001'
OOV2             EQU  H'0002'
OOV3             EQU  H'0003'
OOV4             EQU  H'0004'
OOV5             EQU  H'0005'
OOV6             EQU  H'0006'
OOV7             EQU  H'0007'


;----- CCD1OVRFCON Bits -----------------------------------------------------
CCD1OOV0         EQU  H'0000'
CCD1OOV1         EQU  H'0001'
CCD1OOV2         EQU  H'0002'
CCD1OOV3         EQU  H'0003'
CCD1OOV4         EQU  H'0004'
CCD1OOV5         EQU  H'0005'
CCD1OOV6         EQU  H'0006'
CCD1OOV7         EQU  H'0007'


;----- CREFCON Bits -----------------------------------------------------
CREF0            EQU  H'0000'
CREF1            EQU  H'0001'
CREF2            EQU  H'0002'
CREF3            EQU  H'0003'
CREF4            EQU  H'0004'
CREF5            EQU  H'0005'
CREF6            EQU  H'0006'
CREF7            EQU  H'0007'


;----- VREF2CON Bits -----------------------------------------------------
VREF20           EQU  H'0000'
VREF21           EQU  H'0001'
VREF22           EQU  H'0002'
VREF23           EQU  H'0003'
VREF24           EQU  H'0004'
VREF25           EQU  H'0005'
VREF26           EQU  H'0006'
VREF27           EQU  H'0007'


;----- OSCTUNE Bits -----------------------------------------------------
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'


;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'


;----- ADCON1 Bits -----------------------------------------------------
VCFG             EQU  H'0000'


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


;----- PIE1 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
CC1IE            EQU  H'0002'
CC2IE            EQU  H'0003'
SSPIE            EQU  H'0004'
BCLIE            EQU  H'0005'
RCIE             EQU  H'0006'
TXIE             EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
UVLOIE           EQU  H'0000'
OVLOIE           EQU  H'0001'
DRUVIE           EQU  H'0002'
OVIE             EQU  H'0003'
OTIE             EQU  H'0004'
ADIE             EQU  H'0006'
CDSIE            EQU  H'0007'


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


;----- VINCON Bits -----------------------------------------------------
OVLOINTN         EQU  H'0000'
OVLOINTP         EQU  H'0001'
OVLOOUT          EQU  H'0002'
OVLOEN           EQU  H'0003'
UVLOINTN         EQU  H'0004'
UVLOINTP         EQU  H'0005'
UVLOOUT          EQU  H'0006'
UVLOEN           EQU  H'0007'


;----- CCD1DIMCON Bits -----------------------------------------------------
CCD1O0EN         EQU  H'0000'
CCD1O1EN         EQU  H'0001'
CCD1O2EN         EQU  H'0002'
CCD1O3EN         EQU  H'0003'
SYNCHDIS         EQU  H'0004'
CCD1OVRFEN       EQU  H'0005'


;----- OVCON Bits -----------------------------------------------------
OVINTN           EQU  H'0000'
OVINTP           EQU  H'0001'
OVOUT            EQU  H'0002'
OVEN             EQU  H'0003'


;----- SLPCRCON Bits -----------------------------------------------------
SLPBY            EQU  H'0006'


;----- DESATCON Bits -----------------------------------------------------
CDSINTN          EQU  H'0000'
CDSINTP          EQU  H'0001'
CDSOUT           EQU  H'0002'
CDSOE            EQU  H'0003'
CDSPOL           EQU  H'0004'
CDSWDE           EQU  H'0006'
CDSMUX           EQU  H'0007'


;----- WPUGPA Bits -----------------------------------------------------
WPUGPA0          EQU  H'0000'
WPUGPA1          EQU  H'0001'
WPUGPA2          EQU  H'0002'
WPUGPA3          EQU  H'0003'
WPUGPA4          EQU  H'0004'
WPUGPA5          EQU  H'0005'
WPUGPA6          EQU  H'0006'

WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WPUA2            EQU  H'0002'
WPUA3            EQU  H'0003'
WPUA5            EQU  H'0005'


;----- WPUGPB Bits -----------------------------------------------------
WPUGPB1          EQU  H'0001'
WPUGPB2          EQU  H'0002'
WPUGPB3          EQU  H'0003'
WPUGPB4          EQU  H'0004'
WPUGPB5          EQU  H'0005'
WPUGPB6          EQU  H'0006'
WPUGPB7          EQU  H'0007'

WPUB1            EQU  H'0001'
WPUB2            EQU  H'0002'
WPUB4            EQU  H'0004'
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'
WPUB7            EQU  H'0007'


;----- PE1 Bits -----------------------------------------------------
PWMSTR_SEN       EQU  H'0000'
PWMSTR_PEN       EQU  H'0001'
FLTREN           EQU  H'0003'
SDRVBY           EQU  H'0004'
PDRVBY           EQU  H'0005'
SDRVEN           EQU  H'0006'
PDRVEN           EQU  H'0007'


;----- MODECON Bits -----------------------------------------------------
CEAVEACTRL       EQU  H'0000'
MSC0             EQU  H'0005'
MSC1             EQU  H'0006'
MSC2             EQU  H'0007'


;----- ABECON Bits -----------------------------------------------------
ANAOEN           EQU  H'0000'
CEADIS           EQU  H'0001'
VEADIS           EQU  H'0002'
DRUVSEL          EQU  H'0003'
DSEL0            EQU  H'0004'
DSEL1            EQU  H'0005'
DSEL2            EQU  H'0006'
DIGOEN           EQU  H'0007'


;----- ATSTCON Bits -----------------------------------------------------
TMPTBY           EQU  H'0000'
DRUVBY           EQU  H'0001'
MANAZ            EQU  H'0002'
SELMANAZ         EQU  H'0003'
SWFRQOR          EQU  H'0004'
BRGBY            EQU  H'0006'
CCD1STAT         EQU  H'0007'


;----- ICOACON Bits -----------------------------------------------------
ICOAC0           EQU  H'0000'
ICOAC1           EQU  H'0001'
ICOAC2           EQU  H'0002'
ICOAC3           EQU  H'0003'


;----- ICLEBCON Bits -----------------------------------------------------
ICLEBC0          EQU  H'0000'
ICLEBC1          EQU  H'0001'


;----- SSPCON1 Bits -----------------------------------------------------
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'

SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'


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


;----- BAUDCON Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- SPBRGL Bits -----------------------------------------------------
SPBRG0           EQU  H'0000'
SPBRG1           EQU  H'0001'
SPBRG2           EQU  H'0002'
SPBRG3           EQU  H'0003'
SPBRG4           EQU  H'0004'
SPBRG5           EQU  H'0005'
SPBRG6           EQU  H'0006'
SPBRG7           EQU  H'0007'


;----- SPBRGH Bits -----------------------------------------------------
SPBRG8           EQU  H'0000'
SPBRG9           EQU  H'0001'
SPBRG10          EQU  H'0002'
SPBRG11          EQU  H'0003'
SPBRG12          EQU  H'0004'
SPBRG13          EQU  H'0005'
SPBRG14          EQU  H'0006'
SPBRG15          EQU  H'0007'


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
ANSA4            EQU  H'0004'


;----- ANSELB Bits -----------------------------------------------------
ANSB1            EQU  H'0001'
ANSB2            EQU  H'0002'
ANSB3            EQU  H'0003'
ANSB4            EQU  H'0004'
ANSB5            EQU  H'0005'
ANSB6            EQU  H'0006'


;----- CRSPCAL Bits -----------------------------------------------------
CRSPCAL0         EQU  H'0000'
CRSPCAL1         EQU  H'0001'
CRSPCAL2         EQU  H'0002'
CRSPCAL3         EQU  H'0003'
CRSPCAL4         EQU  H'0004'


;----- VR2SPCAL Bits -----------------------------------------------------
VR2SPCAL0        EQU  H'0000'
VR2SPCAL1        EQU  H'0001'
VR2SPCAL2        EQU  H'0002'
VR2SPCAL3        EQU  H'0003'
VR2SPCAL4        EQU  H'0004'


;----- OVRSPCAL Bits -----------------------------------------------------
OVRSPCAL0        EQU  H'0000'
OVRSPCAL1        EQU  H'0001'
OVRSPCAL2        EQU  H'0002'
OVRSPCAL3        EQU  H'0003'
OVRSPCAL4        EQU  H'0004'


;----- DSTCAL Bits -----------------------------------------------------
DSTCAL0          EQU  H'0000'
DSTCAL1          EQU  H'0001'
DSTCAL2          EQU  H'0002'
DSTCAL3          EQU  H'0003'
DSTCAL4          EQU  H'0004'


;----- OSCCAL Bits -----------------------------------------------------
OSCCAL0          EQU  H'0000'
OSCCAL1          EQU  H'0001'
OSCCAL2          EQU  H'0002'
OSCCAL3          EQU  H'0003'
OSCCAL4          EQU  H'0004'
OSCCAL5          EQU  H'0005'
OSCCAL6          EQU  H'0006'


;----- AVDDCAL Bits -----------------------------------------------------
AVDDCAL0         EQU  H'0000'
AVDDCAL1         EQU  H'0001'
AVDDCAL2         EQU  H'0002'
AVDDCAL3         EQU  H'0003'


;----- TTACAL Bits -----------------------------------------------------
TTACAL0          EQU  H'0000'
TTACAL1          EQU  H'0001'
TTACAL2          EQU  H'0002'
TTACAL3          EQU  H'0003'


;----- BGRCAL Bits -----------------------------------------------------
BGRCAL0          EQU  H'0000'
BGRCAL1          EQU  H'0001'
BGRCAL2          EQU  H'0002'
BGRCAL3          EQU  H'0003'
BGRCAL4          EQU  H'0004'


;----- CGMVGMCAL Bits -----------------------------------------------------
VGMCAL0          EQU  H'0000'
VGMCAL1          EQU  H'0001'
VGMCAL2          EQU  H'0002'
VGMCAL3          EQU  H'0003'
CGMCAL0          EQU  H'0004'
CGMCAL1          EQU  H'0005'
CGMCAL2          EQU  H'0006'
CGMCAL3          EQU  H'0007'


;----- A2GCAL Bits -----------------------------------------------------
A2GCAL0          EQU  H'0000'
A2GCAL1          EQU  H'0001'
A2GCAL2          EQU  H'0002'
A2GCAL3          EQU  H'0003'


;----- TMPSNSCAL Bits -----------------------------------------------------
TMPSNSCAL0       EQU  H'0000'
TMPSNSCAL1       EQU  H'0001'
TMPSNSCAL2       EQU  H'0002'
TMPSNSCAL3       EQU  H'0003'
TMPSNSCAL4       EQU  H'0004'




;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'01FF'
       __BADRAM  H'0012'
       __BADRAM  H'0089'
       __BADRAM  H'009C'
       __BADRAM  H'010F'
       __BADRAM  H'0189'
       __BADRAM  H'019B'-H'01A0'

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

_WRT_ALL             EQU  H'33FF'; 000h to FFFh write protected, entire program memory is write protected.
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
