        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP19625 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP19625 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP19625
;       2. LIST directive in the source file
;               LIST   P=MCP19625
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP19625
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
OVCCON           EQU  H'0019'
OVFCON           EQU  H'001A'
OSCTUNE          EQU  H'001B'
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
APFCON           EQU  H'0089'
VINLVL           EQU  H'0090'
OCCON            EQU  H'0091'
CSGSCON          EQU  H'0093'
CSDGCON          EQU  H'0095'
VZCCON           EQU  H'0097'
CMPZCON          EQU  H'0098'
OUVCON           EQU  H'0099'
OOVCON           EQU  H'009A'
DEADCON          EQU  H'009B'
SLPCRCON         EQU  H'009C'
SLVGNCON         EQU  H'009D'
RELEFF           EQU  H'009E'

;-----Bank2------------------
WPUGPA           EQU  H'0105'
WPUGPB           EQU  H'0106'
PE1              EQU  H'0107'
BUFFCON          EQU  H'0108'
ABECON           EQU  H'0109'
SSPADD           EQU  H'0110'
SSPBUF           EQU  H'0111'
SSPCON1          EQU  H'0112'
SSPCON2          EQU  H'0113'
SSPCON3          EQU  H'0114'
SSPMSK           EQU  H'0115'
SSPSTAT          EQU  H'0116'
SSPADD2          EQU  H'0117'
SSPMSK2          EQU  H'0118'

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
PMADRL           EQU  H'0192'
PMADRH           EQU  H'0193'
PMDATL           EQU  H'0194'
PMDATH           EQU  H'0195'
OSCCAL           EQU  H'0198'
DOVCAL           EQU  H'0199'
TTACAL           EQU  H'019A'
BGRCAL           EQU  H'019B'
VROCAL           EQU  H'019C'
ZROCAL           EQU  H'019D'
ATSTCON          EQU  H'019F'

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
GPB4             EQU  H'0004'
GPB5             EQU  H'0005'


;----- PORTGPB Bits -----------------------------------------------------
GPB0             EQU  H'0000'
GPB1             EQU  H'0001'
GPB4             EQU  H'0004'
GPB5             EQU  H'0005'


;----- PIR1 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
SSPIF            EQU  H'0004'
BCLIF            EQU  H'0005'
ADIF             EQU  H'0006'


;----- PIR2 Bits -----------------------------------------------------
DCERIF           EQU  H'0000'
VINIF            EQU  H'0001'
OVIF             EQU  H'0004'
OCIF             EQU  H'0005'
UVIF             EQU  H'0007'


;----- PCON Bits -----------------------------------------------------
NOT_POR          EQU  H'0001'
NOT_OT           EQU  H'0002'


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


;----- OVCCON Bits -----------------------------------------------------
OVC0             EQU  H'0000'
OVC1             EQU  H'0001'
OVC2             EQU  H'0002'
OVC3             EQU  H'0003'
OVC4             EQU  H'0004'
OVC5             EQU  H'0005'
OVC6             EQU  H'0006'
OVC7             EQU  H'0007'


;----- OVFCON Bits -----------------------------------------------------
OVF0             EQU  H'0000'
OVF1             EQU  H'0001'
OVF2             EQU  H'0002'
OVF3             EQU  H'0003'
OVF4             EQU  H'0004'
VOUTON           EQU  H'0007'


;----- OSCTUNE Bits -----------------------------------------------------
TUN0             EQU  H'0000'
TUN1             EQU  H'0001'
TUN2             EQU  H'0002'
TUN3             EQU  H'0003'
TUN4             EQU  H'0004'


;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'
CHS0             EQU  H'0002'
CHS1             EQU  H'0003'
CHS2             EQU  H'0004'
CHS3             EQU  H'0005'
CHS4             EQU  H'0006'


;----- ADCON1 Bits -----------------------------------------------------
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
TRISA7           EQU  H'0007'


;----- TRISGPA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA3           EQU  H'0003'
TRISA4           EQU  H'0004'
TRISA5           EQU  H'0005'
TRISA7           EQU  H'0007'


;----- TRISGPB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'


;----- PIE1 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
SSPIE            EQU  H'0004'
BCLIE            EQU  H'0005'
ADIE             EQU  H'0006'


;----- PIE2 Bits -----------------------------------------------------
DCERIE           EQU  H'0000'
VINIE            EQU  H'0001'
OVIE             EQU  H'0004'
OCIE             EQU  H'0005'
UVIE             EQU  H'0007'


;----- APFCON Bits -----------------------------------------------------
CLKSEL           EQU  H'0000'


;----- VINLVL Bits -----------------------------------------------------
UVLO0            EQU  H'0000'
UVLO1            EQU  H'0001'
UVLO2            EQU  H'0002'
UVLO3            EQU  H'0003'
UVLO4            EQU  H'0004'
UVLOEN           EQU  H'0007'


;----- OCCON Bits -----------------------------------------------------
OOC0             EQU  H'0000'
OOC1             EQU  H'0001'
OOC2             EQU  H'0002'
OOC3             EQU  H'0003'
OOC4             EQU  H'0004'
OCLEB0           EQU  H'0005'
OCLEB1           EQU  H'0006'
OCEN             EQU  H'0007'


;----- CSGSCON Bits -----------------------------------------------------
CSGS0            EQU  H'0000'
CSGS1            EQU  H'0001'
CSGS2            EQU  H'0002'
CSGS3            EQU  H'0003'


;----- CSDGCON Bits -----------------------------------------------------
CSDG0            EQU  H'0000'
CSDG1            EQU  H'0001'
CSDG2            EQU  H'0002'
CSDGEN           EQU  H'0007'


;----- VZCCON Bits -----------------------------------------------------
VZC0             EQU  H'0000'
VZC1             EQU  H'0001'
VZC2             EQU  H'0002'
VZC3             EQU  H'0003'
VZC4             EQU  H'0004'
VZC5             EQU  H'0005'
VZC6             EQU  H'0006'
VZC7             EQU  H'0007'


;----- CMPZCON Bits -----------------------------------------------------
CMPZG0           EQU  H'0000'
CMPZG1           EQU  H'0001'
CMPZG2           EQU  H'0002'
CMPZG3           EQU  H'0003'
CMPZF0           EQU  H'0004'
CMPZF1           EQU  H'0005'
CMPZF2           EQU  H'0006'
CMPZF3           EQU  H'0007'


;----- OUVCON Bits -----------------------------------------------------
OUV0             EQU  H'0000'
OUV1             EQU  H'0001'
OUV2             EQU  H'0002'
OUV3             EQU  H'0003'
OUV4             EQU  H'0004'
OUV5             EQU  H'0005'
OUV6             EQU  H'0006'
OUV7             EQU  H'0007'


;----- OOVCON Bits -----------------------------------------------------
OOV0             EQU  H'0000'
OOV1             EQU  H'0001'
OOV2             EQU  H'0002'
OOV3             EQU  H'0003'
OOV4             EQU  H'0004'
OOV5             EQU  H'0005'
OOV6             EQU  H'0006'
OOV7             EQU  H'0007'


;----- DEADCON Bits -----------------------------------------------------
LDLY0            EQU  H'0000'
LDLY1            EQU  H'0001'
LDLY2            EQU  H'0002'
LDLY3            EQU  H'0003'
HDLY0            EQU  H'0004'
HDLY1            EQU  H'0005'
HDLY2            EQU  H'0006'
HDLY3            EQU  H'0007'


;----- SLPCRCON Bits -----------------------------------------------------
SLPS0            EQU  H'0000'
SLPS1            EQU  H'0001'
SLPS2            EQU  H'0002'
SLPS3            EQU  H'0003'
SLPG0            EQU  H'0004'
SLPG1            EQU  H'0005'
SLPG2            EQU  H'0006'
SLPG3            EQU  H'0007'


;----- SLVGNCON Bits -----------------------------------------------------
SLVGN0           EQU  H'0000'
SLVGN1           EQU  H'0001'
SLVGN2           EQU  H'0002'
SLVGN3           EQU  H'0003'
SLVGN4           EQU  H'0004'


;----- RELEFF Bits -----------------------------------------------------
RE0              EQU  H'0000'
RE1              EQU  H'0001'
RE2              EQU  H'0002'
RE3              EQU  H'0003'
RE4              EQU  H'0004'
RE5              EQU  H'0005'
RE6              EQU  H'0006'
MSDONE           EQU  H'0007'


;----- WPUGPA Bits -----------------------------------------------------
WPUGPA0          EQU  H'0000'
WPUGPA1          EQU  H'0001'
WPUGPA2          EQU  H'0002'
WPUGPA3          EQU  H'0003'
WPUGPA5          EQU  H'0005'

WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WPUA2            EQU  H'0002'
WPUA3            EQU  H'0003'
WPUA5            EQU  H'0005'


;----- WPUGPB Bits -----------------------------------------------------
WPUGPB1          EQU  H'0001'
WPUGPB4          EQU  H'0004'
WPUGPB5          EQU  H'0005'

WPUB1            EQU  H'0001'
WPUB2            EQU  H'0002'
WPUB4            EQU  H'0004'
WPUB5            EQU  H'0005'
WPUB6            EQU  H'0006'
WPUB7            EQU  H'0007'


;----- PE1 Bits -----------------------------------------------------
OVTEE            EQU  H'0000'
UVTEE            EQU  H'0001'
PUEN             EQU  H'0002'
PDEN             EQU  H'0003'
LDLYBY           EQU  H'0004'
HDLYBY           EQU  H'0005'
DVRSTR           EQU  H'0006'
DECON            EQU  H'0007'


;----- BUFFCON Bits -----------------------------------------------------
ASEL0            EQU  H'0000'
ASEL1            EQU  H'0001'
ASEL2            EQU  H'0002'
ASEL3            EQU  H'0003'
ASEL4            EQU  H'0004'
MLTPH0           EQU  H'0005'
MLTPH1           EQU  H'0006'
MLTPH2           EQU  H'0007'


;----- ABECON Bits -----------------------------------------------------
PATHEN           EQU  H'0000'
RECIREN          EQU  H'0001'
TMPSEN           EQU  H'0002'
CRTMEN           EQU  H'0003'
SLCPBY           EQU  H'0004'
MEASEN           EQU  H'0005'
UVDCEN           EQU  H'0006'
OVDCEN           EQU  H'0007'


;----- SSPCON1 Bits -----------------------------------------------------
SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'


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
IOCA7            EQU  H'0007'


;----- IOCB Bits -----------------------------------------------------
IOCB0            EQU  H'0000'
IOCB1            EQU  H'0001'
IOCB4            EQU  H'0004'
IOCB5            EQU  H'0005'


;----- ANSELA Bits -----------------------------------------------------
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA3            EQU  H'0003'


;----- ANSELB Bits -----------------------------------------------------
ANSB1            EQU  H'0001'
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


;----- OSCCAL Bits -----------------------------------------------------
FCALT0           EQU  H'0000'
FCALT1           EQU  H'0001'
FCALT2           EQU  H'0002'
FCALT3           EQU  H'0003'
FCALT4           EQU  H'0004'
FCALT5           EQU  H'0005'
FCALT6           EQU  H'0006'


;----- DOVCAL Bits -----------------------------------------------------
DOVT0            EQU  H'0000'
DOVT1            EQU  H'0001'
DOVT2            EQU  H'0002'
DOVT3            EQU  H'0003'


;----- TTACAL Bits -----------------------------------------------------
TTA0             EQU  H'0000'
TTA1             EQU  H'0001'
TTA2             EQU  H'0002'
TTA3             EQU  H'0003'


;----- BGRCAL Bits -----------------------------------------------------
BGRT0            EQU  H'0000'
BGRT1            EQU  H'0001'
BGRT2            EQU  H'0002'
BGRT3            EQU  H'0003'


;----- VROCAL Bits -----------------------------------------------------
VROT0            EQU  H'0000'
VROT1            EQU  H'0001'
VROT2            EQU  H'0002'
VROT3            EQU  H'0003'


;----- ZROCAL Bits -----------------------------------------------------
ZROT0            EQU  H'0000'
ZROT1            EQU  H'0001'
ZROT2            EQU  H'0002'
ZROT3            EQU  H'0003'


;----- ATSTCON Bits -----------------------------------------------------
DRVDIS           EQU  H'0000'
BNCHEN           EQU  H'0001'
LODIS            EQU  H'0002'
HIDIS            EQU  H'0003'




;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'01FF'
       __BADRAM  H'0012'
       __BADRAM  H'0017'-H'0018'
       __BADRAM  H'008C'-H'008F'
       __BADRAM  H'0092'
       __BADRAM  H'0094'
       __BADRAM  H'0096'
       __BADRAM  H'009F'
       __BADRAM  H'010C'-H'010F'
       __BADRAM  H'0119'-H'011F'
       __BADRAM  H'0189'
       __BADRAM  H'0196'-H'0197'
       __BADRAM  H'019E'
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

_WRT_ALL             EQU  H'33FF'; 000h to 7FFh write protected, entire program memory is write protected.
_WRT_HALF            EQU  H'37FF'; 000h to 3FFh write protected, 400h to 7FFh may be modified by PMCON1 control
_WRT_BOOT            EQU  H'3BFF'; 000h to 1FFh write protected, 200h to 7FFh may be modified by PMCON1 control
_WRT_OFF             EQU  H'3FFF'; Write protection off

;----- DEVID Equates --------------------------------------------------
_DEVID1          EQU  H'2006'

;----- IDLOC Equates --------------------------------------------------
_IDLOC0          EQU  H'2000'
_IDLOC1          EQU  H'2001'
_IDLOC2          EQU  H'2002'
_IDLOC3          EQU  H'2003'

        LIST
