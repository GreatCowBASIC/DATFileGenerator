        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM PS810 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the PS810 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /PS810
;       2. LIST directive in the source file
;               LIST   P=PS810
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __PS810
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

;==========================================================================
;       16Cxxx/17Cxxx Substitutions
;==========================================================================
  #define DDRA  TRISA      ; PIC17Cxxx SFR substitution
  #define DDRB  TRISB      ; PIC17Cxxx SFR substitution
  #define DDRC  TRISC      ; PIC17Cxxx SFR substitution
  #define DDRD  TRISD      ; PIC17Cxxx SFR substitution
  #define DDRE  TRISE      ; PIC17Cxxx SFR substitution

;==========================================================================
;
;       Register Definitions
;
;==========================================================================

;----- Register Files -----------------------------------------------------
PORTA            EQU  H'0F80'
PORTB            EQU  H'0F81'
LATB             EQU  H'0F8A'
PDPB             EQU  H'0F91'
TRISB            EQU  H'0F93'
REFCAL           EQU  H'0F9A'
OSCCAL           EQU  H'0F9B'
BGCAL            EQU  H'0F9C'
PIE1             EQU  H'0F9D'
PIR1             EQU  H'0F9E'
IPR1             EQU  H'0F9F'
PIE2             EQU  H'0FA0'
PIR2             EQU  H'0FA1'
IPR2             EQU  H'0FA2'
EECON1           EQU  H'0FA6'
EECON2           EQU  H'0FA7'
T3CON            EQU  H'0FB1'
TMR3             EQU  H'0FB2'
CMCON            EQU  H'0FB4'
ADCON2           EQU  H'0FC0'
ADCON1           EQU  H'0FC1'
ADCON0           EQU  H'0FC2'
ADRESL           EQU  H'0FC3'
ADRESH           EQU  H'0FC4'
ASICON2          EQU  H'0FC5'
ASICON1          EQU  H'0FC6'
ASISTAT          EQU  H'0FC7'
ASIADD           EQU  H'0FC8'
ASIBUF           EQU  H'0FC9'
T2CON            EQU  H'0FCA'
TMR2             EQU  H'0FCC'
T1CON            EQU  H'0FCD'
TMR1             EQU  H'0FCE'
RCON             EQU  H'0FD0'
WDTCON           EQU  H'0FD1'
OSCCON           EQU  H'0FD3'
T0CON            EQU  H'0FD5'
TMR0L            EQU  H'0FD6'
TMR0H            EQU  H'0FD7'
STATUS           EQU  H'0FD8'
FSR2L            EQU  H'0FD9'
FSR2H            EQU  H'0FDA'
PLUSW2           EQU  H'0FDB'
PREINC2          EQU  H'0FDC'
POSTDEC2         EQU  H'0FDD'
POSTINC2         EQU  H'0FDE'
INDF2            EQU  H'0FDF'
BSR              EQU  H'0FE0'
FSR1L            EQU  H'0FE1'
FSR1H            EQU  H'0FE2'
PLUSW1           EQU  H'0FE3'
PREINC1          EQU  H'0FE4'
POSTDEC1         EQU  H'0FE5'
POSTINC1         EQU  H'0FE6'
INDF1            EQU  H'0FE7'
WREG             EQU  H'0FE8'
FSR0L            EQU  H'0FE9'
FSR0H            EQU  H'0FEA'
PLUSW0           EQU  H'0FEB'
PREINC0          EQU  H'0FEC'
POSTDEC0         EQU  H'0FED'
POSTINC0         EQU  H'0FEE'
INDF0            EQU  H'0FEF'
INTCON2          EQU  H'0FF1'
INTCON           EQU  H'0FF2'
PROD             EQU  H'0FF3'
PRODL            EQU  H'0FF3'
PRODH            EQU  H'0FF4'
TABLAT           EQU  H'0FF5'
TBLPTR           EQU  H'0FF6'
TBLPTRL          EQU  H'0FF6'
TBLPTRH          EQU  H'0FF7'
TBLPTRU          EQU  H'0FF8'
PC               EQU  H'0FF9'
PCL              EQU  H'0FF9'
PCLATH           EQU  H'0FFA'
PCLATU           EQU  H'0FFB'
STKPTR           EQU  H'0FFC'
TOS              EQU  H'0FFD'
TOSL             EQU  H'0FFD'
TOSH             EQU  H'0FFE'
TOSU             EQU  H'0FFF'

;----- PORTA Bits -----------------------------------------------------
RA5              EQU  H'0005'

MCLR             EQU  H'0005'

INT0             EQU  H'0005'


;----- PORTB Bits -----------------------------------------------------
RB0              EQU  H'0000'
RB1              EQU  H'0001'
RB2              EQU  H'0002'
RB3              EQU  H'0003'
RB4              EQU  H'0004'
RB5              EQU  H'0005'
RB6              EQU  H'0006'
RB7              EQU  H'0007'

SCL              EQU  H'0000'
SDA              EQU  H'0001'
VC2              EQU  H'0005'
NTC              EQU  H'0006'

SPS              EQU  H'0001'
PGC              EQU  H'0006'
PGD              EQU  H'0007'


;----- LATB Bits -----------------------------------------------------
LATB0            EQU  H'0000'
LATB1            EQU  H'0001'
LATB2            EQU  H'0002'
LATB3            EQU  H'0003'
LATB4            EQU  H'0004'
LATB5            EQU  H'0005'
LATB6            EQU  H'0006'
LATB7            EQU  H'0007'


;----- PDPB Bits -----------------------------------------------------
PDPB0            EQU  H'0000'
PDPB1            EQU  H'0001'
PDPB2            EQU  H'0002'
PDPB3            EQU  H'0003'
PDPB4            EQU  H'0004'
PDPB5            EQU  H'0005'
PDPB6            EQU  H'0006'
PDPB7            EQU  H'0007'


;----- TRISB Bits -----------------------------------------------------
TRISB0           EQU  H'0000'
TRISB1           EQU  H'0001'
TRISB2           EQU  H'0002'
TRISB3           EQU  H'0003'
TRISB4           EQU  H'0004'
TRISB5           EQU  H'0005'
TRISB6           EQU  H'0006'
TRISB7           EQU  H'0007'


;----- OSCCAL Bits -----------------------------------------------------
REXT             EQU  H'0007'

OSC0             EQU  H'0000'
OSC1             EQU  H'0001'
OSC2             EQU  H'0002'
OSC3             EQU  H'0003'
OSC4             EQU  H'0004'
OSC5             EQU  H'0005'
OSC6             EQU  H'0006'


;----- BGCAL Bits -----------------------------------------------------
BGTC0            EQU  H'0000'
BGTC1            EQU  H'0001'
BGTC2            EQU  H'0002'
BGTC3            EQU  H'0003'


;----- PIE1 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
ASIIE            EQU  H'0003'
ADIE             EQU  H'0006'


;----- PIR1 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
ASIIF            EQU  H'0003'
ADIF             EQU  H'0006'


;----- IPR1 Bits -----------------------------------------------------
TMR1IP           EQU  H'0000'
TMR2IP           EQU  H'0001'
ASIIP            EQU  H'0003'
ADIP             EQU  H'0006'


;----- PIE2 Bits -----------------------------------------------------
TMR3IE           EQU  H'0001'
AOFIE            EQU  H'0003'
CMIE             EQU  H'0006'


;----- PIR2 Bits -----------------------------------------------------
TMR3IF           EQU  H'0001'
AOFIF            EQU  H'0003'
CMIF             EQU  H'0006'


;----- IPR2 Bits -----------------------------------------------------
TMR3IP           EQU  H'0001'
AOFIP            EQU  H'0003'
CMIP             EQU  H'0006'


;----- EECON1 Bits -----------------------------------------------------
RD               EQU  H'0000'
WR               EQU  H'0001'
WREN             EQU  H'0002'
WRERR            EQU  H'0003'
FREE             EQU  H'0004'
COMA             EQU  H'0005'
CFGS             EQU  H'0006'
EEPGD            EQU  H'0007'


;----- T3CON Bits -----------------------------------------------------
T3PSA            EQU  H'0003'
T3ON             EQU  H'0007'

T3PS0            EQU  H'0000'
T3PS1            EQU  H'0001'
T3PS2            EQU  H'0002'


;----- CMCON Bits -----------------------------------------------------
CWVEN            EQU  H'0000'
CWVI             EQU  H'0004'
CWTST            EQU  H'0007'


;----- ADCON2 Bits -----------------------------------------------------
C340             EQU  H'0007'

ADCS0            EQU  H'0000'
ADCS1            EQU  H'0001'
ADCS2            EQU  H'0002'
ADCS3            EQU  H'0003'


;----- ADCON1 Bits -----------------------------------------------------
ADCOV            EQU  H'0007'

SIZE0            EQU  H'0004'
SIZE1            EQU  H'0005'
SIZE2            EQU  H'0006'


;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'

GO               EQU  H'0001'

NOT_DONE         EQU  H'0001'

DONE             EQU  H'0001'


;----- ASICON2 Bits -----------------------------------------------------
AACS_SPSPS0      EQU  H'0000'
BACS_SPSPS1      EQU  H'0001'
CKEN_SPSPS2      EQU  H'0002'
NACK_BBMPS0      EQU  H'0003'
GCEN_BBMPS1      EQU  H'0004'
AEN_BBMPS2       EQU  H'0005'
SLWRT_BBM        EQU  H'0006'
TP               EQU  H'0007'

AACS             EQU  H'0000'
BACS             EQU  H'0001'
CKEN             EQU  H'0002'
NACK             EQU  H'0003'
GCEN             EQU  H'0004'
AEN              EQU  H'0005'
SLWRT            EQU  H'0006'

SPSPS0           EQU  H'0000'
SPSPS1           EQU  H'0001'
SPSPS2           EQU  H'0002'
BBMPS0           EQU  H'0003'
BBMPS1           EQU  H'0004'
BBMPS2           EQU  H'0005'
BBM              EQU  H'0006'


;----- ASICON1 Bits -----------------------------------------------------
BIE              EQU  H'0000'
SRIE             EQU  H'0001'
RSIE             EQU  H'0002'
SIE              EQU  H'0003'
PIE              EQU  H'0004'

ASIMOD2          EQU  H'0006'
ASIMOD1          EQU  H'0007'


;----- ASISTAT Bits -----------------------------------------------------
BF               EQU  H'0000'
SRF              EQU  H'0001'
RSF              EQU  H'0002'
SF               EQU  H'0003'
PF               EQU  H'0004'
R_NOT_W          EQU  H'0005'
D_NOT_A          EQU  H'0006'
ASIOV            EQU  H'0007'

R_W              EQU  H'0005'
D_A              EQU  H'0006'


;----- T2CON Bits -----------------------------------------------------
T2PSA            EQU  H'0003'
T2ON             EQU  H'0007'

T2PS0            EQU  H'0000'
T2PS1            EQU  H'0001'
T2PS2            EQU  H'0002'


;----- T1CON Bits -----------------------------------------------------
T1PSA            EQU  H'0003'
T1ON             EQU  H'0007'

T1PS0            EQU  H'0000'
T1PS1            EQU  H'0001'
T1PS2            EQU  H'0002'


;----- RCON Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'
NOT_PD           EQU  H'0002'
NOT_TO           EQU  H'0003'
NOT_RI           EQU  H'0004'
GPB              EQU  H'0006'
IPEN             EQU  H'0007'

BOR              EQU  H'0000'
POR              EQU  H'0001'
PD               EQU  H'0002'
TO               EQU  H'0003'
RI               EQU  H'0004'


;----- WDTCON Bits -----------------------------------------------------
SWDTEN           EQU  H'0000'


;----- OSCCON Bits -----------------------------------------------------
OSTS             EQU  H'0003'
IDLEN            EQU  H'0007'

SCS0             EQU  H'0000'
SCS1             EQU  H'0001'
SCS2             EQU  H'0002'


;----- T0CON Bits -----------------------------------------------------
T0PSA            EQU  H'0003'
T0RD16           EQU  H'0005'
T08BIT           EQU  H'0006'
TMR0ON           EQU  H'0007'

T0PS0            EQU  H'0000'
T0PS1            EQU  H'0001'
T0PS2            EQU  H'0002'


;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
OV               EQU  H'0003'
N                EQU  H'0004'


;----- INTCON2 Bits -----------------------------------------------------
RBIP             EQU  H'0000'
TMR0IP           EQU  H'0002'
INTEDG0          EQU  H'0006'
NOT_RBPU         EQU  H'0007'

T0IP             EQU  H'0002'
RBPU             EQU  H'0007'


;----- INTCON Bits -----------------------------------------------------
RBIF             EQU  H'0000'
INT0IF           EQU  H'0001'
TMR0IF           EQU  H'0002'
RBIE             EQU  H'0003'
INT0IE           EQU  H'0004'
TMR0IE           EQU  H'0005'
PEIE_GIEL        EQU  H'0006'
GIE_GIEH         EQU  H'0007'

INT0F            EQU  H'0001'
T0IF             EQU  H'0002'
INT0E            EQU  H'0004'
T0IE             EQU  H'0005'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'

GIEL             EQU  H'0006'
GIEH             EQU  H'0007'


;----- STKPTR Bits -----------------------------------------------------
STKUNF           EQU  H'0006'
STKOVF           EQU  H'0007'

STKFUL           EQU  H'0007'



;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'0FFF'
       __BADRAM  H'0200'-H'0DFF'
       __BADRAM  H'0E40'-H'0F7F'
       __BADRAM  H'0F82'-H'0F89'
       __BADRAM  H'0F8B'-H'0F90'
       __BADRAM  H'0F92'
       __BADRAM  H'0F94'-H'0F99'
       __BADRAM  H'0FA3'-H'0FA5'
       __BADRAM  H'0FA8'-H'0FB0'
       __BADRAM  H'0FB3'
       __BADRAM  H'0FB5'-H'0FBF'
       __BADRAM  H'0FCB'
       __BADRAM  H'0FCF'
       __BADRAM  H'0FD2'
       __BADRAM  H'0FD4'
       __BADRAM  H'0FF0'

;==========================================================================
;
;   IMPORTANT: For the PIC18 devices, the __CONFIG directive has been
;              superseded by the CONFIG directive.  The following settings
;              are available for this device.
;
;   Watchdog Timer:
;     WDT = OFF            Disabled-Controlled by SWDTEN bit
;     WDT = ON             Enabled
;
;   Master Clear Enable:
;     MCLRE = OFF          MCLR disabled, RA5 input enabled
;     MCLRE = ON           MCLR enabled, RA5 input disabled
;
;   Stack Overflow Reset:
;     STVR = OFF           Disabled
;     STVR = ON            Enabled
;
;   Code Protect 000200-001FFF:
;     CP01 = ON            Enabled
;     CP01 = OFF           Disabled
;
;   Code Protect Boot 000000-0001FF:
;     CPB = ON             Enabled
;     CPB = OFF            Disabled
;
;   Table Write Protect 000200-001FFF:
;     WRT01 = ON           Enabled
;     WRT01 = OFF          Disabled
;
;   Config. Write Protect 300000-3000FF:
;     WRTC = ON            Enabled
;     WRTC = OFF           Disabled
;
;   Table Write Protect Boot 000000-0001FF:
;     WRTB = ON            Enabled
;     WRTB = OFF           Disabled
;
;   Table Read Protect 000200-001FFF:
;     EBTR01 = ON          Enabled
;     EBTR01 = OFF         Disabled
;
;   Table Read Protect Boot 000000-0001FF:
;     EBTRB = ON           Enabled
;     EBTRB = OFF          Disabled
;
;==========================================================================
;==========================================================================
;
;       Configuration Bits
;
;   NAME            Address
;   CONFIG2H        300003h
;   CONFIG3H        300005h
;   CONFIG4L        300006h
;   CONFIG5L        300008h
;   CONFIG5H        300009h
;   CONFIG6L        30000Ah
;   CONFIG6H        30000Bh
;   CONFIG7L        30000Ch
;   CONFIG7H        30000Dh
;
;==========================================================================

; The following is an assignment of address values for all of the
; configuration registers for the purpose of table reads
_CONFIG2H       EQU  H'300003'
_CONFIG3H       EQU  H'300005'
_CONFIG4L       EQU  H'300006'
_CONFIG5L       EQU  H'300008'
_CONFIG5H       EQU  H'300009'
_CONFIG6L       EQU  H'30000A'
_CONFIG6H       EQU  H'30000B'
_CONFIG7L       EQU  H'30000C'
_CONFIG7H       EQU  H'30000D'

;----- CONFIG2H Options --------------------------------------------------
_WDT_OFF_2H          EQU  H'FE'; Disabled-Controlled by SWDTEN bit
_WDT_ON_2H           EQU  H'FF'; Enabled

;----- CONFIG3H Options --------------------------------------------------
_MCLRE_OFF_3H        EQU  H'7F'; MCLR disabled, RA5 input enabled
_MCLRE_ON_3H         EQU  H'FF'; MCLR enabled, RA5 input disabled

;----- CONFIG4L Options --------------------------------------------------
_STVR_OFF_4L         EQU  H'FE'; Disabled
_STVR_ON_4L          EQU  H'FF'; Enabled

;----- CONFIG5L Options --------------------------------------------------
_CP01_ON_5L          EQU  H'FE'; Enabled
_CP01_OFF_5L         EQU  H'FF'; Disabled

;----- CONFIG5H Options --------------------------------------------------
_CPB_ON_5H           EQU  H'BF'; Enabled
_CPB_OFF_5H          EQU  H'FF'; Disabled

;----- CONFIG6L Options --------------------------------------------------
_WRT01_ON_6L         EQU  H'FE'; Enabled
_WRT01_OFF_6L        EQU  H'FF'; Disabled

;----- CONFIG6H Options --------------------------------------------------
_WRTC_ON_6H          EQU  H'DF'; Enabled
_WRTC_OFF_6H         EQU  H'FF'; Disabled

_WRTB_ON_6H          EQU  H'BF'; Enabled
_WRTB_OFF_6H         EQU  H'FF'; Disabled

;----- CONFIG7L Options --------------------------------------------------
_EBTR01_ON_7L        EQU  H'FE'; Enabled
_EBTR01_OFF_7L       EQU  H'FF'; Disabled

;----- CONFIG7H Options --------------------------------------------------
_EBTRB_ON_7H         EQU  H'BF'; Enabled
_EBTRB_OFF_7H        EQU  H'FF'; Disabled


;----- DEVID Equates --------------------------------------------------
_DEVID1          EQU  H'3FFFFE'
_DEVID2          EQU  H'3FFFFF'

;----- IDLOC Equates --------------------------------------------------
_IDLOC0          EQU  H'200000'
_IDLOC1          EQU  H'200001'
_IDLOC2          EQU  H'200002'
_IDLOC3          EQU  H'200003'
_IDLOC4          EQU  H'200004'
_IDLOC5          EQU  H'200005'
_IDLOC6          EQU  H'200006'
_IDLOC7          EQU  H'200007'

        LIST
