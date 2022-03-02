        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP25055 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP25055 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP25055
;       2. LIST directive in the source file
;               LIST   P=MCP25055
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP25055
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
GPIO             EQU  H'0005'
PCLATH           EQU  H'000A'
INTCON           EQU  H'000B'
PIR1             EQU  H'000C'
ADRES            EQU  H'001E'
ADCON0           EQU  H'001F'

;-----Bank1------------------
OPTION_REG       EQU  H'0081'
; TRIS is a reserved word
; TRIS             EQU  H'0085'
TRISIO           EQU  H'0085'
PIE1             EQU  H'008C'
PCON             EQU  H'008E'
OSCCAL           EQU  H'008F'
ADCON1           EQU  H'009F'

;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
NOT_PD           EQU  H'0003'
NOT_TO           EQU  H'0004'
IRP              EQU  H'0007'

RP0              EQU  H'0005'
RP1              EQU  H'0006'


;----- GPIO Bits -----------------------------------------------------
GP0              EQU  H'0000'
GP1              EQU  H'0001'
GP2              EQU  H'0002'
GP3              EQU  H'0003'
GP4              EQU  H'0004'
GP5              EQU  H'0005'


;----- INTCON Bits -----------------------------------------------------
GPIF             EQU  H'0000'
INTF             EQU  H'0001'
T0IF             EQU  H'0002'
GPIE             EQU  H'0003'
INTE             EQU  H'0004'
T0IE             EQU  H'0005'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- PIR1 Bits -----------------------------------------------------
ADIF             EQU  H'0006'


;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0002'

GO_DONE          EQU  H'0002'
CHS0             EQU  H'0003'
CHS1             EQU  H'0004'
ADCS0            EQU  H'0006'
ADCS1            EQU  H'0007'

NOT_DONE         EQU  H'0002'

GO               EQU  H'0002'


;----- OPTION_REG Bits -----------------------------------------------------
PSA              EQU  H'0003'
T0SE             EQU  H'0004'
T0CS             EQU  H'0005'
INTEDG           EQU  H'0006'
NOT_GPPU         EQU  H'0007'

PS0              EQU  H'0000'
PS1              EQU  H'0001'
PS2              EQU  H'0002'


;----- TRIS Bits -----------------------------------------------------
TRIS0            EQU  H'0000'
TRIS1            EQU  H'0001'
TRIS2            EQU  H'0002'
TRIS3            EQU  H'0003'
TRIS4            EQU  H'0004'
TRIS5            EQU  H'0005'


;----- TRISIO Bits -----------------------------------------------------
TRIS0            EQU  H'0000'
TRIS1            EQU  H'0001'
TRIS2            EQU  H'0002'
TRIS3            EQU  H'0003'
TRIS4            EQU  H'0004'
TRIS5            EQU  H'0005'


;----- PIE1 Bits -----------------------------------------------------
ADIE             EQU  H'0006'


;----- PCON Bits -----------------------------------------------------
NOT_POR          EQU  H'0001'


;----- OSCCAL Bits -----------------------------------------------------
CALSLW           EQU  H'0002'
CALFST           EQU  H'0003'

CAL0             EQU  H'0004'
CAL1             EQU  H'0005'
CAL2             EQU  H'0006'
CAL3             EQU  H'0007'


;----- ADCON1 Bits -----------------------------------------------------
PCFG0            EQU  H'0000'
PCFG1            EQU  H'0001'
PCFG2            EQU  H'0002'




;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'00FF'
       __BADRAM  H'0006'-H'0009'
       __BADRAM  H'000D'-H'001D'
       __BADRAM  H'0086'-H'0089'
       __BADRAM  H'008D'
       __BADRAM  H'0090'-H'009E'

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
_OSC_LP              EQU  H'3FFC'; LP
_OSC_XT              EQU  H'3FFD'; XT
_OSC_HS              EQU  H'3FFF'; HS

_RSTEN_OFF           EQU  H'3FFB'; GP7 is general I/O
_RSTEN_ON            EQU  H'3FFF'; Reset enabled on GP7 I/O pin



        LIST
