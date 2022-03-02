        LIST

;==========================================================================
; Build date : Oct 24 2019
;  MPASM MCP19132 processor include
; 
;  (c) Copyright 1999-2019 Microchip Technology, All rights reserved
;==========================================================================

        NOLIST

;==========================================================================
;  This header file defines configurations, registers, and other useful
;  bits of information for the MCP19132 microcontroller.  These names
;  are taken to match the data sheets as closely as possible.
;
;  Note that the processor must be selected before this file is included.
;  The processor may be selected the following ways:
;
;       1. Command line switch:
;               C:\MPASM MYFILE.ASM /MCP19132
;       2. LIST directive in the source file
;               LIST   P=MCP19132
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog
;==========================================================================

;==========================================================================
;
;       Verify Processor
;
;==========================================================================
        IFNDEF __MCP19132
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
TRISA            EQU  H'0011'
TRISB            EQU  H'0012'
TRISC            EQU  H'0013'
LATA             EQU  H'0016'
LATB             EQU  H'0017'
LATC             EQU  H'0018'
TMR0L            EQU  H'001C'
TMR0H            EQU  H'001D'
T0CON0           EQU  H'001E'
T0CON1           EQU  H'001F'

;-----Bank1------------------
ADRES            EQU  H'008C'
ADRESL           EQU  H'008C'
ADRESH           EQU  H'008D'
ADCON0           EQU  H'008E'
ADCON1           EQU  H'008F'
ADACT            EQU  H'0090'

;-----Bank2------------------
RC1REG           EQU  H'0119'
RCREG            EQU  H'0119'
RCREG1           EQU  H'0119'
TX1REG           EQU  H'011A'
TXREG            EQU  H'011A'
TXREG1           EQU  H'011A'
SP1BRG           EQU  H'011B'
SP1BRGL          EQU  H'011B'
SPBRG            EQU  H'011B'
SPBRG1           EQU  H'011B'
SPBRGL           EQU  H'011B'
SP1BRGH          EQU  H'011C'
SPBRGH           EQU  H'011C'
SPBRGH1          EQU  H'011C'
RC1STA           EQU  H'011D'
RCSTA            EQU  H'011D'
RCSTA1           EQU  H'011D'
TX1STA           EQU  H'011E'
TXSTA            EQU  H'011E'
TXSTA1           EQU  H'011E'
BAUD1CON         EQU  H'011F'
BAUDCON          EQU  H'011F'
BAUDCON1         EQU  H'011F'
BAUDCTL          EQU  H'011F'
BAUDCTL1         EQU  H'011F'

;-----Bank3------------------
SSP1BUF          EQU  H'018C'
SSP1ADD          EQU  H'018D'
SSP1MSK          EQU  H'018E'
SSP1STAT         EQU  H'018F'
SSP1CON1         EQU  H'0190'
SSP1CON2         EQU  H'0191'
SSP1CON3         EQU  H'0192'
SSP1ADD2         EQU  H'0193'
SSP1MSK2         EQU  H'0194'

;-----Bank4------------------
TMR1L            EQU  H'020C'
TMR1H            EQU  H'020D'
T1CON            EQU  H'020E'
T1GCON           EQU  H'020F'
T1GATE           EQU  H'0210'
T1CLK            EQU  H'0211'
TMR3L            EQU  H'0212'
TMR3H            EQU  H'0213'
T3CON            EQU  H'0214'
T3GCON           EQU  H'0215'
T3GATE           EQU  H'0216'
T3CLK            EQU  H'0217'
CCPTMRS0         EQU  H'021E'

;-----Bank5------------------
T2TMR            EQU  H'028C'
T2PR             EQU  H'028D'
T2CON            EQU  H'028E'
T2HLT            EQU  H'028F'
T2CLKCON         EQU  H'0290'
T2RST            EQU  H'0291'
T4TMR            EQU  H'0292'
T4PR             EQU  H'0293'
T4CON            EQU  H'0294'
T4HLT            EQU  H'0295'
T4CLKCON         EQU  H'0296'
T4RST            EQU  H'0297'

;-----Bank6------------------
CCP1RL           EQU  H'030C'
CCP1RH           EQU  H'030D'
CCP1CON          EQU  H'030E'
CCP1CAP          EQU  H'030F'
CCP2RL           EQU  H'0310'
CCP2RH           EQU  H'0311'
CCP2CON          EQU  H'0312'
CCP2CAP          EQU  H'0313'

;-----Bank7------------------
SWFRQCON         EQU  H'038C'
SWFRQPR          EQU  H'038D'
SWFRQ            EQU  H'038E'
SWFRQDITHER      EQU  H'038F'
SWFRQPWH         EQU  H'0390'
SWFRQPWL         EQU  H'0391'
SWFRQDPS         EQU  H'0392'
DRVCON0          EQU  H'0397'
DRVCON1          EQU  H'0398'
DRVCON2          EQU  H'0399'
DEADCON          EQU  H'039A'
DEMCON           EQU  H'039B'
FVRCON           EQU  H'039C'
TTASLOPE         EQU  H'039D'
TTAOFFSET        EQU  H'039E'
TSTBUFCON        EQU  H'039F'

;-----Bank8------------------
SCANLADRL        EQU  H'040C'
SCANLADRH        EQU  H'040D'
SCANHADRL        EQU  H'040E'
SCANHADRH        EQU  H'040F'
SCANCON0         EQU  H'0410'
SCANDTRIG        EQU  H'0411'
CRCDATA          EQU  H'0416'
CRCDATL          EQU  H'0416'
CRCDATH          EQU  H'0417'
CRCACC           EQU  H'0418'
CRCACCL          EQU  H'0418'
CRCACCH          EQU  H'0419'
CRCSHFT          EQU  H'041A'
CRCSHIFTL        EQU  H'041A'
CRCSHIFTH        EQU  H'041B'
CRCXOR           EQU  H'041C'
CRCXORL          EQU  H'041C'
CRCXORH          EQU  H'041D'
CRCCON0          EQU  H'041E'
CRCCON1          EQU  H'041F'

;-----Bank10------------------
IVMRAWRESL       EQU  H'050D'
IVMRAWRESH       EQU  H'050E'
IVMCON0          EQU  H'050F'
IVMCON1          EQU  H'0510'
IVMCON2          EQU  H'0511'
ICMCON1          EQU  H'0512'
IVMUVR           EQU  H'0513'
IVMUVF           EQU  H'0514'
IVMOVR           EQU  H'0515'
IVMOVF           EQU  H'0516'
IVMOVUVOUT       EQU  H'0517'
IVMCORRESL       EQU  H'0518'
IVMCORRESH       EQU  H'0519'
ICMOCCON         EQU  H'051A'
ICMOFF           EQU  H'051B'
ICMGAIN          EQU  H'051C'

;-----Bank11------------------
OCMRAWRESL       EQU  H'058D'
OCMRAWRESH       EQU  H'058E'
OCMCON0          EQU  H'058F'
OCMCON1          EQU  H'0590'
OCMCON2          EQU  H'0591'
OCMUCR           EQU  H'0593'
OCMUCF           EQU  H'0594'
OCMOCR           EQU  H'0595'
OCMOCF           EQU  H'0596'
OCMOCUCOUT       EQU  H'0597'
OCMCORRESL       EQU  H'0598'
OCMCORRESH       EQU  H'0599'
LPFCON0          EQU  H'059A'
OCROFF           EQU  H'059B'
OCRGAIN          EQU  H'059C'

;-----Bank12------------------
OVMRAWRESL       EQU  H'060D'
OVMRAWRESH       EQU  H'060E'
OVMCON0          EQU  H'060F'
OVMCON1          EQU  H'0610'
OVMCON2          EQU  H'0611'
OVMCON3          EQU  H'0612'
OVMUVR           EQU  H'0613'
OVMUVF           EQU  H'0614'
OVMOVR           EQU  H'0615'
OVMOVF           EQU  H'0616'
OVMOVUVOUT       EQU  H'0617'
OVMCORRESL       EQU  H'0618'
OVMCORRESH       EQU  H'0619'

;-----Bank14------------------
PIR0             EQU  H'070C'
PIR1             EQU  H'070D'
PIR2             EQU  H'070E'
PIR3             EQU  H'070F'
PIR4             EQU  H'0710'
PIR5             EQU  H'0711'
PIR6             EQU  H'0712'
PIR7             EQU  H'0713'
PIE0             EQU  H'0716'
PIE1             EQU  H'0717'
PIE2             EQU  H'0718'
PIE3             EQU  H'0719'
PIE4             EQU  H'071A'
PIE5             EQU  H'071B'
PIE6             EQU  H'071C'
PIE7             EQU  H'071D'

;-----Bank15------------------
PMD0             EQU  H'0796'
PMD1             EQU  H'0797'
PMD2             EQU  H'0798'
PMD3             EQU  H'0799'
PMD4             EQU  H'079A'

;-----Bank16------------------
WDTCON0          EQU  H'080C'
WDTCON1          EQU  H'080D'
WDTPSL           EQU  H'080E'
WDTPSH           EQU  H'080F'
WDTTMR           EQU  H'0810'
BORCON           EQU  H'0811'
VREGCON          EQU  H'0812'
PCON0            EQU  H'0813'
CCDCON           EQU  H'0814'
NVMADRL          EQU  H'081A'
NVMADRH          EQU  H'081B'
NVMDATL          EQU  H'081C'
NVMDATH          EQU  H'081D'
NVMCON1          EQU  H'081E'
NVMCON2          EQU  H'081F'

;-----Bank17------------------
CPUDOZE          EQU  H'088C'
OSCCON1          EQU  H'088D'
OSCCON2          EQU  H'088E'
OSCCON3          EQU  H'088F'
OSCSTAT1         EQU  H'0890'
OSCEN            EQU  H'0891'
OSCTUNE          EQU  H'0892'
OSCFREQ          EQU  H'0893'

;-----Bank18------------------
DCOMPCON0        EQU  H'090F'
DCOMPCON1        EQU  H'0910'
DCOMPCON2        EQU  H'0911'
VREFH            EQU  H'0913'
VREFL            EQU  H'0914'
IREFH            EQU  H'0916'
IREFL            EQU  H'0917'
DOUTH            EQU  H'0919'
DOUTL            EQU  H'091A'

;-----Bank19------------------
KPI              EQU  H'098F'
KVI              EQU  H'0990'
KPV              EQU  H'0991'
KVV              EQU  H'0992'
KIV              EQU  H'0993'
MAXDC            EQU  H'0995'
XI               EQU  H'0996'
YI               EQU  H'0997'
XV               EQU  H'0998'
YV               EQU  H'0999'
NS               EQU  H'099A'
NP               EQU  H'099B'
RLOAD            EQU  H'099C'
RLOSS            EQU  H'099D'

;-----Bank27------------------
TST_DMAH         EQU  H'0D8C'
TST_DMAL         EQU  H'0D8D'
TST_ISMSTAT      EQU  H'0D90'
TST_ISMREAD      EQU  H'0D91'
TST_PERDOVR      EQU  H'0D92'
TST_PIBREAD      EQU  H'0D96'

;-----Bank28------------------
UMTOAP           EQU  H'0E0C'
UMTOAL           EQU  H'0E0D'
UMTOAH           EQU  H'0E0E'
CLCDATA          EQU  H'0E0F'
CLC1CON          EQU  H'0E10'
CLC1POL          EQU  H'0E11'
CLC1SEL0         EQU  H'0E12'
CLC1SEL1         EQU  H'0E13'
CLC1SEL2         EQU  H'0E14'
CLC1SEL3         EQU  H'0E15'
CLC1GLS0         EQU  H'0E16'
CLC1GLS1         EQU  H'0E17'
CLC1GLS2         EQU  H'0E18'
CLC1GLS3         EQU  H'0E19'
CLC2CON          EQU  H'0E1A'
CLC2POL          EQU  H'0E1B'
CLC2SEL0         EQU  H'0E1C'
CLC2SEL1         EQU  H'0E1D'
CLC2SEL2         EQU  H'0E1E'
CLC2SEL3         EQU  H'0E1F'
CLC2GLS0         EQU  H'0E20'
CLC2GLS1         EQU  H'0E21'
CLC2GLS2         EQU  H'0E22'
CLC2GLS3         EQU  H'0E23'

;-----Bank29------------------
PPSLOCK          EQU  H'0E8F'
INTPPS           EQU  H'0E90'
T0CKIPPS         EQU  H'0E91'
T1CKIPPS         EQU  H'0E92'
T1GPPS           EQU  H'0E93'
T3CKIPPS         EQU  H'0E94'
T3GPPS           EQU  H'0E95'
T2AINPPS         EQU  H'0E9C'
T4AINPPS         EQU  H'0E9D'
CCP1PPS          EQU  H'0EA1'
CCP2PPS          EQU  H'0EA2'
CLCIN0PPS        EQU  H'0EBB'
CLCIN1PPS        EQU  H'0EBC'
CLCIN2PPS        EQU  H'0EBD'
CLCIN3PPS        EQU  H'0EBE'
ADCACTPPS        EQU  H'0EC3'
DCPHLDPPS        EQU  H'0EC4'
SSP1CLKPPS       EQU  H'0EC5'
SSP1DATPPS       EQU  H'0EC6'
SSP1SSPPS        EQU  H'0EC7'
RXDTPPS          EQU  H'0ECB'
CKTXPPS          EQU  H'0ECC'
TST_PPSIN1       EQU  H'0ED0'
TST_PPSIN2       EQU  H'0ED1'
TST_PPSIN3       EQU  H'0ED2'
TST_PPSIN4       EQU  H'0ED3'
TST_PPSDOV       EQU  H'0ED8'
TST_PPSEOV       EQU  H'0ED9'

;-----Bank30------------------
RA0PPS           EQU  H'0F10'
RA1PPS           EQU  H'0F11'
RA2PPS           EQU  H'0F12'
RA3PPS           EQU  H'0F13'
RB0PPS           EQU  H'0F18'
RB1PPS           EQU  H'0F19'
RB2PPS           EQU  H'0F1A'
RB3PPS           EQU  H'0F1B'
RB4PPS           EQU  H'0F1C'
RB5PPS           EQU  H'0F1D'
RB6PPS           EQU  H'0F1E'
RB7PPS           EQU  H'0F1F'
RC4PPS           EQU  H'0F24'
RC5PPS           EQU  H'0F25'
RC6PPS           EQU  H'0F26'
ANSELA           EQU  H'0F38'
WPUA             EQU  H'0F39'
ODCONA           EQU  H'0F3A'
SLRCONA          EQU  H'0F3B'
INLVLA           EQU  H'0F3C'
IOCAP            EQU  H'0F3D'
IOCAN            EQU  H'0F3E'
IOCAF            EQU  H'0F3F'
CCDNA            EQU  H'0F40'
CCDPA            EQU  H'0F41'
ANSELB           EQU  H'0F43'
WPUB             EQU  H'0F44'
ODCONB           EQU  H'0F45'
SLRCONB          EQU  H'0F46'
INLVLB           EQU  H'0F47'
IOCBP            EQU  H'0F48'
IOCBN            EQU  H'0F49'
IOCBF            EQU  H'0F4A'
WPUC             EQU  H'0F4F'
INLVLC           EQU  H'0F52'
IOCCP            EQU  H'0F53'
IOCCN            EQU  H'0F54'
IOCCF            EQU  H'0F55'

;-----Bank31------------------
ICDIO            EQU  H'0F8C'
ICDCON0          EQU  H'0F8D'
ICDDC            EQU  H'0F8E'
ICDSTAT          EQU  H'0F91'
CLKRFRZ          EQU  H'0F92'
ICDINSTL         EQU  H'0F96'
ICDINSTH         EQU  H'0F97'
ICDBP0AL         EQU  H'0F98'
ICDBP0AH         EQU  H'0F99'
ICDBP0D          EQU  H'0F9B'
ICDBP0CON        EQU  H'0F9C'
ICDBP0CNT        EQU  H'0F9D'
BSR_ICDSHAD      EQU  H'0FE3'
STATUS_SHAD      EQU  H'0FE4'
WREG_SHAD        EQU  H'0FE5'
BSR_SHAD         EQU  H'0FE6'
PCLATH_SHAD      EQU  H'0FE7'
FSR0L_SHAD       EQU  H'0FE8'
FSR0H_SHAD       EQU  H'0FE9'
FSR1L_SHAD       EQU  H'0FEA'
FSR1H_SHAD       EQU  H'0FEB'
STKPTR           EQU  H'0FED'
TOSL             EQU  H'0FEE'
TOSH             EQU  H'0FEF'

;----- STATUS Bits -----------------------------------------------------
C                EQU  H'0000'
DC               EQU  H'0001'
Z                EQU  H'0002'
NOT_PD           EQU  H'0003'
NOT_TO           EQU  H'0004'


;----- BSR Bits -----------------------------------------------------
BSR0             EQU  H'0000'
BSR1             EQU  H'0001'
BSR2             EQU  H'0002'
BSR3             EQU  H'0003'
BSR4             EQU  H'0004'


;----- INTCON Bits -----------------------------------------------------
INTEDG           EQU  H'0000'
PEIE             EQU  H'0006'
GIE              EQU  H'0007'


;----- PORTA Bits -----------------------------------------------------
RA0              EQU  H'0000'
RA1              EQU  H'0001'
RA2              EQU  H'0002'
RA3              EQU  H'0003'


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
RC4              EQU  H'0004'
RC5              EQU  H'0005'
RC6              EQU  H'0006'
RC7              EQU  H'0007'


;----- TRISA Bits -----------------------------------------------------
TRISA0           EQU  H'0000'
TRISA1           EQU  H'0001'
TRISA2           EQU  H'0002'
TRISA3           EQU  H'0003'


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
TRISC4           EQU  H'0004'
TRISC5           EQU  H'0005'
TRISC6           EQU  H'0006'
TRISC7           EQU  H'0007'


;----- LATA Bits -----------------------------------------------------
LATA0            EQU  H'0000'
LATA1            EQU  H'0001'
LATA2            EQU  H'0002'
LATA3            EQU  H'0003'


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
LATC4            EQU  H'0004'
LATC5            EQU  H'0005'
LATC6            EQU  H'0006'


;----- T0CON0 Bits -----------------------------------------------------
T016BIT          EQU  H'0004'
T0OUT            EQU  H'0005'
T0EN             EQU  H'0007'

T0OUTPS0         EQU  H'0000'
T0OUTPS1         EQU  H'0001'
T0OUTPS2         EQU  H'0002'
T0OUTPS3         EQU  H'0003'


;----- T0CON1 Bits -----------------------------------------------------
T0ASYNC          EQU  H'0004'

T0CKPS0          EQU  H'0000'
T0CKPS1          EQU  H'0001'
T0CKPS2          EQU  H'0002'
T0CKPS3          EQU  H'0003'
T0CS0            EQU  H'0005'
T0CS1            EQU  H'0006'
T0CS2            EQU  H'0007'


;----- ADCON0 Bits -----------------------------------------------------
ADON             EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'

CHS0             EQU  H'0002'
CHS1             EQU  H'0003'
CHS2             EQU  H'0004'
CHS3             EQU  H'0005'
CHS4             EQU  H'0006'


;----- ADCON1 Bits -----------------------------------------------------
ADPREF           EQU  H'0000'
ADFM             EQU  H'0007'

ADCS0            EQU  H'0004'
ADCS1            EQU  H'0005'
ADCS2            EQU  H'0006'


;----- ADACT Bits -----------------------------------------------------
ADACT0           EQU  H'0000'
ADACT1           EQU  H'0001'
ADACT2           EQU  H'0002'
ADACT3           EQU  H'0003'


;----- RC1STA Bits -----------------------------------------------------
RX9D             EQU  H'0000'
OERR             EQU  H'0001'
FERR             EQU  H'0002'
ADDEN            EQU  H'0003'
CREN             EQU  H'0004'
SREN             EQU  H'0005'
RX9              EQU  H'0006'
SPEN             EQU  H'0007'


;----- RCSTA Bits -----------------------------------------------------
RX9D             EQU  H'0000'
OERR             EQU  H'0001'
FERR             EQU  H'0002'
ADDEN            EQU  H'0003'
CREN             EQU  H'0004'
SREN             EQU  H'0005'
RX9              EQU  H'0006'
SPEN             EQU  H'0007'


;----- RCSTA1 Bits -----------------------------------------------------
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
SYNC             EQU  H'0004'
TXEN             EQU  H'0005'
TX9              EQU  H'0006'
CSRC             EQU  H'0007'


;----- TXSTA Bits -----------------------------------------------------
TX9D             EQU  H'0000'
TRMT             EQU  H'0001'
BRGH             EQU  H'0002'
SENDB            EQU  H'0003'
SYNC             EQU  H'0004'
TXEN             EQU  H'0005'
TX9              EQU  H'0006'
CSRC             EQU  H'0007'


;----- TXSTA1 Bits -----------------------------------------------------
TX9D             EQU  H'0000'
TRMT             EQU  H'0001'
BRGH             EQU  H'0002'
SENDB            EQU  H'0003'
SYNC             EQU  H'0004'
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


;----- BAUDCON Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- BAUDCON1 Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- BAUDCTL Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


;----- BAUDCTL1 Bits -----------------------------------------------------
ABDEN            EQU  H'0000'
WUE              EQU  H'0001'
BRG16            EQU  H'0003'
SCKP             EQU  H'0004'
RCIDL            EQU  H'0006'
ABDOVF           EQU  H'0007'


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
R_NOT_W          EQU  H'0002'
S                EQU  H'0003'
P                EQU  H'0004'
D_NOT_A          EQU  H'0005'
CKE              EQU  H'0006'
SMP              EQU  H'0007'

R_W              EQU  H'0002'
D_A              EQU  H'0005'

NOT_W            EQU  H'0002'
NOT_A            EQU  H'0005'

NOT_WRITE        EQU  H'0002'
NOT_ADDRESS      EQU  H'0005'

READ_WRITE       EQU  H'0002'
DATA_ADDRESS     EQU  H'0005'

I2C_READ         EQU  H'0002'
I2C_START        EQU  H'0003'
I2C_STOP         EQU  H'0004'
I2C_DAT          EQU  H'0005'


;----- SSP1CON1 Bits -----------------------------------------------------
CKP              EQU  H'0004'
SSPEN            EQU  H'0005'
SSPOV            EQU  H'0006'
WCOL             EQU  H'0007'

SSPM0            EQU  H'0000'
SSPM1            EQU  H'0001'
SSPM2            EQU  H'0002'
SSPM3            EQU  H'0003'


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


;----- SSP1CON3 Bits -----------------------------------------------------
DHEN             EQU  H'0000'
AHEN             EQU  H'0001'
SBCDE            EQU  H'0002'
SDAHT            EQU  H'0003'
BOEN             EQU  H'0004'
SCIE             EQU  H'0005'
PCIE             EQU  H'0006'
ACKTIM           EQU  H'0007'


;----- SSP1ADD2 Bits -----------------------------------------------------
SSPADD0          EQU  H'0000'
SSPADD1          EQU  H'0001'
SSPADD2          EQU  H'0002'
SSPADD3          EQU  H'0003'
SSPADD4          EQU  H'0004'
SSPADD5          EQU  H'0005'
SSPADD6          EQU  H'0006'
SSPADD7          EQU  H'0007'


;----- SSP1MSK2 Bits -----------------------------------------------------
SSPMSK0          EQU  H'0000'
SSPMSK1          EQU  H'0001'
SSPMSK2          EQU  H'0002'
SSPMSK3          EQU  H'0003'
SSPMSK4          EQU  H'0004'
SSPMSK5          EQU  H'0005'
SSPMSK6          EQU  H'0006'
SSPMSK7          EQU  H'0007'


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

T1CKPS0          EQU  H'0004'
T1CKPS1          EQU  H'0005'


;----- T1GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO_NOT_DONE     EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL             EQU  H'0006'
GE               EQU  H'0007'


;----- T1GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'


;----- T1CLK Bits -----------------------------------------------------
CS0              EQU  H'0000'
CS1              EQU  H'0001'
CS2              EQU  H'0002'
CS3              EQU  H'0003'


;----- TMR3L Bits -----------------------------------------------------
TMR3L0           EQU  H'0000'
TMR3L1           EQU  H'0001'
TMR3L2           EQU  H'0002'
TMR3L3           EQU  H'0003'
TMR3L4           EQU  H'0004'
TMR3L5           EQU  H'0005'
TMR3L6           EQU  H'0006'
TMR3L7           EQU  H'0007'


;----- TMR3H Bits -----------------------------------------------------
TMR3H0           EQU  H'0000'
TMR3H1           EQU  H'0001'
TMR3H2           EQU  H'0002'
TMR3H3           EQU  H'0003'
TMR3H4           EQU  H'0004'
TMR3H5           EQU  H'0005'
TMR3H6           EQU  H'0006'
TMR3H7           EQU  H'0007'


;----- T3CON Bits -----------------------------------------------------
ON_T3CON         EQU  H'0000'
RD16             EQU  H'0001'
NOT_SYNC         EQU  H'0002'

T3CKPS0          EQU  H'0004'
T3CKPS1          EQU  H'0005'


;----- T3GCON Bits -----------------------------------------------------
GVAL             EQU  H'0002'
GGO_NOT_DONE     EQU  H'0003'
GSPM             EQU  H'0004'
GTM              EQU  H'0005'
GPOL             EQU  H'0006'
GE               EQU  H'0007'


;----- T3GATE Bits -----------------------------------------------------
GSS0             EQU  H'0000'
GSS1             EQU  H'0001'
GSS2             EQU  H'0002'
GSS3             EQU  H'0003'


;----- T3CLK Bits -----------------------------------------------------
CS0              EQU  H'0000'
CS1              EQU  H'0001'
CS2              EQU  H'0002'
CS3              EQU  H'0003'


;----- CCPTMRS0 Bits -----------------------------------------------------
C1TSEL           EQU  H'0000'
C2TSEL           EQU  H'0001'


;----- T2CON Bits -----------------------------------------------------
ON_T2CON         EQU  H'0007'

OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0            EQU  H'0004'
CKPS1            EQU  H'0005'
CKPS2            EQU  H'0006'


;----- T2HLT Bits -----------------------------------------------------
CKSYNC           EQU  H'0005'
CKPOL            EQU  H'0006'
PSYNC            EQU  H'0007'

MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'


;----- T2CLKCON Bits -----------------------------------------------------
CS0              EQU  H'0000'
CS1              EQU  H'0001'
CS2              EQU  H'0002'


;----- T2RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'


;----- T4CON Bits -----------------------------------------------------
ON_T4CON         EQU  H'0007'

OUTPS0           EQU  H'0000'
OUTPS1           EQU  H'0001'
OUTPS2           EQU  H'0002'
OUTPS3           EQU  H'0003'
CKPS0            EQU  H'0004'
CKPS1            EQU  H'0005'
CKPS2            EQU  H'0006'


;----- T4HLT Bits -----------------------------------------------------
T4CSYNC          EQU  H'0005'
T4CPOL           EQU  H'0006'
T4PSYNC          EQU  H'0007'

MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'


;----- T4CLKCON Bits -----------------------------------------------------
CS0              EQU  H'0000'
CS1              EQU  H'0001'
CS2              EQU  H'0002'


;----- T4RST Bits -----------------------------------------------------
RSEL0            EQU  H'0000'
RSEL1            EQU  H'0001'
RSEL2            EQU  H'0002'


;----- CCP1RL Bits -----------------------------------------------------
CCP1PR0          EQU  H'0000'
CCP1PR1          EQU  H'0001'
CCP1PR2          EQU  H'0002'
CCP1PR3          EQU  H'0003'
CCP1PR4          EQU  H'0004'
CCP1PR5          EQU  H'0005'
CCP1PR6          EQU  H'0006'
CCP1PR7          EQU  H'0007'


;----- CCP1RH Bits -----------------------------------------------------
CCP1PR8          EQU  H'0000'
CCP1PR9          EQU  H'0001'
CCP1PR10         EQU  H'0002'
CCP1PR11         EQU  H'0003'
CCP1PR12         EQU  H'0004'
CCP1PR13         EQU  H'0005'
CCP1PR14         EQU  H'0006'
CCP1PR15         EQU  H'0007'


;----- CCP1CON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
FMT              EQU  H'0004'
OUT              EQU  H'0005'
EN               EQU  H'0007'


;----- CCP1CAP Bits -----------------------------------------------------
CCP1CAP0         EQU  H'0000'
CCP1CAP1         EQU  H'0001'
CCP1CAP2         EQU  H'0002'


;----- CCP2RL Bits -----------------------------------------------------
CCP2PR0          EQU  H'0000'
CCP2PR1          EQU  H'0001'
CCP2PR2          EQU  H'0002'
CCP2PR3          EQU  H'0003'
CCP2PR4          EQU  H'0004'
CCP2PR5          EQU  H'0005'
CCP2PR6          EQU  H'0006'
CCP2PR7          EQU  H'0007'


;----- CCP2RH Bits -----------------------------------------------------
CCP2PR8          EQU  H'0000'
CCP2PR9          EQU  H'0001'
CCP2PR10         EQU  H'0002'
CCP2PR11         EQU  H'0003'
CCP2PR12         EQU  H'0004'
CCP2PR13         EQU  H'0005'
CCP2PR14         EQU  H'0006'
CCP2PR15         EQU  H'0007'


;----- CCP2CON Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'
MODE3            EQU  H'0003'
FMT              EQU  H'0004'
OUT              EQU  H'0005'
EN               EQU  H'0007'


;----- CCP2CAP Bits -----------------------------------------------------
CCP2CAP0         EQU  H'0000'
CCP2CAP1         EQU  H'0001'
CCP2CAP2         EQU  H'0002'


;----- SWFRQCON Bits -----------------------------------------------------
SWFRQPS0         EQU  H'0000'
SWFRQPS1         EQU  H'0001'
SWFRQPS2         EQU  H'0002'
PWOUT            EQU  H'0003'
MANUAL           EQU  H'0004'
DLLDIS           EQU  H'0005'
DITHER           EQU  H'0006'
SWFRQON          EQU  H'0007'


;----- SWFRQPWH Bits -----------------------------------------------------
SWFRQPW6         EQU  H'0000'
SWFRQPW7         EQU  H'0001'
SWFRQPW8         EQU  H'0002'
SWFRQPW9         EQU  H'0003'
SWFRQPW10        EQU  H'0004'
SWFRQPW11        EQU  H'0005'
SWFRQPW12        EQU  H'0006'
SWFRQPW13        EQU  H'0007'


;----- SWFRQPWL Bits -----------------------------------------------------
SWFRQPW0         EQU  H'0002'
SWFRQPW1         EQU  H'0003'
SWFRQPW2         EQU  H'0004'
SWFRQPW3         EQU  H'0005'
SWFRQPW4         EQU  H'0006'
SWFRQPW5         EQU  H'0007'


;----- SWFRQDPS Bits -----------------------------------------------------
DPS0             EQU  H'0004'
DPS1             EQU  H'0005'
DPS2             EQU  H'0006'
DRANGE           EQU  H'0007'


;----- DRVCON0 Bits -----------------------------------------------------
DRVMODE0         EQU  H'0000'
DRVMODE1         EQU  H'0001'
DRVMODE2         EQU  H'0002'
DRVCTL           EQU  H'0003'
DTSEL0           EQU  H'0004'
DTSEL1           EQU  H'0005'
LODIS            EQU  H'0006'
HODIS            EQU  H'0007'


;----- DRVCON1 Bits -----------------------------------------------------
IVMUVFEL         EQU  H'0000'
IVMOVFEH         EQU  H'0001'
ICMOCFEL         EQU  H'0002'
OVMUVFEL         EQU  H'0003'
OVMOVFEL         EQU  H'0004'
OCMUCFEL         EQU  H'0005'
OCMOCFEL         EQU  H'0006'
HOLDEL           EQU  H'0007'


;----- DRVCON2 Bits -----------------------------------------------------
IVMUVFEH         EQU  H'0000'
IVMOFEH          EQU  H'0001'
ICMOCFEH         EQU  H'0002'
OVMUVFEH         EQU  H'0003'
OVMOFEH          EQU  H'0004'
OCMUCFEH         EQU  H'0005'
OCMOCEH          EQU  H'0006'
HOLDEH           EQU  H'0007'


;----- DEMCON Bits -----------------------------------------------------
DEMPOL           EQU  H'0004'
DEMOUT           EQU  H'0005'
DEMEN            EQU  H'0007'


;----- FVRCON Bits -----------------------------------------------------
FVRINTN          EQU  H'0000'
FVRINTP          EQU  H'0001'
FVR2OK           EQU  H'0002'
FVR1OK           EQU  H'0003'
TSRNG            EQU  H'0004'
TSEN             EQU  H'0005'
FVRRDY           EQU  H'0006'
FVREN            EQU  H'0007'


;----- TSTBUFCON Bits -----------------------------------------------------
TSTPT0           EQU  H'0000'
TSTPT1           EQU  H'0001'
SLOT0            EQU  H'0002'
SLOT1            EQU  H'0003'
SLOT2            EQU  H'0004'
SLOT3            EQU  H'0005'
TSTMODE0         EQU  H'0006'
TSTMODE1         EQU  H'0007'


;----- SCANLADRL Bits -----------------------------------------------------
LADR0            EQU  H'0000'
LADR1            EQU  H'0001'
LADR2            EQU  H'0002'
LADR3            EQU  H'0003'
LADR4            EQU  H'0004'
LADR5            EQU  H'0005'
LADR6            EQU  H'0006'
LADR7            EQU  H'0007'


;----- SCANLADRH Bits -----------------------------------------------------
LADR8            EQU  H'0000'
LADR9            EQU  H'0001'
LADR10           EQU  H'0002'
LADR11           EQU  H'0003'
LADR12           EQU  H'0004'
LADR13           EQU  H'0005'
LADR14           EQU  H'0006'
LADR15           EQU  H'0007'


;----- SCANHADRL Bits -----------------------------------------------------
HADR0            EQU  H'0000'
HADR1            EQU  H'0001'
HADR2            EQU  H'0002'
HADR3            EQU  H'0003'
HADR4            EQU  H'0004'
HADR5            EQU  H'0005'
HADR6            EQU  H'0006'
HADR7            EQU  H'0007'


;----- SCANHADRH Bits -----------------------------------------------------
HADR8            EQU  H'0000'
HADR9            EQU  H'0001'
HADR10           EQU  H'0002'
HADR11           EQU  H'0003'
HADR12           EQU  H'0004'
HADR13           EQU  H'0005'
HADR14           EQU  H'0006'
HADR15           EQU  H'0007'


;----- SCANCON0 Bits -----------------------------------------------------
MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
INTM             EQU  H'0003'
INVALID          EQU  H'0004'
BUSY             EQU  H'0005'
SCANGO           EQU  H'0006'
EN               EQU  H'0007'


;----- SCANDTRIG Bits -----------------------------------------------------
TSEL0            EQU  H'0000'
TSEL1            EQU  H'0001'
TSEL2            EQU  H'0002'


;----- CRCDATL Bits -----------------------------------------------------
DATA0            EQU  H'0000'
DATA1            EQU  H'0001'
DATA2            EQU  H'0002'
DATA3            EQU  H'0003'
DATA4            EQU  H'0004'
DATA5            EQU  H'0005'
DATA6            EQU  H'0006'
DATA7            EQU  H'0007'


;----- CRCDATH Bits -----------------------------------------------------
DATA8            EQU  H'0000'
DATA9            EQU  H'0001'
DATA10           EQU  H'0002'
DATA11           EQU  H'0003'
DATA12           EQU  H'0004'
DATA13           EQU  H'0005'
DATA14           EQU  H'0006'
DATA15           EQU  H'0007'


;----- CRCACCL Bits -----------------------------------------------------
ACC0             EQU  H'0000'
ACC1             EQU  H'0001'
ACC2             EQU  H'0002'
ACC3             EQU  H'0003'
ACC4             EQU  H'0004'
ACC5             EQU  H'0005'
ACC6             EQU  H'0006'
ACC7             EQU  H'0007'


;----- CRCACCH Bits -----------------------------------------------------
ACC8             EQU  H'0000'
ACC9             EQU  H'0001'
ACC10            EQU  H'0002'
ACC11            EQU  H'0003'
ACC12            EQU  H'0004'
ACC13            EQU  H'0005'
ACC14            EQU  H'0006'
ACC15            EQU  H'0007'


;----- CRCSHIFTL Bits -----------------------------------------------------
SHFT0            EQU  H'0000'
SHFT1            EQU  H'0001'
SHFT2            EQU  H'0002'
SHFT3            EQU  H'0003'
SHFT4            EQU  H'0004'
SHFT5            EQU  H'0005'
SHFT6            EQU  H'0006'
SHFT7            EQU  H'0007'


;----- CRCSHIFTH Bits -----------------------------------------------------
SHFT8            EQU  H'0000'
SHFT9            EQU  H'0001'
SHFT10           EQU  H'0002'
SHFT11           EQU  H'0003'
SHFT12           EQU  H'0004'
SHFT13           EQU  H'0005'
SHFT14           EQU  H'0006'
SHFT15           EQU  H'0007'


;----- CRCXORL Bits -----------------------------------------------------
X1               EQU  H'0001'
X2               EQU  H'0002'
X3               EQU  H'0003'
X4               EQU  H'0004'
X5               EQU  H'0005'
X6               EQU  H'0006'
X7               EQU  H'0007'


;----- CRCXORH Bits -----------------------------------------------------
X8               EQU  H'0000'
X9               EQU  H'0001'
X10              EQU  H'0002'
X11              EQU  H'0003'
X12              EQU  H'0004'
X13              EQU  H'0005'
X14              EQU  H'0006'
X15              EQU  H'0007'


;----- CRCCON0 Bits -----------------------------------------------------
FULL             EQU  H'0000'
SHIFTM           EQU  H'0001'
ACCM             EQU  H'0004'
BUSY             EQU  H'0005'
CRCGO            EQU  H'0006'
EN               EQU  H'0007'

CRCEN            EQU  H'0007'


;----- CRCCON1 Bits -----------------------------------------------------
PLEN0            EQU  H'0000'
PLEN1            EQU  H'0001'
PLEN2            EQU  H'0002'
PLEN3            EQU  H'0003'
DLEN0            EQU  H'0004'
DLEN1            EQU  H'0005'
DLEN2            EQU  H'0006'
DLEN3            EQU  H'0007'


;----- IVMRAWRESL Bits -----------------------------------------------------
AD0              EQU  H'0000'
AD1              EQU  H'0001'
AD2              EQU  H'0002'
AD3              EQU  H'0003'
AD4              EQU  H'0004'
AD5              EQU  H'0005'
AD6              EQU  H'0006'
AD7              EQU  H'0007'


;----- IVMRAWRESH Bits -----------------------------------------------------
AD8              EQU  H'0000'
AD9              EQU  H'0001'


;----- IVMCON0 Bits -----------------------------------------------------
IVMON            EQU  H'0000'
GOnDONE          EQU  H'0001'
IVMCTS           EQU  H'0003'
IVMLT0           EQU  H'0005'
IVMLT1           EQU  H'0006'
IVMLT2           EQU  H'0007'


;----- IVMCON1 Bits -----------------------------------------------------
IVMOVFINT        EQU  H'0002'
IVMOVRINT        EQU  H'0003'
IVMOVLAT         EQU  H'0004'
IVMOVCNT0        EQU  H'0005'
IVMOVCNT1        EQU  H'0006'
IVMOVCNT2        EQU  H'0007'


;----- IVMCON2 Bits -----------------------------------------------------
IVMUVFINT        EQU  H'0000'
IVMUVRINT        EQU  H'0001'
IVMUVLAT         EQU  H'0004'
IVMUVCNT0        EQU  H'0005'
IVMUVCNT1        EQU  H'0006'
IVMUVCNT2        EQU  H'0007'


;----- ICMCON1 Bits -----------------------------------------------------
ICMOCRINT        EQU  H'0003'
ICMOCLAT         EQU  H'0004'
ICMOCCNT0        EQU  H'0005'
ICMOCCNT1        EQU  H'0006'
ICMOCCNT2        EQU  H'0007'


;----- IVMOVUVOUT Bits -----------------------------------------------------
MIVMUVF          EQU  H'0000'
MIVMUVR          EQU  H'0001'
MIVMOVF          EQU  H'0002'
MIVMOVR          EQU  H'0003'
IVMUV            EQU  H'0004'
IVMOV            EQU  H'0005'


;----- IVMCORRESL Bits -----------------------------------------------------
IVM0             EQU  H'0006'
IVM1             EQU  H'0007'


;----- IVMCORRESH Bits -----------------------------------------------------
IVM2             EQU  H'0000'
IVM3             EQU  H'0001'
IVM4             EQU  H'0002'
IVM5             EQU  H'0003'
IVM6             EQU  H'0004'
IVM7             EQU  H'0005'
IVM8             EQU  H'0006'
IVM9             EQU  H'0007'


;----- ICMOCCON Bits -----------------------------------------------------
ICMOCSEL0        EQU  H'0000'
ICMOCSEL1        EQU  H'0001'
ICMOCSEL2        EQU  H'0002'
ICMOCSEL3        EQU  H'0003'
ICMOCSEL4        EQU  H'0004'
ICMOC            EQU  H'0006'
ICMOCON          EQU  H'0007'


;----- ICMOFF Bits -----------------------------------------------------
MICMOC0          EQU  H'0000'
MICMOC1          EQU  H'0001'
MICMOC2          EQU  H'0002'
MICMOC3          EQU  H'0003'
MICMOC4          EQU  H'0004'


;----- ICMGAIN Bits -----------------------------------------------------
MICMGC0          EQU  H'0000'
MICMGC1          EQU  H'0001'
MICMGC2          EQU  H'0002'
MICMGC3          EQU  H'0003'
MICMGC4          EQU  H'0004'
MICMGC5          EQU  H'0005'
MICMGC6          EQU  H'0006'


;----- OCMRAWRESL Bits -----------------------------------------------------
AD0              EQU  H'0000'
AD1              EQU  H'0001'
AD2              EQU  H'0002'
AD3              EQU  H'0003'
AD4              EQU  H'0004'
AD5              EQU  H'0005'
AD6              EQU  H'0006'
AD7              EQU  H'0007'


;----- OCMRAWRESH Bits -----------------------------------------------------
AD8              EQU  H'0000'
AD9              EQU  H'0001'


;----- OCMCON0 Bits -----------------------------------------------------
OCMON            EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'
OCMCTS           EQU  H'0003'
OCMLT0           EQU  H'0005'
OCMLT1           EQU  H'0006'
OCMLT2           EQU  H'0007'


;----- OCMCON1 Bits -----------------------------------------------------
OCMOCFINT        EQU  H'0002'
OCMOCRINT        EQU  H'0003'
OCMOCLAT         EQU  H'0004'
OCMOCCNT0        EQU  H'0005'
OCMOCCNT1        EQU  H'0006'
OCMOCCNT2        EQU  H'0007'


;----- OCMCON2 Bits -----------------------------------------------------
OCMUCFINT        EQU  H'0000'
OCMUCRINT        EQU  H'0001'
OCMUCLAT         EQU  H'0004'
OCMUCCNT0        EQU  H'0005'
OCMUCCNT1        EQU  H'0006'
OCMUCCNT2        EQU  H'0007'


;----- OCMUCR Bits -----------------------------------------------------
OCMUCR0          EQU  H'0000'
OCMUCR1          EQU  H'0001'
OCMUCR2          EQU  H'0002'
OCMUCR3          EQU  H'0003'
OCMUCR4          EQU  H'0004'
OCMUCR5          EQU  H'0005'
OCMUCR6          EQU  H'0006'
OCMUCR7          EQU  H'0007'


;----- OCMUCF Bits -----------------------------------------------------
OCMUCF0          EQU  H'0000'
OCMUCF1          EQU  H'0001'
OCMUCF2          EQU  H'0002'
OCMUCF3          EQU  H'0003'
OCMUCF4          EQU  H'0004'
OCMUCF5          EQU  H'0005'
OCMUCF6          EQU  H'0006'
OCMUCF7          EQU  H'0007'


;----- OCMOCR Bits -----------------------------------------------------
OCMOCR0          EQU  H'0000'
OCMOCR1          EQU  H'0001'
OCMOCR2          EQU  H'0002'
OCMOCR3          EQU  H'0003'
OCMOCR4          EQU  H'0004'
OCMOCR5          EQU  H'0005'
OCMOCR6          EQU  H'0006'
OCMOCR7          EQU  H'0007'


;----- OCMOCF Bits -----------------------------------------------------
OCMOCF0          EQU  H'0000'
OCMOCF1          EQU  H'0001'
OCMOCF2          EQU  H'0002'
OCMOCF3          EQU  H'0003'
OCMOCF4          EQU  H'0004'
OCMOCF5          EQU  H'0005'
OCMOCF6          EQU  H'0006'
OCMOCF7          EQU  H'0007'


;----- OCMOCUCOUT Bits -----------------------------------------------------
MOCMUCR          EQU  H'0000'
MOCMUCF          EQU  H'0001'
MOCMOCR          EQU  H'0002'
MOCMOCF          EQU  H'0003'
OCMUC            EQU  H'0004'
OCMOC            EQU  H'0005'


;----- OCMCORRESL Bits -----------------------------------------------------
OCM0             EQU  H'0006'
OCM1             EQU  H'0007'


;----- OCMCORRESH Bits -----------------------------------------------------
OCM2             EQU  H'0000'
OCM3             EQU  H'0001'
OCM4             EQU  H'0002'
OCM5             EQU  H'0003'
OCM6             EQU  H'0004'
OCM7             EQU  H'0005'
OCM8             EQU  H'0006'
OCM9             EQU  H'0007'


;----- LPFCON0 Bits -----------------------------------------------------
LPFCHS0          EQU  H'0000'
LPFCHS1          EQU  H'0001'
LPFEN            EQU  H'0007'


;----- OCROFF Bits -----------------------------------------------------
MOCROC0          EQU  H'0000'
MOCROC1          EQU  H'0001'
MOCROC2          EQU  H'0002'
MOCROC3          EQU  H'0003'
MOCROC4          EQU  H'0004'


;----- OCRGAIN Bits -----------------------------------------------------
MOCRGC0          EQU  H'0000'
MOCRGC1          EQU  H'0001'
MOCRGC2          EQU  H'0002'
MOCRGC3          EQU  H'0003'
MOCRGC4          EQU  H'0004'
MOCRGC5          EQU  H'0005'
MOCRGC6          EQU  H'0006'


;----- OVMRAWRESL Bits -----------------------------------------------------
AD0              EQU  H'0000'
AD1              EQU  H'0001'
AD2              EQU  H'0002'
AD3              EQU  H'0003'
AD4              EQU  H'0004'
AD5              EQU  H'0005'
AD6              EQU  H'0006'
AD7              EQU  H'0007'


;----- OVMRAWRESH Bits -----------------------------------------------------
AD8              EQU  H'0000'
AD9              EQU  H'0001'


;----- OVMCON0 Bits -----------------------------------------------------
OCMON            EQU  H'0000'
GO_NOT_DONE      EQU  H'0001'
OMCTS            EQU  H'0003'
OMLT0            EQU  H'0005'
OMLT1            EQU  H'0006'
OMLT2            EQU  H'0007'


;----- OVMCON1 Bits -----------------------------------------------------
OVMOVFINT        EQU  H'0002'
OVMOVRINT        EQU  H'0003'
OVMOVLAT         EQU  H'0004'
OVMOVCNT0        EQU  H'0005'
OVMOVCNT1        EQU  H'0006'
OVMOVCNT2        EQU  H'0007'


;----- OVMCON2 Bits -----------------------------------------------------
OVMUVFINT        EQU  H'0000'
OVMUVRINT        EQU  H'0001'
OVMUVLAT         EQU  H'0004'
OVMUVCNT0        EQU  H'0005'
OVMUVCNT1        EQU  H'0006'
OVMUVCNT2        EQU  H'0007'


;----- OVMCON3 Bits -----------------------------------------------------
OVMGS0           EQU  H'0000'
OVMGS1           EQU  H'0001'
OVMGS2           EQU  H'0002'
OVMSIGN          EQU  H'0007'


;----- OVMOVUVOUT Bits -----------------------------------------------------
MOVMUVF          EQU  H'0000'
MOVMUVR          EQU  H'0001'
MOVMOVF          EQU  H'0002'
MOVMOVR          EQU  H'0003'
OVMUV            EQU  H'0004'
OVMOV            EQU  H'0005'


;----- OVMCORRESL Bits -----------------------------------------------------
OVM0             EQU  H'0006'
OVM1             EQU  H'0007'


;----- OVMCORRESH Bits -----------------------------------------------------
OVM2             EQU  H'0000'
OVM3             EQU  H'0001'
OVM4             EQU  H'0002'
OVM5             EQU  H'0003'
OVM6             EQU  H'0004'
OVM7             EQU  H'0005'
OVM8             EQU  H'0006'
OVM9             EQU  H'0007'


;----- PIR0 Bits -----------------------------------------------------
INT0IF           EQU  H'0000'
IOCIF            EQU  H'0004'
TMR0IF           EQU  H'0005'
FVRIF            EQU  H'0006'


;----- PIR1 Bits -----------------------------------------------------
ADIF             EQU  H'0000'
CSWIF            EQU  H'0003'
NVMIF            EQU  H'0005'
CRCIF            EQU  H'0006'
DMAIF            EQU  H'0007'


;----- PIR2 Bits -----------------------------------------------------
TMR1IF           EQU  H'0000'
TMR2IF           EQU  H'0001'
TMR3IF           EQU  H'0002'
TMR4IF           EQU  H'0003'
TMR1GIF          EQU  H'0006'
TMR3GIF          EQU  H'0007'


;----- PIR3 Bits -----------------------------------------------------
CCP1IF           EQU  H'0000'
CCP2IF           EQU  H'0001'
CLC1IF           EQU  H'0004'
CLC2IF           EQU  H'0005'


;----- PIR4 Bits -----------------------------------------------------
SSP1IF           EQU  H'0000'
BCL1IF           EQU  H'0001'
TXIF             EQU  H'0004'
RCIF             EQU  H'0005'


;----- PIR5 Bits -----------------------------------------------------
OCMUCIF          EQU  H'0000'
OCMOCIF          EQU  H'0001'
OCMADCIF         EQU  H'0002'
OVMUVIF          EQU  H'0004'
OVMOVIF          EQU  H'0005'
OVMADCIF         EQU  H'0006'


;----- PIR6 Bits -----------------------------------------------------
ICMOCIF          EQU  H'0001'
IVMUVIF          EQU  H'0004'
IVMOVIF          EQU  H'0005'
IVMADCIF         EQU  H'0006'


;----- PIR7 Bits -----------------------------------------------------
DEMIF            EQU  H'0000'
TSWIF            EQU  H'0002'
SWFRQIF          EQU  H'0004'


;----- PIE0 Bits -----------------------------------------------------
INT0IE           EQU  H'0000'
IOCIE            EQU  H'0004'
TMR0IE           EQU  H'0005'
FVRIE            EQU  H'0006'


;----- PIE1 Bits -----------------------------------------------------
ADIE             EQU  H'0000'
CSWIE            EQU  H'0003'
NVMIE            EQU  H'0005'
CRCIE            EQU  H'0006'
DMAIE            EQU  H'0007'


;----- PIE2 Bits -----------------------------------------------------
TMR1IE           EQU  H'0000'
TMR2IE           EQU  H'0001'
TMR3IE           EQU  H'0002'
TMR4IE           EQU  H'0003'
TMR1GIE          EQU  H'0006'
TMR3GIE          EQU  H'0007'


;----- PIE3 Bits -----------------------------------------------------
CCP1IE           EQU  H'0000'
CCP2IE           EQU  H'0001'
CLC1IE           EQU  H'0004'
CLC2IE           EQU  H'0005'


;----- PIE4 Bits -----------------------------------------------------
SSP1IE           EQU  H'0000'
BCL1IE           EQU  H'0001'
TXIE             EQU  H'0004'
RCIE             EQU  H'0005'


;----- PIE5 Bits -----------------------------------------------------
OCMUCIE          EQU  H'0000'
OCMOCIE          EQU  H'0001'
OCMADCIE         EQU  H'0002'
OVMUVIE          EQU  H'0004'
OVMOVIE          EQU  H'0005'
OVMADCIE         EQU  H'0006'


;----- PIE6 Bits -----------------------------------------------------
ICMOCIE          EQU  H'0001'
IVMUVIE          EQU  H'0004'
IVMOVIE          EQU  H'0005'
IVMADCIE         EQU  H'0006'


;----- PIE7 Bits -----------------------------------------------------
DEMIE            EQU  H'0000'
TSWIE            EQU  H'0002'
SWFRQIE          EQU  H'0004'


;----- PMD0 Bits -----------------------------------------------------
IOCMD            EQU  H'0000'
NVMMD            EQU  H'0002'
SCANMD           EQU  H'0003'
CRCMD            EQU  H'0004'
FVRMD            EQU  H'0006'
SYSCMD           EQU  H'0007'


;----- PMD1 Bits -----------------------------------------------------
TMR0MD           EQU  H'0000'
TMR1MD           EQU  H'0001'
TMR2MD           EQU  H'0002'
TMR3MD           EQU  H'0003'
TMR4MD           EQU  H'0004'
CCP1MD           EQU  H'0006'
CCP2MD           EQU  H'0007'


;----- PMD2 Bits -----------------------------------------------------
CLC1MD           EQU  H'0000'
CLC2MD           EQU  H'0001'
ADCMD            EQU  H'0003'
MSSP1MD          EQU  H'0005'
UART1MD          EQU  H'0007'


;----- PMD3 Bits -----------------------------------------------------
ICMMD            EQU  H'0000'
IVMMD            EQU  H'0002'
OCMMD            EQU  H'0004'
OVMMD            EQU  H'0006'


;----- PMD4 Bits -----------------------------------------------------
OCRMD            EQU  H'0000'
DCOMPMD          EQU  H'0002'
SWFRQMD          EQU  H'0004'
DEMMD            EQU  H'0006'


;----- WDTCON0 Bits -----------------------------------------------------
SWDTEN           EQU  H'0000'
WDTPS0           EQU  H'0001'
WDTPS1           EQU  H'0002'
WDTPS2           EQU  H'0003'
WDTPS3           EQU  H'0004'
WDTPS4           EQU  H'0005'


;----- WDTCON1 Bits -----------------------------------------------------
WINDOW0          EQU  H'0000'
WINDOW1          EQU  H'0001'
WINDOW2          EQU  H'0002'
WDTCS0           EQU  H'0004'
WDTCS1           EQU  H'0005'
WDTCS2           EQU  H'0006'


;----- WDTPSL Bits -----------------------------------------------------
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
STATE            EQU  H'0002'

PSCNT16          EQU  H'0000'
PSCNT17          EQU  H'0001'
WDTTMR0          EQU  H'0003'
WDTTMR1          EQU  H'0004'
WDTTMR2          EQU  H'0005'
WDTTMR3          EQU  H'0006'


;----- BORCON Bits -----------------------------------------------------
BORRDY           EQU  H'0000'
SBOREN           EQU  H'0007'


;----- VREGCON Bits -----------------------------------------------------
VREGPM1          EQU  H'0001'


;----- PCON0 Bits -----------------------------------------------------
NOT_BOR          EQU  H'0000'
NOT_POR          EQU  H'0001'
NOT_RI           EQU  H'0002'
NOT_RMCLR        EQU  H'0003'
NOT_RWDT         EQU  H'0004'
NOT_WDTWV        EQU  H'0005'
STKUNF           EQU  H'0006'
STKOVF           EQU  H'0007'


;----- CCDCON Bits -----------------------------------------------------
CCDS0            EQU  H'0000'
CCDS1            EQU  H'0001'
CCDEN            EQU  H'0007'


;----- NVMADRL Bits -----------------------------------------------------
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


;----- NVMDATL Bits -----------------------------------------------------
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


;----- CPUDOZE Bits -----------------------------------------------------
DOZE0            EQU  H'0000'
DOZE1            EQU  H'0001'
DOZE2            EQU  H'0002'
DOE              EQU  H'0004'
ROI              EQU  H'0005'
DOZEN            EQU  H'0006'
IDLEN            EQU  H'0007'


;----- OSCCON1 Bits -----------------------------------------------------
NDIV0            EQU  H'0000'
NDIV1            EQU  H'0001'
NDIV2            EQU  H'0002'
NDIV3            EQU  H'0003'
NOSC0            EQU  H'0004'
NOSC1            EQU  H'0005'


;----- OSCCON2 Bits -----------------------------------------------------
CDIV0            EQU  H'0000'
CDIV1            EQU  H'0001'
CDIV2            EQU  H'0002'
CDIV3            EQU  H'0003'
COSC0            EQU  H'0004'
COSC1            EQU  H'0005'


;----- OSCCON3 Bits -----------------------------------------------------
NOSCR            EQU  H'0003'
ORDY             EQU  H'0004'
CSWHOLD          EQU  H'0007'


;----- OSCSTAT1 Bits -----------------------------------------------------
PLL32OR          EQU  H'0000'
ADOR             EQU  H'0002'
LFOR             EQU  H'0004'
MFOR             EQU  H'0005'
HFOR             EQU  H'0006'


;----- OSCEN Bits -----------------------------------------------------
PLL32EN          EQU  H'0000'
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


;----- OSCFREQ Bits -----------------------------------------------------
HFFRQ0           EQU  H'0000'
HFFRQ1           EQU  H'0001'
HFFRQ2           EQU  H'0002'


;----- DCOMPCON0 Bits -----------------------------------------------------
TFS0             EQU  H'0000'
TFS1             EQU  H'0001'
VCOMPEN          EQU  H'0006'
ICOMPEN          EQU  H'0007'


;----- DCOMPCON1 Bits -----------------------------------------------------
IVMUVFE          EQU  H'0000'
IVMOVFE          EQU  H'0001'
ICMOCFE          EQU  H'0002'
HOLDM0           EQU  H'0003'
HOLDM1           EQU  H'0004'
HOLDM2           EQU  H'0005'
HOLDOVR          EQU  H'0006'
HOLD             EQU  H'0007'


;----- DCOMPCON2 Bits -----------------------------------------------------
OVMUVFE0         EQU  H'0000'
OVMUVFE1         EQU  H'0001'
OVMOVFE0         EQU  H'0002'
OVMOVFE1         EQU  H'0003'
OCMUCFE0         EQU  H'0004'
OCMUCFE1         EQU  H'0005'
OCMOCFE0         EQU  H'0006'
OCMOCFE1         EQU  H'0007'


;----- VREFH Bits -----------------------------------------------------
VREF8            EQU  H'0000'
VREF9            EQU  H'0001'


;----- VREFL Bits -----------------------------------------------------
VREF0            EQU  H'0000'
VREF1            EQU  H'0001'
VREF2            EQU  H'0002'
VREF3            EQU  H'0003'
VREF4            EQU  H'0004'
VREF5            EQU  H'0005'
VREF6            EQU  H'0006'
VREF7            EQU  H'0007'


;----- IREFH Bits -----------------------------------------------------
IREF8            EQU  H'0000'
IREF9            EQU  H'0001'


;----- IREFL Bits -----------------------------------------------------
IREF0            EQU  H'0000'
IREF1            EQU  H'0001'
IREF2            EQU  H'0002'
IREF3            EQU  H'0003'
IREF4            EQU  H'0004'
IREF5            EQU  H'0005'
IREF6            EQU  H'0006'
IREF7            EQU  H'0007'


;----- DOUTH Bits -----------------------------------------------------
DOUT6            EQU  H'0000'
DOUT7            EQU  H'0001'
DOUT8            EQU  H'0002'
DOUT9            EQU  H'0003'
DOUT10           EQU  H'0004'
DOUT11           EQU  H'0005'
DOUT12           EQU  H'0006'
DOUT13           EQU  H'0007'


;----- DOUTL Bits -----------------------------------------------------
DOUT0            EQU  H'0002'
DOUT1            EQU  H'0003'
DOUT2            EQU  H'0004'
DOUT3            EQU  H'0005'
DOUT4            EQU  H'0006'
DOUT5            EQU  H'0007'


;----- KPI Bits -----------------------------------------------------
KPI0             EQU  H'0000'
KPI1             EQU  H'0001'
KPI2             EQU  H'0002'
KPI3             EQU  H'0003'
KPI4             EQU  H'0004'


;----- KVI Bits -----------------------------------------------------
KVI0             EQU  H'0000'
KVI1             EQU  H'0001'
KVI2             EQU  H'0002'
KVI3             EQU  H'0003'
KVI4             EQU  H'0004'


;----- KPV Bits -----------------------------------------------------
KPV0             EQU  H'0000'
KPV1             EQU  H'0001'
KPV2             EQU  H'0002'
KPV3             EQU  H'0003'
KPV4             EQU  H'0004'


;----- KVV Bits -----------------------------------------------------
KVV0             EQU  H'0000'
KVV1             EQU  H'0001'
KVV2             EQU  H'0002'
KVV3             EQU  H'0003'
KVV4             EQU  H'0004'


;----- KIV Bits -----------------------------------------------------
KIV0             EQU  H'0000'
KIV1             EQU  H'0001'
KIV2             EQU  H'0002'
KIV3             EQU  H'0003'
KIV4             EQU  H'0004'


;----- MAXDC Bits -----------------------------------------------------
MAXDC0           EQU  H'0000'
MAXDC1           EQU  H'0001'
MAXDC2           EQU  H'0002'
MAXDC3           EQU  H'0003'
MAXDC4           EQU  H'0004'
MAXDC5           EQU  H'0005'
MAXDC6           EQU  H'0006'
MAXDC7           EQU  H'0007'


;----- XI Bits -----------------------------------------------------
XI0              EQU  H'0000'
XI1              EQU  H'0001'
XI2              EQU  H'0002'
XI3              EQU  H'0003'
XI4              EQU  H'0004'


;----- YI Bits -----------------------------------------------------
YI0              EQU  H'0000'
YI1              EQU  H'0001'
YI2              EQU  H'0002'
YI3              EQU  H'0003'
YI4              EQU  H'0004'


;----- XV Bits -----------------------------------------------------
XV0              EQU  H'0000'
XV1              EQU  H'0001'
XV2              EQU  H'0002'
XV3              EQU  H'0003'
XV4              EQU  H'0004'


;----- YV Bits -----------------------------------------------------
YV0              EQU  H'0000'
YV1              EQU  H'0001'
YV2              EQU  H'0002'
YV3              EQU  H'0003'
YV4              EQU  H'0004'


;----- NS Bits -----------------------------------------------------
NS0              EQU  H'0000'
NS1              EQU  H'0001'
NS2              EQU  H'0002'
NS3              EQU  H'0003'
NS4              EQU  H'0004'


;----- NP Bits -----------------------------------------------------
NP0              EQU  H'0000'
NP1              EQU  H'0001'
NP2              EQU  H'0002'
NP3              EQU  H'0003'
NP4              EQU  H'0004'


;----- RLOAD Bits -----------------------------------------------------
RLOAD0           EQU  H'0000'
RLOAD1           EQU  H'0001'
RLOAD2           EQU  H'0002'
RLOAD3           EQU  H'0003'
RLOAD4           EQU  H'0004'
RLOAD5           EQU  H'0005'
RLOAD6           EQU  H'0006'
RLOAD7           EQU  H'0007'


;----- RLOSS Bits -----------------------------------------------------
RLOSS0           EQU  H'0000'
RLOSS1           EQU  H'0001'
RLOSS2           EQU  H'0002'
RLOSS3           EQU  H'0003'
RLOSS4           EQU  H'0004'
RLOSS5           EQU  H'0005'
RLOSS6           EQU  H'0006'
RLOSS7           EQU  H'0007'


;----- TST_DMAH Bits -----------------------------------------------------
DMA_DATA8        EQU  H'0000'
DMA_DATA9        EQU  H'0001'
DMA_DATA10       EQU  H'0002'
DMA_DATA11       EQU  H'0003'
DMA_DATA12       EQU  H'0004'
DMA_DATA13       EQU  H'0005'
DMA_DATA14       EQU  H'0006'
DMA_DATA15       EQU  H'0007'


;----- TST_DMAL Bits -----------------------------------------------------
DMA_DATA0        EQU  H'0000'
DMA_DATA1        EQU  H'0001'
DMA_DATA2        EQU  H'0002'
DMA_DATA3        EQU  H'0003'
DMA_DATA4        EQU  H'0004'
DMA_DATA5        EQU  H'0005'
DMA_DATA6        EQU  H'0006'
DMA_DATA7        EQU  H'0007'


;----- TST_ISMSTAT Bits -----------------------------------------------------
PIBSTAT          EQU  H'0000'
ISMSTAT          EQU  H'0004'


;----- TST_ISMREAD Bits -----------------------------------------------------
ISMREAD0         EQU  H'0000'
ISMREAD1         EQU  H'0001'
ISMREAD2         EQU  H'0002'
ISMREAD3         EQU  H'0003'
ISMREAD4         EQU  H'0004'
ISMREAD5         EQU  H'0005'


;----- TST_PERDOVR Bits -----------------------------------------------------
PEROVR0          EQU  H'0000'
PEROVR1          EQU  H'0001'
PEROVR2          EQU  H'0002'
PEROVR3          EQU  H'0003'
PEROVR4          EQU  H'0004'
PEROVR5          EQU  H'0005'


;----- TST_PIBREAD Bits -----------------------------------------------------
PIBREAD0         EQU  H'0000'
PIBREAD1         EQU  H'0001'
PIBREAD2         EQU  H'0002'
PIBREAD3         EQU  H'0003'
PIBREAD4         EQU  H'0004'
PIBREAD5         EQU  H'0005'


;----- UMTOAP Bits -----------------------------------------------------
UMTOAP0          EQU  H'0000'
UMTOAP1          EQU  H'0001'
UMTOAP2          EQU  H'0002'
UMTOAP3          EQU  H'0003'
UMTOAP4          EQU  H'0004'
UMTOAP5          EQU  H'0005'


;----- UMTOAL Bits -----------------------------------------------------
UMTOA0           EQU  H'0000'
UMTOA1           EQU  H'0001'
UMTOA2           EQU  H'0002'
UMTOA3           EQU  H'0003'
UMTOA4           EQU  H'0004'
UMTOA5           EQU  H'0005'
UMTOA6           EQU  H'0006'
UMTOA7           EQU  H'0007'


;----- UMTOAH Bits -----------------------------------------------------
UMTOA8           EQU  H'0000'
UMTOA9           EQU  H'0001'
UMTOA10          EQU  H'0002'
UMTOA11          EQU  H'0003'
UMTOA12          EQU  H'0004'
UMTOA13          EQU  H'0005'


;----- CLCDATA Bits -----------------------------------------------------
LC1OUT_CLCDATA   EQU  H'0000'
LC2OUT_CLCDATA   EQU  H'0001'
LC3OUT           EQU  H'0002'
LC4OUT           EQU  H'0003'


;----- CLC1CON Bits -----------------------------------------------------
LC1INTN          EQU  H'0003'
LC1INTP          EQU  H'0004'
LC1OUT_CLC1CON   EQU  H'0005'
LC1EN            EQU  H'0007'

LC1MODE0         EQU  H'0000'
LC1MODE1         EQU  H'0001'
LC1MODE2         EQU  H'0002'

INTN             EQU  H'0003'
INTP             EQU  H'0004'
OUT              EQU  H'0005'
EN               EQU  H'0007'

MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- CLC1POL Bits -----------------------------------------------------
LC1G1POL         EQU  H'0000'
LC1G2POL         EQU  H'0001'
LC1G3POL         EQU  H'0002'
LC1G4POL         EQU  H'0003'
LC1POL           EQU  H'0007'

G1POL            EQU  H'0000'
G2POL            EQU  H'0001'
G3POL            EQU  H'0002'
G4POL            EQU  H'0003'
POL              EQU  H'0007'


;----- CLC1SEL0 Bits -----------------------------------------------------
LC1D1S0          EQU  H'0000'
LC1D1S1          EQU  H'0001'
LC1D1S2          EQU  H'0002'
LC1D1S3          EQU  H'0003'
LC1D1S4          EQU  H'0004'
LC1D1S5          EQU  H'0005'



D1S0             EQU  H'0000'
D1S1             EQU  H'0001'
D1S2             EQU  H'0002'
D1S3             EQU  H'0003'
D1S4             EQU  H'0004'
D1S5             EQU  H'0005'


;----- CLC1SEL1 Bits -----------------------------------------------------
LC1D2S0          EQU  H'0000'
LC1D2S1          EQU  H'0001'
LC1D2S2          EQU  H'0002'
LC1D2S3          EQU  H'0003'
LC1D2S4          EQU  H'0004'
LC1D2S5          EQU  H'0005'



D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'


;----- CLC1SEL2 Bits -----------------------------------------------------
LC1D3S0          EQU  H'0000'
LC1D3S1          EQU  H'0001'
LC1D3S2          EQU  H'0002'
LC1D3S3          EQU  H'0003'
LC1D3S4          EQU  H'0004'
LC1D3S5          EQU  H'0005'



D3S0             EQU  H'0000'
D3S1             EQU  H'0001'
D3S2             EQU  H'0002'
D3S3             EQU  H'0003'
D3S4             EQU  H'0004'
D3S5             EQU  H'0005'


;----- CLC1SEL3 Bits -----------------------------------------------------
LC1D4S0          EQU  H'0000'
LC1D4S1          EQU  H'0001'
LC1D4S2          EQU  H'0002'
LC1D4S3          EQU  H'0003'
LC1D4S4          EQU  H'0004'
LC1D4S5          EQU  H'0005'



D4S0             EQU  H'0000'
D4S1             EQU  H'0001'
D4S2             EQU  H'0002'
D4S3             EQU  H'0003'
D4S4             EQU  H'0004'
D4S5             EQU  H'0005'


;----- CLC1GLS0 Bits -----------------------------------------------------
LC1G1D1N         EQU  H'0000'
LC1G1D1T         EQU  H'0001'
LC1G1D2N         EQU  H'0002'
LC1G1D2T         EQU  H'0003'
LC1G1D3N         EQU  H'0004'
LC1G1D3T         EQU  H'0005'
LC1G1D4N         EQU  H'0006'
LC1G1D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC1GLS1 Bits -----------------------------------------------------
LC1G2D1N         EQU  H'0000'
LC1G2D1T         EQU  H'0001'
LC1G2D2N         EQU  H'0002'
LC1G2D2T         EQU  H'0003'
LC1G2D3N         EQU  H'0004'
LC1G2D3T         EQU  H'0005'
LC1G2D4N         EQU  H'0006'
LC1G2D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC1GLS2 Bits -----------------------------------------------------
LC1G3D1N         EQU  H'0000'
LC1G3D1T         EQU  H'0001'
LC1G3D2N         EQU  H'0002'
LC1G3D2T         EQU  H'0003'
LC1G3D3N         EQU  H'0004'
LC1G3D3T         EQU  H'0005'
LC1G3D4N         EQU  H'0006'
LC1G3D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC1GLS3 Bits -----------------------------------------------------
LC1G4D1N         EQU  H'0000'
LC1G4D1T         EQU  H'0001'
LC1G4D2N         EQU  H'0002'
LC1G4D2T         EQU  H'0003'
LC1G4D3N         EQU  H'0004'
LC1G4D3T         EQU  H'0005'
LC1G4D4N         EQU  H'0006'
LC1G4D4T         EQU  H'0007'

G4D1N            EQU  H'0000'
G4D1T            EQU  H'0001'
G4D2N            EQU  H'0002'
G4D2T            EQU  H'0003'
G4D3N            EQU  H'0004'
G4D3T            EQU  H'0005'
G4D4N            EQU  H'0006'
G4D4T            EQU  H'0007'


;----- CLC2CON Bits -----------------------------------------------------
LC2INTN          EQU  H'0003'
LC2INTP          EQU  H'0004'
LC2OUT_CLC2CON   EQU  H'0005'
LC2EN            EQU  H'0007'

LC2MODE0         EQU  H'0000'
LC2MODE1         EQU  H'0001'
LC2MODE2         EQU  H'0002'

INTN             EQU  H'0003'
INTP             EQU  H'0004'
OUT              EQU  H'0005'
EN               EQU  H'0007'

MODE0            EQU  H'0000'
MODE1            EQU  H'0001'
MODE2            EQU  H'0002'


;----- CLC2POL Bits -----------------------------------------------------
LC2G1POL         EQU  H'0000'
LC2G2POL         EQU  H'0001'
LC2G3POL         EQU  H'0002'
LC2G4POL         EQU  H'0003'
LC2POL           EQU  H'0007'

G1POL            EQU  H'0000'
G2POL            EQU  H'0001'
G3POL            EQU  H'0002'
G4POL            EQU  H'0003'
POL              EQU  H'0007'


;----- CLC2SEL0 Bits -----------------------------------------------------
LC2D1S0          EQU  H'0000'
LC2D1S1          EQU  H'0001'
LC2D1S2          EQU  H'0002'
LC2D1S3          EQU  H'0003'
LC2D1S4          EQU  H'0004'
LC2D1S5          EQU  H'0005'



D1S0             EQU  H'0000'
D1S1             EQU  H'0001'
D1S2             EQU  H'0002'
D1S3             EQU  H'0003'
D1S4             EQU  H'0004'
D1S5             EQU  H'0005'


;----- CLC2SEL1 Bits -----------------------------------------------------
LC2D2S0          EQU  H'0000'
LC2D2S1          EQU  H'0001'
LC2D2S2          EQU  H'0002'
LC2D2S3          EQU  H'0003'
LC2D2S4          EQU  H'0004'
LC2D2S5          EQU  H'0005'



D2S0             EQU  H'0000'
D2S1             EQU  H'0001'
D2S2             EQU  H'0002'
D2S3             EQU  H'0003'
D2S4             EQU  H'0004'
D2S5             EQU  H'0005'


;----- CLC2SEL2 Bits -----------------------------------------------------
LC2D3S0          EQU  H'0000'
LC2D3S1          EQU  H'0001'
LC2D3S2          EQU  H'0002'
LC2D3S3          EQU  H'0003'
LC2D3S4          EQU  H'0004'
LC2D3S5          EQU  H'0005'



D3S0             EQU  H'0000'
D3S1             EQU  H'0001'
D3S2             EQU  H'0002'
D3S3             EQU  H'0003'
D3S4             EQU  H'0004'
D3S5             EQU  H'0005'


;----- CLC2SEL3 Bits -----------------------------------------------------
LC2D4S0          EQU  H'0000'
LC2D4S1          EQU  H'0001'
LC2D4S2          EQU  H'0002'
LC2D4S3          EQU  H'0003'
LC2D4S4          EQU  H'0004'
LC2D4S5          EQU  H'0005'



D4S0             EQU  H'0000'
D4S1             EQU  H'0001'
D4S2             EQU  H'0002'
D4S3             EQU  H'0003'
D4S4             EQU  H'0004'
D4S5             EQU  H'0005'


;----- CLC2GLS0 Bits -----------------------------------------------------
LC2G1D1N         EQU  H'0000'
LC2G1D1T         EQU  H'0001'
LC2G1D2N         EQU  H'0002'
LC2G1D2T         EQU  H'0003'
LC2G1D3N         EQU  H'0004'
LC2G1D3T         EQU  H'0005'
LC2G1D4N         EQU  H'0006'
LC2G1D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC2GLS1 Bits -----------------------------------------------------
LC2G2D1N         EQU  H'0000'
LC2G2D1T         EQU  H'0001'
LC2G2D2N         EQU  H'0002'
LC2G2D2T         EQU  H'0003'
LC2G2D3N         EQU  H'0004'
LC2G2D3T         EQU  H'0005'
LC2G2D4N         EQU  H'0006'
LC2G2D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC2GLS2 Bits -----------------------------------------------------
LC2G3D1N         EQU  H'0000'
LC2G3D1T         EQU  H'0001'
LC2G3D2N         EQU  H'0002'
LC2G3D2T         EQU  H'0003'
LC2G3D3N         EQU  H'0004'
LC2G3D3T         EQU  H'0005'
LC2G3D4N         EQU  H'0006'
LC2G3D4T         EQU  H'0007'

D1N              EQU  H'0000'
D1T              EQU  H'0001'
D2N              EQU  H'0002'
D2T              EQU  H'0003'
D3N              EQU  H'0004'
D3T              EQU  H'0005'
D4N              EQU  H'0006'
D4T              EQU  H'0007'


;----- CLC2GLS3 Bits -----------------------------------------------------
LC2G4D1N         EQU  H'0000'
LC2G4D1T         EQU  H'0001'
LC2G4D2N         EQU  H'0002'
LC2G4D2T         EQU  H'0003'
LC2G4D3N         EQU  H'0004'
LC2G4D3T         EQU  H'0005'
LC2G4D4N         EQU  H'0006'
LC2G4D4T         EQU  H'0007'

G4D1N            EQU  H'0000'
G4D1T            EQU  H'0001'
G4D2N            EQU  H'0002'
G4D2T            EQU  H'0003'
G4D3N            EQU  H'0004'
G4D3T            EQU  H'0005'
G4D4N            EQU  H'0006'
G4D4T            EQU  H'0007'


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


;----- T3CKIPPS Bits -----------------------------------------------------
T3CKIPPS0        EQU  H'0000'
T3CKIPPS1        EQU  H'0001'
T3CKIPPS2        EQU  H'0002'
T3CKIPPS3        EQU  H'0003'
T3CKIPPS4        EQU  H'0004'


;----- T3GPPS Bits -----------------------------------------------------
T3GPPS0          EQU  H'0000'
T3GPPS1          EQU  H'0001'
T3GPPS2          EQU  H'0002'
T3GPPS3          EQU  H'0003'
T3GPPS4          EQU  H'0004'


;----- T2AINPPS Bits -----------------------------------------------------
T2AINPPS0        EQU  H'0000'
T2AINPPS1        EQU  H'0001'
T2AINPPS2        EQU  H'0002'
T2AINPPS3        EQU  H'0003'
T2AINPPS4        EQU  H'0004'


;----- T4AINPPS Bits -----------------------------------------------------
T4AINPPS0        EQU  H'0000'
T4AINPPS1        EQU  H'0001'
T4AINPPS2        EQU  H'0002'
T4AINPPS3        EQU  H'0003'
T4AINPPS4        EQU  H'0004'


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


;----- CLCIN0PPS Bits -----------------------------------------------------
CLCIN0PPS0       EQU  H'0000'
CLCIN0PPS1       EQU  H'0001'
CLCIN0PPS2       EQU  H'0002'
CLCIN0PPS3       EQU  H'0003'
CLCIN0PPS4       EQU  H'0004'


;----- CLCIN1PPS Bits -----------------------------------------------------
CLCIN1PPS0       EQU  H'0000'
CLCIN1PPS1       EQU  H'0001'
CLCIN1PPS2       EQU  H'0002'
CLCIN1PPS3       EQU  H'0003'
CLCIN1PPS4       EQU  H'0004'


;----- CLCIN2PPS Bits -----------------------------------------------------
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


;----- TST_PPSIN1 Bits -----------------------------------------------------
INT              EQU  H'0000'
T0CKI            EQU  H'0001'
T1CKI            EQU  H'0002'
T1G              EQU  H'0003'
T3CKI            EQU  H'0004'
T3G              EQU  H'0005'


;----- TST_PPSIN2 Bits -----------------------------------------------------
T2IN             EQU  H'0000'
T4IN             EQU  H'0001'
CCP1             EQU  H'0004'
CCP2             EQU  H'0005'


;----- TST_PPSIN3 Bits -----------------------------------------------------
CLCIN0           EQU  H'0000'
CLCIN1           EQU  H'0001'
CLCIN2           EQU  H'0002'
CLCIN3           EQU  H'0003'
ADCACT           EQU  H'0006'
DCPHLD           EQU  H'0007'


;----- TST_PPSIN4 Bits -----------------------------------------------------
RXDT             EQU  H'0000'
TXCK             EQU  H'0001'
SSP1CLK          EQU  H'0005'
SSP1DAT          EQU  H'0006'
SSP1SS           EQU  H'0007'


;----- TST_PPSDOV Bits -----------------------------------------------------
PPSDOV0          EQU  H'0000'
PPSDOV1          EQU  H'0001'
PPSDOV2          EQU  H'0002'
PPSDOV3          EQU  H'0003'


;----- TST_PPSEOV Bits -----------------------------------------------------
PPSEOV0          EQU  H'0000'
PPSEOV1          EQU  H'0001'
PPSEOV2          EQU  H'0002'
PPSEOV3          EQU  H'0003'


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


;----- ANSELA Bits -----------------------------------------------------
ANSA0            EQU  H'0000'
ANSA1            EQU  H'0001'
ANSA2            EQU  H'0002'
ANSA3            EQU  H'0003'


;----- WPUA Bits -----------------------------------------------------
WPUA0            EQU  H'0000'
WPUA1            EQU  H'0001'
WPUA2            EQU  H'0002'
WPUA3            EQU  H'0003'


;----- ODCONA Bits -----------------------------------------------------
ODCA0            EQU  H'0000'
ODCA1            EQU  H'0001'
ODCA2            EQU  H'0002'
ODCA3            EQU  H'0003'


;----- SLRCONA Bits -----------------------------------------------------
SLRA0            EQU  H'0000'
SLRA1            EQU  H'0001'
SLRA2            EQU  H'0002'
SLRA3            EQU  H'0003'


;----- INLVLA Bits -----------------------------------------------------
INLVLA0          EQU  H'0000'
INLVLA1          EQU  H'0001'
INLVLA2          EQU  H'0002'
INLVLA3          EQU  H'0003'


;----- IOCAP Bits -----------------------------------------------------
IOCAP0           EQU  H'0000'
IOCAP1           EQU  H'0001'
IOCAP2           EQU  H'0002'
IOCAP3           EQU  H'0003'


;----- IOCAN Bits -----------------------------------------------------
IOCAN0           EQU  H'0000'
IOCAN1           EQU  H'0001'
IOCAN2           EQU  H'0002'
IOCAN3           EQU  H'0003'


;----- IOCAF Bits -----------------------------------------------------
IOCAF0           EQU  H'0000'
IOCAF1           EQU  H'0001'
IOCAF2           EQU  H'0002'
IOCAF3           EQU  H'0003'


;----- CCDNA Bits -----------------------------------------------------
CCDNA1           EQU  H'0001'
CCDNA2           EQU  H'0002'
CCDNA3           EQU  H'0003'


;----- CCDPA Bits -----------------------------------------------------
CCDPA1           EQU  H'0001'
CCDPA2           EQU  H'0002'
CCDPA3           EQU  H'0003'


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


;----- WPUC Bits -----------------------------------------------------
WPUC7            EQU  H'0007'


;----- INLVLC Bits -----------------------------------------------------
INLVLC2          EQU  H'0002'
INLVLC3          EQU  H'0003'
INLVLC4          EQU  H'0004'
INLVLC5          EQU  H'0005'
INLVLC6          EQU  H'0006'
INLVLC7          EQU  H'0007'


;----- IOCCP Bits -----------------------------------------------------
IOCCP4           EQU  H'0004'
IOCCP5           EQU  H'0005'
IOCCP6           EQU  H'0006'
IOCCP7           EQU  H'0007'


;----- IOCCN Bits -----------------------------------------------------
IOCCN4           EQU  H'0004'
IOCCN5           EQU  H'0005'
IOCCN6           EQU  H'0006'
IOCCN7           EQU  H'0007'


;----- IOCCF Bits -----------------------------------------------------
IOCCF4           EQU  H'0004'
IOCCF5           EQU  H'0005'
IOCCF6           EQU  H'0006'
IOCCF7           EQU  H'0007'


;----- ICDIO Bits -----------------------------------------------------
ICD_SLRC         EQU  H'0000'
TRIS_ICDCLK      EQU  H'0002'
TRIS_ICDDAT      EQU  H'0003'
LAT_ICDCLK       EQU  H'0004'
LAT_ICDDAT       EQU  H'0005'
PORT_ICDCLK      EQU  H'0006'
PORT_ICDDAT      EQU  H'0007'


;----- ICDCON0 Bits -----------------------------------------------------
RSTVEC           EQU  H'0000'
DBGINEX          EQU  H'0003'
SSTEP            EQU  H'0005'
FREEZE           EQU  H'0006'
INBUG            EQU  H'0007'


;----- ICDDC Bits -----------------------------------------------------
BP1STAT          EQU  H'0000'
HLTSTAT          EQU  H'0003'
XTHI             EQU  H'0004'
DTSTAT           EQU  H'0005'
XTEN             EQU  H'0006'
DCEN             EQU  H'0007'


;----- ICDSTAT Bits -----------------------------------------------------
USRHLTF          EQU  H'0001'
TRP0HLTF         EQU  H'0006'
TRP1HLTF         EQU  H'0007'


;----- CLKRFRZ Bits -----------------------------------------------------
CLKRFRZEN        EQU  H'0000'


;----- ICDINSTL Bits -----------------------------------------------------
DBGIN0           EQU  H'0000'
DBGIN1           EQU  H'0001'
DBGIN2           EQU  H'0002'
DBGIN3           EQU  H'0003'
DBGIN4           EQU  H'0004'
DBGIN5           EQU  H'0005'
DBGIN6           EQU  H'0006'
DBGIN7           EQU  H'0007'


;----- ICDINSTH Bits -----------------------------------------------------
DBGIN8           EQU  H'0000'
DBGIN9           EQU  H'0001'
DBGIN10          EQU  H'0002'
DBGIN11          EQU  H'0003'
DBGIN12          EQU  H'0004'
DBGIN13          EQU  H'0005'


;----- ICDBP0AL Bits -----------------------------------------------------
BPA0             EQU  H'0000'
BPA1             EQU  H'0001'
BPA2             EQU  H'0002'
BPA3             EQU  H'0003'
BPA4             EQU  H'0004'
BPA5             EQU  H'0005'
BPA6             EQU  H'0006'
BPA7             EQU  H'0007'


;----- ICDBP0AH Bits -----------------------------------------------------
BPA8             EQU  H'0000'
BPA9             EQU  H'0001'
BPA10            EQU  H'0002'
BPA11            EQU  H'0003'
BPA12            EQU  H'0004'
BPA13            EQU  H'0005'
BPA14            EQU  H'0006'


;----- ICDBP0D Bits -----------------------------------------------------
BPD0             EQU  H'0000'
BPD1             EQU  H'0001'
BPD2             EQU  H'0002'
BPD3             EQU  H'0003'
BPD4             EQU  H'0004'
BPD5             EQU  H'0005'
BPD6             EQU  H'0006'
BPD7             EQU  H'0007'


;----- ICDBP0CON Bits -----------------------------------------------------
CAPEN            EQU  H'0003'
DCMP             EQU  H'0004'
MSSEL0           EQU  H'0005'
MSSEL1           EQU  H'0006'
BPEN             EQU  H'0007'


;----- ICDBP0CNT Bits -----------------------------------------------------
BPC0             EQU  H'0000'
BPC1             EQU  H'0001'
BPC2             EQU  H'0002'
BPC3             EQU  H'0003'
BPC4             EQU  H'0004'
BPC5             EQU  H'0005'
BPC6             EQU  H'0006'
BPC7             EQU  H'0007'


;----- STATUS_SHAD Bits -----------------------------------------------------
C_SHAD           EQU  H'0000'
DC_SHAD          EQU  H'0001'
Z_SHAD           EQU  H'0002'



;==========================================================================
;
;       RAM Definitions
;
;==========================================================================
       __MAXRAM  H'0FFF'
       __BADRAM  H'000F'-H'0010'
       __BADRAM  H'0014'
       __BADRAM  H'0015'
       __BADRAM  H'0019'
       __BADRAM  H'001A'
       __BADRAM  H'001B'
       __BADRAM  H'0091'-H'009F'
       __BADRAM  H'010C'-H'0118'
       __BADRAM  H'0195'-H'019F'
       __BADRAM  H'0218'-H'021D'
       __BADRAM  H'021F'
       __BADRAM  H'0298'-H'029F'
       __BADRAM  H'0314'-H'031F'
       __BADRAM  H'0393'-H'0396'
       __BADRAM  H'0412'
       __BADRAM  H'0413'
       __BADRAM  H'0414'
       __BADRAM  H'0415'
       __BADRAM  H'048C'-H'049F'
       __BADRAM  H'050C'
       __BADRAM  H'051D'-H'051F'
       __BADRAM  H'058C'
       __BADRAM  H'0592'
       __BADRAM  H'059D'-H'059F'
       __BADRAM  H'060C'
       __BADRAM  H'061A'-H'061C'
       __BADRAM  H'061D'-H'061F'
       __BADRAM  H'0650'-H'066F'
       __BADRAM  H'068C'
       __BADRAM  H'068D'
       __BADRAM  H'068E'
       __BADRAM  H'068F'
       __BADRAM  H'0690'
       __BADRAM  H'0691'
       __BADRAM  H'0692'
       __BADRAM  H'0693'
       __BADRAM  H'0694'
       __BADRAM  H'0695'
       __BADRAM  H'0696'
       __BADRAM  H'0697'
       __BADRAM  H'0698'
       __BADRAM  H'0699'
       __BADRAM  H'069A'
       __BADRAM  H'069B'
       __BADRAM  H'069C'
       __BADRAM  H'069D'
       __BADRAM  H'069E'
       __BADRAM  H'069F'
       __BADRAM  H'06A0'-H'06EF'
       __BADRAM  H'0714'-H'0715'
       __BADRAM  H'071E'-H'071F'
       __BADRAM  H'0720'-H'076F'
       __BADRAM  H'078C'-H'0795'
       __BADRAM  H'079B'
       __BADRAM  H'079C'
       __BADRAM  H'079D'
       __BADRAM  H'079E'
       __BADRAM  H'079F'
       __BADRAM  H'07A0'-H'07EF'
       __BADRAM  H'0815'-H'0819'
       __BADRAM  H'0820'-H'086F'
       __BADRAM  H'0894'-H'089F'
       __BADRAM  H'08A0'-H'08EF'
       __BADRAM  H'090C'-H'090E'
       __BADRAM  H'0912'
       __BADRAM  H'0915'
       __BADRAM  H'0918'
       __BADRAM  H'091B'-H'091E'
       __BADRAM  H'091F'
       __BADRAM  H'0920'-H'096F'
       __BADRAM  H'098C'-H'098E'
       __BADRAM  H'0994'
       __BADRAM  H'099E'-H'099F'
       __BADRAM  H'09A0'-H'09EF'
       __BADRAM  H'0A0C'-H'0A1D'
       __BADRAM  H'0A1E'-H'0A1F'
       __BADRAM  H'0A20'-H'0A6F'
       __BADRAM  H'0A8C'-H'0A9E'
       __BADRAM  H'0A9F'
       __BADRAM  H'0AA0'-H'0AEF'
       __BADRAM  H'0B0C'-H'0B1E'
       __BADRAM  H'0B1F'
       __BADRAM  H'0B20'-H'0B6F'
       __BADRAM  H'0B8C'-H'0B9E'
       __BADRAM  H'0B9F'
       __BADRAM  H'0BA0'-H'0BEF'
       __BADRAM  H'0C0C'-H'0C1E'
       __BADRAM  H'0C1F'
       __BADRAM  H'0C20'-H'0C6F'
       __BADRAM  H'0C8C'-H'0C9E'
       __BADRAM  H'0C9F'
       __BADRAM  H'0CA0'-H'0CEF'
       __BADRAM  H'0D0C'-H'0D1E'
       __BADRAM  H'0D1F'
       __BADRAM  H'0D20'-H'0D6F'
       __BADRAM  H'0D8E'-H'0D8F'
       __BADRAM  H'0D93'-H'0D95'
       __BADRAM  H'0D97'-H'0D9E'
       __BADRAM  H'0D9F'
       __BADRAM  H'0DA0'-H'0DEF'
       __BADRAM  H'0E24'-H'0E6E'
       __BADRAM  H'0E6F'
       __BADRAM  H'0E8C'-H'0E8E'
       __BADRAM  H'0E96'-H'0E9B'
       __BADRAM  H'0E9E'-H'0EA0'
       __BADRAM  H'0EA3'-H'0EBA'
       __BADRAM  H'0EBF'-H'0EC2'
       __BADRAM  H'0EC8'-H'0ECA'
       __BADRAM  H'0ECD'-H'0ECF'
       __BADRAM  H'0ED4'-H'0ED7'
       __BADRAM  H'0EDA'-H'0EED'
       __BADRAM  H'0EEE'
       __BADRAM  H'0EEF'
       __BADRAM  H'0F0C'-H'0F0F'
       __BADRAM  H'0F14'-H'0F17'
       __BADRAM  H'0F20'-H'0F23'
       __BADRAM  H'0F27'-H'0F37'
       __BADRAM  H'0F42'
       __BADRAM  H'0F4B'-H'0F4E'
       __BADRAM  H'0F50'-H'0F51'
       __BADRAM  H'0F56'-H'0F6E'
       __BADRAM  H'0F6F'
       __BADRAM  H'0F8F'-H'0F90'
       __BADRAM  H'0F93'-H'0F95'
       __BADRAM  H'0F9A'
       __BADRAM  H'0F9E'-H'0FC0'
       __BADRAM  H'0FC1'-H'0FDF'
       __BADRAM  H'0FE0'-H'0FE2'
       __BADRAM  H'0FEC'

;==========================================================================
;
;       Configuration Bits
;
;   NAME            Address
;   CONFIG1           8007h
;   CONFIG2           8008h
;   CONFIG3           8009h
;   CONFIG4           800Ah
;   CONFIG5           800Bh
;
;==========================================================================

; The following is an assignment of address values for all of the
; configuration registers for the purpose of table reads
_CONFIG1        EQU  H'8007'
_CONFIG2        EQU  H'8008'
_CONFIG3        EQU  H'8009'
_CONFIG4        EQU  H'800A'
_CONFIG5        EQU  H'800B'

;----- CONFIG1 Options --------------------------------------------------
_ICM_CM_HIGHCMR      EQU  H'3FFE'; Input Current Monitor High Voltage Range
_ICM_CM_LOWCMR       EQU  H'3FFF'; Input Current Monitor High Voltage Range

_RSTOSC_LFINTOSC     EQU  H'3FCF'; 32KHz
_RSTOSC_HFINTOSC0    EQU  H'3FDF'; 4MHz
_RSTOSC_HFINTOSC1    EQU  H'3FEF'; 16MHz
_RSTOSC_RESERVED     EQU  H'3FFF'; RESERVED

_CSWEN_OFF           EQU  H'37FF'; The NOSC and NDIV bits cannot be changed by user software
_CSWEN_ON            EQU  H'3FFF'; Writing to NOSC and NDIV is allowed

;----- CONFIG2 Options --------------------------------------------------
_MCLRE_OFF           EQU  H'3FFE'; MCLR pin function is port defined function
_MCLRE_ON            EQU  H'3FFF'; MCLR pin is Master Clear function

_PWRTE_ON            EQU  H'3FFD'; PWRT enabled
_PWRTE_OFF           EQU  H'3FFF'; PWRT disabled

_LPBOREN_ON          EQU  H'3FDF'; ULPBOR enabled
_LPBOREN_OFF         EQU  H'3FFF'; ULPBOR disabled

_BOREN_OFF           EQU  H'3F3F'; Brown-out reset disabled
_BOREN_SBOREN        EQU  H'3F7F'; Brown-out reset enabled according to SBOREN bit
_BOREN_NSLEEP        EQU  H'3FBF'; Brown-out Reset enabled while running, disabled in sleep; SBOREN is ignored
_BOREN_ON            EQU  H'3FFF'; Brown-out Reset Enabled, SBOREN bit is ignored

_DEMDIS_ON           EQU  H'3BFF'; Diode Emulation Mode Enabled
_DEMDIS_OFF          EQU  H'3FFF'; Diode Emulation Mode Disabled

_PPS1WAY_OFF         EQU  H'37FF'; The PPSLOCK bit can be set and cleared repeatedly by software
_PPS1WAY_ON          EQU  H'3FFF'; The PPSLOCK bit can be cleared and set only once in software

_STVREN_OFF          EQU  H'2FFF'; Stack Overflow or Underflow will not cause a reset
_STVREN_ON           EQU  H'3FFF'; Stack Overflow or Underflow will cause a reset

_DEBUG_ON            EQU  H'1FFF'; background debugger enabled
_DEBUG_OFF           EQU  H'3FFF'; Background Debugger disabled

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

_WDTE_OFF            EQU  H'3F9F'; WDT Disabled, SWDTEN is ignored
_WDTE_SWDTEN         EQU  H'3FBF'; WDT enabled/disabled by SWDTEN bit in WDTCON0
_WDTE_NSLEEP         EQU  H'3FDF'; WDT enabled while sleep=0, suspended when sleep=1; SWDTEN ignored
_WDTE_ON             EQU  H'3FFF'; WDT enabled regardless of sleep; SWDTEN ignored

_WDTCWS_WDTCWS_0     EQU  H'38FF'; window delay = 87.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_1     EQU  H'39FF'; window delay = 75 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_2     EQU  H'3AFF'; window delay = 62.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_3     EQU  H'3BFF'; window delay = 50 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_4     EQU  H'3CFF'; window delay = 37.5 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_5     EQU  H'3DFF'; window delay = 25 percent of time; no software control; keyed access required
_WDTCWS_WDTCWS_6     EQU  H'3EFF'; window always open (100%); no software control; keyed access required
_WDTCWS_WDTCWS_7     EQU  H'3FFF'; window always open (100%); software control; keyed access not required

_WDTCCS_LFINTOSC     EQU  H'07FF'; WDT reference clock is the 31.0kHz LFINTOSC output
_WDTCCS_HFINTOSC     EQU  H'0FFF'; WDT reference clock is the 31.25 kHz HFINTOSC
_WDTCCS_SC           EQU  H'3FFF'; Software Control

;----- CONFIG4 Options --------------------------------------------------
_WRT_ON              EQU  H'3FFC'; 0x0000 to 0x1FFF write protected
_WRT_WRT_lower       EQU  H'3FFD'; 0x0000 to x0FFF write protected
_WRT_WRT_upper       EQU  H'3FFE'; 0x0000 to 0x01FF write protected
_WRT_OFF             EQU  H'3FFF'; Write protection off

_WRTC_ON             EQU  H'3EFF'; Configuration Words are Write Protected
_WRTC_OFF            EQU  H'3FFF'; Configuration Words Not Write Protected

_SCANE_not_available EQU  H'2FFF'; Scanner module is not available for use
_SCANE_available     EQU  H'3FFF'; Scanner module is available for use

_LVP_OFF             EQU  H'1FFF'; High Voltage on MCLR/Vpp must be used for programming
_LVP_ON              EQU  H'3FFF'; Low Voltage programming enabled. MCLR/Vpp pin function is MCLR.

;----- CONFIG5 Options --------------------------------------------------
_CP_ON               EQU  H'3FFE'; Program Memory code protection enabled
_CP_OFF              EQU  H'3FFF'; Program Memory code protection disabled

_CPD_ON              EQU  H'3FFD'; Data EEPROM code protection enabled
_CPD_OFF             EQU  H'3FFF'; Data EEPROM code protection disabled

;----- DEVID Equates --------------------------------------------------

;----- IDLOC Equates --------------------------------------------------
_IDLOC0          EQU  H'8000'
_IDLOC1          EQU  H'8001'
_IDLOC2          EQU  H'8002'
_IDLOC3          EQU  H'8003'

        LIST
