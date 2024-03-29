
       LIST
; MCP250XX.INC  Standard Header File, Version 2.00    Microchip Technology, Inc.
	NOLIST

; This header file is used to define the configuration for MCP250XX devices.
; To use this header file, include it in the beginning of a source file.  Use
; the various macros below to select the configuration options.  Then at the end
; of the source file, invoke the macro GENERATE.

;*******************************************************************************
;CHANGE HISTORY
;9/13/01 PKR Version 0.20
;- Added a "DEVICE" macro to select between mixed signal (MCP2505x) and digital
;  only (MCP2502x) devices.
;- Fixed "RECEIVE_BUFFERS" and "TRANSMIT_BUFFERS" macro.  Extended IDs were not
;  being configured correctly.
;
;10/29/01 PKR Version 0.30
;- Corrected the DATA directives in the "GENERATE" macro to place bytes in the
;  proper order.  Bytes were incorrectly swapped in the resulting HEX file.
;
;11/14/01 PKR Version 1.00
;- Initial release
;
;06/25/02 PKR Version 2.00
;- Added the ability to configure the Configuration Word
;*******************************************************************************
;	Quick Reference
;
; CAN Configuration Macros
; DEVICE                  SET_DEVICE
;	RECEIVE_BUFFERS		      MASK, MID, FILTER0, F0ID, FILTER1, F1ID
;	TRANSMIT_BUFFERS	      TXB0, TXB0ID, TXB1, TXB1ID, TXB2, TXB2ID
;	CAN_BIT_TIMING		      BRP, SJW, PS1, PS2, PROP, SP, P2S, WF
; CAN_MODE	MACRO         MODE, PUMODE, TXID1MSG, ERROR_RECOVERY, TX_ON_ERROR, SLEEPMODE, MSGTYPE, PWM_POR, LOTOSLEEP
;	SCHEDULED_TRANSMISSION	MODE, INFO, MULTIPLIER, FREQUENCY
;
; Peripheral Configuration Macros
;	CLOCK_OUT		      MODE, PRESCALER
;	IO_DIRECTION		  B6, B5, B4, B3, B2, B1, B0
;	IO_LATCH		      B6, B5, B4, B3, B2, B1, B0
;	IO_FUNCTION		    B3, B2, B1, B0
;	IO_XMIT_ON_CHANGE	B7, B6, B5, B4, B3, B2, B1, B0
;	IO_WEAK_PULLUPS		MODE
;	PWM1			        TMRON, PRESCALER, PERIOD, DUTY_CYCLE
;	PWM2			        TMRON, PRESCALER, PERIOD, DUTY_CYCLE
;	ADC_SETUP		      MODE, SEQ_DELAY, CLOCK_SOURCE, VREF_POS, VREF_NEG, TIME
;	ADC_COMPARES		  CHANNEL0, CHANNEL1, CHANNEL2, CHANNEL3
;
; User Defined Configuration Macros
;	SET_USER#		VALUE
;
; Generate Configuration Information - REQUIRED!
;	GENERATE

	LIST	P=EEPROM16, M=0x100
	NOEXPAND

;*******************************************************************************
; __config
;
; Configuration bits
;
; Reset Setting & Osc Setting
; Example: __config _RST_ON & _HS_OSC
;
_RST_ON                      EQU     H'3FFF'
_RST_OFF                     EQU     H'3FFB'
_LP_OSC                      EQU     H'3FFC'
_XT_OSC                      EQU     H'3FFD'
_HS_OSC                      EQU     H'3FFF'


;*******************************************************************************
;*******************************************************************************
; Initialize configuration words
;*******************************************************************************
;*******************************************************************************

IOINTEN		SET	H'FF'
IOINTPO		SET	H'FF'
GPLAT		SET	H'FF'
RES_03		SET	H'FF'
OPTREG1		SET	H'FF'
T1CON		SET	H'FF'
T2CON		SET	H'FF'
PR1		SET	H'FF'
PR2		SET	H'FF'
PWM1DCH		SET	H'FF'
PWM2DCH		SET	H'FF'
CNF1		SET	H'FF'
CNF2		SET	H'FF'
CNF3		SET	H'FF'
ADCON0		SET	H'FF'
ADCON1		SET	H'FF'
STCON		SET	H'FF'
OPTREG2		SET	H'FF'
RES_12		SET	H'FF'
RES_13		SET	H'FF'
RXMSIDH		SET	H'FF'
RXMSIDL		SET	H'FF'
RXMEID8		SET	H'FF'
RXMEID0		SET	H'FF'
RXF0SIDH	SET	H'FF'
RXF0SIDL	SET	H'FF'
RXF0EID8	SET	H'FF'
RXF0EID0	SET	H'FF'
RXF1SIDH	SET	H'FF'
RXF1SIDL	SET	H'FF'
RXF1EID8	SET	H'FF'
RXF1EID0	SET	H'FF'
TXID0SIDH	SET	H'FF'
TXID0SIDL	SET	H'FF'
TXID0EID8	SET	H'FF'
TXID0EID0	SET	H'FF'
TXID1SIDH	SET	H'FF'
TXID1SIDL	SET	H'FF'
TXID1EID8	SET	H'FF'
TXID1EID0	SET	H'FF'
TXID2SIDH	SET	H'FF'
TXID2SIDL	SET	H'FF'
TXID2EID8	SET	H'FF'
TXID2EID0	SET	H'FF'
ADCMP3H		SET	H'FF'
ADCMP3L		SET	H'FF'
ADCMP2H		SET	H'FF'
ADCMP2L		SET	H'FF'
ADCMP1H		SET	H'FF'
ADCMP1L		SET	H'FF'
ADCMP0H		SET	H'FF'
ADCMP0L		SET	H'FF'
GPDDR		SET	H'FF'
USER0		SET	H'FF'
USER1		SET	H'FF'
USER2		SET	H'FF'
USER3		SET	H'FF'
USER4		SET	H'FF'
USER5		SET	H'FF'
USER6		SET	H'FF'
USER7		SET	H'FF'
USER8		SET	H'FF'
USER9		SET	H'FF'
USERA		SET	H'FF'
USERB		SET	H'FF'
USERC		SET	H'FF'
USERD		SET	H'FF'
USERE		SET	H'FF'
USERF		SET	H'FF'
CRCSUM		SET	H'FF'

;*******************************************************************************
;*******************************************************************************
; Internal values
;*******************************************************************************
;*******************************************************************************

ADC_FOSC_2	EQU	0x00
ADC_FOSC_8	EQU	0x40
ADC_FOSC_32	EQU	0x80
ADC_FRC		EQU	0xC0
ADC_RESULT	EQU	0x40
ANALOG		EQU	0
CLK_FOSC	EQU	0x00
CLK_FOSC_2	EQU	0x10
CLK_FOSC_4	EQU	0x20
CLK_FOSC_8	EQU	0x30
CMD_ACK     EQU 0x80
DIGITAL		EQU	1
DISABLED	EQU	0
ENABLED		EQU	1
ERR_RECVRY_LO EQU 0x40
ERR_RECVRY_NORM EQU 0
EXTENDED	EQU	1
EXTERNAL	EQU	1
INPUT		EQU	1
IO_FALL		EQU	1
IO_RISE		EQU	3
LISTEN_ONLY	EQU	0x04
LO_TO_SLEEP_EN EQU 0x02
LO_TO_SLEEP_DIS EQU 0
MCP2502X      EQU 0
MCP2505X      EQU 1
MSG_TYPE_DATA     EQU 0x08
MSG_TYPE_RTR     EQU 0
NO_TX_ON_ERR      EQU 0
NONE		EQU	0x00
NORMAL		EQU	0x00
OUTPUT		EQU	0
PHASE1_AND_IPT	EQU	0x00
PHASE2		EQU	0x80
PWRUP_LO          EQU 0
PWRUP_NORM      EQU 0x01
PWM_POR_DEF   EQU 0x04
PWM_POR_UNCH   EQU 0
RX_OVRFLW       EQU 0
SLEEP_DIS      EQU 0
SLEEP_EN     EQU 0x10
STANDARD	EQU	0
TIMES_1		EQU	0x00
TIMES_3		EQU	0x40
TOSC32		EQU	0x00
TOSC64		EQU	0x10
TOSC128		EQU	0x20
TOSC256	EQU	0x30
TOSC512	EQU	0x40
TOSC1024	EQU	0x50
TOSC2048	EQU	0x60
TOSC4096	EQU	0x70
TX_ON_ERR     EQU 0x20
VDD		EQU	0
VSS		EQU	0
X1_64TOSC	EQU	0x00
X2_64TOSC	EQU	0x01
X4_64TOSC	EQU	0x02
X8_64TOSC	EQU	0x03
X1_4096TOSC	EQU	0x00
X16_4096TOSC	EQU	0x10
X256_4096TOSC	EQU	0x20
X4096_4096TOSC	EQU	0x30

DEVICE_NUM  SET 0

;*******************************************************************************
;*******************************************************************************
; Configuration Macros
;*******************************************************************************
;*******************************************************************************

;*******************************************************************************
; DEVICE
;
; Call this macro to set the device for mixed signal (MCP2505X)
; or digital only (MCP2502X).
;
; SET_DEVICE enter MCP2505X or MCP2502X

DEVICE MACRO SET_DEVICE
  DEVICE_NUM SET SET_DEVICE
  ENDM

;*******************************************************************************
; RECEIVE_BUFFERS
;
; Call this macro to set up the receive buffers.  Macro paramaters are:
;
;	MASK		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	MID		enter STANDARD or EXTENDED
;	FILTER0		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	F0ID		enter STANDARD or EXTENDED
;	FILTER1		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	F1ID		enter STANDARD or EXTENDED

RECEIVE_BUFFERS	MACRO	MASK, MID, FILTER0, F0ID, FILTER1, F1ID

	IF MID == STANDARD
RXMSIDH		SET	MASK >> 3
RXMSIDL		SET	((MASK & 0x07) << 5) + 3
	ELSE
RXMSIDH   SET (MASK >> 0x15) & 0xFF
RXMSIDL		SET	0x08 + (((MASK >> 0x0D) & 0xE0) | ((MASK >> 0x10) & 0x03))
RXMEID8		SET	(MASK >> 8) & 0xFF
RXMEID0		SET	MASK & 0xFF
	ENDIF

	IF F0ID == STANDARD
RXF0SIDH	SET	FILTER0 >> 3
RXF0SIDL	SET	((FILTER0 & 0x07) << 5) + 3
	ELSE
RXF0SIDH   SET (FILTER0 >> 0x15) & 0xFF
RXF0SIDL		SET	0x08 + (((FILTER0 >> 0x0D) & 0xE0) | ((FILTER0 >> 0x10) & 0x03))
RXF0EID8		SET	(FILTER0 >> 8) & 0xFF
RXF0EID0		SET	FILTER0 & 0xFF
	ENDIF

	IF F1ID == STANDARD
RXF1SIDH	SET	FILTER1 >> 3
RXF1SIDL	SET	((FILTER1 & 0x07) << 5) + 3
	ELSE
RXF1SIDH   SET (FILTER1 >> 0x15) & 0xFF
RXF1SIDL		SET	0x08 + (((FILTER1 >> 0x0D) & 0xE0) | ((FILTER1 >> 0x10) & 0x03))
RXF1EID8		SET	(FILTER1 >> 8) & 0xFF
RXF1EID0		SET	FILTER1 & 0xFF
	ENDIF

	ENDM

;*******************************************************************************
; TRANSMIT_BUFFERS
;
; Call this macro to set up the transmit buffers.  Macro parameters are:
;
;	TXB0		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	TXB0ID		enter STANDARD or EXTENDED
;	TXB1		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	TXB1ID		enter STANDARD or EXTENDED
;	TXB2		message identifier [eleven (max 7FFh) or twenty nine bits (max 1FFFFFFF)]
;	TXB2ID		enter STANDARD or EXTENDED

TRANSMIT_BUFFERS	MACRO	TXB0, TXB0ID, TXB1, TXB1ID, TXB2, TXB2ID

	IF TXB0ID == STANDARD
TXID0SIDH	SET	TXB0 >> 3
TXID0SIDL	SET	((TXB0 & 0x07) << 5) + 3
	ELSE
TXID0SIDH   SET (TXB0 >> 0x15) & 0xFF
TXID0SIDL		SET	0x08 + (((TXB0 >> 0x0D) & 0xE0) | ((TXB0 >> 0x10) & 0x03))
TXID0EID8		SET	(TXB0 >> 8) & 0xFF
TXID0EID0		SET	TXB0 & 0xFF
	ENDIF

	IF TXB1ID == STANDARD
TXID1SIDH	SET	TXB1 >> 3
TXID1SIDL	SET	((TXB1 & 0x07) << 5) + 3
	ELSE
TXID1SIDH   SET (TXB1 >> 0x15) & 0xFF
TXID1SIDL		SET	0x08 + (((TXB1 >> 0x0D) & 0xE0) | ((TXB1 >> 0x10) & 0x03))
TXID1EID8		SET	(TXB1 >> 8) & 0xFF
TXID1EID0		SET	TXB1 & 0xFF
	ENDIF

	IF TXB2ID == STANDARD
TXID2SIDH	SET	TXB2 >> 3
TXID2SIDL	SET	((TXB2 & 0x07) << 5) + 3
	ELSE
TXID2SIDH   SET (TXB2 >> 0x15) & 0xFF
TXID2SIDL		SET	0x08 + (((TXB2 >> 0x0D) & 0xE0) | ((TXB2 >> 0x10) & 0x03))
TXID2EID8		SET	(TXB2 >> 8) & 0xFF
TXID2EID0		SET	TXB2 & 0xFF
	ENDIF

	ENDM

;*******************************************************************************
; CAN_BIT_TIMING
;
; Call this macro to set up the CAN bit timing.  Macro parameters are:
;
;	BRP	Baud Rate Prescaler	enter a value from 0 to 0x3F
;	SJW	Synchronized Jump Width	enter a value from 1 to 4
;	PS1	Phase 1 Segment Width	enter a value from 1 to 8
;	PS2	Phase 2 Segment Width	enter a value from 2 to 8
;	PROP	Propagation Width	enter a value from 1 to 8
;	SP	Sample Point		enter TIMES_3 or TIMES_1
;	P2S	Phase 2 Source		enter PHASE2 or PHASE1_IPT
;	WF	Wake-up Filter		enter ENABLED or DISABLED

CAN_BIT_TIMING	MACRO	BRP, SJW, PS1, PS2, PROP, SP, P2S, WF

CNF1	SET	((SJW-1) << 6) + BRP
CNF2	SET	P2S + (SP << 6) + ((PS1-1) << 3) + (PROP-1)
CNF3	SET	(WF << 6) + (PS2-1)
	ENDM

;*******************************************************************************
; CAN_MODE
;
; Call this macro to set up the initial CAN mode.  Macro parameters are:
;
; MODE  enter NORMAL or LISTEN_ONLY
;	PUMODE	enter PWRUP_NORM or PWRUP_LO
; TXID1MSG enter CMD_ACK or RX_OVRFLW
; ERROR_RECOVERY  enter ERR_RECVRY_LO or ERR_RECVRY_NORM
; TX_ON_ERROR enter TX_ON_ERR or NO_TX_ON_ERR
; SLEEPMODE enter SLEEP_EN or SLEEP_DIS
; MSGTYPE enter MSG_TYPE_DATA or MSG_TYPE_RTR
; PWM_POR enter PWM_POR_DEF OR PWM_POR_UNCH
; LOTOSLEEP enter LO_TO_SLEEP_EN OR LO_TO_SLEEP_DIS

CAN_MODE	MACRO   MODE, PUMODE, TXID1MSG, ERROR_RECOVERY, TX_ON_ERROR, SLEEPMODE, MSGTYPE, PWM_POR, LOTOSLEEP
TEMP    SET PUMODE | TXID1MSG | ERROR_RECOVERY | TX_ON_ERROR
OPTREG2 SET TEMP | SLEEPMODE | MSGTYPE | PWM_POR | LOTOSLEEP
OPTREG1	SET	(OPTREG1 & 0xF3) | MODE
	ENDM

;*******************************************************************************
; SCHEDULED_TRANSMISSION
;
; Call this macro to set up scheduled transmission.  Macro parameters are:
;
;	MODE		enter ENABLED or DISABLED
;	INFO		enter NONE or ADC_RESULT
;	MULTIPLIER	enter a value from 1 to 0x10
;	FREQUENCY	enter X1_4096TOSC, X16_4096TOSC, X256_4096TOSC, or
;				X4096_4096TOSC

SCHEDULED_TRANSMISSION	MACRO   MODE, INFO, MULTIPLIER, FREQUENCY

STCON	SET	(MODE << 7) + INFO + FREQUENCY + (MULTIPLIER-1)
	ENDM

;*******************************************************************************
; CLOCK_OUT
;
; Call this macro to configure the Clock Out Function.  Macro parameters are:
;
;	MODE		enter ENABLED or DISABLED
;	PRESCALER	enter CLK_FOSC, CLK_FOSC_2, CLK_FOSC_4, or CLK_FOSC_8

CLOCK_OUT	MACRO	MODE, PRESCALER

  IF MODE == 0
OPTREG1	SET	(OPTREG1 & 0x8F) | (0x40 + PRESCALER)
  ELSE
OPTREG1	SET	(OPTREG1 & 0x8F) | PRESCALER
  ENDIF
	ENDM

;*******************************************************************************
; IO_DIRECTION
;
; Call this macro to set up the I/O direction for the  digital I/O's.
; Macro parameters are:
;
;	B6		enter INPUT or OUTPUT for pin GP6
;	B5		enter INPUT or OUTPUT for pin GP5
;	B4		enter INPUT or OUTPUT for pin GP4
;	B3		enter INPUT or OUTPUT for pin GP3
;	B2		enter INPUT or OUTPUT for pin GP2
;	B1		enter INPUT or OUTPUT for pin GP1
;	B0		enter INPUT or OUTPUT for pin GP0

IO_DIRECTION	MACRO	B6, B5, B4, B3, B2, B1, B0

TEMP	SET	(((((B6 << 1) + B5) << 1) + B4) << 1) + B3
GPDDR	SET	(((((TEMP << 1) + B2) << 1) + B1) << 1) + B0
	ENDM

;*******************************************************************************
; IO_LATCH
;
; Call this macro to set the latch values for the digital I/O's.  Macro
; parameters are:
;
;	B6		enter 0 or 1 for pin GP6
;	B5		enter 0 or 1 for pin GP5
;	B4		enter 0 or 1 for pin GP4
;	B3		enter 0 or 1 for pin GP3
;	B2		enter 0 or 1 for pin GP2
;	B1		enter 0 or 1 for pin GP1
;	B0		enter 0 or 1 for pin GP0

IO_LATCH	MACRO	B6, B5, B4, B3, B2, B1, B0

TEMP	SET	(((((B6 << 1) + B5) << 1) + B4) << 1) + B3
GPLAT	SET	(((((TEMP << 1) + B2) << 1) + B1) << 1) + B0
	ENDM

;*******************************************************************************
; IO_FUNCTION
;
; Call this macro to set the pin function to analog or digital.  Macro
; parameters are:
;
;	B3		enter ANALOG or DIGITAL for pin AN3
;	B2		enter ANALOG or DIGITAL for pin AN2
;	B1		enter ANALOG or DIGITAL for pin AN1
;	B0		enter ANALOG or DIGITAL for pin AN0

IO_FUNCTION	MACRO	B3, B2, B1, B0

  IF DEVICE_NUM == MCP2502X
ADCON1  SET 0x0F
  ELSE 
ADCON1	SET	(ADCON1 & 0xF0) | (((((B3 << 1) + B2) << 1) + B1) << 1) + B0
  ENDIF
	ENDM

;*******************************************************************************
; IO_XMIT_ON_CHANGE
;
; Call this macro to set the Transmit on Change for each pin.  Macro parameters
; are:
;
;	B7		enter DISABLE, IO_RISE, or IO_FALL for pin GP7
;	B6		enter DISABLE, IO_RISE, or IO_FALL for pin GP6
;	B5		enter DISABLE, IO_RISE, or IO_FALL for pin GP5
;	B4		enter DISABLE, IO_RISE, or IO_FALL for pin GP4
;	B3		enter DISABLE, IO_RISE, or IO_FALL for pin GP3
;	B2		enter DISABLE, IO_RISE, or IO_FALL for pin GP2
;	B1		enter DISABLE, IO_RISE, or IO_FALL for pin GP1
;	B0		enter DISABLE, IO_RISE, or IO_FALL for pin GP0

IO_XMIT_ON_CHANGE	MACRO	B7, B6, B5, B4, B3, B2, B1, B0

TEMP	SET	((((((B7&1) << 1) + (B6&1)) << 1) + (B5&1)) << 1) + (B4&1)
IOINTEN	SET	(((((((TEMP << 1) + (B3&1)) << 1) + (B2&1)) << 1) + (B1&1)) << 1) + (B0&1)
TEMP	SET	((((((B7&2) <<1) + (B6&2)) << 1) + (B5&2)) << 1) + (B4&2)
IOINTPO	SET	(((((((TEMP << 1) + (B3&2)) << 1) + (B2&2)) << 1) + (B1&2)) << 1) + (B0&2)
	ENDM

;*******************************************************************************
; IO_WEAK_PULLUPS
;
; Call this macro to configure weak pull-ups.  Macro parameters are:
;
;	MODE		enter ENABLED or DISABLED

IO_WEAK_PULLUPS	MACRO	MODE

  IF MODE == 0
OPTREG1	SET	(OPTREG1 & 0x7F) | 0x80
  ELSE
OPTREG1	SET	(OPTREG1 & 0x7F)
  ENDIF
	ENDM

;*******************************************************************************
; PWM1
;
; Call this macro to configure PWM1.  Macro parameters are:
;
;	TMRON		enter ENABLED or DISABLED
;	PRESCALER	Timer Prescaler; enter 1, 4, or 0x10
;	PERIOD		PWM Period; enter a value from 0 to 0xFF
;	DUTY_CYCLE	PWM Duty Cycle; enter a value from 0 to 0x3FF

PWM1	MACRO	TMRON, PRESCALER, PERIOD, DUTY_CYCLE

	IF PRESCALER == 0x10
TEMP	SET	0x20
	ELSE
	IF PRESCALER == 4
TEMP	SET	0x10
	ELSE
TEMP	SET	0x00
	ENDIF
	ENDIF
T1CON	SET	(TMRON << 7) | TEMP | (DUTY_CYCLE & 0x03)
PR1	SET	PERIOD
PWM1DCH	SET	DUTY_CYCLE >> 2
	ENDM

;*******************************************************************************
; PWM2
;
; Call this macro to configure PWM2.  Macro parameters are:
;
;	TMRON		enter ENABLED or DISABLED
;	PRESCALER	Timer Prescaler; enter 1, 4, or 0x10
;	PERIOD		PWM Period; enter a value from 0 to 0xFF
;	DUTY_CYCLE	PWM Duty Cycle; enter a value from 0 to 0x3FF

PWM2	MACRO	TMRON, PRESCALER, PERIOD, DUTY_CYCLE

	IF PRESCALER == 0x10
TEMP	SET	0x20
	ELSE
	IF PRESCALER == 4
TEMP	SET	0x10
	ELSE
TEMP	SET	0x00
	ENDIF
	ENDIF
T2CON	SET	(TMRON << 7) | TEMP | (DUTY_CYCLE & 0x03)
PR2	SET	PERIOD
PWM2DCH	SET	DUTY_CYCLE >> 2
	ENDM


;*******************************************************************************
; ADC_SETUP
;
; Call this macro to set up the ADC.  Macro parameters are:
;
;	MODE		enter ENABLED or DISABLED
;	SEQ_DELAY	Minimum Sequence Delay; enter TOSC32, TOSC64, TOSC128,
;				TOSC256, TOSC512, TOSC1024, TOSC2048, or TOSC4096
;	CLOCK_SOURCE	Clock Source; enter ADC_FOSC_2, ADC_FOSC_8, ADC_FOSC_32,
;				or ADC_FRC
;	VREF_POS	Vref+ Source; enter VDD or EXTERNAL
;	VREF_NEG	Vref- Source; enter VSS or EXTERNAL
;	TIME		Acquisition Time; enter X1_64TOSC, X2_64TOSC, X4_64TOSC,
;				or X8_64TOSC
;
; Note: Be sure to call the macro IO_FUNCTION to configure the pins as
;       analog or digital.

ADC_SETUP	MACRO	MODE, SEQ_DELAY, CLOCK_SOURCE, VREF_POS, VREF_NEG, TIME
  IF DEVICE_NUM == MCP2502X
ADCON0  SET 0x00
ADCON1  SET 0x0F
OPTREG1	SET	(OPTREG1 & 0xFC)
  ELSE
ADCON0	SET	(MODE << 7) + SEQ_DELAY
TEMP	SET	CLOCK_SOURCE + (VREF_NEG << 5) + (VREF_POS << 4)
ADCON1	SET	(ADCON1 & 0x0F) | TEMP
OPTREG1	SET	(OPTREG1 & 0xFC) | TIME
  ENDIF
	ENDM

;*******************************************************************************
; ADC_COMPARES
;
; Call this macro to set up the ADC compare values.  Macro parameters are:
;
;	CHANNEL0	enter a value from 0 to 0x3FF
;	CHANNEL1	enter a value from 0 to 0x3FF
;	CHANNEL2	enter a value from 0 to 0x3FF
;	CHANNEL3	enter a value from 0 to 0x3FF

ADC_COMPARES	MACRO	CHANNEL0, CHANNEL1, CHANNEL2, CHANNEL3

  IF DEVICE_NUM == MCP2502X
ADCMP0L	SET	0x00
ADCMP0H	SET	0x00
ADCMP1L	SET	0x00
ADCMP1H	SET	0x00
ADCMP2L	SET	0x00
ADCMP2H	SET	0x00
ADCMP3L	SET	0x00
ADCMP3H	SET	0x00
  ELSE ;IF DEVICE_NUM == MCP2505X
ADCMP0L	SET	(CHANNEL0 & 0x003) << 6
ADCMP0H	SET	(CHANNEL0 & 0x3FC) >> 2
ADCMP1L	SET	(CHANNEL1 & 0x003) << 6
ADCMP1H	SET	(CHANNEL1 & 0x3FC) >> 2
ADCMP2L	SET	(CHANNEL2 & 0x003) << 6
ADCMP2H	SET	(CHANNEL2 & 0x3FC) >> 2
ADCMP3L	SET	(CHANNEL3 & 0x003) << 6
ADCMP3H	SET	(CHANNEL3 & 0x3FC) >> 2
  ENDIF
	ENDM

;*******************************************************************************
; SET_USER#
;
; Call these macros to set the user data values.  Macro parameters are:
;
;	VALUE	enter a value from 0 to 0xFF

SET_USER0	MACRO	VALUE
USER0	SET	VALUE
	ENDM

SET_USER1	MACRO	VALUE
USER1	SET	VALUE
	ENDM

SET_USER2	MACRO	VALUE
USER2	SET	VALUE
	ENDM

SET_USER3	MACRO	VALUE
USER3	SET	VALUE
	ENDM

SET_USER4	MACRO	VALUE
USER4	SET	VALUE
	ENDM

SET_USER5	MACRO	VALUE
USER5	SET	VALUE
	ENDM

SET_USER6	MACRO	VALUE
USER6	SET	VALUE
	ENDM

SET_USER7	MACRO	VALUE
USER7	SET	VALUE
	ENDM

SET_USER8	MACRO	VALUE
USER8	SET	VALUE
	ENDM

SET_USER9	MACRO	VALUE
USER9	SET	VALUE
	ENDM

SET_USERA	MACRO	VALUE
USERA	SET	VALUE
	ENDM

SET_USERB	MACRO	VALUE
USERB	SET	VALUE
	ENDM

SET_USERC	MACRO	VALUE
USERC	SET	VALUE
	ENDM

SET_USERD	MACRO	VALUE
USERD	SET	VALUE
	ENDM

SET_USERE	MACRO	VALUE
USERE	SET	VALUE
	ENDM

SET_USERF	MACRO	VALUE
USERF	SET	VALUE
	ENDM

;*******************************************************************************
; GENERATE
;
; Call this macro after using all of the other macros to configure the device.
; This macro will actually generate the hex file output to configure the
; device using a Device Programmer.  This macro has no parameters.

GENERATE	MACRO

TEMP	SET	IOINTEN + IOINTPO + GPLAT + RES_03 + OPTREG1 + T1CON
TEMP	SET	TEMP + T2CON + PR1 + PR2 + PWM1DCH + PWM2DCH + CNF1
TEMP	SET	TEMP + CNF2 + CNF3 + ADCON0 + ADCON1 + STCON + OPTREG2
TEMP	SET	TEMP + RES_12 + RES_13 + RXMSIDH + RXMSIDL + RXMEID8
TEMP	SET	TEMP + RXMEID0 + RXF0SIDH + RXF0SIDL + RXF0EID8 + RXF0EID0
TEMP	SET	TEMP + RXF1SIDH + RXF1SIDL + RXF1EID8 + RXF1EID0 + TXID0SIDH
TEMP	SET	TEMP + TXID0SIDL + TXID0EID8 + TXID0EID0 + TXID1SIDH
TEMP	SET	TEMP + TXID1SIDL + TXID1EID8 + TXID1EID0 + TXID2SIDH
TEMP	SET	TEMP + TXID2SIDL + TXID2EID8 + TXID2EID0 + ADCMP3H + ADCMP3L
TEMP	SET	TEMP + ADCMP2H + ADCMP2L + ADCMP1H + ADCMP1L + ADCMP0H + ADCMP0L
TEMP	SET	TEMP + GPDDR + USER0 + USER1 + USER2 + USER3 + USER4 + USER5
TEMP	SET	TEMP + USER6 + USER7 + USER8 + USER9 + USERA + USERB + USERC
TEMP	SET	TEMP + USERD + USERE + USERF
CRCSUM	SET	(-TEMP) & 0xFF

;	EXPAND
	DATA	 0x3400 | (IOINTEN & 0xFF)
	DATA	 0x3400 | (IOINTPO & 0xFF)
	DATA	 0x3400 | (GPLAT & 0xFF)
	DATA	 0x3400 | (RES_03 & 0xFF)
	DATA	 0x3400 | (OPTREG1 & 0xFF)
	DATA	 0x3400 | (T1CON & 0xFF)
	DATA	 0x3400 | (T2CON & 0xFF)
	DATA	 0x3400 | (PR1 & 0xFF)
	DATA	 0x3400 | (PR2 & 0xFF)
	DATA	 0x3400 | (PWM1DCH & 0xFF)
	DATA	 0x3400 | (PWM2DCH & 0xFF)
	DATA	 0x3400 | (CNF1 & 0xFF)
	DATA	 0x3400 | (CNF2 & 0xFF)
	DATA	 0x3400 | (CNF3 & 0xFF)
	DATA	 0x3400 | (ADCON0 & 0xFF)
	DATA	 0x3400 | (ADCON1 & 0xFF)
	DATA	 0x3400 | (STCON & 0xFF)
	DATA	 0x3400 | (OPTREG2 & 0xFF)
	DATA	 0x3400 | (RES_12 & 0xFF)
	DATA	 0x3400 | (RES_13 & 0xFF)
	DATA	 0x3400 | (RXMSIDH & 0xFF)
	DATA	 0x3400 | (RXMSIDL & 0xFF)
	DATA	 0x3400 | (RXMEID8 & 0xFF)
	DATA	 0x3400 | (RXMEID0 & 0xFF)
	DATA	 0x3400 | (RXF0SIDH & 0xFF)
	DATA	 0x3400 | (RXF0SIDL & 0xFF)
	DATA	 0x3400 | (RXF0EID8 & 0xFF)
	DATA	 0x3400 | (RXF0EID0 & 0xFF)
	DATA	 0x3400 | (RXF1SIDH & 0xFF)
	DATA	 0x3400 | (RXF1SIDL & 0xFF)
	DATA	 0x3400 | (RXF1EID8 & 0xFF)
	DATA	 0x3400 | (RXF1EID0 & 0xFF)
	DATA	 0x3400 | (TXID0SIDH & 0xFF)
	DATA	 0x3400 | (TXID0SIDL & 0xFF)
	DATA	 0x3400 | (TXID0EID8 & 0xFF)
	DATA	 0x3400 | (TXID0EID0 & 0xFF)
	DATA	 0x3400 | (TXID1SIDH & 0xFF)
	DATA	 0x3400 | (TXID1SIDL & 0xFF)
	DATA	 0x3400 | (TXID1EID8 & 0xFF)
	DATA	 0x3400 | (TXID1EID0 & 0xFF)
	DATA	 0x3400 | (TXID2SIDH & 0xFF)
	DATA	 0x3400 | (TXID2SIDL & 0xFF)
	DATA	 0x3400 | (TXID2EID8 & 0xFF)
	DATA	 0x3400 | (TXID2EID0 & 0xFF)
	DATA	 0x3400 | (ADCMP3H & 0xFF)
	DATA	 0x3400 | (ADCMP3L & 0xFF)
	DATA	 0x3400 | (ADCMP2H & 0xFF)
	DATA	 0x3400 | (ADCMP2L & 0xFF)
	DATA	 0x3400 | (ADCMP1H & 0xFF)
	DATA	 0x3400 | (ADCMP1L & 0xFF)
	DATA	 0x3400 | (ADCMP0H & 0xFF)
	DATA	 0x3400 | (ADCMP0L & 0xFF)
	DATA	 0x3400 | (GPDDR & 0xFF)
	DATA	 0x3400 | (USER0 & 0xFF)
	DATA	 0x3400 | (USER1 & 0xFF)
	DATA	 0x3400 | (USER2 & 0xFF)
	DATA	 0x3400 | (USER3 & 0xFF)
	DATA	 0x3400 | (USER4 & 0xFF)
	DATA	 0x3400 | (USER5 & 0xFF)
	DATA	 0x3400 | (USER6 & 0xFF)
	DATA	 0x3400 | (USER7 & 0xFF)
	DATA	 0x3400 | (USER8 & 0xFF)
	DATA	 0x3400 | (USER9 & 0xFF)
	DATA	 0x3400 | (USERA & 0xFF)
	DATA	 0x3400 | (USERB & 0xFF)
	DATA	 0x3400 | (USERC & 0xFF)
	DATA	 0x3400 | (USERD & 0xFF)
	DATA	 0x3400 | (USERE & 0xFF)
	DATA	 0x3400 | (USERF & 0xFF)
	DATA	 0x3400 | (CRCSUM & 0xFF)

	ENDM

	LIST
































































