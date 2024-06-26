
; $Id: P18MACRO.INC,v 1.1 2003/12/17 19:32:43 sealep Exp $

;==========================================================================
;
;         MACROS
;
;==========================================================================
        LIST


StkSetPtrToOffset macro Ptr,Offset
  ; Set pointer Ptr to argument in stack frame at Offset.
          movlw     LOW(Offset)
          addwf     FSR1L,W,ACCESS
          movwf     Ptr,ACCESS
          movlw     HIGH(Offset&0xFFFF)
          addwfc    FSR1H,W,ACCESS
          movwf     Ptr+1
 endm


StkPushPtrToOffset macro Offset
  ; Push pointer onto stack to point to Offset location in stack.
  ; Offset defined before push.
          movlw     LOW(Offset)
          addwf     FSR1L,W,ACCESS
          movwf     INDF1,ACCESS
          movlw     HIGH(Offset&0xFFFF)
          addwfc    FSR1H,W,ACCESS
          movwf     PREINC1,ACCESS
          movf      POSTINC1,F,ACCESS     ; increment FSR1
 endm



StkAddStackPtr macro N
  ; Add N to FSR1
          movlw     LOW(N)
          addwf     FSR1L,F,ACCESS
          movlw     HIGH(N&0xFFFF)
          addwfc    FSR1H,F,ACCESS
 endm


Stk1PushFromFSR0 macro
  ; Get one byte from FSR0 and push on stack.  FSR0, WREG unchanged.
          movff     INDF0,POSTINC1
 endm

Stk2PushFromFSR0 macro
  ; Get two bytes from FSR0 and push on stack. FSR0, WREG unchanged.
          movff     POSTINC0,POSTINC1
          movff     POSTDEC0,POSTINC1
 endm

Stk3PushFromFSR0 macro
  ; Get three bytes from FSR0 and push on stack. FSR0, WREG unchanged.
          movff     POSTINC0,POSTINC1
          movff     POSTINC0,POSTINC1
          movff     POSTDEC0,POSTINC1
          movf      POSTDEC0,F,ACCESS
 endm



Stk1PushFromFSR2 macro
  ; Get one byte from FSR2 and push on stack.  FSR2, WREG unchanged.
          movff     INDF2,POSTINC1
 endm

Stk2PushFromFSR2 macro
  ; Get two bytes from FSR2 and push on stack. FSR2, WREG unchanged.
          movff     POSTINC2,POSTINC1
          movff     POSTDEC2,POSTINC1
 endm

Stk3PushFromFSR2 macro
  ; Get three bytes from FSR2 and push on stack. FSR2, WREG unchanged.
          movff     POSTINC2,POSTINC1
          movff     POSTINC2,POSTINC1
          movff     POSTDEC2,POSTINC1
          movf      POSTDEC2,F,ACCESS
 endm


Stk1PushFromReg macro Reg
  ; Get one byte from Reg and push on stack.  WREG unchanged.
          movff     Reg,POSTINC1
 endm

Stk2PushFromReg macro Reg
  ; Get two bytes from Reg and push on stack.  WREG unchanged.
          movff     Reg,POSTINC1
          movff     Reg+1,POSTINC1
 endm


Stk3PushFromReg macro Reg
  ; Get three bytes from Reg and push on stack.  WREG unchanged.
          movff     Reg,POSTINC1
          movff     Reg+1,POSTINC1
          movff     Reg+2,POSTINC1
 endm


Stk1PopToFSR0 macro
  ; Pop one byte from stack and put it at FSR0 without exposing stack.
  ; WREG changed, FSR0 unchanged.
          movlw     0xFF
          movff     PLUSW1,INDF0
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm


Stk2PopToFSR0 macro
  ; Pop two bytes from stack and put them at FSR0 without exposing stack.
  ; WREG changed, FSR0 unchanged.
          movlw     0xFE
          movff     PLUSW1,POSTINC0
          movlw     0xFF
          movff     PLUSW1,POSTDEC0
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm

Stk3PopToFSR0 macro
  ; Pop three bytes from stack and put them at FSR0 without exposing stack.
  ; WREG changed, FSR0 unchanged.
          movlw     0xFD
          movff     PLUSW1,POSTINC0
          movlw     0xFE
          movff     PLUSW1,POSTINC0
          movlw     0xFF
          movff     PLUSW1,POSTDEC0
          movf      POSTDEC0,F,ACCESS     ; restore FSR0
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm


Stk1PopToFSR2 macro
  ; Pop one byte from stack and put it at FSR2 without exposing stack.
  ; WREG changed, FSR2 unchanged.
          movlw     0xFF
          movff     PLUSW1,INDF2
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm


Stk2PopToFSR2 macro
  ; Pop two bytes from stack and put them at FSR2 without exposing stack.
  ; WREG changed, FSR2 unchanged.
          movlw     0xFE
          movff     PLUSW1,POSTINC2
          movlw     0xFF
          movff     PLUSW1,POSTDEC2
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm

Stk3PopToFSR2 macro
  ; Pop three bytes from stack and put them at FSR2 without exposing stack.
  ; WREG changed, FSR2 unchanged.
          movlw     0xFD
          movff     PLUSW1,POSTINC2
          movlw     0xFE
          movff     PLUSW1,POSTINC2
          movlw     0xFF
          movff     PLUSW1,POSTDEC2
          movf      POSTDEC2,F,ACCESS     ; restore FSR2
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm


Stk1PopToReg macro Reg
  ; Pop one byte from stack and put it at Reg without exposing stack.
  ; WREG changed.
          movlw     0xFF
          movff     PLUSW1,Ptr
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm



Stk2PopToReg macro Reg
  ; Pop two bytes from stack and put them at Reg without exposing stack.
  ; WREG changed.
          movlw     0xFF
          movff     PLUSW1,Reg+1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movlw     0xFF
          movff     PLUSW1,Reg
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm

Stk3PopToReg macro Reg
  ; Pop three bytes from stack and put them at Reg without exposing stack.
  ; WREG changed.
          movlw     0xFF
          movff     PLUSW1,Reg+2
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movlw     0xFF
          movff     PLUSW1,Reg+1
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
          movlw     0xFF
          movff     PLUSW1,Reg
          movf      POSTDEC1,F,ACCESS     ; decrement FSR1
 endm



Stk1CpyToFSR0 macro Offset
  ; Copy one byte from stack frame at Offset to FSR0
  ; WREG changed, FSR0 unchanged.
          movlw     LOW(Offset)
          movff     PLUSW1,INDF0
 endm


Stk2CpyToFSR0 macro Offset
  ; Copy two bytes from stack frame at Offset to FSR0
  ; WREG changed, FSR0 unchanged.
          movlw     LOW(Offset)
          movff     PLUSW1,POSTINC0
          movlw     LOW(Offset+1)
          movff     PLUSW1,POSTDEC0
 endm


Stk1CpyToFSR2 macro Offset
  ; Copy one byte from stack frame at Offset to FSR2
  ; WREG changed, FSR2 unchanged.
          movlw     LOW(Offset)
          movff     PLUSW1,INDF2
 endm


Stk2CpyToFSR2 macro Offset
  ; Copy two bytes from stack frame at Offset to FSR2
  ; WREG changed, FSR2 unchanged.
          movlw     LOW(Offset)
          movff     PLUSW1,POSTINC2
          movlw     LOW(Offset+1)
          movff     PLUSW1,POSTDEC2
 endm

Stk4CpyToFSR2 macro Offset
  ; Copy four bytes from stack frame at Offset to FSR2
  ; WREG changed, FSR2 unchanged.
          movlw     LOW(Offset)
          movff     PLUSW1,POSTINC2
          movlw     LOW(Offset+1)
          movff     PLUSW1,POSTINC2
          movlw     LOW(Offset+2)
          movff     PLUSW1,POSTINC2
          movlw     LOW(Offset+3)
          movff     PLUSW1,POSTDEC2
          movf      POSTDEC2,F,ACCESS
          movf      POSTDEC2,F,ACCESS
 endm


Stk1CpyToReg macro Offset,Reg
  ; Copy one byte from stack frame at Offset to Reg
          movlw     LOW(Offset)
          movff     PLUSW1,Reg
 endm


Stk2CpyToReg macro Offset,Reg
  ; Copy two bytes from stack frame at Offset to Reg
          movlw     LOW(Offset)
          movff     PLUSW1,Reg
          movlw     LOW(Offset+1)
          movff     PLUSW1,Reg+1
 endm


Stk3CpyToReg macro Offset,Reg
  ; Copy three bytes from stack frame at Offset to Reg
          movlw     LOW(Offset)
          movff     PLUSW1,Reg
          movlw     LOW(Offset+1)
          movff     PLUSW1,Reg+1
          movlw     LOW(Offset+2)
          movff     PLUSW1,Reg+2
 endm



Stk1CpyFromFSR0 macro Offset
  ; Copy one byte from FSR0 to stack frame at Offset
          movlw     LOW(Offset)
          movff     INDF0,PLUSW1
 endm


Stk2CpyFromFSR0 macro Offset
  ; Copy two bytes from FSR0 to stack frame at Offset
          movlw     Offset
          movff     POSTINC0,PLUSW1
          movlw     Offset+1
          movff     POSTDEC0,PLUSW1
 endm


Stk3CpyFromFSR0 macro Offset
  ; Copy three bytes from FSR0 to stack frame at Offset
          movlw     Offset
          movff     POSTINC0,PLUSW1
          movlw     Offset+1
          movff     POSTDEC0,PLUSW1
          movlw     Offset+2
          movff     POSTDEC0,PLUSW1
 endm


Stk4CpyFromFSR0 macro Offset
  ; Copy four bytes from FSR0 to stack frame at Offset
          movlw     Offset
          movff     POSTINC0,PLUSW1
          movlw     Offset+1
          movff     POSTINC0,PLUSW1
          movlw     Offset+2
          movff     POSTINC0,PLUSW1
          movlw     Offset+3
          movff     POSTDEC0,PLUSW1
          movf      POSTDEC0,F,ACCESS
          movf      POSTDEC0,F,ACCESS
 endm



Stk1CpyFromFSR2 macro Offset
  ; Copy one byte from FSR2 to stack frame at Offset
          movlw     LOW(Offset)
          movff     INDF2,PLUSW1
 endm


Stk2CpyFromFSR2 macro Offset
  ; Copy two bytes from FSR2 to stack frame at Offset
          movlw     Offset
          movff     POSTINC2,PLUSW1
          movlw     Offset+1
          movff     POSTDEC2,PLUSW1
 endm


Stk4CpyFromFSR2 macro Offset
  ; Copy four bytes from FSR2 to stack frame at Offset
          movlw     Offset
          movff     POSTINC2,PLUSW1
          movlw     Offset+1
          movff     POSTINC2,PLUSW1
          movlw     Offset+2
          movff     POSTINC2,PLUSW1
          movlw     Offset+3
          movff     POSTDEC2,PLUSW1
          movf      POSTDEC2,F,ACCESS
          movf      POSTDEC2,F,ACCESS
 endm




Stk1CpyFromReg macro Reg,Offset
  ; Copy one byte from Reg to stack frame at Offset
          movlw     LOW(Offset)
          movff     Reg,PLUSW1
 endm


Stk2CpyFromReg macro Reg,Offset
  ; Copy two bytes from Reg to stack frame at Offset
          movlw     Offset
          movff     Reg,PLUSW1
          movlw     Offset+1
          movff     Reg+1,PLUSW1
 endm


Stk3CpyFromReg macro Reg,Offset
  ; Copy three bytes from Reg to stack frame at Offset
          movlw     Offset
          movff     Reg,PLUSW1
          movlw     Offset+1
          movff     Reg+1,PLUSW1
          movlw     Offset+2
          movff     Reg+2,PLUSW1
 endm




Stk1Inc macro Offset
  ; Increment byte in stack at Offset
          movlw     Offset
          incf      PLUSW1,F,ACCESS
 endm


Stk2Inc macro Offset
  ; Increment 2 bytes in stack at Offset
          movlw     Offset
          incf      PLUSW1,F,ACCESS
          movlw     Offset+1
        btfsc     STATUS,Z
          incf      PLUSW1,F,ACCESS
 endm


Stk3Inc macro Offset
  ; Increment 3 bytes in stack at Offset
          local jEnd
          movlw     Offset
          incf      PLUSW1,F,ACCESS
          bnz       jEnd
          movlw     Offset+1
          incf      PLUSW1,F,ACCESS
          bnz       jEnd
          movlw     Offset+2
          incf      PLUSW1,F,ACCESS
jEnd:
 endm



Stk1Dec macro Offset
  ; Decrement byte in stack at Offset
          movlw     Offset
          decf      PLUSW1,F,ACCESS
 endm


Stk2Dec macro Offset
  ; Decrement 2 bytes in stack at Offset
          local jEnd
          movlw     Offset
          decf      PLUSW1,F,ACCESS
          bc        jEnd
          movlw     Offset+1
          decf      PLUSW1,F,ACCESS
jEnd:
 endm

Stk3Dec macro Offset
  ; Decrement 3 bytes in stack at Offset
          local jEnd
          movlw     Offset
          decf      PLUSW1,F,ACCESS
          bc        jEnd
          movlw     Offset+1
          decf      PLUSW1,F,ACCESS
          bc        jEnd
          movlw     Offset+2
          decf      PLUSW1,F,ACCESS
jEnd:
 endm


Stk1TestForZero macro Offset,jZero
  ; Test 1 byte at Offset.  Branch if zero.  Returns 0 in WREG if zero.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          bz        jZero
 endm


Stk2TestForZero macro Offset,jZero
  ; Test 2 bytes at Offset.  Branch if zero.  Returns 0 in WREG if zero.
          local     jContinue
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          bnz       jContinue
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          bz        jZero
jContinue
 endm


Stk3TestForZero macro Offset,jZero
  ; Test 3 bytes at Offset.  Branch if zero.  Returns 0 in WREG if zero.
          local     jContinue
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          bnz       jContinue
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          bnz       jContinue
          movlw     Offset+2
          movf      PLUSW1,W,ACCESS
          bz        jZero
jContinue
 endm

Stk1AddToFSR0 macro Offset
  ; Add 1 byte in stack at Offset to byte at FSR0. Pointer FSR0 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     INDF0,F,ACCESS
 endm


Stk2AddToFSR0 macro Offset
  ; Add 2 bytes in stack at Offset to 2 bytes at FSR0.  Pointer FSR0 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC0,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC0,F,ACCESS
 endm


Stk3AddToFSR0 macro Offset
  ; Add 3 bytes in stack at Offset to 3 bytes at FSR0.  Pointer FSR0 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC0,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC0,F,ACCESS
          movlw     Offset+2
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC0,F,ACCESS
          movf      POSTDEC0,F,ACCESS
 endm


Stk4AddToFSR0 macro Offset
  ; Add 4 bytes in stack at Offset to 4 bytes at FSR0. Pointer FSR0 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC0,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC0,F,ACCESS
          movlw     Offset+2
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC0,F,ACCESS
          movlw     Offset+3
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC0,F,ACCESS
          movf      POSTDEC0,F,ACCESS
          movf      POSTDEC0,F,ACCESS
 endm


Stk1AddToFSR2 macro Offset
  ; Add 1 byte in stack at Offset to byte at FSR2. Pointer FSR2 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     INDF2,F,ACCESS
 endm


Stk2AddToFSR2 macro Offset
  ; Add 2 bytes in stack at Offset to 2 bytes at FSR2.  Pointer FSR2 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC2,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC2,F,ACCESS
 endm


Stk3AddToFSR2 macro Offset
  ; Add 3 bytes in stack at Offset to 3 bytes at FSR2.  Pointer FSR2 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC2,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC2,F,ACCESS
          movlw     Offset+2
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC2,F,ACCESS
          movf      POSTDEC2,F,ACCESS
 endm


Stk4AddToFSR2 macro Offset
  ; Add 4 bytes in stack at Offset to 4 bytes at FSR2. Pointer FSR2 unchanged.
          movlw     Offset
          movf      PLUSW1,W,ACCESS
          addwf     POSTINC2,F,ACCESS
          movlw     Offset+1
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC2,F,ACCESS
          movlw     Offset+2
          movf      PLUSW1,W,ACCESS
          addwfc    POSTINC2,F,ACCESS
          movlw     Offset+3
          movf      PLUSW1,W,ACCESS
          addwfc    POSTDEC2,F,ACCESS
          movf      POSTDEC2,F,ACCESS
          movf      POSTDEC2,F,ACCESS
 endm




Stk1ShiftLeft macro Offset
  ; Shift left 1 byte in stack at Offset
          bcf       STATUS,C
          movlw     Offset
          rlcf      PLUSW1,F,ACCESS
 endm


Stk2ShiftLeft macro Offset
  ; Shift left 2 bytes in stack at Offset
          bcf       STATUS,C
          movlw     Offset
          rlcf      PLUSW1,F,ACCESS
          movlw     Offset+1
          rlcf      PLUSW1,F,ACCESS
 endm


Stk4ShiftLeft macro Offset
  ; Shift left 4 bytes in stack at Offset
          bcf       STATUS,C
          movlw     Offset
          rlcf      PLUSW1,F,ACCESS
          movlw     Offset+1
          rlcf      PLUSW1,F,ACCESS
          movlw     Offset+2
          rlcf      PLUSW1,F,ACCESS
          movlw     Offset+3
          rlcf      PLUSW1,F,ACCESS
 endm


Stk1Negate macro Offset
  ; Negate 1 byte in stack at Offset
          movlw     Offset
          comf      PLUSW1,F,ACCESS
          incf      PLUSW1,F,ACCESS
 endm


Stk2Negate macro Offset
  ; Negate 2 bytesin stack at Offset
          movlw     Offset
          comf      PLUSW1,F,ACCESS
          incf      PLUSW1,F,ACCESS
          movlw     Offset+1
          btfsc     STATUS,Z
          decf      PLUSW1,F,ACCESS
          comf      PLUSW1,F,ACCESS
 endm


Stk4Negate macro Offset
  ; Negate 4 bytes in stack at Offset
          local jEnd
          movlw     Offset
          comf      PLUSW1,F,ACCESS
          movlw     Offset+1
          comf      PLUSW1,F,ACCESS
          movlw     Offset+2
          comf      PLUSW1,F,ACCESS
          movlw     Offset+3
          comf      PLUSW1,F,ACCESS

          movlw     Offset
          incf      PLUSW1,F,ACCESS
          bnz       jEnd
          movlw     Offset+1
          incf      PLUSW1,F,ACCESS
          bnz       jEnd
          movlw     Offset+2
          incf      PLUSW1,F,ACCESS
          bnz       jEnd
          movlw     Offset+3
          incf      PLUSW1,F,ACCESS
jEnd:
 endm

Stk1NegateFSR2 macro
  ; Negate 1 byte at FSR2.  FSR2 unchanged.
          comf      INDF2,F,ACCESS
          incf      INDF2,F,ACCESS
 endm


Stk2NegateFSR2 macro
  ; Negate 2 bytes at FSR2.  FSR2 unchanged.
          comf      INDF2,F,ACCESS
          incf      POSTINC2,F,ACCESS
          btfsc     STATUS,Z
          decf      INDF2,F,ACCESS
          comf      POSTDEC2,F,ACCESS
 endm


;====== These branches are short ==============

jmpWeqZ macro   Label        ; jmp if W == 0
        movf      WREG,F,ACCESS
        bz        Label
          endm

jmpWneZ macro   Label        ; jmp if W != 0
        tstfsz    WREG,ACCESS
        bra       Label
          endm


jmpFeqZ macro   Reg,Acc,Label
        movf      Reg,F,Acc
        bz        Label
        endm

jmpFneZ macro   Reg,Acc,Label
        movf      Reg,F,Acc
        bnz       Label
        endm

jmpFeqL macro   Reg,Acc,bVal,Label
          movlw     bVal
          subwf     Reg,W,Acc
        bz        Label
        endm

jmpFneL macro   Reg,Acc,bVal,Label
          movlw     bVal
          subwf     Reg,W,Acc
        bnz       Label
        endm

jmpFleL macro   Reg,Acc,bVal,Label
          movlw     bVal
          cpfsgt    Reg,Acc
        bra       Label
        endm

jmpFgeL macro   Reg,Acc,bVal,Label
          movlw     bVal
          cpfslt    Reg,Acc
        bra       Label
        endm


jmpFeqF macro   Reg1,Acc1,Reg2,Acc2,Label
          movf      Reg1,W,Acc1
          subwf     Reg2,W,Acc2
        bz        Label
        endm

jmpFneF macro   Reg1,Acc1,Reg2,Acc2,Label
          movf      Reg1,W,Acc1
          subwf     Reg2,W,Acc2
        bnz       Label
        endm

jmpClr  macro Reg,Bit,Acc,Label
        btfss   Reg,Bit,Acc
        bra       Label
        endm

jmpSet  macro Reg,Bit,Acc,Label
        btfsc   Reg,Bit,Acc
        bra       Label
        endm


