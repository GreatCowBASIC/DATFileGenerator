

'This examines the INDEX_IDX file in the MPLAB-X directory and returns the version etc for a specific Chip


  #include once "ext/xml/dom.bi"
  #include "file.bi"

  #DEFINE kINDEX_IDX  ""    'UserProfile+kINDEX.IDX
  #DEFINE kIDEVersion "6.05"
  #DEFINE kUniqueBits ""
  #DEFINE kKillRegister "RTC_CMP,SREG,CPU_SPL, CPU_SPH "
  #DEFINE kKillBits "CPU_RAMPZ"
  #DEFINE kXLScs "avr chipdata.csv"

    'These are the columns in the XLS
    #Define XLSchip       0
    #Define XLSNotTested  1
    #Define XLSKbytes	    2
    #Define XLSEEPROM	    3
    #Define XLSRAM	      4
    #Define XLSIO	        5
    #Define XLSADCChannels  6	
    #Define XLSMaxSpeedMHz	7
    #Define XLSADC	        8
    #Define XLSGPR	        9
    #Define XLSPINS	        10
    #Define XLSUSART	      11
    #Define XLSChipFamilyOverride 12	
    #Define XLSChipSubFamily	    13
    #Define XLSAVRFamily	        14
    #Define XLSAVRGCC	            15
    #Define XLSAVRDX	            16
    #Define XLSAVRAlias	          17
    #Define XLSCompiler	          18
    #Define XLSPackages           19
    #Define XLSUSARTConfig        20
    #Define ADCMapType            21


  Type SFR
    RegisterName as String
    RegisterAddresss as String
    RegisterLength as String
    Count as Integer
  End Type

  Type SFRBits
    RegisterRaw as String
    RegisterBitName as String
    RegisterBitAddress as String
    RegisterBitRename as Integer
    RegisterName as string
    Count as Integer
  End Type

  Type ChipSourceFile
    ChipName As String
    FileName As String
    FileType As Integer '4 = AVR Studio 4, 6 = Atmel Studio 6
  End Type

  Type SourceFiles
    ChipName As String
    Core As String
    Family as String
    SourceFolder As String
    Version as string
    Pack as String
    UserProfile As String
  End Type

  Dim Shared SourceFileArray(100000) As SourceFiles
  Dim Shared SourceFileArrayPointer as Integer = 0
  Dim Shared SFRBitsArrayPointer as Integer = 0
  Dim Shared SFRDataCount as Integer = 0
  Dim Shared SFRData(3000) as SFR
  Dim Shared SFRBits(20000) as SFRBits
  Dim Shared SFRUniques() as string
  Dim Shared currentitem as string
  Dim Shared BadRam(100) as string
  Dim Shared BadRamPointer as Integer = 0
  Dim Shared ConfigFilename as string
  Dim Shared ShowDebug as Integer = 0
  Dim Shared UserProfile as String

  Dim Shared ParamUpper as string
  Dim Shared targetchip as string
  Dim Shared As String chip, chipdetails, chiphsource, chipincsource
  Dim Shared chipparameters() as string
  Dim Shared fsp_ini as string
  Dim Shared fsp_ini_h as string
  Dim Shared folder_DFP as string

  Dim Shared SFRRegister() as string
  Dim Shared DataSource as string
  Dim Shared RegistersToBeIgnored() as string
  Dim Shared as Integer RegistersToBeIgnoredCounter, FoundRegistersToBeIgnored
  Dim Shared CurrentBank as Integer
  Dim Shared ThisBank as Integer

  Dim Shared readNextLine as Integer = -1
  Dim Shared RegisterBits() as string
  Dim Shared BitsToBeIgnored() as string
  Dim Shared CurrentRegister as string: CurrentRegister = ""
  Dim Shared NewRegister as string: NewRegister = ""
  Dim Shared EqualPosition as Integer
  Dim Shared SemiColonPosition as Integer

  Dim Shared getRamstartStr as String
  Dim Shared getRamStartInt as Integer

  Dim Shared getRamSizeStr as String
  Dim Shared getRamSizeInt as Integer

  Dim Shared ParamMeters() as String
  Declare Sub PopulateSourceFilesLocation
  Declare Function FindChip ( chiptofind as string, ignore as string ) as string
  Declare SUB Replace (DataVar As String, Find As String, Rep As String)
  Declare Sub Split(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)
  Declare Function RegisterString( chipstringtofind as string ) as string
  Declare Sub PopulateRAMDefinitions ( SourceFolder as string, Version as string, Chip as string )
  Declare Sub PopulateConfig ( SourceFolder as string, Version as string, Chip as string )
  

  Declare Sub InitAndGetFiles
  Declare Sub PrintHeader
  Declare Sub Printregisters
  Declare Sub PrintBits
  Declare Sub PrintInterrupts
  Declare Sub PrintAliases
  Declare Sub PrintPointers
  Declare Sub PrintChipData
  Declare Sub PrintFooter
  Declare Sub PrintFreeRam
  Declare Sub PrintAVRMasks
  Declare Sub PrintAVRChipSpecifics
  Declare Function GetValue (  searchString as String, errorhandler as Integer = -1 ) as String
  Declare Function GetCSVValue (  searchString as String, returnParameter as Byte ) As String

'*******************************
InitAndGetFiles


PrintHeader
PrintChipData: PrintAVRChipSpecifics  
PrintPointers
PrintInterrupts
PrintAliases
PrintAVRMasks
Printregisters
PrintBits
PrintFreeRam
PrintFooter


Sub PrintFreeRam
    
  Print ""
  Print "[FreeRAM]"
  Print "'This is the extent of the RAM.  Inclusive of start and end address"

  getRamstartStr = "&h" + GetValue ( "INT_SRAM START_ADDR") 
  getRamStartInt = Val (getRamstartStr)

  getRamSizeStr = "&h" + GetValue ( "INT_SRAM SIZE")
  getRamSizeInt = Val( getRamSizeStr)

  print hex(getRamStartInt)+":"+hex( getRamStartInt + getRamSizeInt )

End Sub

Sub PrintFooter
  Print ""
  Print "[ConfigOps]"
  Print "NoConfig-PRG"

  Print ""
  Print "[Config]"
  Print "NoConfig-PRG"
End Sub


Sub PrintChipData

  Dim cacheString as String
  Dim cacheArray() as String
  Dim cacheCount as Integer

  Print ""
  Print "[ChipData]"
  
    Print "';All items in the ChipData section are available to user programs as constants"
    Print "';The constants have the prefix of Chip: See the details below"
    Print
    Print "'This constant is exposed as ChipWORDS"
  Print "Prog="+Str(Val("&h"+GetValue ("MAPPED_PROGMEM_SIZE")) /2 )

    Print
    Print "'This constant is exposed as ChipEEPROM"
  Print "EEPROM="+Str(Val("&h"+GetValue ("EEPROM_SIZE")))
  
    Print
    Print "'This constant is exposed as ChipRAM"
  Print "RAM="+Str(Val("&h"+GetValue ("INT_SRAM SIZE")))

    Print
    Print "'This constant is exposed as ChipIO - sourced from `" + kXLScs +"`"
  Print "I/O=" + GetCSVValue ( targetchip, XLSIO )

    Print
    Print "'This constant is exposed as ChipADC - sourced from `" + kXLScs +"`"
  Print "ADC=" + GetCSVValue ( targetchip, XLSADCChannels )
  Print "ADCPPORTMAP=" + GetCSVValue ( targetchip, ADCMapType )

    Print
    Print "'This constant is exposed as ChipMhz"
  If Val("&h"+GetValue ("FUSE_FREQSEL_20MHZ_gc", 0 )) > 0 then
    Print "MaxMHz=20"
    Print  "'This constant is exposed with only the first parameter (if more than one)"
    Print "IntOsc=20, 10, 5, 2.5, 1.25, 0.625, 0.3125, 3.3333333, 2, 1.6666667, 0.8333333, 0.4166667"

  ElseIf Val("&h"+GetValue ("FUSE_FREQSEL_16MHZ_gc", 0 )) > 0 then
    Print "MaxMHz=16"
    Print  "'This constant is exposed with only the first parameter (if more than one)"
    Print "IntOsc=16"

  Else
      Print "This constant is exposed as ChipMaxMhz  - sourced from `" + kXLScs +"`"
      Print "MaxMHz="  + GetCSVValue ( targetchip, XLSMaxSpeedMHz )
      If Val(GetCSVValue ( targetchip, XLSMaxSpeedMHz )) = 24 then
        Print  "'This constant is exposed with only the first parameter (if more than one)"
        Print "IntOsc=24, 20, 16, 12, 8, 4, 3, 2, 1"
      End If
  End If

    Print
    Print "'This constant is exposed as ChipPins - sourced from `" + kXLScs +"`"
  Print "Pins=" + GetCSVValue ( targetchip, XLSPINS )

    Print
    Print "'This constant is exposed as ChipUSART - sourced from `" + kXLScs +"`"
  Print "USART=" + GetCSVValue ( targetchip, XLSUSART )

    Print
    Print "'These USART constants are exposed with the prefix of CHIP - sourced from `" + kXLScs +"`"
      cacheString = GetCSVValue ( targetchip, XLSUSARTConfig )
    Split( cacheString, "|",0, cacheArray())
    For cacheCount = 0 to ubound(cacheArray)
      If Trim(cacheArray(cacheCount)) <> "" Then Print cacheArray(cacheCount)
    Next
    
    Print
    Print "'This constant is exposed as ChipFamily - sourced from `" + kXLScs +"`"
  Print "Family="+ GetCSVValue ( targetchip, XLSChipFamilyOverride )
  
    Print
    Print "'This constant is exposed as ChipConfWords"
  Print "ConfigWords=0"

    Print
    Print "'This constant is exposed as ChipGPR"
  Print "GPR=32"

    getRamstartStr = "&h" + GetValue ( "INT_SRAM START_ADDR") 
    getRamStartInt = Val (getRamstartStr)

    getRamSizeStr = "&h" + GetValue ( "INT_SRAM SIZE")
    getRamSizeInt = Val( getRamSizeStr)

    Print
    Print "'This constant is exposed as ChipMaxAddress. This value is the maximum address of the internal SRAM.  SRAM is used for data storage and stack."
  Print "MaxAddress=" + Str( getRamStartInt + getRamSizeInt )

    Print
    Print "'This constant is exposed as ChipHardwareMult"
  Print "HardwareMult=y"
  if Val("&h"+GetValue ("VPORTA_OUT")) > 0 then
      Print
      Print "'This constant is exposed as ChipAVRFamily - sourced from `" + kXLScs +"`"
    Print "AVRFamily=" + GetCSVValue ( targetchip, XLSAVRFamily )
      Print
      Print "'This constant is exposed as ChipAVRGCC - sourced from `" + kXLScs +"`"
    Print "AVRGCC="+ GetCSVValue ( targetchip, XLSAVRGCC )
      Print
      Print "'This constant is exposed as ChipAVRDX - sourced from `" + kXLScs +"`"
    Print "AVRDX=" + GetCSVValue ( targetchip, XLSAVRDX )
  End If

  IF val(GetCSVValue ( targetchip, XLSNotTested ))> 0 then
      Print
      Print "'This constant is exposed as ChipNotTested - sourced from `" + kXLScs +"`"
      print "' NotTested is a numeric bitwise value"
      Print "' 1 = Chip DAT file not tested and therefore no validated"
      Print "' 2 = Chip DAT file has an [interrupt] section that is incomplete"
      print "NotTested="+str(val(GetCSVValue ( targetchip, XLSNotTested )))
  End If 
End Sub


Sub PrintPointers
        Print ""
        Print "[Pointers]"
          Print "'This section are the registers used by compilers"
        Print "XL:26"
        Print "XH:27"
        Print "YL:28"
        Print "YH:29"
        Print "ZL:30"
        Print "ZH:31"
End Sub

Sub PrintAliases

  print ""
  print "[AVRAlias]"
  print "  'This section is for the AVRDX support"
  print "  'The aliasing is megaAVR register = value = AVRDX register"
  print "  ' example:          ALIAS_PORTA_DIR = 0 = DDRA"
  print "  '"
  print " 'Format is Strict: "
  print "  ' There must be two entries per alias, the order is not importand, the relatiionship as shown above is critical."
  print "  ' The value is NOT a register address value. The value MUST be unique"
  print "  ' The assignment must be an equal sign `=`"
  print "   '"
  print " 'How does this all work?"
  print "  ' The compiler will look up a megaAVR register ( ignoring any register.bits), and, using the returned number, lookup the AVRDX alias."
  print "  ' DDRA = 0, lookup the AVRDX alias of 0, returns ALIAS_PORTA_DIR"
  print "  ' the compiler strips off the `ALIAS_`PORTA_DIR prefix as the register to be used."
  print "  '"
  print " 'A deeper dive."
  print "  ' The compiler treats this list of AVRAlias(es) just like the [register] section of this file."
  print "  ' The compiler loads these AVRAlias(es) but with the attribute of .ALIAS set (true)."
  print "  ' This means, using the example above, that DDRA is not a real register and it is has the .ALIAS attribute set. "
  print "  ' So, the compiler knows that DDRA for this specific chip and all AVRDX chips that DDRA is actually to be transformed to an AVRDX set of instructiions."
  print 

  if Val("&h"+GetValue ("PORTA_OUT", 0)) > 0 then
    print "'PortA"
    print "ALIAS_PORTA_DIR=0                                      ; 0000             "
    print "DDRA=0                                          ; 0000 alias"
    print "ALIAS_PORTA_OUT=1                                      ; 0001             "
    print "PORTA=1                                         ; 0001 alias         "
    print "ALIAS_PORTA_IN=2                                       ; 0002   "
    print "PINA=2                                          ; 0002 alias"
    print ""
  End if


  if Val("&h"+GetValue ("PORTB_OUT", 0)) > 0 then
    print "'PortB"
    print "ALIAS_PORTB_DIR=4                                      ; 0004            "
    print "DDRB=4                                          ; 0004 alias"
    print "ALIAS_PORTB_OUT=5                                      ; 0005              "
    print "PORTB=5                                         ; 0005 alias"
    print "ALIAS_PORTB_IN=6                                       ; 0006"
    print "PINB=6                                          ; 0006 alias"
    print ""
  End If

  if Val("&h"+GetValue ("PORTC_OUT", 0)) > 0 then
    print "'PortC"
    print "ALIAS_PORTC_DIR=8                                      ; 0008          "
    print "DDRC=8                                          ; 0008 alias"
    print "ALIAS_PORTC_OUT=9                                      ; 0009"
    print "PORTC=9                                         ; 0009 alias"
    print "ALIAS_PORTC_IN=10                                      ; 000A"
    print "PINC=10                                         ; 000A alias"
    print ""
  End if

  if Val("&h"+GetValue ("PORTD_OUT", 0)) > 0 then  
    print "'PortD"
    print "ALIAS_PORTD_DIR=11"
    print "DDRD=11"
    print "ALIAS_PORTD_OUT=12"
    print "PORTD=12"
    print "ALIAS_PORTD_IN=13"
    print "PIND=13"
    print ""
  end if

  if Val("&h"+GetValue ("PORTE_OUT", 0)) > 0 then
    print "'PortE"
    print "ALIAS_PORTE_DIR=14"
    print "DDRE=14"
    print "ALIAS_PORTE_OUT=15"
    print "PORTE=15"
    print "ALIAS_PORTE_IN=16"
    print "PINE=16"
    print ""
  end if

  if Val("&h"+GetValue ("PORTF_OUT", 0)) > 0 then
    print "'PortF"
    print "ALIAS_PORTF_DIR=17"
    print "DDRF=17"
    print "ALIAS_PORTF_OUT=18"
    print "PORTF=18"
    print "ALIAS_PORTF_IN=19"
    print "PINF=19"
    print ""
  end if

  print "'Additional Aliases are required"
  print "ALIAS_CPU_SPL=61    "
  print "CPU_SPL=61    "
  print "ALIAS_CPU_SPH=62                                                                                     "
  print "CPU_SPH=62 "

  print "'Additional lock register alias"
  print "ALIAS_CPU_CCP=52                                  ; 0034"
  print "CPU_CCP=52                                        ; 0034"

  print "'Required register"
  print "SREG=63                                           ; 003F alias"
  print "ALIAS_SREG=63"

End Sub

Sub PrintInterrupts

        Print ""
        Print "[Interrupts]"

          Print "'For specific details of the interrupts see the microcontroller datasheet"
          Print "'The first parameter is the GCBASIC identifier used in user code to expose the specific interrupt"
          Print "'"
          Print "' Specific to AVRDX chips - an explaination - the entries"
          print "' GCBASIC friendly name:Source Interrupt Name,vector,register.bit to enable the interrupt,register.bit or !register.bit to clear the interrupt"
          print "'   name     interrupt   #    enable reg.bit(s)                  clear reg.bit(s)"
          print "' NMIfromCRC:CRCSCAN_NMI,2,CRCSCAN_CTRLA.CRCSCAN_NMIEN_bp,!CRCSCAN_CTRLA.CRCSCAN_RESET_bp"
          print "'"
          print "' The enable reg.bit(s) can be a single bit or bits will be used to set the register"
          print "' The clear reg.bit(s) can be a single bit or bits will be used to set the register"
          print "' ! (eqautes to NOT ) inverts the bit"
          print "'"
          
        open fsp_ini for input as #1
        If Err>0 Then
          Print "Error opening the file "+chr(34)+fsp_ini_h+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End
        End if

        ' print "INTERRUPT VECTORS, ABSOLUTE ADDRESSES"

        do
            Line input #1, DataSource
        loop while not eof(1) and instr( ucase(DataSource),"INTERRUPT VECTORS, ABSOLUTE ADDRESSES") = 0



        Dim IntValue as Integer = 0
        Dim Interrupts() as string
        Dim As String vector, oldVector

        Do while not eof ( 1 )
            Do
                Line input #1, DataSource
                If Instr( DataSource, "INTERRUPT VECTORS, MODULE BASES") > 0 then goto fin1
                if eof ( 1 ) then goto fin1
                If Left( DataSource,1) = ";" then Print DataSource
            Loop While instr( DataSource, ".equ" ) = 0 or trim(DataSource) = ""

            If left( DataSource, 4) = ".equ" Then
              ' example line .equ CRCSCAN_NMI_vect = 0x0002           ; 
              ' to
              ' NMIfromCRC:CRCSCAN_NMI,2,,
              Replace ( DataSource, ".equ", "" )
              Replace ( DataSource, "_vect", "" )
              Replace ( DataSource, " ;", "" )
              Replace ( DataSource, "0x", "&h" )
              
              
              DataSource = Trim(DataSource)
              Vector = Trim(Mid( DataSource, Instr(DataSource, "=")+1 ))
              If val(Vector) <> val(oldVector) Then 
                'prevent duplicate                               
                oldVector = Vector

                Select Case Trim(Left( DataSource, Instr(DataSource, "=")-1))
                  Case "CCL_CCL":
                    print chr(9);"CCL_CCL:CCL_CCL,"+str(val(Vector))+",CCL_INTCTRL0.CCL_INTMODE0_gp,!CCL_INTFLAGS.CCL_INT_gp"
                  Case "CRCSCAN_NMI":
                    print chr(9);"NMIfromCRC:CRCSCAN_NMI,"+str(val(Vector))+",CRCSCAN_CTRLA.CRCSCAN_NMIEN_bp,!CRCSCAN_CTRLA.CRCSCAN_RESET_bp"
                  Case "BOD_VLM":
                    print chr(9);"BOD_VLM:BOD_VLM,"+ str(val(Vector)) + ",BOD_INTCTRL.BOD_VLMIE_bp,!BOD_INTFLAGS.BOD_VLMIF_bp"
                  Case "NVMCTRL_EE"
                    print chr(9);";EEREADY Interrupt Flag.  This flag is set continuously as long as the EEPROM is not busy. This flag is cleared by writing a '1' to it."
                    print chr(9);"NVMCTRLReady:NVMCTRL_EE,"+ str(val(Vector))+",NVMCTRL_INTCTRL.NVMCTRL_EEREADY_bp ,NVMCTRL_INTFLAGS.NVMCTRL_EEREADY_bp"
                  Case "RTC_CNT"
                    print chr(9);"RTCOverflow:RTC_CNT,"+ str(val(Vector))+",RTC_INTCTRL.RTC_OVF_bp,RTC_INTFLAGS.RTC_OVF_bp"
                  Case "PORTA_PORT"
                    print chr(9);"PortAChange:PORTA_PORT,"+ str(val(Vector))+", PORTA_PORTCTRL.PORT_INT_gm,PORTA_INTFLAGS.PORT_INT_gm"
                  Case "PORTB_PORT"
                    print chr(9);"PORTBChange:PORTB_PORT,"+ str(val(Vector))+",PORTB_PORTCTRL.PORT_INT_gm,PORTB_INTFLAGS.PORT_INT_gm"
                  Case "PORTC_PORT"
                    print chr(9);"PORTCChange:PORTC_PORT,"+ str(val(Vector))+",PORTC_PORTCTRL.PORT_INT_gm,PORTC_INTFLAGS.PORT_INT_gm"
                  Case "PORTD_PORT"
                    print chr(9);"PORTDChange:PORTD_PORT,"+ str(val(Vector))+",PORTD_PORTCTRL.PORT_INT_gm,PORTD_INTFLAGS.PORT_INT_gm"
                  Case "PORTE_PORT"
                    print chr(9);"PORTEChange:PORTE_PORT,"+ str(val(Vector))+",PORTE_PORTCTRL.PORT_INT_gm,PORTE_INTFLAGS.PORT_INT_gm"
                  Case "PORTF_PORT"
                    print chr(9);"PORTFChange:PORTF_PORT,"+ str(val(Vector))+",PORTF_PORTCTRL.PORT_INT_gm,PORTF_INTFLAGS.PORT_INT_gm"
                  Case "PORTG_PORT"
                    print chr(9);"PORTGChange:PORTG_PORT,"+ str(val(Vector))+",PORTG_PORTCTRL.PORT_INT_gm,PORTG_INTFLAGS.PORT_INT_gm"
                  Case "ADC0_READY"
                    print chr(9);"ADCReady:ADC0_READY,"+ str(val(Vector))+ ",ADC0_INTCTRL.ADC_RESRDY_bp,ADC0_INTFLAGS.ADC_RESRDY_bp"
                  Case "USART0_RXC"
                    print chr(9);"Usart0RXReady:USART0_RXC,"+ str(val(Vector))+", USART0_CTRLA.USART_RXCIE_bp, USART0_STATUS.USART_RXCIF_bp"
                  Case "USART1_RXC"
                    print chr(9);"Usart1RXReady:USART1_RXC,"+ str(val(Vector))+",USART1_CTRLA.USART_RXCIE_bp,USART1_STATUS.USART_RXCIF_bp"
                  Case "USART2_RXC"
                    print chr(9);"Usart2RXReady:USART2_RXC,"+ str(val(Vector))+",USART2_CTRLA.USART_RXCIE_bp,USART2_STATUS.USART_RXCIF_bp"
                  Case "USART3_RXC"
                    print chr(9);"Usart3RXReady:USART3_RXC,"+ str(val(Vector))+",USART3_CTRLA.USART_RXCIE_bp,USART3_STATUS.USART_RXCIF_bp"
                  Case "TCA0_LUNF"
                    print chr(9);"Timer0Overflow:TCA0_LUNF,"+ str(val(Vector))+",TCA0_SINGLE_INTCTRL.TCA_SINGLE_OVF_bp,!TCA0_SINGLE_INTFLAGS.TCA_SINGLE_OVF_bp"
                  Case "TCA0_HUNF"
                    print chr(9);"Timer0Underflow:TCA0_HUNF,"+ str(val(Vector))+",TCA_SPLIT_HUNF_bp,!TCA0_SPLIT_INTFLAGS.TCA_SPLIT_HUNF_bp "                    
                  Case "TCA0_CMP0"
                    print chr(9);"Timer0Match0:TCA0_CMP0,"+ str(val(Vector))+",TCA0_SINGLE_INTCTRL.TCA_SINGLE_CMP0_bp,TCA0_SINGLE_INTFLAGS.TCA_SINGLE_CMP0_bp"
                  Case "TCA0_CMP1"
                    print chr(9);"Timer0Match1:TCA0_CMP1,"+ str(val(Vector))+",TCA0_SINGLE_INTCTRL.TCA_SINGLE_CMP1_bp,TCA0_SINGLE_INTFLAGS.TCA_SINGLE_CMP1_bp"
                  Case "TCA0_CMP2"
                    print chr(9);"Timer0Match2:TCA0_CMP2,"+ str(val(Vector))+",TCA0_SINGLE_INTCTRL.TCA_SINGLE_CMP2_bp,TCA0_SINGLE_INTFLAGS.TCA_SINGLE_CMP0_bp"

                  Case "TCB0_INT"
                    print chr(9);"Timer0Capture:TCB0_INT,"+ str(val(Vector))+",TCB0_INTCTRL.TCB_CAPT_bp,TCB0_INTFLAGS.TCB_CAPT_bp"
                  Case "TCB1_INT"
                    print chr(9);"Timer1Capture:TCB1_INT,"+ str(val(Vector))+",TCB1_INTCTRL.TCB_CAPT_bp,TCB1_INTFLAGS.TCB_CAPT_bp"
                  Case "TCB2_INT"
                    print chr(9);"Timer2Capture:TCB2_INT,"+ str(val(Vector))+",TCB2_INTCTRL.TCB_CAPT_bp,TCB2_INTFLAGS.TCB_CAPT_bp"
                  Case "TCB3_INT"
                    print chr(9);"Timer3Capture:TCB3_INT,"+ str(val(Vector))+",TCB3_INTCTRL.TCB_CAPT_bp,TCB3_INTFLAGS.TCB_CAPT_bp"
                  Case "USART0_DRE"
                    print chr(9);"UsartTX0Ready:USART0_DRE,"+ str(val(Vector))+",USART0_CTRLA.USART_DREIE_bp,USART0_STATUS.USART_DREIF_bp"
                  Case "USART0_TXC"
                    print chr(9);"UsartTX0Sent:USART0_TXC,"+ str(val(Vector))+",USART0_CTRLA.USART_TXCIE_bp,USART0_STATUS.USART_TXCIF_bp"
                  Case "USART1_DRE"
                    print chr(9);"UsartTX1Ready:USART1_DRE,"+ str(val(Vector))+",USART1_CTRLA.USART_DREIE_bp,USART1_STATUS.USART_DREIF_bp"
                  Case "USART1_TXC"
                    print chr(9);"UsartTX1Sent:USART1_TXC,"+ str(val(Vector))+",USART1_CTRLA.USART_TXCIE_bp,USART1_STATUS.USART_TXCIF_bp"
                  Case "USART2_DRE"
                    print chr(9);"UsartTX2Ready:USART2_DRE,"+ str(val(Vector))+",USART2_CTRLA.USART_DREIE_bp,USART2_STATUS.USART_DREIF_bp"
                  Case "USART2_TXC"
                    print chr(9);"UsartTX2Sent:USART2_TXC,"+ str(val(Vector))+",USART2_CTRLA.USART_TXCIE_bp,USART2_STATUS.USART_TXCIF_bp"
                  Case "USART3_DRE"
                    print chr(9);"UsartTX3Ready:USART3_DRE,"+ str(val(Vector))+",USART3_CTRLA.USART_DREIE_bp,USART3_STATUS.USART_DREIF_bp"
                  Case "USART3_TXC"
                    print chr(9);"UsartTX3Sent:USART3_TXC,"+ str(val(Vector))+",USART3_CTRLA.USART_TXCIE_bp,USART3_STATUS.USART_TXCIF_bp"
                  Case "TWI0_TWIS"
                    print chr(9);"TWISlaveReady:TWI0_TWIS,"+ str(val(Vector))+",TWI0_SCTRLA.TWI_DIEN_bp ,TWI0_SSTATUS.TWI_DIF_bp"
                  Case "TWI0_TWIM"
                    print chr(9);"TWIMasterReady:TWI0_TWIM,"+ str(val(Vector))+",TWI0_MCTRLA.TWI_RIEN_bp,TWI0_MSTATUS.TWI_RIF_bp"
                  Case "SPI0_INT"
                    print chr(9);"SPIReady:SPI0_INT,"+ str(val(Vector))+",SPI0_INTCTRL.SPI_IE_bp,SPI0_INTFLAGS.SPI_IF_bp"
                  Case "RTC_CNT"
                    print chr(9);"RTCOverflow:RTC_CNT,"+ str(val(Vector))+",RTC_INTCTRL.RTC_OVF_bp,RTC_INTFLAGS.RTC_OVF_bp"
                  Case "RTC_PIT"
                          print chr(9);"PITOverflow:RTC_PIT,"+ str(val(Vector))+",RTC_PITINTCTRL.RTC_PI_bp,RTC_PITINTFLAGS.RTC_PI_bp"
                  Case "AC0_AC"
                    print chr(9);"AC0Compare:AC0_AC,"+ str(val(Vector))+",AC0_INTCTRL.AC_CMP_bp,AC0_STATUS.AC_CMP_bp "
                  Case "ADC0_WCOMP"
                    print chr(9);"ADC0_WindowCompare:ADC0_WCOMP,"+ str(val(Vector))+",ADC0_INTCTRL.ADC_WCMP_bp,ADC0_INTFLAGS.ADC_WCMP_bp"
                  Case "ADC0_WCMP"
                    print chr(9);"ADC0_WindowCompare:ADC0_WCMP,"+ str(val(Vector))+",ADC0_INTCTRL.ADC_WCMP_bp,ADC0_INTFLAGS.ADC_WCMP_bp"                  
                Case Else
                  print chr(9)+chr(9)+Trim(Left( DataSource, Instr(DataSource, "=")-1)) + ":" + Trim(Left( DataSource, Instr(DataSource, "=")-1)) + "," +  str(val(Vector)) + ",,"

                End Select
              End If
            Else  
              if left(DataSource,1) = ";" Then Print DataSource
            End If


        Loop


  fin1:
      Close
End Sub








Function RegisterString( chipaddresstofind as string ) as string

  Dim loopcounter as Integer
  for loopcounter = 0 to 3000
      if SFRData( loopcounter ).RegisterAddresss = chipaddresstofind   then
          return SFRData( loopcounter ).RegisterName
      end if
  next




End function





Function FindChip ( chiptofind as string, ignore as string ) as string
    Dim PotentialMatches( 100 ) as Integer
    Dim PotentialMatch as Integer = 0
    Dim loopcounter as Integer
    chiptofind = trim(ucase( chiptofind ))


    for loopcounter = 0 to SourceFileArrayPointer - 1
      'print SourceFileArray( loopcounter ).ChipName, chiptofind,SourceFileArray( loopcounter ).ChipName= chiptofind
      with SourceFileArray( loopcounter )
          if  ucase(trim( .ChipName )) = chiptofind then
              PotentialMatches( PotentialMatch ) = loopcounter
              PotentialMatch= PotentialMatch + 1
          end if
      end with
    next

    dim lastestversion as string = ""
    dim arraypointer as Integer = -1
    if PotentialMatch > 0 then
      for loopcounter = 0 to PotentialMatch - 1
        If SourceFileArray( PotentialMatches ( loopcounter ) ).Version = ignore Then
          SourceFileArray( PotentialMatches ( loopcounter ) ).Version = ""
        End If
        if SourceFileArray( PotentialMatches ( loopcounter ) ).Version > lastestversion then
          if SourceFileArray( PotentialMatches ( loopcounter ) ).Version <> ignore then
              lastestversion = SourceFileArray( PotentialMatches ( loopcounter ) ).Version
              arraypointer = PotentialMatches ( loopcounter )
          end if
        end if
      next
    end if

    if arraypointer <> -1 then
      with SourceFileArray( arraypointer )
        return   chiptofind+","+.Version + "," + .Core + "," + .Family +","+.Pack
      end with
    else
      If chiptofind <> "" then
        return "NOCHIP - " + chiptofind
      else
        return ""
      End if
    end if

  
End Function


Sub PopulateSourceFilesLocation

  Dim As ext.xml.node Ptr xmlroot
  Dim As ext.xml.node Ptr idx, pdsc, releases, release, devices
  Dim releaseversions() as string
  Dim loopcounter as Integer
  dim latestversion as string = ""

  Var xmldoc = new ext.xml.tree


  xmldoc->load( UserProfile + kINDEX_IDX+"\index.idx" )


  If xmldoc = 0 Then
    Print "XML file seems empty"
    print UserProfile + kINDEX_IDX+"\index.idx"
    GoTo FinishRead
  End If
  xmlroot = xmldoc->root
    'Pprint xmlroot->Children()
  If xmlroot = 0 Then
    Print "Missing root node"
    print UserProfile + kINDEX_IDX+"\index.idx"
    GoTo FinishRead
  End If

  'Get idx
  idx = xmlroot->Child("idx")
  
  If idx = 0 Then
    Print "Missing idx node in "+ kINDEX_IDX+"\index.idx" + "  XML source"
    print UserProfile + kINDEX_IDX+"\index.idx"
  else

    'Get pdsc
    pdsc = idx->Child("pdsc")
    If pdsc = 0 Then
      Print "Missing pdsc node"

    else

        dim childenpdsccounter as Integer
        for childenpdsccounter = 0 to idx->Children("pdsc")  - 1

            'print str(childenpdsccounter)+": "+ idx->Child("pdsc", childenpdsccounter )->Attribute("name")

            pdsc = idx->Child("pdsc", childenpdsccounter )

            if pdsc = 0 then
              Print "Missing pdsc node"
            else


              dim childenreleasescounter as Integer
              for childenreleasescounter = 0 to pdsc->Children("atmel:releases") -1

                releases = pdsc->Child("atmel:releases", childenreleasescounter )

                redim releaseversions( 0 )
                dim childenreleasecounter as Integer
                for childenreleasecounter = 0 to releases->Children("atmel:release") -1

                   releaseversions( ubound(releaseversions)  ) = releases->Child("atmel:release", childenreleasecounter )->Attribute("version")
                   redim preserve releaseversions( ubound(releaseversions) + 1 )

                   release = releases->Child("atmel:release", childenreleasecounter )
                   'print "*"+release->Attribute("version")

                   'Now find the devices

                   dim devicescounter as integer
                   for devicescounter = 0 to release->Children( "atmel:devices" ) -1

                       devices = release->Child("atmel:devices", devicescounter)

                       dim devicecounter as integer
                       for devicecounter = 0 to devices->Children( "atmel:device" ) -1

                          with SourceFileArray( SourceFileArrayPointer )
                                .ChipName = devices->Child( "atmel:device" , devicecounter )->Attribute("name")
                                .Core = devices->Child( "atmel:device" , devicecounter )->Attribute("core")
                                .Family = devices->Child( "atmel:device" , devicecounter )->Attribute("family")
                                .Version = releases->Child("atmel:release", childenreleasecounter )->Attribute("version")
                                .Pack = idx->Child("pdsc", childenpdsccounter )->Attribute("name")
                                .UserProfile = UserProfile + kINDEX_IDX
                          end with
                          ' print devices->Child( "atmel:device" , devicecounter )->Attribute("name")
                          SourceFileArrayPointer = SourceFileArrayPointer + 1
                       next
                   next



                next

                'now determine the best version
                latestversion = ""
                for childenreleasecounter = 0 to ubound(releaseversions)
                    if releaseversions( childenreleasecounter ) > latestversion then
                      latestversion = releaseversions( childenreleasecounter )
                    end if
                next


              next


            end if

        next

    End If


  End If

FinishRead:

End Sub

SUB Replace (DataVar As String, Find As String, Rep As String)
    Dim As String VarTemp, FindTemp, NewData

    VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
    IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB

    NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
    NewData = NewData + Rep
    NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))

    DataVar = NewData
END Sub


Sub Split(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)

   Dim As Long x, p
   If Count < 1 Then
      Do
         x = InStr(x + 1, Text, Delim)
         p += 1
      Loop Until x = 0
      Count = p - 1
   ElseIf Count = 1 Then
      ReDim Ret(Count - 1)
      Ret(0) = Text
   Else
      Count -= 1
   End If
   Dim RetVal(Count) As Long
   x = 0
   p = 0
   Do Until p = Count
      x = InStr(x + 1,Text,Delim)
      RetVal(p) = x
      p += 1
   Loop
   ReDim Ret(Count)
   Ret(0) = Left(Text, RetVal(0) - 1 )
   p = 1
   Do Until p = Count
      Ret(p) = Mid(Text, RetVal(p - 1) + 1, RetVal(p) - RetVal(p - 1) - 1 )
      p += 1
   Loop
   Ret(Count) = Mid(Text, RetVal(Count - 1) + 1)

End Sub


Sub PopulateRAMDefinitions ( SourceFolder as string, Version as string, Chip as string )

  'reads Chip  +.PIC file. It is XML to extract the BADRAM. I could not find BADRAM anywhere else.
  'populates the badram() array with an array of strings with the badram

  Dim As ext.xml.node Ptr xmlroot
  Dim As ext.xml.node Ptr PIC, DataSpace, RegardlessOfMode, SFRDataSector


  Var xmldoc = new ext.xml.tree
  xmldoc->load( kINDEX_IDX+"\Microchip\"+SourceFolder+"_DFP\"+Version+"\edc\"+ Chip  +".PIC" )

  'Print "; XML source : " + kINDEX_IDX+"\Microchip\"+SourceFolder+"_DFP\"+Version+"\edc\"+ Chip  +".PIC"

  If xmldoc = 0 Then
    Print "XML file seems empty"
    GoTo FinishReadRAM
  End If
  xmlroot = xmldoc->root
  If xmlroot = 0 Then
    Print "Missing root node"
    GoTo FinishReadRAM
  End If

  PIC=xmlroot->Child("edc:PIC")

  if PIC <> 0 then

      DataSpace = PIC->Child("edc:ProgramSpace")->Child("edc:CodeSector")
      '       __MAXRAM  H'3FFF'
      badrampointer += 1

      badram ( badrampointer ) = "       __MAXRAM  H'" +right("0000"+ hex(val(DataSpace->Attribute("edc:endaddr"))-1),4)+"'"

      DataSpace = PIC->Child("edc:DataSpace")

      RegardlessOfMode = DataSpace->Child("edc:RegardlessOfMode")

      dim childenofRegardlessOfMode as Integer
      for childenofRegardlessOfMode = 0 to RegardlessOfMode->Children("edc:SFRDataSector") - 1
           'print RegardlessOfMode->Child("edc:SFRDataSector", childenofRegardlessOfMode )->Attribute("edc:bank")

           SFRDataSector = RegardlessOfMode->Child("edc:SFRDataSector", childenofRegardlessOfMode )

            dim _addressstr as string
            dim _lengthstr as string
            dim _endaddress as Integer
            dim childrenofSFRDataSector as Integer
            for childrenofSFRDataSector = 0 to  SFRDataSector->Children("edc:AdjustPoint") -1

              _lengthstr  = SFRDataSector->Child("edc:AdjustPoint", childrenofSFRDataSector )->Attribute("edc:offset")
              _addressstr = SFRDataSector->Child("edc:AdjustPoint", childrenofSFRDataSector )->Attribute("edc:_addr")
              _endaddress = val( _addressstr ) + val(  _lengthstr ) -1


              if val( _lengthstr ) -1  = 0 then
                  badrampointer += 1
                  badram ( badrampointer ) = "       __BADRAM  H'" + right("0000"+hex(val( _addressstr )),4)+"'"
              else
                  badrampointer += 1
                  badram ( badrampointer ) = "       __BADRAM  H'" + right("0000"+hex(val( _addressstr )),4)+"'-" +_
                  "H'"+right("0000"+hex(_endaddress),4)+"'"

              end if


            next
      next



   else
      Print "XML read error"

  end if

FinishReadRAM:

End Sub



Sub PopulateConfig( SourceFolder as string, Version as string, Chip as string )

    Dim DataSource as string
    Dim ConfigAddress() as string
    Dim ConfigOption() as string
    Dim ConfigChoices() as string
    Dim fsp_config as string
    Dim configname as string
    Dim configmask as string
    Dim configdefaultvalue as string
    Dim newmaskstring as string
    Dim newmask as Integer
    Dim configsetting as string
    Dim configaliaspointer as Integer
    Dim configsuffix as string
    Dim idlocsuffix as Integer = 0

    Dim ConfigCount as Integer = 0
    fsp_config = kINDEX_IDX+"\Microchip\"+SourceFolder+"_DFP\"+Version+"\xc8\avr\dat\cfgdata\"+ Chip  +".cfgdata"

    open fsp_config for input as #1
    do while not eof(1)
        Line input #1, DataSource
        DataSource = ucase(DataSource)
        if left(datasource,6)="CWORD:" and instr(datasource,"CONFIG")<>0 then
            split( DataSource, ":", 99, ConfigAddress() )
            print "_"+left(ConfigAddress(4)+"      ",13)+"EQU  H'"+ConfigAddress(1)+"'"
            ConfigCount += 1
        end if
    loop
    close #1

    'reread file now extracting options

'      #     CWORD:<address>:<mask>:<default value>[:<name>[,<alias list>]]
'      #
'      # for each CWORD the configuration settings are listed as
'      #
'      #     CSETTING:<mask>:<name>[,<alias list>]:<description>
'      #
'      # lastly for each CSETTING all possible values are listed as
'      #
'      #     CVALUE:<value>:<name>[,<alias list>]:<description>

    Dim configloop as Integer



'      CWORD:300000:77:FF:CONFIG1L
'        CSETTING:7:FEXTOSC:External Oscillator Selection
'            CVALUE:0:LP:LP (crystal oscillator) optimized for 32.768 kHz; PFM set to low power
'            CVALUE:1:XT:XT (crystal oscillator) above 100 kHz, below 8 MHz; PFM set to medium power
'            CVALUE:2:HS:HS (crystal oscillator) above 8 MHz; PFM set to high power
'            CVALUE:4:OFF:Oscillator not enabled
'            CVALUE:5:ECL:EC (external clock) below 100 kHz; PFM set to low power
'            CVALUE:6:ECM:EC (external clock) for 500 kHz to 8 MHz; PFM set to medium power
'            CVALUE:7:ECH:EC (external clock) above 8 MHz; PFM set to high power
'        CSETTING:70:RSTOSC:Reset Oscillator Selection
'            CVALUE:0:HFINTOSC_64MHZ:HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1
'            CVALUE:20:EXTOSC_4PLL:EXTOSC with 4x PLL, with EXTOSC operating per FEXTOSC bits
'            CVALUE:40:SOSC:Secondary Oscillator
'            CVALUE:50:LFINTOSC:Low-Frequency Oscillator
'            CVALUE:60:HFINTOSC_1MHZ:HFINTOSC with HFFRQ = 4 MHz and CDIV = 4:1
'            CVALUE:70:EXTOSC:EXTOSC operating per FEXTOSC bits (device manufacturing default)


    open fsp_config for input as #1

    Print ";=========================================================================="
    print "             "+ConfigFilename
    Print ";=========================================================================="

    do while not eof(1)
        Line input #1, DataSource
        DataSource = trim(DataSource)

        if left(ucase(datasource),6)="CWORD:" and instr(datasource,"CONFIG")<>0 then
            '16f
            'CWORD:8007:1133:3FFF:CONFIG1

            split( DataSource, ":", 99, ConfigAddress() )

            configdefaultvalue = ConfigAddress(3)

            configname = ConfigAddress(4)
            configaliaspointer = Instr(configname,",")

            'strip of the alias
            if configaliaspointer <> 0 then
                configname = left(configname,configaliaspointer)
            end if

            'old config words have L or H but new config bytes have no L or H
            if ucase(right(configname,1))="L" or ucase(right(configname,1))="H" then
              configsuffix = right(trim(configname),2)
            else
              configsuffix = right(trim(configname),1)

            end if



            print ""
            print ";----- "+configname+" Options --------------------------------------------------"
         elseif left(ucase(datasource),9)="CSETTING:" then
            'CSETTING:3:FEXTOSC:External Oscillator Mode Selection bits

            split( datasource, ":", 99, ConfigOption() )
            configsetting = "_"+ConfigOption(2)
            configmask = ConfigOption(1)


         elseif left(ucase(datasource),7)="CVALUE:" then

            '16F
            'CWORD:8007:1133:3FFF:CONFIG1
            'CSETTING:3:FEXTOSC:External Oscillator Mode Selection bits
            'CVALUE:3:ECH:EC (external clock) 16 MHz and above
            'CVALUE:2:ECL:EC (external clock) below 16MHz
            'CVALUE:1:OFF:Oscillator not enabled
            'CSETTING:30:RSTOSC:Power-up Default Value for COSC bits


            split( datasource, ":", -1, ConfigChoices() )
            'merge strings
            select case ubound(ConfigChoices)

              case 4
                  ConfigChoices(3) = ConfigChoices(3) +":"+ ConfigChoices(4)
              case 5
                  ConfigChoices(3) = ConfigChoices(3) +":"+ ConfigChoices(4) +":"+ ConfigChoices(5)

            end select


            newmask = ( val("&H"+configmask) XOR val("&H"+configdefaultvalue) ) OR val("&H"+ConfigChoices(1))

            newmaskstring = right("00"+hex(newmask),4)


            if len( configsetting+"_"+ConfigChoices(2)+"_"+configsuffix ) < 21 then
              print left(configsetting+"_"+ConfigChoices(2)+"                 ",21)+"EQU  H'"+ newmaskstring +"'; "+ConfigChoices(3)
            else
              print configsetting+"_"+ConfigChoices(2)+" EQU  H'"+ newmaskstring +"'; "+ConfigChoices(3)
            end if


          elseif left(ucase(datasource),6)="CWORD:" and instr(datasource,"IDLOC")<>0  then
'          CWORD:20000E:FFF:FFF:IDLOC7
            if idlocsuffix = 0 then
                print ""
                print ";----- DEVID Equates --------------------------------------------------"
            end if
            split( datasource, ":", 99, ConfigChoices() )

            print left("_IDLOC"+ str(idlocsuffix)+"                ",18)+" EQU  H'"+ ConfigChoices(1)+"'"
              idlocsuffix = idlocsuffix + 1
            print left("_IDLOC"+ str(idlocsuffix)+"                ",18)+" EQU  H'"+ hex(val("&H"+ConfigChoices(1))+1)+"'"
              idlocsuffix = idlocsuffix + 1
         end if


    loop
    close #1

End Sub


Sub InitAndGetFiles

  ' reads both IDX files.
  Dim CD as integer = 1
  Dim forIndex as Integer
  
  UserProfile = ENVIRON("Userprofile")
  UserProfile = UserProfile + "\.mchp_packs"

    For forIndex = 1 to 2 

        'read the kINDEX_IDX to populate SourceFileArray()
        PopulateSourceFilesLocation

        UserProfile = "C:\Program Files\Microchip\MPLABX\v"+kIDEVersion+"\packs"

    Next

    If COMMAND(CD) = "" then
        Print "MPLAB-IDE version " + kIDEVersion
        'Display the contents of the kINDEX_IDX to the console... essentially shows SourceFileArray()
        Dim loopcounter as Integer
        for loopcounter = 0 to SourceFileArrayPointer - 1
            Dim localpack as string
            Dim localchipname as string

            with SourceFileArray( loopcounter )

                localchipname = .chipname
                replace ( localchipname, "attiny", "tn" )
                'create a filename from the data
                localpack = "\"+.Pack
                replace(localpack, ".pdsc","")
                replace(localpack, "Microchip.","Microchip\")
                localpack = .UserProfile+localpack + "\" + .Version + "\avrasm\inc\"+localchipname+"def.inc" 
                localpack = lcase(localpack)

                print left(.Pack+"                              ",45)+ left(.ChipName+"                              ",25) +  left(.Version+"                              ",15)+.Core + "  " + _ 
                right("   "+str(fileexists(localpack)),3) + " " + localpack

            end with
        next
        
    End If


    If COMMAND(CD) = "?" then
        Print "No help available"
        End
    End if

    If Ucase(COMMAND(CD)) = "DEBUG" then
        ShowDebug = -1
        CD = CD + 1
        print ";Debug:  Second command line parameter is: "+trim(COMMAND(CD))
    End if

    
    'Process the command line
    
    ParamUpper = Ucase(COMMAND(CD))

    targetchip = ParamUpper
    If left(targetchip,3) <> "AVR" then

      If left(targetchip,2) <> "AT" then
          targetchip = "AT"+targetchip
      End if

    End If

    if ShowDebug = -1 then print ";Debug: kINDEX_IDX directory location is: "+UserProfile+kINDEX_IDX
    if ShowDebug = -1 then print ";Debug: targetchip is: "+targetchip

    
    UserProfile = ENVIRON("Userprofile")
    UserProfile = UserProfile + "\.mchp_packs"

      dim ignoreversion as String = ""

      For forIndex = 1 to 2 
          Do
          
            chipdetails = FindChip ( targetchip , ignoreversion )
            
            if left(chipdetails,6) = "NOCHIP" or  chipdetails = "" then
              if forindex = 2 then 
                print "Not a valid chip, or no DFP files .. ", chipdetails
                print fsp_ini
                End
              End if
              exit do
            end if

            chip = targetchip
            replace(chip,"AT","")

            chipincsource = chip
            If instr( ucase(chipincsource) , "MEGA" ) > 0 then
                replace( chipincsource, "MEGA", "")
                chipincsource = "m"+chipincsource+"def"
            End if
            If instr( ucase(chipincsource) , "TINY" ) > 0 then
                replace( chipincsource, "TINY", "")
                chipincsource = "tn"+chipincsource+"def"
            End if

            chiphsource = chip
            If instr( ucase(chiphsource) , "MEGA" ) > 0 then
                replace( chiphsource, "MEGA", "")
                chiphsource = "iom"+chiphsource
            End if

            If instr( ucase(chiphsource) , "TINY" ) > 0 then
                replace( chiphsource, "TINY", "")
                chiphsource = "iotn"+chiphsource
            End if

            If left( ucase(chipincsource), 3) = "AVR" then
                chipincsource = chipincsource+"def"
            End if


            split( chipdetails, ",",-1,chipparameters() )

            fsp_ini = UserProfile+kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\avrasm\inc\"+ chipincsource  +".inc"


            if dir(fsp_ini) <> "" then
              Exit Sub
            End if  

            ignoreversion = chipparameters(1)
          
          Loop
          UserProfile = "C:\Program Files\Microchip\MPLABX\v"+kIDEVersion+"\packs"

      Next

End Sub

Sub PrintHeader

    Print ";        .DAT sections"
    Print ";=========================================================================="
    Print ";"
    Print ";  Built by GCBASIC converter"
    Print ";  XC8 processor include for the chip shown below"
    Print ";"
    print "; Microchip IDE version " + kIDEVersion
    Print "; " + UserProfile + kINDEX_IDX+"\index.idx"
    Print "; " + fsp_ini
    Print "; " + kXLScs
    print "; " + UserProfile + kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\edc\AT"+ Chip  +".PIC"
    print "; " + ConfigFileName
    print ";"
    print "; Registers not processed: " + kKillRegister
    print "; Bits not processed: " + kKillBits
    
    Print ";=========================================================================="


    Print ";=========================================================================="
    Print ";"
    Print ";       Register Definitions"
    Print ";"
    Print ";=========================================================================="
    Print ""
    Print ";----- Register Files -----------------------------------------------------"



End Sub

Sub Printregisters

    if instr(kKillRegister,",") <> 0 then
      Split ( kKillRegister, ",", -1, RegistersToBeIgnored() )
    else
      redim RegistersToBeIgnored(1)
      RegistersToBeIgnored(0) = trim(kKillRegister)
    end if

    CurrentBank = -1

    open fsp_ini for input as #1
    If Err>0 Then Print "Error opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End

    'read down to "ABSOLUTE I/O REGISTER LOCATIONS"

    do
        Line input #1, DataSource
    loop while not eof(1) and instr( ucase(DataSource),"ABSOLUTE I/O REGISTER LOCATIONS") = 0

    If eof(1) then
      Close
      Print "Unexpected end of file "+chr(34)+fsp_ini+chr(34):Print "I/O REGISTER DEFINITIONS text not found"
      End
    End If

    Print ""
    Print "[Registers]"

      print "'For specific details of the registers see the microcontroller datasheet"
      print "'The first parameter is the GCBASIC register name used in user code to expose the specific register"

    do while not eof(1)
        Line input #1, DataSource

        If DataSource <> "" and instr( ucase(datasource), ".EQU" ) > 0 Then

            'check to see if entry should be ignored.
            FoundRegistersToBeIgnored = 0
            for RegistersToBeIgnoredCounter = 0 to ubound( RegistersToBeIgnored )
              if trim(RegistersToBeIgnored ( RegistersToBeIgnoredCounter)) <> "" Then
                  if  instr( trim(ucase( datasource)), trim(ucase(RegistersToBeIgnored ( RegistersToBeIgnoredCounter)+" "))) = 6 then
                      FoundRegistersToBeIgnored = 1
                      print "; "+datasource + " a duplicate or resevered register or ALIAS_"
                  end if
              End if
            next
            if FoundRegistersToBeIgnored = 0 then
                SFRDataCount += 1
                SFRData(SFRDataCount).Count = SFRDataCount
                replace ( DataSource, ".equ", "")

                replace ( DataSource, "_BASE", "")


                Do while instr(DataSource, chr(9) ) > 0
                  replace ( DataSource, chr(9), " ")
                Loop
                DataSource = trim( DataSource )
                'print "*"+DataSource+"*",
                SFRData(SFRDataCount).RegisterName = trim(left(Trim(UCase(DataSource)),instr(DataSource, "=" )-1 ))
                with SFRData(SFRDataCount)
                    .RegisterAddresss = "&H00"+trim(mid(DataSource, instr( DataSource,"=" )+4 , instr( DataSource,")")-instr( DataSource,";")-1  ))
                    'Output register
                    print " "+.RegisterName+","+str(val(.RegisterAddresss))
                end with
            end if
        end if

        if instr( ucase(DataSource),"ALL MODULE BASE ADRESSES") > 0 Then
          Exit Do
        End if
    loop
    close   ' close all open files

End Sub

Sub PrintAVRMasks

        Dim killIndex as Integer

        if instr(kKillBits,",") <> 0 then
          Split ( kKillBits, ",", -1, BitsToBeIgnored() )
        else
          redim BitsToBeIgnored(1)
          BitsToBeIgnored(0) = trim(kKillBits)
        endif


        open fsp_ini for input as #1
        If Err>0 Then
          Print "Error re-opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End
        End if


        ' print "read down to `* BIT AND VALUE DEFINITIONS * - `"

        do
            Line input #1, DataSource
        loop while not eof(1) and instr( ucase(DataSource),"* BIT AND VALUE DEFINITIONS *") = 0

        If eof(1) then
          Close
          Print "Unexpected end of file "+chr(34)+fsp_ini+chr(34):Print "`* BIT AND VALUE DEFINITIONS * - ` text not found"
          End
        End If

        Print ""
        Print "[AVRMASKS]"
          print "'For details of the see the microcontroller datasheet"

        SFRBitsArrayPointer = 0

        do while not eof(1)
            
            if readNextLine = -1 then Line input #1, DataSource

            if instr( Datasource, "* CPU REGISTER DEFINITIONS *" ) > 0 then 
              exit do
            End If
            
            DataSource = trim( DataSource )

            replace ( DataSource , ".equ", "" )
            DataSource = trim( DataSource )
            print "  " + DataSource
            If Instr( DataSource, "ADC_MUXPOS_AIN" ) > 0 then
                replace ( DataSource, "ADC_MUXPOS_", "" )
                replace ( DataSource, "_gc", "")
                print "  " + DataSource
            End if

        loop

        Close

End Sub

Sub PrintAVRChipSpecifics

        Dim killIndex as Integer

        if instr(kKillBits,",") <> 0 then
          Split ( kKillBits, ",", -1, BitsToBeIgnored() )
        else
          redim BitsToBeIgnored(1)
          BitsToBeIgnored(0) = trim(kKillBits)
        endif


        open fsp_ini for input as #1
        If Err>0 Then
          Print "Error re-opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End
        End if

        do
            Line input #1, DataSource
        loop while not eof(1) and instr( ucase(DataSource),"* DATA MEMORY DECLARATIONS *") = 0

        If eof(1) then
          Close
          Print "Unexpected end of file "+chr(34)+fsp_ini+chr(34):Print "`* DATA MEMORY DECLARATIONS *` text not found"
          End
        End If

        Print ""
          print "'For details of microcontroller specifications see the microcontroller datasheet"

        SFRBitsArrayPointer = 0

        do while not eof(1)
            
            if readNextLine = -1 then Line input #1, DataSource

            if instr( Datasource, "; Legacy definitions" ) > 0 then 
              exit do
            End If
            
            DataSource = trim( DataSource )
            If DataSource <> "" Then
              replace ( DataSource , "#DEFINE", "" )
              DataSource = trim( DataSource )
              replace ( DataSource, " ", " = ")
              print "_" + DataSource
            End If
        loop

        Close

End Sub


Sub PrintBits

        Dim killIndex as Integer

        if instr(kKillBits,",") <> 0 then
          Split ( kKillBits, ",", -1, BitsToBeIgnored() )
        else
          redim BitsToBeIgnored(1)
          BitsToBeIgnored(0) = trim(kKillBits)
        endif


        open fsp_ini for input as #1
        If Err>0 Then
          Print "Error re-opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End
        End if


        ' print "read down to `* BIT AND VALUE DEFINITIONS * - `"

        do
            Line input #1, DataSource
        loop while not eof(1) and instr( ucase(DataSource),"* BIT AND VALUE DEFINITIONS *") = 0

        If eof(1) then
          Close
          Print "Unexpected end of file "+chr(34)+fsp_ini+chr(34):Print "`* BIT AND VALUE DEFINITIONS * - ` text not found"
          End
        End If

        Print ""
        Print "[Bits]"
          print "'For details of the bits (relative to a register in terms of registerbits) see the microcontroller datasheet"
          print "'The first parameter is the GCBASIC bit name used in user code to expose the specific registerbit"
            print " ' Required for GCBASIC operations"          
            print "  I,SREG,7"
            print "  T,SREG,6"
            print "  H,SREG,5"
            print "  S,SREG,4"
            print "  V,SREG,3"
            print "  N,SREG,2"
            print "  Z,SREG,1"
            print "  C,SREG,0"

        SFRBitsArrayPointer = 0

        do while not eof(1)
            
            if readNextLine = -1 then Line input #1, DataSource

            if instr( Datasource, "* CPU REGISTER DEFINITIONS *" ) > 0 then 
              exit do
            End If
            
            DataSource = trim( DataSource )

            'find register
            If DataSource <> "" and left(datasource,2) = "; " and instr( ucase(datasource), "MASKS" ) > 0 Then
            
                 'cache register
                  replace ( DataSource , "; ", "" )
                  replace ( DataSource , chr(9), " " )
                  replace ( DataSource , "MASKS", "" )
                  DataSource = trim( DataSource )
                  NewRegister =  DataSource 

                  readNextLine = -1

                  'new register found!
                  if NewRegister <> CurrentRegister then
                      ' print "New Register = "; "'"+NewRegister+"'"

                      NewRegister = Mid( NewRegister, 1, Instr( NewRegister, "_") - 1)
                      CurrentRegister = trim(NewRegister)
                      ' read next line

                      Dim orgline as String

                      Do

                          Line input #1, DataSource
                          Orgline = DataSource
                          If trim(DataSource) = "" then Exit Do
                          ' print 0, DataSource
                          if instr( ucase(datasource), ucase(".equ") ) > 0  and  instr( ucase(datasource), ucase("bit position") ) > 0  then
                              'ensures this is a .equ
                                  ' print 1, DataSource, 
                              replace( DataSource,".equ","")
                              replace( DataSource,chr(9)," ")

                              ' Do some data corrections
                                replace ( DataSource, "_bp", ""  )

                              DataSource = Trim( DataSource )
                              EqualPosition = Instr ( DataSource, "=" ) -1
                              SemiColonPosition = Instr ( DataSource, ";" ) -1

                                  ' print trim( Left( DataSource, EqualPosition ) )


                          
                              SFRBitsArrayPointer+=1
                              SFRBits(SFRBitsArrayPointer).RegisterRaw=";----- "+ DataSource
                              SFRBits(SFRBitsArrayPointer).RegisterBitName = trim( Left( DataSource, EqualPosition ) )
                              replace( SFRBits(SFRBitsArrayPointer).RegisterBitName,chr(9)," ")


                              SFRBits(SFRBitsArrayPointer).RegisterBitAddress = trim( Mid( DataSource, EqualPosition+2, SemiColonPosition-EqualPosition-1 ) )
                              SFRBits(SFRBitsArrayPointer).Count=SFRBitsArrayPointer

                              'KILL bits
                              Dim BitPrint as Integer = -1
                              For killIndex = 0 to ubound(  BitsToBeIgnored ) -1
                                  if Instr( ucase(trim(SFRBits(SFRBitsArrayPointer).RegisterBitName))  , Ucase(BitsToBeIgnored(killIndex)))  Then
                                    BitPrint = 0
                                  End If
                              next

                              If BitPrint = -1 then
                                Print trim(SFRBits(SFRBitsArrayPointer).RegisterBitName)+","+NewRegister+","+SFRBits(SFRBitsArrayPointer).RegisterBitAddress + chr(9)+ chr(9)+ chr(9) + "' " + mid( Orgline, Instr( OrgLine, "; ")+1)
                              Else
                                Print "'"  +  trim(SFRBits(SFRBitsArrayPointer).RegisterBitName)+","+NewRegister+","+SFRBits(SFRBitsArrayPointer).RegisterBitAddress + chr(9)+ chr(9)+ chr(9) + "' " + mid( Orgline, Instr( OrgLine, "; ")+1)   +  "           Removed as duplicate"
                              End If

                              If instr( SFRBits(SFRBitsArrayPointer).RegisterBitName ,"SREG_" ) > 0 then
                                  replace (   SFRBits(SFRBitsArrayPointer).RegisterBitName, "SREG_","")
                                  Print SFRBits(SFRBitsArrayPointer).RegisterBitName+","+NewRegister+","+SFRBits(SFRBitsArrayPointer).RegisterBitAddress
                              End if

                          Else

                              ' print "Not an equ or bit position"
                              'readNextLine = 0
                              
                          End if
                          'keep reading until not an empty line
                      loop while trim(DataSource) <> ""

                  end if

            End if
        loop

        Close

End Sub


Function GetValue (  searchString as String, errorhandler as Integer = -1 ) As String

    open fsp_ini for input as #1
    If Err>0 Then Print "Error opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End

    do
        Line input #1, DataSource
    loop while not eof(1) and instr( ucase(DataSource), Ucase(searchString)) = 0

    If eof(1) and instr( ucase(DataSource), Ucase(searchString)) = 0 Then
        Close
        If errorhandler = -1 then
            
            Print "Unexpected end of file "+chr(34)+fsp_ini+chr(34):Print "Search string `" + searchString + "` not found"
            End
        Else
            Return ""
        End If
    End If



    DataSource = trim(  mid( DataSource, instr( DataSource, "0x") +2 ) )

    close   ' close all open files

    Return  DataSource
End Function

Function GetCSVValue (  searchString as String, returnParameter as Byte ) As String

    Dim RetString as String 
    returnParameter = returnParameter * 1 

    open kXLScs for input as #1
    If Err>0 Then Print "Error opening the file "+chr(34)+kXLScs+chr(34):Print  chipparameters(1) :End

    do
        Line input #1, DataSource
        ' DEBUG print DataSource, Ucase(searchString), instr( ucase(DataSource), Ucase(searchString))
    loop while not eof(1) and instr( ucase(DataSource), Ucase(searchString)) = 0

    If eof(1) and instr( ucase(DataSource), Ucase(searchString)) = 0 then
      Close
      Print "Unexpected end of file "+chr(34)+kXLScs+chr(34):Print "Search string `" + searchString + "` not found"
      End
    End If

    close   ' close all open files

    Split ( DataSource, ",", 99, ParamMeters() )
    'send back the parameter from the XLS
    return ParamMeters(returnParameter)
End Function