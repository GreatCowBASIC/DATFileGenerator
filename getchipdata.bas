'Chip data importer
'Generates GCBASIC chipdata files
'Sources of input data are:
' - chipdata.csv: Main list of chips, memory size, pin count, A/D channels, max clock speed
' - MPASM .inc files: SFRs, SFR bits, CONFIG options
' - MPLAB .dev files (can skip): Exact I/O pins, 18F default config
' - MPLAB VDI .xml files (can skip): Exact pinout
'Data calculated from input:
' - RAM banks (from memory size, chip name in some cases)
' - INTOSC speeds (from SFRs present)
' - Interrupts (from SFR bits present)
' - Some alternative pin functions (SPI/I2C from pin count, SFRs)
' added CriticalChanges capability

'Fixed 10f320 issue
'$ Delimiter to handle _ in configasm section
'increased all arrys to handle crashes
'added clear Arrays to resolve incorrect output
'corrected ASMConfig section and revised ReadMPASMInfo. bit masks For FOSC2 are incorrect on the "J" chip data file.  Example 18F24J10
'added full Register_Bit for the I2C bits.  The flat namespace meant PPStool was not able to find all the i2c bits.

Declare Sub AddMissingData
Declare Sub ReadIncFileLineIn
Declare Sub AddPinFunction(Key As String, Port As String, Direction As String)
Declare Sub CalcConfigMasks
Declare Sub CalcIntOscSpeeds
Declare Sub CalcRamBlocks
Declare Sub CalcNoBankString ( localNoBankRAMStr as string  )
Declare Function CheckBit (BitName As String) As Integer
Declare Function CompareLFFile ( Fchip as string) as Integer
Declare function ReplaceStr(ByRef src as String,ByRef find As String,ByRef repl as String,start As Integer=1) as String
Declare Function GetPin(PinName As String, ChipPins As Integer) As Integer
Declare Sub GetPowerPins (ChipPins As Integer, OscPins As Integer, MCLRPin As Integer)
Declare Function GetIntName (IntVect As String) As String
Declare Sub GuessChipPinout
Declare Sub GuessDefaultConfig
Declare Sub GuessPinAltFunctions (PinCount As Integer)
Declare Function HasSFR(SFRName As String) As Integer
Declare Function HasSFRBit(BitName As String) As Integer
Declare FUNCTION IsDivider (Temp As String) As Integer
Declare Function IsSameConfig(Name1 As String, Name2 As String) As Integer
Declare Sub ReadDevFiles (DevFileName As String)
Declare Sub ReadMPASMInfo
Declare Sub ReadXmlFiles (ChipName As String)
DECLARE SUB Replace (DataVar As String, Find As String, Rep As String)
Declare Function SplitInfoLine(DataIn As String, LineItem() As String) As Integer
Declare Sub Split(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)
DECLARE FUNCTION WholeINSTR (DataIn As String, FindIn As String) As Integer
Declare Sub Read18FASMConfigFile
Declare Sub ReadCriticalChanges
Declare Function CountUSART as string
Declare Function TALLY(SomeString As String,PartString As String) As Long

Enum Boolean
    False = 0, True = Not False
End Enum

Type ChipPin
    PinName As String
    Number As Integer
    FunctionList(20, 2) As String
    FunctionCount As Integer
End Type

Type PinOut
    Name As String
    PinCount As Integer
    PinList(100) As ChipPin
End Type

Type SysVarBit
    Name As String
    OldName As String
    Reg As String
    Bit As Integer
End Type

Type IntData
    EventName As String
    EnableBit As String
    FlagBit As String
End Type

Type ConfigSetting
    Name As String
    Value As Integer
    Location As Integer
    Duplicate As Integer
    SettingName As String
End Type

Type ConfigSettingOption
  Name As String
  Choices As String
  DefaultGuess As String
End Type

Dim Shared As Integer PowerPinCount, SVBC, SVC, PSP, ConfWords, MaxChipAddress

dim Shared As Integer HandlingAddingLine

Dim Shared As Long MaxCount
Dim Shared As String s(), text, Delimiter, LastLineRead

Dim As Integer ILC, ChipDataCount, ChipIncCount, StartChip, CurrentChip
Dim Shared As Integer CurrentChipCheck

Dim As Integer RC, CW, T, DisableRead, CVNC, CheckSFR, CheckSFR2, CheckSFRBit, SingleByteConfigElement
Dim As Integer PD, DL, CD, IntFound, FindInt, SortMore, PPO, ArrayPointer
Dim As Integer OscPins, MCLRPin, PF
Dim Shared As Integer ChipFamily, RBC, SRBC, ConfigOptions, ChipFamilyVariant, ChipNotTested
Dim Shared As Integer ChipEEPROM, ChipRAM, ChipIO, ChipADC
Dim Shared As Double ChipWords, ChipMHz, ChipIntOsc, ChipUSART
Dim Shared As String ChipIntOscSpeeds, FixedConstants

Dim Shared As String InstDir, IncFileDir, DevFileDir, MPASMInfoFile, OutputDir, VDIFileDir, ChipName, _31kSupportValueStr,  _31kSupportValueExpandedStr,ADCHelperStr
Dim Shared As Integer DefConf, ConfMask, DFN, ChipPins, COC, PinoutCount, PowerPinsAdded
Dim Shared As Integer ChipSubFamily, HEFMemory, SAFMemory, ProgMemRowSize, ProgMemRowSizeMandate, PWMTimerVariant, SMTClockSourceVariant, ConfigBaseLocVariant, IntOSCCONFormatVariant, READAD10BITFORCEVariant, Stacks, IDAddress, IDLength, SectorRAMAddress, SectorRAMAddressSize, ChipMinimumBankSel, ChipSelfWrite
Dim Shared As Integer WriteFlashBlockSize, EraseFlashBlockSize


Dim Shared As Integer ChipDataSuspect, ConfigGuessed

Dim Shared As String M, D, Y, DataSource, NoBankRAMStr

Dim Shared As String ChipData(2000), ChipIncList(2000)
Dim Shared Config(2047) As ConfigSetting
Dim Shared PowerPin(20) As String: PowerPinCount = 0
'Dim Shared As String ConfigSettings(500, 2)
Dim Shared ConfigOption(1000) As ConfigSettingOption
Dim Shared As String DefConfig(1000, 2)
Dim Shared As Integer ConfigMask(40)

Dim Shared As String SysVars(2000, 2)
Dim Shared SysVarBits(16000) As SysVarBit
Dim Shared IntList(400) As IntData: ILC = 0
Dim Shared As String RamBlock(100)
Dim Shared As String SharedRamBlock(40)
Dim Shared Pinouts(20) As Pinout: PinoutCount = 0
Dim As String IntName, IntFlag, TempData, CurrentDate

Dim As String BitVal, DSO, ChipNameTemp
Dim As String CurrConfWord, MaxRam, VarLoc, CurrentVar, BitName, BitLoc, ThisChipData
Dim As String ConfName, ConfValue, DevFileName, NewData

Dim Shared As String Read18FConfigLines(1000)
Dim Shared As Integer Read18FConfigArray
Dim Shared as Integer SilentRunning

Dim Shared As String ReadCriticalDatLine(2000)
Dim Shared As Integer CriticalChangeIndex

InstDir = "."
IncFileDir = InstDir + "\incfiles"
DevFileDir = InstDir + "\MPLAB Files\Device"
VDIFileDir = InstDir + "\MPLAB Files\XML"
MPASMInfoFile = InstDir + "\MPLAB Files\8bit_device.info"
OutputDir = InstDir + "\chipdata"

TempData = Date
M = Left(TempData, INSTR(TempData, "-") - 1)
TempData = Mid(TempData, INSTR(TempData, "-") + 1)
D = Left(TempData, INSTR(TempData, "-") - 1)
Y = Mid(TempData, INSTR(TempData, "-") + 1)
IF LEFT(D, 1) = "0" Then D = MID(D, 2)
IF LEFT(M, 1) = "0" Then M = MID(M, 2)
CurrentDate = D + "/" + M + "/" + Y


SilentRunning = true

Close
if SilentRunning = false then
  PRINT "GCBASIC/GCGB Chip Data File Generator"
  PRINT
end if

'Read chipdata.csv
if SilentRunning = false then
  PRINT "Reading chipdata.csv ..."
end if
ChipDataCount = 0
open "chipdata.csv" for input as #1
do while not eof(1)
    Line input #1, DataSource
    If DataSource <> "" Then
        ChipDataCount += 1
        ChipData(ChipDataCount) = Trim(UCase(DataSource))
    end if
loop
close
if SilentRunning = false then
    PRINT

    Print "Reading 18f Default ASM Configuration File...."
end if
Read18FASMConfigFile
if SilentRunning = false then PRINT


if SilentRunning = false then Print "Reading Critical Changes  File...."
ReadCriticalChanges
if SilentRunning = false then PRINT


'Build list of .inc files
if SilentRunning = false then PRINT "Finding .inc files ..."
Dim FileList(2000) As String
Dim As Integer FileListCount, SearchChip
FileListCount = 0
DataSource = Dir(IncFileDir + "\*.inc")
do While DataSource <> ""
  FileListCount += 1
  FileList(FileListCount) = Trim(LCase(DataSource))
  DataSource = Dir
Loop

'Filter list
Dim f As Integer
f = FreeFile
Open OutputDir + "\lflist.dat" For Output As #f
ChipIncCount = 0
Dim ProcessChip As Integer
Dim NonLFChip As String
For CurrentChip = 1 To FileListCount
  DataSource = FileList(CurrentChip)
  ' print DataSource
  ProcessChip = -1
  'Skip 17C, or files for multiple chips
  If InStr(LCase(DataSource), "p17c") Then ProcessChip = 0
  If InStr(LCase(DataSource), "x") Then ProcessChip = 0


  'Skip LF chips where F versions also exist
'  If InStr(LCase(DataSource), "lf") <> 0 Then
'    NonLFChip = LCase(DataSource)
'    Replace NonLFChip, "lf", "f"

    For SearchChip = 1 To FileListCount
      ' print LCase(FileList(SearchChip))," : ", NonLFChip,LCase(FileList(SearchChip)) = NonLFChip
      If LCase(FileList(SearchChip)) = NonLFChip Then

        ChipName = UCase(Left(DataSource, INSTR(DataSource, ".") - 1))
        If Left(ChipName, 1) = "P" THEN ChipName = Mid(ChipName, 2)
        ChipName = Trim(ChipName)

        If command = "" then
            ProcessChip = CompareLFFile ( LCase(FileList(SearchChip)) )  'return 0 or -1
        End If
'        ProcessChip = 0
'        Exit For
        if ProcessChip = -1 then
'            print "      Dont process chip"
            Print #f, ChipName
            Exit For
        end if

'        print "      Process chip"
      End If
    Next
'  End If

  If ProcessChip Then
    ChipIncCount += 1
    ChipIncList(ChipIncCount) = IncFileDir + "\" + Trim(LCase(DataSource))
  End If
Next
Close #f
if SilentRunning = false then PRINT

'FOR CurrentChip = StartChip to ChipIncCount
'print ChipIncList(CurrentChip)
'next

'Get start location
StartChip = 1
IF INSTR(Command, "START") <> 0 THEN StartChip = VAL(Mid(Command, 6))



'Convert .inc to .dat
if SilentRunning = false then Print "Processing ..."
FOR CurrentChip = StartChip to ChipIncCount

  SingleByteConfigElement = 0

  dim as Integer Reinit, Reinit2
  for Reinit = 0 to 1000
    DefConfig(Reinit, 0 ) =""
    DefConfig(Reinit, 1 ) =""
    DefConfig(Reinit, 2 ) =""
    ConfigOption(Reinit).Name =""
    ConfigOption(Reinit).Choices =""
    ConfigOption(Reinit).DefaultGuess =""
    Config(Reinit).Name =""
    Config(Reinit).Value =0
    Config(Reinit).Location =0
    Config(Reinit).Duplicate =0
    Config(Reinit).SettingName =""


'Type PinOut
'    Name As String
'    PinCount As Integer
'    PinList(100) As ChipPin
'End Type
'
'
'Type ChipPin
'    PinName As String
'    Number As Integer
'    FunctionList(20, 2) As String
'    FunctionCount As Integer
'End Type
'


  next

  for Reinit = 0 to 20

    PowerPin(REinit)=""
    PinOuts(ReInit).Name=""
    PinOuts(ReInit).PinCount=0
    For Reinit2 = 0 to 100
        PinOuts(ReInit).PinList(Reinit2).PinNAme=""
        PinOuts(ReInit).PinList(Reinit2).Number=0
        PinOuts(ReInit).PinList(Reinit2).FunctionCount =0
    Next

  Next


  If INKEY = Chr(27) Then
    Print "Cancelled"
    GoTo ConvertCancelled
  End If

  'Get chip name, family
  ChipName = UCase(ChipIncList(CurrentChip))
  Do WHILE INSTR(ChipName, "\") <> 0: ChipName = Mid(ChipName, INSTR(ChipName, "\") + 1): LOOP
  ChipName = Left(ChipName, INSTR(ChipName, ".") - 1)
  If Left(ChipName, 1) = "P" THEN ChipName = Mid(ChipName, 2)
  ChipName = Trim(ChipName)
  ChipFamily = 14
  If INSTR(ChipName, "C5") <> 0 OR INSTR(ChipName, "CR5") <> 0 Or InStr(ChipName, "F5") OR INSTR(ChipName, "10F") THEN ChipFamily = 12
  If INSTR(ChipName, "18F") <> 0 OR INSTR(ChipName, "18LF") <> 0 OR INSTR(ChipName, "18C") <> 0 THEN ChipFamily = 16
  If InStr(ChipName, "12F1") <> 0 Or InStr(ChipName, "16F1") <> 0 Then ChipFamily = 15
  If InStr(ChipName, "10F3") <> 0 Or InStr(ChipName, "10LF3") <> 0 Then ChipFamily = 14
  If InStr(ChipName, "12LF1") <> 0 Or InStr(ChipName, "16LF1") <> 0 Then ChipFamily = 15

    'Clear counters
    COC = 0
    SVC = 0
    SVBC = 0
    RBC = 0
    SRBC = 0
    ILC = 0

    ChipDataSuspect = 0
    MaxChipAddress = 0
    DefConf = 0
    PinoutCount = 0
    ConfigOptions = 0
    ConfigGuessed = 0
    ChipFamilyVariant = 0
    ChipMinimumBankSel = 0
    ChipSelfWrite = 0

    FOR CD = 1 to 40
      ConfigMask(CD) = 0
    Next

    IF Command <> "" AND StartChip = 1 Then
        If Right(Command, 1) = "." Then
            If UCase(ChipName) <> UCase(Left(Command, Len(Command) - 1)) Then
                Goto NotSkip
            end if
        Else
            If Left(UCase(ChipName), LEN(Command)) <> UCase(Command) THEN
                GOTO NotSkip
            end if
        End If

    End If
    Print SPC(5); ChipName; " ("; Str(ChipFamily); ") "; Str(CurrentChip); "/"; Str(ChipIncCount)

    'Read .inc
    OPEN ChipIncList(CurrentChip) For input as #1

    'Get number of config words, and check for PSP
    PSP = 0
    SEEK #1, 1
    ConfWords = 1
    HandlingAddingLine = 0
    DO WHILE NOT EOF(1)
        'LINE INPUT #1, DataSource
        ReadIncFileLineIn
        DataSource = UCase(LTrim(DataSource))
'print "1";DataSource
        IF Left(DataSource, 7) = "_CONFIG" THEN
            DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": LOOP
            DataSource = Left(Mid(DataSource, 8), 1)
            IF VAL(DataSource) > ConfWords THEN ConfWords = VAL(DataSource)
        END IF

        IF Left(DataSource, 8) = "__MAXRAM" THEN
            MaxRam = Mid(DataSource, INSTR(DataSource, " ") + 1)
            Replace MaxRam, "H'", ""
            Replace MaxRam, "'", ""
            MaxRam = Trim(MaxRam)
            MaxChipAddress = VAL("&H" + MaxRam)
            
        END IF

        IF INSTR(DataSource, "PSPMODE") <> 0 THEN PSP = 1

    LOOP

    'Find valid CONFIG options
    IF ChipFamily <> 16 THEN
        SEEK #1, 1
        RC = 0
        COC = 0
        CW = 1
        DO WHILE NOT EOF(1)
            ReadIncFileLineIn
'print "2";DataSource
            ' LINE INPUT #1, DataSource
            IF INSTR(DataSource, "Bits") <> 0 THEN RC = 0
            IF INSTR(DataSource, "Register") <> 0  and INSTR(DataSource, "_") = 0   THEN RC = 0  'searching for 'Register File'
            IF INSTR(DataSource, "Configuration Bits") <> 0 THEN RC = 1
            If InStr(DataSource, "CONFIG") <> 0 And InStr(DataSource, "Options") <> 0 Then

              RC = 1
                IF INSTR(DataSource, "1") <> 0 THEN CW = 1
                IF INSTR(DataSource, "2") <> 0 THEN CW = 2
                IF INSTR(DataSource, "3") <> 0 THEN CW = 3
                IF INSTR(DataSource, "4") <> 0 THEN CW = 4
                IF INSTR(DataSource, "5") <> 0 THEN CW = 5
                IF INSTR(DataSource, "6") <> 0 THEN CW = 6

            End If

            '"PROTECTED" deals with issue reading p16F19156 file, otherwise comment containing "Configuration Word write-protected" upsets CW
'            IF INSTR(DataSource, ";") <> 0 AND INSTR(UCase(DataSource), "CONFIGURATION") <> 0 And INSTR(UCase(DataSource), "PROTECTED") =0 AND RC = 1 And ConfWords > 1 Then
'                IF INSTR(DataSource, "1") <> 0 THEN CW = 1
'                IF INSTR(DataSource, "2") <> 0 THEN CW = 2
'                IF INSTR(DataSource, "3") <> 0 THEN CW = 3
'                IF INSTR(DataSource, "4") <> 0 THEN CW = 4
'                IF INSTR(DataSource, "5") <> 0 THEN CW = 5
'                IF INSTR(DataSource, "6") <> 0 THEN CW = 6
'            END IF

            IF RC = 1 AND INSTR(UCase(DataSource), " EQU ") <> 0 THEN

                DSO = DataSource
                DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": LOOP
                DataSource = Trim(DataSource)

                BitVal = Trim(Mid(DataSource, INSTR(DataSource, " EQU ") + 5))
                DataSource = Left(DataSource, INSTR(DataSource, " EQU") - 1)
                IF Left(DataSource, 1) = "_" THEN DataSource = Mid(DataSource, 2)

                BitVal = Mid(BitVal, INSTR(BitVal, "H'") + 2)
                BitVal = Left(BitVal, INSTR(BitVal, "'") - 1)

                IF Left(UCase(DataSource), 6) <> "CONFIG" THEN
                    FOR T = 1 to COC
                        IF IsSameConfig(Trim(DataSource), Config(T).Name) Then
                          'Print "Found duplicate config: ", Trim(DataSource), Config(T).Name
                          GOTO ConfIsDup
                        End If
                    Next
                    COC += 1
                    With Config(COC)
                        .Name = Trim(DataSource)
                        .Location = CW
                        .Value = VAL("&H" + BitVal)
                        .Duplicate = 0
                        IF INSTR(UCase(DSO), "BACKWARDS") <> 0 Then .Duplicate = -1
                    End With
                END IF
            END IF

      ConfIsDup:

        LOOP
    END IF

    IF ChipFamily = 16 THEN
        Dim WriteConfig as Integer = -1
        SEEK #1, 1
        COC = 0
        RC = 0
        DO WHILE NOT EOF(1)
            ReadIncFileLineIn
            
            'LINE INPUT #1, DataSource
            Do While INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), "": LOOP
            DataSource = Trim(UCase(DataSource))
            'RC: 0 - disable read, 1 - enable read, 2 - disable read for one line only

            IF INSTR(DataSource, "CONFIG") <> 0 AND INSTR(DataSource, "OPTIONS") <> 0 Then RC = 1
            IF INSTR(DataSource, "IDLOCS") <> 0 THEN RC = 0
            IF INSTR(DataSource, "DEVID") <> 0 THEN RC = 0
            IF Left(DataSource, 7) = "_CONFIG" THEN
              RC = 2
              'Q43 fix for BYTES as config addresses!!!!  LOOK no H OR L on the config!!
              '_CONFIG1        EQU  H'300000'
              IF Trim(Left(DataSource, 10)) = "_CONFIG1" THEN
                  If SingleByteConfigElement = 0 then Print "Transforming Config from Bytes to Words"
                  SingleByteConfigElement = -1

              end if

            end if

            IF RC = 1 AND INSTR(DataSource, " EQU ") <> 0 AND Left(DataSource, 1) <> ";" Then

                IF INSTR(DataSource, ";") <> 0 THEN
                  DataSource = Left(DataSource, INSTR(DataSource, ";") - 1)
                END IF
                BitVal = Trim(Mid(DataSource, INSTR(DataSource, " EQU ") + 5))

                DataSource = Trim(Left(DataSource, INSTR(DataSource, " EQU ") - 1))

                TempData = DataSource
                IF Left(TempData, 1) = "_" THEN

                  TempData = Mid(TempData, 2)

                end if

                Replace BitVal, "H'", ""
                Replace BitVal, "'", ""
                CurrConfWord = ""

                FOR T = LEN(TempData) To 2 Step -1
                    IF Mid(TempData, T, 1) = "_" Then CurrConfWord = Mid(TempData, T + 1): TempData = Left(TempData, T - 1): Exit For
                NEXT
                IF CurrConfWord <> "" THEN
                    'fix for Q43s
                    IF SingleByteConfigElement = 0 then
                        CW = VAL(CurrConfWord) * 2
                        IF INSTR(CurrConfWord, "L") <> 0 THEN CW -= 1
                    ELSE
                        CW = VAL(CurrConfWord)
                    END IF
                END IF

                

                If Instr(chipname,"Q20") <> 0 then
                      'Q20 dump config 11 or above as GCBASIC does not support
                      If CW > 12 or WriteConfig = 0 Then
                        WriteConfig = 0
                        Print "18FxxQ20 - ignoring CONFIG "+str(CW)+ " none contiguous config memory",
                        Print DataSource
                      End if
                End IF

                If WriteConfig = -1 then
                  COC += 1
                  With Config(COC)
                      .Name = TempData
                        If Instr(chipname,"Q20") <> 0 then
                          'Q20 have non-contigous config memory... so, fix by delete the value by 1
                          If CW > 9 Then
                            CW = CW -1 
                          End if
                        End IF
                      .Location = CW
                      .Value = VAL("&H" + BitVal)
                  End With
                End If

            END IF

            IF RC = 2 THEN RC = 1

        LOOP
    END IF

    'Find data memory reserved for system variables
    SEEK #1, 1
    DisableRead = 1
    DO WHILE NOT EOF(1)
        ReadIncFileLineIn
'print "4";DataSource


        'LINE INPUT #1, DataSource
        IF INSTR(DataSource, "Register Files") <> 0 THEN DisableRead = 0
        IF INSTR(DataSource, "Bits") <> 0 AND INSTR(DataSource, "-----") <> 0 THEN DisableRead = 1
        IF INSTR(UCase(DataSource), "EQU") <> 0 AND INSTR(UCase(DataSource), "RESERVED") = 0 AND DisableRead = 0 THEN
            DO WHILE INSTR(DataSource, Chr(9)) <> 0
                Replace DataSource, Chr(9), " "
            LOOP
            TempData = DataSource
            VarLoc = Mid(TempData, INSTR(TempData, "H'") + 2)
            VarLoc = Left(VarLoc, LEN(VarLoc) - 1)
            TempData = Left(TempData, INSTR(TempData, " ") - 1)
            IF INSTR(VarLoc, "'") <> 0 THEN VarLoc = Left(VarLoc, INSTR(VarLoc, "'") - 1)
            SVC = SVC + 1
            SysVars(SVC, 1) = UCase(Trim(TempData))
            SysVars(SVC, 2) = VarLoc
        END IF
    LOOP

    'Find bits of SFRs
    SEEK #1, 1
    CurrentVar = ""
    SVBC = 0
    DO WHILE NOT EOF(1)
        ReadIncFileLineIn
'print "5";DataSource
        'LINE INPUT #1, DataSource
        DO WHILE INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), "": LOOP
        CVNC = 0
        DataSource = UCase(Trim(DataSource))

        IF INSTR(DataSource, "-") <> 0 OR INSTR(DataSource, "=") <> 0 THEN CurrentVar = ""

        IF INSTR(DataSource, "BITS") <> 0 AND INSTR(DataSource, "CONFIGURATION") = 0 Then

            Replace DataSource, ";", ""
            Replace DataSource, "BITS", ""
            Do While INSTR(DataSource, "-") <> 0
                Replace DataSource, "-", ""
            Loop
            CurrentVar = Trim(DataSource)
            If InStr(CurrentVar, " ") = 0 Then
              CVNC = 1
            Else
              CurrentVar = ""
              CVNC = 0
            End If

        END IF

        IF INSTR(DataSource, ";") <> 0 AND INSTR(DataSource, "-") <> 0 AND CVNC = 0 THEN
            FOR T = 1 TO SVC
                TempData = SysVars(T, 1)
                IF WholeINSTR(UCase(DataSource), TempData) = 2 THEN
                    CurrentVar = TempData
                    EXIT FOR
                END IF
            NEXT
        END IF

        If INSTR(DataSource, "EQU") <> 0 AND CurrentVar <> "" AND Left(DataSource, 1) <> ";" THEN
            BitName = Trim(Left(DataSource, INSTR(DataSource, "EQU") - 1))
            BitLoc = Trim(Mid(DataSource, INSTR(DataSource, "EQU") + 3))
            BitLoc = Mid(BitLoc, INSTR(BitLoc, "H'") + 2)
            IF INSTR(BitLoc, "'") <> 0 THEN BitLoc = Left(BitLoc, INSTR(BitLoc, "'") - 1)

            'Patches for errors in .inc files
            If CurrentVar = "OSCCON" Then
              If BitName = "IRFC3" Then BitName = "IRCF3"
            ElseIf CurrentVar = "ADCON0" Then
              If BitName = "GO_NOT_DONE" Then
                'Add GO_DONE bit as well
                SVBC = SVBC + 1
                  With SysVarBits(SVBC)
                      .Name = "GO_DONE"
                      .OldName = "GO_DONE"
                      .Reg = CurrentVar
                      .Bit = Val("&H" + BitLoc)
                  End With
              End If
            End If

            If CurrentVar <> "ECANCON" Or BitName <> "F" Then
                SVBC = SVBC + 1
                With SysVarBits(SVBC)
                    .Name = BitName
                    .OldName = BitName
                    .Reg = CurrentVar
                    .Bit = Val("&H" + BitLoc)
                End With
            End If
        End IF

    LOOP

    CLOSE

    'Check for duplicate names of SFRs/SFR bits
    For CheckSFR = 1 to SVC
        TempData = SysVars(CheckSFR, 1)
        For CheckSFR2 = CheckSFR + 1 to SVC
            If TempData = SysVars(CheckSFR2, 1) Then
                'Print "Conflicting SFR name: "; TempData
                'Rename var
                SysVars(CheckSFR2, 1) = SysVars(CheckSFR2, 1) + SysVars(CheckSFR2, 2)
            End If
        Next
        For CheckSFRBit = 1 to SVBC
            If TempData = UCase(Trim(SysVarBits(CheckSFRBit).Name)) Then
                'Print "Conflicting SFR/bit name: "; TempData
                'Rename bit
                With SysVarBits(CheckSFRBit)
                    .Name = .Reg + "_" + .Name
                End With
            End If
        Next
    Next
    For CheckSFR = 1 to SVBC
        TempData = UCase(Trim(SysVarBits(CheckSFR).Name))
        For CheckSFRBit = CheckSFR + 1 to SVBC
            If TempData = UCase(Trim(SysVarBits(CheckSFRBit).Name)) Then
                #define SFRB1 SysVarBits(CheckSFR)
                #define SFRB2 SysVarBits(CheckSFRBit)

                If SFRB1.Reg = "EECON1" Then
                    SFRB2.Name = SFRB2.Reg + "_" + SFRB2.Name
                ElseIf SFRB1.Reg = "SSP1CON2" Then
                    SFRB2.Name = SFRB2.Reg + "_" + SFRB2.Name
                ElseIf SFRB2.Reg = "EECON1" Then
                    SFRB1.Name = SFRB1.Reg + "_" + SFRB1.Name
                ElseIf SFRB2.Reg = "STATUS" Then
                  SFRB1.Name = SFRB1.Reg + "_" + SFRB1.Name
                ElseIf SFRB1.Bit = SFRB2.Bit Then
                    SFRB2.Name = SFRB2.Reg + "_" + SFRB2.Name
                ElseIf SFRB1.Bit <> SFRB2.Bit Then
                    SFRB1.Name = SFRB1.Reg + "_" + SFRB1.Name
                Else
                    Print "Unresolved bit name conflict:"
                    Print SFRB1.Name, SFRB1.Reg, SFRB2.Name, SFRB2.Reg
                End If

            End If
        Next
    Next

   'Obtain chip data
   ThisChipData = ""
   ADCHelperStr=""
   ChipNameTemp = ChipName
   GetChipDataAgain:
   If Left(ChipNameTemp, 2) = "RF" Then
    ChipNameTemp = "12F" + Left(Mid(ChipNameTemp, 3), 3)
   End IF
   FOR PD = 1 to ChipDataCount
    TempData = UCase(ChipData(PD))
    If Left(TempData, 3) = "PIC" THEN TempData = Mid(TempData, 4)
    If Left(Trim(TempData), LEN(ChipNameTemp)) = UCase(ChipNameTemp) THEN
      ThisChipData = TempData
      Exit For
    End IF
   Next
   If ThisChipData = "" Then
    If InStr(ChipNameTemp, "LF") <> 0 Then
      Replace ChipNameTemp, "LF", "F"
      GoTo GetChipDataAgain
    End If

    'No data, so file suspect
    Print "Missing data in chipdata.csv"
    ChipDataSuspect = -1

   Else
    TempData = ThisChipData

    TempData = Mid(TempData, INSTR(TempData, ",") + 1)
    ChipNotTested = VAL(TempData)

    TempData = Mid(TempData, INSTR(TempData, ",") + 1)
    ChipSubFamily = VAL(TempData)

    TempData = Mid(TempData, INSTR(TempData, ",") + 1)

    ChipWords = VAL(TempData)
    If ChipWords = -1 Then
      Print "Ghost chip, never manufactured"
      ChipDataSuspect = -1


    Else
      ProgMemRowSizeMandate = 0

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipEEPROM = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      HEFMemory = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      SAFMemory = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ProgMemRowSize = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipRAM = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipIO = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipADC = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipMHz = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipIntOsc = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipUSART = VAL(TempData)

      'PSP
      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ChipPins = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      PWMTimerVariant = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      FixedConstants = trim(left(TempData, INSTR(TempData, ",") - 1))

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      SMTClockSourceVariant = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ConfigBaseLocVariant = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      IntOSCCONFormatVariant = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      READAD10BITFORCEVariant = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      Stacks = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      IDAddress = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      IDLength = VAL(TempData)


      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      NoBankRAMStr = trim(left(TempData, INSTR(TempData, ",") - 1))

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      _31kSupportValueStr = trim(left(TempData, INSTR(TempData, ",") - 1))
      'added to handle expansion of 31ksupport
      if len( trim(_31kSupportValueStr )) <> 0 then
          _31kSupportValueExpandedStr = _31kSupportValueStr
          do while instr( _31kSupportValueExpandedStr, "|" ) > 0
            _31kSupportValueExpandedStr = ReplaceStr ( _31kSupportValueExpandedStr, "|", ",")
          loop
      end if



      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      SectorRAMAddress = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      SectorRAMAddressSize = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      WriteFlashBlockSize = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      EraseFlashBlockSize = VAL(TempData)

      TempData = Mid(TempData, INSTR(TempData, ",") + 1)
      ADCHelperStr = Left(TempData, INSTR(TempData, ",") - 1)

      If CheckBit("CHSN3") and ChipADC = 11 And ChipPins = 28 Then
        'set to AN13
        Replace  ADCHelperStr, "34", Str(13)
        Replace  ADCHelperStr, "34", Str(13)
      ElseIf CheckBit("CHSN3") and ChipADC = 14 And ChipPins = 40 Then
        'set to AN21
        Replace  ADCHelperStr, "34", Str(21)
        Replace  ADCHelperStr, "34", Str(21)
      ElseIF instr(ADCHelperStr, "XX") <> 0 Then
        'print ADCHelperStr,
        'Replace  ADCHelperStr, "XX", Str(ChipADC)
        'print ADCHelperStr, Str(ChipADC)
      
          ADCHelperStr = Replacestr (ADCHelperStr, "XX", Str(ChipADC))
          ADCHelperStr = Replacestr (ADCHelperStr, "XX", Str(ChipADC))
      ElseIF instr(ADCHelperStr, "34") <> 0 Then
          Replace  ADCHelperStr, "34", Str(ChipADC)
          Replace  ADCHelperStr, "34", Str(ChipADC)
      End If

    End If

    'Correct chip data
    If ChipWords = 0.256 Then
      ChipWords = 0.25
    ElseIf ChipWords = 0.512 Then
      ChipWords = 0.5
    End If
'    remove hard c0dee value!
'    If ChipNameTemp = "10F320" Then
'      ChipRAM = 32
'    End If

   End IF

   'Calculate free RAM blocks
   CalcRamBlocks

   'Calculate internal oscillator speeds
   CalcIntOscSpeeds

   'Find config options
    ConfigOptions = 0
    FOR PD = 1 to COC

        If Config(PD).Duplicate THEN GOTO ConfigSettingBad

        TempData = UCase(Config(PD).Name)
        ConfName = Trim(Left(TempData, INSTR(TempData, "_") - 1))
        ConfValue = Trim(Mid(TempData, INSTR(TempData, "_") + 1))

        IF INSTR(TempData, "INTRC") <> 0 OR INSTR(TempData, "EXTRC") <> 0 OR TempData = "EXTCLK" THEN
            ConfName = "OSC"
            ConfValue = TempData
            TempData = ""
        END IF

        IF ConfValue = "OSC" THEN
            Swap ConfName, ConfValue
        END IF

        IF INSTR(TempData, "OSC") <> 0 AND INSTR(TempData, "ON") = 0 AND INSTR(TempData, "OFF") = 0 AND _
          INSTR(TempData, "CPUDIV") = 0 And InStr(TempData, "SOSCSEL") = 0 And InStr(TempData, "WDTCCS") = 0 And _
          INSTR(TempData, "FEXTOSC") = 0 And InStr(TempData, "RSTOSC") = 0 Then
            ConfName = "OSC"
            ConfValue = TempData
            TempData = ""
        END IF

        IF ConfName = "" AND INStr(ConfValue, "BOR") <> 0 THEN
            ConfName = "BORV"
            Replace ConfValue, "BOR", ""
            Replace ConfValue, "V", ""
        END IF

        IF Left(ConfValue, 4) = "OSC_" THEN ConfValue = Mid(ConfValue, 5)
        IF Left(ConfValue, 5) = "FOSC_" THEN ConfValue = Mid(ConfValue, 6)
        If Right(ConfValue, 4) = "_OSC" Then ConfValue = Left(ConfValue, Len(ConfValue) - 4)

        If ConfName <> "" Then
          Config(PD).SettingName = ConfName
          DL = 0
          FOR CD = 1 to ConfigOptions
             If ConfigOption(CD).Name = ConfName THEN DL = CD: EXIT FOR
          Next
          IF DL <> 0 THEN
              If WholeINSTR(ConfigOption(DL).Choices, ConfValue) <> 2 Then ConfigOption(DL).Choices = ConfigOption(DL).Choices + "," + ConfValue
          END IF
          IF DL = 0 THEN
              ConfigOptions += 1
              With ConfigOption(ConfigOptions)
                .Name = ConfName
                .Choices = ConfValue
                .DefaultGuess = ""
              End With
              DL = ConfigOptions
          END If

          'Possible default?
          If Config(PD).Value = 255 Then
            'At this point the string in the DefaultGuess is correct
            ConfigOption(DL).DefaultGuess = ConfValue

          End If
        End If
        ConfigSettingBad:
    NEXT

    'Open .dev file
    ChipNameTemp = UCase(ChipName)
    If Left(ChipNameTemp, 2) = "RF" Then
        ChipNameTemp = "12F" + Left(Mid(ChipNameTemp, 3), 3)
    END IF
    DevFileName = DevFileDir + "\pic" + ChipNameTemp + ".dev"
    DFNameChanged:
    IF DIR(DevFileName) = "" THEN
        If Left(ChipName, 2) = "18" Then
            IF INSTR(DevFileName, "1.dev") <> 0 THEN Replace DevFileName, "1.dev", "0.dev": GOTO DFNameChanged
            IF INSTR(DevFileName, "2.dev") <> 0 THEN Replace DevFileName, "2.dev", "1.dev": GOTO DFNameChanged
            IF INSTR(DevFileName, "3.dev") <> 0 THEN Replace DevFileName, "3.dev", "2.dev": GOTO DFNameChanged
        Else
            IF INSTR(DevFileName, "16C84") <> 0 THEN Replace DevFileName, "16C84", "16F84": GOTO DFNameChanged
            IF INSTR(DevFileName, "16C61") <> 0 THEN Replace DevFileName, "16C61", "16C62": GOTO DFNameChanged
            IF INSTR(DevFileName, "a.dev") = 0 THEN Replace DevFileName, ".dev", "a.dev": GOTO DFNameChanged
        End If
        'Print "Skipping dev file "; DevFileName
        GOTO SkipDevFile
    END IF

    ReadDevFiles DevFileName
    SkipDevFile:

    ReadXmlFiles ChipName

    ReadMPASMInfo

    'Make sure max address set
    If MaxChipAddress = 0 Then
      If ChipFamily = 14 Then
        MaxChipAddress = 511
      ElseIf ChipFamily = 16 Then
        MaxChipAddress = 4095
      Else
        Print "Warning: Max chip data address not set"
      End If
    End If

  'Guess default config
  GuessDefaultConfig

  'If no pinout found, guess it
  If PinoutCount = 0 Then
    GuessChipPinout
  End If

  'Guess alternate pin functions
  GuessPinAltFunctions ChipPins

  'Find chips with OPTION_REG and bits from OPTION, change bit to be from OPTION
  If HasSFR("OPTION_REG") Then
    For PD = 1 to SVBC
      With SysVarBits(PD)
        If .Reg = "OPTION" Then
          .Reg = "OPTION_REG"
          'Print "Replaced OPTION with OPTION_REG in bit " + .Name
        End If
      End With
    Next
  End If

    'Process default config options
    IF Left(ChipName, 2) = "18" THEN
        'Sort list
        TidyConfList:
        T = 0
        FOR PD = 1 to DefConf - 1

            IF DefConfig(PD, 0) > DefConfig(PD + 1, 0) THEN
                T = 1
                TempData = DefConfig(PD, 0)
                DefConfig(PD, 0) = DefConfig(PD + 1, 0)
                DefConfig(PD + 1, 0) = TempData
            END IF
        NEXT
        IF T = 1 THEN GOTO TidyConfList

        'Remove duplicates
        T = 0
        RemDupConf:
        T = T + 1
'        print mid(DefConfig(T, 0),1,INstr(DefConfig(T, 0),"_" )-1) , mid(DefConfig(T + 1, 0),1,INstr(DefConfig(T + 1, 0),"_" )-1)
            IF DefConfig(T, 0) = DefConfig(T + 1, 0)  or mid(DefConfig(T, 0),1,INstr(DefConfig(T, 0),"_" )-1) = mid(DefConfig(T + 1, 0),1,INstr(DefConfig(T + 1, 0),"_" )-1) THEN
                FOR CD = T To DefConf - 1
                    DefConfig(CD, 0) = DefConfig(CD + 1, 0)
                NEXT
                DefConfig(DefConf, 0) = ""
                DefConf -= 1
            END IF
        IF T < DefConf THEN Goto RemDupConf

        'Process
        FOR PD = 1 to DefConf
            TempData = Trim(DefConfig(PD, 0))

            if ( Instr(TempData, "$") = 0 ) then
              'the orginal method where we assume the name does not have '_'
              DefConfig(PD, 1) = Left(TempData, INSTR(TempData, "_") - 1)
              DefConfig(PD, 2) = Mid(TempData, INSTR(TempData, "_") + 1)
            Else
              'the orginal method where we assume the name has a '$' Delimiter
              DefConfig(PD, 1) = Left(TempData, INSTR(TempData, "$") - 1)
              DefConfig(PD, 2) = Mid(TempData, INSTR(TempData, "$") + 1)
            end if

        NEXT
    END IF

    'Add in any data known to be missing from source files
    AddMissingData

  'If no config settings, data is suspect
  If ConfigOptions = 0 Then ChipDataSuspect = -1

    'Get interrupt bits
    ILC = 0
    FOR PD = 1 to SVBC
        With SysVarBits(PD)
            If Left(.Reg, 3) = "PIE" or Left(.Reg, 6) = "INTCON" And Right(.OldName, 1) = "E" And .OldName <> "GIE" And .OldName <> "PEIE" Then
                IntName = GetIntName(.OldName)

                If IntName <> "" Then
                    IntFound = 0
                    For FindInt = 1 to ILC
                        If IntList(FindInt).EventName = IntName Then
                            IntFound = FindInt
                            Exit For
                        End If
                    Next

                    If IntFound = 0 Then
                        If INSTR(.OldName, "_PIE") <> 0 Then
                            IntFlag = .OldName
                            Mid(IntFlag, INSTR(IntFlag, "_") - 1) = "F"
                            'IntFlag = Left(.OldName, INSTR(.OldName, "_") - 1) + "F"
                        Else
                            IntFlag = Left(.OldName, Len(.OldName) - 1) + "F"
                        End If
                        If Not CheckBit(IntFlag) Then Print "Bad bit name:", IntFlag

                        ILC += 1
                        IntList(ILC).EventName = IntName
                        IntList(ILC).EnableBit = .OldName
                        IntList(ILC).FlagBit = IntFlag
                        'Print IntName, .Name, IntFlag

                    End If
                End If



            End If
        End With
    NEXT

    'added Jan 2017
    '
    if HasSFR("IOCAF") or HasSFR("IOCBF") or HasSFR("IOCCF") or HasSFR("IOCDF") or HasSFR("IOCEF") then
      ILC += 1
      IntList(ILC).EventName = "PortChange"
      IntList(ILC).EnableBit = "IOCIE"
      IntList(ILC).FlagBit = "IOCIF"

      elseif HasSFRbit("RABIE")  or HasSFRbit("RABIF")  then
                  ILC += 1
                  IntList(ILC).EventName = "PortChange"
                  IntList(ILC).EnableBit = "RABIE"
                  IntList(ILC).FlagBit = "RABIF"
        elseif HasSFRbit("RBIE")  then
            ILC += 1
            IntList(ILC).EventName = "PortChange"
            IntList(ILC).EnableBit = "RBIE"
            IntList(ILC).FlagBit = "RBIF"

          elseif HasSFRbit("RAIE")  then
            ILC += 1
            IntList(ILC).EventName = "PortChange"
            IntList(ILC).EnableBit = "RAIE"
            IntList(ILC).FlagBit = "RAIF"
          End if

    'Sort interrupt list
    Do
        SortMore = 0
        For PD = 2 to ILC
            If IntList(PD - 1).EventName > IntList(PD).EventName Then
                Swap IntList(PD - 1), IntList(PD)
                SortMore = -1
            End If
        Next
    Loop While SortMore

    'Write to output file
    If ChipDataSuspect Then
      Print "Chip data file is suspect"
      OPEN OutputDir + "\suspect\" + LCase(ChipName) + ".dat" for output as #1
      kill (LCase(ChipName) + ".dat")
    Else
      OPEN OutputDir + "\" + LCase(ChipName) + ".dat" for output as #1
      kill ( OutputDir + "\suspect\" + LCase(ChipName) + ".dat" )
    End If

    Print #1, "'GCBASIC/GCGB Chip Data File"
    PRINT #1, "'Chip: " + ChipName
'    PRINT #1, "'Generated " + CurrentDate
    PRINT #1, "'Main Format last revised:   14/07/2017"
    PRINT #1, "'Header Format last revised: 22/05/2021"
    PRINT #1, ""

    Print #1, "[ChipData]"
    Print #1, "';All items in the ChipData section are available to user programs as constants"
    Print #1, "';The constants have the prefix of Chip: See the examples below"
    Print #1, ""
    
    If ChipNotTested = 1 Then
        Print #1, "'This chip has not been tested or validated.  This is a development DAT file"
        Print #1, "NotTested=1"
        Print #1, ""
    End If

    Print #1, "'This constant is exposed as ChipWORDS"
    Print #1, "Prog=" + Str(ChipWords * 1024)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipEEPROM"
    Print #1, "EEPROM=" + Str(ChipEEPROM)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipRAM"
    Print #1, "RAM=" + Str(ChipRAM)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipIO"
    Print #1, "I/O=" + Str(ChipIO)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipADC"
    Print #1, "ADC=" + Str(ChipADC)
    PRINT #1, ""


    IF ChipADC > 0 and ADCHelperStr <> "" then
      Print #1, "'These constants are the valid ADC constants"
      Print #1, "ADCConstants=" + ADCHelperStr
      Print #1, ""  
    End If


    Print #1, "'This constant is exposed as ChipMhz"
    Print #1, "MaxMHz=" + Str(ChipMHz)
    PRINT #1, ""
    Print #1, "'This constant is exposed with only the first parameter (if more than one)"
    Print #1, "IntOsc=" + ChipIntOscSpeeds

    IF _31kSupportValueStr <> "" then
      Print #1, ""
      Print #1, "'31kSupport is exposed as Chip31Kconfig, Chip31Kregister, Chip31KValue"
      Print #1, "31kSupport=" + _31kSupportValueExpandedStr
    End If


    'Hack for 10F DIP pinout
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipPins"
    If ChipPins = 6 Then
      Print #1, "Pins=8"
    Else
      Print #1, "Pins=" + Str(ChipPins)
    End If
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipFamily"
    Print #1, "Family=" + Str(ChipFamily)
    If ChipFamilyVariant <> 0 Then
      PRINT #1, ""
      Print #1, "'This constant is exposed as ChipFamilyVariant"
      Print #1, "FamilyVariant=" + Str(ChipFamilyVariant)
    End If

    Print #1, ""
    Print #1, "'This constant is exposed as ChipSubFamily"
    If ChipSubFamily = 0 Then
        Print #1, "SubFamily=" + Str(ChipFamily)+"000"
    else
        Print #1, "SubFamily=" + Str(ChipSubFamily)
    end if


    IF ChipFamily = 16 Then
        If Instr(chipname,"Q40") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if
        If Instr(chipname,"Q41") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if
        If Instr(chipname,"Q43") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if
        If Instr(chipname,"Q10") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMCON2"
        End if
        If Instr(chipname,"K40") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMCON2"
        End if        
        If Instr(chipname,"Q83") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if
        If Instr(chipname,"Q84") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if
        If Instr(chipname,"Q71") <> 0 then
            Print #1, ""
            Print #1, "'This constant is exposed as ChipMemorylock"
            Print #1, "MemoryLock=NVMLOCK"
        End if


    End if


    IF ChipFamily <> 16 Then
        PRINT #1, ""
        Print #1, "'This constant is exposed as ChipConfWords"
        Print #1, "ConfigWords=" + Str(ConfWords)
    END if
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipPSP"
    Print #1, "PSP=" + Str(PSP)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipUSART"
    Print #1, "USART=" + CountUSART ' Str(ChipUSART)
    PRINT #1, ""
    Print #1, "'This constant is exposed as ChipMaxAddress"
    Print #1, "MaxAddress=" + Str(MaxChipAddress)

    Print #1, ""

    Print #1, "';Microcontroller specific configuration Constants used in specific libraries, compiler or user programs"
    Print #1, "';This section of constants is specific to a microcontroller, so the constants may not be present for all microcontrollers"

    IF PWMTimerVariant <> 0 then
      Print #1, ""
      Print #1, "'ChipPWMTimerVariant constant is used in pwmh and provides information for the timer register used for PWM"
      Print #1, "PWMTimerVariant=" + Str( PWMTimerVariant )
    End If

    IF FixedConstants <> "" then

        if instr(FixedConstants, "|" ) = 0 then
            Print #1, ""
            Print #1, "'Chip"+FixedConstants+ " constant is used within the compiler or libaries to control functionality. Value may be decimal representation of a hex number."
            Print #1, FixedConstants
        else
            FixedConstants = FixedConstants + "|"
            do while instr(FixedConstants, "|") <> 0
                Print #1, ""
                Print #1, "'Chip"+mid( FixedConstants ,1, instr(FixedConstants, "|") - 1 )+ " constant is used within the compiler or libaries to control functionality"
                Print #1, mid( FixedConstants ,1, instr(FixedConstants, "|") - 1 )
                FixedConstants =  mid( FixedConstants , instr(FixedConstants, "|") + 1 )
            loop
        end if
    End If

    IF SMTClockSourceVariant <> 0 then
      Print #1, ""
      Print #1, "'ChipSMTClockSourceVariant constant is used in smth and provides information for the register used for SMT"
      Print #1, "SMTClockSourceVariant=" + Str( SMTClockSourceVariant )
    End If


    IF ConfigBaseLocVariant <> 0 then
      Print #1, ""
      Print #1, "'ChipConfigBaseLoc is used within compiler to control configuration base address"
      Print #1, "ConfigBaseLoc=0x" + hex( ConfigBaseLocVariant )
    End If


    IF IntOSCCONFormatVariant <> 0 then
      Print #1, ""
      Print #1, "'ChipIntOSCCONFormat is used within system.h.  This constant provides information with respect to the register used for setting the frequency"
      Print #1, "IntOSCCONFormat=" + Str( IntOSCCONFormatVariant )
    End If


    IF READAD10BITFORCEVariant <> 0 then
      Print #1, ""
      Print #1, "'ChipReadAD10BitForceVariant is used within adc.h.  This constant provides information with respect to the register used for setting the ADC operations"
      Print #1, "ReadAD10BitForceVariant=" + Str( READAD10BITFORCEVariant )
    End If


    IF HEFMemory <> 0 then
      Print #1, ""
      Print #1, "'ChipHEFMemWords is used within hefsaf.h.  This constant provides information with respect to the register used for setting the HEF operations"
      Print #1, "HEFMemWords=" + Str( HEFMemory )
      IF ProgMemRowSize = 0 Then
          Print "ProgMemRowSize missing for this part"
      end if
    End If
    IF SAFMemory <> 0 then
      Print #1, ""
      Print #1, "'ChipSAFMemWords is used within hefsaf.h.  This constant provides information with respect to the register used for setting the SAF operations"
      Print #1, "SAFMemWords=" + Str( SAFMemory )
      IF ProgMemRowSize = 0 Then
          Print "ProgMemRowSize missing for this part"
      end if
    End If
    IF ProgMemRowSize <> 0 then
      Print #1, ""
      Print #1, "'ChipsEraseRowSizeWords is used within hefsaf.h.  This constant provides information with respect to the register used for setting the HEFSAF operations"
      Print #1, "EraseRowSizeWords=" + Str( ProgMemRowSize )
    End If


    IF Stacks <> 0 then
      Print #1, ""
      Print #1, "'ChipStacks constant can be used in user programs and provides the available stack depth"
      Print #1, "Stacks=" + Str( Stacks )
    End If


    IF IDAddress <> 0 then
      Print #1, ""
      Print #1, "'ChipUserIDAddress constant is used in user programs and provides the User ID address"
      Print #1, "UserIDAddress=" + Str( IDAddress )
    End If
    IF IDLength <> 0 then
      Print #1, ""
      Print #1, "'ChipUserIDLength constant is used in user programs to provide the length of the UserID (in words)"
      Print #1, "UserIDLength=" + Str( IDLength )
    End If

    IF SectorRAMAddress <> 0 then
      Print #1, ""
      Print #1, "'ChipSectorRAMAddress constant is used in user programs to provide the value to Sector RAM Address"
      Print #1, "SectorRAMAddress=" + Str( SectorRAMAddress )
    End If

    IF SectorRAMAddressSize <> 0 then
      Print #1, ""
      Print #1, "'ChipSectorRAMAddressSize constant is used in user programs to provide the size of the Sector RAM"
      Print #1, "SectorRAMAddressSize=" + Str( SectorRAMAddressSize )
    End If

    IF WriteFlashBlockSize <> -1 then
      Print #1, ""
      Print #1, "'WriteFlashBlocksize constant is used in user programs that write to flash memory in bytes"
      Print #1, "WriteFlashBlockSize=" + Str( WriteFlashBlockSize )
    End If

    IF EraseFlashBlockSize <> -1 then
      Print #1, ""
      Print #1, "'EraseFlashBlockSize constant is used in user programs that write to flash memory in bytes"
      Print #1, "EraseFlashBlockSize=" + Str( EraseFlashBlockSize )
    End If



    IF ChipMinimumBankSel <> 0 then
      Print #1, ""
      Print #1, "'ChipMinimumBankSelelect constant is used within the compiler to set the minimum BANKSELECT"
      Print #1, "MinimumBankSelect=" + Str( ChipMinimumBankSel )
    End if


    Print #1, ""
    Print #1, "'ChipSelfWrite constant is used within the compiler to indicate the chip is self write capable"
    Print #1, "SelfWrite="+str(ChipSelfWrite)

    If Instr(Ucase(chipname),"LF" ) > 0 Then
      Print #1, ""
      Print #1, "'ChipLF constant can be used within the programmer compiler to indicate the chip is a low voltage chip"
      Print #1, "LF=True"
    End if

    Print #1, ""

    

    Print #1, "[Interrupts]"
    Print #1, "'For specific details of the interrupts see the microcontroller datasheet"
    Print #1, "'The first parameter is the GCBASIC identifier used in user code to expose the specific interrupt"

    For PD = 1 to ILC
        With IntList(PD)
            Print #1, .EventName + ":" + .EnableBit + "," + .FlagBit
        End With
    Next
    Print #1, ""

    PRINT #1, "[Registers]"
    Print #1, "'For specific details of the registers see the microcontroller datasheet"
    Print #1, "'The first parameter is the GCBASIC register name used in user code to expose the specific register"
    FOR PD = 1 to SVC
        If Val("&H" + SysVars(PD, 2)) <= MaxChipAddress or ( ( instr(ucase(chipname),"16F152")<>0  or instr(ucase(chipname),"16F180")<>0  or instr(ucase(chipname),"16F171")<>0  or instr(ucase(chipname),"16F181")<>0  ) and len(chipname)= 8) Then  'PIC16F15213
            TempData = SysVars(PD, 1) + "," + Str(Val("&H" + SysVars(PD, 2)))
            PRINT #1, TempData
        Else
            Print "Skipping suspect register " + SysVars(PD, 1) + " (Address 0x" + SysVars(PD, 2) + ", max 0x" + Hex(MaxChipAddress) + ")"
        End If
    NEXT
    Print #1, ""

    PRINT #1, "[Bits]"
    Print #1, "'For details of the bits (relative to a register in terms of registerbits) see the microcontroller datasheet"
    Print #1, "'The first parameter is the GCBASIC bit name used in user code to expose the specific registerbit"
    FOR PD = 1 to SVBC
        With SysVarBits(PD)

            PRINT #1, .Name + "," + .Reg + "," + Str(.Bit)

            'add the additional _ to ensure code is consistent
            if Instr(.Name,"_") = 0 and Instr(ucase(.Reg),"1I2C") Then
              PRINT #1, .Reg +"_"+.Name+ "," + .Reg + "," + Str(.Bit)
            end if


            If Not HasSFR(.Reg) And (ChipFamily <> 12 And .Reg <> "OPTION_REG" And Left(.Reg, 4) <> "TRIS") Then
            Print "Bit " + .Name + " belongs to " + .Reg + " which is not defined"
          End If
        End With
    NEXT
    Print #1, ""

    Print #1, "[FreeRAM]"
    FOR PD = 1 to RBC
        PRINT #1, RamBlock(PD)
    NEXT
    Print #1, ""

    Print #1, "[NoBankRAM]"
    Print #1, "'NoBankRAM is somewhat misnamed - it is used for the defintion of (any) access bank locations"
    Print #1, "'If a memory location is defined in both NoBankRAM and FreeRAM, then the compiler knows that it is access bank RAM."
    Print #1, "'If an SFR location is in one of the NoBankRAM ranges, then the compiler knows not to do any bank selection when accessing that register."
    Print #1, ""
    Print #1, "'The NoBankRAM section must include two ranges, one for access bank RAM, one for access bank SFRs, or there will be issues."
    Print #1, "'The first range MUST be the ACCESS RAM range"
    Print #1, "'The first range is the FAST SFR range"

    Print #1, ""
    Print #1, "'If there are no ranges defined in NoBankRAM, the compiler will try to guess them."
    Print #1, "'On 18Fs, it will guess based on where the lowest SFR is, and from what the total RAM on the chip is. If there's only one range defined"
    Print #1, "'in the NoBankRAM locations, the compiler will assume that is the range for the RAM, and then will guess where the range for the access bank SFRs is."




    For PD = 1 To SRBC
      Print #1, SharedRamBlock(PD)
    Next
    Print #1, ""

  For PPO = 1 to PinoutCount
    With Pinouts(PPO)
      Print #1, "[Pins-" + .Name + "]"
      OscPins = 0
      MCLRPin = 0

      'List power pins, to merge with other pins
      If Not PowerPinsAdded Then
        GetPowerPins (ChipPins, 0, 0)
      End If

      'List I/O pin details
      For PD = 1 to .PinCount
        DataSource = ""
        With .PinList(PD)
          DataSource = Str(.Number)
          'Find key function
          For PF = 1 to .FunctionCount
            NewData = .FunctionList(PF, 1)
            If Len(NewData) = 3 And MID(NewData, 1, 1) = "R" Then
              If MID(NewData, 2, 1) >= "A" And MID(NewData, 2, 1) <= "Z" Then
                If MID(NewData, 3, 1) >= "0" AND MID(NewData, 3, 1) <= "9" Then
                  DataSource = DataSource + "," + .FunctionList(PF, 1)
                  If .FunctionList(PF, 2) <> "" Then DataSource = DataSource + "(" + .FunctionList(PF, 2) + ")"
                  .FunctionList(PF, 1) = ""
                End If
              End If
            End If
          Next
          'List other functions
          For PF = 1 to .FunctionCount
            If .FunctionList(PF, 1) <> "" Then
              DataSource = DataSource + "," + .FunctionList(PF, 1)
              If .FunctionList(PF, 2) <> "" Then DataSource = DataSource + "(" + .FunctionList(PF, 2) + ")"
            End If
          Next
          'Add relevant power pin functions
          For PF = 1 to PowerPinCount
            Dim As Integer CurrPowerPin
            CurrPowerPin = Val(Left(PowerPin(PF), InStr(PowerPin(PF), ",") - 1))
            If CurrPowerPin = .Number Then
              DataSource = DataSource + Mid(PowerPin(PF), InStr(PowerPin(PF), ","))
            End If
          Next

          'Check if OSC pins are found
          If INSTR(DataSource, "OSC1") <> 0 Or INSTR(DataSource, "OSC2") <> 0 Then OscPins = -1
          'Pins with MCLR are input only
          If INSTR(DataSource, "MCLR") <> 0 Then
            MCLRPin = -1
            Do While INSTR(DataSource, "(IO)") <> 0: Replace DataSource, "(IO)", "(I)": Loop
          End If
        End With
        Print #1, DataSource
      Next

      'List power pins and add to output
      If Not PowerPinsAdded Then
        GetPowerPins (ChipPins, OscPins, MCLRPin)
        For PD = 1 to PowerPinCount
          Print #1, PowerPin(PD)
        Next
      End If

      Print #1, ""
    End With
  Next

    IF Left(ChipName, 2) = "18" THEN
        Print #1, "[ASMConfig]"
        Print #1, "'The GCBASIC compiler default configuration for a specific microcontroller"

        FOR PD = 1 to DefConf
            'print DefConfig(PD, 0) + ">" +DefConfig(PD, 1) + "=" + DefConfig(PD, 2)

            'Process 18f Default file where Read18FConfigArray is size of the 18FDefaultASMConfig.TXT converted to an  array
            For ArrayPointer =  0 to Read18FConfigArray
                if instr((ucase(Read18FConfigLines( ArrayPointer)))  , (ucase(DefConfig(PD, 1)))+"=" )= 1 then

                  'PRINT #1, "'18FDefaultASMConfig Setting " '+mid( Read18FConfigLines( ArrayPointer), Instr( Read18FConfigLines( ArrayPointer), "=")+1)
                  DefConfig(PD, 2) = mid( Read18FConfigLines( ArrayPointer), Instr( Read18FConfigLines( ArrayPointer), "=")+1)
                end if
            Next

            ' PRINT #1, "'Standard Config Setting"
            ' Kill some CONFIG things that are not supported by PICKPlus.. users can uncomment in the DAT file.
            If Instr(Ucase(DefConfig(PD, 1)),"SAFLOCK") = 1 Then DefConfig(PD, 1) = "'"+DefConfig(PD, 1)
            If Instr(Ucase(DefConfig(PD, 1)),"SAFSZ") = 1 Then DefConfig(PD, 1) = "'"+DefConfig(PD, 1)
            PRINT #1, DefConfig(PD, 1) + "=" + DefConfig(PD, 2)
        NEXT


        Print #1, ""
        Print #1, "[ConfigMask]"
        FOR PD = 1 to ConfMask
            TempData = Trim(Str(ConfigMask(PD)))
            IF TempData = "0" THEN TempData = "255"
            'IF Instr ( ChipName, "18F" ) > 0 and ( Instr ( ChipName, "Q83" ) > 0 or Instr ( ChipName, "Q84" ) > 0 or Instr ( ChipName, "Q71" ) > 0 ) Then TempData = "255"
            If SingleByteConfigElement = 0 then
              ' all the Chips that need the bytes converted to WORDS
              PRINT #1, TempData
            Else
              PRINT #1, 255
            End If
        NEXT
        Print #1, ""
    END IF

    Print #1, "[ConfigOps]"
    Print #1, "'For details of the config options see the microcontroller datasheet"
    Print #1, "'The first parameter is the configuration field that can be used to expose specific configuration bits"
    Print #1, "'The other parameters are the configuration field options that can be set"

    FOR PD = 1 to ConfigOptions
        PRINT #1, ConfigOption(PD).Name + "=" + ConfigOption(PD).Choices
    NEXT
    Print #1, ""

    Print #1, "[Config]"
    Print #1, "'For details of the config addresses see the microcontroller datasheet"
    If SingleByteConfigElement = -1 then Print #1, "'CONFIG Single Byte config elements have been transformed into CONFIG Words for GCBASIC compiler"


    FOR PD = 1 TO COC
        With Config(PD)
            PRINT #1, .Name + "," + STR(.Location) + "," + STR(.Value)
        End With
    NEXT
    Print #1, ""

    Close
    Goto NotSkip

    SkipChip:
    Print "Skipped"

    NotSkip:

NEXT

ConvertCancelled:
if SilentRunning = false then
  PRINT
  PRINT "Conversion finished": ', press any key to continue"
end if

'TempData = INKEY
'DO WHILE INKEY = "": Sleep 50: LOOP

END

Sub AddMissingData
  Dim As Integer PD, CD, FoundXINST, FoundSOSCSEL

  'Add family variant where needed
  ChipFamilyVariant = 0
  If ChipFamily = 12 Then
    '16F527 and 16F570 chips have some extra instructions, can detect with presence of IBSR register
    If HasSFR("IBSR") Then
      ChipFamilyVariant = 1
    End If
  ElseIf ChipFamily = 15 Then
    If HasSFRBit("BSR5") Then
      ChipFamilyVariant = 1
    ' Removed next two lines at thes 16f15325 and other chips that need ChipFamilyVariant = 1 are less than 2048 - errant warning
    ' ElseIf ChipRAM >= 2048 Then
    '  Print "Warning: BSR5 detection may have failed, movlb may be broken"
    End If
  ElseIf ChipFamily = 16 Then
    '18F chips with 4096 or more data addresses have movffl and different lfsr instruction
    If MaxChipAddress >= 4096 Then ChipFamilyVariant = 1
    If ChipSubFamily = 16102 or ChipSubFamily = 16105  Then ChipFamilyVariant = 1  'Q40 and Q41
  End If

  'Add debug bit to PICs where it doesn't exist
  'DEBUG_ON,7,127 on just about all (all?) 18Fs
  'Doesn't exist on 18F14K22 or 18F14K50, presumably all 18F1xKxx?
  If ChipFamily = 16 and instr(ChipName,"Q10") = 0  Then
    'Search for DEBUG config option
    FOR CD = 1 to ConfigOptions
      If ConfigOption(CD).Name = "DEBUG" THEN
        GoTo FoundDebugConfigBit
      End if
    Next

    'If it doesn't have any config, don't try adding 1!
    If ConfigOptions = 0 Then
      GoTo FoundDebugConfigBit
    End if

    Print "No DEBUG config bit!"

    'Add config setting
    ConfigOptions += 1
    ConfigOption(ConfigOptions).Name = "DEBUG"
    ConfigOption(ConfigOptions).Choices = "OFF,ON"

    'Add values
    'DEBUG_ON,7,127
    'DEBUG_OFF,7,255
    COC += 1
    With Config(COC)
      .Name = "DEBUG_ON"
      .Location = 7
      .Value = 127
    End With
    COC += 1
    With Config(COC)
      .Name = "DEBUG_OFF"
      .Location = 7
      .Value = 255
    End With

    'Add to default asm config
    DefConf += 1
    DefConfig(DefConf, 1) = "DEBUG"
    DefConfig(DefConf, 2) = "OFF"

    'Config mask?
    'Print #1, "[ConfigMask]"
    'For PD = 1 to ConfMask
    ' TempData = Trim(Str(ConfigMask(PD)))
    ' If TempData = "0" THEN TempData = "255"
    ' Print #1, TempData
    'Next


  End If
FoundDebugConfigBit:

  'Set default CPUDIV = none on 18F14K50
  'Possibly others?
  If Left(ChipName, 2) = "18" Then

    'Is there a CPUDIV setting?
    CD = 0
    For PD = 1 to ConfigOptions
          If ConfigOption(PD).Name = "CPUDIV" Then
            'Is there a "NOCLKDIV" option?
            If InStr(ConfigOption(PD).Choices, "NOCLKDIV") <> 0 Then
              CD = -1
            End If
            Exit For
          End If
      Next

    'NOCLKDIV option found, select it by default
    If CD Then
      'Check if already set
      FOR PD = 1 to DefConf
            If DefConfig(PD, 1) = "CPUDIV" Then
              DefConfig(PD, 2) = "NOCLKDIV"
              GoTo NoDivSet
            End If
      Next
      'Set
      DefConf += 1
      DefConfig(DefConf, 1) = "CPUDIV"
      DefConfig(DefConf, 2) = "NOCLKDIV"

      NoDivSet:
    End If
  End If

  'Set default XINST=OFF on PIC18F14K22
  '(And 29 others, or 30 total as of 25/6/2014)
  If Left(ChipName, 2) = "18" Then

    FoundXINST = 0
    FOR PD = 1 to DefConf
          If DefConfig(PD, 1) = "XINST" Then
            FoundXINST = -1
            If DefConfig(PD, 2) <> "OFF" Then
              DefConfig(PD, 2) = "OFF"
            End If
          End If
    Next

    'If no XINST default, but bit exists, add one
    If Not FoundXINST Then
      For PD = 1 to ConfigOptions
            If ConfigOption(PD).Name = "XINST" Then
              Print "Added missing XINST default (set OFF)"
              DefConf += 1
              DefConfig(DefConf, 1) = "XINST"
              DefConfig(DefConf, 2) = "OFF"
            End If
      Next
    End If

  End If

  'Set default SOSCSEL=DIG on PIC18FxxK80
  If Left(ChipName, 2) = "18" Then

    FoundSOSCSEL = 0

    FOR PD = 1 to DefConf
         ' A DefConfig(PD, 1)
          If DefConfig(PD, 1) = "SOSCSEL" Then

            FoundSOSCSEL = -1
            If DefConfig(PD, 2) <> "DIG" Then
              DefConfig(PD, 2) = "DIG"
            End If
          End If
    Next

    'If no XINST default, but bit exists, add one
    If Not FoundSOSCSEL Then
      For PD = 1 to ConfigOptions
            If ConfigOption(PD).Name = "SOSCSEL" Then
              Print "Added missing SOSCSEL default (set DIG)"
              DefConf += 1
              DefConfig(DefConf, 1) = "SOSCSEL"
              DefConfig(DefConf, 2) = "DIG"
            End If
      Next
    End If

  End If

  'search WRT
  For PD = 1 to ConfigOptions
        If LEFT(ConfigOption(PD).Name,3) = "WRT" and ( HasSFR("EECON1")  or HasSFR("NVMCON1") or   HasSFR("PMCON1") )Then
          ChipSelfWrite = 1
        End If
  Next

  'Add ADCON0.CHS bits on 18F25K50
  'Maybe others, how to detect?
  If ChipADC > 0 Then
    If HasSFR("ADCON0") Then
      'Should have CHS bits or ADCHS register
      If Not (HasSFR("ADCHS") Or HasSFRBit("CHS0")) Then
        Print "Adding CHS bits to ADCON0"
        For PD = 0 To 4
          SVBC += 1
          With SysVarBits(SVBC)
            .Name = "CHS" + Str(PD)
            .Reg = "ADCON0"
            .Bit = 2 + PD
          End With
        Next
      End If
    End If

    If HasSFR("ADCON1") and not HasSFR("ADCLK") Then
     'Should have ADCS0:2 bits
      If Not HasSFRBit("ADCS0") Then
          Print "Adding ADCS bits to ADCON1"
          For PD = 0 To 2
            SVBC += 1
            With SysVarBits(SVBC)
              .Name = "ADCS" + Str(PD)
              .Reg = "ADCON1"
              .Bit = 4 + PD
            End With
          Next
      End if
    End if
  End If

'  'Correct locations for TRISA/TRISC on 16F18326
'  If ChipName = "16F18326" Then
'    For PD = 1 To SVC
'      If SysVars(PD, 1) = "TRISA" Then
'        SysVars(PD, 2) = "8C"
'      ElseIf SysVars(PD, 1) = "TRISC" Then
'        SysVars(PD, 2) = "8E"
'      End If
'    Next
'  End If

  'Generate config masks again, stuff from .dev files may not be accurate
  CalcConfigMasks
End Sub

Sub AddPinFunction(Key As String, Port As String, Direction As String)
  Dim As Integer FoundPin, SL

  With Pinouts(PinoutCount)
       FoundPin = 0
       FOR SL = 1 to .PinCount
           If UCase(.PinList(SL).PinName) = UCase(Port) Then FoundPin = SL: Exit For
       Next

       'Add function
       If FoundPin <> 0 Then
           With .PinList(FoundPin)
               .FunctionCount += 1
               .FunctionList(.FunctionCount, 1) = Key
               .FunctionList(.FunctionCount, 2) = Direction
           End With
       End If
   End With
End Sub

Sub CalcConfigMasks
  Dim As Integer CurrLoc, CurrConf

  'Mask should have 1 for implemented bits, 0 for not used bits
  ConfMask = 0


  'FOR CurrLoc = 1 to 20
  '  ConfigMask(CurrLoc) = 0
  'Next
  For CurrConf = 1 To COC
    CurrLoc = Config(CurrConf).Location
    If CurrLoc <= 20 Then
      If CurrLoc > ConfMask Then ConfMask = CurrLoc
        if Instr(chipname,"18F") <> 0 AND Instr(chipname,"Q4") <> 0 then
            ConfigMask(CurrLoc) = 255
        else
            ConfigMask(CurrLoc) = (ConfigMask(CurrLoc) Or Not(Config(CurrConf).Value)) And 255
        end if
    End If
  Next

End Sub

Sub CalcIntOscSpeeds
  'Calculate speeds supported by internal oscillator

  'Does chip have OSCCON? If yes, it probably has an adjustable internal osc
  If HasSFR("OSCCON") Then
    'Does it have software PLL enable? If yes, can do up to 32 MHz
    'If HasSFRBit("SPLLEN") Or HasSFRBit("PLLEN") Then
    If HasSFRBit("SPLLEN") Or HasSFRBit("PLLEN") or IntOSCCONFormatVariant=2 Then
      Select Case ChipIntOsc
        Case 64:
        if InStr(LCase(chipname), "k20")<>0 or InStr(LCase(chipname), "k22")<>0 or InStr(LCase(chipname), "k80")<>0 or InStr(LCase(chipname), "k90")<>0 then
            ChipIntOscSpeeds = "64, 32, 16, 8, 4, 2, 1, 0.5, 0.25"
        else
            ChipIntOscSpeeds = "64, 32, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
        end if
        Case 48: ChipIntOscSpeeds = "48, 32, 24, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 32: ChipIntOscSpeeds = "32, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 16: ChipIntOscSpeeds = "16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 8: ChipIntOscSpeeds = "8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 0.032: ChipIntOscSpeeds = Str(ChipIntOsc)
        Case Else:
          ChipIntOscSpeeds = Str(ChipIntOsc)
          Print "Warning: W101: unusual int osc speed:" + Str(ChipIntOsc) + " (Has OSCCON, Has SPLLEN/PLLEN)"
      End Select

    'If no SPLLEN, can do up to 8
    Else
      Select Case ChipIntOsc
        Case 32: ChipIntOscSpeeds = "32, 16, 8, 4, 2, 1"'chips like 16f15214 class Family 15001
        Case 0.16125:  ChipIntOscSpeeds = "16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 16: ChipIntOscSpeeds = "16, 8, 4, 2"
        Case 8: ChipIntOscSpeeds = "8, 4, 2, 1, 0.5, 0.25, 0.125"
        Case 0.032: ChipIntOscSpeeds = Str(ChipIntOsc)
        Case 0: ChipIntOscSpeeds = "0"
        Case Else:
          ChipIntOscSpeeds = Str(ChipIntOsc)
          Print "Warning: W102: unusual int osc speed:" + Str(ChipIntOsc)  + " (Has OSCCON, No SPLLEN)"
      End Select

    End If

  'Newer style, uses NDIV in OSCCON2 to set speed
  ElseIf HasSFR("OSCCON2") Then
    Select Case ChipIntOsc
      Case 64:
          If HasSFRBit("NDIV3") = -1 then
              ChipIntOscSpeeds = "64, 48, 32, 24, 16, 12, 8, 4, 2, 1, 0.5, 0.25, 0.125"
          else
              ChipIntOscSpeeds = "64, 48, 32, 24, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125"
          end if
      Case 32: 
        IF ChipSubFamily = 15002  or ChipSubFamily = 15003 or  ChipSubFamily = 15004 Then
          ChipIntOscSpeeds = "32, 16, 8, 4, 2, 1"
        Else
          ChipIntOscSpeeds = "32, 24, 16, 8, 4, 2, 1, 0.5, 0.25, 0.125, 0.0625"
        End If
      Case Else:
        ChipIntOscSpeeds = Str(ChipIntOsc)
        Print "Warning: W103: unusual int osc speed:" + Str(ChipIntOsc)  + " (No OSCCON, Has OSCCON2)"
    End Select

  ElseIf HasSFRBit("IOSCFS") or ChipIntOsc = 0.84 Then
     ChipIntOscSpeeds = "8, 4"

  'No OSCCON, use whatever speed is in chipdata.csv
  Else
    ChipIntOscSpeeds = Str(ChipIntOsc)

  End If

End Sub

Sub CalcRamBlocks

  'Calculate free RAM blocks
  RBC = 0
  SRBC = 0
  IF ChipFamily <> 16 THEN
    ' print ChipRAM
    If ChipRAM = 16 THEN
      RBC = RBC + 1: RamBlock(RBC) = "10:1F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "10:1F"
    ElseIf ChipRAM = 23 Then
      RBC = RBC + 1: RamBlock(RBC) = "09:1F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "09:1F"
    ElseIf ChipRAM = 24 THEN
      RBC = RBC + 1: RamBlock(RBC) = "08:1F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "08:1F"
    ElseIf ChipRAM = 25 THEN
      RBC = RBC + 1: RamBlock(RBC) = "07:1F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "07:1F"
    ElseIf ChipRAM = 32 Then
      RBC = RBC + 1: RamBlock(RBC) = "60:7F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 36 THEN
      RBC = RBC + 1: RamBlock(RBC) = "0C:2F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0C:2F"
    ElseIf ChipRAM = 38 Then
      RBC = RBC + 1: RamBlock(RBC) = "0A:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      'SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0A:0F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0A:0F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if

    ElseIf ChipRAM = 41 THEN
      RBC = RBC + 1: RamBlock(RBC) = "07:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "07:0F"
    ElseIf ChipRAM = 64 THEN
      If ChipFamily = 15 Then
        RBC = RBC + 1: RamBlock(RBC) = "20:4F"
        RBC = RBC + 1: RamBlock(RBC) = "70:7F"

        If NoBankRAMStr = "" then
          SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
        else
          CalcNoBankString ( NoBankRAMStr )
        End if

      Else

        If ucase(ChipName) = "12F752" or ucase(ChipName) = "12HV752" Then
            RBC = RBC + 1: RamBlock(RBC) = "40:7F"
        Else
            RBC = RBC + 1: RamBlock(RBC) = "20:5F"
        End If

        'added to handle 18f NoBankRAM June 2019
        if len( NoBankRAMStr ) <> 0 then

          if instr(NoBankRAMStr, "|") = 0 then
              SRBC = SRBC + 1: SharedRamBlock(SRBC) = NoBankRAMStr
          else
              NoBankRAMStr = NoBankRAMStr + "|"
              do while instr(NoBankRAMStr, "|") <> 0
                  SRBC = SRBC + 1: SharedRamBlock(SRBC) = mid( NoBankRAMStr ,1, instr(NoBankRAMStr, "|") - 1 )
                  NoBankRAMStr =  mid( NoBankRAMStr , instr(NoBankRAMStr, "|") + 1 )
              loop
          end if
        else
          SRBC = SRBC + 1: SharedRamBlock(SRBC) = "20:5F"
        end if


        
      End If
    ElseIf ChipRAM = 67 Then
      RBC = RBC + 1: RamBlock(RBC) = "0D:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      RBC = RBC + 1: RamBlock(RBC) = "50:5F"
      RBC = RBC + 1: RamBlock(RBC) = "70:7F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0D:0F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0D:0F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 68 THEN
      RBC = RBC + 1: RamBlock(RBC) = "0C:4F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0C:4F"
    ElseIf ChipRAM = 72 THEN
      RBC = RBC + 1: RamBlock(RBC) = "08:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      RBC = RBC + 1: RamBlock(RBC) = "50:5F"
      RBC = RBC + 1: RamBlock(RBC) = "70:7F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "08:1F"
    ElseIf ChipRAM = 73 THEN
      RBC = RBC + 1: RamBlock(RBC) = "07:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      RBC = RBC + 1: RamBlock(RBC) = "50:5F"
      RBC = RBC + 1: RamBlock(RBC) = "70:7F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "07:0F"
    ElseIf ChipRAM = 80 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:6F"
    ElseIf ChipRAM = 96 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 128 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:BF"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 134 Then
      RBC = RBC + 1: RamBlock(RBC) = "0A:0F"
      RBC = RBC + 1: RamBlock(RBC) = "10:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      RBC = RBC + 1: RamBlock(RBC) = "50:5F"
      RBC = RBC + 1: RamBlock(RBC) = "70:7F"
      RBC = RBC + 1: RamBlock(RBC) = "90:9F"
      RBC = RBC + 1: RamBlock(RBC) = "B0:BF"
      RBC = RBC + 1: RamBlock(RBC) = "D0:DF"
      RBC = RBC + 1: RamBlock(RBC) = "F0:FF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "0A:0F"
    ElseIf ChipRAM = 176 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 192 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:FF"
    ElseIf ChipRAM = 201 Then
      RBC = RBC + 1: RamBlock(RBC) = "07:0F"
      RBC = RBC + 1: RamBlock(RBC) = "10:1F"
      RBC = RBC + 1: RamBlock(RBC) = "30:3F"
      RBC = RBC + 1: RamBlock(RBC) = "50:5F"
      RBC = RBC + 1: RamBlock(RBC) = "70:7F"
      RBC = RBC + 1: RamBlock(RBC) = "80:9F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:BF"
      RBC = RBC + 1: RamBlock(RBC) = "C0:DF"
      RBC = RBC + 1: RamBlock(RBC) = "E0:FF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "07:0F"
    ElseIf ChipRAM = 224 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:14F"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 256 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 336 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 352 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "110:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 368 THEN
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "110:16F"
      RBC = RBC + 1: RamBlock(RBC) = "190:1EF"
      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
    ElseIf ChipRAM = 384 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:24F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 512 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"
      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:32F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 768 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"
      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:32F"
      RBC = RBC + 1: RamBlock(RBC) = "3A0:3EF"
      RBC = RBC + 1: RamBlock(RBC) = "420:46F"
      RBC = RBC + 1: RamBlock(RBC) = "4A0:4BF"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 1024 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"
      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:36F"
      RBC = RBC + 1: RamBlock(RBC) = "3A0:3EF"
      RBC = RBC + 1: RamBlock(RBC) = "420:46F"
      RBC = RBC + 1: RamBlock(RBC) = "4A0:4EF"
      RBC = RBC + 1: RamBlock(RBC) = "520:56F"
      RBC = RBC + 1: RamBlock(RBC) = "5A0:5EF"
      RBC = RBC + 1: RamBlock(RBC) = "620:64F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 1536 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"
      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:36F"
      RBC = RBC + 1: RamBlock(RBC) = "3A0:3EF"
      RBC = RBC + 1: RamBlock(RBC) = "420:46F"
      RBC = RBC + 1: RamBlock(RBC) = "4A0:4EF"
      RBC = RBC + 1: RamBlock(RBC) = "520:56F"
      RBC = RBC + 1: RamBlock(RBC) = "5A0:5EF"
      RBC = RBC + 1: RamBlock(RBC) = "620:66F"
      RBC = RBC + 1: RamBlock(RBC) = "6A0:6EF"
      RBC = RBC + 1: RamBlock(RBC) = "720:76F"
      RBC = RBC + 1: RamBlock(RBC) = "7A0:7EF"
      RBC = RBC + 1: RamBlock(RBC) = "820:86F"
      RBC = RBC + 1: RamBlock(RBC) = "8A0:8EF"
      RBC = RBC + 1: RamBlock(RBC) = "920:96F"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if
    ElseIf ChipRAM = 2048 Then
      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"
      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:36F"
      RBC = RBC + 1: RamBlock(RBC) = "3A0:3EF"
      RBC = RBC + 1: RamBlock(RBC) = "420:46F"
      RBC = RBC + 1: RamBlock(RBC) = "4A0:4EF"
      RBC = RBC + 1: RamBlock(RBC) = "520:56F"
      RBC = RBC + 1: RamBlock(RBC) = "5A0:5EF"
      RBC = RBC + 1: RamBlock(RBC) = "620:66F"
      RBC = RBC + 1: RamBlock(RBC) = "6A0:6EF"
      RBC = RBC + 1: RamBlock(RBC) = "720:76F"
      RBC = RBC + 1: RamBlock(RBC) = "7A0:7EF"
      RBC = RBC + 1: RamBlock(RBC) = "820:86F"
      RBC = RBC + 1: RamBlock(RBC) = "8A0:8EF"
      RBC = RBC + 1: RamBlock(RBC) = "920:96F"

      RBC = RBC + 1: RamBlock(RBC) = "9A0:9EF"
      RBC = RBC + 1: RamBlock(RBC) = "A20:A6F"
      RBC = RBC + 1: RamBlock(RBC) = "AA0:AEF"
      RBC = RBC + 1: RamBlock(RBC) = "B20:B6F"
      RBC = RBC + 1: RamBlock(RBC) = "BA0:BEF"
      RBC = RBC + 1: RamBlock(RBC) = "C20:C6F"
      RBC = RBC + 1: RamBlock(RBC) = "CA0:CBF"
'      SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if

    ElseIf ChipRAM = 4096 Then
'
'pragma  data               0x20-0x7F,0xA0-0xEF,0x120-0x16F,0x1A0-0x1EF,0x220-0x26F
'pragma  data               0x2A0-0x2EF,0x320-0x36F,0x3A0-0x3EF,0x420-0x46F,0x4A0-0x4EF
'pragma  data               0x520-0x56F,0x5A0-0x5EF,0x620-0x66F,0x6A0-0x6EF,0x720-0x76F
'pragma  data               0x7A0-0x7EF,0x820-0x86F,0x8A0-0x8EF,0x920-0x96F,0x9A0-0x9EF
'pragma  data               0xA20-0xA6F,0xAA0-0xAEF,0xB20-0xB6F,0xBA0-0xBEF,0xC20-0xC6F

      RBC = RBC + 1: RamBlock(RBC) = "20:7F"
      RBC = RBC + 1: RamBlock(RBC) = "A0:EF"
      RBC = RBC + 1: RamBlock(RBC) = "120:16F"
      RBC = RBC + 1: RamBlock(RBC) = "1A0:1EF"
      RBC = RBC + 1: RamBlock(RBC) = "220:26F"

      RBC = RBC + 1: RamBlock(RBC) = "2A0:2EF"
      RBC = RBC + 1: RamBlock(RBC) = "320:36F"
      RBC = RBC + 1: RamBlock(RBC) = "3A0:3EF"
      RBC = RBC + 1: RamBlock(RBC) = "420:46F"
      RBC = RBC + 1: RamBlock(RBC) = "4A0:4EF"

      RBC = RBC + 1: RamBlock(RBC) = "520:56F"
      RBC = RBC + 1: RamBlock(RBC) = "5A0:5EF"
      RBC = RBC + 1: RamBlock(RBC) = "620:66F"
      RBC = RBC + 1: RamBlock(RBC) = "6A0:6EF"
      RBC = RBC + 1: RamBlock(RBC) = "720:76F"

      RBC = RBC + 1: RamBlock(RBC) = "7A0:7EF"
      RBC = RBC + 1: RamBlock(RBC) = "820:86F"
      RBC = RBC + 1: RamBlock(RBC) = "8A0:8EF"
      RBC = RBC + 1: RamBlock(RBC) = "920:96F"
      RBC = RBC + 1: RamBlock(RBC) = "9A0:9EF"

      RBC = RBC + 1: RamBlock(RBC) = "A20:A6F"
      RBC = RBC + 1: RamBlock(RBC) = "AA0:AEF"
      RBC = RBC + 1: RamBlock(RBC) = "B20:B6F"
      RBC = RBC + 1: RamBlock(RBC) = "BA0:BEF"
      RBC = RBC + 1: RamBlock(RBC) = "C20:C6F"

'pragma  data               0xCA0-0xCEF,0xD20-0xD6F,0xDA0-0xDEF,0xE20-0xE6F,0xEA0-0xEEF
'pragma  data               0xF20-0xF6F,0xFA0-0xFEF,0x1020-0x106F,0x10A0-0x10EF,0x1120-0x116F
'pragma  data               0x11A0-0x11EF,0x1220-0x126F,0x12A0-0x12EF,0x1320-0x136F,0x13A0-0x13EF
'pragma  data               0x1420-0x146F,0x14A0-0x14EF,0x1520-0x156F,0x15A0-0x15EF,0x1620-0x166F
'pragma  data               0x16A0-0x16EF,0x1720-0x176F,0x17A0-0x17EF,0x1820-0x186F,0x18A0-0x18EF
'pragma  data               0x1920-0x196F

      RBC = RBC + 1: RamBlock(RBC) = "CA0:CEF"
      RBC = RBC + 1: RamBlock(RBC) = "D20:D6F"
      RBC = RBC + 1: RamBlock(RBC) = "DA0:DEF"
      RBC = RBC + 1: RamBlock(RBC) = "E20:E6F"
      RBC = RBC + 1: RamBlock(RBC) = "EA0:EEF"

      RBC = RBC + 1: RamBlock(RBC) = "F20:F6F"
      RBC = RBC + 1: RamBlock(RBC) = "FA0:FEF"
      RBC = RBC + 1: RamBlock(RBC) = "1020:106F"
      RBC = RBC + 1: RamBlock(RBC) = "10A0:10EF"
      RBC = RBC + 1: RamBlock(RBC) = "1120:116F"

      RBC = RBC + 1: RamBlock(RBC) = "11A0:11EF"
      RBC = RBC + 1: RamBlock(RBC) = "1220:126F"
      RBC = RBC + 1: RamBlock(RBC) = "12A0:12EF"
      RBC = RBC + 1: RamBlock(RBC) = "1320:136F"
      RBC = RBC + 1: RamBlock(RBC) = "13A0:13EF"

      RBC = RBC + 1: RamBlock(RBC) = "1420:146F"
      RBC = RBC + 1: RamBlock(RBC) = "14A0:14EF"
      RBC = RBC + 1: RamBlock(RBC) = "1520:156F"
      RBC = RBC + 1: RamBlock(RBC) = "15A0:15EF"
      RBC = RBC + 1: RamBlock(RBC) = "1620:166F"

      RBC = RBC + 1: RamBlock(RBC) = "16A0:16EF"
      RBC = RBC + 1: RamBlock(RBC) = "1720:176F"
      RBC = RBC + 1: RamBlock(RBC) = "17A0:17EF"
      RBC = RBC + 1: RamBlock(RBC) = "1820:186F"
      RBC = RBC + 1: RamBlock(RBC) = "18A0:18EF"

      RBC = RBC + 1: RamBlock(RBC) = "1920:196F"

      If NoBankRAMStr = "" then
        SRBC = SRBC + 1: SharedRamBlock(SRBC) = "70:7F"
      else
        CalcNoBankString ( NoBankRAMStr )
      End if

    Else
      Print "Could not build memory map"
      ChipDataSuspect = -1
    End IF

  '18F code
  Else

    '18F13K50 has weird layout
    If UCase(ChipName) = "18F13K50" Or UCase(ChipName) = "18LF13K50" Then
      RBC = RBC + 1: RamBlock(RBC) = "0:FF"
      RBC = RBC + 1: RamBlock(RBC) = "200:2FF"

    Elseif   Instr(UCase(ChipName),"Q43" ) <> 0 or Instr(UCase(ChipName),"Q41" ) <> 0 or Instr(UCase(ChipName),"Q40" ) <> 0 or Instr(UCase(ChipName),"Q83" ) <> 0 or Instr(UCase(ChipName),"Q84") <> 0 or Instr(UCase(ChipName),"Q71") <> 0 Then

      ChipMinimumBankSel = 5
      'add 1280 for the base of 0x500
      RBC = RBC + 1: RamBlock(RBC) = "500:" + HEX(ChipRAM - 1 + 1280 )


      'added to handle 18f NoBankRAM June 2019
      if len( NoBankRAMStr ) <> 0 then

        if instr(NoBankRAMStr, "|") = 0 then
            SRBC = SRBC + 1: SharedRamBlock(SRBC) = NoBankRAMStr
        else
            NoBankRAMStr = NoBankRAMStr + "|"
            do while instr(NoBankRAMStr, "|") <> 0
                SRBC = SRBC + 1: SharedRamBlock(SRBC) = mid( NoBankRAMStr ,1, instr(NoBankRAMStr, "|") - 1 )
                NoBankRAMStr =  mid( NoBankRAMStr , instr(NoBankRAMStr, "|") + 1 )
            loop
        end if
      end if
        
    Elseif Instr(UCase(ChipName),"Q10" ) <> 0 Then

        'added to handle SECTOR RAM
        Select Case ChipRAM

          Case 1024
              RBC = RBC + 1: RamBlock(RBC) = "0:3FF"
              RBC = RBC + 1: RamBlock(RBC) = "'400:4FF   Sector RAM. Excluded from RAM usage, and overall RAM reduced by 256 bytes.  Was " + str(ChipRAM) + " bytes now mofified to " + Str(ChipRAM-256) + " bytes"
'              RBC = RBC + 1: RamBlock(RBC) = "500:" + HEX(ChipRAM - 1)
          Case 2048
              RBC = RBC + 1: RamBlock(RBC) = "0:7FF"
              RBC = RBC + 1: RamBlock(RBC) = "'800:8FF   Sector RAM. Excluded from RAM usage, and overall RAM reduced by 256 bytes.  Was " + str(ChipRAM) + " bytes now mofified to " + Str(ChipRAM-256) + " bytes"
'              RBC = RBC + 1: RamBlock(RBC) = "900:" + HEX(ChipRAM - 1)
          Case 3615
              RBC = RBC + 1: RamBlock(RBC) = "0:CFF"
              RBC = RBC + 1: RamBlock(RBC) = "'D00:DFF   Sector RAM. Excluded from RAM usage, and overall RAM reduced by 256 bytes.  Was " + str(ChipRAM) + " bytes now mofified to " + Str(ChipRAM-256) + " bytes"
              RBC = RBC + 1: RamBlock(RBC) = "E00:E1E"
        End Select
        ChipRAM = ChipRAM - 256


        if len( NoBankRAMStr ) <> 0 then

        if instr(NoBankRAMStr, "|") = 0 then
            SRBC = SRBC + 1: SharedRamBlock(SRBC) = NoBankRAMStr
        else
            NoBankRAMStr = NoBankRAMStr + "|"
            do while instr(NoBankRAMStr, "|") <> 0
                SRBC = SRBC + 1: SharedRamBlock(SRBC) = mid( NoBankRAMStr ,1, instr(NoBankRAMStr, "|") - 1 )
                NoBankRAMStr =  mid( NoBankRAMStr , instr(NoBankRAMStr, "|") + 1 )
            loop
        end if
        end if

    Else
      'default operation
      RBC = RBC + 1: RamBlock(RBC) = "0:" + HEX(ChipRAM - 1)

      'added to handle 18f NoBankRAM June 2019
      if len( NoBankRAMStr ) <> 0 then
      'erv
        if instr(NoBankRAMStr, "|") = 0 then
            SRBC = SRBC + 1: SharedRamBlock(SRBC) = NoBankRAMStr
        else
            NoBankRAMStr = NoBankRAMStr + "|"
            do while instr(NoBankRAMStr, "|") <> 0
                SRBC = SRBC + 1: SharedRamBlock(SRBC) = mid( NoBankRAMStr ,1, instr(NoBankRAMStr, "|") - 1 )
                NoBankRAMStr =  mid( NoBankRAMStr , instr(NoBankRAMStr, "|") + 1 )
            loop
        end if
      end if

    End If

  End If

  'Override default RAM blocks
  Select Case UCase(ChipName)
'moved as this has same RAM as the others.
'    Case "10F320":
'      RBC = 1: RamBlock(1) = "60:7F"
'      SRBC = 1: SharedRamBlock(1) = "70:7F"
    Case "10F320", "10F322", "12F609", "12F615", "12F635", "16F631":
      RBC = 1: RamBlock(1) = "40:7F"
      SRBC = 1: SharedRamBlock(1) = "70:7F"
  End Select

End Sub

Function CheckBit (BitName As String) As Integer

    Dim As Integer PD

    For PD = 1 to SVBC
        With SysVarBits(PD)
            If .Name = BitName Then Return -1
        End With
    Next

    Return 0

End Function

Function GetIntName (IntVect As String) As String
  Dim As String IntName, NewVect

  IntName = ""
  Select Case UCASE(IntVect)
    Case "ACTIE", "STIE":
      IntName = "ActiveClockTuning"

    Case "CTMUIE":
      IntName = "ChargeTimeMeasurement"

    Case "COG1IE":
      IntName = "COG"

    Case "HLTMR1IE":
      IntName = "HardwareLimitTimer"

    Case "HLTMR2IE":
      IntName = "HardwareLimitTimer2"

    Case "INTE":
      IntName = "ExtInt0"

    Case "INT0IE", "INT0E":
      IntName = "ExtInt0"

    Case "INT1IE", "INT1E":
      IntName = "ExtInt1"

    Case "INT2IE", "INT2E":
      IntName = "ExtInt2"

        Case "INT3IE", "INT3E":
            IntName = "ExtInt3"

        Case "USBIE":
            IntName = "USB"

        Case "ETHIE":
            IntName = "Ethernet"

        Case "CRIE":
            IntName = "Crypto"

        Case "RAIE":

            IntName = "PORTAChange"

        Case "RBIE", "IOCIE":
          if HasSFR("IOCAF") and HasSFR("IOCBF") then
            IntName = "PORTABChange"
          elseif HasSFR("IOCAF") then
            IntName = "PORTAChange"
          else
            IntName = "PORTBChange"
          end if

        Case "GPIE":
            IntName = "GPIOChange"

        Case "RABIE":
            IntName = "PORTABChange"

    Case "ADIE", "AD1IE":
            IntName = "ADCReady"

    Case "AD2IE":
            IntName = "ADC2Ready"

        Case "CMP0IE":
            IntName = "Comp0Change"

        Case "CMIE", "CM1IE", "C1IE", "CMP1IE":
            IntName = "Comp1Change"

        Case "C2IE", "CM2IE", "CMP2IE":
            IntName = "Comp2Change"

      Case "C3IE", "CM3IE", "CMP3IE":
        IntName = "Comp3Change"

    Case "C4IE", "CM4IE":
      IntName = "Comp4Change"
    Case "C5IE", "CM5IE":
      IntName = "Comp5Change"
    Case "C6IE", "CM6IE":
      IntName = "Comp6Change"
    Case "C7IE", "CM7IE":
      IntName = "Comp7Change"
    Case "C8IE", "CM8IE":
      IntName = "Comp8Change"

        Case "TXIE", "TBIE", "TX1IE", "TXIE_PIE1":
            IntName = "UsartTX1Ready"

        Case "TX2IE":
            IntName = "UsartTX2Ready"

        Case "TX3IE":
            IntName = "UsartTX3Ready"

        Case "TX4IE":
            IntName = "UsartTX4Ready"

        Case "TX5IE":
            IntName = "UsartTX5Ready"

        Case "PTIE":
            IntName = "PWMTimeBase"

        Case "RCIE", "RC1IE":
            IntName = "UsartRX1Ready"

        Case "RC2IE":
            IntName = "UsartRX2Ready"

        Case "RC3IE":
            IntName = "UsartRX3Ready"

        Case "RC4IE":
            IntName = "UsartRX4Ready"

        Case "RC5IE":
            IntName = "UsartRX5Ready"

        Case "OSCFIE", "OSFIE":
            IntName = "OscillatorFail"

        Case "PSPIE":
            IntName = "PSPReady"

        Case "PMPIE":
            IntName = "PMPReady"

        Case "LCDIE":
            IntName = "LCDReady"

        Case "SSPIE", "SSP1IE":
            IntName = "SSP1Ready"

        Case "SSP2IE":
            IntName = "SSP2Ready"

        Case "SPPIE":
            IntName = "SPPReady"

        Case "BCLIE", "BCL1IE":
            IntName = "SSP1Collision"

        Case "BCL2IE":
            IntName = "SSP2Collision"

        Case "CCP1IE", "ECCP1IE", "CCPIE", "ECCPIE":
            IntName = "CCP1"

        Case "CCP2IE":
            IntName = "CCP2"

        Case "CCP3IE":
            IntName = "CCP3"

        Case "CCP4IE":
            IntName = "CCP4"

        Case "CCP5IE":
            IntName = "CCP5"

    Case "CCP6IE":
      IntName = "CCP6"

    Case "CCP7IE":
      IntName = "CCP7"

    Case "CCP8IE":
      IntName = "CCP8"

    Case "CCP9IE":
      IntName = "CCP9"

    Case "CCP10IE":
      IntName = "CCP10"

        Case "TMR0IE", "T0IE":
            IntName = "Timer0Overflow"

        Case "TMR1IE", "T1IE":
            IntName = "Timer1Overflow"

    Case "TMR1GIE":
      IntName = "Timer1Gate"

    Case "TMR3GIE":
      IntName = "Timer3Gate"

    Case "TMR5GIE":
      IntName = "Timer5Gate"

        Case "TMR2IE", "T2IE":
            IntName = "Timer2Match"

        Case "TMR3IE", "T3IE":
            IntName = "Timer3Overflow"

        Case "TMR4IE", "T4IE":
            IntName = "Timer4Match"

        Case "TMR5IE", "T5IE":
            IntName = "Timer5Overflow"

    Case "TMR6IE", "T6IE":
      IntName = "Timer6Match"

    Case "TMR7IE", "T7IE":
      IntName = "Timer7Overflow"

    Case "TMR7GIE":
      IntName = "Timer7Gate"

    Case "TMR8IE", "T8IE":
      IntName = "Timer8Match"

    Case "TMR10IE", "T10IE":
      IntName = "Timer10Match"

    Case "TMR12IE", "T12IE":
      IntName = "Timer12Match"

        Case "IC3DRIE":
            IntName = "Timer5CAP3"

        Case "IC2QEIE":
            IntName = "Timer5CAP2"

        Case "IC1IE":
            IntName = "Timer5CAP1"

        Case "EEIE":
            IntName = "EEPROMReady"

        Case "LVDIE", "HLVDIE":
            IntName = "VoltageFail"

        Case "ULPWUIE":
            IntName = "LPWU"

        Case "WAKIE":
            IntName = "CANActivity"

        Case "ERRIE":
            IntName = "CANError"

        Case "IRXIE":
            IntName = "CANBadMessage"

        Case "FIFOWMIE", "FIFOMWIE":
            IntName = "CANHighWatermark"

        Case "RXB0IE":
            IntName = "CANRx0Ready"

        Case "RXB1IE":
            IntName = "CANRx1Ready"

        Case "RXB2IE":
            IntName = "CANRx2Ready"

        Case "RXBNIE":
            IntName = "CANRxReady"

        Case "TXB0IE":
            IntName = "CANTx0Ready"

        Case "TXB1IE":
            IntName = "CANTx1Ready"

        Case "TXB2IE":
            IntName = "CANTx2Ready"

        Case "TXBNIE":
            IntName = "CANTxReady"

      Case "RTCCIE":
        IntName = "RTCAlarmTrigger"

    Case "CLC1IE", "CLCIE":
      IntName = "LogicCell1Event"

    Case "CLC2IE":
      IntName = "LogicCell2Event"

    Case "CLC3IE":
      IntName = "LogicCell3Event"

    Case "CLC4IE":
      IntName = "LogicCell4Event"

    Case "CLC5IE":
      IntName = "LogicCell5Event"

    Case "CLC6IE":
      IntName = "LogicCell6Event"

    Case "CLC7IE":
      IntName = "LogicCell7Event"

    Case "CLC8IE":
      IntName = "LogicCell8Event"


    Case "NCO1IE", "NCOIE":
      IntName = "NumericOscOverflow"

    Case "NCO2IE":
      IntName = "NumericOscOverflow2"

    Case "NCO3IE":
      IntName = "NumericOscOverflow3"


    'Following events first manually added to 16F18855 by WMR
    Case "ADTIE":
      IntName = "ADThreshold"

    Case "CSWIE":
      IntName = "ClockSwitchComplete"

    Case "ZCDIE":
      IntName = "ZeroCrossDetect"

    Case "CWG1IE":
      IntName = "CWG1ShutDown"

    Case "CWG2IE":
      IntName = "CWG2ShutDown"

    Case "CWG3IE":
      IntName = "CWG3ShutDown"

    Case "NVMIE":
      IntName = "NVMComplete"

    Case "CRCIE":
      IntName = "CRCComplete"

    Case "SCANIE"
      IntName = "ScanComplete"

    Case "SMT1IE":
      IntName = "SMT1Overflow"

    Case "SMT1PRAIE":
      IntName = "SMT1PeriodAcquired"

    Case "SMT1PWAIE":
      IntName = "SMT1PulseWidthAcquired"

    Case "SMT2IE":
      IntName = "SMT2Overflow"

    Case "SMT2PRAIE":
      IntName = "SMT2PeriodAcquired"

    Case "SMT2PWAIE":
      IntName = "SMT2PulseWidthAcquired"

    'New events added 25/4/2016
    Case "AT1IE"
      IntName = "AngularTimer"

    Case "COGIE", "CWGIE", "COG1IE":
      IntName = "ComplementaryGenerator"
    Case "COG2IE":
      IntName = "ComplementaryGenerator2"
    Case "COG3IE":
      IntName = "ComplementaryGenerator3"
    Case "COG4IE":
      IntName = "ComplementaryGenerator4"

    Case "PID1DIE"
      IntName = "PID1Complete"
    Case "PID1EIE":
      IntName = "PID1Error"

    Case "PSMC1SIE"
      IntName = "PSMC1Shutdown"
    Case "PSMC1TIE"
      IntName = "PSMC1TimeBase"
    Case "PSMC2SIE"
      IntName = "PSMC2Shutdown"
    Case "PSMC2TIE"
      IntName = "PSMC2TimeBase"
    Case "PSMC3SIE"
      IntName = "PSMC3Shutdown"
    Case "PSMC3TIE", "PMSC3TIE"
      IntName = "PSMC3TimeBase"
    Case "PSMC4SIE"
      IntName = "PSMC4Shutdown"
    Case "PSMC4TIE"
      IntName = "PSMC4TimeBase"

    Case "PWM1IE":
      IntName = "PWM1MatchEvent"
    Case "PWM2IE":
      IntName = "PWM2MatchEvent"
    Case "PWM3IE":
      IntName = "PWM3MatchEvent"
    Case "PWM4IE":
      IntName = "PWM4MatchEvent"
    Case "PWM5IE":
      IntName = "PWM5MatchEvent"
    Case "PWM6IE":
      IntName = "PWM6MatchEvent"
    Case "PWM7IE":
      IntName = "PWM7MatchEvent"
    Case "PWM8IE":
      IntName = "PWM8MatchEvent"
    Case "PWM9IE":
      IntName = "PWM9MatchEvent"
    Case "PWM10IE":
      IntName = "PWM10MatchEvent"
    Case "PWM11IE":
      IntName = "PWM11MatchEvent"
    Case "PWM12IE":
      IntName = "PWM12MatchEvent"

    Case "PWMPIE", "PWM1PIE":
      IntName = "PWMPeriodEvent"
    Case "PWM2PIE":
      IntName = "PWM2PeriodEvent"
    Case "PWM3PIE":
      IntName = "PWM3PeriodEvent"


    Case "TMRAIE":
      IntName = "TimerAOverflow"
    Case "TMRBIE":
      IntName = "TimerBOverflow"

    'even newwer events... june 2017
    Case "SWIE"
        IntName = "SoftwareInterrupt"

    Case "DMA1SCNTIE"
        IntName = "DMA1SourceCountInterrupt"
    Case "DMA1DCNTIE"
        IntName = "DMA1DestinationCountInterrupt"
    Case "DMA1ORIE"
        IntName = "DMA1OverrunInterrupt"
    Case "DMA1AIE"
        IntName = "DMA1AbortInterrupt"

    Case "DMA5SCNTIE"
        IntName = "DMA5SourceCountInterrupt"
    Case "DMA5DCNTIE"
        IntName = "DMA5DestinationCountInterrupt"
    Case "DMA5ORIE"
        IntName = "DMA5OverrunInterrupt"
    Case "DMA5AIE"
        IntName = "DMA5AbortInterrupt"

    Case "DMA6SCNTIE"
        IntName = "DMA6SourceCountInterrupt"
    Case "DMA6DCNTIE"
        IntName = "DMA6DestinationCountInterrupt"
    Case "DMA6ORIE"
        IntName = "DMA6OverrunInterrupt"
    Case "DMA6AIE"
        IntName = "DMA6AbortInterrupt"




    Case "SPI1RXIE"
        IntName = "SPIReceiveInterrupt"
    Case "SPI1TXIE"
        IntName = "SPITransmitInterrupt"
    Case "SPI1IE"
        IntName = "SPIInterrupt"

    Case "SPI2RXIE"
        IntName = "SPI2ReceiveInterrupt"

    Case "SPI2TXIE"
        IntName = "SPI2TransmitInterrupt"
    Case "SPI2IE"
        IntName = "SPI2Interrupt"

    Case "I2C1RXIE"
        IntName = "I2C1ReceiveInterrupt"
    Case "I2C1TXIE"
        IntName = "I2C1TransmitInterrupt"
    Case "I2C1IE"
        IntName = "I2C1Interrupt"
    Case "I2C1EIE"
        IntName = "I2C1ErrorInterrupt"
    Case "U1RXIE"
        IntName = "UART1ReceiveInterrupt"
    Case "U1TXIE"
        IntName = "UART1TransmitInterrupt"
    Case "U1EIE"
        IntName = "UART1FramingErrorInterrupt"
    Case "U1IE"
        IntName = "UART1Interrupt"
    Case "DMA2SCNTIE"
        IntName = "DMA2SourceCountInterrupt"
    Case "DMA2DCNTIE"
        IntName = "DMA2DestinationCountInterrupt"
    Case "DMA2ORIE"
        IntName = "DMA2OverrunInterrupt"
    Case "DMA2AIE"
        IntName = "DMA2AbortInterrupt"


    Case "DMA3SCNTIE"
        IntName = "DMA3SourceCountInterrupt"
    Case "DMA3DCNTIE"
        IntName = "DMA3DestinationCountInterrupt"
    Case "DMA3ORIE"
        IntName = "DMA3OverrunInterrupt"
    Case "DMA3AIE"
        IntName = "DMA3AbortInterrupt"

    Case "DMA4SCNTIE"
        IntName = "DMA4SourceCountInterrupt"
    Case "DMA4DCNTIE"
        IntName = "DMA4DestinationCountInterrupt"
    Case "DMA4ORIE"
        IntName = "DMA4OverrunInterrupt"
    Case "DMA4AIE"
        IntName = "DMA4AbortInterrupt"

    Case "I2C2RXIE"
        IntName = "I2C2ReceiveInterrupt"
    Case "I2C2TXIE"
        IntName = "SPI2TransmitInterrupt"
    Case "I2C2IE"
        IntName = "I2C2Interrupt"
    Case "I2C2EIE"
        IntName = "I2C2ErrorInterrupt"
    Case "U2RXIE"
        IntName = "UART2ReceiveInterrupt"
    Case "U2TXIE"
        IntName = "UART2TransmitInterrupt"
    Case "U2EIE"
        IntName = "UART2FramingErrorInterrupt"
    Case "U2IE"
        IntName = "UART2Interrupt"


    Case "I3C3RXIE"
        IntName = "I3C3ReceiveInterrupt"
    Case "I3C3TXIE"
        IntName = "SPI3TransmitInterrupt"
    Case "I3C3IE"
        IntName = "I3C3Interrupt"
    Case "I3C3EIE"
        IntName = "I3C3ErrorInterrupt"
    Case "U3RXIE"
        IntName = "UART3ReceiveInterrupt"
    Case "U3TXIE"
        IntName = "UART3TransmitInterrupt"
    Case "U3EIE"
        IntName = "UART3FramingErrorInterrupt"
    Case "U3IE"
        IntName = "UART3Interrupt"


    Case "I4C4RXIE"
        IntName = "I4C4ReceiveInterrupt"
    Case "I4C4TXIE"
        IntName = "SPI4TransmitInterrupt"
    Case "I4C4IE"
        IntName = "I4C4Interrupt"
    Case "I4C4EIE"
        IntName = "I4C4ErrorInterrupt"
    Case "U4RXIE"
        IntName = "UART4ReceiveInterrupt"
    Case "U4TXIE"
        IntName = "UART4TransmitInterrupt"
    Case "U4EIE"
        IntName = "UART4FramingErrorInterrupt"
    Case "U4IE"
        IntName = "UART4Interrupt"


    Case "I5C5RXIE"
        IntName = "I5C5ReceiveInterrupt"
    Case "I5C5TXIE"
        IntName = "SPI5TransmitInterrupt"
    Case "I5C5IE"
        IntName = "I5C5Interrupt"
    Case "I5C5EIE"
        IntName = "I5C5ErrorInterrupt"
    Case "U5RXIE"
        IntName = "UART5ReceiveInterrupt"
    Case "U5TXIE"
        IntName = "UART5TransmitInterrupt"
    Case "U5EIE"
        IntName = "UART5FramingErrorInterrupt"
    Case "U5IE"
        IntName = "UART5Interrupt"

    Case "ACTLOCKIE"
        IntName = "ActiveClockTuningLockInterrupt "
    Case "ACTORSIE"
        IntName = "ActiveClockTuningOutofRangeInterrupt"

    Case "ADCH1IE"
        IntName = "ADCContext1ThresholdInterrupt"
    Case "ADCH2IE"
        IntName = "ADCContext1ThresholdInterrupt"
    Case "ADCH3IE"
        IntName = "ADCContext1ThresholdInterrupt"
    Case "ADCH4IE"
        IntName = "ADCContext1ThresholdInterrupt"
    Case "IOCVIE"
        IntName = "VirtualPortsIOCInterrupt"
    Case "TU16AIE"
        IntName ="16bitUniversalTimerAInterrupt"
    Case "TU16BIE"
        IntName ="16bitUniversalTimerBInterrupt"


    Case Else:
            If Instr(IntVect, "_") <> 0 Then
                IntName = GetIntName(Mid(IntVect, Instr(IntVect, "_") + 1))
            Else
                Print "Unrecognised interrupt source: ", IntVect
            End If

    End Select

  Return IntName
End Function

Function GetPin(PinName As String, ChipPins As Integer) As Integer
  Dim As Integer PinNo

  PinNo = 0
  If Left(PinName, 4) = "GPIO" Then PinName = "GP" + MID(PinName, 5)

  Select Case ChipPins
    Case 6:
      Select Case PinName
        Case "GP0", "RA0": PinNo = 5
        Case "GP1", "RA1": PinNo = 4
        Case "GP2", "RA2": PinNo = 3
        Case "GP3", "RA3": PinNo = 8
      End Select

    Case 8:
      Select Case PinName
        Case "GP0", "RA0", "RB0": PinNo = 7
        Case "GP1", "RA1", "RB1": PinNo = 6
        Case "GP2", "RA2", "RB2": PinNo = 5
        Case "GP3", "RA3", "RB3": PinNo = 4
        Case "GP4", "RA4", "RB4": PinNo = 3
        Case "GP5", "RA5", "RB5": PinNo = 2
      End Select

    Case 14:
      Select Case PinName
            Case "RA0": PinNo = 13
            Case "RA1": PinNo = 12
            Case "RA2": PinNo = 11
            Case "RA3": PinNo = 4
            Case "RA4": PinNo = 3
            Case "RA5": PinNo = 2

            Case "RB0": PinNo = 13
            Case "RB1": PinNo = 12
            Case "RB2": PinNo = 11
            Case "RB3": PinNo = 4
            Case "RB4": PinNo = 3
            Case "RB5": PinNo = 2

            Case "RC0": PinNo = 10
            Case "RC1": PinNo = 9
            Case "RC2": PinNo = 8
            Case "RC3": PinNo = 7
            Case "RC4": PinNo = 6
            Case "RC5": PinNo = 5

        End Select

    Case 18:
      If Left(ChipName, 4) = "18F1" Then
        '18F1220/1230/1320/1330
        Select Case PinName:
          Case "RA0": PinNo = 1
          Case "RA1": PinNo = 2
          Case "RA2": PinNo = 6
          Case "RA3": PinNo = 7
          Case "RA4": PinNo = 3
          Case "RA5": PinNo = 4
          Case "RA6": PinNo = 15
          Case "RA7": PinNo = 16

          Case "RB0": PinNo = 8
          Case "RB1": PinNo = 9
          Case "RB2": PinNo = 17
          Case "RB3": PinNo = 18
          Case "RB4": PinNo = 10
          Case "RB5": PinNo = 11
          Case "RB6": PinNo = 12
          Case "RB7": PinNo = 13
        End Select

      Else
        'Most other 18 pin chips
        Select Case PinName:
          Case "RA0": PinNo = 17
          Case "RA1": PinNo = 18
          Case "RA2": PinNo = 1
          Case "RA3": PinNo = 2
          Case "RA4": PinNo = 3
          Case "RA5": PinNo = 4
          Case "RA6": PinNo = 15
          Case "RA7": PinNo = 16

          Case "RB0": PinNo = 6
          Case "RB1": PinNo = 7
          Case "RB2": PinNo = 8
          Case "RB3": PinNo = 9
          Case "RB4": PinNo = 10
          Case "RB5": PinNo = 11
          Case "RB6": PinNo = 12
          Case "RB7": PinNo = 13
        End Select

      End If

    Case 20:
        Select Case PinName
            Case "RA0": PinNo = 19
            Case "RA1": PinNo = 18
            Case "RA2": PinNo = 17
            Case "RA3": PinNo = 4
            Case "RA4": PinNo = 3
            Case "RA5": PinNo = 2

            Case "RB4": PinNo = 13
            Case "RB5": PinNo = 12
            Case "RB6": PinNo = 11
            Case "RB7": PinNo = 10

            Case "RC0": PinNo = 16
            Case "RC1": PinNo = 15
            Case "RC2": PinNo = 14
            Case "RC3": PinNo = 7
            Case "RC4": PinNo = 6
          Case "RC5": PinNo = 5
            Case "RC6": PinNo = 8
            Case "RC7": PinNo = 9

        End Select

        Case 28:
        Select Case PinName
            Case "RA0": PinNo = 2
            Case "RA1": PinNo = 3
            Case "RA2": PinNo = 4
            Case "RA3": PinNo = 5
            Case "RA4": PinNo = 6
            Case "RA5": PinNo = 7
            Case "RA6": PinNo = 10
            Case "RA7": PinNo = 9

            Case "RB0": PinNo = 21
            Case "RB1": PinNo = 22
            Case "RB2": PinNo = 23
            Case "RB3": PinNo = 24
            Case "RB4": PinNo = 25
            Case "RB5": PinNo = 26
            Case "RB6": PinNo = 27
            Case "RB7": PinNo = 28

            Case "RC0": PinNo = 11
            Case "RC1": PinNo = 12
            Case "RC2": PinNo = 13
            Case "RC3": PinNo = 14
            Case "RC4": PinNo = 15
            Case "RC5": PinNo = 16
            Case "RC6": PinNo = 17
            Case "RC7": PinNo = 18

            Case "RE3": PinNo = 1
        End Select

        Case 40:
        Select Case PinName:
            Case "RA0": PinNo = 2
            Case "RA1": PinNo = 3
            Case "RA2": PinNo = 4
            Case "RA3": PinNo = 5
            Case "RA4": PinNo = 6
            Case "RA5": PinNo = 7
            Case "RA6": PinNo = 14
            Case "RA7": PinNo = 13

            Case "RB0": PinNo = 33
            Case "RB1": PinNo = 34
            Case "RB2": PinNo = 35
            Case "RB3": PinNo = 36
            Case "RB4": PinNo = 37
            Case "RB5": PinNo = 38
            Case "RB6": PinNo = 39
            Case "RB7": PinNo = 40

            Case "RC0": PinNo = 15
            Case "RC1": PinNo = 16
            Case "RC2": PinNo = 17
            Case "RC3": PinNo = 18
            Case "RC4": PinNo = 23
            Case "RC5": PinNo = 24
            Case "RC6": PinNo = 25
            Case "RC7": PinNo = 26

            Case "RD0": PinNo = 19
            Case "RD1": PinNo = 20
            Case "RD2": PinNo = 21
            Case "RD3": PinNo = 22
            Case "RD4": PinNo = 27
            Case "RD5": PinNo = 28
            Case "RD6": PinNo = 29
            Case "RD7": PinNo = 30

            Case "RE0": PinNo = 8
            Case "RE1": PinNo = 9
            Case "RE2": PinNo = 10
            Case "RE3": PinNo = 1

        End Select

        Case 64:
        Select Case PinName:
            Case "RA0": PinNo = 24
            Case "RA1": PinNo = 23
            Case "RA2": PinNo = 22
            Case "RA3": PinNo = 21
            Case "RA4": PinNo = 28
            Case "RA5": PinNo = 27
            Case "RA6": PinNo = 40
            Case "RA7": PinNo = 39

            Case "RB0": PinNo = 48
            Case "RB1": PinNo = 47
            Case "RB2": PinNo = 46
            Case "RB3": PinNo = 45
            Case "RB4": PinNo = 44
            Case "RB5": PinNo = 43
            Case "RB6": PinNo = 42
            Case "RB7": PinNo = 37

            Case "RC0": PinNo = 30
            Case "RC1": PinNo = 29
            Case "RC2": PinNo = 33
            Case "RC3": PinNo = 34
            Case "RC4": PinNo = 35
            Case "RC5": PinNo = 36
            Case "RC6": PinNo = 31
            Case "RC7": PinNo = 32

            Case "RD0": PinNo = 58
            Case "RD1": PinNo = 55
            Case "RD2": PinNo = 54
            Case "RD3": PinNo = 53
            Case "RD4": PinNo = 52
            Case "RD5": PinNo = 51
            Case "RD6": PinNo = 50
            Case "RD7": PinNo = 49

            Case "RE0": PinNo = 2
            Case "RE1": PinNo = 1
            Case "RE2": PinNo = 64
            Case "RE3": PinNo = 63
            Case "RE4": PinNo = 62
            Case "RE5": PinNo = 61
            Case "RE6": PinNo = 60
            Case "RE7": PinNo = 59

            Case "RF0": PinNo = 18
            Case "RF1": PinNo = 17
            Case "RF2": PinNo = 16
            Case "RF3": PinNo = 15
            Case "RF4": PinNo = 14
            Case "RF5": PinNo = 13
            Case "RF6": PinNo = 12
            Case "RF7": PinNo = 11

            Case "RG0": PinNo = 3
            Case "RG1": PinNo = 4
            Case "RG2": PinNo = 5
            Case "RG3": PinNo = 6
            Case "RG4": PinNo = 8
            Case "RG5": PinNo = 7

        End Select

        Case 80:
        Select Case PinName:
            Case "RA0": PinNo = 30
            Case "RA1": PinNo = 29
            Case "RA2": PinNo = 28
            Case "RA3": PinNo = 27
            Case "RA4": PinNo = 34
            Case "RA5": PinNo = 33
            Case "RA6": PinNo = 50
            Case "RA7": PinNo = 49

            Case "RB0": PinNo = 58
            Case "RB1": PinNo = 57
            Case "RB2": PinNo = 56
            Case "RB3": PinNo = 55
            Case "RB4": PinNo = 54
            Case "RB5": PinNo = 53
            Case "RB6": PinNo = 52
            Case "RB7": PinNo = 47

            Case "RC0": PinNo = 36
            Case "RC1": PinNo = 35
            Case "RC2": PinNo = 43
            Case "RC3": PinNo = 44
            Case "RC4": PinNo = 45
            Case "RC5": PinNo = 46
            Case "RC6": PinNo = 37
            Case "RC7": PinNo = 38

            Case "RD0": PinNo = 72
            Case "RD1": PinNo = 69
            Case "RD2": PinNo = 68
            Case "RD3": PinNo = 67
            Case "RD4": PinNo = 66
            Case "RD5": PinNo = 65
            Case "RD6": PinNo = 64
            Case "RD7": PinNo = 63

            Case "RE0": PinNo = 4
            Case "RE1": PinNo = 3
            Case "RE2": PinNo = 78
            Case "RE3": PinNo = 77
            Case "RE4": PinNo = 76
            Case "RE5": PinNo = 75
            Case "RE6": PinNo = 74
            Case "RE7": PinNo = 73

            Case "RF0": PinNo = 24
            Case "RF1": PinNo = 23
            Case "RF2": PinNo = 18
            Case "RF3": PinNo = 17
            Case "RF4": PinNo = 16
            Case "RF5": PinNo = 15
            Case "RF6": PinNo = 14
            Case "RF7": PinNo = 13

            Case "RG0": PinNo = 5
            Case "RG1": PinNo = 6
            Case "RG2": PinNo = 7
            Case "RG3": PinNo = 8
            Case "RG4": PinNo = 10
            Case "RG5": PinNo = 9
            'Case "RG6": PinNo =
            'Case "RG7": PinNo =

            Case "RH0": PinNo = 79
            Case "RH1": PinNo = 80
            Case "RH2": PinNo = 1
            Case "RH3": PinNo = 2
            Case "RH4": PinNo = 22
            Case "RH5": PinNo = 21
            Case "RH6": PinNo = 20
            Case "RH7": PinNo = 19

            'Case "RI0": PinNo =
            'Case "RI1": PinNo =
            'Case "RI2": PinNo =
            'Case "RI3": PinNo =
            'Case "RI4": PinNo =
            'Case "RI5": PinNo =
            'Case "RI6": PinNo =
            'Case "RI7": PinNo =

            Case "RJ0": PinNo = 62
            Case "RJ1": PinNo = 61
            Case "RJ2": PinNo = 60
            Case "RJ3": PinNo = 59
            Case "RJ4": PinNo = 39
            Case "RJ5": PinNo = 40
            Case "RJ6": PinNo = 41
            Case "RJ7": PinNo = 42

        End Select

        Case 100:
        Select Case PinName:
            Case "RA0": PinNo = 35
            Case "RA1": PinNo = 34
            Case "RA2": PinNo = 33
            Case "RA3": PinNo = 32
            Case "RA4": PinNo = 42
            Case "RA5": PinNo = 41
            'Case "RA6": PinNo =
            'Case "RA7": PinNo =

            Case "RB0": PinNo = 5
            Case "RB1": PinNo = 6
            Case "RB2": PinNo = 7
            Case "RB3": PinNo = 8
            Case "RB4": PinNo = 69
            Case "RB5": PinNo = 68
            Case "RB6": PinNo = 67
            Case "RB7": PinNo = 57

            Case "RC0": PinNo = 44
            Case "RC1": PinNo = 43
            Case "RC2": PinNo = 53
            Case "RC3": PinNo = 54
            Case "RC4": PinNo = 55
            Case "RC5": PinNo = 56
            Case "RC6": PinNo = 45
            Case "RC7": PinNo = 46

            Case "RD0": PinNo = 92
            Case "RD1": PinNo = 91
            Case "RD2": PinNo = 90
            Case "RD3": PinNo = 89
            Case "RD4": PinNo = 88
            Case "RD5": PinNo = 87
            Case "RD6": PinNo = 84
            Case "RD7": PinNo = 83

            'Case "RE0": PinNo =
            'Case "RE1": PinNo =
            Case "RE2": PinNo = 98
            Case "RE3": PinNo = 97
            Case "RE4": PinNo = 96
            Case "RE5": PinNo = 95
            Case "RE6": PinNo = 94
            Case "RE7": PinNo = 93

            Case "RF0": PinNo = 12
            Case "RF1": PinNo = 28
            Case "RF2": PinNo = 23
            Case "RF3": PinNo = 22
            Case "RF4": PinNo = 21
            Case "RF5": PinNo = 20
            Case "RF6": PinNo = 19
            Case "RF7": PinNo = 18

            Case "RG0": PinNo = 71
            Case "RG1": PinNo = 70
            Case "RG2": PinNo = 52
            Case "RG3": PinNo = 53
            Case "RG4": PinNo = 14
            Case "RG5": PinNo = 11
            Case "RG6": PinNo = 10
            Case "RG7": PinNo = 38

            Case "RH0": PinNo = 99
            Case "RH1": PinNo = 100
            Case "RH2": PinNo = 1
            Case "RH3": PinNo = 2
            Case "RH4": PinNo = 27
            Case "RH5": PinNo = 26
            Case "RH6": PinNo = 25
            Case "RH7": PinNo = 24

            'Case "RI0": PinNo =
            'Case "RI1": PinNo =
            'Case "RI2": PinNo =
            'Case "RI3": PinNo =
            'Case "RI4": PinNo =
            'Case "RI5": PinNo =
            'Case "RI6": PinNo =
            'Case "RI7": PinNo =

            Case "RJ0": PinNo = 49
            Case "RJ1": PinNo = 50
            Case "RJ2": PinNo = 66
            Case "RJ3": PinNo = 61
            Case "RJ4": PinNo = 47
            Case "RJ5": PinNo = 48
            Case "RJ6": PinNo = 58
            Case "RJ7": PinNo = 39

        End Select

        'Make special adjustments for some chips
        If (ChipName = "16C505" or ChipName = "16F505") AND Left(PinName, 2) = "RB" Then
            Select Case PinName
                Case "RB0": PinNo = 13
                Case "RB1": PinNo = 12
                Case "RB2": PinNo = 11
                Case "RB3": PinNo = 4
                Case "RB4": PinNo = 3
                Case "RB5": PinNo = 2
            End Select
        End If

    End Select

    Return PinNo
End Function

Sub GuessChipPinout

  Dim As String SFRName
  Dim As Integer CurrBit, PinNo, CheckPin, PinUnique

  'Guess pinout if none found
  If PinoutCount > 0 Then Exit Sub
  'Can only work if number of pins known
  If ChipPins = 0 Then
    Print "No pinout found, and could not guess"
    Exit Sub
  End If

  'Guess package type
  PinoutCount = 1
  With Pinouts(1)
    If ChipPins <= 40 Then
      .Name = "DIP"
    Else
      .Name = "TQFP"
    End If
    .PinCount = 0
  End With

  'Guess pins
  For CurrBit = 1 To SVBC

    SFRName = LCase(SysVarBits(CurrBit).Reg)

    If ((Left(ChipName, 2) = "12" Or Left(ChipName, 2) = "10") And (SFRName = "gpio" Or Left(SFRName, 4) = "port")) Or _
           ((Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") And Left(SFRName, 4) = "port") Then

            PinNo = GetPin(SysVarBits(CurrBit).Name, ChipPins)

            If PinNo > 0 Then
              'Pin already added?
              PinUnique = -1
              For CheckPin = 1 To Pinouts(1).PinCount
                If PinNo = Pinouts(1).PinList(CheckPin).Number Then
                  PinUnique = 0
                  Exit For
                End If
              Next
              If PinUnique Then
                With Pinouts(1)
                  .PinCount += 1
                  With .PinList(.PinCount)
                    .Number = PinNo
                    .PinName = SysVarBits(CurrBit).Name
                    .FunctionCount = 1
                    .FunctionList(1, 1) = .PinName
                    .FunctionList(1, 2) = "IO"
                  End With
                End With
              End If

            End If

    End If
  Next

End Sub

Sub GuessDefaultConfig
  'Guess default config for 18F if none found in .dev file, or .dev file missing

  If DefConf > 0 Or Left(ChipName, 2) <> "18" Then Exit Sub

  ConfigGuessed = -1

  Dim As Integer CurrConf
  Dim As String ChosenOption
  Dim As Integer CurrLoc

  'Search through all found config settings
  For CurrConf = 1 To ConfigOptions


'erv      'some debug... I feel that I will be back...
'      if  ConfigOption(CurrConf).name = "FOSC2" then
'        print   ConfigOption(CurrConf).name,
'        print   ConfigOption(CurrConf).Choices,
'        print   ConfigOption(CurrConf).DefaultGuess
'      end if



    'Choose default
    ChosenOption = ConfigOption(CurrConf).DefaultGuess
    'print "ChosenOption = " + ChosenOption
    If ChosenOption = "" Then
      ChosenOption = ConfigOption(CurrConf).Choices
      'print "1. ChosenOption = " + ChosenOption,
      If InStr(ChosenOption, ",") <> 0 Then

        ChosenOption = Trim(Left(ChosenOption, InStr(ChosenOption, ",") - 1))

        'Print ChosenOption

      End If
    End If

    'Set default
    DefConf += 1
    'print ConfigOption(CurrConf).Name
    'Change to a $ delimiter to resovle items with _ in .name
    DefConfig(DefConf, 0) = ConfigOption(CurrConf).Name + "$" + ChosenOption


  Next

  'Need to set up config register masks as well
  CalcConfigMasks

End Sub

Sub GetPowerPins (ChipPins As Integer, OscPins As Integer, MCLRPin As Integer)
    PowerPinCount = 0

    Select Case(ChipPins)
    Case 6:
        PowerPinCount += 1: PowerPin(PowerPinCount) = "7,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "2,Vdd"

    Case 8:
        PowerPinCount += 1: PowerPin(PowerPinCount) = "8,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "1,Vdd"

    Case 14:
        PowerPinCount += 1: PowerPin(PowerPinCount) = "14,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "1,Vdd"

    Case 18:
        If Not MCLRPin Then PowerPinCount += 1: PowerPin(PowerPinCount) = "4,MCLR"
        If Not OSCPins Then
            PowerPinCount += 1: PowerPin(PowerPinCount) = "16,OSC1"
            PowerPinCount += 1: PowerPin(PowerPinCount) = "15,OSC2"
        End If
        PowerPinCount += 1: PowerPin(PowerPinCount) = "5,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "14,Vdd"

    Case 20:
        PowerPinCount += 1: PowerPin(PowerPinCount) = "20,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "1,Vdd"

    Case 28:
        If Not MCLRPin Then PowerPinCount += 1: PowerPin(PowerPinCount) = "1,MCLR"
        If Not OSCPins Then
            PowerPinCount += 1: PowerPin(PowerPinCount) = "9,OSC1"
            PowerPinCount += 1: PowerPin(PowerPinCount) = "10,OSC2"
        End If
        PowerPinCount += 1: PowerPin(PowerPinCount) = "8,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "19,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "20,Vdd"

    Case 40:
        If Not MCLRPin Then PowerPinCount += 1: PowerPin(PowerPinCount) = "1,MCLR"
        If Not OSCPins Then
            PowerPinCount += 1: PowerPin(PowerPinCount) = "13,OSC1"
            PowerPinCount += 1: PowerPin(PowerPinCount) = "14,OSC2"
        End If
        PowerPinCount += 1: PowerPin(PowerPinCount) = "12,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "11,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "31,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "32,Vdd"

    Case 64:
        If Not MCLRPin Then PowerPinCount += 1: PowerPin(PowerPinCount) = "7,MCLR"
        If Not OSCPins Then
            PowerPinCount += 1: PowerPin(PowerPinCount) = "39,OSC1"
            PowerPinCount += 1: PowerPin(PowerPinCount) = "40,OSC2"
        End If
        PowerPinCount += 1: PowerPin(PowerPinCount) = "9,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "10,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "19,AVdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "20,AVss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "25,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "26,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "38,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "41,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "56,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "57,Vdd"

    Case 80:
        If Not MCLRPin Then PowerPinCount += 1: PowerPin(PowerPinCount) = "9,MCLR"
        If Not OSCPins Then
            PowerPinCount += 1: PowerPin(PowerPinCount) = "49,OSC1"
            PowerPinCount += 1: PowerPin(PowerPinCount) = "50,OSC2"
        End If
        PowerPinCount += 1: PowerPin(PowerPinCount) = "11,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "12,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "25,AVdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "26,AVss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "31,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "32,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "48,Vdd"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "51,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "70,Vss"
        PowerPinCount += 1: PowerPin(PowerPinCount) = "71,Vdd"

    End Select
End Sub

Sub GuessPinAltFunctions (PinCount As Integer)
  'Guess alternate pin functions that may be present on chip but not listed
  'in any input files

  'I2C/SPI pins
  If HasSFR("SSPCON") Or HasSFR("SSPCON1") Then
    'Print "Found SSP module"

    '20 pin chips, with SSP on weird mix of pins
    If PinCount = 20 Then
      AddPinFunction("SCL", "RB6", "IO")
      AddPinFunction("SCK", "RB6", "IO")
      AddPinFunction("SDA", "RB4", "IO")
      AddPinFunction("SDI", "RB4", "I")
      AddPinFunction("SDO", "RC7", "O")

    'Chips with SSP on PORTC (mostly 28+ pin ones)
    'Correct for 16F882, 16F1936, 18F4620
    ElseIf HasSFR("PORTC") Then
      AddPinFunction("SCL", "RC3", "IO")
      AddPinFunction("SCK", "RC3", "IO")
      AddPinFunction("SDA", "RC4", "IO")
      AddPinFunction("SDI", "RC4", "I")
      AddPinFunction("SDO", "RC5", "O")

    'Chips with SSP on PORTB (18 pin ones)
    'Correct for 16F819
    ElseIf HasSFR("PORTB") Then
      AddPinFunction("SCL", "RB4", "IO")
      AddPinFunction("SCK", "RB4", "IO")
      AddPinFunction("SDA", "RB1", "IO")
      AddPinFunction("SDI", "RB1", "I")
      AddPinFunction("SDO", "RB2", "O")
    End If

  End If


End Sub

Sub Read18FASMConfigFile

  Dim as String Read18FConfigLine


  OPEN "18FDefaultASMConfig.TXT" For Input As #1

  Read18FConfigArray = 0
  Do while Not EOF(1)

            Line Input #1, Read18FConfigLine
            'populate array
            Read18FConfigLines(Read18FConfigArray) = Read18FConfigLine
            'print Read18FConfigLine
            Read18FConfigArray = Read18FConfigArray + 1

  Loop
  Close


End Sub


Sub ReadCriticalChanges

  Dim as String ReadCriticalDatChanges


  OPEN "CriticalChanges.txt" For Input As #1

  CriticalChangeIndex = 0
  Do while Not EOF(1)

            Line Input #1, ReadCriticalDatChanges
            if (left(ucase(ReadCriticalDatChanges),1)) <> ";" then
              'populate array
              ReadCriticalDatLine(CriticalChangeIndex) = ReadCriticalDatChanges
              'fprint ReadCriticalDatLine(CriticalChangeIndex)
              CriticalChangeIndex = CriticalChangeIndex + 1
            end if
  Loop
  Close
  if CriticalChangeIndex > 0 then CriticalChangeIndex=CriticalChangeIndex-1

End Sub

Sub ReadDevFiles (DevFileName As String)

    Dim As String DataSource, PinKey, PinDir, PinPort, ConfName, TempData, DTempData
    Dim As Integer FoundPin, SL, CurrentPin, ReadingCFG, CurrentWord, PinNo
    Dim As Integer CurrentMask, DefaultMask, CD, FCO, T

    OPEN DevFileName For Input As #1
    'PRINT "File " + DIR$(DevFileDir + "\pic" + ChipName + ".dev")

    'Get IO pins
    PinoutCount = 1
    With Pinouts(1)
        If ChipPins <= 40 Then
            .Name = "DIP"
        Else
            .Name = "TQFP"
        End If
        .PinCount = 0
    End With

    Do While Not EOF(1)
        Line Input #1, DataSource

        Do While INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": Loop
        Do While INSTR(DataSource, "  ") <> 0: Replace DataSource, "  ", " ": Loop
        DataSource = Trim(LCase(DataSource))

        If Left(DataSource, 5) = "iopin" Then
            'iopin (key=RB1 dir=inout)
            PinKey = Mid(DataSource, INSTR(DataSource, "key=") + 4)
            PinKey = UCase(Left(PinKey, INSTR(PinKey, " ") - 1))
            PinDir = Mid(DataSource, INSTR(DataSource, "dir=") + 4)
            PinDir = Left(PinDir, INSTR(PinDir, ")") - 1)
            If INSTR(PinDir, "in") <> 0 Then Replace PinDir, "in", "I"
            If INSTR(PinDir, "out") <> 0 Then Replace PinDir, "out", "O"

            PinNo = GetPin(PinKey, ChipPins)

      With Pinouts(PinoutCount)
        'Print PinNo, PinKey$, PinDir$

            .PinCount += 1
            With .PinList(.PinCount)
              .PinName = PinKey
              .Number = PinNo
              .FunctionCount = 1
              .FunctionList(.FunctionCount, 1) = PinKey
              .FunctionList(.FunctionCount, 2) = PinDir
            End With
      End With
    End If
  Loop

    'Get pin functions
    Seek #1, 1
    Do While Not EOF(1)
        Line Input #1, DataSource

        Do While INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": Loop
        Do While INSTR(DataSource, "  ") <> 0: Replace DataSource, "  ", " ": Loop
        DataSource = Trim(LCase(DataSource))

        IF Left(DataSource, 7) = "pinfunc" Then
            'pinfunc (key=AN0 port=RA0 dir=in)
            PinKey = Mid(DataSource, INSTR(DataSource, "key=") + 4)
            PinKey = UCase(Left(PinKey, INSTR(PinKey, " ") - 1))
            PinPort = Trim(Mid(DataSource, INSTR(DataSource, "port=") + 5))
            PinPort = UCase(Left(PinPort, INSTR(PinPort, " ") - 1))
            PinDir = Mid(DataSource, INSTR(DataSource, "dir=") + 4)
            PinDir = Left(PinDir, INSTR(PinDir, ")") - 1)
            If INSTR(PinDir, "in") <> 0 Then Replace PinDir, "in", "I"
            If INSTR(PinDir, "out") <> 0 Then Replace PinDir, "out", "O"

            'Find pin
            AddPinFunction(PinKey, PinPort, PinDir)

        End If

    Loop

    'Plan B for getting pinout data
    Dim As String SFRName, SFRAccess, SFRBits(8)
    If Pinouts(1).PinCount = 0 Then
        Seek #1, 1
        Do While Not Eof(1)
            Line Input #1, DataSource
            DataSource = LCase(Trim(DataSource))
            If Left(DataSource, 3) = "sfr" Then
                SFRName = Mid(DataSource, Instr(DataSource, "key") + 3)
                SFRName = Trim(Mid(SFRName, Instr(SFRName, "=") + 1))
                SFRName = Left(SFRName, Instr(SFRName, " ") - 1)

                If ((Left(ChipName, 2) = "12" Or Left(ChipName, 2) = "10") And (SFRName = "gpio" Or Left(SFRName, 4) = "port")) Or _
                   ((Left(ChipName, 2) = "16" Or Left(ChipName, 2) = "18") And Left(SFRName, 4) = "port") Then

                    SFRAccess = Mid(DataSource, Instr(DataSource, "access") + 3)
                    SFRAccess = Mid(SFRAccess, Instr(SFRAccess, "'") + 1)
                    SFRAccess = Trim(Left(SFRAccess, Instr(SFRAccess, "'") - 1))

                    For SL = 8 to 2 Step -1
                        SFRBits(SL) = Left(SFRAccess, Instr(SFRAccess, " ") - 1)
                        SFRAccess = Trim(Mid(SFRAccess, Instr(SFRAccess, " ") + 1))
                    Next
                    SFRBits(1) = Trim(SFRAccess)

                    For CurrentPin = 1 to 8
                        If SFRBits(CurrentPin) <> "u" Then
                            With Pinouts(1)
                                .PinCount += 1
                                With .PinList(.PinCount)
                                    If SFRName = "gpio" Then
                                        .PinName = "GP" + Str(CurrentPin - 1)
                                    Else
                                        .PinName = "R" + UCase(Right(SFRName, 1)) + Str(CurrentPin - 1)
                                    End If
                                    .Number = GetPin(.PinName, ChipPins)
                                    .FunctionCount = 1
                                    .FunctionList(1, 1) = .PinName
                                    SFRAccess = ""
                                    If Instr(SFRBits(CurrentPin), "r") <> 0 Then SFRAccess = "I"
                                    If Instr(SFRBits(CurrentPin), "w") <> 0 Then SFRAccess += "O"
                                    .FunctionList(1, 2) = SFRAccess
                                End With
                            End With
                        End If

                    Next

                End If
            End If
        Loop
    End If

    'Get default assembler config (18 only)
    IF Left(ChipName, 2) = "18" THEN
        SEEK #1, 1
        DefConf = 0
        ConfMask = 0
        ReadingCFG = 0
        CurrentWord = 0
        CurrentMask = 0
        DefaultMask = 0
        FOR CD = 1 to 20
            ConfigMask(CD) = 0
        NEXT

        DO WHILE NOT EOF(1)
            LINE INPUT #1, DataSource

            IF DataSource = "" THEN ReadingCFG = 0

            IF LCase(Left(DataSource, 7)) = "cfgbits" THEN
                ReadingCFG = 1
                TempData = LCase(Mid(DataSource, INSTR(LCase(DataSource), "config") + 6))
                TempData = Left(TempData, INSTR(TempData, " ") - 1)
                CurrentWord = VAL(TempData) * 2
                If INSTR(TempData, "l") <> 0 Then CurrentWord -= 1
                GOTO LoadNextConfLine
            END IF

            IF ReadingCFG = 1 THEN
                TempData = DataSource
                Do WHILE INSTR(TempData, Chr(9)) <> 0: Replace TempData, Chr(9), "": LOOP
                TempData = LCase(Trim(TempData))

                IF Left(TempData, 5) = "field" THEN
                    'Get mask and default value
                    DTempData = Trim(Mid(TempData, INSTR(TempData, "mask=") + 5))
                    IF INSTR(DTempData, " ") <> 0 THEN DTempData = Left(DTempData, INSTR(DTempData, " ") - 1)
                    IF Left(DTempData, 2) = "0x" THEN DTempData = "&H" + Mid(DTempData, 3)
                    CurrentMask = VAL(DTempData)
                    ConfigMask(CurrentWord) = ConfigMask(CurrentWord) OR CurrentMask
                    If CurrentWord > ConfMask Then ConfMask = CurrentWord
                    IF INSTR(TempData, "init=") <> 0 THEN
                        DTempData = Trim(Mid(TempData, INSTR(TempData, "init=") + 5))
                        IF INSTR(DTempData, " ") <> 0 THEN DTempData = Left(DTempData, INSTR(DTempData, " ") - 1)
                        IF Left(DTempData, 2) = "0x" THEN DTempData = "&H" + Mid(DTempData, 3)
                        DefaultMask = VAL(DTempData)
                    END IF
                    IF INSTR(TempData, "init=") = 0 THEN DefaultMask = CurrentMask

                    'Find matching setting
                    'and with not mask, then or with test value and compare
                    'Print CurrentWord, DefaultMask, CurrentMask

                    FOR FCO = 1 to COC
                        With Config(FCO)
                            If .Value + CurrentMask = 255 AND CurrentWord = .Location THEN
                                ConfName = .Name
                                ConfName = Left(ConfName, INSTR(ConfName, "_") - 1)
                                FOR T = 1 to COC
                                    'IF INSTR(ConfigOp$(T), N$) <> 0 AND VAL(ConfigVal$(T)) + (CurrentMask - DefaultMask) = 255 AND CurrentWord = ConfigLoc%(T) THEN
                                    IF INSTR(Config(T).Name, ConfName) <> 0 AND Config(T).Value = (255 AND (NOT CurrentMask) OR DefaultMask) AND CurrentWord = Config(T).Location THEN
                                        DefConf += 1
                                        DefConfig(DefConf, 0) = Config(T).Name
                                    END IF
                                NEXT
                                EXIT FOR
                            END IF
                        End With
                    NEXT


                END IF

            END IF

            LoadNextConfLine:

        LOOP
    END IF

    CLOSE

End Sub

Sub ReadMPASMInfo
  'If no config bits found, try to read them from the MPASM 8bit_device.info file
  If COC <> 0 And ConfigGuessed = 0 And DefConf <> 0 Then Exit Sub
  'Print "Reading config from MPASM info"

  Dim As Integer f, CorrectChip, LineItems, CurrItem, NoConfig, CurrDefault, CurrOption
  Dim As Integer OptionMask, CurrValue, FirstUnderscore, SecondUnderscore
  Dim As String InLine, LineItem(50)

  Dim As LongInt CurrWordLoc
  Dim As Integer CurrWordMask, CurrSettingMask, CurrSettingValMask, DefWordValue(50), PD
  Dim As Integer LoadConfig
  Dim As String CurrSetting, CurrSettingVal

  LoadConfig = 0
  If COC = 0 Then LoadConfig = -1
  'Print "Load config is "; LoadConfig

  CorrectChip = 0
  f = FreeFile


  Open MPASMInfoFile For Input As #f
  Do While Not Eof(f)
    Line Input #f, InLine
    If Left(InLine, 16) = "<PART_INFO_TYPE>" Then
      If InStr(InLine, "<PIC" + UCase(ChipName) + ">") <> 0 Then
        CorrectChip = -1
      Else
        CorrectChip = 0
      End If

    ElseIf CorrectChip Then

      LineItems = SplitInfoLine(InLine, LineItem())

      Select Case LineItem(1)
        Case "CONFIGREG_INFO_TYPE":
          'Select new config word
          CurrWordLoc = Val("&H" + LineItem(2))
          CurrWordMask = 0 'Val("&H" + LineItem(4))
          'Store word mask
          If CurrWordLoc >= &H300000 Then

            CurrWordLoc = CurrWordLoc - &H2FFFFF
          ElseIf CurrWordLoc >= &H1FFF0 Then
            CurrWordLoc = CurrWordLoc - &H1FFEF
          Else
            CurrWordLoc = (CurrWordLoc And &HFFF) - &HFF7
          End If
          If CurrWordLoc > 0 And CurrWordLoc <= 50 Then
            'Store default word value
            DefWordValue(CurrWordLoc) = Val("&H" + LineItem(4))

            'Fix for 'K42, does this break anything?
          If Val("&H" + LineItem(4)) = 255 Then
            CurrWordMask = 255
            ConfigMask(CurrWordLoc) = 255
            'Print "Forcing config mask for word "; CurrWordLoc; " to 255"
          End If

          End If
          If LoadConfig Then
            If CurrWordLoc < 1 Or CurrWordLoc > 16 Then
              Print "Odd config word number: "; CurrWordLoc
              COC = 0
              ConfigOptions = 0
              Close #f
              Exit Sub
            End If
            ConfigMask(CurrWordLoc) = CurrWordMask
          End If

        Case "SWITCH_INFO_TYPE":
          'Found new setting
          If LoadConfig Then
            CurrSetting = LineItem(2)
            CurrSettingMask = Val("&H" + LineItem(4))
            ConfigMask(CurrWordLoc) = ConfigMask(CurrWordLoc) Or CurrSettingMask
            ConfigOptions += 1
            ConfigOption(ConfigOptions).Name = CurrSetting
            ConfigOption(ConfigOptions).Choices = ""
          End If

        Case "SETTING_VALUE_TYPE":
          'Found a new value for a setting
          If LoadConfig Then

            CurrSettingVal = LineItem(2)
            CurrSettingValMask = Val("&H" + LineItem(Lineitems))
            'Print CurrWordLoc, CurrSetting, CurrSettingVal
            COC += 1
            With Config(COC)
              .Name = CurrSetting + "_" + CurrSettingVal
              .Location = CurrWordLoc
              .Value = (255 And (Not CurrSettingMask)) Or CurrSettingValMask
              .SettingName = CurrSetting
            End With

            If ConfigOption(ConfigOptions).Choices = "" Then
              ConfigOption(ConfigOptions).Choices = CurrSettingVal
            Else
              ConfigOption(ConfigOptions).Choices = ConfigOption(ConfigOptions).Choices + "," + CurrSettingVal
            End If
          End If

      End Select

    End If

  Loop

  Close #f

  If LoadConfig Then
    CalcConfigMasks
  End If

  'Find default settings?
  If DefConf = 0 Then
    'Print "Reading default config from MPASM info"

    'Old way, won't work:
    'For each config option, try ANDing with default mask
    'No change means this option has already been ANDed with default mask and therefore is selected

    'New way:
    'Find mask for each option
    'And default mask for word and current option with mask for option, check if they match.

    'Find all options for setting
    'For CurrOption = 1 To COC
    ' 'is this a default?
    ' With Config(CurrOption)
    '   If (.Value And DefWordValue(.Location)) = DefWordValue(.Location) Then
    '     Print "Found default "; .Name
    '   End If
    ' End With
    'Next

    'For each option:
    For CurrOption = 1 To ConfigOptions
      'Find location and mask for that option only
      OptionMask = 255
      CurrWordLoc = -1

      For CurrValue = 1 To COC
        'Is value for this option?
        If Config(CurrValue).SettingName = ConfigOption(CurrOption).Name Then
          OptionMask = OptionMask And Config(CurrValue).Value
          If CurrWordLoc = -1 Then
            CurrWordLoc = Config(CurrValue).Location
          ElseIf CurrWordLoc <> Config(CurrValue).Location Then
            Print "Config word for " + Config(CurrValue).Name + " is unclear"
          End If
        End If
      Next
      OptionMask = (Not OptionMask) And 255

      'Print ConfigOption(CurrOption).Name, CurrWordLoc, OptionMask

      'Now that the mask for a single option is known, find the default one
      For CurrValue = 1 To COC
        If Config(CurrValue).SettingName = ConfigOption(CurrOption).Name And (Config(CurrValue).Value And OptionMask) = (DefWordValue(CurrWordLoc) And OptionMask) Then
          'Print ConfigOption(CurrOption).Name, CurrWordLoc, OptionMask, Config(CurrValue).Name
          CurrSettingVal = Config(CurrValue).Name

          If InStr(CurrSettingVal, "_") <> 0 Then
            'using InStrRev rather than Instr to resolve items with _ in SettingName.....
            if InStr(CurrSettingVal, "WPEND_PAGE_") <> 0 or InStr(CurrSettingVal, "WPFP_PAGE_")  then
                dim as Integer stringpostion
                stringpostion = InStr(CurrSettingVal, "_PAGE_")
                CurrSettingVal = mid(CurrSettingVal, stringpostion-len("_PAGE_")+7 )

            else

                select case tally(CurrSettingVal, "_")
                  case 1:

                    CurrSettingVal = Mid(CurrSettingVal, InStrRev(CurrSettingVal, "_") + 1)
                  case 2,3:
'print ConfigOption(CurrOption).Name,
'print CurrSettingVal +" = ",
                    if ChipFamily = 16 then
                       FirstUnderscore = 0
                       SecondUnderscore = 0
                       FirstUnderscore = InStr(CurrSettingVal, "_")
                       SecondUnderscore = InStr( FirstUnderscore+1, CurrSettingVal, "_")

                       '

                       FOR PD = 1 to ConfigOptions
                            If ConfigOption(CurrOption).Name = ConfigOption(PD).Name then
                              dim tempstr as string
                              tempstr = trim(Mid(CurrSettingVal, FirstUnderscore + 1))
                              'print "<"+tempstr+">",
                              If instr( ConfigOption(PD).Choices, tempstr ) > 0 then
                                  'print "*"+ConfigOption(PD).Choices+"*",
                                  CurrSettingVal = tempstr
                                  goto foundmatch

                              End if
                            End if
                       NEXT

                       'default safety
                       CurrSettingVal = Mid(CurrSettingVal, SecondUnderscore + 1)
                       foundmatch:


'print CurrSettingVal
'print ""
                    else
                       CurrSettingVal = Left(CurrSettingVal, InStrRev(CurrSettingVal, "_") - 1)+";1"

                    end if
                  case else
                    'print ConfigOption(CurrOption).Name,
                    'print CurrSettingVal +" = ",
                    'print
                    CurrSettingVal  = CurrSettingVal + "' multiple underscores not handled in GetChipData.exe ..best fix"
                end select
            end if

          End If

          ConfigOption(CurrOption).DefaultGuess = CurrSettingVal
          Exit For
        End If
      Next
    Next

  End If

End Sub

Function TALLY(SomeString As String,PartString As String) As Long
    Dim As Long LenP=Len(PartString),count
    Dim As Long position=Instr(SomeString,PartString)
    If position=0 Then Return 0
    While position>0
        count+=1
        position=Instr(position+LenP,SomeString,PartString)
    Wend
    Return count
End Function

Sub ReadXmlFiles (ChipName As String)
    Dim As String ChipFile, PinoutFile, DataSource, TypeData, DirData, PolData, NameData
    ChipFile = VDIFileDir + "\PIC" + ChipName + ".xml"
    PowerPinsAdded = 0

    'Check if an xml file for the current chip exists
    If Dir(ChipFile) = "" Then Exit Sub

    'Get the name of the pinout file
    PinoutFile = ""
    Open ChipFile For Input As #1
    Do While Not EOF(1)
        Line Input #1, DataSource
        DataSource = LCase(TRIM(DataSource, Any " " + Chr(9)))

        If Left(DataSource, 20) = "<pindefinitions name" Then
            PinoutFile = MID(DataSource, INSTR(DataSource, Chr(34)) + 1)
            PinoutFile = Left(PinoutFile, INSTR(PinoutFile, Chr(34)) - 1)
            Replace PinoutFile, "/", "\"
            PinoutFile = VDIFileDir + "\" + PinoutFile + ".xml"
            Exit Do
        End If
    Loop
    Close
    If PinoutFile = "" Then Exit Sub

    'Clear out the old pinout data
    PinoutCount = 0

    'Open the pinout file
    Open PinoutFile For Input As #1
    Do While Not EOF(1)
        Line Input #1, DataSource

        DirData = LCase(Trim(DataSource, Any " " + Chr(9)))

        If Left(DirData, 16) = "<pindefinitions " Then
            PinoutCount = 1
            With Pinouts(PinoutCount)
                .Name = UCASE(Mid(DataSource, INSTR(LCASE(DataSource), "type=") + 5))
                .Name = Mid(.Name, INSTR(.Name, Chr(34)) + 1)
                .Name = Left(.Name, INSTR(.Name, Chr(34)) - 1)
                If .Name = "PDIP" Then .Name = "DIP"
                .PinCount = 0
            End With

        ElseIf Left(DirData, 5) = "<pin " Then
            NameData = MID(DirData, INSTR(DirData, "number") + 6)
            NameData = MID(NameData, INSTR(NameData, Chr(34)) + 1)
            NameData = Left(NameData, INSTR(NameData, Chr(34)) - 1)

            With Pinouts(PinoutCount)
                .PinCount += 1
                With .PinList(.PinCount)
                    .PinName = ""
                    .Number = Val(NameData)
                    .FunctionCount = 0
                End With
            End With

        ElseIf Left(DirData, 12) = "<connection " Then
            'Type
            TypeData = Mid(DataSource, INSTR(LCASE(DataSource), "type") + 4)
            TypeData = Mid(TypeData, INSTR(TypeData, Chr(34)) + 1)
            TypeData = LCase(LEFT(TypeData, INSTR(TypeData, Chr(34)) - 1))

            'Name
            NameData = MID(DataSource, INSTR(DataSource, "name") + 4)
            NameData = MID(NameData, INSTR(NameData, Chr(34)) + 1)
            NameData = Left(NameData, INSTR(NameData, Chr(34)) - 1)
            IF INSTR(NameData, "/") <> 0 Then NameData = Left(NameData, INSTR(NameData, "/") - 1)
            If MID(NameData, LEN(NameData) - 1, 1) = "-" Then NameData = Left(NameData, INSTR(NameData, "-") - 1)

            'Polarity (direction)
            PolData = MID(DataSource, INSTR(DataSource, "polarity") + 8)
            PolData = MID(PolData, INSTR(PolData, Chr(34)) + 1)
            PolData = LCASE(Left(PolData, INSTR(PolData, Chr(34)) - 1))

            With Pinouts(PinoutCount)
                With .PinList(.PinCount)
                    .FunctionCount += 1
                    .FunctionList(.FunctionCount, 1) = NameData
                    If TypeData <> "other" Then
                        If PolData = "inp" Then
                            .FunctionList(.FunctionCount, 2) = "I"
                        ElseIf PolData = "out" Then
                            .FunctionList(.FunctionCount, 2) = "O"
                        ElseIf PolData = "i/o" Then
                            .FunctionList(.FunctionCount, 2) = "IO"
                        Else
                            Print "Bad polarity:", PolData
                        End If
                    Else
                        .FunctionList(.FunctionCount, 2) = ""
                    End If
                End With
            End With

        Else

        End If

    Loop
    Close

    PowerPinsAdded = -1

End Sub

SUB Replace (DataVar As String, Find As String, Rep As String)
    Dim As String VarTemp, FindTemp, NewData

    VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
    IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB

    NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
    NewData = NewData + Rep
    NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))

    DataVar = NewData
END SUB

FUNCTION IsDivider (Temp As String) As Integer

    Select Case Temp
    Case " ", "(", ")", ",", ".", ":", ";", "+", "-", "*", "/", "%": Return -1
    Case "=", "!", "<", ">", "{", "}", "~", "&", "|", "#": Return -1
    Case Else: Return 0
    End Select

END Function

Function IsSameConfig(Name1 As String, Name2 As String) As Integer
  Dim As String N1, N2, S1, S2, V1, V2
  Dim As Integer SwapNo
  N1 = UCase(Name1)
  N2 = UCase(Name2)

  If N1 = N2 Then Return -1

  For SwapNo = 1 To 2

    If InStr(N1, "_") <> 0 Then
      S1 = Left(N1, InStr(N1, "_") - 1)
      V1 = Mid(N1, InStr(N1, "_") + 1)
    Else
      S1 = N1
      V1 = ""
    End If
    If InStr(N2, "_") <> 0 Then
      S2 = Left(N2, InStr(N2, "_") - 1)
      V2 = Mid(N2, InStr(N2, "_") + 1)
    Else
      S2 = N2
      V2 = ""
    End If

    If V1 = V2 Then
      If S1 = "WDTE" And S2 = "WDT" Then Return -1
      If S1 = "WDTEN" And S2 = "WDT" Then Return -1
      If S1 = "WDTEN" And S2 = "WDTE" Then Return -1
      If S1 = "MCLRE" And S2 = "MCLR" Then Return -1
      If WholeINSTR("BOREN BODEN BOR BOD", S1) = 2 And WholeINSTR("BOREN BODEN BOR BOD", S2) = 2 Then Return -1
      If S1 = "CCPMX" And S2 = "CCP1" Then Return -1
      If S1 = "PWRTE" And S2 = "PWRT" Then Return -1
      If S1 = "FOSC" And S2 = "OSC" Then Return -1
    End If

    Swap N1, N2
  Next

  Return 0

End Function

Function HasSFR(SFRName As String) As Integer
  Dim As Integer PD
  Dim As String TidiedName

  'Search system variable list to find register
  TidiedName = UCase(Trim(SFRName))

  For PD = 1 To SVC
    If SysVars(PD, 1) = TidiedName Then Return -1
  Next

  Return 0
End Function

Function HasSFRBit(BitName As String) As Integer
  Dim As Integer FSFR
  Dim As String BitNameUpper
  BitNameUpper = UCase(BitName)

  For FSFR = 1 to SVBC
    If UCASE(SysVarBits(FSFR).Name) = BitNameUpper Then
      Return -1
    End If
  Next

  Return 0
End Function

Function SplitInfoLine(DataIn As String, LineItem() As String) As Integer
  'Split line in format <item1><item2><item3>
  Dim As Integer Items
  Dim As String Item, Remaining
  Items = 0

  Remaining = DataIn
  Do While InStr(Remaining, "<") <> 0
    If InStr(Remaining, ">") = 0 Then
      Print "Error: info line missing >"
      Exit Do
    End If

    Item = Left(Remaining, InStr(Remaining, ">") - 1)
    Item = Trim(Mid(Item, InStr(Item, "<") + 1))
    Items += 1
    LineItem(Items) = Item
    Remaining = Mid(Remaining, InStr(Remaining, ">") + 1)
  Loop

  Return Items
End Function

FUNCTION WholeINSTR (DataIn As String, FindIn As String) As Integer
    Dim As String DataSource, Temp, Find
    Dim As Integer T

    DataSource = UCase(DataIn): Find = UCase(FindIn)

    IF INSTR(DataSource, Find) = 0 THEN WholeINSTR = 0: EXIT FUNCTION
    IF LEN(DataSource) = LEN(Find) THEN WholeINSTR = 2: EXIT FUNCTION

    DoWholeINSTRAgain:
    T = 0

    IF INSTR(DataSource, Find) = 1 THEN T = 1
    IF T = 0 THEN
        Temp = Mid(DataSource, INSTR(DataSource, Find) - 1, 1)
        IF IsDivider(Temp) THEN T = 1
    END IF

    IF INSTR(DataSource, Find) + LEN(Find) - 1 = LEN(DataSource) THEN T = T + 1
    IF T < 2 THEN
        Temp = Mid(DataSource, INSTR(DataSource, Find) + LEN(Find), 1)
        IF IsDivider(Temp) THEN T = T + 1
    END IF

    IF T = 1 THEN
        Replace DataSource, Find, ""
        IF INSTR(DataSource, Find) <> 0 THEN GOTO DoWholeINSTRAgain
    END IF

    WholeINSTR = T

END FUNCTION


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


sub ReadIncFileLineIn

    IF HandlingAddingLine = 0 then
        LINE INPUT #1, DataSource
    Else
        DataSource = LastLineRead
        'Print ">";DataSource
        HandlingAddingLine = 0
        LastLineRead=""
        exit sub
    END IF

    'check  is this a file we need to fix using CriticalChanges.txt
    For CurrentChipCheck = lbound(ReadCriticalDatLine) to CriticalChangeIndex
        'print ReadCriticalDatLine(CurrentChipCheck)
        text = ReadCriticalDatLine(CurrentChipCheck)
        MaxCount = 4
        Delimiter = ","
        split  ( text, Delimiter, MaxCount, s() )

        if ucase(s(0)) = ucase("p"+ChipName+".inc")  then

          if ucase(s(1)) = "A" THEN

            if INSTR(ucase(DataSource),ucase(s(2))) <> 0 Then
                'found a match
                    'append a Line
                    LastLineRead = s(3)
                    HandlingAddingLine  = 1
                    'DataSource =  s(3)
	            Print SPC(5);DataSource		
                    Print SPC(5);"(A)";s(3)
                    'ReadCriticalDatLine(CurrentChipCheck)=""
                end if

          END IF

          if ucase(s(1)) = "R" THEN
                do while instr(s(2)," ")<> 0
                    s(2) = replacestr(s(2)," ","")
                loop

                text = DataSource
                do while instr(text," ")<> 0
                    text = replacestr(text," ","")
                loop

                if s(2)=text Then
                    DataSource = s(3)
                    print SPC(5);"(R)";DataSource
                end if

          END IF
        end if




    Next




end sub



function ReplaceStr(ByRef src as String,ByRef find As String,ByRef repl as String,start As Integer=1) as String
   Dim source As String,buf As String
   source = src
   buf = find

    Dim indx As Integer = InStr(start,source,buf)
    If indx=0 Then Return ""
    If indx = 1 Then Return repl & Mid(source,Len(buf)+1)
    Dim n As Integer = 0
    Dim tmp As String
    Do
       n+=1
       If n=indx Then Exit do
       tmp += Mid(source,n,1)
    Loop

    Return tmp & repl & Mid(source,indx + Len(buf))
end Function


Function CompareLFFile ( Fchip as String ) as Integer

  dim FChipLine as string, LFChipLine as string, lfchip as String
  Dim As Integer ff, lff
  lfchip = UCase( Fchip)
  replace lfchip, "F", "LF"

  If Dir(IncFileDir +"\"+fchip) = "" Then
    Print "Error reading " + IncFileDir +"\"+fchip
    Return 0
  End If
  If Dir(IncFileDir +"\"+lfchip) = "" Then
    Print "Error reading " + IncFileDir +"\"+lfchip
    Return 0
  End If

  ff = FreeFile
  open IncFileDir +"\"+fchip for input as #ff
  lff = FreeFile
  open IncFileDir +"\"+lfchip for input as #lff

  'dump all the start stuff
  do while not eof(ff)
    Line input #ff, FChipLine
    Line input #lff, LFChipLine
    If  Instr( FChipLine, "Register Definitions")  Then
        Exit Do
    end if
  loop



  do while not eof(ff)
    Line input #ff, FChipLine
    Line input #lff, LFChipLine

    If FChipLine <> LFChipLine Then

        Close ff
        Close lff
        Return 0

    end if
  Loop

  Close ff
  Close lff


  Return -1
End Function


Sub CalcNoBankString ( localNoBankRAMStr as string )

      'This is workaround as these chips dont seem to support NoBankRAM
      'This just over writes the SharedRamBlock(SRBC) with the last entry... until the compiler is fixed
      Dim lastwrite as Byte
      lastwrite = 0


      if len( localNoBankRAMStr ) <> 0 then

        if instr(localNoBankRAMStr, "|") = 0 then
            SRBC = SRBC + 1: SharedRamBlock(SRBC) = localNoBankRAMStr
        else
            localNoBankRAMStr = localNoBankRAMStr + "|"
            do while instr(localNoBankRAMStr, "|") <> 0
'                if lastwrite = 0 then
                  SRBC = SRBC + 1
'                  lastwrite = 1
'                end if
                SharedRamBlock(SRBC) = mid( localNoBankRAMStr ,1, instr(localNoBankRAMStr, "|") - 1 )
'                print SharedRamBlock(SRBC)
                localNoBankRAMStr =  mid( localNoBankRAMStr , instr(localNoBankRAMStr, "|") + 1 )
            loop
        end if
      end if

End Sub

Function CountUSART as string

dim lCountUSART as string


    lCountUSART = str(ChipUSART)

    if HasSFRBit( "TX2IE" ) or HasSFRBit( "U2IE" ) then
      lCountUSART = "2"
    end if
    if HasSFRBit( "TX3IE" )  or HasSFRBit( "U3IE") then
      lCountUSART = "3"
    end if
    if HasSFRBit( "TX4IE" )  or HasSFRBit( "U4IE") then
      lCountUSART = "4"
    end if
    if HasSFRBit( "TX5IE" )  or HasSFRBit( "U5IE") then
      lCountUSART = "5"
    end if
    if HasSFRBit( "TX6IE" )  or HasSFRBit( "U6IE") then
      lCountUSART = "6"
    end if
    if HasSFRBit( "TX7IE" )  or HasSFRBit( "U7IE") then
      lCountUSART = "7"
    end if

    Return lCountUSART

End Function
