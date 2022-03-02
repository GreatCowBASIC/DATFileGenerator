'Chip data generation from for Great Cow BASIC

#include once "ext/xml/dom.bi"

Type ChipSourceFile
	ChipName As String
	FileName As String
	FileType As Integer '4 = AVR Studio 4, 6 = Atmel Studio 6
End Type

Type ChipPin
    'PinName As String
    Number As Integer
    FunctionList(20, 2) As String
    FunctionCount As Integer
End Type

Type PinOut
    Name As String
    PinCount As Integer
    PinList(100) As ChipPin
End Type

Type IntData
    VectorID As Integer
    Vector As String
    VectorLoc As Integer
    EventName As String
    EnableBit As String
    FlagBit As String
End Type

Type SysVarBit
    Name As String
    Reg As String
    Bit As Integer
End Type

Type SysVar
    Name As String
    Loc As Integer
End Type

Type ExtraRegs
    Name As String
    Loc As Integer
    BitCount As Integer
    Bits(8) As String
End Type

Type ExtraChipData
    Chip As String
    RegCount As Integer
    Regs(50) As ExtraRegs
    BitCount As Integer
    Bits(50) As SysVarBit
    InfoCount As Integer
    Info(20, 2) As String
End Type


Declare Sub AddIOPin(PinList() As ChipPin, PinNumber As Integer, PinName As String)
Declare Sub AddPinFunction(Pin As Integer, Port As String, Direction As String)
Declare Function CheckBit (BitName As String) As Integer
Declare Sub EliminateDuplicates
Declare Sub EliminateDuplicateInterrupts
Declare Function GetIntName (IntVect As String) As String
Declare Function GetBitName (IntVect As String) As String
Declare Function GetSFRBitLoc(BitName As String) As Integer
Declare Function GetSFRLoc(SFRName As String) As Integer
Declare Sub GuessPinouts (PinoutString As String)
Declare Function HasSFR(SFRName As String) As Integer
Declare Function HasSFRBit(BitName As String) As Integer
Declare FUNCTION IsConst (DataSource As String) As Integer
Declare FUNCTION IsDivider (Temp As String) As Integer
Declare Function IsMatch (InExp As String, InString As String) As Integer
Declare Sub LoadExtraData (InFile As String)
Declare Function MakeDec(InVal As String) As Integer
Declare Function MaskToBit(InMask As Integer) As Integer
Declare SUB Replace (DataVar As String, Find As String, Rep As String)
Declare Sub ReadIncFile
Declare Sub ReadXml4 (FileName As String)
Declare Sub ReadXml6 (FileName As String)
Declare Sub SetSFR(SFRName As String, Location As Integer)
Declare Sub SetSFRBit(BitName As String, Reg As String, BitNo As Integer)
Declare FUNCTION WholeINSTR (DataIn As String, FindIn As String) As Integer

Dim As Integer ChipDataCount, ChipCount, StartChip, CurrentChip
Dim As Integer LineSize, InComment, PC, Temp, SpecificSource
Dim As Integer PD, CD, FV, RBC
Dim As Integer FCD, AR, AB, SortMore, PP, PF, FindChip

'Chip info
Dim Shared As Integer ChipRAM, RamStart, ChipFamily, ChipPins, CurrentPinout
Dim Shared As Integer IntC, SVC, ChipADC, ChipIO, LastMask, LastValue, MaxChipAddress
Dim Shared As Integer PinoutCount, HardwareMult, ChipEEPROM, GPRCounTemp
Dim Shared As Integer RegXL, RegXH, RegYL, RegYH, RegZL, RegZH, ILC
Dim Shared As Double ChipMHz, ChipWords
Dim Shared As String ChipName, Packages, ChipNameTidy

Dim Shared As Integer SVBC, EDC
Dim Shared As String ChipData(500)
Dim Shared ChipList(500) As ChipSourceFile
Dim Shared As String ConfigSettings(500, 2)
Dim Shared As String DefConfig(500, 2)
Dim Shared As Integer ConfigMask(20)
Dim Shared IntList(200) As IntData: ILC = 0
Dim Shared ExtraData(100) As ExtraChipData: EDC = 0
Dim shared SysVars(1000) As SysVar
Dim shared SysVarBits(8000) As SysVarBit
Dim Shared As String RamBlock(10)
Dim Shared Pinouts(5) As Pinout
Dim Shared As String IntName

Dim Shared As String InstDir, XmlFileDir, Xml6FileDir, TempDataFile, OutputDir
Dim Shared As String IncFileDir, ExtraDataFile

Dim As String M, D, Y, DataSource, TempData, CurrentDate, CurrFile
'Dim As String CurrentTag, LastRegName, CurrentPinS, TidyIn
Dim As String NewData, NameTemp, ThisChipData, ChipPackageList 'PinNameTemp, PinKey, 

InstDir = "C:\Final\New Chip Data"
XmlFileDir = InstDir + "\Atmel Files\Partdescriptionfiles"
Xml6FileDir = InstDir + "\Atmel Files\devices"
IncFileDir = InstDir + "\Atmel Files\avrassembler"
TempDataFile = InstDir + "\chipdataavr.tmp"
OutputDir = InstDir + "\chipdata"
ExtraDataFile = InstDir + "\avrextras.dat"

TempData = Date
M = Left(TempData, INSTR(TempData, "-") - 1)
TempData = Mid(TempData, INSTR(TempData, "-") + 1)
D = Left(TempData, INSTR(TempData, "-") - 1)
Y = Mid(TempData, INSTR(TempData, "-") + 1)
IF LEFT(D, 1) = "0" Then D = MID(D, 2)
IF LEFT(M, 1) = "0" Then M = MID(M, 2)
CurrentDate = D + "/" + M + "/" + Y

close
PRINT "GCBASIC/GCGB Chip Data File Generator"
PRINT

ChipDataCount = 0

'Read chipdata.csv
PRINT "Reading avr chipdata.csv ..."
ChipDataCount = 0
Open InstDir + "\avr chipdata.csv" for input as #1
do while not eof(1)
    Line input #1, DataSource
    If DataSource <> "" Then
        ChipDataCount += 1
        ChipData(ChipDataCount) = Trim(UCase(DataSource))
    end if
loop
close

'Load extra data
Print "Loading extra data ..."
LoadExtraData ExtraDataFile

'Build list of .xml files
PRINT "Finding .xml files (from AVR Studio 4) ..."
ChipCount = 0
CurrFile = Dir(XmlFileDir + "\*.xml")
Do While CurrFile <> ""
    If Instr(LCase(CurrFile), "comp") = 0 THEN
        
        'Get chip name
	    ChipName = UCASE(CurrFile)
	    DO WHILE INSTR(ChipName, "\") <> 0: ChipName = Mid(ChipName, INSTR(ChipName, "\") + 1): LOOP
	    ChipName = Left(ChipName, INSTR(ChipName, ".") - 1)
	    If Left(ChipName, 2) = "AT" THEN ChipName = Mid(ChipName, 3)
	    ChipName = Trim(ChipName)
	  	
	  	'Don't try to support 89C or xmega chips
   		If Left(ChipName, 2) = "90" Or Left(ChipName, 4) = "MEGA" Or Left(ChipName, 4) = "TINY" Then
	  		ChipCount += 1
        	ChipList(ChipCount).ChipName = ChipName
        	ChipList(ChipCount).FileName = XmlFileDir + "\" + CurrFile
        	ChipList(ChipCount).FileType = 4
	  	End If
        
    END If
    
    CurrFile = Dir
Loop
PRINT "Finding .xml files (from Atmel Studio 6) ..."
CurrFile = Dir(Xml6FileDir + "\*.xml")
Do While CurrFile <> ""
    If Instr(LCase(CurrFile), "comp") = 0 THEN
        
        'Get chip name
	    ChipName = UCASE(CurrFile)
	    DO WHILE INSTR(ChipName, "\") <> 0: ChipName = Mid(ChipName, INSTR(ChipName, "\") + 1): LOOP
	    ChipName = Left(ChipName, INSTR(ChipName, ".") - 1)
	    If Left(ChipName, 2) = "AT" THEN ChipName = Mid(ChipName, 3)
	    ChipName = Trim(ChipName)
	  	
	  	'Only read 90, tiny, mega chips
	  	If Left(ChipName, 2) = "90" Or Left(ChipName, 4) = "MEGA" Or Left(ChipName, 4) = "TINY" Then
	  		
	  		'Old behaviour: Only read files not found from Studio 4
	  		'New behaviour: use 6 files rather than 4 when 6 is available
	  		For FindChip = 1 To ChipCount
	  			If ChipList(FindChip).ChipName = ChipName Then
	  				With ChipList(FindChip)
	  					.FileName = Xml6FileDir + "\" + CurrFile
	  					.FileType = 6
	  				End With
	  				GoTo SkipChipFile
	  			End If
	  		Next
	  		
	  		ChipCount += 1
        	ChipList(ChipCount).ChipName = ChipName
        	ChipList(ChipCount).FileName = Xml6FileDir + "\" + CurrFile
        	ChipList(ChipCount).FileType = 6
        	
        	SkipChipFile:
	  	End If
        
    END If
    
    CurrFile = Dir
Loop
Print
PRINT

'Get start location
StartChip = 1
IF INSTR(COMMAND, "START") <> 0 THEN StartChip = VAL(MID(COMMAND, 6))
SpecificSource = -1
If Command = "V4" Then SpecificSource = 4
If Command = "V6" Then SpecificSource = 6

'Convert .xml to .dat
Print "Processing ..."
FOR CurrentChip = StartChip to ChipCount
  	
  	ChipName = ChipList(CurrentChip).ChipName
  	
    IF Command <> "" AND StartChip = 1 And SpecificSource = -1 And Left(UCase(ChipName), LEN(Command)) <> UCase(Command) THEN GOTO SkipChip
	If SpecificSource <> -1 And SpecificSource <> ChipList(CurrentChip).FileType Then GoTo SkipChip

    Print SPC(5); ChipName; CurrentChip; "/"; ChipCount
    
    'Clear counters
    IntC = 0
    SVC = 0
    SVBC = 0
    ChipADC = 0
    ChipIO = 0
    LastMask = 0
    LastValue = 0
    MaxChipAddress = 0
    RegXL = 0: RegXH = 0
    RegYL = 0: RegYH = 0
    RegZL = 0: RegZH = 0
    Packages = ""
    PinoutCount = 0
    ILC = 0
    
    'Clear values
    ChipMHz = -1
    ChipIO = -1
    ChipADC = -1
    ChipWords = 0
    ChipRAM = 0
    
    'Read .xml
    If ChipList(CurrentChip).FileType = 4 Then
    	ReadXml4(ChipList(CurrentChip).FileName)
    Else
    	ReadXml6(ChipList(CurrentChip).FileName)
    	ChipNameTidy = "AT" + LCase(ChipList(CurrentChip).ChipName)
    End If
    
    'Calculate Ram block
    If ChipRAM > 0 Then
        RBC = 1
        RamBlock(1) = Trim(Hex(RamStart)) + ":" + Trim(Hex(RamStart + ChipRAM))
    End If
    
    'MaxAddress
    MaxChipAddress = RamStart + ChipRAM
    
    'Hardware multiply
    HardwareMult = 0
    NameTemp = Trim(LCase(ChipNameTidy))
    IF INSTR(NameTemp, "mega") <> 0 Then HardwareMult = -1
    IF INSTR(NameTemp, "can") <> 0 Then HardwareMult = -1
    IF INSTR(NameTemp, "usb") <> 0 Then HardwareMult = -1
    IF INSTR(NameTemp, "mega103") <> 0 Then HardwareMult = 0
    
	'Obtain chip data
	ThisChipData = ""
	FOR PD = 1 to ChipDataCount
		TempData = UCase(ChipData(PD))
		If Left(TempData, 2) = "AT" THEN TempData = Mid(TempData, 3)
		If InStr(TempData, ",") <> 0 Then
			TempData = Trim(Left(TempData, InStr(TempData, ",") - 1))
		End If
		If TempData = ChipName Then
   			ThisChipData = UCase(ChipData(PD))
   			Exit For
		End If
	Next
	If ThisChipData <> "" Then
		TempData = Mid(ThisChipData, INSTR(ThisChipData, ",") + 1)
		
		'ChipWords = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		'ChipEEPROM = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		'ChipRAM = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		'If ChipIO = -1 Then ChipIO = VAL(TempData)
		ChipIO = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		If ChipADC = -1 Then ChipADC = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		If ChipMHz = -1 Then ChipMHz = VAL(TempData)
		TempData = Mid(TempData, INSTR(TempData, ",") + 1)
		
		ChipPackageList = Trim(TempData, Chr(34))
		
	Else
		'No data in avr chipdata.csv, so zero some things
		If ChipIO = -1 Then ChipIO = 0
		If ChipADC = -1 Then ChipADC = 0
		If ChipMHz = -1 Then ChipMHz = 1
	End If
    
    'Eliminate duplicate variables
    EliminateDuplicates
    
    'Add any missing variables
    For FCD = 1 to EDC
        
        With ExtraData(FCD)
            If IsMatch(.Chip, ChipName) Then
                
                'Add extra data
                For AR = 1 To .InfoCount
                	Select Case .Info(AR, 1)
                		Case "prog": ChipWords = Val(.Info(AR, 2))
                		Case "eeprom": ChipEEPROM = Val(.Info(AR, 2))
                		Case "ram": ChipRAM = Val(.Info(AR, 2))
                		Case "i/o": ChipIO = Val(.Info(AR, 2))
                		Case "adc": ChipADC = Val(.Info(AR, 2))
                		Case "maxmhz": ChipMHZ = Val(.Info(AR, 2))
                		Case "pins": ChipPins = Val(.Info(AR, 2))
                		Case "family": ChipFamily = Val(.Info(AR, 2))
                		Case "gpr": GPRCounTemp = Val(.Info(AR, 2))
                		Case "maxaddress": MaxChipAddress = Val(.Info(AR, 2))
                	End Select
                Next
                
                'Add SFRs
                For AR = 1 to .RegCount
                    With .Regs(AR)
                        SetSFR(.Name, .Loc)
                        
                        FOR AB = 1 to .BitCount
                            If .Bits(AB) <> "-" Then
                            	SetSFRBit(.Bits(AB), .Name, 8 - AB)
                            End If
                        Next
                    End With
                Next
                
                'Add SFR bits
                For AB = 1 to .BitCount
                    With .Bits(AB)
                        SetSFRBit(.Name, .Reg, .Bit)
                        
                    End With
                Next
            End If
        End With
    Next
    
    'Interrupts
    For PD = 1 To ILC
        
        With IntList(PD)
            IntName = GetIntName(.Vector)
            If IntName <> "" Then
                .EventName = IntName
                .EnableBit = GetBitName(.Vector)
                .FlagBit = ""
                If Instr(.EnableBit, ",") <> 0 Then
                    .FlagBit = Mid(.EnableBit, Instr(.EnableBit, ",") + 1)
                    .EnableBit = Left(.EnableBit, Instr(.EnableBit, ",") - 1)
                End If
            
            End If
        End With
        
    Next
    
    'Sort interrupt list
    EliminateDuplicateInterrupts
    Do
        SortMore = 0
        For PD = 2 to ILC
            If IntList(PD - 1).EventName > IntList(PD).EventName Then
                Swap IntList(PD - 1), IntList(PD)
                'Swap IntList(PD - 1, 2), IntList(PD, 2)
                SortMore = -1
            End If
        Next
    Loop While SortMore
    
    'If no pinout found, invent one so that GCGB will show pins
    If PinoutCount = 0 Then
    	GuessPinouts (ChipPackageList)
    End If
    
    'Write to output file
    OPEN OutputDir + "\" + LCase(ChipName) + ".dat" for output as #1
    
    PRINT #1, "'GCBASIC/GCGB Chip Data File"
    PRINT #1, "'Chip: " + ChipNameTidy
    PRINT #1, "'Generated " + CurrentDate
    PRINT #1, "'Format last revised: 23/9/2007"
    PRINT #1, ""
    
    Print #1, "[ChipData]"
    Print #1, "Prog=" + Str(ChipWords * 512) ' Input generally in kBytes, GCBASIC uses Words
    Print #1, "EEPROM=" + Str(ChipEEPROM)
    Print #1, "RAM=" + Str(ChipRAM)
    Print #1, "I/O=" + Str(ChipIO)
    Print #1, "ADC=" + Str(ChipADC)
    Print #1, "MaxMHz=" + Str(ChipMHZ)
    'Print #1, "IntOsc=" + Str(ChipIntOsc!)
    Print #1, "Pins=" + Str(ChipPins)
    Print #1, "Family=" + Str(ChipFamily)
    Print #1, "ConfigWords=0"
    Print #1, "GPR=" + Str(GPRCounTemp)
    Print #1, "MaxAddress=" + Str(MaxChipAddress)
    If HardwareMult Then
        Print #1, "HardwareMult=y"
    Else
        Print #1, "HardwareMult=n"
    End If
    Print #1, ""
    
    Print #1, "[Pointers]"
    Print #1, "XL:" + Trim(Str(RegXL))
    Print #1, "XH:" + Trim(Str(RegXH))
    Print #1, "YL:" + Trim(Str(RegYL))
    Print #1, "YH:" + Trim(Str(RegYH))
    Print #1, "ZL:" + Trim(Str(RegZL))
    Print #1, "ZH:" + Trim(Str(RegZH))
    Print #1, ""
    
    Print #1, "[Interrupts]"
    For PD = 1 to ILC
        With IntList(PD)
            If .EventName <> "" Then Print #1, .EventName + ":" + .Vector + "," + Str(.VectorLoc) + "," + .EnableBit + "," + .FlagBit
        End With
    Next
    Print #1, ""
    
    PRINT #1, "[Registers]"
    FOR PD = 1 to SVC
        TempData = SysVars(PD).Name + "," + Str(SysVars(PD).Loc)
        PRINT #1, TempData
    NEXT
    Print #1, ""
    
    PRINT #1, "[Bits]"
    FOR PD = 1 to SVBC
        TempData = SysVarBits(PD).Name + "," + SysVarBits(PD).Reg + "," + Str(SysVarBits(PD).Bit)
        PRINT #1, TempData
    NEXT
    Print #1, ""
    
    If ChipRAM > 0 Then
        Print #1, "[FreeRAM]"
        FOR PD = 1 to RBC
            PRINT #1, RamBlock(PD)
        NEXT
        Print #1, ""
    End If
    
    For PP = 1 to PinoutCount
    	With Pinouts(PP)
            Print #1, "[Pins-" + .Name + "]"
            For PD = 1 to .PinCount
                With .PinList(PD)
                    DataSource = Str(.Number)
                    For PF = 1 to .FunctionCount
                        NewData = .FunctionList(PF, 1)
                        If Len(NewData) = 3 And MID(NewData, 1, 1) = "P" Then
                            If MID(NewData, 2, 1) >= "A" And MID(NewData, 2, 1) <= "Z" Then
                                If MID(NewData, 3, 1) >= "0" AND MID(NewData, 3, 1) <= "9" Then
                                    DataSource = DataSource + "," + .FunctionList(PF, 1)
                                    If .FunctionList(PF, 2) <> "" Then DataSource = DataSource + "(" + .FunctionList(PF, 2) + ")"
                                    .FunctionList(PF, 1) = ""
                                End If
                            End If
                        End If
                    Next
                    For PF = 1 to .FunctionCount
                        If .FunctionList(PF, 1) <> "" Then
                            DataSource = DataSource + "," + .FunctionList(PF, 1)
                            If .FunctionList(PF, 2) <> "" Then DataSource = DataSource + "(" + .FunctionList(PF, 2) + ")"
                        End If
                    Next
                End With
                Print #1, DataSource
            Next
            Print #1, ""
        End With
    Next
    
    Print #1, "[ConfigOps]"
    Print #1, "NoConfig-PRG"
    Print #1, ""
    
    Print #1, "[Config]"
    Print #1, "NoConfig-PRG"
    Print #1, ""
    
    Close

    SkipChip:

NEXT

PRINT
PRINT "Conversion finished, press any key to continue"
GetKey

END

Sub AddIOPin(PinList() As ChipPin, PinNumber As Integer, PinName As String)
	'Add function to list of pin functions
	
	With PinList(PinNumber)
		'Set pin name and number, add to function count
		.Number = PinNumber
		.FunctionCount = 1
		.FunctionList(.FunctionCount, 1) = UCase(PinName)
		
		'Add data direction (if applicable)
		Select Case UCase(PinName)
			Case "VCC", "AVCC", "GND", "RESET", "AREF", "UVCC", "DN", "DP", "UGND", "UCAP", "VBUS": .FunctionList(.FunctionCount, 2) = ""
			Case "XTAL1", "XTAL2": .FunctionList(.FunctionCount, 2) = ""
			Case Else: .FunctionList(.FunctionCount, 2) = "IO" 
		End Select
	End With
	
End Sub

Sub AddPinFunction(Pin As Integer, Port As String, Direction As String)
	Dim As Integer FoundPin, SL
	
	With Pinouts(PinoutCount)
       'Add function
       With .PinList(Pin)
			Do While InStr(Port, ",") <> 0
				.FunctionCount += 1
	           .FunctionList(.FunctionCount, 1) = Left(Port, InStr(Port, ",") - 1)
	           .FunctionList(.FunctionCount, 2) = Direction
	           Port = Mid(Port, InStr(Port, ",") + 1)
			Loop
			
           .FunctionCount += 1
           .FunctionList(.FunctionCount, 1) = Port
           .FunctionList(.FunctionCount, 2) = Direction
       End With
   End With
End Sub

Function CheckBit (BitName As String) As Integer
    Dim PD As Integer
    
    For PD = 1 to SVBC
        With SysVarBits(PD)
            If .Name = BitName Then Return -1
        End With
    Next
    
    Return 0
    
End Function

Sub EliminateDuplicates
	
	Dim As Integer SearchPos1, SearchPos2
	
	'Find and remove duplicate IO vars
	SearchPos1 = 1
	Do While SearchPos1 < SVC
		SearchPos2 = SearchPos1 + 1
		Do While SearchPos2 <= SVC
			'If 1 and 2 are same, need to fix
			If SysVars(SearchPos1).Name = SysVars(SearchPos2).Name Then
				'Warn?
				If SysVars(SearchPos1).Loc <> SysVars(SearchPos2).Loc Then
					Print "Warning: duplicate conflicting definitions for " + SysVars(SearchPos1).Name
				End If
				'Remove second one
				SysVars(SearchPos2).Name = SysVars(SVC).Name
				SysVars(SearchPos2).Loc = SysVars(SVC).Loc
				SVC -= 1
				SearchPos2 -= 1
			End If
			
			SearchPos2 += 1
		Loop
		
		'Search for any bits with same name as var (really should not happen!)
		SearchPos2 = 1
		Do While SearchPos2 <= SVBC
			If SysVars(SearchPos1).Name = SysVarBits(SearchPos2).Name Then
				Print "Warning: " + SysVars(SearchPos1).Name + " is defined as var and bit"
				'Remove bit
				Swap SysVarBits(SearchPos2), SysVarBits(SVBC)
				SVBC -= 1
				SearchPos2 -= 1
			End If
			
			SearchPos2 += 1
		Loop
		
		SearchPos1 += 1
	Loop
	
	'Find and remove duplicate SFR bits
	SearchPos1 = 1
	Do While SearchPos1 < SVBC
		SearchPos2 = SearchPos1 + 1
		Do While SearchPos2 <= SVBC
			'If 1 and 2 are same, need to fix
			If SysVarBits(SearchPos1).Name = SysVarBits(SearchPos2).Name Then
				'Warn?
				If SysVarBits(SearchPos1).Reg <> SysVarBits(SearchPos2).Reg Or _
				   SysVarBits(SearchPos1).Bit <> SysVarBits(SearchPos2).Bit Then
					Print "Warning: duplicate conflicting definitions for " + SysVarBits(SearchPos1).Name
				End If
				'Remove second one
				Swap SysVarBits(SearchPos2), SysVarBits(SVBC)
				SVBC -= 1
				SearchPos2 -= 1
			End If
			
			SearchPos2 += 1
		Loop
		SearchPos1 += 1
	Loop
	
End Sub

Sub EliminateDuplicateInterrupts
	
	Dim As Integer SearchPos1, SearchPos2
	
	'Find and remove duplicate interrupts
	SearchPos1 = 1
	Do While SearchPos1 < ILC
		SearchPos2 = SearchPos1 + 1
		Do While SearchPos2 <= ILC
			
			If IntList(SearchPos1).EventName = IntList(SearchPos2).EventName Then
				'Duplicate found, remove it
				Swap IntList(SearchPos2), IntList(ILC)
				ILC -= 1
				SearchPos2 -= 1
			End If
			
			SearchPos2 += 1
		Loop
		SearchPos1 += 1
	Loop
	
End Sub

Function GetBitName (IntVect As String) As String
    Dim As String BitName, FlagName
    
    BitName = ""
    Select Case UCASE(IntVect)
        
        Case "RESET", "RESETB":
        
    	Case "RESERVED1", "RESERVED2", "RESERVED3", "RESERVED4", "RESERVED5", "RESERVED6", "RESERVED15", "RESERVED30", "RESERVED31", "NOT_USED":
        
        Case "WDT", "WDT_OVERFLOW", "WATCHDOG":
            If CheckBit("WDTIE") Then
                BitName = "WDTIE"
                FlagName = "WDTIF"
            Else
                BitName = "WDIE"
            End If
            FlagName = "-"
            
    	Case "BPINT":
            BitName = "-"
            
        Case "VREGMON":
            BitName = "ROCWIE"
            FlagName = "ROCWIF"
            
        Case "WAKE_UP":
            BitName = "WUTIE"
            FlagName = "WUTIF"
        
        Case "INT0", "EXT_INT0":
            BitName = "INT0"
            If CheckBit("INTF0") Then
                FlagName = "INTF0"
            Else
                FlagName = "-"
            End If
            
        Case "INT1":
            BitName = "INT1"
            FlagName = "INTF1"
        
        Case "INT2":
            BitName = "INT2"
            FlagName = "INTF2"
            
        Case "INT3":
            BitName = "INT3"
            FlagName = "INTF3"
            
        Case "INT4":
            BitName = "INT4"
            FlagName = "INTF4"
        
        Case "INT5":
            BitName = "INT5"
            FlagName = "INTF5"
            
        Case "INT6":
            BitName = "INT6"
            FlagName = "INTF6"
            
        Case "INT7":
            BitName = "INT7"
            FlagName = "INTF7"
        
    	Case "PCINT0", "PCINT", "IO_PINS", "PCINT_B":
            If CheckBit("PCIE") Then
                BitName = "PCIE"
            Else
                BitName = "PCIE0"
            End If
            If CheckBit("PCIF") Then
                FlagName = "PCIF"
            Else
                FlagName = "PCIF0"
            End If
            
    	Case "PCINT1", "PCINT_A":
            BitName = "PCIE1"
            FlagName = "PCIF1"
            
    	Case "PCINT2", "PCINT_D":
            BitName = "PCIE2"
            FlagName = "PCIF2"
            
        Case "PCINT3", "PCINT_C":
            BitName = "PCIE3"
            FlagName = "PCIF3"
            
        Case "PCINT4":
            BitName = "PCIE4"
            FlagName = "PCIF4"
            
        Case "PCINT5":
            BitName = "PCIE5"
            FlagName = "PCIF5"
            
        Case "PCINT6":
            BitName = "PCIE6"
            FlagName = "PCIF6"
            
        Case "PCINT7":
            BitName = "PCIE7"
            FlagName = "PCIF7"
            
        Case "LOW-LEVEL_IO_PINS":
            BitName = "LLIE"
            FlagName = "-"
        
        Case "PSC0_CAPT":
            BitName = "-"
        
        Case "PSC0_EC":
            BitName = "PEOPE0"
            FlagName = "PEOP0"
            
        Case "PSC1_CAPT":
            BitName = "-"
        
        Case "PSC1_EC":
            BitName = "PEOPE1"
            FlagName = "PEOP1"
            
        Case "PSC2_CAPT":
            BitName = "-"
        
        Case "PSC2_EC":
            BitName = "PEOPE2"
            FlagName = "PEOP2"
            
        Case "TIMER0_OVF", "TIMER0_OVF0", "TIM0_OVF":
            BitName = "TOIE0"
            FlagName = "TOV0"
            
        Case "TIMER0_COMP", "TIMER0_COMPA", "TIMER0_COMP_A", "TIM0_COMPA":
            If CheckBit("OCIE0") Then
                BitName = "OCIE0"
                FlagName = "OCF0"
            Else
                BitName = "OCIE0A"
                FlagName = "OCF0A"
            End If
            
        Case "TIMER0_COMPB", "TIMER0_COMP_B", "TIM0_COMPB":
            BitName = "OCIE0B"
            FlagName = "OCF0B"
            
        Case "TIMER0_CAPT", "TIMER0_IC", "TIM0_CAPT":
            If CheckBit("TICIE0") Then
                BitName = "TICIE0"
            Else
                BitName = "ICIE0"
            End If
            FlagName = "ICF0"
        
        Case "TIMER1_OVF", "TIMER1_OVF1", "TIM1_OVF":
            BitName = "TOIE1"
            FlagName = "TOV1"
            
        Case "TIMER1_COMPA", "TIMER1_COMP1", "TIMER1_COMP", "TIM1_COMP", "TIMER1_CMPA", "TIM1_COMPA":
            If CheckBit("OCF1") Then
                BitName = "OCIE1"
                FlagName = "OCF1"
            Else
                BitName = "OCIE1A"
                FlagName = "OCF1A"
            End If
            
        Case "TIMER1_COMPB", "TIMER1_CMPB", "TIM1_COMPB":
            BitName = "OCIE1B"
            FlagName = "OCF1B"
            
        Case "TIMER1_COMPC":
            BitName = "OCIE1C"
            FlagName = "OCF1C"
            
        Case "TIMER1_COMPD":
            BitName = "OCIE1D"
            FlagName = "OCF1D"
            
        Case "TIMER1_CAPT", "TIMER1_CAPT1", "TIM1_CAPT", "TIMER1_IC":
            If CheckBit("TICIE1") Then
                BitName = "TICIE1"
                FlagName = "ICF1"
            Else
                BitName = "ICIE1"
                FlagName = "ICF1"
            End If
            
        Case "FAULT_PROTECTION":
            BitName = "FPIE1"
            FlagName = "FPF1"
            
        Case "TIMER2_OVF":
            BitName = "TOIE2"
            FlagName = "TOV2"
            
        Case "TIMER2_COMP", "TIMER2_COMPA":
            If CheckBit("OCIE2") Then
                BitName = "OCIE2"
                FlagName = "OCF2"
            Else
                BitName = "OCIE2A"
                FlagName = "OCF2A"
            End If
            
        Case "TIMER2_COMPB":
            BitName = "OCIE2B"
            FlagName = "OCF2B"
        
        Case "TIMER3_OVF":
            BitName = "TOIE3"
            FlagName = "TOV3"
            
        Case "TIMER3_COMPA":
            BitName = "OCIE3A"
            FlagName = "OCF3A"
            
        Case "TIMER3_COMPB":
            BitName = "OCIE3B"
            FlagName = "OCF3B"
            
        Case "TIMER3_COMPC":
            BitName = "OCIE3C"
            FlagName = "OCF3C"
            
        Case "TIMER3_CAPT":
            If CheckBit("TICIE3") Then
                BitName = "TICIE3"
                FlagName = "ICF3"
            Else
                BitName = "ICIE3"
                FlagName = "ICF3"
            End If
            
        Case "TIMER4_OVF":
            BitName = "TOIE4"
            FlagName = "TOV4"
            
        Case "TIMER4_COMPA":
            BitName = "OCIE4A"
            FlagName = "OCF4A"
            
        Case "TIMER4_COMPB":
            BitName = "OCIE4B"
            FlagName = "OCF4B"
            
        Case "TIMER4_COMPC":
            BitName = "OCIE4C"
            FlagName = "OCF4C"
            
        Case "TIMER4_CAPT":
            BitName = "ICIE4"
            FlagName = "ICF4"
            
        Case "TIMER5_OVF":
            BitName = "TOIE5"
            FlagName = "TOV5"
            
        Case "TIMER5_COMPA":
            BitName = "OCIE5A"
            FlagName = "OCF5A"
            
        Case "TIMER5_COMPB":
            BitName = "OCIE5B"
            FlagName = "OCF5B"
            
        Case "TIMER5_COMPC":
            BitName = "OCIE5C"
            FlagName = "OCF5C"
            
        Case "TIMER5_CAPT":
            BitName = "ICIE5"
            FlagName = "ICF5"
        
        Case "USART_UDRE", "USART0_UDRE", "UART_UDRE", "UART0_UDRE":
            If CheckBit("UDRIE") Then
                BitName = "UDRIE"
                FlagName = "UDRE"
            ElseIf CheckBit("UDR0IE0") Then
                BitName = "UDR0IE0"
                FlagName = "UDRE0"
            Else
                BitName = "UDRIE0"
                FlagName = "UDRE0"
            End If
            
        Case "USART_TXC", "USART_TX", "UART_TX", "UART0_TX", "USART0_TX", "USART0_TXC":
            If CheckBit("TXCIE") Then
                BitName = "TXCIE"
                FlagName = "TXC"
            Else
                BitName = "TXCIE0"
                FlagName = "TXC0"
            End If
            
        Case "USART_RXC", "USART_RX", "UART_RX", "UART0_RX", "USART0_RX", "USART0_RXC":
            If CheckBit("RXCIE") Then
                BitName = "RXCIE"
                FlagName = "RXC"
            Else
                BitName = "RXCIE0"
                FlagName = "RXC0"
            End If
        
        Case "USART1_UDRE", "UART1_UDRE":
            If CheckBit("UDR1IE1") Then
                BitName = "UDR1IE1"
                FlagName = "UDRE1"
            Else
                BitName = "UDRIE1"
                FlagName = "UDRE1"
            End If
            
        Case "USART1_TX", "UART1_TX", "USART1_TXC":
            BitName = "TXCIE1"
            FlagName = "TXC1"
            
        Case "USART1_RX", "UART1_RX", "USART1_RXC":
            BitName = "RXCIE1"
            FlagName = "RXC1"
            
        Case "USART2_UDRE":
            BitName = "UDRIE2"
            FlagName = "UDRE2"
            
        Case "USART2_TX":
            BitName = "TXCIE2"
            FlagName = "TXC2"
            
        Case "USART2_RX":
            BitName = "RXCIE2"
            FlagName = "RXC2"
        
        Case "USART3_UDRE":
            BitName = "UDRIE3"
            FlagName = "UDRE3"
            
        Case "USART3_TX":
            BitName = "TXCIE3"
            FlagName = "TXC3"
            
        Case "USART3_RX":
            BitName = "RXCIE3"
            FlagName = "RXC3"
            
        Case "SPI_STC":
            If CheckBit("SPIE0") Then
                BitName = "SPIE0"
                FlagName = "SPIF0"
            Else
                BitName = "SPIE"
                FlagName = "SPIF"
            End If
            
        Case "USI_START", "USI_STRT", "USI_STR":
            BitName = "USISIE"
            FlagName = "USISIF"
            
        Case "USI_OVERFLOW", "USI_OVF":
            BitName = "USIOIE"
            FlagName = "USIOIF"
            
        Case "USB_GEN":
            BitName = "-"
            
        Case "USB_COM":
            BitName = "-"
            
        Case "LCD":
            BitName = "LCDIE"
            FlagName = "LCDIF"
            
        Case "TWI":
            BitName = "TWIE"
            FlagName = "TWINT"
            
        Case "TWI_BUS_CD":
            BitName = "TWBCIE"
            FlagName = "TWBCIF"
            
        Case "EE_RDY", "EE_READY", "EEPROM_READY":
            BitName = "EERIE"
            FlagName = "-"
            
        Case "SPM_RDY", "SPM_READY":
            BitName = "SPMIE"
            FlagName = "-"
        
        Case "ADC", "VADC", "ADC_READY", "ADC_ADC":
            If CheckBit("VADCCIF") Then
                BitName = "VADCCIE"
                FlagName = "VADCCIF"
            Else
                BitName = "ADIE"
                FlagName = "ADIF"
            End If
            
        Case "CCADC_CONV":
            BitName = "CADICIE"
            FlagName = "CADICIF"
            
        Case "CCADC_REG_CUR":
            BitName = "CADRCIE"
            FlagName = "CADRCIF"
            
        Case "CCADC_ACC":
            BitName = "CADACIE"
            FlagName = "CADACIF"
        
        Case "ANALOG_COMP_0":
            BitName = "AC0IE"
            FlagName = "AC0IF"
            
        Case "ANA_COMP", "ANALOG_COMP", "ANALOG_COMP_1":
            If CheckBit("ACIE") Then
                BitName = "ACIE"
                FlagName = "ACI"
            Else
                BitName = "AC1IE"
                FlagName = "AC1IF"
            End If
            
        Case "ANALOG_COMP_2":
            BitName = "AC2IE"
            FlagName = "AC2IF"
            
        Case "CANIT":
            BitName = "ENIT"
            FlagName = "CANIT"
            
        Case "OVRIT":
            BitName = "ENOVRT"
            FlagName = "OVRTIM"
        
        Case Else:
            
    End Select
    
    If BitName = "-" Then
        Return ""
    ElseIf FlagName = "" Then
        Print "No flag:", IntVect
        If Not CheckBit(BitName) Then Print "Bad bit name:", BitName
        Return BitName
    ElseIf FlagName = "-" Then
        If Not CheckBit(BitName) Then Print "Bad bit name:", BitName
        Return BitName
    Else
        If Not CheckBit(BitName) Then Print "Bad bit name:", BitName
        If Not CheckBit(FlagName) Then Print "Bad bit name:", FlagName
        Return BitName + "," + FlagName
    End If
        
End Function

Function GetIntName (IntVect As String) As String
    
    IntName = ""
    Select Case UCASE(IntVect)
        
        Case "RESET", "RESETB":
        
    	Case "RESERVED1", "RESERVED2", "RESERVED3", "RESERVED4", "RESERVED5", "RESERVED6", "RESERVED15", "RESERVED30", "RESERVED31", "NOT_USED":
        
        Case "WDT", "WDT_OVERFLOW", "WATCHDOG":
            IntName = "WDT"
            
        Case "BPINT":
            IntName = "BatteryFail"
            
        Case "VREGMON":
            IntName = "VoltageRegulator"
            
        Case "WAKE_UP":
            IntName = "WakeUp"
        
        Case "INT0", "EXT_INT0":
            IntName = "ExtInt0"
            
        Case "INT1":
            IntName = "ExtInt1"
        
        Case "INT2":
            IntName = "ExtInt2"
            
        Case "INT3":
            IntName = "ExtInt3"
            
        Case "INT4":
            IntName = "ExtInt4"
        
        Case "INT5":
            IntName = "ExtInt5"
            
        Case "INT6":
            IntName = "ExtInt6"
        
        Case "INT7":
            IntName = "ExtInt7"
        
    	Case "PCINT0", "PCINT_A":
            IntName = "PinChange0"
            
    	Case "PCINT1", "PCINT_B":
            IntName = "PinChange1"
            
    	Case "PCINT2", "PCINT_C":
            IntName = "PinChange2"
            
    	Case "PCINT3", "PCINT_D":
            IntName = "PinChange3"
            
        Case "PCINT4", "PCINT_E":
            IntName = "PinChange4"
            
        Case "PCINT5":
            IntName = "PinChange5"
            
        Case "PCINT6":
            IntName = "PinChange6"
            
        Case "PCINT7":
            IntName = "PinChange7"
        
        Case "PCINT", "IO_PINS":
            IntName = "PinChange"
            
        Case "LOW-LEVEL_IO_PINS":
            IntName = "PORTBChange"
        
        Case "PSC0_CAPT":
            IntName = "PSC0Capture"
        
        Case "PSC0_EC":
            IntName = "PSC0EndCycle"
            
        Case "PSC1_CAPT":
            IntName = "PSC1Capture"
        
        Case "PSC1_EC":
            IntName = "PSC1EndCycle"
            
        Case "PSC2_CAPT":
            IntName = "PSC2Capture"
        
        Case "PSC2_EC":
            IntName = "PSC2EndCycle"
            
        Case "TIMER0_OVF", "TIMER0_OVF0", "TIM0_OVF":
            IntName = "Timer0Overflow"
            
        Case "TIMER0_COMP", "TIMER0_COMPA", "TIMER0_COMP_A", "TIM0_COMPA":
            IntName = "Timer0Match1"
            
        Case "TIMER0_COMPB", "TIMER0_COMP_B", "TIM0_COMPB":
            IntName = "Timer0Match2"
            
        Case "TIMER0_CAPT", "TIMER0_IC", "TIM0_CAPT":
            IntName = "Timer0Capture"
        
        Case "TIMER1_OVF", "TIMER1_OVF1", "TIM1_OVF":
            IntName = "Timer1Overflow"
            
        Case "TIMER1_COMPA", "TIMER1_COMP1", "TIMER1_COMP", "TIM1_COMP", "TIMER1_CMPA", "TIM1_COMPA":
            IntName = "Timer1Match1"
            
        Case "TIMER1_COMPB", "TIMER1_CMPB", "TIM1_COMPB":
            IntName = "Timer1Match2"
            
        Case "TIMER1_COMPC":
            IntName = "Timer1Match3"
            
        Case "TIMER1_COMPD":
            IntName = "Timer1Match4"
            
        Case "TIMER1_CAPT", "TIMER1_CAPT1", "TIM1_CAPT", "TIMER1_IC":
            IntName = "Timer1Capture"
            
        Case "FAULT_PROTECTION":
            IntName = "Timer1Error"
            
        Case "TIMER2_OVF":
            IntName = "Timer2Overflow"
            
        Case "TIMER2_COMP", "TIMER2_COMPA":
            IntName = "Timer2Match1"
            
        Case "TIMER2_COMPB":
            IntName = "Timer2Match2"
        
        Case "TIMER3_OVF":
            IntName = "Timer3Overflow"
            
        Case "TIMER3_COMPA":
            IntName = "Timer3Match1"
            
        Case "TIMER3_COMPB":
            IntName = "Timer3Match2"
            
        Case "TIMER3_COMPC":
            IntName = "Timer3Match3"
            
        Case "TIMER3_CAPT":
            IntName = "Timer3Capture"
            
        Case "TIMER4_OVF":
            IntName = "Timer4Overflow"
            
        Case "TIMER4_COMPA":
            IntName = "Timer4Match1"
            
        Case "TIMER4_COMPB":
            IntName = "Timer4Match2"
            
        Case "TIMER4_COMPC":
            IntName = "Timer4Match3"
            
        Case "TIMER4_CAPT":
            IntName = "Timer4Capture"
            
        Case "TIMER5_OVF":
            IntName = "Timer5Overflow"
            
        Case "TIMER5_COMPA":
            IntName = "Timer5Match1"
            
        Case "TIMER5_COMPB":
            IntName = "Timer5Match2"
            
        Case "TIMER5_COMPC":
            IntName = "Timer5Match3"
            
        Case "TIMER5_CAPT":
            IntName = "Timer5Capture"
        
        Case "USART_UDRE", "USART0_UDRE", "UART_UDRE", "UART0_UDRE":
            IntName = "UsartTX1Ready"
            
        Case "USART_TXC", "USART_TX", "UART_TX", "UART0_TX", "USART0_TX", "USART0_TXC":
            IntName = "UsartTX1Sent"
            
        Case "USART_RXC", "USART_RX", "UART_RX", "UART0_RX", "USART0_RX", "USART0_RXC":
            IntName = "UsartRX1Ready"
        
        Case "USART1_UDRE", "UART1_UDRE":
            IntName = "UsartTX2Ready"
            
        Case "USART1_TX", "UART1_TX", "USART1_TXC":
            IntName = "UsartTX2Sent"
            
        Case "USART1_RX", "UART1_RX", "USART1_RXC":
            IntName = "UsartRX2Ready"
            
        Case "USART2_UDRE":
            IntName = "UsartTX3Ready"
            
        Case "USART2_TX":
            IntName = "UsartTX3Sent"
            
        Case "USART2_RX":
            IntName = "UsartRX3Ready"
        
        Case "USART3_UDRE":
            IntName = "UsartTX4Ready"
            
        Case "USART3_TX":
            IntName = "UsartTX4Sent"
            
        Case "USART3_RX":
            IntName = "UsartRX4Ready"
            
        Case "SPI_STC":
            IntName = "SPIReady"
            
        Case "USI_START", "USI_STRT", "USI_STR":
            IntName = "USIStart"
            
        Case "USI_OVERFLOW", "USI_OVF":
            IntName = "USIOverflow"
            
        Case "USB_GEN":
            IntName = "USB"
            
        Case "USB_COM":
            IntName = "USBEndpoint"
            
        Case "LCD":
            IntName = "LCDReady"
            
        Case "TWI":
            IntName = "TWIReady"
            
        Case "TWI_BUS_CD":
            IntName = "TWIConnect"
            
        Case "EE_RDY", "EE_READY", "EEPROM_READY":
            IntName = "EEPROMReady"
            
        Case "SPM_RDY", "SPM_READY":
            IntName = "SPMReady"
        
        Case "ADC", "VADC", "ADC_READY", "ADC_ADC":
            IntName = "ADCReady"
            
        Case "CCADC_CONV":
            IntName = "CCADCReady"
            
        Case "CCADC_REG_CUR":
            IntName = "CCADCRegular"
            
        Case "CCADC_ACC":
            IntName = "CCADCAccReady"
        
        Case "ANALOG_COMP_0":
            IntName = "Comp0Change"
        
        Case "ANA_COMP", "ANALOG_COMP", "ANALOG_COMP_1":
            IntName = "Comp1Change"
            
        Case "ANALOG_COMP_2":
            IntName = "Comp2Change"
            
        Case "CANIT":
            IntName = "CANTransferComplete"
            
        Case "OVRIT":
            IntName = "CANError"
        
        Case Else:
            Print "Unrecognised interrupt vector: ", IntVect
        
    End Select
    
    Return IntName
    
End Function

Sub GuessPinouts (PinoutString As String)
	'Try to guess pinout of chip from string from website
	Dim As Integer PD, CurrPackage, PackagePins
	Dim As String PackageName
	Dim Package(20) As String
	Dim Packages As Integer
	
	'Don't run if there are already pinouts
	If PinoutCount > 0 Then
		Exit Sub
	End If
	
	'Read all packages
	Packages = 0
	Do While InStr(PinoutString, ",") <> 0
		Packages += 1
		Package(Packages) = Trim(Left(PinoutString, InStr(PinoutString, ",") - 1))
		PinoutString = Trim(Mid(PinoutString, InStr(PinoutString, ",") + 1))
	Loop
	If PinoutString <> "" Then
		Packages += 1: Package(Packages) = PinoutString
	End If
	
	For CurrPackage = 1 To Packages
		'Read package
		If InStr(Package(CurrPackage), " ") = 0 Then
			Print "Strange package: " + Package(CurrPackage)
			
		Else
			'If there are spaces, get name from before first, pin count from after last
			PackageName = Left(Package(CurrPackage), InStr(Package(CurrPackage), " ") - 1)
			PackagePins = Val(Mid(Package(CurrPackage), InStrRev(Package(CurrPackage), " ") + 1))
			If PackageName = "PDIP" Then PackageName = "DIP"
			
			Select Case PackageName
				Case "DIP", "SOIC"
					PinoutCount += 1
					With Pinouts(PinoutCount)
						.Name = PackageName
						.PinCount = PackagePins
						ChipPins = PackagePins
						Select Case PackagePins
							Case 8
								AddIOPin(.PinList(), 1, "PB5")
								AddIOPin(.PinList(), 2, "PB3")
								AddIOPin(.PinList(), 3, "PB4")
								AddIOPin(.PinList(), 4, "GND")
								AddIOPin(.PinList(), 5, "PB0")
								AddIOPin(.PinList(), 6, "PB1")
								AddIOPin(.PinList(), 7, "PB2")
								AddIOPin(.PinList(), 8, "VCC")
								
								AddPinFunction(1, "RESET", "I")
								AddPinFunction(7, "SCK", "IO")
								AddPinFunction(5, "MOSI", "IO")
								AddPinFunction(6, "MISO", "IO")
								
								If ChipADC > 0 Then AddPinFunction(1, "ADC0", "I")
								If ChipADC > 1 Then AddPinFunction(7, "ADC1", "I")
								If ChipADC > 2 Then AddPinFunction(3, "ADC2", "I")
								If ChipADC > 3 Then AddPinFunction(2, "ADC3", "I")
								
							Case 14
								AddIOPin(.PinList(), 1, "VCC")
								AddIOPin(.PinList(), 2, "PB0")
								AddIOPin(.PinList(), 3, "PB1")
								AddIOPin(.PinList(), 4, "PB3")
								AddIOPin(.PinList(), 5, "PB2")
								AddIOPin(.PinList(), 6, "PA7")
								AddIOPin(.PinList(), 7, "PA6")
								AddIOPin(.PinList(), 8, "PA5")
								AddIOPin(.PinList(), 9, "PA4")
								AddIOPin(.PinList(), 10, "PA3")
								AddIOPin(.PinList(), 11, "PA2")
								AddIOPin(.PinList(), 12, "PA1")
								AddIOPin(.PinList(), 13, "PA0")
								AddIOPin(.PinList(), 14, "GND")
								
								AddPinFunction(4, "RESET", "I")
								AddPinFunction(9, "SCK", "IO")
								AddPinFunction(7, "MOSI", "IO")
								AddPinFunction(8, "MISO", "IO")
								
								If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
									AddPinFunction(12, "TXD", "O")
									AddPinFunction(11, "RXD", "I")
								End If
								
								If ChipADC > 0 Then AddPinFunction(13, "ADC0", "I")
								If ChipADC > 1 Then AddPinFunction(12, "ADC1", "I")
								If ChipADC > 2 Then AddPinFunction(11, "ADC2", "I")
								If ChipADC > 3 Then AddPinFunction(10, "ADC3", "I")
								If ChipADC > 4 Then AddPinFunction(9, "ADC4", "I")
								If ChipADC > 5 Then AddPinFunction(8, "ADC5", "I")
								If ChipADC > 6 Then AddPinFunction(7, "ADC6", "I")
								If ChipADC > 7 Then AddPinFunction(6, "ADC7", "I")
								If ChipADC > 8 Then AddPinFunction(5, "ADC8", "I")
								If ChipADC > 9 Then AddPinFunction(4, "ADC9", "I")
								If ChipADC > 10 Then AddPinFunction(3, "ADC10", "I")
								If ChipADC > 11 Then AddPinFunction(2, "ADC11", "I")
								
							Case 20
								If ChipIO = 16 Then
									AddIOPin(.PinList(), 1, "PB0")
									AddIOPin(.PinList(), 2, "PB1")
									AddIOPin(.PinList(), 3, "PB2")
									AddIOPin(.PinList(), 4, "PB3")
									AddIOPin(.PinList(), 5, "VCC")
									AddIOPin(.PinList(), 6, "GND")
									AddIOPin(.PinList(), 7, "PB4")
									AddIOPin(.PinList(), 8, "PB5")
									AddIOPin(.PinList(), 9, "PB6")
									AddIOPin(.PinList(), 10, "PB7")
									AddIOPin(.PinList(), 11, "PA7")
									AddIOPin(.PinList(), 12, "PA6")
									AddIOPin(.PinList(), 13, "PA5")
									AddIOPin(.PinList(), 14, "PA4")
									AddIOPin(.PinList(), 15, "AVCC")
									AddIOPin(.PinList(), 16, "AGND")
									AddIOPin(.PinList(), 17, "PA3")
									AddIOPin(.PinList(), 18, "PA2")
									AddIOPin(.PinList(), 19, "PA1")
									AddIOPin(.PinList(), 20, "PA0")
									
									AddPinFunction(10, "RESET", "I")
									AddPinFunction(3, "SCK", "IO")
									AddPinFunction(1, "MOSI", "IO")
									AddPinFunction(2, "MISO", "IO")
									
									If ChipADC > 0 Then AddPinFunction(20, "ADC0", "I")
									If ChipADC > 1 Then AddPinFunction(19, "ADC1", "I")
									If ChipADC > 2 Then AddPinFunction(18, "ADC2", "I")
									If ChipADC > 3 Then AddPinFunction(14, "ADC3", "I")
									If ChipADC > 4 Then AddPinFunction(13, "ADC4", "I")
									If ChipADC > 5 Then AddPinFunction(12, "ADC5", "I")
									If ChipADC > 6 Then AddPinFunction(11, "ADC6", "I")
									If ChipADC > 7 Then AddPinFunction(7, "ADC7", "I")
									If ChipADC > 8 Then AddPinFunction(8, "ADC8", "I")
									If ChipADC > 9 Then AddPinFunction(9, "ADC9", "I")
									If ChipADC > 10 Then AddPinFunction(10, "ADC10", "I")
									
								ElseIf LCase(ChipName) = "tiny1634" Then
									AddIOPin(.PinList(), 1, "PA2")
									AddIOPin(.PinList(), 2, "PD0")
									AddIOPin(.PinList(), 3, "PD1")
									AddIOPin(.PinList(), 4, "PA1")
									AddIOPin(.PinList(), 5, "PA0")
									AddIOPin(.PinList(), 6, "PD2")
									AddIOPin(.PinList(), 7, "PD3")
									AddIOPin(.PinList(), 8, "PD4")
									AddIOPin(.PinList(), 9, "PD5")
									AddIOPin(.PinList(), 10, "GND")
									AddIOPin(.PinList(), 11, "PD6")
									AddIOPin(.PinList(), 12, "PB0")
									AddIOPin(.PinList(), 13, "PB1")
									AddIOPin(.PinList(), 14, "PB2")
									AddIOPin(.PinList(), 15, "PB3")
									AddIOPin(.PinList(), 16, "PB4")
									AddIOPin(.PinList(), 17, "PB5")
									AddIOPin(.PinList(), 18, "PB6")
									AddIOPin(.PinList(), 19, "PB7")
									AddIOPin(.PinList(), 20, "VCC")
									
									AddPinFunction(1, "RESET", "I")
									AddPinFunction(19, "SCK", "IO")
									AddPinFunction(17, "MOSI", "IO")
									AddPinFunction(18, "MISO", "IO")
									
									If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
										AddPinFunction(3, "TXD", "O")
										AddPinFunction(2, "RXD", "I")
									End If
									
								Else
									'Correct for tiny2313/4313, maybe others
									AddIOPin(.PinList(), 1, "PA2")
									AddIOPin(.PinList(), 2, "PD0")
									AddIOPin(.PinList(), 3, "PD1")
									AddIOPin(.PinList(), 4, "PA1")
									AddIOPin(.PinList(), 5, "PA0")
									AddIOPin(.PinList(), 6, "PD2")
									AddIOPin(.PinList(), 7, "PD3")
									AddIOPin(.PinList(), 8, "PD4")
									AddIOPin(.PinList(), 9, "PD5")
									AddIOPin(.PinList(), 10, "GND")
									AddIOPin(.PinList(), 11, "PD6")
									AddIOPin(.PinList(), 12, "PB0")
									AddIOPin(.PinList(), 13, "PB1")
									AddIOPin(.PinList(), 14, "PB2")
									AddIOPin(.PinList(), 15, "PB3")
									AddIOPin(.PinList(), 16, "PB4")
									AddIOPin(.PinList(), 17, "PB5")
									AddIOPin(.PinList(), 18, "PB6")
									AddIOPin(.PinList(), 19, "PB7")
									AddIOPin(.PinList(), 20, "VCC")
									
									AddPinFunction(1, "RESET", "I")
									AddPinFunction(19, "SCK", "IO")
									AddPinFunction(17, "MOSI", "IO")
									AddPinFunction(18, "MISO", "IO")
									
									If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
										AddPinFunction(3, "TXD", "O")
										AddPinFunction(2, "RXD", "I")
									End If
								
								End If
								
							Case 28
								AddIOPin(.PinList(), 1, "PC6")
								AddIOPin(.PinList(), 2, "PD0")
								AddIOPin(.PinList(), 3, "PD1")
								AddIOPin(.PinList(), 4, "PD2")
								AddIOPin(.PinList(), 5, "PD3")
								AddIOPin(.PinList(), 6, "PD4")
								AddIOPin(.PinList(), 7, "VCC")
								AddIOPin(.PinList(), 8, "GND")
								AddIOPin(.PinList(), 9, "PB6")
								AddIOPin(.PinList(), 10, "PB7")
								AddIOPin(.PinList(), 11, "PD5")
								AddIOPin(.PinList(), 12, "PD6")
								AddIOPin(.PinList(), 13, "PD7")
								AddIOPin(.PinList(), 14, "PB0")
								AddIOPin(.PinList(), 15, "PB1")
								AddIOPin(.PinList(), 16, "PB2")
								AddIOPin(.PinList(), 17, "PB3")
								AddIOPin(.PinList(), 18, "PB4")
								AddIOPin(.PinList(), 19, "PB5")
								AddIOPin(.PinList(), 20, "AVCC")
								If ChipIO > 23 Then
									AddIOPin(.PinList(), 21, "PC7")
								Else
									AddIOPin(.PinList(), 21, "AREF")
								End If
								AddIOPin(.PinList(), 22, "GND")
								AddIOPin(.PinList(), 23, "PC0")
								AddIOPin(.PinList(), 24, "PC1")
								AddIOPin(.PinList(), 25, "PC2")
								AddIOPin(.PinList(), 26, "PC3")
								AddIOPin(.PinList(), 27, "PC4")
								AddIOPin(.PinList(), 28, "PC5")
								
								If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
									AddPinFunction(3, "TXD", "O")
									AddPinFunction(2, "RXD", "I")
								End If
								
								AddPinFunction(1, "RESET", "I")
								AddPinFunction(19, "SCK", "IO")
								AddPinFunction(17, "MOSI", "IO")
								AddPinFunction(18, "MISO", "IO")
								
								If ChipADC > 0 Then AddPinFunction(23, "ADC0", "I")
								If ChipADC > 1 Then AddPinFunction(24, "ADC1", "I")
								If ChipADC > 2 Then AddPinFunction(25, "ADC2", "I")
								If ChipADC > 3 Then AddPinFunction(26, "ADC3", "I")
								If ChipADC > 4 Then AddPinFunction(27, "ADC4", "I")
								If ChipADC > 5 Then AddPinFunction(28, "ADC5", "I")
								
							Case 40
								AddIOPin(.PinList(), 1, "PB0")
								AddIOPin(.PinList(), 2, "PB1")
								AddIOPin(.PinList(), 3, "PB2")
								AddIOPin(.PinList(), 4, "PB3")
								AddIOPin(.PinList(), 5, "PB4")
								AddIOPin(.PinList(), 6, "PB5")
								AddIOPin(.PinList(), 7, "PB6")
								AddIOPin(.PinList(), 8, "PB7")
								AddIOPin(.PinList(), 10, "VCC")
								AddIOPin(.PinList(), 11, "GND")
								AddIOPin(.PinList(), 12, "XTAL2")
								AddIOPin(.PinList(), 13, "XTAL1")
								AddIOPin(.PinList(), 14, "PD0")
								AddIOPin(.PinList(), 15, "PD1")
								AddIOPin(.PinList(), 16, "PD2")
								AddIOPin(.PinList(), 17, "PD3")
								AddIOPin(.PinList(), 18, "PD4")
								AddIOPin(.PinList(), 19, "PD5")
								AddIOPin(.PinList(), 20, "PD6")
								AddIOPin(.PinList(), 21, "PD7")
								AddIOPin(.PinList(), 22, "PC0")
								AddIOPin(.PinList(), 23, "PC1")
								AddIOPin(.PinList(), 24, "PC2")
								AddIOPin(.PinList(), 25, "PC3")
								AddIOPin(.PinList(), 26, "PC4")
								AddIOPin(.PinList(), 27, "PC5")
								AddIOPin(.PinList(), 28, "PC6")
								AddIOPin(.PinList(), 29, "PC7")
								AddIOPin(.PinList(), 30, "AVCC")
								AddIOPin(.PinList(), 31, "GND")
								AddIOPin(.PinList(), 32, "AREF")
								AddIOPin(.PinList(), 33, "PA7")
								AddIOPin(.PinList(), 34, "PA6")
								AddIOPin(.PinList(), 35, "PA5")
								AddIOPin(.PinList(), 36, "PA4")
								AddIOPin(.PinList(), 37, "PA3")
								AddIOPin(.PinList(), 38, "PA2")
								AddIOPin(.PinList(), 39, "PA1")
								AddIOPin(.PinList(), 40, "PA0")
								
								
								If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
									AddPinFunction(15, "TXD", "O")
									AddPinFunction(14, "RXD", "I")
								End If
								
								AddPinFunction(9, "RESET", "I")
								AddPinFunction(8, "SCK", "IO")
								AddPinFunction(6, "MOSI", "IO")
								AddPinFunction(7, "MISO", "IO")
								
								If ChipADC > 0 Then AddPinFunction(40, "ADC0", "I")
								If ChipADC > 1 Then AddPinFunction(39, "ADC1", "I")
								If ChipADC > 2 Then AddPinFunction(38, "ADC2", "I")
								If ChipADC > 3 Then AddPinFunction(37, "ADC3", "I")
								If ChipADC > 4 Then AddPinFunction(36, "ADC4", "I")
								If ChipADC > 5 Then AddPinFunction(35, "ADC5", "I")
								If ChipADC > 6 Then AddPinFunction(34, "ADC6", "I")
								If ChipADC > 7 Then AddPinFunction(33, "ADC7", "I")
								
							Case Else
								Print "Unable to add pinout for " + PackageName + " " + Str(PackagePins)
								PinoutCount -= 1	
						End Select
					End With
					
				Case "TQFP", "QFN"
					PinoutCount += 1
					With Pinouts(PinoutCount)
						.Name = PackageName
						.PinCount = PackagePins
						ChipPins = PackagePins
						Select Case PackagePins
							Case 44
								AddIOPin(.PinList(), 1, "PE6")
								AddIOPin(.PinList(), 2, "UVCC")
								AddIOPin(.PinList(), 3, "DN")
								AddIOPin(.PinList(), 4, "DP")
								AddIOPin(.PinList(), 5, "UGND")
								AddIOPin(.PinList(), 6, "UCAP")
								AddIOPin(.PinList(), 7, "VBUS")
								AddIOPin(.PinList(), 8, "PB0")
								AddIOPin(.PinList(), 9, "PB1")
								AddIOPin(.PinList(), 10, "PB2")
								AddIOPin(.PinList(), 11, "PB3")
								
								AddIOPin(.PinList(), 12, "PB7")
								AddIOPin(.PinList(), 13, "RESET")
								AddIOPin(.PinList(), 14, "VCC")
								AddIOPin(.PinList(), 15, "GND")
								AddIOPin(.PinList(), 16, "XTAL2")
								AddIOPin(.PinList(), 17, "XTAL1")
								AddIOPin(.PinList(), 18, "PD0")
								AddIOPin(.PinList(), 19, "PD1")
								AddIOPin(.PinList(), 20, "PD2")
								AddIOPin(.PinList(), 21, "PD3")
								AddIOPin(.PinList(), 22, "PD5")
								
								AddIOPin(.PinList(), 23, "GND")
								AddIOPin(.PinList(), 24, "AVCC")
								AddIOPin(.PinList(), 25, "PD4")
								AddIOPin(.PinList(), 26, "PD6")
								AddIOPin(.PinList(), 27, "PD7")
								AddIOPin(.PinList(), 28, "PB4")
								AddIOPin(.PinList(), 29, "PB5")
								AddIOPin(.PinList(), 30, "PB6")
								AddIOPin(.PinList(), 31, "PC6")
								AddIOPin(.PinList(), 32, "PC7")
								AddIOPin(.PinList(), 33, "PE2")
								
								AddIOPin(.PinList(), 34, "VCC")
								AddIOPin(.PinList(), 35, "GND")
								AddIOPin(.PinList(), 36, "PF7")
								AddIOPin(.PinList(), 37, "PF6")
								AddIOPin(.PinList(), 38, "PF5")
								AddIOPin(.PinList(), 39, "PF4")
								AddIOPin(.PinList(), 40, "PF1")
								AddIOPin(.PinList(), 41, "PF0")
								AddIOPin(.PinList(), 42, "AREF")
								AddIOPin(.PinList(), 43, "GND")
								AddIOPin(.PinList(), 44, "AVCC")
								
								
								If HasSFR("UDR") Or HasSFR("UDR0") Or HasSFR("UDR1") Then
									AddPinFunction(21, "TXD", "O")
									AddPinFunction(20, "RXD", "I")
								End If
								
								AddPinFunction(9, "SCK", "IO")
								AddPinFunction(10, "MOSI", "IO")
								AddPinFunction(11, "MISO", "IO")
								
								If ChipADC > 0 Then AddPinFunction(41, "ADC0", "I")
								If ChipADC > 1 Then AddPinFunction(40, "ADC1", "I")
								If ChipADC > 2 Then AddPinFunction(39, "ADC4", "I")
								If ChipADC > 3 Then AddPinFunction(38, "ADC5", "I")
								If ChipADC > 4 Then AddPinFunction(37, "ADC6", "I")
								If ChipADC > 5 Then AddPinFunction(36, "ADC7", "I")
								If ChipADC > 6 Then AddPinFunction(25, "ADC8", "I")
								If ChipADC > 7 Then AddPinFunction(26, "ADC9", "I")
								If ChipADC > 8 Then AddPinFunction(27, "ADC10", "I")
								If ChipADC > 9 Then AddPinFunction(28, "ADC11", "I")
								If ChipADC > 10 Then AddPinFunction(29, "ADC12", "I")
								If ChipADC > 11 Then AddPinFunction(30, "ADC13", "I")
								
						End Select
					End With
					
				Case Else
					'Unknown package type
			End Select
			
		End If
	Next
	
End Sub

Function IsMatch (InExp As String, InString As String) As Integer
    Dim As String UExp, UString
    UExp = Trim(UCase(InExp))
    UString = Trim(UCase(InString))
    'Print UExp, UString
    
    'If InExp is a list, check each element
    Do While InStr(UExp, ",") <> 0
    	If IsMatch(Trim(Left(UExp, InStr(UExp, ",") - 1)), UString) Then
    		Return -1
    	End If
    	UExp = Trim(Mid(UExp, InStr(UExp, ",") + 1))
    Loop
    
    'Direct match
    If UExp = UString Then Return -1
    
    'Exp starts with *
    If Left(UExp, 1) = "*" And Right(UExp, Len(UExp) - 1) = Right(UString, Len(UExp) - 1) Then Return -1
    
    'Exp ends with *
    If Right(UExp, 1) = "*" And Left(UExp, Len(UExp) - 1) = Left(UString, Len(UExp) - 1) Then Return -1
    
    'Print "No Match"
    Return 0
    
End Function

Sub LoadExtraData (InFile As String)
    Dim As Integer TDC, AED
    Dim LastSection As String
    Dim DataStore(10) As String
    Dim As String DataSource, TempData
    TDC = 0
    
    Open InFile For input As #1
    LastSection = ""
    Do While Not EOF(1)
        Line Input #1, DataSource
        If Left(DataSource, 1) = "'" Then
            Continue Do
            
        ElseIf Trim(DataSource) = "" Then
            Continue Do
            
        ElseIf Left(DataSource, 1) = "[" And Right(DataSource, 1) = "]" Then
            LastSection = Mid(LCase(DataSource), 2, Len(DataSource) - 2)
            EDC += 1
            With ExtraData(EDC)
                .Chip = LastSection
                .RegCount = 0
            End With
            
        ElseIf LastSection = "" Then
            Continue Do
            
        ElseIf Left(LCase(DataSource), 4) = "reg " Then
            'Get data
            TempData = TRIM(MID(DataSource, 5))
            TDC = 0
            Do While INSTR(TempData, ",") <> 0
                TDC += 1: DataStore(TDC) = Trim(Left(TempData, INSTR(TempData, ",") - 1))
                TempData = Mid(TempData, INSTR(TempData, ",") + 1)
            Loop
            If Trim(TempData) <> "" Then
                TDC += 1: DataStore(TDC) = Trim(TempData)
            End If
            
            'Cache
            With ExtraData(EDC)
                .RegCount += 1
                With .Regs(.RegCount)
                    .Name = DataStore(2)
                    .Loc = Val("&H" + DataStore(1))
                    .BitCount = 0
                    For AED = 1 to TDC - 2
                        .BitCount += 1
                        .Bits(.BitCount) = DataStore(AED + 2)
                    Next
                End With
            End With
        
        ElseIf Left(LCase(DataSource), 4) = "bit " Then
            'Get data
            TempData = TRIM(MID(DataSource, 5))
            TDC = 0
            Do While INSTR(TempData, ",") <> 0
                TDC += 1: DataStore(TDC) = Trim(Left(TempData, INSTR(TempData, ",") - 1))
                TempData = Mid(TempData, INSTR(TempData, ",") + 1)
            Loop
            If Trim(TempData) <> "" Then
                TDC += 1: DataStore(TDC) = Trim(TempData)
            End If
            
            'Cache
            With ExtraData(EDC)
                .BitCount += 1
                With .Bits(.BitCount)
                    .Name = DataStore(1)
                    .Bit = Val(DataStore(3))
                    .Reg = DataStore(2)
                End With
            End With
            
        ElseIf Left(LCase(DataSource), 5) = "info " Then
        	TempData = Trim(Mid(DataSource, 6))
        	With ExtraData(EDC)
        		.InfoCount += 1
        		.Info(.InfoCount, 1) = LCase(Left(TempData, InStr(TempData, " ") - 1))
        		.Info(.InfoCount, 2) = Trim(Mid(TempData, InStr(TempData, " ") + 1))
        	End With
            
        End If
    Loop
    Close #1
End Sub

Sub ReadXml4 (FileName As String)
	Dim As String DataSource, TempData, CurrentTag
	Dim As String LastRegName, CurrentPinS, TidyIn
	Dim As String PinNameTemp, PinKey, NewData, NameTemp
	Dim As Integer LineSize, InComment, PD, CD, Temp
	Dim As Integer FindPin, PinNo, CurrentPin, FP, DF, LastNA, VectLoc, FV
	
	Dim As String ParseLine(500)
	
	CurrentTag = ""
    LastRegName = ""
	
	OPEN FileName For input as #1
    
    Do While Not EOF(1)
        Line Input #1, DataSource
        Do While INSTR(DataSource, Chr(9)) <> 0: Replace DataSource, Chr(9), " ": Loop
        DataSource = Trim(DataSource)
        
        LineSize = 0
        InComment = 0
        Do While DataSource <> ""
            If Left(DataSource, 4) = "<!--" Then
                InComment = 1
                If Right(DataSource, 3) = "-->" Then InComment = 0
                DataSource = ""
            End If
            If Left(DataSource, 2) = "<?" Then
                InComment = 1
                If Right(DataSource, 2) = "?>" Then InComment = 0
                DataSource = ""
            End If
            If Right(DataSource, 3) = "-->" Then
                InComment = 0
                DataSource = Trim(Left(DataSource, INSTR(DataSource, "-->") - 1))
            End If
            
            If InComment = 0 Then
                If Left(DataSource, 1) = "<" Then
                    LineSize += 1
                    ParseLine(LineSize) = Trim(Left(DataSource, INSTR(DataSource, ">")))
                    DataSource = Mid(DataSource, INSTR(DataSource, ">") + 1)
                End If
                If Left(DataSource, 1) <> "<" Then
                    LineSize += 1
                    If INSTR(DataSource, "<") <> 0 Then
                        ParseLine(LineSize) = Trim(Left(DataSource, INSTR(DataSource, "<") - 1))
                        DataSource = Mid(DataSource, INSTR(DataSource, "<"))
                    Else
                        ParseLine(LineSize) = Trim(DataSource)
                        DataSource = ""
                    End If
                End If
            End If
        Loop
        
        For PD = 1 to LineSize
            DataSource = ParseLine(PD)
            
            'End of section
            If Left(DataSource, 2) = "</" Then
                TempData = Mid(DataSource, 3): TempData = Left(TempData, LEN(TempData) - 1)
                For Temp = Len(CurrentTag) To 1 Step -1
                    If Mid(CurrentTag, Temp, 1) = "\" Then
                        CurrentTag = Left(CurrentTag, Temp - 1)
                        Exit For
                    End If
                Next
                Goto ProcessNextLine
            End If
            
            'Self closing section
            If Right(DataSource, 2) = "/>" Then Goto ProcessNextLine
            
            'Start of section
            If Left(DataSource, 1) = "<" Then
                TempData = Mid(DataSource, 2): TempData = Left(TempData, LEN(TempData) - 1)
                CurrentTag = CurrentTag + "\" + TempData
                Goto ProcessNextLine
            End If
            
            'Data
            If DataSource <> "" Then
                'Values
                Select Case Trim(UCase(CurrentTag))
                    Case "\AVRPART\ADMIN\PART_NAME":
                        ChipNameTidy = DataSource
                    Case "\AVRPART\MEMORY\PROG_FLASH":
                        ChipWords = VAL(DataSource) / 1024
                    Case "\AVRPART\MEMORY\EEPROM":
                        ChipEEPROM = VAL(DataSource)
                    Case "\AVRPART\MEMORY\INT_SRAM\SIZE":
                        ChipRAM = VAL(DataSource)
                    Case "\AVRPART\MEMORY\INT_SRAM\START_ADDR":
                        RamStart = VAL("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\ADMIN\SPEED":
                        ChipMHZ = VAL(DataSource)
                    Case "\AVRPART\CORE\CORE_VERSION":
                        ChipFamily = 100 + VAL(Mid(DataSource, 2, 1)) * 10
                        'If INSTR(UCase(DataSource), "E") <> 0 Then ChipFamily += 1
                    Case "\AVRPART\CORE\GP_REG_FILE\NMB_REG":
                        GPRCounTemp = VAL(DataSource)
                    Case "\AVRPART\CORE\GP_REG_FILE\X_REG_HIGH":
                        RegXH = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\CORE\GP_REG_FILE\X_REG_LOW":
                        RegXL = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\CORE\GP_REG_FILE\Y_REG_HIGH":
                        RegYH = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\CORE\GP_REG_FILE\Y_REG_LOW":
                        RegYL = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\CORE\GP_REG_FILE\Z_REG_HIGH":
                        RegZH = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\CORE\GP_REG_FILE\Z_REG_LOW":
                        RegZL = Val("&H" + Mid(DataSource, 2))
                    Case "\AVRPART\PACKAGE\DIP\NMB_PIN":
                        ChipPins = VAL(DataSource)
                    Case "\AVRPART\PACKAGE\PDIP\NMB_PIN":
                        ChipPins = VAL(DataSource)
                
                End Select
                
                'Pinout
                TempData = UCase(Trim(CurrentTag))
                If INSTR(TempData, "\AVRPART\PACKAGE") = 1 Then
                    IF INSTR(TempData, "\NAME") <> 0 Or INSTR(TempData, "\PIN_NAME") <> 0 Or INSTR(TempData, "\ALT_NAME") <> 0 Then
                        'Print TempData
                        CurrentPinS = MID(TempData, INSTR(TempData, "\PACKAGE\") + 9)
                        CurrentPinS = UCase(Left(CurrentPinS, Instr(CurrentPinS, "\") - 1))
                        If CurrentPinS = "PDIP" Then CurrentPinS = "DIP"
                        CurrentPinout = 0
                        For FindPin = 1 to PinoutCount
                            If CurrentPinS = Pinouts(FindPin).Name Then CurrentPinout = FindPin: Exit For
                        Next
                        If CurrentPinout = 0 Then
                            PinoutCount += 1
                            CurrentPinout = PinoutCount
                            With Pinouts(CurrentPinout)
                                .Name = CurrentPinS
                                .PinCount = 0
                            End With
                        End If
                        
                        With Pinouts(CurrentPinout)
                            TidyIn = DataSource
                            If Left(TidyIn, 1) = "[" Then TidyIn = Mid(TidyIn, 2, Len(TidyIn) - 2)
                            Do While INSTR(TidyIn, "_") <> 0
                                If MID(TidyIn, INSTR(TidyIn, "_") + 1, 1) = "A" Then
                                    Replace TidyIn, "_", Chr(31)
                                ElseIf MID(TidyIn, INSTR(TidyIn, "_") + 1, 1) = "B" Then
                                    Replace TidyIn, "_", Chr(31)
                                Else
                                    'Print TidyIn
                                    Replace TidyIn, "_", ":"
                                End If
                            Loop
                            Do While INSTR(TidyIn, Chr(31)) <> 0: Replace TidyIn, Chr(31), "_": Loop
                            PinNameTemp = Mid(TempData, INSTR(TempData, "PIN") + 3)
                            PinNameTemp = Left(PinNameTemp, INSTR(PinNameTemp, "\") - 1)
                            PinNo = VAL(PinNameTemp)
                            CurrentPin = 0
                            For FP = 1 to .PinCount
                                If .PinList(FP).Number = PinNo Then CurrentPin = FP: Exit For
                            Next
                            If CurrentPin = 0 Then
                                .PinCount += 1
                                CurrentPin = .PinCount
                                With .PinList(CurrentPin)
                                    .Number = PinNo
                                    .FunctionCount = 0
                                End With
                            End If
                            With .PinList(CurrentPin)
                                If INSTR(TidyIn, ":") <> 0 Then
                                    PinKey = Left(TidyIn, INSTR(TidyIn, ":") - 1)
                                Else
                                    PinKey = TidyIn
                                End If
                                IF Left(PinKey, 1) = "'" Then PinKey = Mid(PinKey, 2)
                                .FunctionCount += 1
                                .FunctionList(.FunctionCount, 1) = PinKey
                                .FunctionList(.FunctionCount, 2) = "IO" 'Should fix later
                                If INSTR(PinKey, "VCC") <> 0 Or INSTR(PinKey, "GND") <> 0 OR Left(PinKey, 4) = "XTAL" OR PinKey = "AREF" OR PinKey = "RESET" Then
                                    .FunctionList(.FunctionCount, 2) = ""
                                ElseIf INSTR(PinKey, "ADC") Or INSTR(PinKey, "AIN") <> 0 Then
                                    .FunctionList(.FunctionCount, 2) = "I"
                                End If
                                
                                Do While INSTR(TidyIn, ":") <> 0
                                    TidyIn = Mid(TidyIn, INSTR(TidyIn, ":") + 1)
                                    TempData = UCase(TidyIn)
                                    IF INSTR(TempData, ":") <> 0 THEN TempData = Left(TempData, INSTR(TempData, ":") - 1)
                                    IF Left(TempData, 1) = "'" Then TempData = Mid(TempData, 2)
                                    .FunctionCount += 1
                                    .FunctionList(.FunctionCount, 1) = TempData
                                    .FunctionList(.FunctionCount, 2) = "IO" 'Should fix later
                                    IF INSTR(TempData, "ADC") Or INSTR(TempData, "AIN") <> 0 Then
                                        .FunctionList(.FunctionCount, 2) = "I"
                                    End If
                                Loop
                            End With
                        End With
                    End If
                    
                End If
                
                'Number of IO pins
                TempData = UCase(Trim(CurrentTag))
                If INSTR(TempData, "\AVRPART\IO_MODULE\PORT") = 1 And INSTR(TempData, "\BIT") <> 0 And Right(TempData, 12) = "\DESCRIPTION" AND INSTR(LCase(DataSource), "data register bit") <> 0 Then
                    ChipIO += 1
                End If
                
                'Number of ADC pins
                TempData = UCase(Trim(CurrentTag))
                If INSTR(TempData, "\AVRPART\PACKAGE") = 1 And Right(TempData, 5) = "\NAME" AND INSTR(DataSource, "ADC") <> 0 Then
                    Temp = VAL(Mid(DataSource, INSTR(DataSource, "ADC") + 3)) + 1
                    If Temp > ChipADC Then ChipADC = Temp
                End If
                
                'Registers and bits (SFR)
                TempData = UCase(Trim(CurrentTag))
                If INSTR(TempData, "\AVRPART\IO_MODULE") = 1 Then
                    'SFR
                    If Right(TempData, 5) = "\NAME" And INSTR(TempData, "\BIT") = 0 Then
                         LastRegName = DataSource

                    ElseIf Right(TempData, 8) = "\IO_ADDR" Then
                        'Check for duplicate
                        DF = 0
                        For CD = 1 To SVC
                            If SysVars(CD).Name = LastRegName Then DF = CD: Exit For
                        Next
                        LastNA = 0
                        If DF = 0 And DataSource <> "NA" Then
                            SVC += 1
                            SysVars(SVC).Name = LastRegName
                            If LEFT(DataSource, 1) = "$" Then 
                                SysVars(SVC).Loc = VAL("&H" + Trim(Mid(DataSource, 2)))
                            Else
                                SysVars(SVC).Loc = VAL("&H" + Trim(Mid(DataSource, 3)))
                            End If
                        ElseIf DataSource = "NA" Then
                            LastNA = -1
                        End If
                        
                    ElseIF RIGHT(TempData, 9) = "\MEM_ADDR" And LastNA Then
                        'Check for duplicate
                        DF = 0
                        For CD = 1 To SVC
                            If SysVars(CD).Name = LastRegName Then DF = CD: Exit For
                        Next
                        LastNA = 0
                        If DF = 0 Then
                            SVC += 1
                            SysVars(SVC).Name = LastRegName
                            If LEFT(DataSource, 1) = "$" Then 
                                SysVars(SVC).Loc = VAL("&H" + Trim(Mid(DataSource, 2)))
                            Else
                                SysVars(SVC).Loc = VAL("&H" + Trim(Mid(DataSource, 3)))
                            End If
                        End If
                    
                    'Bit
                    ElseIF Right(TempData, 5) = "\NAME" And INSTR(TempData, "\BIT") <> 0 Then
                        
                        'Temp = VAL("&H" + Mid(TempData, INSTR(TempData, "\BIT") + 4))
                        TempData = Mid(TempData, INSTR(TempData, "\BIT") + 4)
                        If INSTR(TempData, "\") <> 0 Then TempData = LEFT(TempData, INSTR(TempData, "\") - 1)
                        Temp = VAL("&H" + TempData)
                        
                        DF = 0
                        For CD = 1 To SVBC
                            If SysVarBits(CD).Name = DataSource Then DF = CD: Exit For
                        Next
                        
                        If DF = 0 Then
                            SVBC += 1
                            SysVarBits(SVBC).Name = DataSource
                            SysVarBits(SVBC).Bit = Temp
                            SysVarBits(SVBC).Reg = LastRegName
                        End If
                    End If
                End If
                
                'Interrupt vector
                TempData = UCase(Trim(CurrentTag))
                If INSTR(TempData, "\AVRPART\INTERRUPT_VECTOR\") = 1 Then
                    IF Right(TempData, 7) = "\SOURCE" Then
                        Temp = VAL(Mid(TempData, INSTR(TempData, "\VECTOR") + 7))
                        'If Temp > IntC Then IntC = Temp
                        IF INSTR(DataSource, ",") <> 0 Then Replace DataSource, ",", "_"
                        DataSource = Trim(DataSource)
                        Do While INSTR(DataSource, "\") <> 0: Replace DataSource, "\", "": Loop
                        Do While INSTR(DataSource, "/") <> 0: Replace DataSource, "/", "": Loop
                        Do While INSTR(DataSource, " ") <> 0: Replace DataSource, " ", "_": Loop
                        Do While INSTR(DataSource, ";") <> 0: Replace DataSource, ";", "_": Loop
                        Do While INSTR(DataSource, "__") <> 0: Replace DataSource, "__", "_": Loop
                        'IntVecTempData(Temp, 1) = DataSource
                        VectLoc = 0
                        For FV = 1 to ILC
                            If IntList(FV).VectorID = Temp Then VectLoc = FV: Exit For
                        Next
                        If VectLoc = 0 Then
                            ILC += 1
                            VectLoc = ILC
                            IntList(VectLoc).VectorID = Temp
                        End If
                        IntList(VectLoc).Vector = DataSource
                        
                    End If
                    If Right(TempData, 16) = "\PROGRAM_ADDRESS" Then
                        Temp = VAL(Mid(TempData, INSTR(TempData, "\VECTOR") + 7))
                        'If Temp > IntC Then IntC = Temp
                        'IntVecTempData(Temp, 2) = Str(VAL("&H" + Mid(DataSource, 2)))
                        VectLoc = 0
                        For FV = 1 to ILC
                            If IntList(FV).VectorID = Temp Then VectLoc = FV: Exit For
                        Next
                        If VectLoc = 0 Then
                            ILC += 1
                            VectLoc = ILC
                            IntList(VectLoc).VectorID = Temp
                        End If
                        IntList(VectLoc).VectorLoc = VAL("&H" + Mid(DataSource, 2))
                    End If
                End If
                
            End If
                        
            ProcessNextLine:
        Next
    Loop
    CLOSE #1
    
	
End Sub

Sub ReadXml6 (FileName As String)
	'Read from xml file from Atmel Studio 6
	Dim CurrChild As Integer
	Dim As Integer CurrAddSpace, CurrMemSeg, CurrInterrupt
	Dim As Integer CurrModule, CurrRegGroup, CurrRegister, CurrBit
	Dim As ext.xml.node Ptr xmlroot, DeviceFile
	Dim As ext.xml.node Ptr Devices, Device, Interrupts, Interrupt
	Dim As ext.xml.node Ptr AddSpaces, AddSpace, MemSeg
	Dim As ext.xml.node Ptr Modules, Module, RegGroup, RegBit, Register
	Dim As Integer RegLoc, RegBitNo, RegMask, CurrShift, RegBitCount
	Dim As Integer DF, CD, CurrBitNo
	Dim As String RegBitName
	
	'Note: FreeBASIC xml parser won't read files that start with BOM (3 bytes) - causes segfault
	'Remove this header as a workaround
	Dim As Integer FileIn, FileOut, CurrLoc
	Dim TempByte As UByte
	Dim TempLine As String
	FileIn = FreeFile
	Open FileName For Binary As FileIn
	Get #FileIn, 0, TempByte
	If TempByte = 239 Then
		'Print "XML file contains BOM, removing"
		FileOut = FreeFile
		Open TempDataFile For Binary As FileOut
		For CurrLoc = 3 To Lof(FileIn)
			Get #FileIn, CurrLoc, TempByte
			Put #FileOut, CurrLoc - 3, TempByte
		Next
		Close FileOut
		FileName = TempDataFile
	End If
	Close FileIn
	'Also has trouble with tiny1634 file - doesn't seem to like whitespace after opening tags
	
	Var xmldoc = new ext.xml.tree
	xmldoc->load(FileName)
	If xmldoc = 0 Then
		Print "XML file seems empty"
		GoTo FinishRead
	End If
	xmlroot = xmldoc->root
	If xmlroot = 0 Then
		Print "Missing root node"
		GoTo FinishRead
	End If
	
	'Get devicefile
	DeviceFile = xmlroot->Child("avr-tools-device-file")
	If DeviceFile = 0 Then
		Print "Missing device file node"
		GoTo FinishRead
	End If
	
	'Go through device
	Devices = DeviceFile->Child("devices")
	If Devices = 0 Then
		Print "Missing devices node"
	Else
		Device = Devices->Child("device")
		If Device = 0 Then
			Print "Missing device node"
		Else
			'Find address spaces
			AddSpaces = Device->Child("address-spaces")
			If AddSpaces = 0 Then
				Print "Missing address-spaces node"
			Else
				For CurrAddSpace = 0 To AddSpaces->Children("address-space")
					AddSpace = AddSpaces->Child("address-space", CurrAddSpace)
					If AddSpace <> 0 Then
						
						For CurrMemSeg = 0 To AddSpace->Children("memory-segment")
							MemSeg = AddSpace->Child("memory-segment", CurrMemSeg)
							If MemSeg <> 0 Then
								'Have found memory segment, is it for flash, eeprom or IRAM?
								Select Case LCase(AddSpace->Attribute("name"))
									Case "prog"
										If MakeDec(MemSeg->Attribute("start")) = 0 And ChipWords = 0 Then
											ChipWords = MakeDec(MemSeg->Attribute("size")) / 1024
										End If
									Case "data"
										If LCase(MemSeg->Attribute("type")) = "ram" And Val(MemSeg->Attribute("external")) = 0 Then
											ChipRAM = MakeDec(MemSeg->Attribute("size"))
											RamStart = MakeDec(MemSeg->Attribute("start"))
										ElseIf LCase(MemSeg->Attribute("type")) = "regs" Then
											GPRCounTemp = MakeDec(MemSeg->Attribute("size"))
										End If
										
									Case "eeprom"
										ChipEEPROM = MakeDec(MemSeg->Attribute("size"))
										
								End Select
							End If
						Next
					End If
				Next
			End If
			
			'Find interrupts
			Interrupts = Device->Child("interrupts")
			If Interrupts = 0 Then
				Print "Missing interrupts node"
			Else
				For CurrInterrupt = 0 To Interrupts->Children("interrupt")
					Interrupt = Interrupts->Child("interrupt", CurrInterrupt)
					If Interrupt <> 0 Then
						'Print Interrupt->Attribute("name"), Interrupt->Attribute("index")
						ILC += 1
						IntList(ILC).Vector = Interrupt->Attribute("name")
						If ChipWords > 8 Then
							IntList(ILC).VectorLoc = MakeDec(Interrupt->Attribute("index")) * 2
						Else
							IntList(ILC).VectorLoc = MakeDec(Interrupt->Attribute("index"))
						End If
						
						IntList(ILC).EventName = ""
					End If
				Next
			End If
		End If
		
	End If
	
	'Go through modules
	Modules = DeviceFile->Child("modules")
	If Modules = 0 Then
		Print "Missing modules node"
		GoTo FinishRead
	End If
	For CurrModule = 0 To Modules->Children("module")
		Dim ModName As String
		Module = Modules->Child("module", CurrModule)
		If Module <> 0 Then
			'Found a module
			'Print Module->Attribute("name")
			
			'Get register groups
			For CurrRegGroup = 0 To Module->Children("register-group")
				RegGroup = Module->Child("register-group", CurrRegGroup)
				If RegGroup <> 0 Then
					'Print ,RegGroup->Attribute("name")
					
					'Get registers
					For CurrRegister = 0 To RegGroup->Children("register")
						Register = RegGroup->Child("register", CurrRegister)
						If Register <> 0 Then
							'Have found a register
							'Print ,,Register->Attribute("name"); ":"; Register->Attribute("offset")
							RegLoc = MakeDec(Register->Attribute("offset"))
							If RegLoc > 32 Then
								'First registers get addresses 0-63 (IO space)
								'Subsequent ones get 96 and up (RAM space)
								If RegLoc < 96 Then RegLoc -= 32
								
								If MakeDec(Register->Attribute("size")) = 1 Then
									SVC += 1
									SysVars(SVC).Name = Register->Attribute("name")
									SysVars(SVC).Loc = RegLoc '- 32
								Else
									SVC += 1
									SysVars(SVC).Name = Register->Attribute("name") + "L"
									SysVars(SVC).Loc = RegLoc '- 32
									SVC += 1
									SysVars(SVC).Name = Register->Attribute("name") + "H"
									SysVars(SVC).Loc = RegLoc + 1'- 31
								End If
								
								'Find bits
								For CurrBit = 0 To Register->Children("bitfield")
									RegBit = Register->Child("bitfield", CurrBit)
									If RegBit <> 0 Then
										'Have found a bit/bits
										'Print ,,,RegBit->Attribute("name")
										RegMask = MakeDec(RegBit->Attribute("mask"))
										If RegMask <> -1 Then
											
											'Count bits
											RegBitCount = 0
											For CurrShift = 0 To 7
												If (RegMask And 2 ^ CurrShift) <> 0 Then
													RegBitCount += 1
												End If
											Next
											
											'lsb specified?
											CurrBitNo = 0
											If RegBit->Attribute("lsb") <> "" Then
												CurrBitNo = Val(RegBit->Attribute("lsb"))
											End If
											
											'Add to SFR bit list
											For CurrShift = 0 To 7
												If (RegMask And 2 ^ CurrShift) <> 0 Then
													'Have found exact bit number
													RegBitName = RegBit->Attribute("name")
													If RegBitCount > 1 Then
														RegBitName += Str(CurrBitNo)
														CurrBitNo += 1
													End If
													
													'Check for duplicate
													DF = 0
							                        For CD = 1 To SVBC
							                            If SysVarBits(CD).Name = RegBitName Then DF = CD: Exit For
							                        Next
							                        
							                        If DF = 0 Then
														'Add bit
														SVBC += 1
														SysVarBits(SVBC).Name = RegBitName
														SysVarBits(SVBC).Reg = Register->Attribute("name")
														SysVarBits(SVBC).Bit = CurrShift
							                        End If
													
												End If
											Next
										End If
										
									End If
									
								Next
								
							End If
						End If
						
					Next
					
				End If
				
			Next
			
		End If
	Next
	
	FinishRead:
	Delete xmldoc
	
	'Guess some things
	RegXL = 26
	RegXH = 27
	RegYL = 28
	RegYH = 29
	RegZL = 30
	RegZH = 31
	ChipFamily = 120
	
	'Try to open .inc file, may need this for extra bit definitions
	ReadIncFile
	
End Sub

Sub ReadIncFile
	'Read inc file for current chip, extract any extra info
	
	Dim As ext.xml.node Ptr xmlroot, DeviceFile, DeviceList, Device
	Dim As Integer CurrentDevice, IncFile, ReadState
	Dim As String DevName, IncFileName, InLine, CurrReg, EquName, EquValue
	
	Var xmldoc = new ext.xml.tree
	xmldoc->load(IncFileDir + "\supported-devices.xml")
	If xmldoc = 0 Then
		Print "Devices XML file seems empty"
		GoTo FinishIncRead
	End If
	xmlroot = xmldoc->root
	If xmlroot = 0 Then
		Print "Devices file missing root node"
		GoTo FinishIncRead
	End If
	
	'Get devicefile
	DeviceFile = xmlroot->Child("supported-devices")
	If DeviceFile = 0 Then
		Print "Missing device file node"
		GoTo FinishIncRead
	End If
	
	'Get device list
	DeviceList = DeviceFile->Child("devices")
	If DeviceList = 0 Then
		Print "Missing device list"
		GoTo FinishIncRead
	EndIf
	
	IncFileName = ""
	For CurrentDevice = 0 To DeviceList->Children("device")
		Device = DeviceList->Child("device", CurrentDevice)
		If Device <> 0 Then
			DevName = LCase(Device->Attribute("name"))
			If Left(DevName, 2) = "at" Then DevName = Mid(DevName, 3)
			If DevName = LCase(ChipName) Then
				IncFileName = Device->Attribute("header")
				Exit For
			End If
		End If
	Next
	
	If IncFileName <> "" Then
		CurrReg = ""
		ReadState = 0 '0 = nothing, 1 = registers, 2 = bits of CurrReg
		
		IncFile = FreeFile
		Open IncFileDir + "\include\" + IncFileName For Input As IncFile
		Do While Not Eof(IncFile)
			Line Input #IncFile, InLine
			Do While InStr(InLine, Chr(9))
				Replace InLine, Chr(9), " "
			Loop
			InLine = UCase(Trim(InLine))
			
			
			If Left(InLine, 1) = ";" Then
				If InStr(InLine, "REGISTER DEFINITIONS") <> 0 And InStr(InLine, "CPU") = 0 Then
					ReadState = 1
					
				ElseIf InStr(InLine, "-") <> 0 Then
					CurrReg = Left(InLine, InStr(InLine, "-") - 1)
					CurrReg = Trim(Mid(CurrReg, 2))
					'Is CurrReg referring to an SFR?
					If HasSFR(CurrReg) Then
						ReadState = 2
					Else
						CurrReg = ""
						ReadState = 0
					End If
					
				ElseIf ReadState = 2 Then
					ReadState = 0
				End If
			
			ElseIf Left(InLine, 4) = ".EQU" And ReadState <> 0 Then
				EquName = LTrim(Mid(InLine, 5))
				EquName = RTrim(Left(EquName, InStr(EquName, "=") - 1))
				
				EquValue = LTrim(Mid(InLine, InStr(InLine, "=") + 1))
				If InStr(EquValue, ";") <> 0 Then
					EquValue = RTrim(Left(EquValue, InStr(EquValue, ";") - 1))
				End If
				
				'Is this a register or bit?
				If ReadState = 1 Then
					If Not HasSFR(EquName) Then
						SVC += 1
						SysVars(SVC).Name = EquName
						If IsConst(EquValue) Then
							SysVars(SVC).Loc = Val(EquValue)
						Else
							SysVars(SVC).Loc = GetSFRLoc(EquValue)
						End If
						If SysVars(SVC).Loc = -1 Then
							SVC -= 1
						End If
					End If
					
				ElseIf ReadState = 2 Then
					If Val(EquValue) < 8 And Not HasSFRBit(EquName) Then
						SVBC += 1
						SysVarBits(SVBC).Name = EquName
						'Bit might be reference to another
						If IsConst(EquValue) Then
							SysVarBits(SVBC).Bit = Val(EquValue)
						Else
							'Need to look up bit location
							SysVarBits(SVBC).Bit = GetSFRBitLoc(EquValue)
						End If
						SysVarBits(SVBC).Reg = CurrReg
						If SysVarBits(SVBC).Bit = -1 Then
							SVBC -= 1
						End If
					End If
					
				End If
				
			End If
			
		Loop
		
		Close IncFile
	End If
	
	
	FinishIncRead:
	Delete xmldoc
	
End Sub

Function GetSFRLoc(SFRName As String) As Integer
	Dim As Integer PD
	Dim As String TidiedName
	
	'Search system variable list to find register
	TidiedName = UCase(Trim(SFRName))
	For PD = 1 To SVC
		If SysVars(PD).Name = TidiedName Then Return SysVars(PD).Loc 
	Next
	
	Return -1
End Function

Function GetSFRBitLoc(BitName As String) As Integer
	Dim As Integer FSFR
	Dim As String BitNameUpper
	BitNameUpper = UCase(BitName)
	
	For FSFR = 1 to SVBC
		If UCASE(SysVarBits(FSFR).Name) = BitNameUpper Then
			Return SysVarBits(FSFR).Bit
		End If
	Next
	
	Return -1
End Function

Function HasSFR(SFRName As String) As Integer
	Dim As Integer PD
	Dim As String TidiedName
	
	'Search system variable list to find register
	TidiedName = UCase(Trim(SFRName))
	For PD = 1 To SVC
		If SysVars(PD).Name = TidiedName Then Return -1 
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

FUNCTION IsConst (DataSource As String) As Integer
	
	Dim As String Temp

	Temp = Trim(DataSource)
	IF Left(Temp, 1) = "-" THEN Temp = Mid(Temp, 2)
	
	IsConst = 0
	IF Trim(Temp) = Trim(Str(VAL(Temp))) THEN IsConst = -1
	IF INSTR(LCase(Temp), "b'") <> 0 THEN IsConst = -1
	IF INSTR(LCase(Temp), "0x") <> 0 THEN IsConst = -1
	
	IF INSTR(Temp, "@") <> 0 THEN IsConst = -1
	If INSTR(Temp, ";STRING") <> 0 Then IsConst = -1
	
	IF INSTR(Temp, "+") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "-") > 1 THEN IsConst = 0
	IF INSTR(Temp, "*") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "/") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "%") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "&") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "|") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "!") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "#") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "=") <> 0 THEN IsConst = 0
	IF INSTR(Temp, "<") <> 0 THEN IsConst = 0
	IF INSTR(Temp, ">") <> 0 THEN IsConst = 0

END Function

FUNCTION IsDivider (Temp As String) As Integer
    
    Select Case Temp
    Case " ", "(", ")", ",", ".", ":", ";", "+", "-", "*", "/", "%": Return -1
    Case "=", "!", "<", ">", "{", "}", "~", "&", "|", "#": Return -1
    Case Else: Return 0
    End Select
    
END Function

Function MakeDec(InVal As String) As Integer
	If LCase(Left(InVal, 2)) = "0x" Then
		Return Val("&H" + Mid(InVal, 3))
	Else
		Return Val(InVal)
	End If
End Function

Function MaskToBit(InMask As Integer) As Integer
	Dim As Integer CurrShift, OutVal
	
	OutVal = -1
	For CurrShift = 0 To 7
		If (InMask And 1) = 1 Then
			OutVal = CurrShift
			Exit For
		End If
		InMask = InMask \ 2
	Next
	
	Return OutVal
End Function

SUB Replace (DataVar As String, Find As String, Rep As String)
    Dim As String VarTemp, FindTemp, NewData
    
    VARTemp = UCase(DataVar): FINDTemp = UCase(Find)
    IF INSTR(VARTemp, FINDTemp) = 0 THEN DataVar = DataVar + Rep: EXIT SUB
    
    NewData = Left(DataVar, INSTR(VARTemp, FINDTemp) - 1)
    NewData = NewData + Rep
    NewData = NewData + Mid(DataVar, INSTR(VARTemp, FINDTemp) + LEN(Find))
    
    DataVar = NewData
END Sub

Sub SetSFR(SFRName As String, Location As Integer)
	'Forcibly set SFR bit to new values, overwriting existing values
	Dim As Integer EditPos, SearchPos
	
	'Does bit already exist and need to be replaced?
    EditPos = -1
    For SearchPos = 1 To SVC
    	If SysVars(SearchPos).Name = SFRName Then
    		EditPos = SearchPos
    		Exit For
    	End If
    Next
    
    If EditPos = -1 Then
    	SVC += 1
    	EditPos = SVC
    End If
    
    With SysVars(EditPos)
    	.Name = SFRName
    	.Loc = Location
    End With
    
End Sub

Sub SetSFRBit(BitName As String, Reg As String, BitNo As Integer)
	'Forcibly set SFR bit to new values, overwriting existing values
	Dim As Integer EditPos, SearchPos
	
	'Does bit already exist and need to be replaced?
    EditPos = -1
    For SearchPos = 1 To SVBC
    	If SysVarBits(SearchPos).Name = BitName Then
    		EditPos = SearchPos
    		Exit For
    	End If
    Next
    
    If EditPos = -1 Then
    	SVBC += 1
    	EditPos = SVBC
    End If
    
    With SysVarBits(EditPos)
    	.Name = BitName
    	.Reg = Reg
    	.Bit = BitNo
    End With
    
End Sub

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
