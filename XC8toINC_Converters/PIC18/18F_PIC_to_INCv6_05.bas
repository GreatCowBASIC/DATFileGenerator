' issue _BADRAM  H'0800'-H'37FF' is missing from 25K42 = why?



'This examines the INDEX_IDX file in the MPLAB-X directory and returns the version etc for a specific Chip


#include once "ext/xml/dom.bi"

#DEFINE kINDEX_IDX      "C:\Program Files\Microchip\MPLABX\v6.05\packs"


#DEFINE kUniqueBits "INTN,INTP,OUT,POL,ON,C1OUT,C2OUT,GPOL,IOEN,CS,NOT,CKPS0,CKPS1,CKPS2,CS0,CS1,CS2,OV,SEN,BUSY,RDY,STAT0,STAT1,SPI1MD,GO,IPEN,NOT_DONE,NCH0,NCH1,NCH2,PSS0,PSS1,LD,CPON,PR0,PR1,PR2"
#DEFINE kKillRegister "W,DATA,FSR2,FSR1,FSR0"
#DEFINE kKillBits "DATA"
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
End Type

Dim Shared SourceFileArray(10000) As SourceFiles
Dim Shared SourceFileArrayPointer as Integer = 0
Dim Shared SFRBitsArrayPointer as Integer = 0
Dim Shared SFRDataCount as Integer = 0
Dim Shared SFRData(3000) as SFR
Dim Shared SFRBits(20000) as SFRBits
Dim Shared SFRUniques() as string
Dim Shared currentitem as string
Dim Shared BadRam(100) as string
Dim Shared BadRamPointer as Integer = 0

Declare Sub PopulateSourceFilesLocation ( kINDEX_IDX_location as string )
Declare Function FindChip ( chiptofind as string ) as string
Declare SUB Replace (DataVar As String, Find As String, Rep As String)
Declare Sub Split(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)
Declare Function RegisterString( chipstringtofind as string ) as string
Declare Sub PopulateRAMDefinitions ( SourceFolder as string, Version as string, Chip as string )
Declare Sub PopulateConfig ( SourceFolder as string, Version as string, Chip as string )

PopulateSourceFilesLocation  ( kINDEX_IDX )

  Dim CD as integer = 1
  If COMMAND(CD) <> "" then
    Dim ParamUpper as string = Ucase(COMMAND(CD))
    Dim targetchip as string
    Dim As String chip, chipdetails
    Dim chipparameters() as string
    Dim fsp_ini as string
    Dim folder_DFP as string

    targetchip = ParamUpper
'    print kINDEX_IDX


    if chipdetails = "NOCHIP" then
        print "Not a valid chip"
        end
    end if

    chip = targetchip
    If left(targetchip,3) <> "PIC" then
      targetchip = "PIC"+targetchip
    End if

    chipdetails = FindChip ( targetchip )

    split( chipdetails, ",",-1,chipparameters() )

    fsp_ini = kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\xc8\pic\dat\ini\"+ mid(targetchip,4)  +".ini"

    Print "        LIST"
    Print ";=========================================================================="
    Print ";"
    Print ";  Built by Great Cow BASIC convertor"
    Print ";  MPASM processor include for the chip shown below"
    Print ";"
    Print "; " + kINDEX_IDX+"\index.idx"
    Print "; " + fsp_ini
    print "; " + kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\edc\"+ mid(targetchip,4)  +".PIC"
    print "; " + kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\xc8\pic\dat\cfgdata\"+ mid(targetchip,4)  +".cfgdata"
    print ";"
    Print ";=========================================================================="

    Print "        NOLIST"
    Print "        IFNDEF __" +  chip
    Print "           MESSG " + chr(34) + "Processor-header file mismatch.  Verify selected processor." + chr(34)
    Print "        ENDIF"

    Print ";=========================================================================="
    Print ";       18xxxx Family        EQUates"
    Print ";=========================================================================="

    Print "FSR0             EQU  0"
    Print "FSR1             EQU  1"
    Print "FSR2             EQU  2"

    Print "FAST             EQU  1"

    Print "W                EQU  0"
    Print "A                EQU  0"
    Print "ACCESS           EQU  0"
    Print "BANKED           EQU  1"

    Print ";=========================================================================="
    Print ";"
    Print ";       Register Definitions"
    Print ";"
    Print ";=========================================================================="
    Print ""
    Print ";----- Register Files -----------------------------------------------------"



    Dim SFRRegister() as string
    Dim DataSource as string
    Dim RegistersToBeIgnored() as string
    Dim as Integer RegistersToBeIgnoredCounter, FoundRegistersToBeIgnored

    if instr(kKillRegister,",") <> 0 then
      Split ( kKillRegister, ",", -1, RegistersToBeIgnored() )
    else
      redim RegistersToBeIgnored(1)
      RegistersToBeIgnored(0) = trim(kKillRegister)
    endif



    open fsp_ini for input as #1
    If Err>0 Then Print "Error opening the file "+chr(34)+fsp_ini+chr(34):Print "Update MPLKAB-IDE DFP Pack with pack version " +chipparameters(1) :End

    do while not eof(1)
        Line input #1, DataSource
        If DataSource <> "" and left( datasource, 4 ) = "SFR=" Then
            'check to see if this should be ignored.
            FoundRegistersToBeIgnored = 0
            for RegistersToBeIgnoredCounter = 0 to ubound( RegistersToBeIgnored )
                if  instr( ucase( datasource),   "="+trim(RegistersToBeIgnored ( RegistersToBeIgnoredCounter)+",")) <> 0 then
                    FoundRegistersToBeIgnored = 1
                    print "; "+datasource + " is a reserved word"

                end if
            next
            if FoundRegistersToBeIgnored = 0 then
                SFRDataCount += 1
                SFRData(SFRDataCount).Count = SFRDataCount
                SFRData(SFRDataCount).RegisterName = mid(Trim(UCase(DataSource)),5)
                split( SFRData(SFRDataCount).RegisterName , "," , 99 , SFRRegister() )
                with SFRData(SFRDataCount)
                    .RegisterName = SFRRegister(0)
                    .RegisterAddresss = SFRRegister(1)
                    .RegisterLength = SFRRegister(2)
                    print left(.RegisterName+"                 ",17)+"EQU  H'"+right("000"+.RegisterAddresss,4)+"'"
                end with
            end if
        end if
    loop
    close

    'open Ini for registersbits

'      SFRFLD=NOT_PD,3880,5,1
'      SFRFLD=NOT_TO,3880,6,1
'      SFRFLD=C,3880,0,1
'      SFRFLD=DC,3880,1,1
'      SFRFLD=Z,3880,2,1
'      SFRFLD=OV,3880,3,1
'      SFRFLD=N,3880,4,1
'      SFRFLD=nPD,3880,5,1
'      SFRFLD=nTO,3880,6,1
'      SFRFLD=PD,3880,5,1
'      SFRFLD=TO,3880,6,1

'
'        ;----- STATUS_CSHAD Bits -----------------------------------------------------
'        C                EQU  H'0000'
'        DC               EQU  H'0001'
'        Z                EQU  H'0002'
'        OV_STATUS_CSHAD  EQU  H'0003'
'        N                EQU  H'0004'
'        NOT_PD           EQU  H'0005'
'        NOT_TO           EQU  H'0006'
'
'        PD               EQU  H'0005'
'        TO               EQU  H'0006'


'        Type SFRBits
'          RegisterRaw as String
'          RegisterBitName as String
'          RegisterBitAddress as String
'          Count as Integer
'        End Type

    Dim RegisterBits() as string
    Dim BitsToBeIgnored() as string
    Dim CurrentRegister as string = ""

    if instr(kKillBits,",") <> 0 then
      Split ( kKillBits, ",", -1, BitsToBeIgnored() )
    else
      redim BitsToBeIgnored(1)
      BitsToBeIgnored(0) = trim(kKillBits)
    endif


    open fsp_ini for input as #1
    SFRBitsArrayPointer = 0
    do while not eof(1)
        Line input #1, DataSource
        If DataSource <> "" and left( datasource, 7 ) = "SFRFLD=" Then
            DataSource = ucase(mid( DataSource, 8 ))
            split ( DataSource, ",", 99, RegisterBits() )
            'only handle single bits
            if  trim(RegisterBits(3)) = "1" then
              'new register? Output the break line
              if RegisterBits(1) <> CurrentRegister then
                  SFRBitsArrayPointer+=1: SFRBits(SFRBitsArrayPointer).RegisterRaw="":SFRBits(SFRBitsArrayPointer).Count=SFRBitsArrayPointer  'add a line
                  SFRBitsArrayPointer+=1: SFRBits(SFRBitsArrayPointer).RegisterRaw="":SFRBits(SFRBitsArrayPointer).Count=SFRBitsArrayPointer  'add a line
                  SFRBitsArrayPointer+=1
                    SFRBits(SFRBitsArrayPointer).RegisterRaw=";----- "+ RegisterString(RegisterBits(1)) + " Bits -----------------------------------------------------"
                    SFRBits(SFRBitsArrayPointer).Count=SFRBitsArrayPointer

                  CurrentRegister = RegisterBits(1)
              end if
              SFRBitsArrayPointer+=1
              SFRBits(SFRBitsArrayPointer).RegisterRaw = left(RegisterBits(0)+"                ",17)+"EQU  H'"+right("0000"+RegisterBits(2),4)+"'"
              SFRBits(SFRBitsArrayPointer).RegisterBitName = RegisterBits(0)
              SFRBits(SFRBitsArrayPointer).RegisterBitAddress = RegisterBits(2)
              SFRBits(SFRBitsArrayPointer).Count = SFRBitsArrayPointer
              SFRBits(SFRBitsArrayPointer).RegisterName = RegisterString(CurrentRegister)
            end if
        End if
    loop
    close

'    'Output Bit array
'    SFRBitsArrayPointer = 0
'    do
'        SFRBitsArrayPointer+=1
'        Print SFRBits(SFRBitsArrayPointer).RegisterRaw,
'        Print SFRBits(SFRBitsArrayPointer).RegisterBitName
'
'    loop until SFRBits(SFRBitsArrayPointer).Count = 0


    'look for repeats
    SFRBitsArrayPointer = 1
    dim repeated as Integer = 0
    dim searchpointer as Integer = 1
    split ( kUniqueBits, ",", 99, SFRUniques() )

    'search for duplicate bit names
    do
      currentitem = trim(ucase(SFRBits(SFRBitsArrayPointer).RegisterBitName))

      if currentitem <> "" then
        for searchpointer = 0 to ubound( SFRUniques ) -1

            if currentitem = TRIM(SFRUniques( searchpointer )) then
                'Mark for rename
                SFRBits(SFRBitsArrayPointer).RegisterBitRename = 1

            end if

        next
      end if

      SFRBitsArrayPointer+=1

    loop until SFRBits(SFRBitsArrayPointer).Count = 0


    'rename stuff
    SFRBitsArrayPointer = 0
    do
        SFRBitsArrayPointer+=1
        if SFRBits(SFRBitsArrayPointer).RegisterBitRename = 1 then
            'rename
            with SFRBits(SFRBitsArrayPointer)
              .RegisterBitName = .RegisterBitName+"_"+.RegisterName
              .RegisterRaw = left(.RegisterBitName+"                ",17)+"EQU  H'"+right("0000"+.RegisterBitAddress,4)+"'"
            End With
        end if

    loop until SFRBits(SFRBitsArrayPointer).Count = 0


    'Kill Bits array
    SFRBitsArrayPointer = 0
    dim lenOfKillLbit as Integer
    dim rawRegister as string
    dim Killbit as string
    do
        SFRBitsArrayPointer+=1
        for searchpointer = 0 to ubound(BitsToBeIgnored)
            rawRegister  = ucase ( SFRBits(SFRBitsArrayPointer).RegisterRaw )
            lenOfKillLbit = len(trim(BitsToBeIgnored( searchpointer )))+1
            Killbit = ucase(trim(BitsToBeIgnored( searchpointer )))+" "
            if left( rawRegister, lenOfKillLbit) = Killbit and Killbit<> "" then
                SFRBits(SFRBitsArrayPointer).RegisterRaw = "; Register.bit not unique    "+SFRBits(SFRBitsArrayPointer).RegisterRaw
            end if
        next


    loop until SFRBits(SFRBitsArrayPointer).Count = 0



    'Output Bit array
    SFRBitsArrayPointer = 0
    do
        SFRBitsArrayPointer+=1
        Print SFRBits(SFRBitsArrayPointer).RegisterRaw
    loop until SFRBits(SFRBitsArrayPointer).Count = 0



    print ";=========================================================================="
    print ";"
    print ";       RAM Definitions"
    print ";"
    print ";=========================================================================="

    PopulateRAMDefinitions ( chipparameters(3), chipparameters(1), targetchip )

    Dim loopcounter as Integer
    for loopcounter = 0 to badrampointer
        print trim(badram(loopcounter))
    next

    print ""
    print ""
    print ";=========================================================================="
    print "; The following is an assignment of address values for all of the"
    print "; configuration registers for the purpose of table reads"
    PopulateConfig ( chipparameters(3), chipparameters(1), mid(targetchip,4) )

    print ""
    print "        LIST"

  else

    Dim loopcounter as Integer
    for loopcounter = 0 to SourceFileArrayPointer - 1
      with SourceFileArray( loopcounter )
          print left(.Pack+"                              ",45)+ left(.ChipName+"                              ",25) +  left(.Version+"                              ",15)+.Core
      end with
    next

  end if

end








Function RegisterString( chipaddresstofind as string ) as string

  Dim loopcounter as Integer
  for loopcounter = 0 to 3000
      if SFRData( loopcounter ).RegisterAddresss = chipaddresstofind   then
          return SFRData( loopcounter ).RegisterName
      end if
  next




End function





Function FindChip ( chiptofind as string ) as string

    Dim PotentialMatches( 100 ) as Integer
    Dim PotentialMatch as Integer = 0
    Dim loopcounter as Integer
    chiptofind = trim(ucase( chiptofind ))


    for loopcounter = 0 to SourceFileArrayPointer - 1
      with SourceFileArray( loopcounter )
          if  trim( .ChipName ) = chiptofind then
              PotentialMatches( PotentialMatch ) = loopcounter
              PotentialMatch= PotentialMatch + 1
          end if
      end with
    next

    dim lastestversion as string = ""
    dim arraypointer as Integer = -1
    if PotentialMatch > 0 then
      for loopcounter = 0 to PotentialMatch - 1
        if SourceFileArray( PotentialMatches ( loopcounter ) ).Version > lastestversion then
            lastestversion = SourceFileArray( PotentialMatches ( loopcounter ) ).Version
            arraypointer = PotentialMatches ( loopcounter )
        end if
      next
    end if

    if arraypointer <> -1 then
      with SourceFileArray( arraypointer )
        return   chiptofind+","+.Version + "," + .Core + "," + .Family +","+.Pack
      end with
    else
        return "NOCHIP"
    end if

End Function


Sub PopulateSourceFilesLocation  (  kindex_idx_location as string  )

  Dim As ext.xml.node Ptr xmlroot
  Dim As ext.xml.node Ptr idx, pdsc, releases, release, devices
  Dim releaseversions() as string
  Dim loopcounter as Integer
  dim latestversion as string = ""

  Var xmldoc = new ext.xml.tree

  xmldoc->load( kindex_idx_location+"\index.idx" )


  If xmldoc = 0 Then
    Print "XML file seems empty"
    GoTo FinishRead
  End If
  xmlroot = xmldoc->root
  If xmlroot = 0 Then
    Print "Missing root node"
    GoTo FinishRead
  End If

  'Get idx
  idx = xmlroot->Child("idx")
  If idx = 0 Then

    Print "Missing idx node in "+ kindex_idx_location+"\index.idx" + "  XML source"

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
                          end with
                          'print devices->Child( "atmel:device" , devicecounter )->Attribute("name")
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
                'print ">"+latestversion

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

      DataSpace = PIC->Child("edc:DataSpace")
      '       __MAXRAM  H'3FFF'
      badrampointer += 1
      badram ( badrampointer ) = "       __MAXRAM  H'" +right("0000"+ hex(val(DataSpace->Attribute("edc:endaddr"))-1),4)+"'"

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
    fsp_config = kINDEX_IDX+"\Microchip\"+SourceFolder+"_DFP\"+Version+"\xc8\pic\dat\cfgdata\"+ Chip  +".cfgdata"

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
    do while not eof(1)
        Line input #1, DataSource
        DataSource = trim(DataSource)

        if left(ucase(datasource),6)="CWORD:" and instr(datasource,"CONFIG")<>0 then
            ' CWORD:300000:77:FF:CONFIG1L

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
              configsuffix = mid(trim(configname),7)
            end if

            If val(trim(configsuffix))<11 then

              print ""
              print ";----- "+configname+" Options --------------------------------------------------"

            else
              'consume all the configs
              print ""
              print ";----- Dumping Configs 11 and higher --------------------------------------------------"
              print ""

              Do
                  Line input #1, DataSource
                  DataSource = trim(DataSource)
              loop while instr(datasource,"IDLOC")=0  and  not eof(1)

              print ""
              print ";----- End of dumping Configs --------------------------------------------------"
              print ""

            end if
         elseif left(ucase(datasource),9)="CSETTING:" then
            'CSETTING:70:RSTOSC:Reset Oscillator Selection

            split( datasource, ":", 99, ConfigOption() )
            configsetting = "_"+ConfigOption(2)
            configmask = ConfigOption(1)


         elseif left(ucase(datasource),7)="CVALUE:" then

            'CVALUE:0:HFINTOSC_64MHZ:HFINTOSC with HFFRQ = 64 MHz and CDIV = 1:1
            'CVALUE:40:SOSC:Secondary Oscillator

            split( datasource, ":", -1, ConfigChoices() )
            'merge strings
            select case ubound(ConfigChoices)

              case 4
                  ConfigChoices(3) = ConfigChoices(3) +":"+ ConfigChoices(4)
              case 5
                  ConfigChoices(3) = ConfigChoices(3) +":"+ ConfigChoices(4) +":"+ ConfigChoices(5)

            end select

            newmask = ( val("&H"+ConfigChoices(1)) XOR val("&H"+configmask) ) XOR val("&H"+configdefaultvalue)

            newmaskstring = left(hex(newmask)+"00",2)


            if len( configsetting+"_"+ConfigChoices(2)+"_"+configsuffix ) < 21 then
              print left(configsetting+"_"+ConfigChoices(2)+"_"+configsuffix+"                 ",21)+"EQU  H'"+ newmaskstring +"'; "+ConfigChoices(3)
            else
              'print unformatted
              print configsetting+"_"+ConfigChoices(2)+"_"+configsuffix+" EQU  H'"+ newmaskstring +"'; "+ConfigChoices(3)
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
