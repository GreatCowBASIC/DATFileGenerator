

'This examines the INDEX_IDX file in the MPLAB-X directory and returns the version etc for a specific Chip


#include once "ext/xml/dom.bi"
#DEFINE kINDEX_IDX  ""    'UserProfile+kINDEX.IDX
#DEFINE kUniqueBits ""'"TMR10,TMR11,TMR12,TMR13,TMR14,TMR15,TMR16,TMR17,TMR18,TMR19,ON,OUT,GO,POL,C1OUT,C2OUT,SYNC,INTN,INTP,CKPS0,CKPS1,CKPS2,CS0,CS1,CS2"
#DEFINE kKillRegister ""  '"W,DATA"
#DEFINE kKillBits ""' "DATA"
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

Declare Sub PopulateSourceFilesLocation
Declare Function FindChip ( chiptofind as string ) as string
Declare SUB Replace (DataVar As String, Find As String, Rep As String)
Declare Sub Split(Text As String, Delim As String = " ", Count As Long = -1, Ret() As String)
Declare Function RegisterString( chipstringtofind as string ) as string
Declare Sub PopulateRAMDefinitions ( SourceFolder as string, Version as string, Chip as string )
Declare Sub PopulateConfig ( SourceFolder as string, Version as string, Chip as string )

  UserProfile = ENVIRON("Userprofile")
  UserProfile = "C:\Users\admin\.mchp_packs"
  UserProfile = "C:\Program Files\Microchip\MPLABX\v6.05\packs"
  'read the kINDEX_IDX to populate SourceFileArray()
  PopulateSourceFilesLocation

  Dim CD as integer = 1
  If COMMAND(CD) = "?" then
    Print "No help available"
    End
  End if

  If Ucase(COMMAND(CD)) = "DEBUG" then
    ShowDebug = -1
    CD = CD + 1
    print ";Debug:  Second command line parameter is: "+trim(COMMAND(CD))
  End if


  If COMMAND(CD) = "" then
    'Display the contents of the kINDEX_IDX to the console... essentially shows SourceFileArray()
    Dim loopcounter as Integer
    for loopcounter = 0 to SourceFileArrayPointer - 1
      with SourceFileArray( loopcounter )
          print left(.Pack+"                              ",45)+ left(.ChipName+"                              ",25) +  left(.Version+"                              ",15)+.Core
      end with
    next

  Else
    'Process the commnad line

    Dim ParamUpper as string = Ucase(COMMAND(CD))
    Dim targetchip as string
    Dim As String chip, chipdetails, chiphsource, chipincsource
    Dim chipparameters() as string
    Dim fsp_ini as string
    Dim fsp_ini_h as string
    Dim folder_DFP as string

    targetchip = ParamUpper
    If left(targetchip,2) <> "AT" then
      targetchip = "AT"+targetchip
    End if

    if ShowDebug = -1 then print ";Debug: kINDEX_IDX directory location is: "+UserProfile+kINDEX_IDX
    if ShowDebug = -1 then print ";Debug: targetchip is: "+targetchip

    chipdetails = FindChip ( targetchip )

    if chipdetails = "NOCHIP" then
        print "Not a valid chip"
        end
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


    split( chipdetails, ",",-1,chipparameters() )

    fsp_ini = UserProfile+kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\avrasm\inc\"+ chipincsource  +".inc"
    fsp_ini_h = UserProfile+kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\xc8\avr\include\avr\"+ chiphsource  +".h"
    ConfigFilename = "; " + UserProfile + kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\xc8\avr\dat\cfgdata\"+ Chip  +".cfgdata"

 'C:\Users\admin\.mchp_packs\Microchip\ATmega_DFP\2.3.126\avrasm\inc\m328PBdef.inc

    Print ";        .DAT sections"
    Print ";=========================================================================="
    Print ";"
    Print ";  Built by Great Cow BASIC convertor"
    Print ";  XC8 processor include for the chip shown below"
    Print ";"
    Print "; " + UserProfile + kINDEX_IDX+"\index.idx"
    Print "; " + fsp_ini
    Print "; " + fsp_ini_h
    Print "; " + UserProfile+kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\xc8\avr\include\avr\"+ chiphsource  +".inc"
    print "; " + UserProfile + kINDEX_IDX+"\Microchip\"+chipparameters(3)+"_DFP\"+chipparameters(1)+"\edc\AT"+ Chip  +".PIC"
    print "; " + ConfigFileName
    print ";"
    Print ";=========================================================================="


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
    Dim CurrentBank as Integer
    Dim ThisBank as Integer

    if instr(kKillRegister,",") <> 0 then
      Split ( kKillRegister, ",", -1, RegistersToBeIgnored() )
    else
      redim RegistersToBeIgnored(1)
      RegistersToBeIgnored(0) = trim(kKillRegister)
    end if

    CurrentBank = -1

    'open Ini for registers
    'SFR=STATUS_CSHAD,3880,8
    'STATUS_CSHAD     EQU  H'3880'

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

    Print "[Registers]"
    do while not eof(1)
        Line input #1, DataSource

        If DataSource <> "" and instr( ucase(datasource), ".EQU" ) > 0 Then

            'check to see if entry should be ignored.
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
                    print .RegisterName+","+str(val(.RegisterAddresss))
                end with
            end if
        end if

        if instr( ucase(DataSource),"ALL MODULE BASE ADRESSES") > 0 Then
          Exit Do
        End if
    loop
    close   ' close all open files

    Dim readNextLine as Integer = -1
    Dim RegisterBits() as string
    Dim BitsToBeIgnored() as string
    Dim CurrentRegister as string = ""
    Dim NewRegister as string = ""
    Dim EqualPosition as Integer
    Dim SemiColonPosition as Integer



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

                              Print trim(SFRBits(SFRBitsArrayPointer).RegisterBitName)+","+NewRegister+","+SFRBits(SFRBitsArrayPointer).RegisterBitAddress + chr(9)+ chr(9)+ chr(9) + "' " + mid( Orgline, Instr( OrgLine, "; ")+1)

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


        Print ""
        Print "[Interrupts]"

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
                  Case "CRCSCAN_NMI":
                    print chr(9);"NMIfromCRC:CRCSCAN_NMI,"+ str(val(Vector)) + ",,"
                
                  Case "BOD_VLM":
                    print chr(9);"BOD_VLM:BOD_VLM,"+ str(val(Vector)) + ",,"
                            
                Case Else
                  print chr(9)+chr(9)+Trim(Left( DataSource, Instr(DataSource, "=")-1)) + ":" + Trim(Left( DataSource, Instr(DataSource, "=")-1)) + "," +  str(val(Vector)) + ",,"

                End Select
              End If
            Else  
              if left(DataSource,1) = ";" Then Print DataSource
            End If


        Loop

    end if


fin1:
    Close
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
