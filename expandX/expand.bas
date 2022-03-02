Dim As String ChipList(50), InFile, OutFile, DataSource, TrimData, Condition
Dim As String ChipName, DefList(200)
'Dim As Integer ChipCount, FindExisting, CurrentChip, PD, AllowWrite

'Get filename
If COMMAND(1) = "" Then
    Print "expand filename"
    Getkey
    End
ElseIf Dir(COMMAND(1)) = "" Then
    Print "File not found"
    GetKey
    End
Else
    InFile = COMMAND(1)
End If

'Read file, get chips
Open InFile For Input As #1
Do While Not EOF(1)
    Line Input #1, DataSource
    DataSource = Trim(Trim(DataSource), Chr(9))
    If Left(DataSource, 6) = "IFDEF " And Instr(DataSource, "__1") <> 0 Then
        ChipName = LCase(Trim(Mid(DataSource, Instr(DataSource, "__") + 2)))
        For FindExisting = 1 to ChipCount
            If ChipName = ChipList(FindExisting) Then Goto ChipExists
        Next
        ChipCount += 1
        ChipList(ChipCount) = ChipName
        ChipExists:
    End If
Loop
Close

'Generate file for each chip
For CurrentChip = 1 To ChipCount
    ChipName = ChipList(CurrentChip)
    AllowWrite = -1
    Print ChipName
    
    DefCount = 0
    For PD = 1 to 200
        DefList(PD) = ""
    Next
    
    Open InFile For Input As #1
    Open "p" + ChipName + ".inc" For Output As #2
    Do While Not Eof(1)
        Line Input #1, DataSource
        TrimData = Trim(Trim(DataSource), Chr(9))
        
        If AllowWrite = 0 Then
            If Left(TrimData, 5) = "ENDIF" Then
                AllowWrite = -1
            End If
        Else
            If Left(TrimData, 5) = "ENDIF" Then
                AllowWrite = -1
            ElseIf Left(TrimData, 6) = "IFDEF " Then
                Condition = LCase(Mid(TrimData, Instr(TrimData, " ") + 1))
                AllowWrite = 0
                If Condition = "__" + ChipName Then
                    AllowWrite = -1
                Else
                    For FindDef = 1 to DefCount
                        If DefList(FindDef) = Condition Then
                            AllowWrite = -1
                            Exit For
                        End If
                    Next
                End If
                
            ElseIf Left(TrimData, 7) = "#define" Then
                Condition = LCase(Mid(TrimData, Instr(TrimData, " ") + 1))
                For FindExisting = 1 to DefCount
                    If Condition = DefList(FindExisting) Then Goto AlreadyDefined
                Next
                DefCount += 1
                DefList(DefCount) = Condition
                AlreadyDefined:
                
            ElseIf Left(TrimData, 10) = "#undefine " Then
                Condition = LCase(Mid(TrimData, Instr(TrimData, " ") + 1))
                For FindExisting = 1 to DefCount
                    If Condition = DefList(FindExisting) Then
                        DefList(FindExisting) = DefList(DefCount)
                        DefCount -= 1
                    End If
                Next
            Else
                Print #2, DataSource
            End If
            
        End If
        
    Loop
    Close
Next

Print "Done, press any key to exit"
GetKey