   @echo off
   setlocal enableDelayedExpansion

   :: Define the number of columns (adjust as needed)
   call :defineFor For36InA A 36 ","

   set "row=0"
   for /f usebackq^ delims^=^ eol^= %%A in ("avr chipdata.csv") do (
       set /a row+=1
       call :prepareLine A
       for /f delims^=^ eol^= %%A in ("!ln!") do (
           setlocal disableDelayedExpansion
           %For36InA%
           ( endlocal
           for /l %%C in (1 1 36) do call :decodeAndStoreToken array.!row!.%%C "," !$%%C!
           )
       )
   )

   :: Now you have the CSV data stored in the array variable "array"
   :: You can access individual cells like this: !array.1.2! (row 1, column 2)
	
!array.2.1!

   exit /b

   :defineFor
   setlocal
   for /l %%N in (1 1 %3) do (
       for %%L in (!1!) do (
           set "%%L%%2=%%N"
           shift
       )
   )
   exit /b

   :prepareLine
   set "ln=%%A"
   exit /b

   :decodeAndStoreToken
   setlocal
   set "token=!%~2!"
   set "token=!token:"=""!"
   set "token=!token:^=^^!"
   set "token=!token:!=^!!"
   set "%~1=!token!"
   exit /b


