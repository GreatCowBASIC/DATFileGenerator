echo off
set FileType=inc

for /R incfiles\OrgFiles\ %%F in (p%1.%FileType%) do (
	echo Processing %%F to %%~nF.inc
	gawk -f preprocessIncFile.awk "%%F" > "incfiles\%%~nF.inc"
)

