echo off
set FileType=inc

for /R incfiles\OrgFiles\ %%F in (*.%FileType%) do (
	echo Processing %%F to %%~nF.inc
	gawk -f preprocessIncFile.awk "%%F" > "incfiles\%%~nF.inc"
)

