# DATFileGenerator

This is not an easy process or set of tools.  But, they work.

## History First 

The process of generating a DAT file is process established since 2006.  The output of the process is a microcontrolller specific DAT file.

Throughout the years the output DAT file has remained consistent but the tools and dataset used to create the DAT file has changed.

So, this document will provide insights into each method and which to use in 2025 and beyond.

# Microcontrollers Supported

There a different families of microcontrolllers support, or not supported.

- Microchip PICs
    
    Legacy PIC microcontrolllers, generally identified as NOT having PPS and/or CLC, and, supported by MPASM 
    
    Modern PIC microcontrolllers, generally identified as having PPS and/or CLC, and, NOT supported by MPASM

- ATMEL AVRs

    Legacy microcontrolllers, generally identified as supported by AVRASM

- Microchip AVRs

    AVRDx microcontrolllers, generally identified as supported by AVRASM2 and AVRDx chip family

- Logic Green Technologies - LGTs

    Microcontrolllers that have the prefix of LGT

# High Level Process per type of microcontrolller

The following table shows the high level process and tools to be used.

|Index| Microcontrolller | Process Overview | Tools |
|-|----------|----------|----------|
|1|Legacy PIC|Regen the DAT using GEN_LegacyPIC_ALL.BAT |Uses Getchipdata.exe,GEN_LegacyPIC.awk|
|2|Modern PIC|Create an INC file, then, regen the DAT using GEN_LegacyPIC_ALL.BAT |Uses Getchipdata.exe,GEN_LegacyPIC.awk|
|3|ATMEL AVR|Regen the DAT using avr_getchipdata.exe|avr getchipdata.exe|
|4|AVRDX|Regen the DAT using AVRDX_to_DATv8.exe|AVRDX_to_DATv8.exe|
|5|LGT|No automated tooling|Edit by hand using the Datasheet.  There are 6 microcontrolllers with only memory differences |

The table above shows the high level process to generate the DAT, however, there are many specifics that are required to generate a specific DAT file.


# Low Level Process per type of microcontrolller

## Legacy PIC

## Modern PIC

## ATMEL AVR

## AVRDX

## LGT



