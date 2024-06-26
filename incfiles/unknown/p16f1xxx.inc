        LIST
; p16f1xxx.inc  Standard Header File, Microchip Technology, Inc.
        NOLIST

; This header file defines the list of processor specific header files.  It is
; used to include the appropriate header file.

; Note that the processor must be selected before this file is included.  The
; processor may be selected the following ways:

; Using the PIC16F1937 as an example
;       1. Command line switch:
;               C:\ MPASM MYFILE.ASM /PIC16F1937
;       2. LIST directive in the source file
;               LIST   P=PIC16F1937
;       3. Processor Type entry in the MPASM full-screen interface
;       4. Setting the processor in the MPLAB Project Dialog

;==========================================================================
;
;       Verify Processor
;
;==========================================================================

  IFDEF __16F1xxx
     INCLUDE <p16f1937.inc>
  ENDIF
  IFDEF __12F1501
     INCLUDE <p12f1501.inc>
  ENDIF
  IFDEF __12F1612
     INCLUDE <p12f1612.inc>
  ENDIF
  IFDEF __12F1822
     INCLUDE <p12f1822.inc>
  ENDIF
  IFDEF __12F1840
     INCLUDE <p12f1840.inc>
  ENDIF
  IFDEF __12LF1501
     INCLUDE <p12lf1501.inc>
  ENDIF
  IFDEF __12LF1612
     INCLUDE <p12lf1612.inc>
  ENDIF
  IFDEF __12LF1822
     INCLUDE <p12lf1822.inc>
  ENDIF
  IFDEF __12LF1840
     INCLUDE <p12lf1840.inc>
  ENDIF
  IFDEF __12LF1840T48A
     INCLUDE <p12lf1840t48a.inc>
  ENDIF
  IFDEF __16F1507
     INCLUDE <p16f1507.inc>
  ENDIF
  IFDEF __16F1516
     INCLUDE <p16f1516.inc>
  ENDIF
  IFDEF __16F1454
     INCLUDE <p16f1454.inc>
  ENDIF
  IFDEF __16F1455
     INCLUDE <p16f1455.inc>
  ENDIF
  IFDEF __16F1458
     INCLUDE <p16f1458.inc>
  ENDIF
  IFDEF __16F1459
     INCLUDE <p16f1459.inc>
  ENDIF
  IFDEF __16F1503
     INCLUDE <p16f1503.inc>
  ENDIF
  IFDEF __16F1507
     INCLUDE <p16f1507.inc>
  ENDIF
  IFDEF __16F1508
     INCLUDE <p16f1508.inc>
  ENDIF
  IFDEF __16F1509
     INCLUDE <p16f1509.inc>
  ENDIF
  IFDEF __16F1512
     INCLUDE <p16f1512.inc>
  ENDIF
  IFDEF __16F1513
     INCLUDE <p16f1513.inc>
  ENDIF
  IFDEF __16F1516
     INCLUDE <p16f1516.inc>
  ENDIF
  IFDEF __16F1517
     INCLUDE <p16f1517.inc>
  ENDIF
  IFDEF __16F1518
     INCLUDE <p16f1518.inc>
  ENDIF
  IFDEF __16F1519
     INCLUDE <p16f1519.inc>
  ENDIF
  IFDEF __16F1526
     INCLUDE <p16f1526.inc>
  ENDIF
  IFDEF __16F1527
     INCLUDE <p16f1527.inc>
  ENDIF
  IFDEF __16F1613
     INCLUDE <p16f1613.inc>
  ENDIF
  IFDEF __16F1717
     INCLUDE <p16f1717.inc>
  ENDIF
  IFDEF __16F1718
     INCLUDE <p16f1718.inc>
  ENDIF
  IFDEF __16F1719
     INCLUDE <p16f1719.inc>
  ENDIF
  IFDEF __16F1703
     INCLUDE <p16f1703.inc>
  ENDIF
  IFDEF __16F1704
     INCLUDE <p16f1704.inc>
  ENDIF
  IFDEF __16F1705
     INCLUDE <p16f1705.inc>
  ENDIF
  IFDEF __16F1707
     INCLUDE <p16f1707.inc>
  ENDIF
  IFDEF __16F1708
     INCLUDE <p16f1708.inc>
  ENDIF
  IFDEF __16F1709
     INCLUDE <p16f1709.inc>
  ENDIF
  IFDEF __16F1713
     INCLUDE <p16f1713.inc>
  ENDIF
  IFDEF __16F1716
     INCLUDE <p16f1716.inc>
  ENDIF
  IFDEF __16F1782
     INCLUDE <p16f1782.inc>
  ENDIF
  IFDEF __16F1784
     INCLUDE <p16f1784.inc>
  ENDIF
  IFDEF __16F1786
     INCLUDE <p16f1786.inc>
  ENDIF
  IFDEF __16F1787
     INCLUDE <p16f1787.inc>
  ENDIF
  IFDEF __16F1783
     INCLUDE <p16f1783.inc>
  ENDIF
  IFDEF __16F1788
     INCLUDE <p16f1788.inc>
  ENDIF
  IFDEF __16F1789
     INCLUDE <p16f1789.inc>
  ENDIF  
  IFDEF __16F1823
     INCLUDE <p16f1823.inc>
  ENDIF
  IFDEF __16F1824
     INCLUDE <p16f1824.inc>
  ENDIF
  IFDEF __16F1825
     INCLUDE <p16f1825.inc>
  ENDIF
  IFDEF __16F1826
     INCLUDE <p16f1826.inc>
  ENDIF
  IFDEF __16F1827
     INCLUDE <p16f1827.inc>
  ENDIF
  IFDEF __16F1828
     INCLUDE <p16f1828.inc>
  ENDIF
  IFDEF __16F1829
     INCLUDE <p16f1829.inc>
  ENDIF
  IFDEF __16F1829LIN
     INCLUDE <p16f1829lin.inc>
  ENDIF
  IFDEF __16F1847
     INCLUDE <p16f1847.inc>
  ENDIF
  IFDEF __16F1933
     INCLUDE <p16f1933.inc>
  ENDIF
  IFDEF __16F1934
     INCLUDE <p16f1934.inc>
  ENDIF
  IFDEF __16F1936
     INCLUDE <p16f1936.inc>
  ENDIF
  IFDEF __16F1937
     INCLUDE <p16f1937.inc>
  ENDIF
  IFDEF __16F1938
     INCLUDE <p16f1938.inc>
  ENDIF
  IFDEF __16F1939
     INCLUDE <p16f1939.inc>
  ENDIF
  IFDEF __16F1946
     INCLUDE <p16f1946.inc>
  ENDIF
  IFDEF __16F1947
     INCLUDE <p16f1947.inc>
  ENDIF
  IFDEF __16LF1454
    INCLUDE <p16lf1454.inc>
  ENDIF
  IFDEF __16LF1455
     INCLUDE <p16lf1455.inc>
  ENDIF
  IFDEF __16LF1458
     INCLUDE <p16lf1458.inc>
  ENDIF
  IFDEF __16LF1459
     INCLUDE <p16lf1459.inc>
  ENDIF
  IFDEF __16LF1503
     INCLUDE <p16lf1503.inc>
  ENDIF
  IFDEF __16LF1507
     INCLUDE <p16lf1507.inc>
  ENDIF
  IFDEF __16LF1508
     INCLUDE <p16lf1508.inc>
  ENDIF
  IFDEF __16LF1509
     INCLUDE <p16lf1509.inc>
  ENDIF
  IFDEF __16F1512
     INCLUDE <p16f1512.inc>
  ENDIF
  IFDEF __16F1513
     INCLUDE <p16f1513.inc>
  ENDIF
  IFDEF __16LF1516
     INCLUDE <p16lf1516.inc>
  ENDIF
  IFDEF __16LF1517
     INCLUDE <p16lf1517.inc>
  ENDIF
  IFDEF __16LF1518
     INCLUDE <p16lf1518.inc>
  ENDIF
  IFDEF __16LF1519
     INCLUDE <p16lf1519.inc>
  ENDIF
  IFDEF __16LF1526
     INCLUDE <p16lf1526.inc>
  ENDIF
  IFDEF __16LF1527
     INCLUDE <p16lf1527.inc>
  ENDIF
  IFDEF __16LF1613
     INCLUDE <p16lf1613.inc>
  ENDIF
  IFDEF __16LF1717
     INCLUDE <p16lf1717.inc>
  ENDIF
  IFDEF __16LF1718
     INCLUDE <p16lf1718.inc>
  ENDIF
  IFDEF __16LF1719
     INCLUDE <p16lf1719.inc>
  ENDIF
  IFDEF __16LF1703
     INCLUDE <p16lf1703.inc>
  ENDIF
  IFDEF __16LF1704
     INCLUDE <p16lf1704.inc>
  ENDIF
  IFDEF __16LF1705
     INCLUDE <p16lf1705.inc>
  ENDIF
  IFDEF __16LF1707
     INCLUDE <p16lf1707.inc>
  ENDIF
  IFDEF __16LF1708
     INCLUDE <p16lf1708.inc>
  ENDIF
  IFDEF __16LF1709
     INCLUDE <p16lf1709.inc>
  ENDIF
  IFDEF __16LF1713
     INCLUDE <p16lf1713.inc>
  ENDIF
  IFDEF __16LF1716
     INCLUDE <p16lf1716.inc>
  ENDIF
  IFDEF __16LF1782
     INCLUDE <p16lf1782.inc>
  ENDIF
  IFDEF __16LF1784
     INCLUDE <p16lf1784.inc>
  ENDIF
  IFDEF __16LF1786
     INCLUDE <p16lf1786.inc>
  ENDIF
  IFDEF __16LF1787
     INCLUDE <p16lf1787.inc>
  ENDIF
  IFDEF __16LF1782
     INCLUDE <p16lf1782.inc>
  ENDIF
  IFDEF __16LF1783
     INCLUDE <p16lf1783.inc>
  ENDIF
  IFDEF __16LF1788
     INCLUDE <p16lf1788.inc>
  ENDIF 
  IFDEF __16LF1789
     INCLUDE <p16lf1789.inc>
  ENDIF    
  IFDEF __16LF1823
     INCLUDE <p16lf1823.inc>
  ENDIF
  IFDEF __16LF1824
     INCLUDE <p16lf1824.inc>
  ENDIF
  IFDEF __16LF1825
     INCLUDE <p16lf1825.inc>
  ENDIF
  IFDEF __16LF1826
     INCLUDE <p16lf1826.inc>
  ENDIF
  IFDEF __16LF1827
     INCLUDE <p16lf1827.inc>
  ENDIF
  IFDEF __16LF1828
     INCLUDE <p16lf1828.inc>
  ENDIF
  IFDEF __16LF1829
     INCLUDE <p16lf1829.inc>
  ENDIF
  IFDEF __16LF1847
     INCLUDE <p16lf1847.inc>
  ENDIF
  IFDEF __16LF1902
     INCLUDE <p16lf1902.inc>
  ENDIF
  IFDEF __16LF1903
     INCLUDE <p16lf1903.inc>
  ENDIF
  IFDEF __16LF1904
     INCLUDE <p16lf1904.inc>
  ENDIF
  IFDEF __16LF1906
     INCLUDE <p16lf1906.inc>
  ENDIF
  IFDEF __16LF1907
     INCLUDE <p16lf1907.inc>
  ENDIF
  IFDEF __16LF1933
     INCLUDE <p16lf1933.inc>
  ENDIF
  IFDEF __16LF1934
     INCLUDE <p16lf1934.inc>
  ENDIF
  IFDEF __16LF1936
     INCLUDE <p16lf1936.inc>
  ENDIF
  IFDEF __16LF1937
     INCLUDE <p16lf1937.inc>
  ENDIF
  IFDEF __16LF1938
     INCLUDE <p16lf1938.inc>
  ENDIF
  IFDEF __16LF1939
     INCLUDE <p16lf1939.inc>
  ENDIF
  IFDEF __16LF1946
     INCLUDE <p16lf1946.inc>
  ENDIF
  IFDEF __16LF1947
     INCLUDE <p16lf1947.inc>
  ENDIF
    IFDEF __16LF1554
     INCLUDE <p16lf1554.inc>
  ENDIF
    IFDEF __16LF1559
     INCLUDE <p16lf1559.inc>
  ENDIF
    IFDEF __16F18313
     INCLUDE <p16f18313.inc>
  ENDIF
    IFDEF __16F18323
     INCLUDE <p16f18323.inc>
  ENDIF
    IFDEF __16LF18313
     INCLUDE <p16lf18313.inc>
  ENDIF
    IFDEF __16LF18323
     INCLUDE <p16lf18323.inc>
  ENDIF

