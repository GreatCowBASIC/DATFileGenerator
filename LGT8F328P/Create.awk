BEGIN {
  ccount = 0
  out[0]="LGT8F328P-0.dat"
  out[1]="LGT8F328P-1.dat"
  out[2]="LGT8F328P-2.dat"
  out[3]="LGT8F328P-4.dat"
  out[4]="LGT8F328P-8.dat"
  out[5]="LGT8F328P.dat"

  fout[0]="LGT8F328P-0"
  fout[1]="LGT8F328P-1"
  fout[2]="LGT8F328P-2"
  fout[3]="LGT8F328P-4"
  fout[4]="LGT8F328P-8"
  fout[5]="LGT8F328P"


  Prog[0]="16384"
  Prog[1]="15872"
  Prog[2]="15360"
  Prog[3]="14336"
  Prog[4]="12288"
  Prog[5]="15872"

  EEPROM[0]="0"
  EEPROM[1]="1024"
  EEPROM[2]="2048"
  EEPROM[3]="4096"
  EEPROM[4]="8192"
  EEPROM[5]="1024"




}
{
    lines [ccount ] = $0
    #print lines [ccount ]
    ccount ++
}
END {

  for ( entity=0; entity < 6; entity++ ) {
      print "'GCBASIC/GCGB Chip Data File" > out[entity]

      for ( outcount = 1; outcount <= ccount; outcount++ ) {
           output = 0
           if ( index( lines [ outcount ], "'Chip:" ) > 0 ) {
                output = 1
                printf ("'Chip: %s\n", fout[entity] ) >> out[entity]
              }

           if ( index( lines [ outcount ], "Prog=" ) > 0 ) {
                output = 1
                printf ("Prog=%s\n", Prog[entity]) >> out[entity]
              }


           if ( index( lines [ outcount ], "EEPROM=" ) > 0 ) {
                output = 1
                printf ("EEPROM=%s\n", EEPROM[entity]) >> out[entity]
              }


           if ( output == 0 )
              print lines [ outcount ] >> out[entity]
      }
  }
}
