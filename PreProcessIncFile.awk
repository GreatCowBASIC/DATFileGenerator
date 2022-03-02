Begin{
  pnt=0;
  OutArrayPointer=0
  IncArray[1]="*99";

}

{

    pnt++;
    IncArray[pnt]=$0;
    #printf("%s> %s\n", pnt, IncArray[pnt]);

    #FIX the missing bits in the Q43!!
    if ( index(toupper(FILENAME), "Q43") !=0 ) {
        if ( index(toupper($0), "NVMCON1 BITS") != 0) {
          pnt++
          IncArray[pnt]="NVMCMD0           EQU  H'0000'";
          pnt++;
          IncArray[pnt]="NVMCMD1           EQU  H'0001'";
          pnt++;
          IncArray[pnt]="NVMCMD2           EQU  H'0002'";

        }
    }

}
END {

#cut into three arrays

   bitsstart = 0;
   OutArrayPointer=0;
   for ( nn=1 ; nn <= pnt ; nn++ ){

        #print everything until the bits....
        bitsposition = index(IncArray[nn], "Bits");
        tagposition = index(IncArray[nn], "-------");

        #if found bits section in the INC file
        if ( bitsstart == 0 && bitsposition != 0 && tagposition != 0 ) {

            #OutArray[OutArrayPointer]="**Found Bits";
            #OutArrayPointer++;

            bitsstart = nn;

            #scan thru looking for the end of bits
             bitsend = 0
             for ( nn; nn <= pnt ; nn++ ){
                if ( bitsend == 0 ) {
                    tagposition = index(IncArray[nn], ";==============================");
                    if ( tagposition != 0 ) {
                        bitsend = nn-1 ;
                    }
                }
             }
          }
        }

#create the Arrays

   for ( nn=1 ; nn <bitsstart ; nn++ ){
            startDataArray[nn]=IncArray[nn];
   }

   BitsDataArrayPnt = 1
   for ( nn=bitsstart ; nn <= bitsend ; nn++ ){
            BitsDataArray[BitsDataArrayPnt]=IncArray[nn];
            BitsDataArrayPnt++
   }

   endDataArrayPnt=1
   for ( nn=nn ; nn <= pnt ; nn++ ){
            endDataArray[endDataArrayPnt]=IncArray[nn];
            endDataArrayPnt++
   }


   ProcessBitsArray();


#rebuild the array into the OutArray

   OutArrayPointer = 1
   for ( nn=1 ; nn <bitsstart ; nn++ ){

            OutArray[OutArrayPointer]=startDataArray[nn]
            OutArrayPointer++;
   }

   BitsDataArrayPnt = 1
   for ( nn=bitsstart ; nn <= bitsend ; nn++ ){
            OutArray[OutArrayPointer]=TargetOutArray[BitsDataArrayPnt]
            OutArrayPointer++;
            BitsDataArrayPnt++

   }

   endDataArrayPnt = 1
   for ( nn=nn ; nn <= pnt ; nn++ ){
            OutArray[OutArrayPointer]=endDataArray[endDataArrayPnt]
            OutArrayPointer++;
            endDataArrayPnt++

   }


   for ( nn=1 ; nn <= pnt ; nn++ ){

#    if ( length(trim(OutArray[nn])) != 0 ) {
        print OutArray[nn];
#      }
   }


}


    function ltrim(s) { sub(/^[ \t\r\n]+/, "", s); return s }
    function rtrim(s) { sub(/[ \t\r\n]+$/, "", s); return s }
    function trim(s)  { return rtrim(ltrim(s)); }


function ProcessBitsArray() {
    #using the source array output to another array the target out array

#   BitsDataArrayPnt = 1
#   OutArrayPointer = 1
#   for ( nn=bitsstart ; nn <= bitsend ; nn++ ){
#            TargetOutArray[OutArrayPointer]=BitsDataArray[BitsDataArrayPnt]
#            OutArrayPointer++;
#            BitsDataArrayPnt++
#
#   }

    #find the Bits

      OutArrayPointer = 1

      for ( nn=1 ; nn <= ( bitsend - bitsstart ) ; nn++ ){
        #print everything until the bits....
        bitsposition = index(BitsDataArray[nn], "Bits");
        tagposition = index(BitsDataArray[nn], "-------");
        if ( bitsposition != 0 && tagposition != 0 ) {
            #found bits
            $0 = BitsDataArray[nn];
            #does this have a number in the string?
            #$2

            foundaregisterwithanumber = 0;
            for ( findintegervalue = 2;findintegervalue <= 10;findintegervalue++ ) {
                for ( indexthruregistername=1;indexthruregistername<=length($2);indexthruregistername++) {
                    if ( substr($2,indexthruregistername,1) == findintegervalue ) {
                        #found a register!!
                        #printf("reg = %s.....%s\n",$2, substr($2,indexthruregistername,1));
                        foundaregisterwithanumber = 1


                        #set what we are looking for
                        lf= substr($2,1,indexthruregistername-1);
                        rf= substr($2,indexthruregistername+1);

                        findthistregister = lf"1"rf" Bits";
                        #print "Finding this register "findthistregister;

                    }
                    if ( foundaregisterwithanumber == 1 ){
                        break
                    }
                }
                if ( foundaregisterwithanumber == 1 ){
                    break
                }
            }
            #if we have found a register then deal with the search... else


            if ( foundaregisterwithanumber == 1 ){
#print "Finding this register "findthistregister " from " $2
                  #find the the target register
                  #output to array
                  #delete
                  output the existing Bits


                  for ( findlowestregisterPnt=1; findlowestregisterPnt <= ( bitsend - bitsstart ); findlowestregisterPnt++){
#print BitsDataArray[findlowestregisterPnt]

                      #find the string
                      if ( index(BitsDataArray[findlowestregisterPnt]" Bits",findthistregister) != 0 ) {
                            #we must have found the entry extract

                            do {
                                 #print BitsDataArray[findlowestregisterPnt]
                                 #prevent empty elements from being output, this happens as the array become sparse

                                 #test for repeated empty elements
                                 if ( BitsDataArray[findlowestregisterPnt] != "DELETED ELEMENT" ) {
                                       TargetOutArray[OutArrayPointer]=BitsDataArray[findlowestregisterPnt]
                                       OutArrayPointer++;

                                       BitsDataArray[findlowestregisterPnt]="DELETED ELEMENT";
                                 }
                                 findlowestregisterPnt++
                            } while  ( index(toupper(BitsDataArray[findlowestregisterPnt]),";-") == 0 );#end of the new bits
                            #findlowestregisterPnt = bitsend;
                      }
                  }












            }
            else {


            }


        }

      }#end of nn loop
      #TargetOutArray[OutArrayPointer]=";Process remaining "
      #OutArrayPointer++;
      for ( nn=1 ; nn <= ( bitsend - bitsstart ) ; nn++ ){
          if ( BitsDataArray[nn] != "DELETED ELEMENT" ) {
                   TargetOutArray[OutArrayPointer]=BitsDataArray[nn]
                   OutArrayPointer++;
                  # BitsDataArray[findlowestregisterPnt]="DELETED ELEMENT";
          }
      }



}
