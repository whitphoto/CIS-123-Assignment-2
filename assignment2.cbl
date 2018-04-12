       program-id. Program1 as "ClassSchedule".
       
       environment division.
       input-output section.
       file-control.
           Select input-file assign to "infile.txt"
                  organization is line sequential.
           Select output-file assign to "outfile.txt"
                  organization is line sequential.
       
       data division.
       FILE SECTION.
       FD INPUT-FILE.
       01 class-rec.
           05 in-class         pic x(9).
           05 in-section       pic xx.
           05 in-size          pic xx.
           05 FILLER           PIC XX.
           05 IN-DAY           PIC x.
           05 in-period        pic 9.
           
       FD output-file.
       01 output-rec           pix x(21).
       
       working-storage section.
       
       01 eof pic x value "n".
       
       01 BLANK-LINE           pic X(21) VALUE SPACES.
       
       01 header-1.
           05 filler           pic x(20) value 'AVAILABLE TIME SLOTS'.
           
       01 header-2.
           05 filler           pic x(16) value '   DAY     PERIOD'.
           
       01 free-period.
           05 filler           pic xxxx value spaces
           05 print-day        pic x.
           05 filler           pic x(7).
           05 print-period     pic x.
           
       01 class-schedule.
           05 period occurs 6 times
               indexed by p-idx.
               10 wday occurs 5 times
               indexed by d-idx.
                   15 t-class  pic x(9).
                   15 t-sec    pic xx.
                   15 t-size   pic xx.
                   
           
           
           

       procedure division.
       100-MAIN.
           open input input-file
               output output-file
       
       write output-rec from header-1
       write output-rec from blank-line
       write output-rec from header-2
       
       perform varying 
   
           
       end program.
