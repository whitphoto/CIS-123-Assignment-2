       IDENTIFICATION DIVISION.
       program-id. Program1.
       
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
           05 in-class         pic x(6).
           05 in-section       pic 99.
           05 in-size          pic 99.
           05 FILLER           PIC XX.
           05 IN-DAY           PIC x.
           05 in-period        pic 9.
           
       FD output-file.
       01 output-rec           pic x(21).
       
       WORKING-STORAGE SECTION.
       
       01 eof pic x value "n".
       
       01 BLANK-LINE           pic X(21) VALUE SPACES.
       
       01 header-1.
           05 filler           pic x(20) value 'AVAILABLE TIME SLOTS'.
           
       01 header-2.
           05 filler           pic x(16) value '   DAY    PERIOD'.
           
       01 free-period.
           05 filler           pic xxxx value spaces.
           05 print-day        pic x.
           05 filler           pic x(7).
           05 print-period     pic x.
           
       01 class-schedule.
           05 period occurs 6 times
               indexed by p-idx.
               10 wday occurs 5 times
               indexed by d-idx.
                   15 t-class  pic x(6).
                   15 t-sec    pic 99.
                   15 t-size   pic 99.
                   
       01 ws-day               pic 9.
       01 ws-period            pic 9.
                   
           
           
           

       procedure division.
       100-MAIN.
           open input input-file
               output output-file.
       
       write output-rec from header-1.
       write output-rec from blank-line.
       write output-rec from header-2.
       
       perform until EOF = "Y"
       read input-file
       at end move "Y" to EOF
       not at end 
       perform 200-CHECK
       
       
       
  
       
       end-read
       end-perform.
       
       perform 300-empty
       
       close input-file
           output-file.
       stop run.
       
       
       
       200-CHECK.
       
       set p-idx to in-period
       if IN-DAY = 'M' set d-idx to 1
       if IN-DAY = 'T' set d-idx to 2
       if IN-DAY = 'W' set d-idx to 3
       if IN-DAY = 'X' set d-idx to 4
       if IN-DAY = 'F' set d-idx to 5
       end-if
       
       if wday(p-idx, d-idx) = spaces 
       move in-class to t-class(p-idx, d-idx)
       move in-section to t-sec(p-idx, d-idx) 
       move in-size to t-size(p-idx, d-idx).
       
       
       300-EMPTY.
       
       perform varying ws-day from 1 by 1 until ws-day is > 5
       set d-idx to ws-day
           perform varying ws-period from 1 by 1 until ws-period > 6
           set p-idx to ws-period
               if wday(p-idx, d-idx) = spaces perform 400-print
              
               end-perform.
               
               
       400-PRINT.
       
       if ws-day = 1 move 'M' to print-day.
       if ws-day = 2 move 'T' to print-day.
       if ws-day = 3 move 'W' to print-day.
       if ws-day = 4 move 'X' to print-day.
       if ws-day = 5 move 'F' to print-day.
       
       move ws-period to print-period 
       write output-rec from free-period.
       
       
       
       
   
           

