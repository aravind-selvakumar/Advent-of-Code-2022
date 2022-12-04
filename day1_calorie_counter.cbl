       Identification division.
       PROGRAM-ID. CALORIES-COUNTER.
       environment division.
       input-output section.
       file-control.
      * INPUT CALORIE LIST  
           SELECT File-In ASSIGN TO 'input'
           organization is line sequential.
     
       data division.
       file section.
      * INPUT FILE - 9 BYTES
       FD  File-In.
       01  CALORIES-IN.
           05  CALORIES-IN-STOCK              PIC 9(9).

       WORKING-STORAGE SECTION.
      *VARIABLES
       01 W-Variables.
           05  w-sum-calories                 pic 9(10).
           05  w-elf-counter                  pic 9(4).    
           05  w-max-elf.
               10  w-elf-identifier           pic zzz9.
               10  w-calories-instock         pic zzzzzzzzz9.
      
      *FLAGS 
       01 INPUT-FILE-STATUS                  PIC X(1).
           88 INPUT-FILE-EOF                   VALUE 'Y'.

       Procedure Division.
       0000-Begin Section.
           perform 1000-Initialize 
           perform 2000-Main-Process
           perform 3000-terminate
           stop run.
      ******************************************************************
      * Initialize 
      ******************************************************************
       1000-Initialize section.
           
           initialize  INPUT-FILE-STATUS
                       w-sum-calories
                       w-max-elf
                       w-elf-counter

           open input File-In

           move 1 to w-elf-counter
           .
      ******************************************************************
      * Initialize 
      ******************************************************************
       2000-Main-Process section.
           
           perform 2100-Read-File
      *     
           perform until INPUT-FILE-EOF
               
               if CALORIES-IN-STOCK is > zeroes
                   compute w-sum-calories = 
                           w-sum-calories + CALORIES-IN-STOCK
               else 
                   if w-calories-instock < w-sum-calories
                       move w-elf-counter  to w-elf-identifier
                       move w-sum-calories to w-calories-instock
                   end-if
               
                   initialize w-sum-calories
                   compute w-elf-counter = w-elf-counter + 1
               end-if

               perform 2100-Read-File
                   
           END-PERFORM
           .

      ******************************************************************
      * Initialize 
      ******************************************************************
       2100-Read-File section.
           read File-In into CALORIES-IN 
           at end set INPUT-FILE-EOF to true 
           end-read
           .

      ******************************************************************
      * Initialize 
      ******************************************************************
       3000-terminate section.
           display "Max-calories: " w-calories-instock
           display "Elf-Id:  " w-elf-identifier
           close File-In
           .
