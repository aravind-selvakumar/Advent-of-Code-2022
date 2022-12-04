       Identification division.
       PROGRAM-ID. CALORIES-COUNTER.
       environment division.
       input-output section.
       file-control.
      * INPUT CALORIE LIST  
           SELECT File-In ASSIGN TO '..\data\input'
           organization is line sequential.
     
       data division.
       file section.
      * INPUT FILE - 9 BYTES
       FD  File-In.
       01  CALORIES-IN.
           05  CALORIES-IN-STOCK              PIC x(5).

       WORKING-STORAGE SECTION.
      *VARIABLES
       01 W-Variables.
           05  w-sum-calories                 pic 9(7).
           05  w-elf-counter                  pic 9(4).    
           05  w-max-elf occurs 500 times.
               10  w-calories-instock         pic 9(7).
           05  w-max-sum                      pic 9(10).
           05  w-subscript                    pic 9(4).
      
      *FLAGS 
       01 INPUT-FILE-STATUS                   pic x(1).
           88 INPUT-FILE-EOF                   value 'Y'.

       Procedure Division.
       0000-Begin Section.
           perform 1000-Initialize 
           perform 2000-Main-Process
           perform 3000-print-summary
           stop run.
      ******************************************************************
      * Initialize 
      ******************************************************************
       1000-Initialize section.
           
           initialize  INPUT-FILE-STATUS
                       w-sum-calories
                       w-elf-counter

           open input File-In

           move 1 to w-elf-counter
           move 1 to w-subscript
           .
      ******************************************************************
      * Initialize 
      ******************************************************************
       2000-Main-Process section.
           
           perform 2100-Read-File
      *    
           perform 2200-Load-Table
           
           sort w-max-elf DESCENDING w-calories-instock 
           .

      ******************************************************************
      * Read File 
      ******************************************************************
       2100-Read-File section.
           read File-In into CALORIES-IN 
           at end set INPUT-FILE-EOF to true 
           end-read
           .
      ******************************************************************
      * Load the key and value to internl table
      ******************************************************************
       2200-Load-Table section.
           
           perform until INPUT-FILE-EOF
               
               if CALORIES-IN-STOCK is > zeroes
                   compute w-sum-calories = w-sum-calories +
                            function numval(CALORIES-IN-STOCK)
               else 
      
                    move w-sum-calories 
                      to w-calories-instock(w-subscript) 

                    compute w-subscript = w-subscript + 1
                    initialize w-sum-calories
                      
               end-if
      *         compute w-elf-counter = w-elf-counter + 1
               perform 2100-Read-File
                   
           END-PERFORM
           .

      ******************************************************************
      * Calculate the top three elves with calories in stock 
      ******************************************************************
       2200-compute-top-three section.

           initialize w-subscript
           move 1 to w-subscript
           perform 3 times 
               if w-calories-instock(w-subscript) < w-sum-calories
      *              
                    move w-sum-calories 
                      to w-calories-instock(w-subscript)
               end-if
               compute w-subscript = w-subscript + 1
           end-perform
               
           initialize w-sum-calories
                      w-subscript
           .     

      ******************************************************************
      * Summary  
      ******************************************************************
       3000-print-summary section.

           move 1 to w-subscript
           perform 3 times 
               display "Elf " w-subscript 
               display "Max-calories: " w-calories-instock(w-subscript) 
               display "-----------------"
               
               compute w-max-sum = 
                       w-max-sum + w-calories-instock(w-subscript)
               compute w-subscript = w-subscript + 1
           end-perform
           
           display "w-max-sum : " w-max-sum
           close File-In
           .
