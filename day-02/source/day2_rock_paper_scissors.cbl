       Identification division.
       PROGRAM-ID. Game-Scorer.
       environment division.
       input-output section.
       file-control.
      * INPUT CALORIE LIST  
           SELECT File-In ASSIGN TO '..\data\input'
           organization is line sequential.
     
       data division.
       file section.
      * INPUT FILE - 3 BYTES
       FD  File-In.
       01  Game-In.
           05  Team-A              pic x(1).
           05  filler              pic x(1).
           05  Team-B              pic x(1).

       WORKING-STORAGE SECTION.
      *VARIABLES
       01 W-Variables.
           05  w-game-rounds occurs 3000 times.
               10  team-a-choice         pic x(1).
               10  team-b-choice         pic x(1).
           05  w-sub                     pic 9(4).
           05  total-score               pic 9(6).

      * Constants 
       77 c-rock                         pic 9(1) value 1.                        
       77 c-paper                        pic 9(1) value 2.
       77 c-scissors                     pic 9(1) value 3.
       77 c-win                          pic 9(1) value 6.
       77 c-draw                         pic 9(1) value 3.
       77 c-lose                         pic 9(1) value 0.

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
           initialize total-score
                      w-sub
           open input File-In

           .
      ******************************************************************
      * Initialize 
      ******************************************************************
       2000-Main-Process section.
           
           perform 2100-Read-File
      *    
           perform 2200-score-game
           
           .

      ******************************************************************
      * Read File 
      ******************************************************************
       2100-Read-File section.
           Read File-In into Game-In
           at end set INPUT-FILE-EOF to true 
           end-read
           .
      ******************************************************************
      * Score the game
      ******************************************************************
       2200-score-game section.
           perform until INPUT-FILE-EOF
               perform 2300-calc-points     
               perform 2100-Read-File
                   
           end-perform     
           .
      ******************************************************************
      * Score the game
      ******************************************************************
       2300-calc-points  section.   
           Evaluate true 
      * Team B winning combinations     
               when Team-B = 'X' 
                and Team-A = 'C'
                    compute total-score = total-score +
                                          c-rock +
                                          c-win     
               when Team-B = 'Y' 
                and Team-A = 'A'
                    compute total-score = total-score +
                                          c-paper +
                                          c-win    
               when Team-B = 'Z' 
                and Team-A = 'B'
                    compute total-score = total-score +
                                          c-scissors +
                                          c-win    
          
      * Team B draw combinations
               when Team-B = 'X' 
                and Team-A = 'A'
                    compute total-score = total-score +
                                          c-rock +
                                          c-draw 
               when Team-B = 'Y' 
                and Team-A = 'B'
                    compute total-score = total-score +
                                          c-paper +
                                          c-draw
               when Team-B = 'Z' 
                and Team-A = 'C'
                    compute total-score = total-score +
                                          c-scissors +
                                          c-draw
          
      * Team B losing combinations
               when Team-B = 'X' 
                and Team-A = 'B'
                    compute total-score = total-score +
                                          c-rock +
                                          c-lose 
               when Team-B = 'Y' 
                and Team-A = 'C'
                    compute total-score = total-score +
                                          c-paper +
                                          c-lose
               when Team-B = 'Z' 
                and Team-A = 'A'
                    compute total-score = total-score +
                                          c-scissors +
                                          c-lose

           end-evaluate 
           .

      ******************************************************************
      * Summary  
      ******************************************************************
       3000-print-summary section.

           display "total-score : " total-score
           close File-In
           .     