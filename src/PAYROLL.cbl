      *IDENTIFICATION DIVISION.
      * PROGRAM-ID. PAYROLL-MAIN.
      *DATA DIVISION.
      * WORKING-STORAGE SECTION.
      * 01  MENU-CHOICE      PIC 9 VALUE 9.  *> Initialize to non-zero value
      * 01  DUMMY            PIC X.          *> Buffer cleanup
      *
      * PROCEDURE DIVISION.
      *     PERFORM UNTIL MENU-CHOICE = 0
      *     DISPLAY " "
      *     DISPLAY "--------------------------------------------------"
      * 
      *     DISPLAY "PAYROLL SYSTEM MAIN MENU"
      *  
      *     DISPLAY "--------------------------------------------------"
      *    DISPLAY "1. Employee Menu"
      *     
      *     DISPLAY "2. "
      *     DISPLAY "0. Exit"
      *     DISPLAY "Enter your choice: " WITH NO ADVANCING
      *    ACCEPT MENU-CHOICE 
      *    ACCEPT DUMMY FROM ESCAPE KEY *> Clear input buffer
      *    DISPLAY "------------------------------------"
      *     EVALUATE MENU-CHOICE
      *      WHEN 1 CALL "GEN-PAYSLIP-MENU"
      *     
      *    
      *    WHEN 2 CALL "LEAVE-CALC"
      *     WHEN 0 CONTINUE
      *     WHEN OTHER
      *         DISPLAY "Invalid choice! Press Enter."
      *         ACCEPT DUMMY *> Pause for user
      *     END-EVALUATE
      *     END-PERFORM
      *     STOP RUN.
