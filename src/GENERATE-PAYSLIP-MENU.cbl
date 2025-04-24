        IDENTIFICATION DIVISION.
        PROGRAM-ID. GEN-PAYSLIP-MENU.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  HOME-CHOICE      PIC 9  VALUE 9.  *> Initialize to non-zero value
        01  DUMMY            PIC X.           *> Input buffer cleanup

        PROCEDURE DIVISION.
           PERFORM UNTIL HOME-CHOICE = 0
           DISPLAY " "
           DISPLAY "PAYSLIP MENU"
           DISPLAY "--------------------------------------------------"
           DISPLAY "1. View employee info"
           DISPLAY "2. Generate a payslip"
           DISPLAY "0. Exit"
           DISPLAY "--------------------------------------------------"
           DISPLAY "Enter your choice: " WITH NO ADVANCING
           ACCEPT HOME-CHOICE 
           ACCEPT DUMMY  *> Clear input buffer (simplified)

            EVALUATE HOME-CHOICE
            WHEN 1 CALL "EmployeeManager"
            WHEN 2 CALL "PAYSLIP"
            WHEN 0 DISPLAY "Exiting system..."
            WHEN OTHER
                DISPLAY "Invalid choice! Press Enter."
                ACCEPT DUMMY
           END-EVALUATE
           END-PERFORM
              
           STOP RUN.