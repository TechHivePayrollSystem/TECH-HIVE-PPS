        IDENTIFICATION DIVISION.
        PROGRAM-ID. HOMEPAGE.
        DATA DIVISION.
        WORKING-STORAGE SECTION.
        01  HOME-CHOICE      PIC 9  VALUE 9.  *> Initialize to non-zero value
        01  DUMMY            PIC X.           *> Input buffer cleanup

        PROCEDURE DIVISION.
        PERFORM UNTIL HOME-CHOICE = 0
        DISPLAY " "
        DISPLAY "WELCOME TO THE TECH-HIVE PAYROLL SYSTEM"
        DISPLAY "--------------------------------------------------"
        DISPLAY "1. Login"
        DISPLAY "2. Register Admin"
        DISPLAY "0. Exit"
        DISPLAY "--------------------------------------------------"
        DISPLAY "Enter your choice: " WITH NO ADVANCING
        ACCEPT HOME-CHOICE WITH CONVERSION
        ACCEPT DUMMY  *> Clear input buffer (simplified)

        EVALUATE HOME-CHOICE
            WHEN 1 CALL "LOGIN"
            WHEN 2 CALL "ADMIN-REG"
            WHEN 0 DISPLAY "Exiting system..."
            WHEN OTHER
                DISPLAY "Invalid choice! Press Enter."
                ACCEPT DUMMY
        END-EVALUATE
        END-PERFORM
        STOP RUN.