       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
       SELECT ADMIN-REG ASSIGN TO "TESTING.dat"
        ORGANIZATION IS LINE SEQUENTIAL.

       DATA DIVISION.
       FILE SECTION.
       FD ADMIN-REG.
        01 ADMIN-RECORD.
            05 ADMIN-USER      PIC X(20).
            05 ADMIN-PASS      PIC X(20).

        WORKING-STORAGE SECTION.
       01  WS-USERNAME       PIC X(20).
        01  WS-PASSWORD       PIC X(20).
        01  FILE-STATUS       PIC XX.

       PROCEDURE DIVISION.
           OPEN INPUT ADMIN-REG
      *>  IF FILE-STATUS NOT = "00"
        *>  DISPLAY "No admin registered! Please register first."
       *>  CLOSE ADMIN-REG
       *>  EXIT PROGRAM
        *> END-IF.

            DISPLAY " "
           DISPLAY "--------------------------------------------------"
            DISPLAY "LOGIN"
            DISPLAY "--------------------------------------------------"
            DISPLAY "Username: " WITH NO ADVANCING
           ACCEPT WS-USERNAME
           DISPLAY "Password: " WITH NO ADVANCING
           ACCEPT WS-PASSWORD

           READ ADMIN-REG
            AT END DISPLAY "Admin not found!"
           END-READ

           IF WS-USERNAME = ADMIN-USER AND WS-PASSWORD = ADMIN-PASS
           DISPLAY "Login successful!"
           CALL "PAYROLL-MAIN"
            ELSE
           DISPLAY "Invalid credentials!"
            END-IF
            DISPLAY "--------------------------------------------------"
           CLOSE ADMIN-REG
           EXIT PROGRAM.