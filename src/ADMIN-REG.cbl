        IDENTIFICATION DIVISION.
        PROGRAM-ID. ADMIN-REG.
        AUTHOR. TECH-HIVE.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
            SELECT ADMIN-REG ASSIGN TO "TESTING.dat"
            ORGANIZATION IS LINE SEQUENTIAL.

        DATA DIVISION.
        FILE SECTION.
        FD ADMIN-REG.
        01 ADMIN-RECORD.
            05 ADMIN-USR PIC X(20).
            05 ADMIN-PWD PIC X(20).

        WORKING-STORAGE SECTION.
        01 WS-USRNAME   PIC X(20).
        01 WS-PWD       PIC X(20).
        01 WS-REPEAT-PWD PIC X(20).

        PROCEDURE DIVISION.
            OPEN OUTPUT ADMIN-REG
            DISPLAY "--------------------------------------------------"
            DISPLAY "ADMIN REGISTRATION"
           DISPLAY "--------------------------------------------------"
            DISPLAY "Enter username: "
            ACCEPT WS-USRNAME
            PERFORM PASSWORD-INPUT

            MOVE WS-USRNAME TO ADMIN-USR
            MOVE WS-PWD TO ADMIN-PWD
            WRITE ADMIN-RECORD
            CLOSE ADMIN-REG
            DISPLAY "Admin registered successfully!"
           
            EXIT PROGRAM.  *> Return to home page

        PASSWORD-INPUT.
            DISPLAY "Enter password: "
            ACCEPT WS-PWD
            DISPLAY "Confirm password: "
            ACCEPT WS-REPEAT-PWD
            IF WS-PWD NOT EQUAL WS-REPEAT-PWD
                DISPLAY "Passwords do not match! Try again."
                GO TO PASSWORD-INPUT
            END-IF.