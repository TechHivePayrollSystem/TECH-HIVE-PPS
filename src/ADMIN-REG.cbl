        IDENTIFICATION DIVISION.
        PROGRAM-ID. ADMIN-REG.
        AUTHOR. TECH-HIVE.
        ENVIRONMENT DIVISION.
        INPUT-OUTPUT SECTION.
        FILE-CONTROL.
         SELECT ADMIN-REG ASSIGN TO "TESTING.dat"
        ORGANIZATION IS LINE SEQUENTIAL
        FILE STATUS IS FS-STATUS.  *> Track file status

        DATA DIVISION.
        FILE SECTION.
        FD ADMIN-REG.
         01 ADMIN-RECORD.
        05 ADMIN-USR PIC X(20).
        05 ADMIN-PWD PIC X(20).

         WORKING-STORAGE SECTION.
         01  WS-USRNAME      PIC X(20).
        01  WS-PWD          PIC X(20).
         01  WS-REPEAT-PWD   PIC X(20).
        01  FS-STATUS       PIC XX.          *> File status code
        88 FILE-OK      VALUE "00".      *> Success code

        PROCEDURE DIVISION.
           PERFORM INITIALIZE-FILE
            DISPLAY "--------------------------------------------------"
             DISPLAY "ADMIN REGISTRATION"
            DISPLAY "--------------------------------------------------"
             DISPLAY "Enter username: "
            ACCEPT WS-USRNAME
              PERFORM PASSWORD-INPUT
              PERFORM WRITE-ADMIN-RECORD
             PERFORM CLOSE-FILE
             EXIT PROGRAM.

        INITIALIZE-FILE.
            OPEN OUTPUT ADMIN-REG
            IF NOT FILE-OK
            DISPLAY "ERROR: Cannot create admin file. Status: ",
             FS-STATUS
            PERFORM CLOSE-FILE
             STOP RUN
             END-IF.

       WRITE-ADMIN-RECORD.
            MOVE WS-USRNAME TO ADMIN-USR
             MOVE WS-PWD TO ADMIN-PWD
            WRITE ADMIN-RECORD
            IF NOT FILE-OK
            DISPLAY "ERROR: Failed to write admin record. Status: ",
             FS-STATUS
             ELSE
              DISPLAY "Admin registered successfully!"
              END-IF.

        PASSWORD-INPUT.
             DISPLAY "Enter password: "
            ACCEPT WS-PWD
             DISPLAY "Confirm password: "
             ACCEPT WS-REPEAT-PWD
             IF WS-PWD NOT EQUAL WS-REPEAT-PWD
             DISPLAY "Passwords do not match! Try again."
            GO TO PASSWORD-INPUT
            END-IF.

        CLOSE-FILE.
           CLOSE ADMIN-REG.  *> Explicitly close the file