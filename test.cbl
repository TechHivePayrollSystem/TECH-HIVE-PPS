      ******************************************************************
      * Author:
      * Date:
      * Purpose: The main admin user-login
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. Tech-hive.
                    *>  ENVIRONMENT DIVION BEGINS HERE
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ADMIN-REG ASSIGN TO "TESTING.dat"
           ORGANIZATION IS LINE SEQUENTIAL.
                *>  DATA DIVION BEGINS HERE
       DATA DIVISION.
       FILE SECTION.
       FD ADMIN-REG.
       01 ADMIN-RECORD.
           05 ADMIN-USR PIC X(20).
           05 ADMIN-PWD PIC X(20).
              *>  WORKING DIVION BEGINS HERE
       WORKING-STORAGE SECTION.
       01 WS-USRNAME   PIC X(20).
       01 WS-PWD       PIC X(20).
       01 WS-REPEAT-PWD     PIC X(20).
       01 WS-EOF   PIC X VALUE 'N'.
                   *>  PROCEDURE DIVION BEGINS HERE
       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           OPEN OUTPUT ADMIN-REG.

           DISPLAY "ADMIN REGISTRATION".
           DISPLAY "ENTER USERNAME".
           ACCEPT WS-USRNAME.
       PASSWORD-INPUT.
           DISPLAY "ENTER PASSWORD"
           ACCEPT WS-PWD.
           DISPLAY "CONFIRM PASSWORD".
           ACCEPT WS-REPEAT-PWD.
           IF WS-PWD NOT EQUAL WS-REPEAT-PWD THEN
           DISPLAY "PASSWORD DO NOT MATCH. TRY AGAIN."
             GO TO PASSWORD-INPUT
           END-IF.
           MOVE WS-USRNAME TO ADMIN-USR.
           MOVE WS-PWD TO ADMIN-PWD.
           WRITE ADMIN-RECORD.
           DISPLAY "ADMIN REGISTERED SUCCESSFULY"
            STOP RUN.
       END PROGRAM Tech-hive.
