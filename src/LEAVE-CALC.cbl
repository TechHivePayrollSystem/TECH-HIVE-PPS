       IDENTIFICATION DIVISION.
       PROGRAM-ID. LEAVE-CALC.
        DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
                 *> Employee Information
        01 EMPLOYEE-DATA.
              05 EMP-NUMBER        PIC X(10).
              05 EMP-NAME          PIC X(50).
              05 EMP-SALARY        PIC 9(7)V99 VALUE 0.
              05 EMP-DAILY-RATE    PIC 9(5)V99.
                   *> Leave Types and Balances
         01 LEAVE-BALANCES.
              05 SICK-LEAVE       PIC 99 VALUE 10.
              05 ANNUAL-LEAVE      PIC 99 VALUE 15.
              05 FAMILY-LEAVE      PIC 99 VALUE 5.
                *>  User Input Variables
          01 USER-RESPONSE        PIC X.
              88 YES-RESPONSE      VALUE 'Y', 'y'.
              88 NO-RESPONSE       VALUE 'N', 'n'.
              88 PAID-LEAVE        VALUE 'P', 'p'.
              88 UNPAID-LEAVE      VALUE 'U', 'u'.
                *> Leave Calculation Variables
         01 LEAVE-DETAILS.
          05 LEAVE-TYPE        PIC 9.
              88 SICK-LEAVE-TYPE    VALUE 1.
              88 ANNUAL-LEAVE-TYPE  VALUE 2.
              88 FAMILY-LEAVE-TYPE  VALUE 3.
          05 LEAVE-DAYS        PIC 99.
          05 LEAVE-ADJUSTMENT  PIC S9(7)V99 VALUE 0.
          05 WORKING-DAYS-MONTH PIC 99 VALUE 22.

                *> Display Variables
         01 DISPLAY-VARS.
              05 DISPLAY-SALARY    PIC Z(6)9.99.
              05 DISPLAY-ADJUST    PIC -Z(6)9.99.

       PROCEDURE DIVISION.
          MAIN-PROCEDURE.
              PERFORM 100-INITIALIZE
              PERFORM 200-GET-EMPLOYEE-DATA
              PERFORM 300-CALCULATE-DAILY-RATE
              PERFORM 400-LEAVE-MENU.
              PERFORM 500-CALCULATE-ADJUSTMENT
              PERFORM 600-DISPLAY-RESULTS
              STOP RUN.


         100-INITIALIZE.
              DISPLAY " "
              DISPLAY "***************************************"
              DISPLAY "*       LEAVE MANAGEMENT SYSTEM       *"
              DISPLAY "***************************************".

         200-GET-EMPLOYEE-DATA.
              DISPLAY "ENTER EMPLOYEE NUMBER: "
              ACCEPT EMP-NUMBER
              DISPLAY "ENTER EMPLOYEE NAME: "
              ACCEPT EMP-NAME
              DISPLAY "ENTER MONTHLY SALARY (ZAR): "
              ACCEPT EMP-SALARY.

         300-CALCULATE-DAILY-RATE.
              DIVIDE EMP-SALARY BY WORKING-DAYS-MONTH
                   GIVING EMP-DAILY-RATE.
          400-LEAVE-MENU.
              DISPLAY " "
               DISPLAY "DID EMPLOYEE TAKE LEAVE THIS MONTH? (Y/N): "
               ACCEPT USER-RESPONSE

               IF YES-RESPONSE
               PERFORM 410-SHOW-LEAVE-MENU
               ELSE
               MOVE 0 TO LEAVE-ADJUSTMENT
           END-IF.


         410-SHOW-LEAVE-MENU.
               DISPLAY " "
               DISPLAY "SELECT LEAVE TYPE:"
               DISPLAY "1. SICK LEAVE (BALANCE: " SICK-LEAVE " DAYS)"
              DISPLAY "2. ANNUAL LEAVE (BALANCE: " ANNUAL-LEAVE " DAYS)"
              DISPLAY "3. FAMILY RESPONSIBILITY LEAVE"
              DISPLAY "4. NO LEAVE TAKEN"
              DISPLAY "ENTER CHOICE (1-4): "
              ACCEPT LEAVE-TYPE

              EVALUATE TRUE
               WHEN SICK-LEAVE-TYPE
                   PERFORM 420-PROCESS-SICK-LEAVE
               WHEN ANNUAL-LEAVE-TYPE
                   PERFORM 430-PROCESS-ANNUAL-LEAVE
               WHEN FAMILY-LEAVE-TYPE
                   PERFORM 440-PROCESS-FAMILY-LEAVE
               WHEN OTHER
                   MOVE 0 TO LEAVE-ADJUSTMENT
           END-EVALUATE.

         420-PROCESS-SICK-LEAVE.
            DISPLAY "ENTER SICK LEAVE DAYS TAKEN (MAX " SICK-LEAVE "): "
              ACCEPT LEAVE-DAYS
              IF LEAVE-DAYS <= SICK-LEAVE
               SUBTRACT LEAVE-DAYS FROM SICK-LEAVE
               PERFORM 450-GET-LEAVE-PAYMENT-TYPE
              ELSE
                DISPLAY "WARNING: EXCEEDS AVAILABLE SICK LEAVE!"
                MOVE 0 TO LEAVE-ADJUSTMENT
              END-IF.

         430-PROCESS-ANNUAL-LEAVE.
            DISPLAY 
            "ENTER ANNUAL LEAVE DAYS TAKEN (MAX "ANNUAL-LEAVE "):"
           ACCEPT LEAVE-DAYS
           IF LEAVE-DAYS <= ANNUAL-LEAVE
               SUBTRACT LEAVE-DAYS FROM ANNUAL-LEAVE
               PERFORM 450-GET-LEAVE-PAYMENT-TYPE
           ELSE
               DISPLAY "WARNING: EXCEEDS AVAILABLE ANNUAL LEAVE!"
               MOVE 0 TO LEAVE-ADJUSTMENT
           END-IF.


         440-PROCESS-FAMILY-LEAVE.
            DISPLAY "ENTER FAMILY LEAVE DAYS TAKEN (MAX 3 BY LAW): "
           ACCEPT LEAVE-DAYS
           IF LEAVE-DAYS > 3
               DISPLAY "LEGAL LIMIT EXCEEDED! SETTING TO 3 DAYS"
               MOVE 3 TO LEAVE-DAYS
           END-IF
           PERFORM 450-GET-LEAVE-PAYMENT-TYPE.

          450-GET-LEAVE-PAYMENT-TYPE.
              DISPLAY "IS THIS (P)AID OR (U)NPAID LEAVE? "
               ACCEPT USER-RESPONSE.
         500-CALCULATE-ADJUSTMENT.
           IF UNPAID-LEAVE
           COMPUTE LEAVE-ADJUSTMENT = EMP-DAILY-RATE * LEAVE-DAYS * -1
             ELSE
               MOVE 0 TO LEAVE-ADJUSTMENT
           END-IF.

          600-DISPLAY-RESULTS.
              MOVE EMP-SALARY TO DISPLAY-SALARY
              MOVE LEAVE-ADJUSTMENT TO DISPLAY-ADJUST

           DISPLAY " "
           DISPLAY "LEAVE DEDUCTION REPORT"
           DISPLAY "-----------------------------"
           DISPLAY "EMPLOYEE: " EMP-NUMBER " - " EMP-NAME
           DISPLAY "MONTHLY SALARY: R" DISPLAY-SALARY
           DISPLAY "DAILY RATE:    R" EMP-DAILY-RATE
           DISPLAY "LEAVE ADJUST: " DISPLAY-ADJUST
           DISPLAY "REMAINING LEAVE BALANCES:"
           DISPLAY "  SICK: " SICK-LEAVE " DAYS"
           DISPLAY "  ANNUAL: " ANNUAL-LEAVE " DAYS"
           DISPLAY "  FAMILY: " FAMILY-LEAVE " DAYS"
           DISPLAY "-----------------------------".




            STOP RUN.
       END PROGRAM LEAVE-CALC.
