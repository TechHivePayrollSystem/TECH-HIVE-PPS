       IDENTIFICATION DIVISION.
       PROGRAM-ID. HIVE-PAYROLL-SYSTEM.
       AUTHOR. YOUR-NAME.
       DATE-WRITTEN. TODAYS-DATE.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
      * System Control
       01 MENU-CHOICE          PIC 9.
          88 VALID-CHOICE      VALUES 1 THRU 3.
          88 EXIT-PROGRAM      VALUE 3.
       01 USER-INPUT           PIC X.
          88 YES-RESPONSE      VALUE 'Y', 'y'.
          88 NO-RESPONSE       VALUE 'N', 'n'.

      * Employee Type
       01 EMP-TYPE            PIC 9.
          88 SALARIED         VALUE 1.
          88 HOURLY           VALUE 2.

      * Hourly Wage Data
       01 HOURS-WORKED        PIC 99V99 VALUE 0.
       01 REGULAR-HOURS       PIC 9(5)V99 VALUE 168.00.
       01 OVERTIME-HOURS      PIC 99V99 VALUE 0.
       01 HOURLY-RATE         PIC 999V99 VALUE 0.
       01 OVERTIME-RATE       PIC 9V99 VALUE 1.5.
       01 REGULAR-PAY         PIC 9(5)V99 VALUE 0.
       01 OVERTIME-PAY        PIC 9(5)V99 VALUE 0.

      * Employee Data
       01 EMPLOYEE-DATA.
          05 EMP-NUMBER       PIC X(10).
          05 EMP-NAME         PIC X(50).
          05 EMP-SALARY       PIC 9(7)V99 VALUE 0.

      * Tax Data
       01 TAX-CALC.
          05 TAX-RATE         PIC 9(2)    VALUE 0.
          05 TAX-AMOUNT       PIC 9(7)V99 VALUE 0.
          05 TAX-BRACKETS.
             10 BRACKET-1     PIC 9(6)    VALUE 195850.
             10 BRACKET-2     PIC 9(6)    VALUE 305850.
             10 BRACKET-3     PIC 9(6)    VALUE 423300.
             10 BRACKET-4     PIC 9(6)    VALUE 555600.
             10 BRACKET-5     PIC 9(6)    VALUE 708310.

      * Deductions
       01 DEDUCTIONS.
          05 UIF-CONTRIB      PIC 9(5)V99 VALUE 0.
          05 MEDICAL-AID      PIC 9(5)V99 VALUE 0.
          05 UNION-FEE        PIC 9(3)    VALUE 50.
          05 TOTAL-DEDUCTS    PIC 9(7)V99 VALUE 0.
          05 BIRTHDAY-BONUS  PIC 9(4)    VALUE 500.

      * Date Handling
       01 CURRENT-DATE.
          05 CD-YEAR         PIC 9(4).
          05 CD-MONTH        PIC 9(2).
          05 CD-DAY          PIC 9(2).
       01 BIRTH-DATE.
          05 BD-DAY          PIC 99.
          05 BD-MONTH        PIC 99.

      * Display Formats
       01 DISPLAY-FIELDS.
          05 DISP-SALARY     PIC Z(6)9.99.
          05 DISP-TAX        PIC Z(6)9.99.
          05 DISP-NET        PIC Z(6)9.99.
          05 DISP-UIF        PIC Z(5)9.99.
          05 DISP-MEDICAL    PIC Z(5)9.99.
          05 DISP-REGULAR    PIC Z(5)9.99.
          05 DISP-OVERTIME   PIC Z(5)9.99.

       PROCEDURE DIVISION.
      *000-MAIN-MENU.
           PERFORM 100-INITIALIZE
           PERFORM UNTIL EXIT-PROGRAM
               DISPLAY " "
               DISPLAY "HIV3 INVESTMENTS PAYROLL SYSTEM"
               DISPLAY "1. PROCESS PAYROLL"
               DISPLAY "2. VIEW TAX BRACKETS"
               DISPLAY "3. EXIT"
               DISPLAY "ENTER CHOICE (1-3): "
               ACCEPT MENU-CHOICE

               EVALUATE MENU-CHOICE
                   WHEN 1 PERFORM 200-PROCESS-PAYROLL
                   WHEN 2 PERFORM 300-DISPLAY-TAX-BRACKETS
                   WHEN 3 CONTINUE
                   WHEN OTHER DISPLAY "INVALID CHOICE"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       100-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:4) TO CD-YEAR.
           MOVE FUNCTION CURRENT-DATE(5:2) TO CD-MONTH.
           MOVE FUNCTION CURRENT-DATE(7:2) TO CD-DAY.
            DISPLAY "SYSTEM INITIALIZED ON " 
            CD-DAY "/" CD-MONTH "/" CD-YEAR.

       200-PROCESS-PAYROLL.
           DISPLAY "EMPLOYEE TYPE: (1) SALARIED (2) HOURLY: "
           ACCEPT EMP-TYPE

           EVALUATE TRUE
               WHEN SALARIED PERFORM 210-GET-SALARIED-DATA
               WHEN HOURLY PERFORM 215-GET-HOURLY-DATA
               WHEN OTHER
                   DISPLAY "INVALID SELECTION - USING SALARIED"
                   PERFORM 210-GET-SALARIED-DATA
           END-EVALUATE

           PERFORM 220-CALCULATE-TAX
           PERFORM 230-CALCULATE-DEDUCTIONS
           PERFORM 240-CHECK-BIRTHDAY
           PERFORM 250-DISPLAY-PAYSLIP.

       210-GET-SALARIED-DATA.
           DISPLAY "ENTER EMPLOYEE NUMBER: "
           ACCEPT EMP-NUMBER
           DISPLAY "ENTER EMPLOYEE NAME: "
           ACCEPT EMP-NAME
           DISPLAY "ENTER MONTHLY SALARY (ZAR): "
           ACCEPT EMP-SALARY.

       215-GET-HOURLY-DATA.
           DISPLAY "ENTER EMPLOYEE NUMBER: "
           ACCEPT EMP-NUMBER
           DISPLAY "ENTER EMPLOYEE NAME: "
           ACCEPT EMP-NAME
           DISPLAY "ENTER HOURLY RATE (ZAR): "
           ACCEPT HOURLY-RATE
           DISPLAY "ENTER HOURS WORKED THIS MONTH: "
           ACCEPT HOURS-WORKED

           IF HOURS-WORKED > REGULAR-HOURS
               COMPUTE OVERTIME-HOURS = HOURS-WORKED - REGULAR-HOURS
               COMPUTE REGULAR-PAY = REGULAR-HOURS * HOURLY-RATE
           COMPUTE OVERTIME-PAY = OVERTIME-HOURS *
           HOURLY-RATE * OVERTIME-RATE
           ELSE
               COMPUTE REGULAR-PAY = HOURS-WORKED * HOURLY-RATE
               MOVE 0 TO OVERTIME-PAY
           END-IF

           COMPUTE EMP-SALARY = REGULAR-PAY + OVERTIME-PAY
           DISPLAY "MONTHLY EARNINGS: R" EMP-SALARY.

       220-CALCULATE-TAX.
           IF EMP-SALARY <= 195850
               MOVE 0 TO TAX-AMOUNT
           ELSE
               IF EMP-SALARY <= 305850
                   COMPUTE TAX-AMOUNT = EMP-SALARY * 0.18
               ELSE
                   IF EMP-SALARY <= 423300
                       COMPUTE TAX-AMOUNT = EMP-SALARY * 0.22
                   ELSE
                       IF EMP-SALARY <= 555600
                           COMPUTE TAX-AMOUNT = EMP-SALARY * 0.28
                       ELSE
                           IF EMP-SALARY <= 708310
                               COMPUTE TAX-AMOUNT = EMP-SALARY * 0.31
                           ELSE
                               COMPUTE TAX-AMOUNT = EMP-SALARY * 0.41
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF.



       230-CALCULATE-DEDUCTIONS.
      * UIF Calculation (1% capped at R177.12)
           IF EMP-SALARY <= 14872
               COMPUTE UIF-CONTRIB = EMP-SALARY * 0.01
           ELSE
               MOVE 177.12 TO UIF-CONTRIB
           END-IF.
      * Medical Aid (2%)
           COMPUTE MEDICAL-AID = EMP-SALARY * 0.02.
      * Union Fee (Optional)
           DISPLAY "APPLY UNION FEE OF R50? (Y/N): ".
           ACCEPT USER-INPUT.
           IF YES-RESPONSE
               ADD UNION-FEE TO TOTAL-DEDUCTS
           END-IF.
      * Total Deductions
           COMPUTE TOTAL-DEDUCTS = TAX-AMOUNT 
           + UIF-CONTRIB + MEDICAL-AID.

       240-CHECK-BIRTHDAY.
           DISPLAY "ENTER BIRTH DAY (DD): ".
           ACCEPT BD-DAY.
           DISPLAY "ENTER BIRTH MONTH (MM): ".
           ACCEPT BD-MONTH.
           IF BD-DAY = CD-DAY AND BD-MONTH = CD-MONTH
               ADD BIRTHDAY-BONUS TO EMP-SALARY
               DISPLAY "HAPPY BIRTHDAY! BONUS R500 ADDED"
           END-IF.

       250-DISPLAY-PAYSLIP.
           MOVE EMP-SALARY TO DISP-SALARY
           MOVE TAX-AMOUNT TO DISP-TAX
           MOVE UIF-CONTRIB TO DISP-UIF
           MOVE MEDICAL-AID TO DISP-MEDICAL
           MOVE REGULAR-PAY TO DISP-REGULAR
           MOVE OVERTIME-PAY TO DISP-OVERTIME
           COMPUTE DISP-NET = EMP-SALARY - TOTAL-DEDUCTS

           DISPLAY " "
           DISPLAY "HIV3 INVESTMENTS - PAYSLIP"
           DISPLAY "=========================="
           DISPLAY "EMP NO: " EMP-NUMBER " | " FUNCTION TRIM(EMP-NAME)
           DISPLAY "DATE: " CD-DAY "/" CD-MONTH "/" CD-YEAR
           DISPLAY "--------------------------"

           IF HOURLY
               DISPLAY "PAY TYPE:        HOURLY WAGE"
               DISPLAY "HOURS WORKED:    " HOURS-WORKED
               DISPLAY "REGULAR PAY:     R" DISP-REGULAR
               IF OVERTIME-HOURS > 0
                   DISPLAY "OVERTIME HOURS:  " OVERTIME-HOURS
                   DISPLAY "OVERTIME PAY:    R" DISP-OVERTIME
               END-IF
           ELSE
               DISPLAY "PAY TYPE:        SALARIED"
           END-IF

           DISPLAY "--------------------------"
           DISPLAY "GROSS SALARY:    R" DISP-SALARY
           DISPLAY "TAX (" TAX-RATE "%):      R" DISP-TAX
           DISPLAY "UIF:            R" DISP-UIF
           DISPLAY "MEDICAL AID:    R" DISP-MEDICAL
           IF UNION-FEE > 0
               DISPLAY "UNION FEE:      R" UNION-FEE
           END-IF
           DISPLAY "--------------------------"
           DISPLAY "NET SALARY:      R" DISP-NET
           DISPLAY "==========================".

       300-DISPLAY-TAX-BRACKETS.
           DISPLAY " "
           DISPLAY "SOUTH AFRICAN TAX BRACKETS (2024):"
           DISPLAY "R0 - R195,850:       0%"
           DISPLAY "R195,851 - R305,850: 18%"
           DISPLAY "R305,851 - R423,300: 22%"
           DISPLAY "R423,301 - R555,600: 28%"
           DISPLAY "R555,601 - R708,310: 31%"
           DISPLAY "R708,311+:          41%".
