        IDENTIFICATION DIVISION.
         PROGRAM-ID. MASTER-PAYROLL.
        DATE-WRITTEN. TODAYS-DATE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT  OPTIONAL EMPLOYEE-FILE ASSIGN TO "employee.TXT"
               ORGANIZATION IS LINE SEQUENTIAL
      *        ACCESS MODE IS RANDOM
      **        RECORD KEY IS EMP-NUMBER
               FILE STATUS IS FS-EMPLOYEE.
       
       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 EMP-NUMBER         PIC 9(5).           
           05 EMP-NAME           PIC X(30).          *> Matches EMP-NAME
           05 EMP-HOURLY         PIC 9(3)V99.        *> Matches EMP-HOURLY
           05 EMP-DEPT           PIC X(20).          *> Matches EMP-DEPT
           05 EMP-LEAVE-BAL      PIC 999.            *> Matches EMP-LEAVE-BAL
           05 EMP-STATUS         PIC X(10).          *> Matches EMP-STATUS

       WORKING-STORAGE SECTION.
       
            *> === File Status ===
       01 FS-EMPLOYEE           PIC XX.
           88 RECORD-FOUND       VALUE "00".
           88 RECORD-NOT-FOUND   VALUE "23".

            *> === Menu & Control ===
       01 MENU-CHOICE        PIC 9.
           88 VALID-CHOICE    VALUES 1 THRU 3.
           88 EXIT-PROGRAM    VALUE 3.
       01 USER-INPUT         PIC X.
           88 YES-RESPONSE    VALUE 'Y' 'y'.
           88 NO-RESPONSE     VALUE 'N' 'n'.
           88 PAID-LEAVE      VALUE 'P' 'p'.
           88 UNPAID-LEAVE    VALUE 'U' 'u'.

           *>=== Employee Info ===
        01 EMPLOYEE-DATA.
               05 EMP-DAILY-RATE    PIC 9(5)V99.
               05 EMP-SALARY        PIC 9(7)V99 VALUE 0.

            *> === Pay Type ===
       01 EMP-TYPE        PIC 9.
           88 SALARIED     VALUE 1.
           88 HOURLY       VALUE 2.
       
       *> === Hourly Wages ===
       01 HOURS-WORKED     PIC 99V99 VALUE 0.
       01 REGULAR-HOURS    PIC 9(5)V99 VALUE 168.00.
       01 OVERTIME-HOURS   PIC 99V99 VALUE 0.
       01 OVERTIME-RATE    PIC 9V99 VALUE 1.5.
       01 REGULAR-PAY      PIC 9(5)V99 VALUE 0.
       01 OVERTIME-PAY     PIC 9(5)V99 VALUE 0.

       *> === Tax Data ===
       01 TAX-AMOUNT       PIC 9(7)V99 VALUE 0.

       *> === Deductions ===
       01 UIF-CONTRIB      PIC 9(5)V99 VALUE 0.
       01 MEDICAL-AID      PIC 9(5)V99 VALUE 0.
       01 UNION-FEE        PIC 9(3) VALUE 50.
       01 TOTAL-DEDUCTS    PIC 9(7)V99 VALUE 0.
       01 BIRTHDAY-BONUS   PIC 9(4) VALUE 500.

       *> === Date Handling ===
       01 CURRENT-DATE.
           05 CD-YEAR       PIC 9(4).
           05 CD-MONTH      PIC 9(2).
           05 CD-DAY        PIC 9(2).
       01 BIRTH-DATE.
           05 BD-DAY        PIC 99.
           05 BD-MONTH      PIC 99.

       *> === Leave Balances ===
       01 LEAVE-BALANCES.
           05 SICK-LEAVE       PIC 99 VALUE 10.
           05 ANNUAL-LEAVE     PIC 99 VALUE 15.
           05 FAMILY-LEAVE     PIC 99 VALUE 5.
       01 LEAVE-DETAILS.
             05 LEAVE-TYPE       PIC 9.
           88 SICK-LEAVE-TYPE   VALUE 1.
           88 ANNUAL-LEAVE-TYPE VALUE 2.
           88 FAMILY-LEAVE-TYPE VALUE 3.
       05 LEAVE-DAYS       PIC 99.
           05 LEAVE-ADJUSTMENT PIC S9(7)V99 VALUE 0.
       01 WORKING-DAYS-MONTH  PIC 99 VALUE 22.

       *> === Display ===
       01 DISPLAY-FIELDS.
           05 DISP-SALARY     PIC Z(6)9.99.
           05 DISP-TAX        PIC Z(6)9.99.
           05 DISP-NET        PIC Z(6)9.99.
           05 DISP-UIF        PIC Z(5)9.99.
           05 DISP-MEDICAL    PIC Z(5)9.99.
           05 DISP-REGULAR    PIC Z(5)9.99.
           05 DISP-OVERTIME   PIC Z(5)9.99.
           05 DISPLAY-ADJUST  PIC -Z(6)9.99.

       PROCEDURE DIVISION.
       
       000-MAIN-MENU.
           PERFORM 100-INITIALIZE
           PERFORM UNTIL EXIT-PROGRAM
               DISPLAY " "
               DISPLAY "==== MASTER PAYROLL SYSTEM ===="
               DISPLAY "1. PROCESS PAYROLL"
               DISPLAY "2. VIEW TAX BRACKETS"
               DISPLAY "3. EXIT"
               DISPLAY "Enter your choice (1-3): "
               ACCEPT MENU-CHOICE
               EVALUATE MENU-CHOICE
                   WHEN 1 PERFORM 200-PROCESS-PAYROLL
                   WHEN 2 PERFORM 300-DISPLAY-TAX
                   WHEN 3 CONTINUE
                   WHEN OTHER DISPLAY "INVALID SELECTION"
               END-EVALUATE
           END-PERFORM
           STOP RUN.

       100-INITIALIZE.
           MOVE FUNCTION CURRENT-DATE(1:4) TO CD-YEAR
           MOVE FUNCTION CURRENT-DATE(5:2) TO CD-MONTH
           MOVE FUNCTION CURRENT-DATE(7:2) TO CD-DAY
           DISPLAY "SYSTEM INITIALIZED ON " CD-DAY 
           "/" CD-MONTH "/" CD-YEAR.

       *> --- Payroll Processing ---
       200-PROCESS-PAYROLL.
           DISPLAY "ENTER EMPLOYEE NUMBER: "
           ACCEPT EMP-NUMBER
 

           DISPLAY "ENTER EMPLOYEE NAME: "
           ACCEPT EMP-NAME

           DISPLAY "ENTER DEPARTMENT: "
           ACCEPT EMP-DEPT 


           PERFORM 205-READ-EMPLOYEE-RECORD
           IF RECORD-NOT-FOUND
               DISPLAY "EMPLOYEE NOT FOUND!"
               EXIT PARAGRAPH
           END-IF
   
            DISPLAY "EMPLOYEE TYPE: (1) SALARIED (2) HOURLY: "
           ACCEPT EMP-TYPE
       
            EVALUATE TRUE
               WHEN SALARIED PERFORM 210-GET-SALARIED
               WHEN HOURLY   PERFORM 215-GET-HOURLY
               WHEN OTHER    PERFORM 210-GET-SALARIED
           END-EVALUATE
   
           PERFORM 220-CALC-TAX
           PERFORM 230-CALC-DEDUCTIONS
           PERFORM 240-CHECK-BIRTHDAY
           PERFORM 250-CALC-DAILY-RATE
           PERFORM 260-APPLY-LEAVE-ADJUST
               PERFORM 270-DISPLAY-PAYSLIP.
           
       205-READ-EMPLOYEE-RECORD.
           OPEN INPUT EMPLOYEE-FILE
           IF FS-EMPLOYEE = "05" *> File doesn't exist
               DISPLAY "EMPLOYEE FILE NOT FOUND, CREATING NEW ONE..."
               OPEN OUTPUT EMPLOYEE-FILE
               CLOSE EMPLOYEE-FILE
               OPEN INPUT EMPLOYEE-FILE
           END-IF
       
           READ EMPLOYEE-FILE
               INVALID KEY
                   SET RECORD-NOT-FOUND TO TRUE
               NOT INVALID KEY
                   SET RECORD-FOUND TO TRUE
           END-READ
           CLOSE EMPLOYEE-FILE.

       210-GET-SALARIED.
           DISPLAY "ENTER MONTHLY SALARY: " 
           ACCEPT EMP-SALARY.
       
       215-GET-HOURLY.
           DISPLAY "ENTER HOURS WORKED: " 
           ACCEPT HOURS-WORKED
           IF HOURS-WORKED > REGULAR-HOURS
               COMPUTE OVERTIME-HOURS = HOURS-WORKED - REGULAR-HOURS
               COMPUTE REGULAR-PAY = REGULAR-HOURS * EMP-HOURLY
               COMPUTE OVERTIME-PAY = OVERTIME-HOURS 
               * EMP-HOURLY * OVERTIME-RATE
           ELSE
                   COMPUTE REGULAR-PAY = HOURS-WORKED * EMP-HOURLY
                   MOVE 0 TO OVERTIME-PAY
           END-IF
           COMPUTE EMP-SALARY = REGULAR-PAY + OVERTIME-PAY.

       220-CALC-TAX.
           EVALUATE TRUE
               WHEN EMP-SALARY <= 195850 MOVE 0 TO TAX-AMOUNT
               WHEN EMP-SALARY <= 305850 
               COMPUTE TAX-AMOUNT = EMP-SALARY * 0.18
               WHEN EMP-SALARY <= 423300 COMPUTE
                TAX-AMOUNT = EMP-SALARY * 0.22
               WHEN EMP-SALARY <= 555600 COMPUTE
                TAX-AMOUNT = EMP-SALARY * 0.28
               WHEN EMP-SALARY <= 708310 COMPUTE TAX-AMOUNT
                = EMP-SALARY * 0.31
               WHEN OTHER COMPUTE TAX-AMOUNT = EMP-SALARY * 0.41
           END-EVALUATE.

       230-CALC-DEDUCTIONS.
           IF EMP-SALARY <= 14872
               COMPUTE UIF-CONTRIB = EMP-SALARY * 0.01
           ELSE
                   MOVE 177.12 TO UIF-CONTRIB
           END-IF
           COMPUTE MEDICAL-AID = EMP-SALARY * 0.02
           DISPLAY "APPLY UNION FEE R50? (Y/N): " 
           ACCEPT USER-INPUT
           IF YES-RESPONSE 
                   ADD UNION-FEE TO TOTAL-DEDUCTS
           END-IF
           COMPUTE TOTAL-DEDUCTS = TAX-AMOUNT 
           + UIF-CONTRIB + MEDICAL-AID.

       240-CHECK-BIRTHDAY.
           DISPLAY "ENTER BIRTH DAY (DD): " 
           ACCEPT BD-DAY
           DISPLAY "ENTER BIRTH MONTH (MM): " 
           ACCEPT BD-MONTH
           IF BD-DAY = CD-DAY AND BD-MONTH = CD-MONTH
               ADD BIRTHDAY-BONUS TO EMP-SALARY
               DISPLAY "BIRTHDAY BONUS R500 ADDED"
           END-IF.

       250-CALC-DAILY-RATE.
           DIVIDE EMP-SALARY BY
            WORKING-DAYS-MONTH 
            GIVING EMP-DAILY-RATE.

       260-APPLY-LEAVE-ADJUST.
           DISPLAY "ENTER LEAVE TYPE: (1) SICK (2) ANNUAL (3) FAMILY: "
           ACCEPT LEAVE-TYPE
           DISPLAY "ENTER NUMBER OF LEAVE DAYS TAKEN: "
           ACCEPT LEAVE-DAYS
           IF LEAVE-TYPE = 2 OR LEAVE-TYPE = 3
               DISPLAY "IS THE LEAVE PAID OR UNPAID? (P/U): "
               ACCEPT USER-INPUT
               IF UNPAID-LEAVE
                   COMPUTE LEAVE-ADJUSTMENT =
                    LEAVE-DAYS * EMP-DAILY-RATE
                   MOVE LEAVE-ADJUSTMENT TO DISPLAY-ADJUST
                   DISPLAY "UNPAID LEAVE DEDUCTED: R" LEAVE-ADJUSTMENT
               END-IF
            END-IF
            DISPLAY "LEAVE ADJUSTMENT PROCESSED.".

       270-DISPLAY-PAYSLIP.
           MOVE EMP-SALARY TO DISP-SALARY
           MOVE TAX-AMOUNT TO DISP-TAX
           MOVE UIF-CONTRIB TO DISP-UIF
           MOVE MEDICAL-AID TO DISP-MEDICAL
           MOVE REGULAR-PAY TO DISP-REGULAR
           MOVE OVERTIME-PAY TO DISP-OVERTIME
           COMPUTE DISP-NET = 
           EMP-SALARY - TOTAL-DEDUCTS - LEAVE-ADJUSTMENT

           DISPLAY "==== PAYSLIP ===="
           DISPLAY "EMP NO: " EMP-NUMBER
           DISPLAY "EMP NAME: " EMP-NAME
           DISPLAY "DEPT: " EMP-DEPT
           DISPLAY "STATUS: " EMP-STATUS
           DISPLAY "LEAVE BALANCE: " EMP-LEAVE-BAL " DAYS"
           DISPLAY "DATE: " CD-DAY "/" CD-MONTH "/" CD-YEAR
           DISPLAY "-----------------"
           IF HOURLY
                   DISPLAY "HOURLY RATE: R" EMP-HOURLY
                   DISPLAY "HOURS WORKED: " HOURS-WORKED
                   DISPLAY "REGULAR PAY: R" DISP-REGULAR
                   IF OVERTIME-PAY > 0
                       DISPLAY "OVERTIME PAY: R" DISP-OVERTIME
                   END-IF
           END-IF
           DISPLAY "GROSS: R" DISP-SALARY
           DISPLAY "-----------------"
           DISPLAY "DEDUCTIONS:"
           DISPLAY "TAX: R" DISP-TAX
           DISPLAY "UIF: R" DISP-UIF
           DISPLAY "MEDICAL: R" DISP-MEDICAL
           IF UNION-FEE > 0 
                   DISPLAY "UNION FEE: R" UNION-FEE
           END-IF
           IF LEAVE-ADJUSTMENT NOT = 0
                   DISPLAY "UNPAID LEAVE DEDUCTED: R" DISPLAY-ADJUST
           END-IF
           DISPLAY "-----------------"
           DISPLAY "NET: R" DISP-NET
           DISPLAY "=================".

       300-DISPLAY-TAX.
           DISPLAY "TAX BRACKETS:"
           DISPLAY "R0 - R195,850      0%"
           DISPLAY "R195,851 - R305,850   18%"
           DISPLAY "R305,851 - R423,300   22%"
           DISPLAY "R423,301 - R555,600   28%"
           DISPLAY "R555,601 - R708,310   31%"
           DISPLAY "R708,311+             41%".