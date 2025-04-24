        IDENTIFICATION DIVISION.
        PROGRAM-ID. PAYROLL-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "EMPMST.IDX"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
            RECORD KEY IS EMP-ID.
       DATA DIVISION.
       FILE SECTION.
       FD  EMPLOYEE-FILE.
       01  EMPLOYEE-RECORD.
           05  EMP-ID           PIC X(06).
           05  EMP-NAME         PIC X(30).
           05  EMP-DEPT         PIC X(15).
           05  EMP-POSITION     PIC X(20).
           05  EMP-HIRE-DATE    PIC 9(8).       *> YYYYMMDD
           05  EMP-PAY-RATE     PIC 9(5)V99.
           05  EMP-TAX-RATE     PIC V999.
           05  EMP-STATUS       PIC X.          *> A=Active, I=Inactive

       WORKING-STORAGE SECTION.
       01  SYSTEM-CONTROLS.
           05  FILE-STATUS      PIC XX.
               88  RECORD-FOUND    VALUE '00'.
               88  DUPLICATE-KEY   VALUE '22'.
               88  FILE-EOF        VALUE '10'.
           05  MENU-CHOICE      PIC 9.
           05  SUB-CHOICE       PIC 9.
           05  TEMP-INPUT       PIC X(20).

       01  PAYROLL-CALCS.
           05  WORK-HOURS       PIC 99V9.
           05  OVERTIME-HRS     PIC 99V9.
           05  GROSS-PAY        PIC 9(6)V99.
           05  TAX-AMOUNT       PIC 9(5)V99.
           05  NET-PAY          PIC 9(6)V99.
           05  BONUS-AMT        PIC 9(5)V99 VALUE ZERO.
           05  DEDUCTIONS       PIC 9(5)V99 VALUE ZERO.

       01  CURRENT-DATE.
           05  CUR-YEAR         PIC 9(4).
           05  CUR-MONTH        PIC 9(2).
           05  CUR-DAY          PIC 9(2).

       01  DISPLAY-LINES.
           05  HEADER-LINE      PIC X(50) VALUE
               "================= PAYROLL SYSTEM =================".
           05  SEPARATOR-LINE   PIC X(50) VALUE ALL "-".

       PROCEDURE DIVISION.
       MAIN-LOGIC.
           PERFORM INITIALIZE-SYSTEM
           PERFORM MAIN-MENU UNTIL MENU-CHOICE = 0
           PERFORM CLOSE-SYSTEM
           STOP RUN.

       INITIALIZE-SYSTEM.
           OPEN I-O EMPLOYEE-FILE
           IF FILE-STATUS = '05'  *> File doesn't exist
               OPEN OUTPUT EMPLOYEE-FILE
               CLOSE EMPLOYEE-FILE
               OPEN I-O EMPLOYEE-FILE
           END-IF
           ACCEPT CURRENT-DATE FROM DATE YYYYMMDD.

       MAIN-MENU.
           DISPLAY HEADER-LINE
           DISPLAY "1. EMPLOYEE MAINTENANCE"
           DISPLAY "2. PAYROLL PROCESSING"
           DISPLAY "3. REPORTS"
           DISPLAY "0. EXIT"
           DISPLAY "ENTER CHOICE: " NO ADVANCING
           ACCEPT MENU-CHOICE
           EVALUATE MENU-CHOICE
               WHEN 1 PERFORM EMPLOYEE-MENU
               WHEN 2 PERFORM PAYROLL-MENU
      *        WHEN 3 PERFORM REPORTS-MENU
           END-EVALUATE.

       EMPLOYEE-MENU.
           PERFORM UNTIL SUB-CHOICE = 0
               DISPLAY HEADER-LINE
               DISPLAY "1. ADD EMPLOYEE"
               DISPLAY "2. EDIT EMPLOYEE"
               DISPLAY "3. VIEW EMPLOYEE"
               DISPLAY "0. RETURN"
               DISPLAY "ENTER CHOICE: " NO ADVANCING
               ACCEPT SUB-CHOICE
               EVALUATE SUB-CHOICE
                   WHEN 1 PERFORM ADD-EMPLOYEE
      *            WHEN 2 PERFORM EDIT-EMPLOYEE
      *            WHEN 3 PERFORM VIEW-EMPLOYEE
               END-EVALUATE
           END-PERFORM.

       ADD-EMPLOYEE.
           DISPLAY "ENTER EMPLOYEE ID (6 CHARS): " NO ADVANCING
           ACCEPT EMP-ID
           DISPLAY "FULL NAME: " NO ADVANCING
           ACCEPT EMP-NAME
           DISPLAY "DEPARTMENT: " NO ADVANCING
           ACCEPT EMP-DEPT
           DISPLAY "POSITION: " NO ADVANCING
           ACCEPT EMP-POSITION
           DISPLAY "HIRE DATE (YYYYMMDD): " NO ADVANCING
           ACCEPT EMP-HIRE-DATE
           DISPLAY "HOURLY RATE: " NO ADVANCING
           ACCEPT EMP-PAY-RATE
           DISPLAY "TAX RATE (0.00-0.99): " NO ADVANCING
           ACCEPT EMP-TAX-RATE
           MOVE 'A' TO EMP-STATUS
           WRITE EMPLOYEE-RECORD
               INVALID KEY
                   DISPLAY "ERROR: DUPLICATE EMPLOYEE ID"
               NOT INVALID KEY
                   DISPLAY "EMPLOYEE ADDED SUCCESSFULLY"
           END-WRITE
           PERFORM PRESS-ENTER.

       PAYROLL-MENU.
           DISPLAY "ENTER EMPLOYEE ID: " NO ADVANCING
           ACCEPT EMP-ID
           READ EMPLOYEE-FILE
               INVALID KEY
                   DISPLAY "EMPLOYEE NOT FOUND!"
                   PERFORM PRESS-ENTER
                   EXIT PARAGRAPH
           END-READ
           DISPLAY "PROCESSING PAYROLL FOR: " EMP-NAME
           DISPLAY "ENTER HOURS WORKED: " NO ADVANCING
           ACCEPT WORK-HOURS
           DISPLAY "ENTER OVERTIME HOURS: " NO ADVANCING
           ACCEPT OVERTIME-HRS
           DISPLAY "ENTER BONUS AMOUNT: " NO ADVANCING
           ACCEPT BONUS-AMT
           DISPLAY "ENTER DEDUCTIONS: " NO ADVANCING
           ACCEPT DEDUCTIONS
           PERFORM CALCULATE-PAY
           PERFORM DISPLAY-PAYSLIP.

       CALCULATE-PAY.
           COMPUTE GROSS-PAY = (WORK-HOURS * EMP-PAY-RATE) +
                              (OVERTIME-HRS * EMP-PAY-RATE * 1.5) +
                              BONUS-AMT
           COMPUTE TAX-AMOUNT = GROSS-PAY * EMP-TAX-RATE
           COMPUTE NET-PAY = GROSS-PAY - TAX-AMOUNT - DEDUCTIONS.

       DISPLAY-PAYSLIP.
           DISPLAY HEADER-LINE
           DISPLAY "PAYSLIP FOR PERIOD: " CUR-YEAR "-" CUR-MONTH
           DISPLAY "EMPLOYEE: " EMP-NAME " (" EMP-ID ")"
           DISPLAY "DEPARTMENT: " EMP-DEPT
           DISPLAY SEPARATOR-LINE
           DISPLAY "EARNINGS:"
           DISPLAY "  REGULAR HOURS: " WORK-HOURS " @ " EMP-PAY-RATE
           DISPLAY "OVERTIME HOURS: " OVERTIME-HRS
           DISPLAY "  BONUS: " BONUS-AMT
           DISPLAY SEPARATOR-LINE
           DISPLAY "DEDUCTIONS:"
           DISPLAY "  TAX (%)" TAX-AMOUNT
           DISPLAY "  OTHER: " DEDUCTIONS
           DISPLAY SEPARATOR-LINE
           DISPLAY "GROSS PAY: " GROSS-PAY
           DISPLAY "NET PAY: " NET-PAY
           DISPLAY HEADER-LINE
           PERFORM PRESS-ENTER.

       PRESS-ENTER.
           DISPLAY "PRESS ENTER TO CONTINUE..."
           ACCEPT TEMP-INPUT.

       CLOSE-SYSTEM.
           CLOSE EMPLOYEE-FILE.
