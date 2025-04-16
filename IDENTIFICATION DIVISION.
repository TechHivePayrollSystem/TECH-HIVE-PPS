       IDENTIFICATION DIVISION.
       PROGRAM-ID. PAYROLL-SYSTEM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PAYROLL-FILE ASSIGN TO "payroll.dat"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EMPLOYEE-ID
               FILE STATUS IS FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD PAYROLL-FILE.
       01 PAYROLL-RECORD.
           05 EMPLOYEE-ID     PIC X(5).
           05 EMPLOYEE-NAME   PIC X(20).
           05 HOURLY-RATE     PIC 9(3)V99.
           05 HOURS-WORKED    PIC 9(3)V99.
           05 GROSS-PAY       PIC 9(5)V99.

       WORKING-STORAGE SECTION.
       01 FILE-STATUS          PIC XX.
       01 USER-CHOICE          PIC 9.
       01 CONTINUE-FLAG        PIC X VALUE "Y".
       01 TEMP-ID              PIC X(5).

       PROCEDURE DIVISION.
       MAIN-LOOP.
           PERFORM UNTIL CONTINUE-FLAG = "N"
               DISPLAY "=========================="
               DISPLAY " COBOL PAYROLL SYSTEM MENU"
               DISPLAY "=========================="
               DISPLAY "1. Add Employee"
               DISPLAY "2. Read Employee"
               DISPLAY "3. Update Employee"
               DISPLAY "9. Exit"
               DISPLAY "Enter your choice:"
               ACCEPT USER-CHOICE

               EVALUATE USER-CHOICE
                   WHEN 1
                       PERFORM ADD-EMPLOYEE
                   WHEN 2
                       PERFORM READ-EMPLOYEE
                   WHEN 3
                       PERFORM UPDATE-EMPLOYEE
                   WHEN 9
                       MOVE "N" TO CONTINUE-FLAG
                   WHEN OTHER
                       DISPLAY "Invalid choice, try again."
               END-EVALUATE
           END-PERFORM
           DISPLAY "Program ended. Goodbye!"
           STOP RUN.

       ADD-EMPLOYEE.
           OPEN I-O PAYROLL-FILE
           DISPLAY "Enter Employee ID:"
           ACCEPT EMPLOYEE-ID
           DISPLAY "Enter Employee Name:"
           ACCEPT EMPLOYEE-NAME
           DISPLAY "Enter Hourly Rate:"
           ACCEPT HOURLY-RATE
           DISPLAY "Enter Hours Worked:"
           ACCEPT HOURS-WORKED
           COMPUTE GROSS-PAY = HOURLY-RATE * HOURS-WORKED
           DISPLAY "GROSS PAY: " GROSS-PAY

           DISPLAY " "

           DISPLAY "WRITING TO FILE: " PAYROLL-RECORD
           WRITE PAYROLL-RECORD
               INVALID KEY
                   DISPLAY "** Employee already exists! **"
               NOT INVALID KEY
                   DISPLAY "** Employee added successfully. **"
           CLOSE PAYROLL-FILE.

       READ-EMPLOYEE.
           OPEN INPUT PAYROLL-FILE
           DISPLAY "Enter Employee ID to read:"
           ACCEPT TEMP-ID
           MOVE TEMP-ID TO EMPLOYEE-ID

           READ PAYROLL-FILE KEY IS EMPLOYEE-ID
               INVALID KEY
                   DISPLAY "** Employee not found. **"
               NOT INVALID KEY
                   DISPLAY "----- Employee Record -----"
                   DISPLAY "ID       : " EMPLOYEE-ID
                   DISPLAY "Name     : " EMPLOYEE-NAME
                   DISPLAY "Rate     : " HOURLY-RATE
                   DISPLAY "Hours    : " HOURS-WORKED
                   DISPLAY "Gross Pay: " GROSS-PAY
           CLOSE PAYROLL-FILE.

       UPDATE-EMPLOYEE.
           OPEN I-O PAYROLL-FILE
           DISPLAY "Enter Employee ID to update:"
           ACCEPT TEMP-ID
           MOVE TEMP-ID TO EMPLOYEE-ID

           READ PAYROLL-FILE KEY IS EMPLOYEE-ID
               INVALID KEY
                   DISPLAY "** Employee not found. **"
               NOT INVALID KEY
                   DISPLAY "Enter new Hours Worked:"
                   ACCEPT HOURS-WORKED
                   COMPUTE GROSS-PAY = HOURLY-RATE * HOURS-WORKED
                   REWRITE PAYROLL-RECORD
                   DISPLAY "** Employee record updated. **"
           CLOSE PAYROLL-FILE.
