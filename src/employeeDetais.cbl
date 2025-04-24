       IDENTIFICATION DIVISION.
       PROGRAM-ID. EmployeeManager.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT OPTIONAL EmployeeFile ASSIGN TO "employee.txt"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS EmpID
               FILE STATUS IS FileStatus.

       DATA DIVISION.

       FILE SECTION.
       FD EmployeeFile.
       01 EmployeeRecord.
           05 EmpID        PIC 9(5).
           05 EmpName      PIC X(30).
           05 EmpDept      PIC X(20).
           05 EmpRole      PIC X(20).

       WORKING-STORAGE SECTION.
       01 FileStatus      PIC XX.
       01 Choice          PIC 9.
       01 WS-Choice       PIC X.
       01 WS-EmpID        PIC 9(5).
       01 Continue-Flag   PIC X VALUE 'Y'.
       01 EOF-FLAG        PIC X(1) VALUE 'N'.


       PROCEDURE DIVISION.

           PERFORM Main-Logic.

       Main-Logic.
           OPEN I-O EmployeeFile
           IF FileStatus NOT = "00"



           PERFORM UNTIL Continue-Flag = "N"
               DISPLAY "============================"
               DISPLAY "1. Add Employee"
               DISPLAY "2. Search Employee by ID"
               DISPLAY "3. view employees list"
               DISPLAY "4. Exit"
               DISPLAY "Enter your choice: "
               ACCEPT WS-Choice
               MOVE FUNCTION NUMVAL(WS-Choice) TO Choice
               DISPLAY "You entered: " Choice

               EVALUATE Choice
                   WHEN 1
                       PERFORM AddEmployee
                   WHEN 2
                       PERFORM FindEmployee
                   WHEN 3
                       PERFORM FindAllEmployees
                   WHEN 4
                       MOVE "N" TO Continue-Flag
                   WHEN OTHER
                       DISPLAY "Invalid option. Try again."
                       STOP RUN
               END-EVALUATE
           END-PERFORM

           CLOSE EmployeeFile
           STOP RUN.

       AddEmployee.
           DISPLAY "Enter Employee ID: "
           ACCEPT EmpID
           DISPLAY "Enter Name: "
           ACCEPT EmpName
           DISPLAY "Enter Department:"
           ACCEPT EmpDept
             DISPLAY "Enter Role:"
           ACCEPT EmpRole


           DISPLAY EmployeeRecord
           WRITE EmployeeRecord
               INVALID KEY
                   DISPLAY "Employee ID already exists!"
               NOT INVALID KEY
                   DISPLAY "Employee saved successfully."
           END-WRITE
           .

       FindEmployee.
           DISPLAY ">>> Inside FindEmployee"
           DISPLAY "Enter Employee ID to search: "
           ACCEPT WS-EmpID
           MOVE WS-EmpID TO EmpID

           READ EmployeeFile RECORD KEY IS EmpID
               INVALID KEY
                   DISPLAY "Employee not found."
               NOT INVALID KEY
                   DISPLAY "ID: " EmpID
                   DISPLAY "Name: " EmpName
                   DISPLAY "Dept: " EmpDept
           .

       FindAllEmployees.
           DISPLAY ">>> Listing All Employees"

           OPEN INPUT EmployeeFile

           PERFORM UNTIL EOF-FLAG = 'Y'
               READ EmployeeFile
                   AT END
                       MOVE 'Y' TO EOF-FLAG
                   NOT AT END
                       DISPLAY "-------------------------"
                       DISPLAY "ID: " EmpID
                       DISPLAY "Name: " EmpName
                       DISPLAY "Dept: " EmpDept
                        DISPLAY "Role: " EmpRole
               END-READ
           END-PERFORM

           CLOSE EmployeeFile

           DISPLAY ">>> End of Employee List"

           .
