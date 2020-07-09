       >> SOURCE FORMAT FREE
IDENTIFICATION DIVISION. 
PROGRAM-ID. ATM. 
AUTHOR. NICK CICCHETTI
DATE-WRITTEN. May 5th 2020
ENVIRONMENT DIVISION. 
*> Computer it is on/ Divices avalable/ country specific info
INPUT-OUTPUT SECTION.
FILE-CONTROL.
       SELECT TransactionsHistory ASSIGN TO "TransactionsHistory.dat"
           ORGANIZATION IS LINE SEQUENTIAL    
           ACCESS IS SEQUENTIAL
           FILE STATUS  WSFileStatus.
DATA DIVISION. 
FILE SECTION.
FD  TransactionsHistory.
01 TransactionData.
       02 Ballance PIC S99999999V99.
       02 AccessDate.
           03 CurrentYear PIC 9(04).
           03 CurrentMonth PIC 9(02).
           03 CurrentDay PIC 9(02).
       02 Action PIC X(50).
       02 Amount PIC S99999999V99.
       02 CustomerName.
           03 FullName PIC X(500).
*> Describes data sent/recived 
WORKING-STORAGE SECTION. 
*> varables inside the program
01 DollarFormat PIC $$$,$$$,$$9.99.
01 DateFormat PIC 99/99/9999.
01 WSCusomer.
       02 WSAccessDate PIC X(8).
       02 WSBallance PIC S99999999V99 VALUE ZERO.
       02 WSAction PIC X(50).
       02 WSCustomerName.
           03 WSFullName PIC X(500).
           03 WSFirstName PIC X(250).
           03 WSLastName PIC X(250).

01 WSFileStatus PIC X(2).
              88 WSFileAlreadyOpen VALUE '41'.

01 MenuConfiguration.
       02 MenuInput PIC S9 VALUE -1.
       88 ValidMenuOptions VALUE 0 THRU 2.
       02 DepositInput PIC S999999V99 VALUE ZERO.
       88 ValidDepositOptions VALUE IS 1 THRU 999999.
       02 WidthdrawlInput PIC S999999V99 VALUE -1.
       88 ValidWidthdrawlOptions VALUE IS 0 THRU 999999.
PROCEDURE DIVISION. 
Main.    
       DISPLAY "Enter Your First and Last name. "
       ACCEPT WSFullName

       UNSTRING WSFullName DELIMITED BY SPACE
       INTO WSFirstName, WSLastName
     
       COMPUTE WSBallance = FUNCTION RANDOM(1) * 999999.99
       MOVE WSBallance To DollarFormat
       DISPLAY "Hi "WSFirstName" "WSLastName", your current ballance is "DollarFormat"!"
        
       PERFORM DisplayMenu
       
       DISPLAY "BYE :)"
       Stop RUN.

ShowMenuOptions.
       DISPLAY "Please select one of the following options:"
       DISPLAY "0. Exit"
       DISPLAY "1. Deposit Money"
       DISPLAY "2. Withdrawal Money".

DisplayMenu.
       PERFORM ShowMenuOptions
       PERFORM UNTIL MenuInput IS Equal To 0
           ACCEPT MenuInput
           EVALUATE MenuInput
               WHEN 0 
                   DISPLAY "logging you off"
                   STOP RUN
               WHEN 1
                   COMPUTE MenuInput = -1
                   MOVE "Deposit" TO WSAction
                   PERFORM Deposit
                   PERFORM ShowMenuOptions
               When 2 
                   COMPUTE MenuInput = -1
                   MOVE "Widthdrawl" TO WSAction
                   PERFORM Widthdrawl
                   PERFORM ShowMenuOptions
               WHEN OTHER 
                   DISPLAY "You Entered an Incorrect value please select from one of the 3 options above"
            END-EVALUATE 
       END-PERFORM
       
       MOVE SPACE TO WSAction
       COMPUTE MenuInput = -1.

Deposit.
       MOVE WSBallance To DollarFormat
       DISPLAY "Your Current Ballance is " DollarFormat
       DISPLAY "Enter the amount of money you wish to deposit to your account"
       PERFORM UNTIL ValidDepositOptions
           ACCEPT DepositInput
           EVALUATE DepositInput
               WHEN 1 THRU 999999
                   COMPUTE WSBallance = WSBallance + DepositInput
                   MOVE WSBallance To DollarFormat
                   DISPLAY "Your new Ballance is " DollarFormat
                   PERFORM LogTransaction
               WHEN OTHER 
                   DISPLAY "You Entered an Incorrect value please enter a value greater than 0"
            END-EVALUATE 
       END-PERFORM

       COMPUTE DepositInput = ZERO.

Widthdrawl.
       MOVE WSBallance To DollarFormat
       DISPLAY "Your Current Ballance is " DollarFormat
       DISPLAY "Enter the amount of money you wish to widthdrawl to your account"
       PERFORM UNTIL ValidWidthdrawlOptions
       ACCEPT WidthdrawlInput
           IF(WidthdrawlInput <= WSBallance AND WidthdrawlInput >= 0) 
               COMPUTE WSBallance = WSBallance - WidthdrawlInput
               MOVE WSBallance To DollarFormat
               DISPLAY "Your new Ballance is " DollarFormat
               PERFORM LogTransaction
           ELSE
               DISPLAY "You Entered an Incorrect value please enter a value greater than 0 and less than " DollarFormat
           END-IF   
       END-PERFORM

       COMPUTE WidthdrawlInput = -1.

LogTransaction.         
       IF (WSFileAlreadyOpen)
           CLOSE TransactionsHistory
       END-IF
       OPEN EXTEND TransactionsHistory.
           MOVE FUNCTION CURRENT-DATE to AccessDate
           MOVE WSFullName TO FullName
           MOVE WSAction TO Action
           COMPUTE Ballance = WSBallance
           EVALUATE WSAction
               WHEN "Deposit"
                   COMPUTE Amount = DepositInput
               WHEN "Widthdrawl"
                   COMPUTE Amount = WidthdrawlInput * -1
           END-EVALUATE       
           WRITE TransactionData
           END-WRITE.
       CLOSE TransactionsHistory
       DISPLAY "Logged " WSAction.
