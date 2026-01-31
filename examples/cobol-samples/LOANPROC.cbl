       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOANPROC.
       AUTHOR. MIGRATIONPILOT-EXAMPLE.
      *
      * LOAN PROCESSING SYSTEM
      * This program calculates loan payments, interest, and amortization
      *
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO "LOANS.DAT"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LF-LOAN-NUMBER.
       
       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       01  LOAN-RECORD.
           05  LF-LOAN-NUMBER         PIC 9(10).
           05  LF-CUSTOMER-NAME       PIC X(50).
           05  LF-PRINCIPAL           PIC 9(9)V99.
           05  LF-INTEREST-RATE       PIC 9(2)V9(4).
           05  LF-TERM-MONTHS         PIC 9(3).
           05  LF-START-DATE          PIC 9(8).
           05  LF-LOAN-TYPE           PIC X(1).
               88 FIXED-RATE          VALUE 'F'.
               88 VARIABLE-RATE       VALUE 'V'.
               88 INTEREST-ONLY       VALUE 'I'.
           05  LF-PAYMENT-AMOUNT      PIC 9(7)V99.
           05  LF-BALANCE             PIC 9(9)V99.

       WORKING-STORAGE SECTION.
       01  WS-CALCULATION-FIELDS.
           05  WS-MONTHLY-RATE        PIC 9V9(8).
           05  WS-NUM-PAYMENTS        PIC 9(3).
           05  WS-PAYMENT             PIC 9(7)V99.
           05  WS-TOTAL-INTEREST      PIC 9(9)V99.
           05  WS-PRINCIPAL-PART      PIC 9(7)V99.
           05  WS-INTEREST-PART       PIC 9(7)V99.
           05  WS-REMAINING-BALANCE   PIC 9(9)V99.
           
       01  WS-LATE-FEE-FIELDS.
           05  WS-DAYS-LATE           PIC 9(3).
           05  WS-LATE-FEE            PIC 9(5)V99.
           05  WS-ACCOUNT-TYPE        PIC X(10).
               88 PREMIUM-ACCOUNT     VALUE 'PREMIUM'.
               88 STANDARD-ACCOUNT    VALUE 'STANDARD'.
           05  WS-MAX-LATE-FEE        PIC 9(5)V99 VALUE 500.00.
           05  WS-LATE-FEE-RATE       PIC 9V9(4) VALUE 0.0150.

       01  WS-FLAGS.
           05  WS-EOF-FLAG            PIC X VALUE 'N'.
               88 END-OF-FILE         VALUE 'Y'.
           05  WS-ERROR-FLAG          PIC X VALUE 'N'.
               88 HAS-ERROR           VALUE 'Y'.

       01  WS-ERROR-MESSAGE           PIC X(100).

       PROCEDURE DIVISION.
       
       0000-MAIN-PROCEDURE.
           PERFORM 1000-INITIALIZE
           PERFORM 2000-PROCESS-LOANS UNTIL END-OF-FILE
           PERFORM 9000-CLEANUP
           STOP RUN.

       1000-INITIALIZE.
           OPEN I-O LOAN-FILE
           IF NOT END-OF-FILE
               READ LOAN-FILE
                   AT END SET END-OF-FILE TO TRUE
               END-READ
           END-IF.

       2000-PROCESS-LOANS.
           PERFORM 2100-CALCULATE-PAYMENT
           PERFORM 2200-CALCULATE-LATE-FEE
           PERFORM 2300-UPDATE-BALANCE
           READ LOAN-FILE NEXT
               AT END SET END-OF-FILE TO TRUE
           END-READ.

      *****************************************************************
      * CALCULATE-PAYMENT
      * Business Rule: Calculate monthly payment using standard
      * amortization formula: M = P * [r(1+r)^n] / [(1+r)^n - 1]
      * Where:
      *   P = Principal (loan amount)
      *   r = Monthly interest rate (annual rate / 12)
      *   n = Total number of payments (term in months)
      *****************************************************************
       2100-CALCULATE-PAYMENT.
      * Calculate monthly interest rate
           COMPUTE WS-MONTHLY-RATE = 
               LF-INTEREST-RATE / 12

      * For fixed-rate loans, use standard amortization
           IF FIXED-RATE
               COMPUTE WS-PAYMENT ROUNDED = 
                   LF-PRINCIPAL * 
                   (WS-MONTHLY-RATE * 
                    (1 + WS-MONTHLY-RATE) ** LF-TERM-MONTHS) /
                   ((1 + WS-MONTHLY-RATE) ** LF-TERM-MONTHS - 1)
           END-IF

      * For interest-only loans, just calculate interest
           IF INTEREST-ONLY
               COMPUTE WS-PAYMENT ROUNDED = 
                   LF-PRINCIPAL * WS-MONTHLY-RATE
           END-IF

      * Variable rate loans get recalculated each period
           IF VARIABLE-RATE
               PERFORM 2150-GET-CURRENT-RATE
               COMPUTE WS-PAYMENT ROUNDED = 
                   LF-BALANCE * 
                   (WS-MONTHLY-RATE * 
                    (1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS) /
                   ((1 + WS-MONTHLY-RATE) ** WS-NUM-PAYMENTS - 1)
           END-IF

           MOVE WS-PAYMENT TO LF-PAYMENT-AMOUNT.

       2150-GET-CURRENT-RATE.
      * In production, this would fetch from rate table
           CONTINUE.

      *****************************************************************
      * CALCULATE-LATE-FEE
      * Business Rule: Late fee = balance * 1.5% * days late
      * Premium accounts receive 50% reduction
      * Maximum fee capped at $500
      *****************************************************************
       2200-CALCULATE-LATE-FEE.
           IF WS-DAYS-LATE > 0
      * Calculate base fee
               COMPUTE WS-LATE-FEE = 
                   LF-BALANCE * WS-LATE-FEE-RATE * WS-DAYS-LATE

      * Premium accounts get 50% discount
               IF PREMIUM-ACCOUNT
                   COMPUTE WS-LATE-FEE = WS-LATE-FEE * 0.50
               END-IF

      * Cap at maximum
               IF WS-LATE-FEE > WS-MAX-LATE-FEE
                   MOVE WS-MAX-LATE-FEE TO WS-LATE-FEE
               END-IF
           ELSE
               MOVE 0 TO WS-LATE-FEE
           END-IF.

      *****************************************************************
      * UPDATE-BALANCE
      * Business Rule: Split payment into principal and interest
      * Interest = Balance * Monthly Rate
      * Principal = Payment - Interest
      * New Balance = Old Balance - Principal
      *****************************************************************
       2300-UPDATE-BALANCE.
      * Calculate interest portion
           COMPUTE WS-INTEREST-PART ROUNDED = 
               LF-BALANCE * WS-MONTHLY-RATE

      * Calculate principal portion
           COMPUTE WS-PRINCIPAL-PART = 
               LF-PAYMENT-AMOUNT - WS-INTEREST-PART

      * Ensure principal doesn't exceed balance
           IF WS-PRINCIPAL-PART > LF-BALANCE
               MOVE LF-BALANCE TO WS-PRINCIPAL-PART
           END-IF

      * Update remaining balance
           COMPUTE LF-BALANCE = 
               LF-BALANCE - WS-PRINCIPAL-PART

      * Handle negative balance (overpayment)
           IF LF-BALANCE < 0
               MOVE 0 TO LF-BALANCE
           END-IF

      * Write updated record
           REWRITE LOAN-RECORD
               INVALID KEY
                   MOVE 'ERROR UPDATING LOAN RECORD' 
                       TO WS-ERROR-MESSAGE
                   SET HAS-ERROR TO TRUE
           END-REWRITE.

       9000-CLEANUP.
           CLOSE LOAN-FILE.
