C     COMPOUND INTEREST CALCULATOR
C     FORTRAN 77 EXAMPLE FOR MIGRATIONPILOT
C
      PROGRAM INTEREST
      
      IMPLICIT NONE
      
C     Variable declarations
      REAL PRINCIPAL, RATE, YEARS
      REAL COMPOUND, SIMPLE, CONTINUOUS
      REAL MONTHLY_RATE, NUM_PERIODS
      INTEGER N, I
      REAL TABLE(100,4)
      
C     Constants
      REAL PI
      PARAMETER (PI = 3.14159265359)
      
      PRINT *, 'COMPOUND INTEREST CALCULATOR'
      PRINT *, '============================'
      PRINT *
      
C     Input values
      PRINT *, 'Enter Principal Amount:'
      READ *, PRINCIPAL
      
      PRINT *, 'Enter Annual Interest Rate (as decimal, e.g. 0.05):'
      READ *, RATE
      
      PRINT *, 'Enter Number of Years:'
      READ *, YEARS
      
      PRINT *, 'Enter Compounding Frequency (per year):'
      READ *, N
      
C     Calculate different interest types
      CALL CALC_COMPOUND(PRINCIPAL, RATE, YEARS, N, COMPOUND)
      CALL CALC_SIMPLE(PRINCIPAL, RATE, YEARS, SIMPLE)
      CALL CALC_CONTINUOUS(PRINCIPAL, RATE, YEARS, CONTINUOUS)
      
C     Print results
      PRINT *
      PRINT *, 'RESULTS'
      PRINT *, '======='
      PRINT 100, 'Simple Interest:', SIMPLE
      PRINT 100, 'Compound Interest:', COMPOUND
      PRINT 100, 'Continuous Interest:', CONTINUOUS
      
100   FORMAT(A20, F12.2)
      
C     Generate amortization table
      PRINT *
      PRINT *, 'AMORTIZATION TABLE'
      PRINT *, '=================='
      
      CALL AMORTIZE(PRINCIPAL, RATE, INT(YEARS*12), TABLE)
      
      PRINT '(A6, A12, A12, A12, A12)', 
     &      'Month', 'Payment', 'Principal', 'Interest', 'Balance'
      PRINT *, '----------------------------------------------'
      
      DO 200 I = 1, MIN(INT(YEARS*12), 12)
          PRINT '(I6, 4F12.2)', I, TABLE(I,1), TABLE(I,2), 
     &          TABLE(I,3), TABLE(I,4)
200   CONTINUE
      
      STOP
      END

C     ================================================================
C     SUBROUTINE: CALC_COMPOUND
C     Business Rule: A = P(1 + r/n)^(nt)
C     Where:
C       A = Final amount
C       P = Principal
C       r = Annual interest rate
C       n = Number of times interest is compounded per year
C       t = Number of years
C     ================================================================
      SUBROUTINE CALC_COMPOUND(P, R, T, N, RESULT)
      IMPLICIT NONE
      REAL P, R, T, RESULT
      INTEGER N
      
      RESULT = P * (1.0 + R/REAL(N)) ** (REAL(N) * T)
      
      RETURN
      END

C     ================================================================
C     SUBROUTINE: CALC_SIMPLE
C     Business Rule: A = P(1 + rt)
C     Simple interest formula
C     ================================================================
      SUBROUTINE CALC_SIMPLE(P, R, T, RESULT)
      IMPLICIT NONE
      REAL P, R, T, RESULT
      
      RESULT = P * (1.0 + R * T)
      
      RETURN
      END

C     ================================================================
C     SUBROUTINE: CALC_CONTINUOUS
C     Business Rule: A = Pe^(rt)
C     Continuous compounding formula
C     ================================================================
      SUBROUTINE CALC_CONTINUOUS(P, R, T, RESULT)
      IMPLICIT NONE
      REAL P, R, T, RESULT
      
      RESULT = P * EXP(R * T)
      
      RETURN
      END

C     ================================================================
C     SUBROUTINE: AMORTIZE
C     Business Rule: Calculate monthly payment and amortization
C     Payment = P * [r(1+r)^n] / [(1+r)^n - 1]
C     Where:
C       P = Principal
C       r = Monthly interest rate
C       n = Total number of payments
C     ================================================================
      SUBROUTINE AMORTIZE(PRINCIPAL, ANNUAL_RATE, NUM_MONTHS, TABLE)
      IMPLICIT NONE
      REAL PRINCIPAL, ANNUAL_RATE
      INTEGER NUM_MONTHS
      REAL TABLE(100,4)
      
      REAL MONTHLY_RATE, PAYMENT, BALANCE
      REAL INTEREST_PART, PRINCIPAL_PART
      INTEGER I
      
C     Calculate monthly rate
      MONTHLY_RATE = ANNUAL_RATE / 12.0
      
C     Calculate monthly payment using amortization formula
      IF (MONTHLY_RATE .GT. 0.0) THEN
          PAYMENT = PRINCIPAL * 
     &        (MONTHLY_RATE * (1.0 + MONTHLY_RATE)**NUM_MONTHS) /
     &        ((1.0 + MONTHLY_RATE)**NUM_MONTHS - 1.0)
      ELSE
          PAYMENT = PRINCIPAL / REAL(NUM_MONTHS)
      END IF
      
      BALANCE = PRINCIPAL
      
C     Generate table
      DO 300 I = 1, MIN(NUM_MONTHS, 100)
C         Calculate interest portion
          INTEREST_PART = BALANCE * MONTHLY_RATE
          
C         Calculate principal portion
          PRINCIPAL_PART = PAYMENT - INTEREST_PART
          
C         Handle final payment
          IF (PRINCIPAL_PART .GT. BALANCE) THEN
              PRINCIPAL_PART = BALANCE
              PAYMENT = PRINCIPAL_PART + INTEREST_PART
          END IF
          
C         Update balance
          BALANCE = BALANCE - PRINCIPAL_PART
          
C         Handle rounding errors
          IF (BALANCE .LT. 0.01) THEN
              BALANCE = 0.0
          END IF
          
C         Store in table
          TABLE(I, 1) = PAYMENT
          TABLE(I, 2) = PRINCIPAL_PART
          TABLE(I, 3) = INTEREST_PART
          TABLE(I, 4) = BALANCE
          
300   CONTINUE
      
      RETURN
      END
