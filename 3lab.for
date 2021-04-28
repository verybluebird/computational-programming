      PROGRAM matr
        IMPLICIT NONE
        COMMON mem (1000000)
        COMMON /sizes/ N, M, alSize
        COMMON /mem/ memorySize
        INTEGER N, M, alSize, memorySize
        REAL mem
        memorySize = 1000000
        


        CALL openMatrixVector(mem)

        IF (N .NE. M) THEN
            PAUSE 'Error: incorrect sizes!'
            STOP
        END IF

        CALL mult(mem(1),mem(N+2),mem(2*N+3),mem(2*N+4+alSize),
     > mem(3*N+5+alSize))
        CALL output(mem)
       
       END


      SUBROUTINE openMatrixVector(mem)
       IMPLICIT NONE
       COMMON /sizes/ N,M, alSize
       COMMON /mem/ memorySize
       INTEGER N, M, alSize, memorySize, I
       REAL mem
       DIMENSION mem(*)

       OPEN (10, FILE = 'al.txt', err=1)
       OPEN (20, FILE = 'di.txt', err=1)
       OPEN (30, FILE = 'ia.txt', err=1)
       OPEN (40, FILE = 'msize.txt', err=1)
       OPEN (50, FILE = 'vsize.txt', err=1)
       OPEN (60, FILE = 'b.txt', err=1)

       READ (40, *) N

       IF (N .lt. memorySize)THEN
            READ (50, *) M
            READ (30, *, end = 5) (mem(i), i=1, N+1)!ia
   5        alSize = mem(N+1) - 1
            CALL checkData(alSize)

            READ (20,*,end=6) (mem(i),i = N+2, 2*N+1) !DI
   6        READ (10,*,end=7) (mem(i),i = 2*N+2, 2*N+1+alSize) !AL

   7        READ (60,*,end=8) (mem(i),i = 2*N+2+alSize , 2*N+1+alSize+M) !VEC
       ELSE
            PAUSE 'Not enough memory'
            STOP
       END IF

   2   CLOSE (2)
       CLOSE (1)
       CLOSE (3)
       CLOSE (4)
       CLOSE (2)
       CLOSE (1)
       GOTO 8
       
   1   PAUSE 'Error opening file!'
       STOP    


   8  END



      SUBROUTINE output(mem)
        IMPLICIT NONE
        COMMON /sizes/ N,M, alSize
        DIMENSION mem(*)
        INTEGER N, M, alSize, memorySize, i
        REAL mem
   10   FORMAT (F9.2\)

        OPEN (1, file = 'result.txt', err=20)
        WRITE(1,10) (mem(i),i=2*N+1+alSize+M+1,2*N+1+alSize+M+N)
        GOTO 30
        
   20   PAUSE 'Error opening file!'
        STOP


   30 END

      SUBROUTINE mult(ia, di, al, B, C)
        IMPLICIT NONE
        COMMON /sizes/ N,M, alSize
        INTEGER N, M, alSize, memorySize, I, IAA, IAB, K, ia, J
        REAL C, U, Z, B, di, al
        DIMENSION C(*), ia(*),di(*),al(*),B(*)
        
        DO   I=1,N
         C(I)=di(I)*B(I)
        END DO 
        DO   I=1,N
            IAA = ia (I) 
            IAB = ia(I+1) - 1 
            IF (IAB .LT. IAA) CONTINUE
            U = C(I)
                Z = B(I)
                DO  K=IAA, IAB
                    J=ia(K)
                    U=U+al(K)*B(J)
                    C(J) = C(J) + al(K)*Z
                END DO
                C(I) = U
      
        END DO

      END



      SUBROUTINE checkData(X)
        IMPLICIT NONE
        COMMON /sizes/ N,M, alSize
        COMMON /mem/ memorySize
        INTEGER N, M, alSize, memorySize, X

        IF ( M .LT. 1 .OR. M .NE. N) THEN
            PAUSE 'Not enough memory!'
            STOP
        END IF

        IF ( (2*N +3*M +1+X) .GT. memorySize) THEN
            PAUSE 'Not enough memory!'
            STOP
        END IF

        GOTO 2
    


    2 END

