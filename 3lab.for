      PROGRAM matr
        IMPLICIT NONE
        COMMON mem (1000000)
        COMMON /sizes/ N, M, N_al
        COMMON /mem/ memorySize
        INTEGER  N,M, N_al, memorySize, mem
      
        
        memorySize = 1000000
        


        CALL openMatrixVec(mem(1),mem(N+2),mem(2*N+3),mem(2*N+4+N_al))

        IF (N .NE. M) THEN
            PAUSE 'Error: incorrect sizes!'
            STOP
        END IF

        CALL mult(mem(1),mem(N+2),mem(2*N+3),mem(2*N+4+N_al),
     > mem(3*N+5+N_al))
        CALL output(mem)
       
       END


      SUBROUTINE openMatrixVec(ia, di, al, vec)
       IMPLICIT NONE
       
       COMMON /sizes/ N,M, N_al
       COMMON /mem/ memorySize
       DIMENSION ia(*), di(*), al(*), vec(*)
       INTEGER ia, N,M, N_al, i, memorySize
       REAL di, al, vec

       OPEN (10, FILE = 'al.txt', err=1)
       OPEN (20, FILE = 'di.txt', err=1)
       OPEN (30, FILE = 'ia.txt', err=1)
       OPEN (40, FILE = 'msize.txt', err=1)
       OPEN (50, FILE = 'vsize.txt', err=1)
       OPEN (60, FILE = 'b.txt', err=1)

       READ (40, *) N

       IF (N .lt. memorySize)THEN
            READ (50, *) M
            READ (30, *) (ia(i), i=1, N+1)!ia
   5        N_al = ia(N+1) - 1

            READ (20,*,end=6) (di(i),i = 1, N) !DI
   6        READ (10,*,end=7) (al(i),i = 1, N_al) !AL

   7        READ (60,*,end=8) (vec(i),i = 1  , M) !VEC
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



      SUBROUTINE output(res)
        IMPLICIT NONE
        COMMON /sizes/ N,M, N_al
        DIMENSION res(*)
        INTEGER N,M, N_al, i
        REAL res
        
   
        OPEN (1, file = 'result.txt', err=20)
        WRITE(1,10) (res(i),i=1,N)
        CLOSE(1)
        GOTO 30
        
   20   PAUSE 'Error opening file!'
        STOP
                  
   10   FORMAT(E10.4\, ' ')
                                                                                                                                                                                                                                             

   30 END

      SUBROUTINE mult(ia, di, al, v, res)
        IMPLICIT NONE
        COMMON /sizes/ N,M, N_al
        
        DIMENSION  ia(*), di(*), al(*), v(*), res(*)
        INTEGER ia,  N,M, N_al, i, j, k, a, i_start
        REAL di, al, v, res, h, s, d 
        
        DO   i=1,N
         res(i)=di(i)*v(i)
         s=di(i)
         h=v(i)
         a = res(i)
        END DO 
        DO   i=1,N
             a = ia(i+1)
            i_start = i - ia (i+1)
            DO j = ia(i), ia (i+1) - 1
                k = i_start +j
                res(k) = res(k) + al(j)*v(i)
                d = res(k)
                res(i) = res(i) + al(j)*v(k) 
                a = res (i) 
            END DO
        END DO
        
        CALL output(res)

      END



      SUBROUTINE checkData(X)
        
        COMMON /sizes/ N,M, N_al
        COMMON /mem/ memorySize
        
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

