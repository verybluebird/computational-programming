      !Matrix-vector multiplication program
      PROGRAM matr
        COMMON mem (1000000)
        COMMON /sizes/ N, M, N_al
        COMMON /mem/ memorySize


        memorySize = 1000000
        CALL readSize()
        CALL openMatrixVec(mem(1),mem(N+2))

        CALL mult(mem(1),mem(N+2),mem(2*N+2),mem(2*N+2+N_al),
     > mem(3*N+5+N_al))

      END

      SUBROUTINE readSize()
       COMMON /sizes/ N,M, N_al
       COMMON /mem/ memorySize
       OPEN (40, FILE = 'msize.txt', err=1)
       OPEN (50, FILE = 'vsize.txt', err=1)
       READ (40, *) N
       READ (50, *) M



       IF ( M .LT. 1 .OR. M .NE. N) THEN
            PAUSE 'Wrong input!'
            STOP
       END IF

        CLOSE (40)
        CLOSE (50)

        GOTO 2
   1    PAUSE 'Error opening file!'
        STOP


   2  END

      SUBROUTINE openMatrixVec(ia, A)

       COMMON /sizes/ N,M, N_al
       COMMON /mem/ memorySize
       DIMENSION ia(*), A(*)

       OPEN (10, FILE = 'al.txt', err=1)
       OPEN (20, FILE = 'di.txt', err=1)
       OPEN (30, FILE = 'ia.txt', err=1)
       OPEN (60, FILE = 'b.txt', err=1)



       IF (N .lt. memorySize)THEN

            READ (30, *, end=5) (ia(i), i=1, N+1)!ia
   5        N_al = ia(N+1) - 1

            READ (20,*,end=6) (A(i),i = 1, N) !DI
   6        READ (10,*,end=7) (A(i),i = N+1, N+ N_al) !AL

   7        READ (60,*,end=2) (A(i),i = N+ N_al+ 1  ,2*N+ N_al) !VEC
       ELSE
            PAUSE 'Not enough memory'
            STOP
       END IF

   2   CLOSE (10)
       CLOSE (20)
       CLOSE (30)
       CLOSE (60)

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


        OPEN (1, file = 'result.txt', err=1)
        WRITE(1,10) (res(i),i=1,N)
        CLOSE(1)
        GOTO 30

   1    PAUSE 'Error opening file!'
        STOP

   10   FORMAT(E10.4\, ' ')


   30 END

      SUBROUTINE mult(ia, di, al, v, res)
        IMPLICIT NONE
        COMMON /sizes/ N,M, N_al

        DIMENSION  ia(*), di(*), al(*), v(*), res(*)
        INTEGER ia, N,M, N_al, i, j, k, i_start
        REAL di, al, v, res



        DO   i=1,N
         res(i)=di(i)*v(i)

        END DO
        DO   i=1,N
            i_start = i - ia (i+1)
            DO j = ia(i), ia (i+1) - 1
                k = i_start +j
                res(k) = res(k) + al(j)*v(i)

                res(i) = res(i) + al(j)*v(k)

            END DO
        END DO

        CALL output(res)

      END




