       PROGRAM MAIN
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
      
        COMMON /grid/ N
        COMMON /interval/ a, b, h
    
        DIMENSION memory(100000000) 
        
        CALL readData()
        
        DO i = 0, 25
            N = 2**i
            h =(b - a) / N
            CALL makeGrid(memory(1))
            
            res1 = TrapezoidalQuadrature(memory(1))
            res2 = SimpsonQuadrature(memory(1))

         PRINT *, 'h: ', h      
        PRINT *, 'Trapezoidal: ', res1
         PRINT *, 'Simpson: ', res2
        END DO

        PAUSE
      END
      
      
      
      
      SUBROUTINE makeGrid(grid)
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /grid/ N
        COMMON /interval/ a, b, h 
        
        DIMENSION grid(*)
        
        DO i = 0, N
          grid(i) = a + i * h
        END DO
       ! IF (grid(N) .NE. b) THEN
        ! N = N + 1
         !grid(N) = b
        !END IF
        
        PRINT *, 'Grid created.'
        RETURN  
      END 
      
      REAL FUNCTION fun(x)
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        !fun = (x**0)/3
        !fun = (2*x)/3
        !fun = (3*x**2)/3
        !fun = (4*x**3)/3
        !fun = (5*x**4)/3
        !fun = (6*x**5)
        !fun = (7*x**6)/3
        !fun = (8*x**7)/3
        !fun = (9*x**8)/3
        !fun = (10*x**9)/3
        !fun = (11*x**10)/3
        fun = exp(x)*cos(10*x)
      END
      
       SUBROUTINE readData()
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /grid/ N
        COMMON /interval/ a, b, h
        
        OPEN(1, FILE = 'input.txt', STATUS = 'OLD', ERR = 1)
        READ(1, *) a, b!, N
        CLOSE(1)
        
        IF (b .LT. a .OR. a .EQ. b) GOTO 2
        !IF (N .EQ. 0.0) GOTO 3
        
       ! h = (b - a) / N
      
        PRINT *, 'Input data reading success.'
        RETURN  
    1   PAUSE 'Input error: "input.txt" file does not exist!'
        STOP 
    2   PAUSE 'Input error: invalid interval of integration!'
        STOP
    3   PAUSE 'Input error: zero step!'
        STOP
      END
      
      
      REAL FUNCTION TrapezoidalQuadrature(grid)
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /grid/ N
        COMMON /interval/ a, b, h
        REAL*8 sum8
        
        DIMENSION grid(*)
       
        
        sum = 0.0D0
        !sum8 = 0.0D0
        DO i = 1, N - 1
          
   
          sum = sum + fun(grid(i))*2*h
        END DO
        sum = sum + h*(fun(a)+ fun(b))
        sum = sum/2
        
        TrapezoidalQuadrature = sum
      END 
      
      
      REAL FUNCTION SimpsonQuadrature(grid)
        !IMPLICIT DOUBLE PRECISION(A-H, O-Z)
        COMMON /grid/ N
        COMMON /interval/ a, b, h
        
        DIMENSION grid(*)
       
        
        sum = 0.0D0
        DO i = 1, N 
          sum = sum + fun((grid(i)+ grid(i-1))/2)*h
        END DO
        sum = 4*sum
        DO i = 1, N-1 
          sum = sum + fun(grid(i))*2*h
        END DO
        sum = sum + h*(fun(a)+ fun(b))
        sum = sum/6
        SimpsonQuadrature = sum
      END 
