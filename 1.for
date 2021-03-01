    
      PROGRAM laba1
       IMPLICIT NONE
       REAL a, b, gamma, pi
       REAL alfa, beta, minA
       INTEGER number
       COMMON /var/ a, b, gamma, number, pi
       COMMON /angles/ alfa, beta, minA
       pi = 4 * atan (1.0)
       DO WHILE (number .NE. 5 )
        CALL menu()
        CALL input(number)
        SELECT CASE (number)
         CASE (1)
          CALL input_triangle (a, b, gamma)
         CASE (2)
          CALL area ()
         CASE (3)
          CALL min_angle ()
         CASE (4)   
          CALL cos_min_angle ()
         END SELECT
        END DO
      END
      
      SUBROUTINE input_triangle(a, b, gamma)
        REAL a, b, gamma
        IMPLICIT NONE
        PRINT *, 'Enter 2 sides'
        PRINT *, 'a,b: '
        READ *, a, b
        PRINT *, 'Enter angle between a and b in degrees'
        PRINT *, 'gamma: '
        READ *, gamma
      END
      
      SUBROUTINE area()
       IMPLICIT NONE
       REAL s
       COMMON /var/ a, b, gamma, number, pi
       s = a*b*sin(gamma*pi/180)/2
       print *, s
      END
      
      SUBROUTINE min_angle ()
       IMPLICIT NONE
       REAL alfa, beta
       COMMON /var/ a, b, gamma, number, pi
       COMMON /angles/ alfa, beta, minA
       c = sqrt ( a*a + b*b - 2*a*b*cos(gamma*pi/180))
       alfa = (acos ( (b - a*cos(gamma*pi/180))/ ( c ) ))*180/pi
       beta = 180 - alfa - gamma
       minA = min(alfa, beta, gamma)
       !minA = gamma 
       !if (alfa .LQ. beta .AND. alfa .LQ. gamma) then
       !minA = alfa
       !else if (beta .LQ. alfa .AND. beta .LQ. gamma) then
       !minA = beta 
       !else if (gamma .LQ. alfa .AND. gamma .LQ. beta) then 
       !minA = gamma
       print *, minA
       !END if  
       END
       
      SUBROUTINE cos_min_angle ()
       IMPLICIT NONE
       REAL minCos
       COMMON /var/ a, b, gamma, number, pi
       COMMON /angles/ alfa, beta, minA
       COMMON /cos/ minCos       
       PRINT *, 'Minimal angle:'
       CALL min_angle ()
       PRINT *, 'Minimal cos:'
       minCos = cos(minA * pi/180)
       PRINT *, minCos
      END
       
      SUBROUTINE menu()
        PRINT *, 'MENU'
        PRINT *, '1. Enter the new triangle'
        PRINT *, '2. Calculate the S of triangle'
        PRINT *, '3. Calculate minimum angle in degrees'
        PRINT *, '4. Calculate cos of the minimum angle'
        PRINT *, '5. Exit'
       END
       
      SUBROUTINE input(number)
       IMPLICIT NONE
       INTEGER number
       PRINT *, 'Select menu number: '
       READ *, number
      END
     
