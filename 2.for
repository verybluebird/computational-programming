      IMPLICIT none
      REAL minx, maxx, deltax, miny, maxy, deltay
      COMMON /var/ minx, maxx, deltax, miny, maxy, deltay
      CALL input()
      CALL table()
      END
      
      
      SUBROUTINE input()
       IMPLICIT none
       REAL minx, maxx, deltax, miny, maxy, deltay
       LOGICAL input_chek
       COMMON /var/ minx, maxx, deltax, miny, maxy, deltay
       OPEN (1, FILE='input.txt', ERR = 1)
       READ(1,*, ERR = 2)minx,maxx,deltax,miny,maxy,deltay
       CLOSE (1)
       IF ( input_chek()) THEN
        GOTO 3
       ELSE 
        GOTO 2
       END IF
            
       
    1  PRINT *, 'Error: cant open file'
        GOTO 3
    2  PRINT *, 'Error: incorrect input'
        GOTO 3
    3 END
      
     
      LOGICAL FUNCTION input_chek()
       IMPLICIT none
       REAL minx, maxx, deltax, miny, maxy, deltay, lim
       COMMON /var/ minx, maxx, deltax, miny, maxy, deltay
       input_chek = .FALSE.
       lim = 1E-38
       IF(minx.LE.maxx.AND.minx+deltax.LE.maxx.AND.
     > miny.LE.maxy.AND.miny+deltay.LE.maxy .AND.
     > lim .LE. ABS(deltax) .AND. lim .LE. ABS(deltay) )THEN
            input_chek = .TRUE.
       END IF
      END 
      
      LOGICAL FUNCTION xy_check(x,y)
       IMPLICIT none
       REAL x,y
       IF (ABS(x+y) .LT. 1D-5)THEN
        xy_check = .FALSE.
       ELSE 
        xy_check = .TRUE.
       END IF
      END 
      
      REAL FUNCTION f(x,y)
       IMPLICIT none
       REAL x,y
       f = 1/(x+y)
      END
       
      SUBROUTINE table()
      
       IMPLICIT none
  10   FORMAT(/1x\)
  1    FORMAT(15('-')\)
  2    FORMAT('|     INF     '\)
  3    FORMAT('     y\x     '\)
  4    FORMAT(E12.4\, ' ')
  5    FORMAT('|', E12.4\, ' ')
       REAL minx, maxx, deltax, miny, maxy, deltay
       REAL i, j,  i_next,  j_next, f
       COMMON /var/ minx, maxx, deltax, miny, maxy, deltay
       
       OPEN(2, FILE = 'output.txt', STATUS = 'NEW')
       WRITE (2,3)!x/y
       i = minx
       i_next = i
       DO WHILE (i_next .LT. maxx)       
        WRITE (2,5)i_next ! |x1 |x2 
        i_next = i_next + deltax
       END DO
       WRITE (2,5) maxx   ! |xmax   
       WRITE (2,10) ! \n
      
        
       
       i = minx
       i_next = i
       
       DO WHILE (i_next .LT. maxx)
        WRITE (2,1)! ----- under x
        i_next = i_next + deltax
       END DO
       
       WRITE (2,1) ! -------
       WRITE (2,10) ! \n
       j = miny
       j_next = j
       DO WHILE (j_next .LT. maxy)        
        WRITE(2,10)! \n
        WRITE (2,4)j_next ! yj 
        
        i = minx
        i_next = i
            DO WHILE (i_next .LT. maxx)
               IF (ABS(i_next + j_next) .LT. 1E-38)THEN 
                    WRITE (2,2) !inf
               ELSE  
                    WRITE (2,5)f(i_next, j_next) !|f(xi,yj)
                    
               END IF
               i_next = i_next + deltax
            END DO
        j_next = j_next + deltay 
        
        IF (ABS(maxx + j_next) .LT.1E-38)THEN 
                    WRITE (2,2) !inf
        ELSE  
                    
                    WRITE (2,5)f(maxx, j_next) !|f(xmax,yj)
        END IF
        
        WRITE(2,10)! \n
        WRITE (2,1)! ----- under x
        i = minx
        i_next = i
        
        DO WHILE (i_next .LT. maxx)
         WRITE (2,1)! ----- under x
         i_next = i_next + deltax
        END DO
        WRITE(2,10)! \n
        
         
       END DO
       WRITE (2,10) ! \n
       WRITE (2,4)maxy ! ymax
        i = minx
        i_next = i
            DO WHILE (i_next .LT. maxx)
                IF (ABS(i_next+ maxy) .LT.1E-38)THEN 
                    WRITE (2,2) !inf
                ELSE  
                    WRITE (2,5)f(i_next, maxy) !|f(xi,ymaxj)
                END IF
              
               i_next = i_next + deltax
            END DO
        j_next = j_next + deltay 
        IF (ABS(maxx + maxy) .LT. 1E-38)THEN 
                    WRITE (2,2) !inf
        ELSE  
                   
                    WRITE (2,5)f(maxx, maxy) !|f(xmax,maxy)
        END IF
        
        WRITE(2,10)! \n
       
      END
       
