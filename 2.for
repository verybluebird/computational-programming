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
      
      
      
      REAL FUNCTION round (x)
       IMPLICIT NONE
       REAL x
       CHARACTER*12 x4
      
       WRITE (x4, 6 ) x
       READ (x4, *) round
   6   FORMAT(E12.4) 
      END
      
      
      
      
       
      SUBROUTINE table()
      
     
       
      
       IMPLICIT none
  10   FORMAT(/1x\)
  1    FORMAT(14('-')\)
  2    FORMAT('|     INF     '\)
  3    FORMAT('     y\x     '\)
  4    FORMAT(E12.4\, ' ')
  5    FORMAT('|', E12.4\, ' ')
       REAL minx, maxx, deltax, miny, maxy, deltay
       REAL i, j,  i_next,  j_next, round, i_tmp, j_tmp
       COMMON /var/ minx, maxx, deltax, miny, maxy, deltay
       
       OPEN(2, FILE = 'output.txt', STATUS = 'NEW')
       WRITE (2,3)!x/y
       i = minx
       i_tmp = i
       
       DO WHILE (i .LT. maxx)
        i_next = i + deltax
            IF (ABS(i_tmp - round(i_next)).GE. 1E-4) THEN       
                WRITE (2,5)i_tmp ! |x1 |x2 
                i_next = round (i_next)
                i_tmp = i_next      
            ENDIF
            i = i_next
        END DO
       
       WRITE (2,5) maxx   ! |xmax   
       WRITE (2,10) ! \n
       
       i = minx
       i_tmp = i
       WRITE (2,1)! ----- 
       
       DO WHILE (i .LT. maxx)
        i_next = i + deltax
            IF (ABS(i_tmp - round(i_next)).GE. 1E-4) THEN       
                 WRITE (2,1)! ----- under x
                i_next = round (i_next)
                i_tmp = i_next      
            ENDIF
            i = i_next
        END DO
       
       WRITE (2,1) ! -------
       WRITE (2,10) ! \n
       
       j = miny
       j_tmp = j
       
       DO WHILE (j .LT. maxy) 
        j_next = j + deltay
          IF (ABS(j_tmp - round(j_next)).GE. 1E-4) THEN            
            WRITE(2,10)! \n
            WRITE (2,4)j_tmp ! yj 
            
            i = minx
            i_tmp = i
            DO WHILE (i .LT. maxx)
               i_next = i + deltax
               IF (ABS(i_tmp - round(i_next)).GE. 1E-4) THEN       
                    IF (ABS(i_tmp + j_tmp) .LT. 1E-38)THEN 
                        WRITE (2,2) !|inf
                    ELSE  
                        
                        WRITE (2,5)1/(i_tmp+ j_tmp) !|f(xi,yj)    
                    END IF
                    
                    i_next = round(i_next)
                    i_tmp = i_next
               END IF
               i = i_next  
            END DO 
        
                IF (ABS(maxx + j_tmp) .LT.1E-38)THEN 
                    WRITE (2,2) !inf
                ELSE  
                    WRITE (2,5)1/(maxx+ j_tmp) !|f(xmax,yj)
                END IF
            j_next = round(j_next)
            j_tmp = j_next
          END IF
         j = j_next
        END DO
            
       WRITE (2,10) ! \n
       WRITE (2,4)maxy ! ymax
        i = minx
        i_tmp = i
         DO WHILE (i .LT. maxx)
            i_next = i + deltax
                IF (ABS(i_tmp - round(i_next)).GE. 1E-4) THEN       
                    IF (ABS(i_tmp + maxy) .LT.1E-38)THEN 
                        WRITE (2,2) !inf
                    ELSE
                        WRITE (2,5)1/(i_tmp+ maxy) !|f(xi,ymaxj)
                    ENDIF
                    i_next = round(i_next)
                    i_tmp = i_next      
                ENDIF
            i = i_next
         END DO
        
       IF (ABS(maxx + maxy) .LT. 1E-38)THEN 
                   WRITE (2,2) !inf
        ELSE  
                   WRITE (2,5)1/(maxx+ maxy) !|f(xmax,maxy)
       END IF
      
       
      END
       
