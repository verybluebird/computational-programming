      implicit none
      real minx, maxx, deltax, miny, maxy, deltay
      common minx, maxx, deltax, miny, maxy, deltay
      
      
      
      
      subroutine input()
       implicit none
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       open (1,FILE=’input.txt’)
       read (1,FILE=’input.txt’) minx, maxx, deltax, 
    & miny, maxy, deltay
       close (1)
      end
      
      subroutine calc_m_n()
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       common m,n
      end
      
      
      subroutine out()
        write (1,FILE=’output.txt’)table
      end
      
      logical function input_chek()
       implicit none
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       if (minx .LE. maxx .AND. minx+deltax .LE. maxx .AND. 
    &      miny .LE. maxy .AND. miny+deltay .LE. maxy ) 
    &   input_chek = .TRUE.
      end 
      
      real function f(x,y)
       implicit none
       real x,y
       f = 1/(x+y)
      end
       
      logical function table()
       implicit none
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       
       OPEN(2, FILE = 'output.txt', STATUS = 'NEW', ERR = 1)
       
       
