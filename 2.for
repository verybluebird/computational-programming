      implicit none
      integer m,n
      real minx, maxx, deltax, miny, maxy, deltay
      common minx, maxx, deltax, miny, maxy, deltay
      common m,n
      call input()
      call calc_m_n()
      end
      
      
      subroutine input()
       implicit none
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       open (1,FILE='input.txt')
       read(1,*)minx,maxx,deltax,miny,maxy,deltay
       close (1)
      end
      
      subroutine calc_m_n()
       integer m,n
       real minx, maxx, deltax, miny, maxy, deltay
       common minx, maxx, deltax, miny, maxy, deltay
       common m,n
       m = (maxy-miny)/deltay
       if ((maxy - miny - m*deltay).GE. 0) 
       then m = m + 1
       end if
       
       n = (maxx-minx)/deltax
       if ((maxx - minx - n*deltax).GE. 0) 
       then n = n + 1
       end if
       
      end
      
      
      subroutine out()
        write (1,FILE='output.txt')table
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
      end
       
