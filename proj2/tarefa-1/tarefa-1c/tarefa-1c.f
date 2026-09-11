      real*8 count,total,x,y,r,pi

      iseed = 777
      call srand(iseed)
      count=0d0
      total=0d0

      do i=1,150000000
        total=total+1d0
        x=dble(rand())
        y=dble(rand())
        x=2d0*x-1d0
        y=2d0*y-1d0
        r=x*x+y*y
        if (r.le.1d0) then
            count=count+1d0
        endif
      enddo

      pi=4d0*count/total

      write(*,*) count,total
      write(*,'(A,F12.4)') 'pi=',pi

      end