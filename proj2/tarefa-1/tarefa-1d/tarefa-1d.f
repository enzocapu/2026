      implicit real*8(a-h,o-z)
      
      iseed=777
      call srand(iseed)
      f1=0d0 !integral i
      f2=0d0 !integral ii
      count1=0d0 !contador de iteraçoes
      count2=0d0
      
      do i=1,1650000
        x=dble(rand())
        f1=f1+4d0/(x*x+1d0)
        count1=count1+1d0
      enddo

      
      do i=1,4300000
            y=1d0+9d0*dble(rand())
            f2=f2+1d0/y
            count2=count2+1d0
      enddo
      

      f1=f1/count1
      f2=9d0*f2/count2

      write(*,*) 'count1=',count1,'count2=',count2
      write(*,*) 'i=', f1
      write(*,*) 'ii=', f2

      end