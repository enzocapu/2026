      implicit real*8(a-h,o-z)
      
      iseed=777
      call srand(iseed)
      f1=0d0 !integral i
      f2=0d0 !integral ii
      count1=0d0 !contador de iteraçoes
      count2=0d0
      
      !do i=1,50000000
       ! x=dble(rand())
        !f1=f1+4d0/(x*x+1d0)
        !count1=count1+1d0
      !enddo

      
    
10      y=1d0+9d0*dble(rand())
        f2=f2+1d0/y
        count2=count2+1d0
        ff2=9d0*f2/count2
        if(dabs(dlog(10d0)-ff2).lt.1d-3) goto 77
        goto 10
      

      !f1=f1/count1

      !write(*,'(A,F12.3)') 'i=', f1
77    write(*,*) 'count2=',count2
      write(*,*) 'ii=', ff2

      end