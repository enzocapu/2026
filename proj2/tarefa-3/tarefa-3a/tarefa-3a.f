      count=0 !contador posição
      avg=0 !<x>
      aavg=0 !<x^2>
      n=0 !contador média
      x=0

      do k=1,10000
        iseed=k
        call srand(iseed)
        do i= 1,200
            dx = 2e0*rand()-1e0
            x = x + dx
            avg=avg+x
            aavg=aavg+x*x
            n=n+1e0
            if (x.gt.0.5e0) then
                count=count+1e0
            else
                count=count-1e0
            endif
            avg=avg/n
            aavg=aavg/n
        enddo
      enddo      
      
      write(*,*) 'avg=',avg
      write(*,*) 'aavg=',aavg
      write(*,*) 'x=',count

      end