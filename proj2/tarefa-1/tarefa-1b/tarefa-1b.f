      
      
      open(10,file='tarefa-1b-saida-1.out')

      do m=1,9
        avg=0 !<r>
        avg2=0 !<r²>
        aavg=0 !<<r>> 
        aavg2=0 !<<r>²>
        n=3**m
        do i=1,100
            iseed=i
            call srand(iseed)
            do j=1,n
                r=rand()
                avg=avg+r
                avg2=avg2+(r*r)
            enddo
            avg=avg/n
            avg2=avg2/n
            aavg=aavg+avg
            aavg2=aavg2+(avg*avg)
        enddo
        aavg=aavg/100
        aavg2=aavg2/100
        write(*,*) aavg, aavg*aavg, aavg2, sqrt(aavg2-(aavg*aavg))
        sd=sqrt(aavg2-(aavg*aavg))
        write(10,*) sd, n
      enddo

      close(10)
      end