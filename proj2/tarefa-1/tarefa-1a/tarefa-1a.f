      open(10,file='tarefa-1a-saida-1.out')

      do m=1,9
        avg=0
        avg2=0
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
            sd=sqrt(avg2-(avg*avg))
            write(10,*) avg, sd, n
        enddo
      enddo

      close(10)
      end