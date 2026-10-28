      character*20 nome
100   format(A,I1,A,I1,A,I1,A)

      do k = 1, 3
         do i = 0, 1
            do j = 0, 1
               write(nome,100) 'tarefa-2a-r-',k,i,j,'.dat'               
               open(k*10+2*i+j, file=nome)
               close(k*10+2*i+j)
            enddo
         enddo
      enddo
      end