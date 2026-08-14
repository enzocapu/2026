      real*8 a(500001), b
      n=1

      open(unit=10,file='tarefa-3-entrada-1.in')
      open(unit=11,file='tarefa-3-saida-1.out')

      write(*,*) 'M = ?'
      read(*,*) m
      write(11,*) 'M = ', m

10    read(10,*,end=20) a(n)
            if (n.eq.m) then !aplica-se a estrutura ao atingir m
                  call b_h(a,m)
            endif
            if (n.gt.m) then
                  if (a(n).lt.a(1)) then !confirma se o elemento lido é menor que a raiz
                        b = a(1)
                        a(1) = a(n)
                        a(n) = b
                        call h_d(a,1,m) !reorganiza a estrutura
                  endif
            endif
            n=n+1
            goto 10

20    write(*,*) 'N = ', n-1

      do k=m,2,-1 !ordena o heap em ordem crescente
            b = a(1)
            a(1) = a(k)
            a(k) = b
            call h_d(a,1,k-1)
      enddo

      do i=1,m
            write(11,*) a(i)
      enddo

      close(10)
      close(11)
      end

	subroutine b_h(a,m)
	real*8 a(*)
	do i=m/2,1,-1 !constrói o heap do último nó até a raiz
		call h_d(a,i,m)
	enddo
	return
	end
	
	subroutine h_d(a,j,m)
	real*8 a(*), b
      i=j
50    continue
	il=2*i !indice esquerdo
	ir=2*i+1 !indice direito
	ix=i !indice do maior (inicialmente, o nó)
	if (il.le.m) then
		if (a(il).gt.a(ix)) ix=il !esquerdo>maior?
	endif
	if(ir.le.m) then
		if (a(ir).gt.a(ix)) ix=ir !direito>maior?
	endif
	if(ix.eq.i) then !maior = nó?
		goto 40
	endif
	b = a(i)
	a(i) = a(ix)
	a(ix) = b
	i=ix
	goto 50
40    return
	end  	