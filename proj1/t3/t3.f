      real*8 a(1003), b
      n=1

      open(unit=10,file='input')
      open(unit=11,file='output')

      write(*,*) 'M = ?'
      read(*,*) m

10    read(10,*,end=20) a(n)
            if (n.eq.m) then
                  call b_h(a,m)
            endif
            if (n.gt.m) then
                  if (a(n).lt.a(1)) then
                        b = a(1)
                        a(1) = a(n)
                        a(n) = b
                        call h_d(a,1,m)
                  endif
            endif
            n=n+1
            goto 10

20    write(11,*) 'N = ', n-1

      do k=m,2,-1
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
	do i=m/2,1,-1
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