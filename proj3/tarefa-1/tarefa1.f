      implicit real*8 (a-h,o-z)
      dimension e(6),e0(6),et(6)
      f(x) = exp(x*x)*cos(x+x)/sin(x+x)
      ft(x) = 2*exp(x*x)*(x/tan(x+x)-1/(sin(x+x)**2)) !1derivada analitica
      dt(x)=((1+2*x**2)/tan(2*x)-4*x/sin(2*x)**2)
      fft(x)= 2*exp(x*x)*(4/(sin(2*x)**2 *tan(2*x))+dt(x)) !2derivada analitica

      t=0.1d0
      open(10,file='tarefa1.dat')

      do i=1,6 !preenche o vetor 'erro anterior'
        e0(i)=1
      enddo

      do i=1,12
        h=5d0**(-i)
        f2f = (f(t+h)-f(t))/h
        f2t = (f(t)-f(t-h))/h
        f3s = (f2f+f2t)/2
        f5s = (f(t-2*h)+8*(f(t+h)-f(t-h))-f(t+2*h))/(12*h)
        ff3s=(f(t+h)+f(t-h)-2*(f(t)))/(h*h)
        ff5s=(16*(f(t+h)+f(t-h))-(f(t+2*h)+f(t-2*h)+30*f(t)))/(12*h*h)
        write(10,*) h, f2f, f2t, f3s, f5s, ff3s, ff5s
        e(1)=abs(f2f-ft(t)) !vetor 'erro atual'
        e(2)=abs(f2t-ft(t))
        e(3)=abs(f3s-ft(t))
        e(4)=abs(f5s-ft(t))
        e(5)=abs(ff3s-fft(t))
        e(6)=abs(ff5s-fft(t))
        do k=1,6
            if(e(k).lt.e0(k)) then
                et(k)=i !acumula o índice de menor erro
            endif
            e0(k)=e(k)
        enddo
      enddo

      write(10,*) ft(t), fft(t)

      do i=1,6
        write(*,*) et(i)
      enddo
      
      close(10)

      end