      pi=4.0*atan(1.0)
      s=1 !definindo a série
      j=1 !definindo o fatorial
      isinal=-1
      write(*,*) 'x= ?'
      read(*,*) x
      c=cos(x)

      x=amod(x,2.0*pi) !calcula-se o resto
      if (x.gt.pi)  x=x-2.0*pi !traz x pro intervalo [-\pi,\pi]
      if (x.lt.-pi) x=x+2.0*pi

      do i=2,40,2
            j=i*(i-1)*j
            b=isinal*(x**i)/j
            if (abs(b).le.0.5e-5) goto 77
            s=s+b
            isinal=isinal*(-1)            
      enddo

77    write(*,10) '||sum=',s,'cos(x)=',c,'abs(sum-cos(x))=',abs(s-c)
10    format(3(A,F8.5,'||'))      
      
      end