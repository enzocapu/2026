      real*8 pi,s,c,x,b,j
      integer i,isinal
      
      pi=4.d0*datan(1.d0)
      s=1.d0
      j=1.d0
      isinal=-1

      write(*,*) 'x= ?'
      read(*,*) x

      c=dcos(x)

      x=dmod(x,2.d0*pi)
      if (x.gt.pi)  x=x-2.d0*pi
      if (x.lt.-pi) x=x+2.d0*pi      

      do i=2,40,2
            j=i*(i-1)*j
            b=isinal*(x**i)/j
            if (dabs(b).le.0.5d-15) goto 77
            s=s+b
            isinal=-isinal
      enddo

77    write(*,10) '||sum=',s,'cos(x)=',c,'abs(sum-dcos(x))=',dabs(s-c)           

10    format(3(A,F20.15,'||'))
      
      end