      implicit real*8 (a-h,o-z)
      dimension e(3),e0(3),et(3)

      f(x)=exp(-x)*cos(x)

      pi=4*atan(1d0)
      ti=(1-exp(-2*pi))/2

      open(10,file='tarefa2.dat')

      do k=1,3
        e0(k)=1d99
      enddo

      do i=2,13
        n=2**i
        h=2*pi/n
        st=0
        ss=0
        sb=0

        do j=0,n
            xj=j*h
            fj=f(xj)

            if(j.eq.0.or.j.eq.n) then
                st=st+fj
                ss=ss+fj
                sb=sb+7*fj
            else
                st=st+2*fj

                if(mod(j,2).eq.1) then
                    ss=ss+4*fj
                else
                    ss=ss+2*fj
                endif

                if(mod(j,4).eq.1.or.mod(j,4).eq.3) then
                    sb=sb+32*fj
                else if(mod(j,4).eq.2) then
                    sb=sb+12*fj
                else
                    sb=sb+14*fj
                endif
            endif

        enddo

        st=h*st/2
        ss=h*ss/3
        sb=2*h*sb/45

        write(10,*) n,st,ss,sb

        e(1)=abs(st-ti)
        e(2)=abs(ss-ti)
        e(3)=abs(sb-ti)

        do k=1,3
            if(e(k).lt.e0(k)) then
                e0(k)=e(k)
                et(k)=i
            endif
        enddo

      enddo

      write(10,*) ti

      do k=1,3
        write(*,*) et(k)
      enddo

      close(10)

      end