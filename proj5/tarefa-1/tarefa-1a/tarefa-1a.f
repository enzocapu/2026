        implicit real*8 (a-h,o-z)
        dimension rho(2), v(2)
        write(*,*) 'Excentricidade =?'
        read(*,*) a
        pi=4*atan(1)
        rho(1)=a
        rho(2)=0
        ano=3.2e7
        dtao=ano/(2*pi)
        v(1)=0 !comp. radial
        v(2)=1/sqrt(a) !comp. tangente

        