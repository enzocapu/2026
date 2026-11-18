      dimension dt(2),v0(3) 
      character*16 nome(2)
      r(h)=r0*(1-(beta*h)/temp)**alpha

100   format(a,1i1,a)

      dm=42
      d=149.1e-3 !diametro projetil
      pi=4*atan(1e0)
      v0(1)=377
      v0(2)=1.01*v0(1)
      v0(3)=0.99*v0(1)
      g=9.8
      beta=6.49e-3 !var. temp/alt
      temp=300 !temp. alt. mar
      alpha=4.256
      r0=1.3
      A=pi*(d/2)**2 !area projetil
      dt(1)=0.001
      dt(2)=0.01
      vs=343
      !n=90
      !dtheta=(pi/2)/n !int. theta 1grau
      theta=43*(pi/180)
      dxmax(1)=0 !alcance
      dxmax(2)=0

      do l=1,1 !varia dt
        do k=1,3 !varia v0
            write(nome(1),100) 'tarefa-2c-r',k,'.dat'
            write(nome(2),100) 'tarefa-2c-v',k,'.dat'
            open(k*10,file=nome(1))
            open(k*20,file=nome(2))
                t=0
                i=0
                vx=v0(k)*cos(theta)
                vy=v0(k)*sin(theta)
                v=sqrt(vx**2+vy**2)
                write(k*20,*) t,v
                x=0
                y=0 !conds. iniciais
                write(k*10,*) x,y,t
77              i=i+1 !lançamento
                t=i*dt(l)
                x=x+vx*dt(l)
                y=y+vy*dt(l)
                rho=r(y)
                if (v.lt.vs) then
                    cd=0.295
                else 
                    cd=0.5
                endif
                ak=(rho*A*cd)/(2*dm)
                vx=vx-ak*v*vx*dt(l)
                vy=vy-(g+ak*v*vy)*dt(l)
                v=sqrt(vx**2+vy**2)
                write(k*20,*) t,v
                if(y*(y+vy*dt(l)).lt.0) then
                    x2=x+vx*dt(l)
                    y2=y+vy*dt(l)
                    f=y/(y-y2)
                    x0=x+f*(x2-x)
                    t0=t+f*dt(l)
                    write(k*10,*) x,y,t
                    write(k*10,*) x0,0,t0
                    goto 44
                endif
                write(k*10,*) x,y,t
                goto 77
44              continue
                close(k*10)
        enddo
      enddo

      close(10)
      !close(20)
      close(30)

      end