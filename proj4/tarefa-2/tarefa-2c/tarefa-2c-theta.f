      dimension dt(2),dxmax(2,3),tmax(2,3),v0(3)!VARIA THETA E V0
      character*18 nome(3)
      r(x)=r0*(1-(beta*x)/temp)**alpha

100   format(a,2i1,a)

      dm=42
      d=149.1e-3 !diametro projetil
      pi=4*atan(1e0)
      v0(1)=377
      v0(2)=1.01*377
      v0(3)=0.99*377
      g=9.8
      beta=6.49e-3 !var. temp/alt
      temp=300 !temp. alt. mar
      alpha=4.256
      r0=1.3
      A=pi*(d/2)**2 !area projetil
      dt(1)=0.01
      dt(2)=0.001
      vs=343
      n=90
      dtheta=(pi/2)/n !int. theta 1grau

      do l=1,2 !varia dt
        do k=1,3 !varia v0
            dxmax(l,k)=0 !zerando matriz dx
            write(nome(1),100) 'tarefa-2c-dx',k,l,'.dat'
            write(nome(2),100) 'tarefa-2c-r',k,l,'.dat'
            write(nome(3),100) 'tarefa-2c-v',k,l,'.dat'
            open(10*k,file=nome(1))
            open(20*k,file=nome(2))
            open(30*k,file=nome(3))           
            do j=1,n !var. theta
                theta=j*dtheta              
                t=0
                i=0
                vx=v0(k)*cos(theta)
                vy=v0(k)*sin(theta)
                v=sqrt(vx*vx+vy*vy)
                x=0
                y=0 !conds. iniciais
                if(j.eq.43) then
                    write(20*k,*) x,y,t
                    write(30*k,*) t,v
                endif
77              i=i+1 !lançamento
                t=i*dt(l)
                rho=r(y)
                x=x+vx*dt(l)
                y=y+vy*dt(l)                
                if (v.lt.vs) then
                    cd=0.295
                else 
                    cd=0.5
                endif
                ak=(rho*A*cd)/(2*dm)
                vx=vx-ak*v*vx*dt(l)
                vy=vy-(g+ak*v*vy)*dt(l)
                v=sqrt(vx**2+vy**2)
                if(j.eq.43) then
                    write(20*k,*) x,y,t
                    write(30*k,*) t,v
                endif
                if(y*(y+vy*dt(l)).lt.0) then!!ultimoponto!!!
                    x2=x+vx*dt(l) !pos. negativa
                    y2=y+vy*dt(l)
                    x0=x-y*(x2-x)/(y2-y)
                    write(10*k,*) j, x0
                    if(x0.gt.dxmax(l,k)) then
                        dxmax(l,k)=x0
                        tmax(l,k)=j
                    endif
                    goto 44
                endif
                goto 77
44              continue
            enddo
            write(*,*) dt(l), v0(k), tmax(l,k), dxmax(l,k)
            close(10*k)
            close(20*k)
            close(30*k)
        enddo
      enddo

      end