      dimension dt(2)
      
      open(10,file='tarefa-2b-dx1.dat')
      open(20,file='tarefa-2b-dx2.dat')

      dm=42
      d=149.1e-3 !diametro projetil
      pi=4*atan(1e0)
      v0=377
      g=9.8
      rho=1.3
      cd=0.295
      A=pi*(d/2)**2 !area projetil
      dt(1)=0.01
      dt(2)=0.001
      ak=(rho*A*cd)/(2*dm) !constante ar
      n=90 !num. intervalos (=1grau)
      dtheta=(pi/2)/n

      do k=1,n !varia theta
            do j=1,2 !varia dt
                  theta=k*dtheta
                  t=0
                  i=0
                  vx=v0*cos(theta)
                  vy=v0*sin(theta)
                  x=0
                  y=0
77                i=i+1
                  t=i*dt(j)
                  x=x+vx*dt(j)
                  y=y+vy*dt(j)
                  v=sqrt(vx**2+vy**2)
                  vx=vx-ak*v*vx*dt(j)
                  vy=vy-(g+ak*v*vy)*dt(j)
                  if(y*(y+vy*dt(j)).lt.0) then                  
                        write(10,*) theta, x
                        goto 44
                  endif
                  goto 77
44                continue
            enddo
      enddo

            close(10)

      end