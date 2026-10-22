      dimension vx(3),vy(3),y(3),x(3),theta(3),ak(2), amax(2)
      dm=42
      d=149.1e-3 !diametro projetil
      pi=4*atan(1e0)
      theta(1)=pi/3
      theta(2)=pi/4
      theta(3)=pi/6
      v0=377
      g=9.8
      rho=1.3
      cd=0.295
      A=pi*(d/2)**2 !area projetil
      dt=0.01
      ak(1)=(rho*A*cd)/(2*dm) !constante ar
      ak(2)=0
      amax(1)=0
      amax(2)=0

      do j=1,3 !varia theta
            do k=1,2 !varia res. ar
                  t=0
                  i=0
                  vx(j)=v0*cos(theta(j))
                  vy(j)=v0*sin(theta(j))
                  x(j)=0
                  y(j)=0
77                i=i+1
                  t=i*dt
                  x(j)=x(j)+vx(j)*dt
                  y(j)=y(j)+vy(j)*dt
                  v=sqrt(vx(j)**2+vy(j)**2)
                  vx(j)=vx(j)-ak(k)*v*vx(j)*dt
                  vy(j)=vy(j)-(g+ak(k)*v*vy(j))*dt
                  if(y(j)*(y(j)+vy(j)*dt).lt.0) then
                    if(x(j).gt.amax(j)) then
                        amax(j)=x(j)
                    endif    
                    goto 44
                  endif
                  goto 77
44                continue
            enddo
      enddo

      end