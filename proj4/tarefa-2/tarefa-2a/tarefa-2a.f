      dm=42
      d=149.1e-3 !diametro projetil
      pi=4*atan(1e0)
      theta=pi/3
      v0=377
      g=9.8
      rho=1.3
      cd=0.295
      A=pi*(d/2)**2
      dt=0.01
      x=0
      y=0
      vx=v0*cos(theta)
      vy=v0*sin(theta)
      t=0
      i=0

      open(10,file='tarefa-2a-v.dat')
      open(20,file='tarefa-2a-r.dat')
      write(10,*) vx,vy,t
      write(20,*) x,y,t
      
77    i=i+1
      t=i*dt
      vx=vx-((rho*A*cd)/(2*dm))*sqrt(vx**2+vy**2)*vx*dt
      vy=vy-(g+(rho*A*cd)/(2*dm)*sqrt(vx**2+vy**2)*vy)*dt
      if(y*(y+vy*dt).lt.0) then
        goto 44
      endif
      x=x+vx*dt
      y=y+vy*dt
      write(10,*) vx,vy,t
      write(20,*) x,y,t
      goto 77

44    close(10)
      close(20)
      end