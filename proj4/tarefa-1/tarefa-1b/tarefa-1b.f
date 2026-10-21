      v(x)=sqrt(2*x/dm)
      tk=0
      dt=0.1
      pc=400
      t=0
      dm=70
      n=300/dt
      s=v(0e0)
      rho=1.3
      a=0.3

      open(10,file='tarefa-1b-v.dat')
      open(20,file='tarefa-1b-x.dat')
      write(10,*) t,v(tk)
      write(20,*) t,(dt/3)*s

      do i=1,n
        tk=tk+dt*(pc-sqrt(2/(dm**3))*rho*a*(tk**1.5))
        t=i*dt
        if(i.ne.n) then
          if(mod(i,2).eq.0) then
            s=s+2*v(tk)
            write(20,*) t,(dt/3)*s
            else
            s=s+4*v(tk)
          endif
        endif
        write(10,*) t,v(tk)
      enddo

      s=s+v(tk)
      write(20,*) t, (dt/3)*s
      close(10)
      close(20)
      end