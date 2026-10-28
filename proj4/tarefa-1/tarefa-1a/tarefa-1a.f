      v(x)=sqrt(2*x/dm)
      tk=0
      dt=0.1
      pc=400
      t=0
      dm=70
      n=300/dt
      s=v(0e0)

      open(10,file='tarefa-1a-v.dat')
      open(20,file='tarefa-1a-x.dat')
      write(10,*) t,v(tk)
      write(20,*) t,(dt/3)*s

      do i=1,n
        tk=tk+(dt*pc)
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