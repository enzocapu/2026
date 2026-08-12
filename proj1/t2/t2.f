      dimension v1(3),v2(3),v3(3),v12(3),v13(3),v23(3),v123(3)
      write(*,*) 'v1 (componentes separadas por espaço)?'
      read(*,*) v1
      write(*,*) 'v2 (componentes separadas por espaço)?'
      read(*,*) v2
      write(*,*) 'v3 (componentes separadas por espaço)?'
      read(*,*) v3
           
      call PV(v1,v2,v12) !vv é o prod.vetorial
      call PV(v1,v3,v13)
      call PV(v2,v3,v23)
      call PV(v1-v2,v3,v123)

      if (abss(v12).eq.0) then
        write(*,*) 'v1 e v2 são paralelos!'
        goto 77
      endif

      if (pe(v12,v3).eq.0) then
        write(*,*) 'v3 esta no plano gerado por v1 e v2!'
        goto 77
      endif

      vol=(abs(pe(v12,v3)))/2
      al= abss(v13) + abss(v23) + abss(v123)
      
      write(*,*) 'volume=',vol
      write(*,*) 'arealateraltotal=',al

77    continue
      
      end

      function abss(a) !modulo de um vetor
        dimension a(3)
        abss = sqrt(a(1)**2+a(2)**2+a(3)**2)
      end   

      function PE(a,b) !p.escalar entre dois vetores
        dimension a(3),b(3)
        pe=0e0
        do i=1,3
            c=a(i)*b(i)
            pe=pe+c
        enddo
        return
      end

      subroutine PV(a,b,c) !p.vetorial entre dois vetores
        dimension a(3),b(3),c(3)
        
        c(1) = a(2)*b(3) - a(3)*b(2)
        c(2) = a(3)*b(1) - a(1)*b(3)
        c(3) = a(1)*b(2) - a(2)*b(1)

        return
      end