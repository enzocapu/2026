	pi = 4*atan(1.e0)
	write(*,*) 'r1 c.menor, r2 c.maior?'
	read(*,*) r1,r2
	r = r1 * r2
	pi2 = pi**2
	v = 2*pi2*r*r1
	a = 4*pi2*r
	write(*,*) 'A área superficial é A = ', a
	write(*,*) 'O volume é V = ', v
	end