c	BNDYPT.F (4/20/95)
c
c       
c	BOUNDARY CALCULATIONS
	subroutine bdcalc
	character choice*20
	integer lnblnk
 	write(*,*) 'SELECT ACTION '
 10	write(*,'(8X,A15,3X,A)')
	1    'farey:','Farey Fractions Calculations',
	2    'cusp:','Calculate cusps',
	3    'irrat:','Irrational boundary points',
	4    'allvalues:',
	5    'Calculate all solutions to tr(W_p/q)= given constant',
	5    'anosov:','Calculate fixed point of pseudo-Anosov',
	1    'up:','Just Press Return To Go Up To Previous Menu'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'farey') then
	   call fareya
	else if(choice.eq.'cusp') then
	   call t11pt
	else if(choice.eq.'irrat') then
	   call irrat
	else if(choice.eq.'allvalues') then
	   call psearch
	else if(choice.eq.'anosov') then
	   call anosov
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c       
c	FAREY ARITHMETIC
	subroutine fareya
	integer p,q,denom,idir
	character reply
 5	write(*,'(A$)') 'Enter fraction p/q, denom, idir: '
	read(*,*) p,q,denom,idir
 10	call nextpq(p,q,denom,idir)
	write(*,*) 'Successor is ',p,'/',q
	write(*,'(A$)') 'Another successor? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') go to 10
	write(*,'(A$)') 'Another p,q? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') go to 5
	end
c       
c	MASKIT T_1,1
	subroutine t11pt
	integer p,q,error
	complex*16 mu
	character reply
	call setfmt(0)
 5	write(*,'(A$)') 'Enter fraction p/q: '
	read(*,*) p,q
	call inimu0(p,q,mu,error)
	write(*,'(A,F18.15,1X,F18.15)')
	1    'Critical value is ',dreal(mu),dimag(mu)
	write(*,'(A$)') 'Another p,q? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') go to 5
	end
c
c	MASKIT BOUNDARY: IRRATIONAL POINTS
	subroutine irrat
	integer error
	real*8 r
	complex*16 mu,trx
	character reply

	call setfmt(0)
5	write(*,'(A$)') 'Enter parameter r: '
	read(*,*) r
	call bndypt(r,trx,error)
	mu = (0.0d0,1.0d0)*trx
	write(*,*) 'Critical value is ',mu
	write(*,'(A$)') 'Another r? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') go to 5
	end
c       
c	SCATTERING OF PARABOLICS
	subroutine psearch
	integer p,q,nummu,i
	complex*16 mulist(MAXCUSP),eta
	real*8 x0,y0,x1,y1
	character reply
	call setfmt(0)
 5	write(*,'(A$)') 'Enter fraction p/q: '
	read(*,*) p,q
	write(*,*) 'Enter search frame x0,y0,x1,y1: '
	read(*,*) x0,y0,x1,y1
	eta = (0.0d0,0.0d0)
	write(*,*) 'Enter eta (complex*16) = arccosh(given trace/2): '
	write(*,*) '  (Default value = 0) '
	read(*,*) eta
	call musrch(mulist,eta,x0,y0,x1,y1,p,q,nummu)
	write(*,*) 'Critical mus are: '
	write(*,300) (i,mulist(i),i=1,nummu)
 300	format('mu(',i3,') = ',f20.15,' + i ',f20.15)
	write(*,'(A$)') 'Another p,q? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') go to 5
	end
c       
c	FIXED POINTS OF PSEUDO-ANOSOVS
c       
c	THIS IS AN ADAPTATION OF THE BASIC BOUNDARY COMPUTATION
c	DESIGNED TO CALCULATE TROELS JORGENSEN'S SINGLY
c	DEGENERATE GROUP CORRESPONDING TO PERIODIC CONTINUED FRACTIONS
c       
	subroutine anosov
	integer p,q,error,a,b,c,d,j
	complex*16 mu,mulast
c
	call setfmt(0)
c	MU = I * TRACE X (FIRST GUESS OF PARABOLIC)
	mu=(0.0d0,2.0d0)
	mulast=(0.0d0,2.0d0)
c	OPEN OUTPUT FILE
	open(1,file = 'troels.pts',status='unknown')
c	GET MAPPING CLASS GROUP ELEMENT
	a=-1
	b=1
	c=-1
	d=2
	write(*,*) 'Enter matrix representation of automorphism: '
	write(*,*) 'Default: a=',a,' b=',b,' c=',c,' d=',d
	read(*,*) a,b,c,d
c	FIRST FRACTION
	p=1
	q=3
	write(*,*) 'Enter first fraction:'
	write(*,*) 'Default: p=',p,' q=',q
	read(*,*) p,q
	error=0                   
c	MAIN LOOP
c       FIND THE VALUE OF MU, FOR WHICH THE
c       (P,Q) WORD IS PARABOLIC
 70	call solmu0(p,q,mu,error)
c       OUTPUT VALUE OF MU
	write(1,170) p,q,dreal(mu),dimag(mu)
	write(*,170) p,q,dreal(mu),dimag(mu)
 170	format(I10,',',I10,',(',F20.16,',',F20.16,')')
c       CHECK FOR CONVERGENCE ERROR
	if(error .ne. 0) go to 27
c       END LOOP WHEN MU EQUALS MULAST
	if(mu .eq. mulast) go to 27
c       GET THE NEXT CONVERGENT FOR (2,1,1,...) 
	j=p
	p=a*p+b*q
	q=c*j+d*q
	mulast=mu
	go to 70
c	CLOSE OUTPUT FILE
 27	close(1)
	write(*,307) p,q
 307	format(1x,'last index: ',I10,',',I10)
	end
