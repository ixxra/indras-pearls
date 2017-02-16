c	BNDY.F (7/3/90)
c
c       BOUNDARY DRAWING PROGRAM
c
c
c       BOUNDARY MENU
	subroutine bdmenu
	character choice*15,col*30,outfil*100
	integer lnblnk,trsty,errstop,outsty,penw,inout
	real*8 xmin,ymin,xmax,ymax,cspread
	logical grpon,epson,outon,raston,curvon,colron
	common /flags/ grpon,epson,outon,raston,curvon
	common /trfmt/ trsty
	common /outfmt/ outsty
	common /bdflag/ errstop,colron
	common /picwin/ xmin,ymin,xmax,ymax
	colron=.false.
c       INITIATE OUTPUT FILES
c	DEFAULT SCALES
	call setfmt(0)
	if(trsty.eq.1) then
	   xmin=0
	   ymin=1.5
	   xmax=2
	   ymax=2.5
	else if(trsty.le.3) then
	   xmin=-2
	   ymin=0.5
	   xmax=2
	   ymax=3.5
	else 
	   xmin=-4
	   ymin=-3
	   xmax=4
	   ymax=7
	end if
c       DEFAULTS
	outsty = 1
	curvon=.true.
	errstop=0
c       SET UP FRAME
	call onout(xmin,ymin,xmax,ymax,'bndy')
 10	write(*,'(/,5X,A,/)') ' BOUNDARY DRAWING MENU: '
	write(*,'(8X,A15,3X,A)') 
	1    'piece:','Boundary piece',
	1    'frag:','Boundary fragment starting from any point',
	1    'file:','File of boundary points',
	2    'ksray:','Real trace ray',
	3    'rays:','Collection of rays',
	4    'kshoro:','Keen-Series horocycle',
	5    'horos:','Several Keen-Series horocycles',
	5    'choros:','Color horocycle bands',
	6    'scatter:','Scattering of parabolics',
	7    'ellipse:','Ellipses (Maskit only)',
	9    'fill:','Begin filling curves',
	9    'fillin:','Fill inside',
	9    'fillout:','Fill outside',
	5    'pen:','Set new penwidth',
	9    'color:','Switch to a new color',
	9    'colorson:','Turn on color cycling',
	9    'colorsoff:','Turn off color cycling',
	5    'up:','Just Press Return To Go To MAIN MENU'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'piece') then
	   call boundary
	else if(choice.eq.'frag') then
	   errstop = 1
	   call boundary
	   errstop = 0
	else if(choice.eq.'file') then
c	STORE LIMIT POINTS IN A FILE
c       OUTSTY = 0 MEANS STORE IN FILE
	   outsty = 0
	   write(*,*) 'Enter name of boundary points file: '
	   read(*,'(A)') outfil
	   open(3,file = outfil)
	   errstop = 1
	   call boundary
	   errstop = 0
	   close(3)
	else if(choice.eq.'ksray') then
	   call drwray
	else if(choice.eq.'rays') then
	   call rays
	else if(choice.eq.'kshoro') then
	   call drwhoro
	else if(choice.eq.'horos') then
	   call horos
	else if(choice.eq.'choros') then
	   call choros
	else if(choice.eq.'scatter') then
	   curvon=.false.
	   call scatter
	   curvon=.true.
	else if(choice.eq.'ellipse') then
	   call ellipse
	else if(choice.eq.'fill') then
c	PLOT FILLED CURVES
c       OUTSTY = 2 MEANS POSTSCRIPT RASTER IMAGE OF FILLED CURVE
	   outsty = 2
c       INITATE RASTER MATRICES IF NOT ALREADY
	   if(.not.raston) then 
	      call onbit(xmin,ymin,xmax,ymax)
	      raston=.true.
	   end if
	else if(choice.eq.'fillin'.or.choice.eq.'fillout') then
c       FILL AND END RASTER IMAGE
	   if(raston) call offbit
c       OUTPUT TO FILE (INOUT=1 -> INSIDE)
	   inout=2
	   if(choice.eq.'fillin') inout=1
	   call gfxdoc('Begin Filled Boundary')
	   call offfil(inout)
	   call gfxdoc('End Filled Boundary')
	   raston=.false.
	   outsty = 1
	else if(choice.eq.'pen') then
c       PEN WIDTH: 1=default
	   write(*,'(A$)') 'Enter new penwidth: (integer, default=1) '
	   penw=1
	   read(*,*) penw
	   call penwid(penw)
	else if(choice.eq.'color') then
c	CHOOSE A NEW COLOR
	   write(*,'(A)') 'Enter the name of a color (as ',
	1	'defined in psgraph.pro, e.g. Red):'
	   read(*,'(A)') col
	   call ccolor(col)
	else if(choice.eq.'colorson') then
	   write(*,'(A)') 'Enter color spread: (real*8)'
           read(*,*) cspread
	   call ccycle(cspread)
	   colron=.true.
	else if(choice.eq.'colorsoff') then
	   colron=.false.
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c       
c	PLOT PIECE OF BOUNDARY
	subroutine boundary
	integer pstart,qstart,pend,qend,denom,outsty,nomove
	real*8 xmin,ymin,xmax,ymax
	common /outfmt/ outsty
	common /picwin/ xmin,ymin,xmax,ymax
	common /motion/ nomove
	call getfrac(pstart,qstart,pend,qend,denom)
c	SET UP CURVE TRACE
	call setlab
	if (outsty.eq.1) then 
	   call gfxdoc('Begin boundary')
	   call ongrf(1)
	   call swindw(xmin,ymin,xmax,ymax)
	end if
	nomove = 0
	call bndy(pstart,qstart,pend,qend,denom,dcmplx(0.0d0,0.0d0))
	if (outsty.eq.1) then
	   call offgrf
	   call gfxdoc('End boundary')
	else if(outsty.eq.2) then
	   call clipend
	end if
	call outlab
	end
c       
c	PLOT ONE KEEN HOROCYCLE
	subroutine drwhoro
	integer pstart,qstart,pend,qend,denom,nomove
	real*8 xmin,ymin,xmax,ymax
	complex*16 eta
	common /picwin/ xmin,ymin,xmax,ymax
	common /motion/ nomove
	call getfrac(pstart,qstart,pend,qend,denom)
	write(*,*) 'Enter eta (complex) where tr W_p/q = +-2 cosh(q eta):'
	read(*,*) eta
c	SET UP CURVE TRACE
	call setlab
	call gfxdoc('Begin horocycle')
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	nomove = 0
	call bndy(pstart,qstart,pend,qend,denom,eta)
	call offgrf
	call gfxdoc('End horocycle')
	call outlab
	end
c       
c	PLOT SYSTEM OF KEEN HOROCYCLES
	subroutine horos
	integer denom,pstart,qstart,pend,qend,num,i,fill,nomove
	real*8 xmin,ymin,xmax,ymax
	complex*16 eta,deleta
	common /picwin/ xmin,ymin,xmax,ymax
	common /motion/ nomove
	fill=0
	go to 5
c       
c	PLOT SYSTEM OF KEEN HOROCYCLES
	entry choros
	fill=1
 5	continue
	call getfrac(pstart,qstart,pend,qend,denom)
	eta=0.0d0
	write(*,*) 'Enter minimal eta: (Default ',eta,')'
	read(*,*) eta
	write(*,*) 'Enter increment in eta: '
	read(*,*) deleta
	write(*,*) 'Enter number of increments: '
	read(*,*) num
c	SET UP CURVE TRACE
	call setlab
	call gfxdoc('Begin horocycles')
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 i=1,num+1
	   nomove=0
	   call bndy(pstart,qstart,pend,qend,denom,eta)
	   if(fill.eq.1) then
c       Reverse the current path
	      call revers
c       Append the rest of the path
	      nomove=1
	      call bndy(pstart,qstart,pend,qend,denom,eta+deleta)
c       Select the next color
	      call nxtcol(i-1)
c       Fill the path
	      call ifill(0,0)
	   end if
	   eta=eta+deleta
 10	continue
	call offgrf
	call gfxdoc('End horocycles')
	call outlab
	end
c       
c	PLOT SCATTERING OF ACCIDENTAL PARABOLICS
	subroutine scatter
	integer denom,pstart,qstart,pend,qend,nomove
	real*8 xmin,ymin,xmax,ymax
	complex*16 eta
	common /picwin/ xmin,ymin,xmax,ymax
	common /motion/ nomove
	call getfrac(pstart,qstart,pend,qend,denom)
	eta=0.0d0
	write(*,*) 'Enter eta value sought: (Default ',eta,')'
	read(*,*) eta
c	SET UP CURVE TRACE
	call setlab
	call gfxdoc('Begin scatter plot')
	call ongrf(-1)
	call swindw(xmin,ymin,xmax,ymax)
	nomove = 0
c       OLD LABORIOUS ROUTINE
	call scatt(pstart,qstart,pend,qend,denom,eta)
c       NEW BUT FLAWED ROUTINE
c$$$	call bndy(pstart,qstart,pend,qend,denom,eta)
	call offgrf
	call gfxdoc('End scatter plot')
	call outlab
	end
c       
c	DRAW ELLIPSE MARKING FUNDAMENTAL REGION FOR DEHN TWIST (MASKIT)
	subroutine ellipse
	integer trsty,i,n0,n1
	complex*16 a1,a2,a3,a4,a5,a6,z1
	real*8 lam,fac,theta,a,b,a0,xmin,ymin,xmax,ymax,pi,x,y
	common /trfmt/ trsty
	common /trprms/ a1,a2,a3,a4,a5,a6
	common /picwin/ xmin,ymin,xmax,ymax
	data pi/3.14159265358979323844d0/
	if(trsty.ne.2 ) then
	   write(*,*) 'Not Applicable'
	   return
	end if
	n0=0
	n1=0
	write(*,*) 'Enter starting and ending numbers (nonnegative):'
	write(*,*) 'Defaults:',n0,',',n1
	read(*,*) n0,n1
	n0=max(0,n0)
	n1=max(0,n1)
	z1=cdsqrt(a1)
	lam=0.5d0*dabs(dlog(cdabs(a3+z1)))
	z1=(0.0d0,0.5d0)*cdsqrt(-a2)/z1
	a0=cdabs(z1)
	theta=datan2(dimag(z1),dreal(z1))
	theta=180.0d0*theta/pi
	call gfxdoc('Ellipses')
	call ongrf(3)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 i=n0,n1
	   if(i.eq.0) then
	      x=2.0d0*dreal(z1)
	      y=2.0d0*dimag(z1)
	      call smove(-x,-y)
	      call sline(x,y)
	      go to 10
	   end if
	   fac=dexp(dble(i)*lam)
	   a=a0*(fac+1.0d0/fac)
	   b=a0*(fac-1.0d0/fac)
	   call sellps(0.0d0,0.0d0,a,b,theta)
 10	continue
	call offgrf
	end
c       
c	GET STARTING AND ENDING FRACTIONS AND DENOMINATOR
	subroutine getfrac(pstart,qstart,pend,qend,denom)
	integer pstart,qstart,pend,qend,denom,numpts,errstop
	real*8 pi2by3,diff1,diff2,sigmin,sigmax,taumin,taumax
	character*80 line1,line2
	character reply
	logical colron
	common /bndmax/ sigmin,sigmax,taumin,taumax
	common /bdflag/ errstop,colron
	common /trlbl/ line1,line2
	data pi2by3/3.28986813d0/
	write(*,*) 'Enter pstart,qstart, pend,qend:'
	read(*,*) pstart,qstart,pend,qend
	numpts=1500
	write(*,*) 'Enter desired number of points: '
	write(*,*) 'Default: ',numpts
	read(*,*) numpts
	diff1=dble(pend)/qend
	diff2=dble(pstart)/qstart
	denom = dsqrt(pi2by3*numpts/(diff1-diff2))
c	ERROR HANDLING
	write(*,*) 'Termination upon error? (y or n) '
	read(*,'(A)') reply
	if(reply .eq. 'y') errstop = 2
c	INITIALIZE VARIABLES TO TRACK FRAME USED
	sigmin=1000.0d0
	sigmax=-1000.0d0
	taumin=1000.0d0
	taumax=-1000.0d0
c	LABELLING
	write(2,'(A)') line1
        write(2,'(A)') line2
	write(2,'(4(A,I8))') 
	1    'First p/q =',pstart,'/',qstart,
	2    '; Last p/q =',pend,'/',qend
	write(2,'(A,I10)') 'Denom =',denom
	end
c       
c	FIND OUT THE POINTS TO BE LABELLED
	subroutine setlab
	integer plist(50),qlist(50),numlab
	real*8 ang(50),xlab(50),ylab(50)
	character*40 labl(50)
	common /labls/ xlab,ylab,ang,plist,qlist,numlab,labl
	numlab=0
	write(*,*) 'Enter points to be labelled:'
 10	write(*,*) 'Enter label: (return to end)'
	read(*,'(A)') labl(numlab+1)
	if(labl(numlab+1) .eq. ' ') return
	numlab=numlab+1
	write(*,*) 'Enter p, q, angle:'
	read(*,*) plist(numlab),qlist(numlab),ang(numlab)
	go to 10
	end
c       
c	CHECK TO SEE IF POINT SHOULD BE LABELLED
	subroutine putlab(sigma,tau,p,q)
	integer plist(50),qlist(50),numlab,p,q,i
	real*8 ang(50),xlab(50),ylab(50),sigma,tau
	character*40 labl(50)
	common /labls/ xlab,ylab,ang,plist,qlist,numlab,labl
	do 10 i=1,numlab
 	   if(p.eq.plist(i).and.q.eq.qlist(i)) go to 20
 10	continue
 20	xlab(i)=sigma
	ylab(i)=tau
	end
c       
c	OUTPUT ALL LABELS
	subroutine outlab
	integer plist(50),qlist(50),numlab,i
	real*8 ang(50),xlab(50),ylab(50)
	character*40 labl(50)
	common /labls/ xlab,ylab,ang,plist,qlist,numlab,labl
	do 10 i=1,numlab
	   call slabel(xlab(i),ylab(i), 0.5d0,ang(i),labl(i))
 10	continue
	end
c       
c	CLOSE UP BOUNDARY CALCULATION
	subroutine btrail
	real*8 sigmin,sigmax,taumin,taumax
	common /bndmax/ sigmin,sigmax,taumin,taumax
c	WRITE OUT FRAME LIMITS
	write(*,400) sigmin,sigmax,taumin,taumax
 400	format(f15.10,' < Re(mu) < ',f15.10,/,
	1    f15.10,' < Im(mu) < ',f15.10)
c	SET UP FOR NEXT RUN
	sigmin=1000.0d0
	sigmax=-1000.0d0
	taumin=1000.0d0
	taumax=-1000.0d0
	end
c       
c	RAY PLOTTING ROUTINES
C	
c	PLOT ONE REAL TRACE RAY
	subroutine drwray
	integer p,q,error,trsty
	complex*16 trw,mu
	real*8 xmin,ymin,xmax,ymax
	common /trmft/ trsty
	common /picwin/ xmin,ymin,xmax,ymax
	write(*,*) 'Enter fraction p/q:'
	read(*,*) p,q
	trw = 2.0d0
	if(trsty.gt.3) trw=-trw
	call inimu(p,q,mu,trw,error)
c	SET UP CURVE TRACE
	call gfxdoc('Begin ray')
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	call ray0(mu,p,q)
	call offgrf
	call gfxdoc('End ray')
	end
c       
c	PLOT SYSTEM OF REAL TRACE RAYS
	subroutine rays
	integer pstart,qstart,pend,qend,denom,i
	complex*16 mulist(MAXBNDY),mu0lst(MAXBNDY),eta
	integer plist(MAXBNDY),qlist(MAXBNDY),icount
	real*8 xmin,ymin,xmax,ymax
	common /bndlst/ mulist,mu0lst,plist,qlist,icount
	common /picwin/ xmin,ymin,xmax,ymax
	call getfrac(pstart,qstart,pend,qend,denom)
	eta=0.0d0
	call bndstr(pstart,qstart,pend,qend,denom,eta)
c	SET UP CURVE TRACE
	call gfxdoc('Begin rays')
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 i=1,icount
 	   call ray0(mulist(i),plist(i),qlist(i))
 10	continue
	call offgrf
	call gfxdoc('End rays')
	end



