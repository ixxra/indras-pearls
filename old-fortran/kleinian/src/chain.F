c	CHAIN.F (4/1/95)
c       
c	This program draws chains of circles according to the theory
c	of Wright-Keen-Series. The theory is described in 
c       Wright, The shape of the boundary of T_1,1 in Maskit's
c       embedding, Sections 3-5
c       
c	FIRST INPUT: a fraction p/q 
c       
c	First, the program computes the critical value of mu 
c	associated to this fraction.
c       
c	Define T = | -i*mu    -i | and      S = | 1    2 |
c       |  -i      0  |              | 0    1 |
c       
c	We define a sequence of words M(I) as follows. 
c	Start with   M(Q+1) = S. Then
c       M(J - P)= T^-1 M(J)    for P+1 <= J <= Q+P
c       M(J + Q)= S M(J)       for   1 <  J <= P
c	These relations uniquely define M(J) for all 1 <= J <= Q+P.
c	The critical word is X_P/Q = M(1). The critical value mu0
c	is the unique point on the boundary of T_1,1 where 
c       trace( X_P/Q ) = 2.
c	This implies that X_p/q has a unique fixed point, call it ZFIX. 
c       
c	There is a unique circle C(1) tangent to the real line at -1 and 
c	passing through ZFIX. C(1) is fixed by X_p/q.
c       
c	Define a chain of circles C(I) as follows
c       C(J - P)= T^{-1} ( C(J) )        for P+1 <= J <= Q+P
c       C(J + Q)= S( C(J) ) = C(J) + 2   for 1 <= J <= P
c       
c	Enumerating the words in this way, we have M(I) (C(1)) = C(I).
c	Most importantly, the circles C(I) and C(I+1) are tangent at  
c	the point Z(I) = M(I) (ZFIX). We have Z(1)=ZFIX.
c       
c	If trace( X_p/q ) = t is only real, then we know
c	X_p/q has two fixed points ZFIX1 and ZFIX2. There is a unique circle 
c	C(1) tangent to RR and passing through both fixed points. 
c	Starting from this circle we can define a circle chain exactly as 
c	before.
c       
c       FLIP
c       
c	This program draws the fundamental domain for the group G_mu for mu 
c	near the cuspidal point mu(p/q) in the boundary of Maskit's T_1,1.
c       
c	The calculations of the cuspidal C_1(p/q) and C_2(p/q) proceeds 
c	exactly as in the program ORTHOG. The curve C_2 is plotted as 
c	is, while the curve C_1 is translated by MU-MU(P/Q).
c       
c----------------------------------------------------------------------------
c       
c       CIRCLE CHAIN MENU
	subroutine chmenu
	character choice*15,col*30
	integer outsty,lnblnk
	real*8 xmin,ymin,xmax,ymax
	logical grpon,epson,outon,raston,curvon
	complex*16 mu,mu0,muchn
	integer p,q,chntype
	logical newpq
	common /pqnew/ newpq
	common /crit/ mu,muchn,p,q,chntype
	common /flags/ grpon,epson,outon,raston,curvon
	common /outfmt/ outsty
	common /picwin/ xmin,ymin,xmax,ymax
c       SET UP ROUTINES IN MUFIND
	call setfmt(1)
	newpq=.true.
	chntype=0
	call getpq(mu0,2.0d0)
	mu=mu0
c       INITIATE OUTPUT FILE
	call onout(-1.5d0,-0.2d0,2.5d0,2.2d0,'chain')
 10	write(*,'(/,5X,A,/)') ' CIRCLE CHAIN MENU: '
	write(*,'(8X,A15,3X,A)') 
	1    'upper:','Upper region for Gamma(2)',
	2    'lower:','Lower region for Gamma(2)',
	3    'chain:','Critical circle chain',
	4    'horo0:','Critical horocycle at 0',
	5    'horoinf:','Critical horocycle at infty',
	6    'kschain:','Keen-Series circle chain',
	7    'shift:','Shifted horocycle at infty',
	8    'allchain:','All possible circle chains',
	9    'limchain:','Limit of chains',
	1    'kshoro:','Keen-Series Horocycle',
	2    'delta:','Blow-up of delta_0',
	3    'lset:','Limit Set As Union of Circles',
	9    'color:','Switch to a new color',
	5    'up:','Just Press Return To Go To MAIN MENU'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'upper') then
	   call topgam
	else if(choice.eq.'lower') then
	   call botgam
	else if(choice.eq.'chain') then
	   call critchn
	else if(choice.eq.'horo0') then
	   call horo0
	else if(choice.eq.'horoinf') then
	   call horoinf((0.0d0,0.0d0))
	else if(choice.eq.'kschain') then
	   call genchn
	else if(choice.eq.'shift') then
	   call flip
	else if(choice.eq.'allchain') then
	   call pattern
	else if(choice.eq.'limchain') then
	   call chnlim
	else if(choice.eq.'kshoro') then
	   call kshoro
	else if(choice.eq.'delta') then
	   call delta0
	else if(choice.eq.'lset') then
c	   call cirset
	else if(choice.eq.'color') then
c	CHOOSE A NEW COLOR
	   write(*,'(A)') 'Enter the name of a color (as ',
	1	'defined in psgraph.pro, e.g. Red):'
	   read(*,'(A)') col
	   call ccolor(col)
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c       
c 	GET FRACTION AND CRITICAL MU
	subroutine getpq(mu,trwr)
	integer p,q,p0,q0,error,chntype,numlab
	complex*16 mu,mu0,mu1,trw,muchn
	real*8 trwr
	logical newpq
	character reply
	common /pqnew/ newpq
	common /crit/ mu0,muchn,p,q,chntype
	if(newpq) then
	   p=5
	   q=16
	   write(*,*) 'Enter p,q (Defaults: ',p,'/',q,')'
	   read(*,*)   p,q
	   numlab=1
	   write(2,100) p,q
 100	   format('Fraction = ',I8,'/',I8)
	   newpq=.false.
	end if
c	GET I*TRACE = MU0
	trw=trwr
	p0=p
	q0=q
	call inimu(p0,q0,mu1,trw,error)
 6	write(*,*) 'The desired mu is: ',mu1
	write(*,*) 'Do you wish to use this value? (Y or N)'
	read(*,'(A)') reply
	if(reply .eq. 'n') then
	   write(*,*) 'Enter another first guess: (complex)'
	   read(*,*) mu1
	   call solmu(p0,q0,mu1,trw,error)
	   go to 6
	end if
	mu=mu1
c	LABELLING
	write(2,200) trwr,dreal(mu1),dimag(mu1)
 200	format('For trace W_p/q = ',f8.4,' mu = ',f10.7,'+',f10.7,'i')
	end
c
c	DRAW TOP FUNDAMENTAL REGION FOR GAMMA(2)
	subroutine topgam
	real*8 xmin,ymin,xmax,ymax
	common /picwin/ xmin,ymin,xmax,ymax
	call ongrf(3)
	call swindw(xmin,ymin,xmax,ymax)
	call ssegmt(-1.0d0,ymax,-1.0d0,0.0d0)
	call sarcn(-0.5d0,0.0d0,0.5d0,180.0d0,0.0d0)
	call sarcn(0.5d0,0.0d0,0.5d0,180.0d0,0.0d0)
	call ssegmt(1.0d0,0.0d0,1.0d0,ymax)
	call sline(-1.0d0,ymax)
	call offgrf
	return
c
c	DRAW BOTTOM FUNDAMENTAL REGION FOR GAMMA(2)
	entry botgam
	call ongrf(3)
	call swindw(xmin,ymin,xmax,ymax)
	call ssegmt(-1.0d0,ymin,-1.0d0,0.0d0)
	call sarc(-0.5d0,0.0d0,0.5d0,180.0d0,0.0d0)
	call sarc(0.5d0,0.0d0,0.5d0,180.0d0,0.0d0)
	call ssegmt(1.0d0,0.0d0,1.0d0,ymin)
	call sline(-1.0d0,ymin)
	call offgrf
	end
c       
c	DRAW CRITICAL CHAIN OF CIRCLES
	subroutine critchn
	complex*16 mu0,mu,muchn
	real*8 xmin,ymin,xmax,ymax
	integer p,q,chntype
	common /crit/ mu0,muchn,p,q,chntype
	common /picwin/ xmin,ymin,xmax,ymax
	mu=mu0
	if(chntype .ne. 1) then
	   call getchn(mu)
	   chntype=1
	end if
	call ongrf(3)
	call swindw(xmin,ymin,xmax,ymax)
	call ssegmt(xmin,dimag(mu),xmax,dimag(mu))
	call offgrf
	call drwchn(1,q+p+1)
	end
c       
c	CALCULATE THE CHAIN OF CIRCLES AND INTERSECTION POINTS
	subroutine getchn(mu)
	complex*16 mu,mu0,mu1,z1,z2,mat(4),z,tmuinv(4),c,muchn
	complex*16 cen(20000),zfix1(20000),zfix2(20000)
	real*8 rad(20000),u,r
	integer p,q,p0,q0,i,j,index,nindex,chntype,err
	common /crit/ mu1,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	data tmuinv(1)/(0.0d0,0.0d0)/,tmuinv(2)/(0.0d0,1.0d0)/
	data tmuinv(3)/(0.0d0,1.0d0)/
c 	CALCULATE THE FIRST CIRCLE
c       NOTE THAT M(1) FIXES C(1)
c       FIXED POINTS OF X_p/q:
	p0=p
	q0=q
	mu0=mu
	muchn=mu
	tmuinv(4) = (0.0d0,-1.0d0)*mu
	call mumatw(p0,q0,mu0,mat)
	call fixpt(mat,z1,z2,err)
	zfix1(1) = z1
	zfix2(1) = z2
	if(err .lt. 2 .and. dabs(dimag(z1)) .ge. 1.0d-6) then
	   u = cdabs(z1+1.0d0)
	   rad(1) = (u*u)/(2.0d0*dimag(z1))
	   cen(1) = dcmplx(-1.0d0, rad(1) )
	   rad(1) = dabs(rad(1))
	else
	   rad(1) = -3.14159265358979323844d0
	   cen(1) = -1.0d0
	end if
c	CALCULATE THE LIST OF CIRCLES AND TANGENT POINTS
	index=1
	do 20 i=1,p
	   nindex=index+q
	   cen(nindex)=cen(index)+2.0d0
	   rad(nindex)=rad(index)
	   zfix1(nindex)=zfix1(index)+2.0d0
	   zfix2(nindex)=zfix2(index)+2.0d0
	   index=nindex
	   do 10 j=1,q+1
	      if(index .le. p+1) go to 20
	      nindex=index-p
	      call linfrac(tmuinv,zfix1(index),z,err)
	      zfix1(nindex) = z
	      call linfrac(tmuinv,zfix2(index),z,err)
	      zfix2(nindex) = z
	      call lfapply(tmuinv,cen(index),c,rad(index),r)
	      cen(nindex)=c
	      rad(nindex)=r
 10	      index=nindex
 20	   continue
	end
c       
c	DRAW CHAIN OF CIRCLES
	subroutine drwchn(n0,n1)
	complex*16 mu0,muchn,cen(20000),zfix1(20000),zfix2(20000)
	real*8 rad(20000),xmin,ymin,xmax,ymax
	integer n0,n1,m0,m1,j0,j1,i,k,p,q,chntype
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	m0 = (n0-1)/q
	if( n0 .lt. 1) m0=m0-1
	m1 = (n1-1)/q
	if( n1 .lt. 1) m1=m1-1
	j0 = n0 - m0*q
	j1 = n1 - m1*q
	call swindw(xmin,ymin,xmax,ymax)
	call ongrf(3)
	do 10 k=m0,m1
	   do 10 i=1,q
	      if( k.eq.m0 .and. i.lt.j0) go to 10
	      if( k.eq.m1 .and. i.gt.j1) go to 10
	      call scirc(dreal(cen(i))+2.0d0*k,dimag(cen(i)),rad(i))
 10	   continue
	   call offgrf
	end
c       
c	DRAW THE HOROCYCLE AT 0
	subroutine horo0
	complex*16 mu,mu0,z,w,c,ztemp,c1,muchn
	complex*16 cen(20000),zfix1(20000),zfix2(20000)
	real*8 rad(20000),r1,xmin,ymin,xmax,ymax
	integer p,q,chntype,i,j
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	if(chntype .ne. 1) then
	   mu=mu0
	   call getchn(mu)
	   chntype =1
	end if
c	FIRST COMPUTE THE NEW TANGENT POINT.
c	IT IS ROTATED BY THE FIXER OF 1, T^-1 S T S^-1
	ztemp = (zfix1(q+1)-2)/(2*zfix1(q+1)-3)
c	ROTATE EACH CIRCLE CHAIN BY S_2 = | 1  0 |
c       | 2  1 |
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 j=-10,10
	   do 10 i=2,q+1
	      z=zfix1(i-1)/(2*j*zfix1(i-1)+1)
	      if(i .eq. q+1) then
		 w=ztemp/(2*j*ztemp+1)
	      else
		 w=zfix1(i)/(2*j*zfix1(i)+1)
	      end if
	      c= cen(i) - j*rad(i)*rad(i)/ dconjg(0.5+j*cen(i))
	      c = c/(2*j*c+1)
	      call orthog(z,w,c,c1,cdabs(z-c),r1)
	      call drwarc(z,w,c1,r1)
 10	   continue
	   call offgrf
	end
c       
c	DRAW HOROCYCLE AT INFINITY
	subroutine horoinf(delta)
	complex*16 delta,mu0,z,w,c,c1,mu,muchn
	complex*16 cen(20000),zfix1(20000),zfix2(20000)
	real*8 rad(20000),r1,xmin,ymin,xmax,ymax
	integer p,q,chntype,i,j
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	if(chntype .ne. 1) then
	   mu=mu0
	   call getchn(mu)
	   chntype =1
	end if
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 j=-1,1
	   do 10 i=2,q+1
	      z=zfix1(i-1)+delta+2.0d0*j
	      w=zfix1(i)+delta+2.0d0*j
	      c= cen(i)+delta+2.0d0*j
	      call orthog(z,w,c,c1,rad(i),r1)
	      call drwarc(z,w,c1,r1)
 10	   continue
	   call offgrf
	end
c       
c	GENERAL CIRCLE CHAINS
	subroutine genchn
	complex*16 mu0,mu,muchn
	real*8 trwr,xmin,ymin,xmax,ymax
	integer p,q,chntype
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
c	GET NEARBY MU
	trwr = 3.0d0
	write(*,*) 'Enter real trace W > 2 (default: ',trwr,')'
	read(*,*) trwr
	call getpq(mu,trwr)
	call getchn(mu)
	chntype=2
	call swindw(xmin,ymin,xmax,ymax)
	call ongrf(3)
	call ssegmt(xmin,dimag(mu),xmax,dimag(mu))
	call offgrf
	call drwchn(1,q+p+1)
	end
c       
c	DRAW TRANSLATED HOROCYCLE AT INFINITY
	subroutine flip
	complex*16 mu0,mu,muchn
	integer p,q,chntype
	common /crit/ mu0,muchn,p,q,chntype
c	GET NEARBY MU
	write(*,*) 'Enter nearby mu (complex): '
	write(*,*) 'Critical value:  ',mu0
	read(*,*) mu
	call horoinf(mu-mu0)
c	LABELLING
	write(2,100) dreal(mu),dimag(mu)
 100	format('Nearby mu = ',f10.7,'+',f10.7,'i')
	end
c       
c	PATTERN
c       
c	This program searches for all the circle chain patterns
c	corresponding to all solutions to trace W_p/q = +- 2
	subroutine pattern
	integer p,q,p0,q0,chntype,nummu,nrows,i,j,k,n,numlab
	real*8 x0,y0,x1,y1,xx0,yy0,xx1,yy1,xmin,ymin,xmax,ymax
	real*8 delta,sinch
	complex*16 mu0,mu,muchn,mulist(500)
	character*80 lines(10),title
	character*104 mulab
	character reply
	common /userin/ sinch
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p0,q0,chntype
c	GET LIST OF MUS
	p=p0
	q=q0
	x0=xmin
	y0=ymin
	x1=xmax
	y1=ymax
	write(*,*) 'Default search frame:'
	write(*,*) '      xmin = ',x0,'; ymin = ',y0
	write(*,*) '      xmax = ',x1,'; ymax = ',y1
	write(*,*) 'Change? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'y') then
	   write(*,*) 'Enter search frame: xmin,ymin,xmax,ymax '
	   read(*,*) x0,y0,x1,y1
	end if
	call musrch(mulist,(0.0d0,0.0d0),x0,y0,x1,y1,p,q,nummu)
c	ARRANGE FRAMES SO THAT THEY FIT ON PAGE
c	NROWS**2 SHOULD BE CLOSE TO NUMMU
	nrows = sqrt(float(nummu)) 
	if( nrows*nrows .lt. nummu) nrows = nrows+1
c	FOR EACH MU, PRINT OUT CIRCLE CHAIN
	chntype=1
	call rmargn(0.0d0,0.0d0,nrows*(xmax-xmin),nrows*(ymax-ymin),0.1d0)
	delta=0.1d0*sinch
	n=0
	do 90 j=1,nrows
	   do 90 i=1,nrows
	      if(n .ge. nummu) go to 92
	      n=n+1
	      mu = mulist(n)
	      call getchn(mu)
	      xx0 = xmin - (i-1)*(xmax-xmin+delta)
	      yy0 = ymin - (nrows-j)*(ymax-ymin+delta)
	      xx1 = xmax + (nrows-i)*(xmax-xmin+delta)
	      yy1 = ymax + (j-1)*(ymax-ymin+delta)
c	DRAW THE BASIC CHAIN OF CIRCLES
	      call rmargn(xx0,yy0,xx1,yy1,0.1d0)
	      call sbox(xmin,ymin,xmax,ymax,5)
	      call ongrf(3)
	      call smove(xmin,0.0d0)
	      call sline(xmax,0.0d0)
	      call offgrf
	      call drwchn(1,q+p+1)
 90	   continue
 92	   continue
c       
c	FINISH PICTURE
c	GET TITLE
	write(*,*) 'Enter title of picture: '
	read(*,'(A)') title
	if(title .ne. ' ') call stext(xx0,yy1+0.1*sinch,title)
c	LABELLING
	numlab=numlab+1
	write(lines(numlab),'(A)') 'Values of mu left to right'
	do 10 i=1,numlab
	   if(lines(i).ne.' ') 
	1	call stext(xx0+sinch,ymin-(0.05+i*0.15)*sinch,lines(i))
 10	continue
	y0 = ymin-(0.05+numlab*0.15)*sinch
	do 20 i=1,nummu,4
	   y0=y0-0.15*sinch
	   j=min(i+3,nummu)
	   write(mulab,130) (k,dreal(mulist(k)),dimag(mulist(k)),k=i,j)
 130	   format(4(I2,': ',f10.7,'+',f10.7,'i'))
	   call stext(xx0+sinch,y0,mulab)
 20	continue
	call off
	write(*,*) 'Finishing picture: do you want a new one? (y or n)'
	read(*,'(A)') reply
	if(reply .eq. 'n') stop
	call strtup
	end
c       
c	CHNLIM
c       
c	DRAWS AN APPROXIMATION TO A LIMIT OF CIRCLE CHAINS
	subroutine chnlim
	complex*16 pt(200000),mu,muchn
	integer p,q,chntype,index,i,j
	common /crit/ mu,muchn,p,q,chntype
c	M(q+1) = S
	pt(1) = -1.0d0
	pt(q+1) = 1.0d0
	index=1                  
	do 22 i=1,p
c	MULTIPLY : M(index+q-p) = T^-1 S M(index)
	   pt(index+q-p) = 1.0d0/(pt(index)-mu+2.0d0)
	   index=index+q-p
	   do 20 j=1,q+1
c	MULTIPLY BY T^-1 ON THE LEFT: M(index-p) = T^-1 M(index)
	      if(index .le. p+1) go to 22
	      pt(index-p) = 1.0d0/(pt(index)-mu)
	      index=index-p
 20	   continue
 22	continue
c	PLOT CURVE
	call ongrf(1)
	call smove(dreal(pt(1)),dimag(pt(1)))
	do 40 i=2,q+1
	   call sline(dreal(pt(i)),dimag(pt(i)))
 40	continue
	call offgrf
	end
c       
c	DRAW KEEN-SERIES HOROCYCLE
	subroutine kshoro
	complex*16 mu0,muchn,mu,z1,z2,w1,w2,u1,u2,c1
	complex*16 cen(20000),zfix1(20000),zfix2(20000)
	real*8 rad(20000),r1,xmin,ymin,xmax,ymax,trwr
	integer p,q,chntype,i,err
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	if(chntype .ne. 2) then
c	GET NEARBY MU
	   trwr = 3.0d0
	   write(*,*) 'Enter real trace W > 2 (default: ',trwr,')'
	   read(*,*) trwr
	   call getpq(mu,trwr)
	   call getchn(mu)
	   chntype =2
	end if
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
	do 10 i=2,q+1
	   z1=zfix1(i-1)
	   z2=zfix2(i-1)
	   w1=zfix1(i)
	   w2=zfix2(i)
	   call perp(z1,z2,w1,w2,u1,u2,c1,r1,err)
	   call drwarc(u1,u2,c1,r1)
 10	continue
	call drwarc(u1-2.0d0,u2-2.0d0,c1-2.0d0,r1)
	call offgrf
	end
c       
c	DRAW DELTA_0
	subroutine delta0
	complex*16 mu0,mu,z1,z2,w1,w2,u1,u2,v1,v2,c,w,muchn
	complex*16 cen(20000),zfix1(20000),zfix2(20000)
	complex*16 tmuinv(4),cenp,zp1,zp2,wm(4),xm(4),ym(4)
	complex*16 wim(4),xim(4),yim(4),zone,zexp,w1m(4),a1,a2
	real*8 rad(20000),r,rp,xmin,ymin,xmax,ymax,trwr
	integer p,q,chntype,err,p1,q1,p2,q2
	common /picwin/ xmin,ymin,xmax,ymax
	common /crit/ mu0,muchn,p,q,chntype
	common /circhn/ zfix1,zfix2,cen,rad
	data tmuinv(1)/(0.0d0,0.0d0)/,tmuinv(2)/(0.0d0,1.0d0)/
	data tmuinv(3)/(0.0d0,1.0d0)/
	if(chntype .eq. 0) then
c	GET NEARBY MU
	   trwr = 3.0d0
	   write(*,*) 'Enter real trace W > 2 (default: ',trwr,')'
	   read(*,*) trwr
	   call getpq(mu,trwr)
	   call getchn(mu)
	   chntype =2
	else
	   mu=muchn
	end if
	call drwchn(0,2)
c	COMPUTE DELTA'
	tmuinv(4)=(0.0d0,-1.0d0)*mu
	call linfrac(tmuinv,zfix1(p),zp1,err)
	call linfrac(tmuinv,zfix2(p),zp2,err)
	call lfapply(tmuinv,cen(p),cenp,rad(p),rp)
	call ongrf(3)
	call swindw(xmin,ymin,xmax,ymax)
	call scirc(dreal(cenp),dimag(cenp),rp)
	call offgrf
c	COMPUTE ORTHOGONAL ARCS
c	call ongrf(1)
c	call swindw(xmin,ymin,xmax,ymax)
	z1=zfix1(1)
c	z2=zfix2(1)
	w1=zfix1(q)-2.0d0
c	w2=zfix2(q)-2.0d0
c	call perp(z1,z2,w1,w2,u1,u2,c,r,err)
c	call drwarc(u1,u2,c,r)
c	call perp(z1,z2,zp1,zp2,v1,v2,c,r,err)
c	call drwarc(v1,v2,c,r)
c	call offgrf
c	COMPUTE CRITICAL WORD
	p2=p
	q2=q
	call mumatw(p2,q2,mu,wm)
	call nextpq(p2,q2,q2,1)
	p1=p-p2
	q1=q-q2
	call mumatw(p1,q1,mu,xm)
	call mumatw(p2,q2,mu,ym)
	call matinv(wm,wim)
	call matinv(xm,xim)
	call matinv(ym,yim)
c	COMPUTE FRACTIONAL POWER OF STABILIZING TRANSFORMATION
c	zexp = dble(p)/q
c	call matexp(wim,w1m,zexp)
c	call twocir(a1,a2,cen(2),c,rad(2),r,err)
c	if(cdabs(a1-cen(1)) .gt. rad(1)) a1=a2
c	call linfrac(w1m,a1,w,err)
c	write(*,'(A,f9.6,A,f9.6)') 'W^-p/q[a1] = ',dreal(w),'+i',dimag(w)
c	call slabel(dreal(a1),dimag(a1),0.2d0,0.0d0,'a1')
c	call slabel(dreal(w),dimag(w),0.5d0,100.0d0,'W^-p/q[a1]')
	call slabel(dreal(z1),dimag(z1),0.5d0,90.0d0,'z_0')
	call slabel(dreal(w1),dimag(w1),0.5d0,135.0d0,'z_-1')
	call slabel(dreal(zp1),dimag(zp1),0.5d0,0.0d0,'z''')
	call ongrf(11)
	call spoint(dreal(z1),dimag(z1))
	call spoint(dreal(w1),dimag(w1))
	call spoint(dreal(zp1),dimag(zp1))
c	call spoint(dreal(a1),dimag(a1))
c	call spoint(dreal(w),dimag(w))
	call offgrf
	zone=-1.0d0
	call linfrac(wm,zone,w,err)
	call slabel(dreal(w),dimag(w),0.5d0,0.0d0,'W[-1]')
	call ongrf(11)
	call spoint(dreal(w),dimag(w))
	call offgrf
	call linfrac(wim,zone,w,err)
	call slabel(dreal(w),dimag(w),0.5d0,110.0d0,'W^-1[-1]')
	call ongrf(11)
	call spoint(dreal(w),dimag(w))
	call offgrf
c	call linfrac(ym,u1,w,err)
c	write(*,'(A,f9.6,A,f9.6)') 'Y1[u1] = ',dreal(w),'+i',dimag(w)
c	call slabel(dreal(u1),dimag(u1),0.5d0,120.0d0,'u1')
c	call slabel(dreal(w),dimag(w),0.5d0,-90.0d0,'Y[u1]')
c	call ongrf(11)
c	call spoint(dreal(u1),dimag(u1))
c	call spoint(dreal(w),dimag(w))
c	call offgrf
	end
c
c       SET UP CIRCLE WEB WORDS
c          wts(MAXSIDE) = ARC INTERSECTION NUMBERS ON EACH SIDE
c          wcen(MAXSIDE), wrad(MAXSIDE) = CIRCLES FOR EACH SIDE
c          wbegpt(MAXSIDE), wmidpt(MAXSIDE), wendpt(MAXSIDE) = 
c                 ORTHOGONAL SIDES FOR EACH SIDE
c          wword1(MAXSIDE), wword2(MAXSIDE) = WORDS FOR ENDS OF ARCS
	subroutine setweb
	complex*16 y(4,MAXSIDE)
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	integer wts(MAXSIDE),mask(MAXSIDE),i,j
	character gentyp*20,answer*20,choice,
	1    genlet*MAXSIDE,reply*1,wrdlbl*MAXLEV
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	common /gens/ y, side, fside,  nside, gentyp
	common /gensym/ genlet
	common /cirweb/ wts
c       MAKE SURE GROUP IS SET
	if(.not.grpon) call setgrp
c
c       mask KEEPS TRACK OF WHICH SIDES HAVE BEEN DONE
c       FIRST INITIALIZE AS ALL 0's
	do 3 i=1,nside
	   mask(i)=0
 3	continue
	do 8 i =1, nside
	   if( mask(i) .eq. 1) go to 8
	   write(*,'(A,I3,A$)') 'Enter weight for side ',i,' : '
	   read(*,*) wts(i)
	   j=side(i)
	   wts(j)=wts(i)
	   mask(i) = 1
 8	continue
	end
