c       
c	MUFIND.FOR  (last updated 4/18/95)
c       
c       
c	Possible styles:
c       style=0  => Choose Interactively
c       style=1  => Maskit T_1,1
c       style=2  => Maskit Slice: tr Y and tr[X,Y] constant
c       style=3  => tr Y and tr[X,Y] determined linearly from tr X
c       style=4  => Riley T_0,4/Z
c       style=5  => Riley Slice: tr Y and tr XY constant
c       style=6  => tr Y and tr XY determined linearly from tr X
c       
	subroutine setfmt(style0)
	integer style0,style,trsty
	complex*16 t2,t3,t4,a1,a2,a3,a4,a5,a6
	character*80 line1,line2
	common /trfmt/ trsty
	common /trprms/ a1,a2,a3,a4,a5,a6
	common /trlbl/ line1,line2
	style=style0
	if(style .eq. 0) then
	   write(*,*) '1 = Maskit T_1,1'
	   write(*,*) '2 = Maskit-type slice '
	   write(*,*) '3 = Maskit-type linear slice'
	   write(*,*) '4 = Riley T_0,4/Z'
	   write(*,*) '5 = Riley-type slice '
	   write(*,*) '6 = Riley-type linear slice'
 3	   write(*,*) 'Enter type of slice: '
	   read(*,*) style
	   if( style .lt. 1 .or. style .gt. 6) go to 3
	end if
	trsty=style
c       
c	SET UP TRACE PARAMETERS
c       
c	MASKIT T_1,1
	if(trsty.eq.1) then
c	LABELLING
	   write(line1,100) 2.0d0,0.0d0
	   write(line2,200) -2.0d0,0.0d0
c	MASKIT-TYPE SLICES
	else if(trsty.eq.2) then
	   t2=2.0d0
	   t4=-2.0d0
	   write(*,*) 'Enter trace(Y) (Default= ',t2,')'
	   write(*,*) 'and trace([X,Y]) (Default= ',t4,')'
	   read(*,*) t2,t4
	   a1 = t2*t2*0.25d0 - 1.d0
	   a2 = t4+2.0d0-t2*t2
	   a3 = t2*0.5d0
	   a4 = t2
c	LABELLING
	   write(line1,100) dreal(t2),dimag(t2)
	   write(line2,200) dreal(t4),dimag(t4)
 100	   format('Trace Y        = ',f15.10,' +i ',f15.10)
 200	   format('Trace [X,Y]  = ',f15.10,' +i ',f15.10)
c	LINEARLY DETERMINED SLICE
	else if(trsty.eq.3) then
	   a1=0.0d0
	   a2=0.0d0
	   a3=2.0d0
	   write(*,*) 'Trace Y  = A*tr X +B*conjg[tr X]+C'
	   write(*,*) 'Enter A,B,C (Default=s ',a1,',',a2,',',a3,')'
	   read(*,*) a1,a2,a3
	   a4=0.0d0
	   a5=0.0d0
	   a6=-2.0d0
	   write(*,*) 'Trace [X,Y] = A*tr X +B*conjg[tr X]+C'
	   write(*,*) 'Enter A,B,C (Default=s ',a4,',',a5,',',a6,')'
	   read(*,*) a4,a5,a6
c	LABELLING
	   write(line1,110) dreal(a1),dimag(a1),dreal(a2),dimag(a2),
	1	dreal(a3),dimag(a3)
	   write(line2,210) dreal(a4),dimag(a4),dreal(a5),dimag(a5),
	1	dreal(a6),dimag(a6)
 110	   format('Tr Y   = [',f6.3,'+',f6.3,'i]*tr X +[',
	1	f6.3,'+',f6.3,'i]*cjg[tr X]+',f6.3,'+',f6.3,'i')
 210	   format('Tr[X,Y]= [',f6.3,'+',f6.3,'i]*tr X +[',
	1	f6.3,'+',f6.3,'i]*cjg[tr X]+',f6.3,'+',f6.3,'i')
c       
c	RILEY T_0,4/Z
	else if(trsty.eq.4) then
c	LABELLING
	   write(line1,300) 2.0d0,0.0d0
	   write(line2,400) 2.0d0,0.0d0
c	RILEY-TYPE SLICES
	else if(trsty.eq.5) then
	   t2=2.0d0
	   t3=2.0d0
	   write(*,*) 'Enter trace(Y) (Default= ',t2,')'
	   write(*,*) 'and trace( XY) (Default= ',t4,')'
	   read(*,*) t2,t3
	   a1 = t2
	   a2 = t3
c	LABELLING
	   write(line1,300) dreal(t2),dimag(t2)
	   write(line2,400) dreal(t3),dimag(t3)
 300	   format('Trace Y        = ',f15.10,' +i ',f15.10)
 400	   format('Trace  XY  = ',f15.10,' +i ',f15.10)
c	LINEARLY DETERMINED SLICE
	else if(trsty.eq.6) then
	   a1=0.0d0
	   a2=0.0d0
	   a3=2.0d0
	   write(*,*) 'Trace Y  = A*tr X +B*conjg[tr X]+C'
	   write(*,*) 'Enter A,B,C (Default=s ',a1,',',a2,',',a3,')'
	   read(*,*) a1,a2,a3
	   a4=0.0d0
	   a5=0.0d0
	   a6=2.0d0
	   write(*,*) 'Trace  XY = A*tr X +B*conjg[tr X]+C'
	   write(*,*) 'Enter A,B,C (Default=s ',a4,',',a5,',',a6,')'
	   read(*,*) a4,a5,a6
c	LABELLING
	   write(line1,310) dreal(a1),dimag(a1),dreal(a2),dimag(a2),
	1	dreal(a3),dimag(a3)
	   write(line2,410) dreal(a4),dimag(a4),dreal(a5),dimag(a5),
	1	dreal(a6),dimag(a6)
 310	   format('Tr Y   = [',f6.3,'+',f6.3,'i]*tr X +[',
	1	f6.3,'+',f6.3,'i]*cjg[tr X]+',f6.3,'+',f6.3,'i')
 410	   format('Tr XY= [',f6.3,'+',f6.3,'i]*tr X +[',
	1	f6.3,'+',f6.3,'i]*cjg[tr X]+',f6.3,'+',f6.3,'i')
	end if
	end
c       
	subroutine traces(t1,t2,t3)
	complex*16 t1,t2,t3,t4,a1,a2,a3,a4,a5,a6,temp,temp1
	integer trsty
	common /trfmt/ trsty
	common /trprms/ a1,a2,a3,a4,a5,a6
c       
c	CALCULATE TRACES
	if(trsty.eq.1) then
	   t3 = t1 - (0.0d0,2.0d0)
	   t2=2.0d0
	else if(trsty.eq.2) then
	   temp = a1*t1*t1 + a2
	   t3 = t1*a3 + (0.0d0,-1.0d0)*cdsqrt(-temp)
	   t2=a4
	else if(trsty.eq.3) then
	   t2 = a1*t1+a2*dconjg(t1)+a3
	   t4 = a4*t1+a5*dconjg(t1)+a6
	   temp = (0.25d0*t2*t2-1.0d0)*(t1*t1-4.0d0) +t4-2.0d0
	   t3 = 0.5d0*t1*t2 +(0.0d0,-1.0d0)*cdsqrt(-temp)
	else if(trsty.eq.4) then
	   t2 = 2.0d0
	   t3 = 2.0d0
	else if(trsty.eq.5) then
	   t2=a1
	   t3=a2
	else if(trsty.eq.6) then
	   t2 = a1*t1+a2*dconjg(t1)+a3
	   t3 = a4*t1+a5*dconjg(t1)+a6
	end if
	end
c       
c	CALCULATE TRACE ( WORD_P/Q ( X , Y ) )
c       
c	THE FRACTION P/Q MAY BE ANY RATIONAL NUMBER
c	INCLUDING -1/0 AND 1/0, ASSUMED IN LOWEST TERMS
c       
	subroutine tracew(p0,q0,trx,try,trxy,trw)
	complex*16 trx,try,trxy,trw,tr(3)
	integer p0,q0,p,q,p1,q1,p2,q2,pnext,qnext,trsty
	common /trfmt/ trsty
c       
c	SPECIAL CASE WHERE Q=0 OR P=0
	if(q0 .eq. 0) then
	   if(trsty .le. 3) then
	      trw = try
	   else
	      trw = 2.0d0
	   end if
	   return
	else if(p0 .eq. 0) then
	   trw = trx
	   return
	end if
c	SET UP INITIAL TRACES
c       MASKIT STYLE
	if(trsty .le. 3) then
	   if(dble(p0)/dble(q0) .gt. 0) then
	      p=abs(p0)
	      q=abs(q0)
	      tr(1) = trx
	      tr(2) = try
c	      tr(3) = trx*try-trxy
	      tr(3) = trxy
	   else
	      p=abs(q0)
	      q=abs(p0)
	      tr(1) = try
	      tr(2) = trx
c	      tr(3) = trxy
	      tr(3) = trx*try-trxy
	   end if
	else
c       RILEY STYLE
	   p=abs(p0)
	   q=abs(q0)
	   tr(1) = trx
	   tr(2) = try
	   tr(3) = try*trxy-trx
	end if
	p1=0
	q1=1
	p2=1
	q2=0

 10	pnext=p1+p2
	qnext=q1+q2
	if(trsty .le. 3) then
	   trw=tr(1)*tr(2)-tr(3)
	else
	   trw = -tr(1)*tr(2)-tr(3)
	   if( mod(qnext,2) .eq. 0) then
	      trw = trw+trxy*trxy+try*try
	   else
	      trw = trw +2.0d0*trxy*try
	   end if
	end if
	if(p .eq. pnext .and. q .eq. qnext) return
	if(dble(p)/q .lt. dble(pnext)/qnext) then
	   p2=pnext
	   q2=qnext
	   tr(3)=tr(2)
	   tr(2)=trw
	else
	   p1=pnext
	   q1=qnext
	   tr(3)=tr(1)
	   tr(1)=trw
	end if
	go to 10
	end
c
c	USES TRACEW IN INTERACTIVE APPLICATIONS
	subroutine gettrw(p0,q0,trx0,trw)
	integer p0,q0,p,q
	complex*16 trx0,trx,try,trxy,trw0,trw
	trx=trx0
	p=p0
	q=q0
	call traces(trx,try,trxy)
	call tracew(p,q,trx,try,trxy,trw0)
	trw=trw0
	end
c
c	FUNCTION USED IN NEWTON ITERATION
	subroutine funct(prm,val)
	complex*16 prm,val,trx,trw
	integer p,q,p0,q0
	common /fract/ p0,q0
	p=p0
	q=q0
	trx=prm
	call gettrw(p,q,trx,trw)
	val=trw
	end
c       
c	CALCULATE DERIVATIVE
	subroutine deriv(prm,dif)
	complex*16 dif,prm,prm0,t1,t2,t3,t4,difval
	real*8 h,valeps,prmeps,smidg,diverg
	integer maxitr,onflag,error
	common /ntprms/ h,valeps,prmeps,diverg,maxitr,onflag
	error = 0	
	smidg = prmeps*0.1d0
c	FOURTH-ORDER APPROXIMATION FOR DERIVATIVE
 5	prm0 = prm+smidg
	call funct(prm0,t1)	
	prm0 = prm-smidg
	call funct(prm0,t2)	
	prm0 = prm+dcmplx(0.0d0,smidg)
	call funct(prm0,t3)	
	prm0 = prm-dcmplx(0.0d0,smidg)
	call funct(prm0,t4)	
	difval = 0.25d0*(t1-t2-(0.0d0,1.0d0)*(t3-t4)) 
	dif = difval/smidg
	end
c
c	FINDS THE DERIVATIVE OF TRACEW AT TRX
c          USED IN REAL TRACE RAY CALCULATIONS
	subroutine diftrw(p,q,trx,ans)
	integer p,q,p0,q0
	complex*16 trx,trx0,dif,ans
	common /fract/ p0,q0
	p0=p
	q0=q
	trx0=trx
	call deriv(trx0,dif)
	ans=dif
	end
c	
c	PARAMETERS FOR NEWTON SUBROUTINE
	block data ntdata
	real*8 h,prmeps,valeps,diverg
	integer maxitr,onflag
	common /ntprms/ h,valeps,prmeps,diverg,maxitr,onflag
	data h/1.0d0/,prmeps/1.0d-13/,valeps/1.0d-4/
	data diverg/100.0d0/,maxitr/100/,onflag/0/
	end	
c       
c	REGULA FALSI METHOD TO SOLVE TRACE EQUATION
c       error = 1 MEANS OSCILLATION
c       error = 2 MEANS DIVERGENCE
c       error = 3 MEANS VANISHING DERIVATIVE
c
	subroutine newton(val,prm1,error)
	complex*16 val,val1,prm1,prm,prm0,difprm
	integer error,maxitr,onflag,i
	real*8 h,valeps,prmeps,diverg
c	ERROR MESSAGES
	character*30 errmsg(3)

	common /ntprms/ h,valeps,prmeps,diverg,maxitr,onflag

	data errmsg/'Oscillation.','Divergence.','Double root.'/

c	ASK ABOUT ADJUSTING PARAMETERS
	if(onflag .eq. 0) call chprms

	error =0
	prm = prm1
c	GET FIRST FUNC VALUE
	call funct(prm,val1)
	do 10 i =1,maxitr
	   prm0=prm
c       GET DERIVATIVE FUNC(PRM)
	   call deriv(prm,difprm)
c       DERIVATIVE VANISHES (ERROR = 3)
	   if ( difprm .eq. 0.0d0) then
	      error = 3
	      go to 999
	   end if
	   prm = prm + h*(val-val1)/difprm
	   call funct(prm,val1)
c       CONVERGENCE IF PRM CHANGES LITTLE AND VAL =VAL1
	   if(cdabs(prm-prm0) .le. prmeps*cdabs(prm) .and.
	1	cdabs(val1-val) .le. valeps*cdabs(val)) then
	      prm1 = prm
	      return
	   end if
c       DIVERGENCE (ERROR = 2)
	   if(cdabs(prm) .ge. diverg) then
	      error =2
	      go to 999
	   end if
 10	continue

c	OSCILLATION (ERROR = 1)
	error = 1

 999	continue
c$$$	write(*,*) errmsg(error)
	prm1 = prm
	end
c
c	ASK ABOUT CHANGING PARAMETERS
	subroutine chprms
	real*8 h,valeps,prmeps,diverg
	integer maxitr,onflag
	character*1 reply

	common /ntprms/ h,valeps,prmeps,diverg,maxitr,onflag

	onflag=1
	write(*,*) 'Parameters in Newton Iteration:'
	write(*,*) 'Mollifier = ',h
	write(*,*) 'Maximum error in value = ',valeps
	write(*,*) 'Maximum error in parameter = ',prmeps
	write(*,*) 'Divergence threshold = ',diverg
	write(*,*) 'Maximum iterations = ',maxitr
	write(*,*) 'Do you wish to change any of these? (y or n)'
	read(*,'(A)') reply
	if(reply .ne. 'y') return

	write(*,*) 'Enter mollifier: (default=',h,')'
	read(*,*) h
	write(*,*) 'Enter value epsilon: (default=',valeps,')'
	read(*,*) valeps
	write(*,*) 'Enter parameter epsilon: (default=',prmeps,')'
	read(*,*) prmeps
	write(*,*) 'Enter divergence limit: (default=',diverg,')'
	read(*,*) diverg
	write(*,*) 'Enter maximum iterations: (default=',maxitr,')'
	read(*,*) maxitr
	end
c
c	FINDS THE VALUE OF TRACE X SO THAT TRACE W = TRW
c	INITIALLY, TRX = FIRST GUESS
	subroutine soltrx(p,q,trx,trw,error)
	integer p,q,p0,q0,error,error0
	complex*16 trx,trw,trx0,trw0
	common /fract/ p0,q0
	trx0=trx
	trw0=trw 
	p0=p
	q0=q
	call newton(trw0,trx0,error0)
	trx=trx0
	error=error0
	end
c
c	FINDS THE VALUE OF MU SO THAT TRACE W = TRW
	subroutine solmu(p,q,mu,trw,error)
	integer p,q,p0,q0,error,error0
	complex*16 mu,trx,trw,trw0
	common /fract/ p0,q0
	trx=(0.0d0,-1.0d0)*mu
	trw0=trw
	p0=p
	q0=q
	call newton(trw0,trx,error0)
	mu=(0.0d0,1.0d0)*trx
	error=error0
	end
c
c	FINDS THE VALUE OF MU SO THAT TRACE W = 2
	subroutine solmu0(p,q,mu,error)
	integer p,q,p0,q0,error,error0,trsty
	complex*16 mu,trx,trw0
	common /trfmt/ trsty
	common /fract/ p0,q0
	trx=(0.0d0,-1.0d0)*mu
	trw0=2.0d0
	if(trsty .gt. 3) trw0=-trw0
	p0=p
	q0=q
	call newton(trw0,trx,error0)
	mu=(0.0d0,1.0d0)*trx
	error=error0
	end
c       
c	FIND SOLUTION TO TRACE(W_P/Q) = TRW by RECURSIVE GUESSING
	subroutine initrx(p0,q0,trx0,trw0,error0)
	complex*16 trx0,trx,trw0,trw,trw2,eta,trx1,trx2,try,trxy
	integer p,q,m,p0,q0,p1,q1,p2,q2,error,error0,trsty,i
	real*8 t
	common /trfmt/ trsty
	p = p0
	q = q0
c	ZERO DENOMINATOR DOESN'T WORK; SO RETURN
	if(q0 .eq. 0) return
c
	trw=trw0
c	INITIAL CASES
c       FIND BRACKETING FRACTIONS
	if(trsty .le. 3) then
c	MASKIT STYLE
	   m=p/q
	   if(p.lt.q*m) m=m-1
	   trx1=trw
	   do 31 i = 1, abs(m)
	      call soltrx(sign(m,i),1,trx1,trw,error)
 31	   continue
	   if(p .eq. m .and. q .eq. 1)  then
	      trx0=trx1
	      error0=error
	      return
	   end if
	   p1=m
	   q1=1
	   p2=m+1
	   q2=1
	   trx2=trx1
	   call soltrx(p2,q2,trx2,trw,error)
c       UNTWIST trw BY q
	   call zacosh(0.5*trw,eta)
	   eta=eta/q
	   call zcosh(eta,trw)
	   trw=2*trw
	else
c	RILEY STYLE
	   error0=0
	   m = p/(2*q)
	   if(p.lt.2*q*m) m=m-1
	   p=p-2*q*m
	   call traces(trx,try,trxy)
	   trx=try*trxy
	   if( p .eq. 0 ) then
	      trx0 = trw
	      return
	   else if(p .eq. 1 .and. q .eq. 1) then
	      trx0 = trx-trw
	      return
	   end if
c       UNTWIST trw BY q
	   call zacosh(-0.5*trw,eta)
	   eta=eta/q
	   call zcosh(eta,trw)
	   trw=-2*trw
c
	   call zcosh(2*eta,trw2)
	   trw2=-2*trw2
	   trx0 = 0.5d0*(trx-cdsqrt(trx*trx-
	1	4.0d0*(try*try+trxy*trxy-2.0d0-trw2)))
	   if(p .eq. 1 .and. q .eq. 2) return
	   if(p .eq. 3 .and. q .eq. 2) then
	      trx0= trx-trx0
	      return
	   end if
	   if( 2*p .lt. q ) then
	      p1=0
	      q1=1
	      trx1=trw
	      p2= 1
	      q2= 2
	      trx2=trx0
	   else if(p .lt. q) then
	      p1=1
	      q1=2
	      trx1=trx0
	      p2= 1
	      q2= 1
	      trx2=trx-trw
	   else if(2*p .lt. 3*q) then
	      p1=1
	      q1=1
	      trx1=trx-trw
	      p2= 3
	      q2= 2
	      trx2=trx-trx0
	   else 
	      p1=3
	      q1=2
	      trx1=trx-trx0
	      p2= 2
	      q2= 1
	      trx2=trw
	   end if
	end if
c	   
c	RECURSIVE APPROACH TO SOLUTION
 10	t = (1.0d0*q2)/(q1+q2)
	trx = t*trx2+(1-t)*trx1
	call zcosh((q1+q2)*eta,trw)
	if(trsty .le. 3) then
	   trw=2*trw
	else
	   trw=-2*trw
	end if
	call soltrx(p1+p2,q1+q2,trx,trw,error)
	if(p .eq. p1+p2 .and. q .eq. q1+q2) then
	   error0=error
	   trx0=trx
	   return
	else if( dble(p)/q .lt. dble(p1+p2)/(q1+q2) ) then
	   p2=p1+p2
	   q2=q1+q2
	   trx2=trx
	else
	   p1=p1+p2
	   q1=q1+q2
	   trx1=trx
	end if
	go to 10
	end
c       
c	FINDS THE VALUE OF MU SO THAT TRACE W = TRW
c	PROVIDES INITIAL GUESS
	subroutine inimu(p,q,mu,trw,error)
	integer p,q,p0,q0,error,error0
	complex*16 mu,trx,trw,trw0
	trw0=trw
	p0=p
	q0=q
	call initrx(p0,q0,trx,trw0,error0)
	mu=(0.0d0,1.0d0)*trx
	error=error0
	end
c
c	FINDS THE VALUE OF MU SO THAT TRACE W = 2
c	PROVIDES INITIAL GUESS
	subroutine inimu0(p,q,mu,error)
	integer p,q,p0,q0,error,error0,trsty
	complex*16 mu,trx,trw0
	common /trfmt/ trsty
	p0=p
	q0=q
	trw0=2.0d0
	if(trsty .gt. 3) trw0=-trw0
	call initrx(p0,q0,trx,trw0,error0)
	mu=(0.0d0,1.0d0)*trx
	error=error0
	end
c       
c	FIND SOLUTION TO TRACE(W_R) = 2 by RECURSIVE GUESSING
c	HERE R IS ANY REAL VALUE
	subroutine bndypt(r0,trx0,error0)
	complex*16 trx0,trx,trw
	integer p,q,i,j,error,error0,temp,level,a(45)
	real*8 r,r0
	r = r0
	do 10 level = 1,45
	   a(level)= r
	   if(r .lt. a(level)) a(level) = a(level)-1
	   r=r-a(level)
	   if(dabs(r) .le. 1.0d-8) go to 11
	   if(dabs(1.0d0-r) .le. 1.0d-8) then
	      a(level)=a(level)+1
	      go to 11
	   end if
	   r=1/r
 10	continue
 11	continue
	trx=2.0d0
	trw=2.0d0
	error=0
	do 20 i=1,level
	   p=a(i)
	   q=1
	   do 15 j=1,i-1
	      temp=p
	      p=a(i-j)*p+q
	      q=temp
 15	   continue
	   call soltrx(p,q,trx,trw,error)
	   if(error .ne. 0) then
	      write(*,*) 'Last fraction = ',p,'/',q
	      go to 90
	   end if
 20	continue
 90	error0=error
	trx0=trx
	end
c       
c	THIS PROGRAM SEARCHES FOR ALL ROOTS TO TRACE W_P/Q = +-2 COSH(ETA)
	subroutine musrch(mulist,eta0,x0,y0,x1,y1,p0,q0,nummu)
	integer p0,q0,nummu,p,q,error,numx,numy,oldnum,i,j,l,k
	complex*16 mulist(*), mu,mu0,trw,eta,eta0
	real*8 x0,y0,x1,y1,delx,dely,ratio
c       SEARCH PARAMS
	integer maxitr,onflag
	real*8 h,valeps,prmeps,diverg
	common /ntprms/ h,valeps,prmeps,diverg,maxitr,onflag
	p=p0
	q=q0
	eta=eta0
	nummu=0
	ratio=(y1-y0)/(x1-x0)
	numx= dsqrt(20.0d0*q/ratio)
	if(numx .lt. 1) numx=1
	numy= dsqrt(20.0d0*q*ratio)
	if(numy .lt. 1) numy=1
	delx=(x1-x0)/numx
	dely=(y1-y0)/numy
	mu0=dcmplx(x0,y0)
	call zcosh(eta,trw)
	trw=2.0d0*trw
	do 20 l=1,2
	   oldnum=nummu
	   do 10 i = 0, numx
	      do 10 j = 0, numy
		 mu = mu0 +dcmplx(i*delx,j*dely)
		 error = 0
		 call solmu(p,q,mu,trw,error)
		 if(error .ne. 0) go to 10
		 do 5 k=oldnum+1,nummu
		    if(cdabs(mu-mulist(k)).le.50*prmeps) go to 10
 5		 continue
		 nummu = nummu +1
		 mulist(nummu) = mu
		 if(nummu .eq. q+oldnum) go to 12
 10	   continue
 12	   trw=-trw
 20	continue
	if(nummu .lt. 2*q) write(*,100) nummu,p,q
 100	format('Only ',I6,' solutions at ',I4,'/',I4)
	end
c       
c	CALCULATE MASKIT WORD_P/Q ( X , Y ) 
c       
c	THE FRACTION P/Q MAY BE ANY RATIONAL NUMBER
c	INCLUDING -1/0 AND 1/0
c       
	subroutine matrxw(p0,q0,matx,maty,matw)
	external matmul
	complex*16 matx(4),maty(4),matw(4)
	complex*16 mata(4),matb(4),matc(4),matz(4)
	integer p0,q0,p,q,p1,q1,p2,q2
c	INITIALIZE MATRICES
	mata(1) = matx(1)
	mata(2) = matx(2)
	mata(3) = matx(3)
	mata(4) = matx(4)
	matb(1) = maty(1)
	matb(2) = maty(2)
	matb(3) = maty(3)
	matb(4) = maty(4)

c	SPECIAL CASE WHERE Q=0 OR P=0
	if(p0 .eq. 0 ) then
	   call matcpy(mata,matz)
	   go to 900
	else if(p0 .eq. 1 .and. q0 .eq. 0) then
	   call matcpy(matb,matz)
	   go to 900
	else if(p0 .eq. -1 .and. q0 .eq. 0) then
	   call matdiv(mata,matb,matc)
	   call matdiv(matc,mata,matz)
	   go to 900
	end if

	if((p0 .lt. 0 .and. q0 .lt. 0) .or.
	1    (p0 .gt. 0 .and. q0 .gt. 0)) then
C	START WITH A = X AND B = Y
	   p=abs(p0)
	   q=abs(q0)
	   p1=0
	   q1=1
	   p2=1
	   q2=0
	else
C	START WITH A = XY^-1 X^-1 AND B = X
	   p=-abs(p0)
	   q=abs(q0)
	   p1=-1
	   q1=0
	   p2=0
	   q2=1
	   call matdiv(mata,matb,matc)
	   call matdiv(matc,mata,matz)
	   call matcpy(mata,matb)
	   call matcpy(matz,mata)
	end if

c	UNRAVEL CONTINUED FRACTION EXPANSION OF FRAC
 10	call matmul(mata,matb,matz)
	if( p1+p2 .eq. p .and. q1+q2 .eq. q) go to 900
 	if( (p*1.0d0)/q .gt. ((p1+p2)*1.0d0)/(q1+q2)) then
	   p1 = p1+p2
	   q1 = q1+q2
	   call matcpy(matz,mata)
	else
	   p2 = p1+p2
	   q2 = q1+q2
	   call matcpy(matz,matb)
	end if
	go to 10
 900	matw(1) =matz(1)
	matw(2) =matz(2)
	matw(3) =matz(3)
	matw(4) =matz(4)
	end
c
c	GENERATE MATRICES WITH DESIRED TRACES
	subroutine matgen(x,y,t1,t2,t3)
	complex*16 x(4),y(4),t1,t2,t3,a,b,c
	a = (t1+cdsqrt(t1*t1-4.0d0))/2.0d0
	if( cdabs(a) .lt. 1.0d0) a=1.0d0/a
	b = (t2+cdsqrt(t2*t2-4.0d0))/2.0d0
	if( cdabs(b) .lt. 1.0d0) b=1.0d0/b
	c= t3 -a*b - 1.0d0/(a*b)
	x(1) = a
	x(2) = 1.0d0
	x(3) = 0.0d0
	x(4) = 1.0d0/a
	y(1) = b
	y(2) = 0.0d0
	y(3) = c
	y(4) = 1.0d0/b
	end
c
c	USES MATRXW IN INTERACTIVE APPLICATIONS
	subroutine mumatw(p,q,mu,matw)
	integer p,q,p0,q0
	complex*16 matx(4),maty(4),matw(4),matw0(4),mu
	p0=p
	q0=q
	matx(1) = 0.0d0
	matx(2) = (0.0d0,1.0d0)
	matx(3) = (0.0d0,1.0d0)
	matx(4) = (0.0d0,-1.0d0)*mu
	maty(1) = 1.0d0
	maty(2) = 2.0d0
	maty(3) = 0.0d0
	maty(4) = 1.0d0
	call matrxw(p0,q0,matx,maty,matw0)
	matw(1) = matw0(1)
	matw(2) = matw0(2)
	matw(3) = matw0(3)
	matw(4) = matw0(4)
	end
c       
c	FIND FRACTION FOLLOWING P/Q IN FAREY SEQUENCE OF ORDER DENOM
c	USES EUCLIDEAN ALGORITHM
c	IDIR=+1 --> NEXT ; IDIR=-1 --> PREVIOUS
c       
	subroutine nextpq(p0,q0,denom,idir)
	integer p0,q0,p,q,denom,p1,q1,p2,q2
	integer temp,a,idir,isign,k
	p1 = 0
	q1 = 1
	p2 = 1
	q2 = 0
c       NORMALIZE SO THAT q >= 0
	if( q0 .lt. 0) then
	   q0 = -q0
	   p0 = -p0
	end if
	p = p0
	q = q0
	isign=-idir
 10	a = p/q
	if(p.lt.a*q) a = a-1
	temp = p2
	p2 = a*p2 + p1
	p1=temp
	temp = q2
	q2 = a*q2 + q1
	q1=temp
	temp = q
	q = p - a*q
	p = temp
	isign=-isign
	if(q.ne.0) go to 10
c       COMPUTE THE NEXT P/Q
	k= (denom - isign*q1)/q0
	p0=k*p0+isign*p1
	q0=k*q0+isign*q1
	end
c
c	COMPLEX HYPERBOLIC COSINE
	subroutine zcosh(z,z1)
	complex*16 z,z1
	z1=cdexp(z)
	z1=0.5*(z1+1/z1)
	end
c
c	COMPLEX ARC HYPERBOLIC COSINE (CHOSEN WITH RE > 0)
	subroutine zacosh(z,z1)
	complex*16 z,z1,w
	real*8 u,v
	w=z+cdsqrt(z*z-1)
	u=dlog(cdabs(w))
	v=datan2(dimag(w),dreal(w))
	z1=dcmplx(u,v)
	if(u .lt. 0) z1=-z1
	end




