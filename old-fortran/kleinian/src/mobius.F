c	MOBIUS.F (6/28/90)
c
c	This is a library of general purpose routines
c	for dealing with mobius transformations and circles.
c
c
c       Circles are specified by (center,radius).
c       If radius < 0, then the circle is a straight line
c       passing through center with direction angle = -radius.
c
c       MAKES DETERMINANT 1
	subroutine matfix(n,m)
	complex*16 n(4),m(4),fac
	fac = cdsqrt(n(1)*n(4)-n(2)*n(3))
	m(1) = n(1)/fac
	m(2) = n(2)/fac
	m(3) = n(3)/fac
	m(4) = n(4)/fac
	end
c
c	C = A * B^-1
	subroutine matdiv(a,b,c)
	complex*16 a(4),b(4),c(4)
	c(1) = a(1)*b(4)-a(2)*b(3)
	c(2) = -a(1)*b(2)+a(2)*b(1)
	c(3) = a(3)*b(4)-a(4)*b(3)
	c(4) = -a(3)*b(2)+a(4)*b(1)
	end
c
c	C = A * B
	subroutine matmul(a,b,c)
	complex*16 a(4),b(4),c(4)
	c(1) = a(1)*b(1)+a(2)*b(3)
	c(2) = a(1)*b(2)+a(2)*b(4)
	c(3) = a(3)*b(1)+a(4)*b(3)
	c(4) = a(3)*b(2)+a(4)*b(4)
	end
c
c	B = A
	subroutine matcpy(a,b)
	complex*16 a(4),b(4)
	b(1)=a(1)
	b(2)=a(2)
	b(3)=a(3)
	b(4)=a(4)
	end
c       
c	B = A^-1
	subroutine matinv(a,b)
	complex*16 a(4),b(4),det
	det=cdsqrt(a(1)*a(4)-a(2)*a(3))
	b(1) = a(4)/det
	b(2) = -a(2)/det
	b(3) = -a(3)/det
	b(4) = a(1)/det
	end
c
c       B = CONJG(A)
	subroutine mconjg(a,b)
	complex*16 a(4),b(4)
	b(1) = dconjg(a(1))
	b(2) = dconjg(a(2))
	b(3) = dconjg(a(3))
	b(4) = dconjg(a(4))
	end
c
c	FIXED POINTS (Z1 = ATTRACTIVE, Z2 = REPELLING)
c		ERR = 0 => 2 DISTINCT FIXED POINTS
c		ERR = 1 => 1 FINITE PARABOLIC FIXED POINT
c		ERR = 2 => 1 FINITE, 1 INFINITE
c		ERR = 3 => 1 INFINITE PARABOLIC FIXED POINT
c
c       NOTE: THE MULTIPLIER IS (m(3)*z1+m(4))^2
c
	subroutine fixpt(m,z1,z2,err)
	complex*16 m(4),z1,z2,det,qdet,t,z
	real*8 zero,infty
	integer err
	parameter (zero=1.0d-10,infty=1.0d10)
	if(cdabs(m(3)) .ge. zero) then
	   det = m(1)*m(4)-m(2)*m(3)
	   qdet = cdsqrt(det)
	   t = m(1) + m(4)
	   if(cdabs(t-2.0d0*qdet).le.zero
	1	.or.cdabs(t+2.0d0*qdet).le.zero) then
	      err=1
	      z1=0.5d0*(m(1)-m(4))/m(3)
	      z2=z1
	   else
	      err=0
	      z = cdsqrt( t*t - 4.0d0*det )
	      if( cdabs(t+z) .lt. 2.0d0*cdabs(qdet) ) z=-z
	      z1=0.5d0*(m(1)-m(4)+z)/m(3)
	      z2=z1-(z/m(3))
	   end if
	else if(cdabs(m(1)-m(4)) .ge. zero) then
	   if( cdabs(m(1)/m(4)) .lt. 1.0d0) then
	      z1= m(2)/(m(4)-m(1))
	      z2=infty
	   else
	      z2= m(2)/(m(4)-m(1))
	      z1=infty
           end if
	   err=2
	else
	   z1=infty
	   z2=infty
	   err=3
	end if
	end
c       
c	APPLY A LINEAR FRACT. TRANSFORMATION TO A POINT
c         ERR=0 -> finite result
c         ERR=1 -> infinite result
	subroutine linfrac(m,z,w,err)
	complex*16 m(4),z,w,t
	integer err
	real*8 zero,infty
	parameter (zero=1.0d-8,infty=1.0d8)
	t= m(3)*z+m(4)
	if(cdabs(t) .gt. zero) then
	   w= (m(1)*z+m(2))/t
	   err=0
	else
	   w= infty
	   err=1
	end if
	end
c       
c	APPLY A LINEAR FRACT. TRANSFORMATION TO A CIRCLE
	subroutine lfapply(m,c0,c,r0,r)
	complex*16 m(4),c0,c,z,w
	real*8 r0,r,pi,zero,infty
	parameter (zero=1.0d-10,infty=1.0d10,
	1    pi=3.14159265358979323844d0)
c	STRAIGHT LINE
	if(r0 .lt. 0.0) then
c	REMAINS A STRAIGHT LINE
	   if( cdabs(m(3)) .le. zero) then
	      c = (m(1)*c0+m(2))/m(4)
	      r=r0+datan2(dimag(m(4)/m(1)),dreal(m(4)/m(1)))
	      if( r .ge. 0.0) r= r-2.0d0*pi
	   else
	      c=-m(4)/m(3)
	      z=dcmplx(dcos(-r0),dsin(-r0))
	      c=c0+dconjg((c-c0)/z)*z
	      w=(m(3)*c+m(4))
	      if(cdabs(w) .ge. zero) then
		 c=(m(1)*c+m(2))/w
		 r=cdabs((m(1)/m(3))-c)
	      else
		 w=(m(1)*c+m(2))/(m(3)*z)
		 r=-datan2(dimag(w),dreal(w))
		 if(r .ge. 0.0) r=r-2.0d0*pi
		 c=m(1)/m(3)
	      end if
	   end if
c	TRUE CIRCLE
	else
c	REMAINS A CIRCLE
	   if( cdabs(m(3)) .le. zero) then
	      c = (m(1)*c0+m(2))/m(4)
	      r=r0*cdabs(m(1)/m(4))
c	GOES TO A CIRCLE
	   else
	      c=-m(4)/m(3)
	      if(cdabs(c-c0) .ge. zero) then
		 w=c0+r0*r0/(dconjg(c-c0))
		 z=m(3)*w+m(4)
		 if(cdabs(z) .ge. zero) then
		    c=(m(1)*w+m(2))/z
		    w=(m(1)*(c0+r0)+m(2))/(m(3)*(c0+r0)+m(4))
		    r=cdabs(w-c)
		 else
		    z=2.0d0*c0-w
		    c=(m(1)*z+m(2))/(m(3)*z+m(4))
		    z=(m(1)*w+m(2))/(m(3)*(0.0d0,1.0d0)*(w-c0))
		    r=-datan2(dimag(z),dreal(z))
		    if(r .ge. 0.0) r= r-2.0d0*pi
		 end if
	      else
		 c=m(1)/m(3)
		 z=(m(1)*(c0+r0)+m(2))/(m(3)*(c0+r0)+m(4))
		 r=cdabs(z-c)
	      end if
	   end if
	end if
	end
c       
c	THIS ROUTINE COMPUTES THE ARC ORTHOGONAL TO THE CIRCLE 
C	WITH CENTER cen AND RADIUS r AND WITH ENDPOINTS p1 AND p2, 
C	NOT NECESSARILY ASSUMED TO BE ON THE CIRCLE.
c       
c	IF r < 0, THEN THE CIRCLE IS A STRAIGHT LINE PASSING THROUGH cen 
c	WITH DIRECTION ANGLE -r
	subroutine orthog(p1,p2,c,c1,r,r1)
	complex*16 p1,p2,z1,z2,z3,c,c1,cen,cen1,w1,w2
	real*8 r,r1,rad,rad1,pi,zero,infty
	logical yesno
	parameter (zero=1.0d-10,infty=1.0d10,
	1    pi=3.14159265358979323844d0)
	z1=p1
	z2=p2
	cen=c
	rad=r
	call oncirc(z1,cen,rad,yesno)
c	z1 DOES NOT LIE ON CIRCLE
	if(.not.yesno) then
	   call reflec(z1,z3,cen,rad)
	   call cir3pt(z1,z2,z3,cen1,rad1)
	   go to 900
	end if
	call oncirc(z2,cen,rad,yesno)
c	z2 DOES NOT LIE ON CIRCLE
	if(.not.yesno) then
	   call reflec(z2,z3,cen,rad)
	   call cir3pt(z1,z2,z3,cen1,rad1)
	   go to 900
	end if
c	BOTH z1 AND z2 LIE ON CIRCLE
	if(rad .lt. 0.0) then
	   cen1 = (z1+z2)/2.0d0
	   rad1 = cdabs(z1-z2)/2.0d0
	else
	   w1=z1-cen
	   w2=z2-cen
	   if(cdabs(w1+w2) .lt. zero) then
	      cen1= cen
	      rad1= -datan2(dimag(w1),dreal(w1))
	      if(rad1 .ge. 0.0) rad1=rad1-2.0d0*pi
	   else
	      cen1 = cen + 2*(rad*rad)/dconjg(w1+w2)
	      rad1= cdabs(z1-cen1)
	   end if
	end if
 900	c1=cen1
	r1=rad1
	end
c       
c	TEST IF z IS ON THE CIRCLE OF RADIUS r AND CENTER c
	subroutine oncirc(z,c,r,yesno)
	complex*16 z,c,u
	real*8 r,r1,zero,infty
	logical yesno
	parameter (zero=1.0d-10,infty=1.0d10)
	if(r .lt. 0.0) then
	   r1=datan2(dimag(z-c),dreal(z-c))
	   u=dcmplx(dcos(2.0d0*(r+r1)),dsin(2.0d0*(r+r1)))
	   yesno = cdabs(u-1.0d0) .lt. zero
	else
	   yesno = dabs(cdabs(z-c)-r) .lt. zero
	end if
	end
c       
c	zr = REFLECTION OF z IN CIRCLE (c,r)
	subroutine reflec(z,zr,c,r)
	complex*16 z,zr,c,u
	real*8 r
	if(r .lt. 0.0) then
	   u=dcmplx(dcos(2.0d0*r),-dsin(2.0d0*r))
	   zr = c + dconjg(z-c)*u
	else
	   zr = c + r*r/dconjg(z-c)
	end if
	end
c       
c	m = MATRIX OF REFLECTION IN CIRCLE (c,r)
	subroutine rflmat(m,c,r)
	complex*16 m(4),n(4),c
	real*8 r
	if(r .lt. 0.0) then
	   n(1) = dcmplx(dcos(2.0d0*r),-dsin(2.0d0*r))
	   n(2) = c-n(1)*dconjg(c)
	   n(3) = 0.0d0
	   n(4) = 1.0d0
	else
	   n(1) = c
	   n(3) = 1.0d0
	   n(4) = -dconjg(c)
	   n(2) = r*r+n(1)*n(4)
	end if
	call matfix(n,m)
	end
c
c       SCHOTTKY STRETCHING MAP
c         MAPS C2 ONTO C1
	subroutine stretch(m,c1,r1,c2,r2)
	external matmul
	complex*16 m(4),m1(4),m2(4),m3(4),m4(4),c1,c2
	real*8 r1,r2
	call rflmat(m1,c1,r1)
	call rflmat(m2,c2,r2)
	call mconjg(m2,m3)
	call matmul(m1,m3,m4)
	call msqrt(m4,m)
	end
c       
c	FINDS THE CIRCLE (c,r) THRU THREE POINTS z1,z2,z3
	subroutine cir3pt(p1,p2,p3,c,r)
	complex*16 p1,p2,p3,z1,z2,z3,c,m(4),mi(4),Rcen,cnew
	real*8 r,Rrad,rnew
	data Rcen/(0.0d0,0.0d0)/, Rrad/-3.14159265358979323844d0/
	z1=p1
	z2=p2
	z3=p3
	call mat3pt(m,z1,z2,z3)
	call matinv(m,mi)
	call lfapply(mi,Rcen,cnew,Rrad,rnew)
	c=cnew
	r=rnew
	end
c       
c	FINDS A MATRIX m THAT SEND z1,z2,z3 TO 0, infty, 1
	subroutine mat3pt(n,z1,z2,z3)
	complex*16 z1,z2,z3,m(4),n(4)
	m(1) = z2-z3
	m(2) = -z1*m(1)
	m(3) = z1-z3
	m(4) = -z2*m(3)
	call matfix(m,n)
	end
c       
c	FINDS THE ARC THAT STARTS AT begpt, TRAVELS IN DIRECTION dir
c       AND ENDS AT endpt
c       VALUES COMPUTED AND RETURNED:
c           (c,r) IS CIRCLE OF ARC
c           midpt IS MIDPOINT
	subroutine arc2pt(begpt,endpt,dir,c,midpt,r)
	complex*16 begpt,endpt,dir,c,midpt,z1,z2,z3,zdir,
	1    m(4),mi(4),Rcen,cnew,mp
	real*8 r,Rrad,rnew,Rmid
	integer err
	data Rcen/(0.0d0,0.0d0)/, Rrad/-3.14159265358979323844d0/,
	1    Rmid/(1.0d0,0.0d0)/
	z1=begpt
	z2=endpt
	zdir=dir/cdabs(dir)
	call mat2pt(m,z1,z2,zdir)
	call matinv(m,mi)
	call lfapply(mi,Rcen,cnew,Rrad,rnew)
	c=cnew
	r=rnew
	call linfrac(mi,Rmid,z3,err)
	midpt = z3
	end
c       
c	FINDS A MATRIX m THAT SEND z1,z2 TO 0, infty
c       AND DIRECTION dir AT z1 TO +1
	subroutine mat2pt(n,z1,z2,dir)
	complex*16 z1,z2,dir,m(4),n(4)
	m(1) = z1-z2
	m(2) = -z1*m(1)
	m(3) = dir
	m(4) = -z2*m(3)
	call matfix(m,n)
	end
c       
c	FINDS A MATRIX m THAT SEND z1,z2,z3 TO w1,w2,w3
	subroutine mat6pt(m,z1,z2,z3,w1,w2,w3)
	complex*16 z1,z2,z3,w1,w2,w3,m(4),u(4),v(4)
	call mat3pt(u,z1,z2,z3)
	call mat3pt(v,w1,w2,w3)
	m(1) = v(4)*u(1)-v(2)*u(3)
	m(2) = v(4)*u(2)-v(2)*u(4)
	m(3) = -v(3)*u(1)+v(1)*u(3)
	m(4) = -v(3)*u(2)+v(1)*u(4)
	end
c       
c	FINDS SCHOTTKY TRANSFORMATION
c         m maps circle(c2,r2) onto circle (c1,r1)
c         If delfp = twist = 0, the transformation preserves
c         the perpendicular bisector of the two hemispheres with
c         no twisting.
c         DELFP measures the displacement of the attractive fixed point
c         toward the circle.
c         TWIST (degrees) is an angle giving the rotation around the
c         perpendicular bisector.
c
c       CURRENTLY THIS ASSUMES BOTH CIRCLES ARE FINITE CIRCLES
	subroutine schott(m,c1,c2,r1,r2,delfp,twist,err)
	external matmul
	complex*16 m(4),n1(4),n2(4),n3(4),n4(4),
	1    c1,c2,z1,z2,w1,delfp,b(4),b1(4),u,fp
	real*8 r1,r2,twist,raddeg,zero,x
	integer err
	parameter (raddeg=0.01745329251994329576923690768d0,
	1    zero=1.0d-10)
c       STRETCH MATRICES MAPS C2 ON C1
	call stretch(n2,c1,r1,c2,r2)
	call fixpt(n2,z1,z2,err)
c       MAP TO UNIT DISK
	call stretch(n3,(0.0d0,0.0d0),1.0d0,c1,r1)
	call linfrac(n3,z1,w1,err)
	call matinv(n3,n4)
	if( dabs(cdabs(w1)-1.0d0) .gt. zero ) then
c       FIXED POINT OF BLASCHKE TRANSFORMATION
	   fp = (w1+delfp)/(1.0d0+w1*dconjg(delfp))
	   u = cdexp(dcmplx(0.0d0,twist*raddeg))
c       BLASCHKE TRANSFORMATION
	   b1(1)= u-cdabs(fp*fp)
	   b1(2)= fp*(1.0d0-u)
	   b1(3)= dconjg(fp)*(u-1.0d0)
	   b1(4)= 1-u*cdabs(fp*fp)
	else
c       REAL PART OF DELFP GIVES DISPLACEMENT OF FIXED POINT IN DEGREES
	   u = cdexp(dcmplx(0.0d0,dreal(delfp)*raddeg))
	   fp = w1*u
c       IMAGINARY PART OF DELFP AND TWIST GIVE STABILIZING ELEMENT OF 1
c          IM(DELFP) MUST BE > 0
	   x=dtan(0.5d0*twist*raddeg)
	   b1(1)= u*dcmplx(dcosh(dimag(delfp)), x)
	   b1(2)= fp*dcmplx(dsinh(dimag(delfp)),-x)
	   b1(3)= u*dconjg(b1(2))
	   b1(4)= dconjg(b1(1))
	end if
	call matfix(b1,b)
	call matmul(n3,n2,n1)
	call matmul(b,n1,n2)
	call matmul(n4,n2,m)
	end
c	
c	CHECKS IF z1,z2,z3,z4 LIE ON ONE CIRCLE
C       YESNO = TRUE MEANS THEY DO
C       FALSE MEANS THEY DON'T
	subroutine samcir(z1,z2,z3,z4,yesno)
	complex*16 z1,z2,z3,z4,r
	real*8 zero,infty
	logical yesno
	parameter (zero=1.0d-10,infty=1.0d10)
	r=(z1-z2)*(z3-z4)*dconjg((z1-z3)*(z2-z4))
	yesno = (cdabs(r) .gt. infty) .or. (dimag(r) .lt. zero)
	end
c       
c	COMPUTE THE INTERSECTION OF THE TWO LINES (Z1,Z2) AND (W1,W2)
c       ERR = 1 => PARALLEL LINES
	subroutine fourpt(z1,z2,w1,w2,p,err)
	complex*16 z1,z2,w1,w2,p,a,b
	real*8 r, zero,infty
	integer err
	parameter (zero=1.0d-10,infty=1.0d10)
	a=z1-z2
	b=w2-w1
	r=dimag(b*dconjg(a))
	if(dabs(r) .lt. zero) then
	   err =1
	   return
	end if
	p=(b*dimag(z2*dconjg(z1))+a*dimag(w2*dconjg(w1)))/r
	err=0
	return
	end
c       
c	COMPUTE THE CIRCLE THAT BISECTS THE ARCS (Z1,Z2) AND
C	(W1,W2) (ALL FOUR POINTS MUST LIE ON ONE CIRCLE)
c       ERR =1 => NOT ON SAME CIRCLE
	subroutine bisect(z1,z2,w1,w2,c,r,err)
	complex*16 z1,z2,w1,w2,c,p1,p2,q1,q2,c1
	real*8 r,pi,zero,infty
	integer err,err1
	logical yesno
	parameter (zero=1.0d-10,infty=1.0d10,
	1    pi=3.14159265358979323844d0)
	p1=z1
	p2=z2
	q1=w1
	q2=w2
	call samcir(p1,p2,q1,q2,yesno)
	if(.not.yesno) then
	   err =1 
	   return
	end if
	call fourpt(p1,p2,q1,q2,c1,err1)
	if(err1.eq.0) then
	   c=c1
	   r=dsqrt(cdabs((z1-c1)*(z2-c1)))
	   err=0
	else
	   c=(z1+z2)/2.0d0
	   r = -datan2(dreal(p1-p2),dimag(p2-p1))
	   if(r .ge. 0.0d0) r=r-2.0d0*pi
	end if
	end
c       
c	CALCULATES THE INTERSECTION OF TWO CIRCLES
c       err = 0 => TWO FINITE POINTS
c       err = 1 => ONE FINITE, ONE INFINITE
c       err = 2 => ONE FINITE TANGENT POINT
c       err = 3 => ONE INFINITE TANGENT POINT
c       err = 4 => OVERLAPPING CIRCLES
c       err = 5 => NONINTERSECTING
	subroutine twocir(z1,z2,c1,c2,r1,r2,err)
	complex*16 z1,z2,c1,c2,u,u1,u2
	real*8 r1,r2,l,x,y,zero,infty
	integer err
	parameter (zero=1.0d-10,infty=1.0d10)
	if(r1 .ge. 0.0d0 .and. r2 .ge.0.0d0) then
	   l = cdabs(c1-c2)
	   if(l .gt. r1+r2 .or. l.lt. dabs(r1-r2)) then
	      err=5
	      return
	   else if(l.eq.0.0.and.r1.eq.r2) then
	      err=4
	      z1=c1+r1
	      z2=c1-r1
	      return
	   else if(l .eq. r1+r2.or. l.eq.dabs(r1-r2)) then
	      err=2
	      x=(r1*r1-r2*r2)/(l*l)
	      z1=0.5d0*((1.0d0+x)*c2+(1.0d0-x)*c1)
	      z2=z1
	      return
	   else
	      err=0
	      x=(r1*r1-r2*r2)/(l*l)
	      x=(1.0d0+x)/2.0d0
	      z1=x*c2+(1.0d0-x)*c1
	      y=dsqrt(r1*r1-x*x*l*l)
	      z1=z1+(y/l)*(0.0d0,1.0d0)*(c2-c1)
	      z2=z1-(y/l)*(0.0d0,2.0d0)*(c2-c1)
	      return
	   end if
	else if(r1 .ge. 0.0d0) then
	   u = dcmplx(dcos(r2),-dsin(r2))
	   z1=c1+(0.0d0,1.0d0)*u*dimag((c2-c1)*dconjg(u))
	   l=cdabs(z1-c1)
	   if(l.gt. r1) then
	      err=5
	      return
	   else if(l .eq. r1) then
	      err=2
	      z2=z1
	      return
	   else
	      err=0
	      y=dsqrt((r1*r1-l*l))
	      z1=z1+y*u
	      z2=z1-2.0d0*y*u
	      return
	   end if
	else if(r2 .ge. 0.0d0) then
	   u = dcmplx(dcos(r1),-dsin(r1))
	   z1=c2+(0.0d0,1.0d0)*u*dimag((c1-c2)*dconjg(u))
	   l=cdabs(z2-c2)
	   if(l.gt. r2) then
	      err=5
	      return
	   else if(l .eq. r2) then
	      err=2
	      z2=z1
	      return
	   else
	      err=0
	      y=dsqrt((r2*r2-l*l))
	      z1=z1+y*u
	      z2=z1-2.0d0*y*u
	      return
	   end if
	else 
	   u1=dcmplx(dcos(r1),-dsin(r1))
	   u2=dcmplx(dcos(r2),-dsin(r2))
	   u=u1*u1-u2*u2
	   if(cdabs(u).le.zero) then
	      if(dabs(dimag((c2-c1)/u1)).le. zero) then
		 err=4
		 z1=c1
		 z2=c2
		 return
	      else
		 err=3
		 z1=infty
		 z2=infty
		 return
	      end if
	   else
	      err=2
	      z1=c1-c2
	      z1=z1-dconjg(z1/u)
	      z2=z1
	      return
	   end if
	end if
	end
c       
c	COMPUTES ENDPOINTS OF PERPENDICULAR BISECTOR OF
c	(z1,z2) and (w1,w2)
c       ERR=1 => NOT ON ONE CIRCLE
c	(u1,u2) = ENDPOINTS ON CIRCLE THRU (z1,z2,w1,w2)
c	(c,r) = CIRCLE CONTAINING u1,u2,v1,v2
	subroutine perp(z1,z2,w1,w2,u1,u2,c,r,err)
	complex*16 z1,z2,w1,w2,u1,u2,c,c0,c1
	complex*16 p1,p2,q1,q2,a1,a2,cr
	real*8 r,r0,r1
	integer err,err1
	p1=z1
	p2=z2
	q1=w1
	q2=w2
	call bisect(p1,p2,q1,q2,c1,r1,err1)
	c=c1
	r=r1
	err=err1
	if(err .eq. 1) return
	call cir3pt(p1,p2,q1,c0,r0)
	call twocir(a1,a2,c0,c1,r0,r1,err1)
c	USE CROSS-RATIO TO ARRANGE THAT (z1,u1,z2,u2) IS COUNTERCLOCKWISE
	cr = (z1-a1)*(z2-a2)*dconjg((z1-z2)*(a1-a2))
	if(dreal(cr) .gt. 0) then
	   u1=a1
	   u2=a2
	else
	   u1=a2
	   u2=a1
	end if
	end
c       
c	MATRIX EXPONENTIATION
c       M2 = M^Z
c       MEANING FIXED POINTS OF M2 = FIXED POINTS OF M
c       AND MULTIPLIER OF M2 = ( MULTIPLIER OF M )^Z
	subroutine matexp(m,m2,z)
	complex*16 m(4),m1(4),m2(4),z,z1,z2,l,m0(4),z0,l2,x
	integer err
	z0=z
	go to 10
	entry msqrt(m,m2)
	z0=0.5d0
 10	m0(1)=m(1)
	m0(2)=m(2)
	m0(3)=m(3)
	m0(4)=m(4)
	call fixpt(m0,z1,z2,err)
c	TWO DISTINCT FIXED POINTS
	if(err .eq. 0 .or. err .eq. 2) then
	   l=m0(3)*z1+m0(4)
	   l=(m0(1)*m0(4)-m0(2)*m0(3))/(l*l)
	   call zexpon(l,z0,l2)
	   if(err.eq.0) then
	      m1(1)=z1-l2*z2
	      m1(2)=z1*z2*(l2-1)
	      m1(3)=1-l2
	      m1(4)=l2*z1-z2
	   else
	      m1(1)=l2
	      m1(2)=z1*(1-l2)
	      m1(3)=0
	      m1(4)=1
	   end if
c	INFTY = PARABOLIC FIXED POINT
	else if(err.eq.3) then
	   l=m0(1)
	   call zexpon(l,z0,l2)
	   m1(1)=l2
	   m1(2)=l2*z0*m0(2)
	   m1(3)=0
	   m1(4)=l2
c	FINITE PARABOLIC FIXED POINT
	else 
	   l=m0(1)-z1*m0(3)
           call zexpon(l,z0,l2)
           x=z0*m(3)/l
	   m1(1)=l2*(1+x*z1)
	   m1(2)=-l2*x*z1*z1
	   m1(3)=l2*x
	   m1(4)=l2*(1-x*z1)
	end if
	call matfix(m1,m2)
	end
c	
c	COMPLEX EXPONENTIAL: t= z^w
	subroutine zexpon(z,w,t)
	complex*16 z,w,t,v
	real*8 theta
	theta=datan2(dimag(z),dreal(z))
	v=dcmplx(dlog(cdabs(z)),theta)
	t=cdexp(v*w)
	end


