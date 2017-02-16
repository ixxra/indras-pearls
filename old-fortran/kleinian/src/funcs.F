c
c	PLOT A CURVE
	subroutine curplt
        integer penw,nstep
        real*8 a,b,c,delta,inc,iline,icirc,r,s,t
	real*8 du,umin,umax,umid1,umid2,ucrit
	real*8 xmin,ymin,xmax,ymax,sinch
	character reply
        common /funprm/ a,b,r,s,inc,nstep
	common /picwin/ xmin,ymin,xmax,ymax
        common /userin/ sinch
        write(*,*) 'Plot a parametrized curve.'
        write(*,*) 'Enter line thickness (pixels): '
        read(*,*) penw
	call gfxdoc('Begin curve')
        call ongrf(penw)
	call swindw(xmin,ymin,xmax,ymax)
c       GET PARAMETERS
c           n = index of straight line
c           m = index of circle
c           delta = spacing of lines (in device inches)
c           inc = increment in m variable.
        write(*,*) 'Enter number of steps in each segment: '
        read(*,*) nstep
        write(*,*) 'Enter direction vector (a,b) (2 real*8): '
        read(*,*) a,b
c	MAKE SURE DIRECTION VECTOR HAS NORM 1
	c=dsqrt(a*a+b*b)
	a=a/c
	b=b/c
        write(*,*) 'Enter line spacing (inches): '
        read(*,*) delta
	delta=delta*sinch
        write(*,*) 'Enter increment pattern (real*8): '
        read(*,*) inc
	r=2.0d0*inc/(dsqrt(3.0d0)*delta)
 10	write(*,*) 'Enter line index, circle index (real*8): '
        read(*,*) iline, icirc
        s = (2.0d0*(icirc - inc*iline)-1.0d0)/dsqrt(3.0d0)
        du = dsqrt((b*r+s)*(b*r+s)+4.0d0*(r*a+dabs(r)))
	ucrit=-s/r
        umin = 0.5d0*( b + ucrit - du/dabs(r) )
        umax = 0.5d0*( b + ucrit + du/dabs(r) )
        if((b*r+s)*(b*r+s) .le. 4.0d0*(dabs(r) -r*a)) then
           call segplt(umin,ucrit,1)
           call segplt(umin,ucrit,-1)
           call segplt(umax,ucrit,1)
           call segplt(umax,ucrit,-1)
        else
           du=dsqrt((b*r+s)*(b*r+s)-4.0d0*(dabs(r)- r*a))
           umid1 = 0.5d0*(b + ucrit-du/dabs(r))
           umid2 = 0.5d0*(b + ucrit+du/dabs(r))
           if(ucrit .lt. umid1) then
              call segplt(umin,ucrit,1)
              call segplt(umin,ucrit,-1)
              call segplt(umid1,ucrit,1)
              call segplt(umid1,ucrit,-1)
              call segplt(umid2,umax,1)
              call segplt(umid2,umax,-1)
           else
              call segplt(umax,ucrit,1)
              call segplt(umax,ucrit,-1)
              call segplt(umid2,ucrit,1)
              call segplt(umid2,ucrit,-1)
              call segplt(umin,umid1,1)
              call segplt(umin,umid1,-1)
           end if              
        end if
c       DRAW LINE AND CIRCLE
c       u=iline*delta
c	t=dsqrt(4.0d0-u*u)
c	call smove(-a*t+b*u, -b*t-a*u)
c	call sline(a*t+b*u, b*t-a*u)
c	call scirc(1.0d0,-dsqrt(3.0d0)/(2*icirc-1),
c	1    dsqrt(3.0d0)/dabs(2*icirc-1))
 	write(*,'(A$)') 'Another curve? (y or N) '
	read(*,'(A)') reply
	if(reply.eq.'y') go to 10
	call offgrf
	call gfxdoc('End curve')
	end
c
c     ACTUAL FUNCTION PLOT
        subroutine segplt(u1,u2,brnch)
        real*8 u1,u2,a,b,r,s,inc,du,u,v,f,t,x,y
        integer brnch,nstep,j
        common /funprm/ a,b,r,s,inc,nstep
c       FIRST POINT
	u=u1
	v=(r*u+s)*(u-b)
	f=dabs(-v*v +2.0d0*a*v+b*b)
	t=(a*(r*u+s)-b+brnch*dsqrt(f))/(r*u+s)
	x=a*t+b*u
	y=b*t-a*u
	call smove(x,y)
        du=(u2-u1)/nstep
        do 10 j=1,nstep
           u=u+du
	   v=(r*u+s)*(u-b)
	   f=dabs(-v*v +2.0d0*a*v+b*b)
	   if(dabs(r*u+s) .gt. 0.0001d0 ) then
	      t=(a*(r*u+s)-b+brnch*dsqrt(f))/(r*u+s)
	      x=a*t+b*u
	      y=b*t-a*u
	      if(x*x+y*y .gt. 10.0d0) go to 999
              call sline(x,y)
	   else
	      call sline(-s/(r*b),0.0d0)
	      go to 999
           end if
 10     continue
 999	continue
        end
