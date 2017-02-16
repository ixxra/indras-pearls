#define GRIDSIZE    1024
#define MAXPOINCARE 1.0d8
#define MINPOINCARE 1.0d-8
c
c       MENU OF POINCARE SERIES COMPUTATIONS
	subroutine pncmenu
	character choice*15
	integer outsty,lnblnk,pstype,iused,jused,lused,addtype
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	common /outfmt/ outsty
	common /pnctype/ pstype,addtype,iused,jused,lused
 10	write(*,'(/,5X,A,/)') ' POINCARE SERIES MENU: '
	write(*,'(8X,A15,3X,A)') 
	1    'group:', 'Choose new group structure',
	1    'change:', 'Change group structure',
	4    'calc:','Calculations on individual points',
	2    'plot:','Choose new plotting thresholds',
	5    'file:','Output files of poincare series',
	5    'image:','Plots color image of poincare series',
	1    'up:','Just Press Return To Go To MAIN MENU'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'group') then
c       CHOOSE NEW GROUP STRUCTURE
	   grpon=.false.
	   call setgrp
	else if(choice.eq.'change') then
c       CHANGE GROUP STRUCTURE
	   if(.not.grpon) call setgrp
	   call chgrp
c       OUTPUT FILES OF POINCARE SERIES AND AUTOMORPHIC FUNCTION
	else if(choice.eq.'calc'.or.choice.eq.'file'
	1	.or.choice.eq.'image') then
	   if(choice.eq.'calc') then
	      addtype = 1
	   else
	      addtype = 2
	      write(*,*) 'Enter horizontal, vertical numbers of pixels: '
	      read(*,*) iused,jused
	      lused = 3*iused
	   end if
 21	   write(*,'(/,5X,A,/)') ' TYPE OF POINCARE SERIES: '
	   write(*,'(/,5X,A,/)') ' F(k,l) = sum (az+b)^k (cz+d)^l '
	   write(*,'(8X,A15,3X,A)') 
	1	'1: ', 'F(0,-4), F(2,-6)/F(0,-4)',
	1	'2: ', 'F(0,-4), F(0,-6)^2/F(0,-4)^3',
	1	'3: ', 'F(0,-4), F(-1,-3)/F(0,-4)'
	   write(*,'(/,5X,A$)') 'SELECT: (integer) '
	   read(*,*) pstype
	   if(pstype.le.0.or.pstype.gt.3) go to 21
c       OUTSTY = 0 MEANS OUTPUT POINCARE SERIES
	   outsty = 0
c          OUTPUT FILES OF POINTS
	   if(choice.eq.'calc') then
	      call pscalc
c          OUTPUT FILES OF POINTS
	   else if(choice.eq.'file') then
	      call enumer(1)
	      call outsum
c          PLOT IMAGE
	   else
	      call enumer(1)
	      call gfxdoc('Begin Color Image')
	      call mkcbit
	      call gfxdoc('End Color Image')
	   end if
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c       
c       CALCULATIONS AT INDIVIDUAL POINTS
	subroutine pscalc
	character ans
	complex*16 z0,psum1,psum2,u
	integer pstype,iused,jused,lused,addtype
	common /pnctype/ pstype,addtype,iused,jused,lused
	common /oneterm/ z0,psum1,psum2
 10	write(*,*) 'Enter complex input value: '
	read(*,*) z0
	call enumer(1)
	write(*,*) 'The first poincare sum is: ',psum1
	write(*,*) 'The second poincare sum is: ',psum2
	if(pstype.eq.1.or.pstype.eq.3) then
	   u = psum2/psum1
	else if(pstype.eq.2)then
	   u = psum2/psum1/psum1
	   u = u*psum2/psum1
	end if
	write(*,*) 'The automorphic function is: ',u
	write(*,*) 'Another point? (y or n)'
	read(*,'(A)') ans
	if(ans.eq.'n') return
	go to 10
	end
c
c       ADD THE NEXT TERM OF THE POINCARE SERIES
	subroutine addterm
	real*8 xmin,ymin,xmax,ymax,s,t,delx,dely
	real*8 x0,y0,x1,y1
	complex*16 x(4,MAXLEV), psum(2,GRIDSIZE,GRIDSIZE)
	complex*16 u,w,w2,z,z0,psum1,psum2
	integer tag(MAXLEV),lev,i,j,numpts,maxlev,pstype,
	1    iused,jused,lused,addtype
	common /tree/ x
	common /tags/ tag,lev
	common /picwin/ xmin,ymin,xmax,ymax
	common /gfinfo/ x0,y0,x1,y1,numpts,maxlev
	common /pnctype/ pstype,addtype,iused,jused,lused
	common /poincare/ psum
	common /oneterm/ z0,psum1,psum2
c
	numpts=numpts+1
	if(addtype.eq.2) then
	   delx = (xmax-xmin)/(iused - 1)
	   dely = (ymax-ymin)/(jused - 1)
	   s = xmin
	   do 5 i=1,GRIDSIZE
	      t = ymin
	      do 6 j=1,GRIDSIZE
		 z  = dcmplx(s,t)
c       COMPUTE 1/(c*z+d)
		 w  = 1.0d0/(x(3,lev)*z+x(4,lev))
		 if(pstype.eq.1) then
c       COMPUTE (a*z+b)/(c*z+d)
		    u  = (x(1,lev)*z + x(2,lev))*w
c       SQUARE u 
		    u = u*u
c       TAKE FOURTH POWER OF w
		    w = w*w
		    w = w*w
		    if(numpts.eq.1) then
		       psum(1,i,j) = w
		       psum(2,i,j) = u*w
		    else
		       psum(1,i,j) = psum(1,i,j) + w
		       psum(2,i,j) = psum(2,i,j) + u*w
		    end if
		 else if(pstype.eq.2)then
c       TAKE FOURTH POWER AND SIXTH POWER OF w
		    w = w*w
		    w2 = w*w
c$$$c       FOR DEBUGGING
c$$$    write(*,*) (tag(i),i=1,lev)
c$$$    write(*,*) 'Point is ', z
c$$$    write(*,*) 'psum1 term is ',w2
c$$$    write(*,*) 'psum2 term is ',w2*w
c$$$    call pause
		    if(numpts.eq.1) then
		       psum(1,i,j) = w2
		       psum(2,i,j) = w2*w
		    else
		       psum(1,i,j) = psum(1,i,j) + w2
		       psum(2,i,j) = psum(2,i,j) + w2*w
		    end if
		 else if(pstype.eq.3)then
c       TAKE (c*z+d)^-2
		    w2 = w*w
c       TAKE 1/[(a*z+b)*(c*z+d)^3]
		    w = w2*w/(x(1,lev)*z+x(2,lev))
		    if(numpts.eq.1) then
		       psum(1,i,j) = w2*w2
		       psum(2,i,j) = w
		    else
		       psum(1,i,j) = psum(1,i,j) + w2*w2
		       psum(2,i,j) = psum(2,i,j) + w
		    end if
		 end if
		 t=t+dely
 6	      continue
	      s=s+delx
 5	   continue
c       JUST CALCULATE AT ONE POINT
	else
c       COMPUTE 1/(c*z+d)
	   w  = 1.0d0/(x(3,lev)*z0+x(4,lev))
	   if(pstype.eq.1) then
c       COMPUTE (a*z+b)/(c*z+d)
	      u  = (x(1,lev)*z0 + x(2,lev))*w
c       SQUARE u 
	      u = u*u
c       TAKE FOURTH POWER OF w
	      w = w*w
	      w = w*w
	      if(numpts.eq.1) then
		 psum1 = w
		 psum2 = u*w
	      else
		 psum1 = psum1 + w
		 psum2 = psum2 + u*w
	      end if
	   else if(pstype.eq.2)then
c       TAKE FOURTH POWER AND SIXTH POWER OF w
	      w = w*w
	      w2 = w*w
	      if(numpts.eq.1) then
		 psum1 = w2
		 psum2 = w2*w
	      else
		 psum1 = psum1 + w2
		 psum2 = psum2 + w2*w
	      end if
	   else if(pstype.eq.3)then
c       TAKE (c*z+d)^-2
	      w2 = w*w
c       TAKE 1/[(a*z+b)*(c*z+d)^3]
	      w = w2*w/(x(1,lev)*z0+x(2,lev))
	      if(numpts.eq.1) then
		 psum1 = w2*w2
		 psum2 = w
	      else
		 psum1 = psum1 + w2*w2
		 psum2 = psum2 + w
	      end if
	   end if
	end if
c
	return
	end
c
c
c	OUTPUT THE POINCARE SERIES
	subroutine outsum
	real*8 xmin,ymin,xmax,ymax,r,s
	complex*16 psum(2,GRIDSIZE,GRIDSIZE),u
	integer i,j,numpts,maxlev,pstype,iused,jused,lused,
	1    addtype
	character outfil*100
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	common /pnctype/ pstype,addtype,iused,jused,lused
        common /poincare/ psum
c
	write(*,'(A,I10)') 'The number of nodes reached is ',numpts
	write(*,*) 'Enter name of poincare series file: '
	read(*,'(A)') outfil
	open(3,file = outfil)
	do 10 i=1,iused
	   do 10 j=1,jused
	      r= dmin1( dreal(psum(1,i,j)), MAXPOINCARE)
	      r= dmax1( r , -MAXPOINCARE)
	      s= dmin1( dimag(psum(1,i,j)), MAXPOINCARE)
	      s= dmax1( s , -MAXPOINCARE)
	      write(3,200)  r,s
 200	      format(1x,f21.10, 1x, f21.10)
 10	continue
	close(3)
c
	write(*,*) 'Enter name of automorphic function file: '
	read(*,'(A)') outfil
	open(3,file=outfil)
	do 20 i=1,iused
	   do 20 j=1,jused
c$$$	      write(*,*) 'psum1 term is ',psum(1,i,j)
c$$$	      write(*,*) 'psum2 term is ',psum(2,i,j)
c$$$	      call pause
	      if (cdabs(psum(1,i,j)).GE.MINPOINCARE) then
		 if(pstype.eq.1.or.pstype.eq.3) then
		    u = psum(2,i,j)/psum(1,i,j)
		 else if(pstype.eq.2)then
		    u = psum(2,i,j)/psum(1,i,j)/psum(1,i,j)
		    u = u*psum(2,i,j)/psum(1,i,j)
		 end if
	      else
		 u = MAXPOINCARE
	      endif
	      r= dmin1( dreal(u), MAXPOINCARE)
	      r= dmax1( r , -MAXPOINCARE)
	      s= dmin1( dimag(u), MAXPOINCARE)
	      s= dmax1( s , -MAXPOINCARE)
	      write(3,200)  r,s
20	continue
	close(3)
c
	return
	end
c
c	OUTPUT COLOR IMAGES OF POINCARE SERIES
	subroutine mkcbit
	real*8 u,v,xmin,ymin,xmax,ymax
	integer iused,jused,lused,i,j,ibit,jbit,lbit,addtype
	character cbit(ISIZE,JSIZE),cred,cgreen,cblue,choice*20
	complex*16 psum(2,GRIDSIZE,GRIDSIZE),z
	integer numpts,maxlev,pstype
	character outfil*100
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	common /pnctype/ pstype,addtype,iused,jused,lused
        common /poincare/ psum
	common /clrbit/ cbit,ibit,jbit,lbit
c
	write(*,'(A,I10)') 'The number of nodes reached is ',numpts
c
	write(*,*) 'Enter pnc for poincare series;',
	1    ' aut for automorphic function: '
	read(*,*) choice
c
	ibit = iused
	jbit = jused
	lbit = lused
	do 5 i =1, iused
	   do 6 j=1, jused
	      if(choice.eq.'pnc') then
		 u= dreal(psum(1,i,j))
		 v= dimag(psum(1,i,j))
	      else
		 if (cdabs(psum(1,i,j)).GE.MINPOINCARE) then
		    if(pstype.eq.1.or.pstype.eq.3) then
		       z = psum(2,i,j)/psum(1,i,j)
		    else if(pstype.eq.2)then
		       z = psum(2,i,j)/psum(1,i,j)/psum(1,i,j)
		       z = z*psum(2,i,j)/psum(1,i,j)
		    end if
		 else
		    z = MAXPOINCARE
		 endif
		 u= dreal(z)
		 v= dimag(z)
	      end if
	      call cmap(u,v,cred,cgreen,cblue)
	      cbit(3*i-2,j) = cred
	      cbit(3*i-1,j) = cgreen
	      cbit(3*i,j) = cblue
 6	   continue
 5	continue
c
	call outcbit
	end
c
	subroutine cmap(u,v,cr,cg,cb)
	real*8 u,v,r,twopi
	character cr,cg,cb
	data twopi/6.28318530717958d0/
	r = datan2(v,u)
	if(r.lt.0.0d0) r=r+twopi
	r=6*r/twopi
	if(r.lt.1) then
	   cr = char(255)
	   cg = char(int(255*r))
	   cb = char(0)
	else if (r.lt.2) then
	   cr = char(int(255*(2-r)))
	   cg = char(255)
	   cb = char(0)
	else if (r.lt.3) then
	   cr = char(0)
	   cg = char(255)
	   cb = char(int(255*(r-2)))
	else if (r.lt.4) then
	   cr = char(0)
	   cg = char(int(255*(4-r)))
	   cb = char(255)
	else if (r.lt.5) then
	   cr = char(int(255*(r-4)))
	   cg = char(0)
	   cb = char(255)
	else 
	   cr = char(255)
	   cg = char(0)
	   cb = char(int(255*(6-r)))
	end if	   
	end
