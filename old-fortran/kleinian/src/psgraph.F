C 	PSGRAPH.F: A GRAPHICS PACKAGE FOR POSTSCRIPT
C 		(VERSION INDRA: 7/16/01)
C
C 	This package of FORTRAN subroutines allows one to write FORTRAN
C 	programs to draw any combination of lines, arcs, and circles on
C 	any PostScript printer.
c
c       It uses PostScript macros defined in PSGRAPH.PRO
c       Any output file includes PSGRAPH.PRO in its prolog.
C
c       UNIT 1 is reserved for output to the PostScript File
c       UNIT 11 is used momentarily in on() to load the PostScript
c          Prolog psgraph.pro
c       UNIT 2 is reserved for the labels file.
C
C	SCREEN PARAMETERS FOR A STANDARD 8 1/2 x 11 inch printer
C
	block data scrprm
	real*8 screen,width,height,uninch,unitpx
	real*8 lhmin,lhmax,lvmin,lvmax,phmin,phmax,pvmin,pvmax
	integer penwid

C	SCREEN = HEIGHT / WIDTH  (BOTH MEASURED IN INCHES)
C	UNINCH = RATIO OF DEVICE UNITS PER INCH
	common /scrdat/ screen,width,height,uninch
C	PORTRAIT AND LANDSCAPE SCREEN LIMITS (IN INCHES)
	common /scrdim/ phmin,phmax,pvmin,pvmax,
	1    lhmin,lhmax,lvmin,lvmax
C	PEN WIDTH (UNITPX = DEVICE UNITS PER PIXEL)
	common /penwbk/ unitpx,penwid
	data screen/1.36/, width/8.0/, height/10.92/, uninch/1200.0/
	data phmin/0.25/,phmax/8.25/,pvmin/0.04/,pvmax/10.96/
	data lhmin/0.04/,lhmax/10.960/,lvmin/0.25/,lvmax/8.25/
C       UNITS PER PIXEL
c       (300 DPI printer)
c	data unitpx/4.0/
c       (400 DPI printer)
c	data unitpx/3.0/
c	(600 DPI printer)
	data unitpx/2.0/
	end
c
c       OPEN A POSTSCRIPT FILE AND ENTER IN THE HEADER FILE
	subroutine on(name)
	character name*(*), lname*100,line*80
	integer iarray(3),l,index,lnblnk
	open(1,file = name)
c       OPEN LABELS FILE
	l =index(name,'.')
	lname=name(1:l)//'lbl'
	open(2,file = lname)
c       IDATE IS THE INTRINSIC DATE FUNCTION
	call idate(iarray)
   	write(1,100) iarray(2),iarray(1),iarray(3)
100	format('%!PS-Adobe-2.0 EPSF-2.0',/,
	1    '%%BoundingBox: ',/,
	2    '%%Title: PS Graphics file created by PSGraph 4',/,
	3    '%%CreationDate: ',I2,'/',I2,'/',I4,/,
	4    '%%EndComments')
c       HEADER IS A MACRO DEFINED IN MAKEFILE TO BE THE
C       LOCATION OF PSGRAPH.PRO
	open(11,file=HEADER,status='old')
10	read(11,'(A)',end=999) line
c       LNBLNK IS AN INTRINSIC RETURNING INDEX OF LAST NONBLANK
	write(1,'(A)') line(1:lnblnk(line))
	go to 10
999	close(11)
	end
C
c       CLOSE THE POSTSCRIPT FILE AND APPEND TRAILER
	subroutine off
	write(1,'(A,/,A,/,A,/,A)') '%%Pages: 0 1','%%Trailer',
	1    'showpage','%end(plot)'
	close(1)
c       CLOSE LABELS FILE
	close(2)
	end
C
C	SET SCALE PARAMETERS
C	THIS ROUTINE FITS THE RECTANGLE (X0,X1)x(Y0,Y1) ONTO THE
C	PAGE WITH MAXIMUM AREA.
	subroutine sscale(x0,y0,x1,y1)
	real*8 x0,y0,x1,y1,yxratio
	real*8 factor,xmin,ymin,xmax,ymax,hoff,voff
	real*8 screen, width, height, uninch
	real*8 phmin,phmax,pvmin,pvmax,lhmin,lhmax,lvmin,lvmax
c	ONE INCH IN USER COORDINATES
	real*8 sinch
	integer orient,skip,choice
C	FACTOR = RATIO OF PIXELS TO PROGRAM UNITS OF DISTANCE
C	XMIN (XMAX) = MINIMUM (MAXIMUM) X VALUE ON PAGE
C	YMIN (YMAX) = MINIMUM (MAXIMUM) Y VALUE ON PAGE
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	common /scrdat/ screen,width,height,uninch
	common /scrdim/ phmin,phmax,pvmin,pvmax,
	1    lhmin,lhmax,lvmin,lvmax
	common /userin/ sinch
	save orient
	assign 10 to skip
	go to 5
C
C	SET SCALE PARAMETERS WITH A CHOSEN ORIENTATION
C           orient = 1 -> Portrait ; = 2 -> Landscape
	entry spage(x0,y0,x1,y1,choice)
	orient = choice
	assign 20 to skip
C
C	RESET SCALE PARAMETERS WITHOUT CHANGING ORIENTATION
	entry rescal(x0,y0,x1,y1)
	assign 20 to skip
c       COMPUTE ASPECT OF PICTURE
 5	yxratio=(y1-y0)/(x1-x0)
	go to skip
 10	IF(yxratio .ge. 1.0) then
C	PORTRAIT MODE
		orient =1
C	LANDSCAPE MODE
	else
		orient =2
		write(1,'(A)') '90 rotate 0 -10200 translate'
	end if
 20	continue

	if(orient .le. 1) then
C	PORTRAIT MODE
	   if(yxratio .le. screen) then
	      factor=uninch*width/(x1-x0)
	      xmin=x0
	      xmax=x1
	      ymin=(y1+y0 -screen*(x1-x0))/2.0
	      ymax=ymin +screen*(x1-x0)
	   else
	      factor=uninch*height/(y1-y0)
	      ymin=y0
	      ymax=y1
	      xmin=(x1+x0-(y1-y0)/screen)/2.0
	      xmax=xmin +(y1-y0)/screen
	   end if
	   hoff=phmin*uninch-factor*xmin
	   voff=pvmin*uninch-factor*ymin
C	LANDSCAPE MODE
	else
	   IF(yxratio .le. 1.0/screen) then
	      factor=uninch*height/(x1-x0)
	      xmin=x0
	      xmax=x1
	      ymin=(y1+y0-(x1-x0)/screen)/2.0
	      ymax=ymin +(x1-x0)/screen
	   else
	      factor=uninch*width/(y1-y0)
	      ymin=y0
	      ymax=y1
	      xmin=(x1+x0 -screen*(y1-y0))/2.0
	      xmax=xmin +screen*(y1-y0)
	   end if
	   hoff=lhmin*uninch-factor*xmin
	   voff=lvmin*uninch-factor*ymin
	end if
	sinch = uninch/factor
	end
C
C	PRINT N COPIES
	subroutine copy(n)
	integer n
   	write(1,'(A,I4,A)') '/#copies ',n,' def'
	end
C
C	TURN ON VECTOR GRAPHICS WITH PEN WIDTH = PEN
	subroutine ongrf(pen)
	integer pen, penwid
	real*8 unitpx
	common /penwbk/ unitpx,penwid
	penwid=pen
	if(pen.ge.0) then
	   write(1,'(A,/,f8.4,A)')
	1	'gsave',pen*unitpx,' setlinewidth NP'
	else
	   write(1,'(A)')
	1	'gsave NP'
	end if
	end
c
c       CHANGE COLOR: Assumes the color is known to psgraph.pro
	subroutine ccolor(col)
	character*(*) col
        integer lnblnk
	write(1,'(A,A)') 'ST ',col(1:lnblnk(col))
	end
c
c       CHANGE COLOR: Assumes the color is known to psgraph.pro
	subroutine setcmap(col)
	character*(*) col
        integer lnblnk
	write(1,'(A,A,A)') 'ST /cmap ',col(1:lnblnk(col)),' def'
	end
c
c       SET COLOR CYCLING
	subroutine ccycle(cspread)
	real*8 cspread
	integer k
	k = cspread
	write(1,'(A,I10,A)') '/cfac {',k, ' lcmap mul} def'
	end
c
c       CHOOSE COLOR: according to default cmap in psgraph.pro
	subroutine nxtcol(i)
	integer i
	write(1,'(I6,A)') i,' CC'
	end
c
c       CHOOSE COLOR: according to default cmap in psgraph.pro
	subroutine nxtcol2(i,j)
	integer i,j
	write(1,'(I6,1x,I6,A)') i,j,' CCB'
	end
c
c       CHANGE GRAY (Shade from 0 to 1; 0 = black; 1 = white)
	subroutine cgray(gray)
	real*8 gray
	write(1,'(f8.4,A)') gray, ' setgray'
	end
C
C	TURN OFF VECTOR GRAPHICS
	subroutine offgrf
	write(1,'(A,/,A)') 'ST','grestore'
	end
C
C	SET PEN WIDTH = PEN (NUMBER OF UNITS)
	subroutine penwid(pen)
	integer pen, penw
	real*8 unitpx
	common /penwbk/ unitpx,penw
	penw=pen
	write(1,100) pen*unitpx
100	format(f8.4, ' setlinewidth')
	end
C
C	SET VECTOR PATTERN
	subroutine vecpat(pat)
	integer pat
	character*20 pattrn(5)
	DATA pattrn/'[] 0 ', '[1] 0', '[2] 0','[3] 0','[2] 1'/
	write(1,100) pattrn(pat)
100	format(A,' setdash')
	end
C
C	MOVE WITH PEN UP IN VECTOR GRAPHICS
	subroutine smove(x,y)
	real*8 x,y,factor,xmin,ymin,xmax,ymax,hoff,voff,
	1    x1,y1,x2,y2
c       LAST TWO POINTS
	real*8 xold,yold,ang,degrad,unitpx,den,r,zero,
	1    bang,mang,eang,ang1,ang2,delang
	complex*16 z0,z1,z2,c,u,w,p1,p2,zmid
	integer ix,iy,ix1,iy1,ix2,iy2,ixold,iyold,numang,ir,i,
	1    ixp1,iyp1,ixp2,iyp2,
	1    penwid,lnblnk,count/0/
	character cmd*200, wlabel*(*), newlbl*200
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	common /penwbk/ unitpx,penwid
	parameter (degrad=57.29577951308232087679815481d0,
	1    zero=1.0d-10)
	save ixold,iyold,xold,yold,count
	cmd = 'M'
	go to 10
C
C	MOVE WITH PEN DOWN IN VECTOR GRAPHICS
	entry sline(x,y)
	cmd = 'L'
	go to 10
C
C	PLOT LABELLED POINTS
	entry vpoint(x,y,wlabel)
	cmd = 'P'
	count=count+1
	write(newlbl,'(I3,A,A)') count,': ',wlabel(1:lnblnk(wlabel))
	call slabel(x,y,0.3d0,0.0d0,newlbl)
	go to 10
C
C	DRAW LABELLED LINES (AT LOW RESOLUTION)
	entry vmove(x,y,wlabel)
	cmd = 'M'
	count=count+1
	write(newlbl,'(I3,A,A)') count,': ',wlabel(1:lnblnk(wlabel))
	call slabel(x,y,0.3d0,0.0d0,newlbl)
	go to 10
C
C	DRAW LABELLED LINES (AT LOW RESOLUTION)
	entry vline(x,y,wlabel)
        count=count+1
	ang=datan2(y-yold,x-xold)*degrad
	ang=ang-135.0d0
	if(ang .lt. 0.0) ang=ang+360.0d0
	write(cmd,'(F6.3,1x,F7.2,1x,A,I3,A,A,A)') 0.3d0,ang,
	1    '(',count,': ',wlabel(1:lnblnk(wlabel)),') V'
	go to 10
C
C	PLOT ONE POINT IN VECTOR GRAPHICS
	entry spoint(x,y)
	if( x.gt.xmax.or.x.lt.xmin.or.y.gt.ymax.or.y.lt.ymin )
	1    return
	cmd = 'P'
 10	ix=factor*x+hoff
	iy=factor*y+voff
	go to 20
C
C	POSITION THE CURSOR IN ABSOLUTE COORDINATES (IX,IY)
	entry iposit(ix1,iy1)
	cmd = 'M'
	ix=ix1
	iy=iy1
c       RETURN IF WE'RE NOT MOVING ANYWHERE
 20	if(ixold.eq.ix.and.iyold.eq.iy.and.cmd.ne.'M') return
	ixold=ix
	iyold=iy
	xold=x
	yold=y
	write(1,*) ixold,' ',iyold,' ',cmd(1:lnblnk(cmd))
	return
c
c       DRAW ARCS FROM CURRENT POINT THROUGH (x1,y1) and (x2,y2)
c        ASSUMES (xold,yold) HAS BEEN SET
	entry sarcto(x1,y1,x2,y2)
	z0=dcmplx(xold,yold)
	z1=dcmplx(x1,y1)
	z2=dcmplx(x2,y2)
	den=dimag((z2-z0)*dconjg(z1-z0))
	if(dabs(den).le.zero) then
	   ix2=factor*x2+hoff
	   iy2=factor*y2+voff
	   write(1,*) ix2,' ',iy2,' L'
	   xold=x2
	   yold=y2
	   ixold=ix2
	   iyold=iy2
	   return
	end if
	c=z0+ (0.0d0,0.5d0)*(z2-z0)*(z0-z1)*dconjg(z1-z2)/den
	r= cdabs(z0-c)
	ir=factor*r
c       IF RADIUS IS < HALF A PIXEL, JUST PLOT CENTER
	ir=max(ir,0.5d0*unitpx)
c	TRUE CIRCLE
	bang= degrad*datan2(dimag(z0-c),dreal(z0-c))
	if(bang .lt. 0.0) bang=bang+360.0d0
	eang= degrad*datan2(dimag(z2-c),dreal(z2-c))
	if(eang .lt. 0.0) eang=eang+360.0d0
c       DETERMINE ORIENTATION
	mang= degrad*datan2(dimag(z1-c),dreal(z1-c))
	if(mang .lt. 0.0) mang=mang+360.0d0
	ang1=mang-bang
	if(ang1 .lt. 0.0) ang1=ang1+360.0d0
	ang2=eang-bang
	if(ang2 .lt. 0.0) ang2=ang2+360.0d0
	if(ang1 .lt. ang2) then
c$$$	   numang = dabs(ang2)/45.0d0+1.0d0
c$$$	   delang = ang2/numang
	   call sarc(dreal(c),dimag(c),r,bang,eang)
	else
	   ang2=ang2 - 360.0d0
c$$$	   numang = dabs(ang2)/45.0d0+1.0d0
c$$$	   delang = ang2/numang
	   call sarcn(dreal(c),dimag(c),r,bang,eang)
	end if
c$$$	do 25 i=1,numang
c$$$	   z1=c+
c$$$	1	r*cdexp(dcmplx(0.0d0,(bang+(i-0.5d0)*delang)/degrad))
c$$$	2	/dcos(0.5d0*delang/degrad)
c$$$	   z2=c+r*cdexp(dcmplx(0.0d0,(bang+i*delang)/degrad))
c$$$	   ixp1=factor*dreal(z1)+hoff
c$$$	   iyp1=factor*dimag(z1)+voff
c$$$	   ixp2=factor*dreal(z2)+hoff
c$$$	   iyp2=factor*dimag(z2)+voff
c$$$	   if(ixp1.eq.ixp2.and.iyp1.eq.iyp2) go to 25
c$$$	   write(1,190) ixp1, iyp1, ixp2, iyp2, ir
c$$$ 190	   format(i8,1x,i8,1x,i8,1x,i8,1x,i8,1x,'AT')
c$$$ 25	continue
	xold=x2
	yold=y2
	ixold=ix2
	iyold=iy2
	return
c
c       DRAW ARCS FROM CURRENT POINT THROUGH (x1,y1) and (x2,y2)
c        ASSUMES (xold,yold) HAS BEEN SET
c       USES BEZIER CUBIC APPROXIMATION
	entry scurve(x1,y1,x2,y2)
	z0=dcmplx(xold,yold)
	z1=dcmplx(x1,y1)
	z2=dcmplx(x2,y2)
	ixold=factor*x2+hoff
	iyold=factor*y2+voff
	xold=x2
	yold=y2
	den=dimag((z2-z0)*dconjg(z1-z0))
	if(dabs(den).le.zero) then
	   write(1,*) ixold,' ',iyold,' L'
	   return
	end if
c       CENTER AND RADIUS OF CIRCLE THROUGH Z0, Z1, Z2
	c=z0+ (0.0d0,0.5d0)*dconjg(z1-z2)*( (z2-z0)*(z0-z1)/den )
	r= cdabs(z0-c)
c	COMPUTE ANGLES OF Z0, Z1, Z2
	bang= degrad*datan2(dimag(z0-c),dreal(z0-c))
	if(bang .lt. 0.0) bang=bang+360.0d0
	mang= degrad*datan2(dimag(z1-c),dreal(z1-c))
	if(mang .lt. 0.0) mang=mang+360.0d0
	eang= degrad*datan2(dimag(z2-c),dreal(z2-c))
	if(eang .lt. 0.0) eang=eang+360.0d0
	ang1=mang-bang
	if(ang1 .lt. 0.0) ang1=ang1+360.0d0
	ang2=eang-bang
	if(ang2 .lt. 0.0) ang2=ang2+360.0d0
c       COMPUTE INCREMENTS OF AT MOST 90 DEGREES
	if(ang1 .lt. ang2) then
c       COUNTERCLOCKWISE
	   numang = dabs(ang2)/90.0d0+1.0d0
	   delang = ang2/numang
	else
c       CLOCKWISE: INCREMENT IS NEGATIVE
	   ang2=ang2 - 360.0d0
	   numang = dabs(ang2)/90.0d0+1.0d0
	   delang = ang2/numang
	end if
	w=(0.0d0,-1.3333333333333333d0)*dtan(0.25d0*delang/degrad)
	do 30 i=1,numang
	   z1=c+r*cdexp(dcmplx(0.0d0,(bang+(i-0.5d0)*delang)/degrad))
	   z2=c+r*cdexp(dcmplx(0.0d0,(bang+i*delang)/degrad))
	   p1=z0+w*(c-z0)
	   p2=z2-w*(c-z2)
	   ixp1 = factor*dreal(p1)+hoff
	   iyp1 = factor*dimag(p1)+voff
	   ixp2 = factor*dreal(p2)+hoff
	   iyp2 = factor*dimag(p2)+voff
	   ix2  = factor*dreal(z2)+hoff
	   iy2  = factor*dimag(z2)+voff
	   write(1,200) ixp1, iyp1, ixp2, iyp2, ix2, iy2
 200	   format(i8,1x,i8,1x,i8,1x,i8,1x,i8,1x,i8,1x,'CT')
	   z0 = z2
 30	continue
	end
c
c       A LINE SEGMENT
	subroutine ssegmt(x0,y0,x1,y1)
	real*8 x0,y0,x1,y1
	call smove(x0,y0)
	call sline(x1,y1)
	end
c
c       PLACE AN ARROWHEAD POINTING IN A GIVEN DIRECTION
	subroutine sarrow(x,y,theta)
	real*8 x,y,theta
	call smove(x,y)
	write(1,100) theta
100	format(f8.3,' arrowhead')
	end
C
C	DRAW A CIRCLE OF RADIUS R WITH CENTER (X,Y)
c		FOR HYPERBOLIC GEOMETRY, IF R < 0, THEN
c		THE STRAIGHT LINE THRU (X,Y) WITH DIRECTION ANGLE -R
C		IS PLOTTED.
c
	subroutine scirc(x,y,r)
	real*8 x,y,r,factor,xmin,ymin,xmax,ymax,hoff,voff,
	1   xwin0,ywin0,xwin1,ywin1,x1,y1
	real*8 unitpx,l(4),u,v,temp
	integer ix,iy,ir,penwid,i,j,lev,ltag,lnblnk
	character cmd*20
	logical dofill
	common /penwbk/ unitpx,penwid
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	common /windw/ xwin0,ywin0,xwin1,ywin1
	dofill=.false.
	cmd = ' C'
	go to 10
C
C	DRAW A CIRCLE OF RADIUS R WITH CENTER (X,Y) FILLED
c
c       SET THE FILL COLOR ACCORDING TO LEVEL lev
	entry cdisk(x,y,r,lev,ltag)
	dofill=.true.
	write(cmd,'(1X,I4,1x,I4,A)') lev,ltag,' Dc'
	go to 10
c       ASSUME THE FILL COLOR OR PATTERN HAS BEEN SET
c		FOR HYPERBOLIC GEOMETRY, IF R < 0, THEN
c		THE STRAIGHT LINE THRU (X,Y) WITH DIRECTION -R
C		IS PLOTTED. THE HALF-PLANE ON THE LEFT IS FILLED.
c
	entry sdisk(x,y,r)
	dofill=.true.
	cmd = ' D'
 10	if(r .lt. 0.0) then
	   u=dcos(r)
	   v=-dsin(r)
	   if(u .eq. 0.0) then
	      call ssegmt(x,ywin0,x,ywin1)
	      if(dofill) then
		 if(v.gt.0) then
		    call sline(xwin0,ywin1)
		    call sline(xwin0,ywin0)
		    call fill(0.5*(xwin0+x),0.5*(ywin0+ywin1))
		 else
		    call sline(xwin1,ywin1)
		    call sline(xwin1,ywin0)
		    call fill(0.5*(xwin1+x),0.5*(ywin0+ywin1))
		 end if
	      end if
	      return
	   else if(v .eq. 0.0) then
	      call ssegmt(xwin0,y,xwin1,y)
	      if(dofill) then
		 if(u.gt.0) then
		    call sline(xwin1,ywin1)
		    call sline(xwin0,ywin1)
		    call fill(0.5*(xwin0+xwin1),0.5*(y+ywin1))
		 else
		    call sline(xwin1,ywin0)
		    call sline(xwin0,ywin0)
		    call fill(0.5*(xwin1+xwin0),0.5*(ywin0+y))
		 end if
	      end if
	      return
	   end if
c       FOR SLANTED LINES FIND INTERSECTION WITH FRAME
c       PARAMETRIZE LINE BY (x,y) + l*(u,v)
c          FIND INTERSECTION WITH x = xwin0
	   l(1) = (xwin0-x)/u
c          FIND INTERSECTION WITH x = xwin1
	   l(2) = (xwin1-x)/u
c          FIND INTERSECTION WITH y = ywin0
	   l(3) = (ywin0-y)/v
c          FIND INTERSECTION WITH y = ywin1
	   l(4) = (ywin1-y)/v
c       SORT INTO l(1) < l(2) < l(3) < l(4)
	   do 3 i=1,3
	      do 3 j=i+1,4
		 if( l(i) .gt. l(j)) then
		    temp=l(i)
		    l(i)=l(j)
		    l(j)=temp
		 end if
 3	   continue
	   call ssegmt(x+l(2)*u,y+l(2)*v,x+l(3)*u,y+l(3)*v)
c       NEEDS FIXING
	   if(dofill) then
	      x1=x+l(3)*u
	      y1=y+l(3)*v
	      if(x1.eq.xwin0) then
		 call sline(xwin0,ywin0)
		 if(-(xwin1-x)*v+(ywin0-y)*u.gt.0.0) call sline(xwin1,ywin0)
		 if(-(xwin1-x)*v+(ywin1-y)*u.gt.0.0) call sline(xwin1,ywin1)
	      else if(x1.eq.xwin1) then
		 call sline(xwin1,ywin1)
		 if(-(xwin0-x)*v+(ywin1-y)*u.gt.0.0) call sline(xwin0,ywin1)
		 if(-(xwin0-x)*v+(ywin0-y)*u.gt.0.0) call sline(xwin0,ywin0)
	      else if(y1.eq.ywin0) then
		 call sline(xwin1,ywin0)
		 if(-(xwin1-x)*v+(ywin1-y)*u.gt.0.0) call sline(xwin1,ywin1)
		 if(-(xwin0-x)*v+(ywin1-y)*u.gt.0.0) call sline(xwin0,ywin1)
	      else
		 call sline(xwin0,ywin1)
		 if(-(xwin0-x)*v+(ywin0-y)*u.gt.0.0) call sline(xwin0,ywin0)
		 if(-(xwin1-x)*v+(ywin0-y)*u.gt.0.0) call sline(xwin1,ywin0)
	      end if
	      call fill(x,y)
	   end if
	   return
	end if
c       IF CIRCLE DOES NOT TOUCH FRAME, THEN SKIP
	if( x.gt.xwin1+r.or.x.lt.xwin0-r
	1    .or.y.gt.ywin1+r.or.y.lt.ywin0-r) return
 	ir=factor*r
c       IF RADIUS IS < HALF A PIXEL, JUST PLOT CENTER
	ir=max(ir,0.5d0*unitpx)
	ix=factor*x+hoff
	iy=factor*y+voff
	write(1,100) ix,iy,ir,cmd(1:lnblnk(cmd))
 100	format(i8,1x,i8,1x,i8,A)
	end
c
c       SET UP COLORING OF TILES
	subroutine ctile(n)
	integer n,clrsty,numclr
	common /clrfmt/ clrsty,numclr
	if(n.eq.0) then
	   write(1,'(A)') '/T { pop} def','/ET { pop} def'
	else if(n.eq.1) then
	   write(1,'(A)') '/T { CC} def','/ET { pop} def'
	else if(n.eq.2) then
	   write(1,100) '/T { 1 sub ',numclr,' mod ',
	1	numclr,' div CCC} def'
 100	   format(A,I4,A,I4,A)
	   write(1,'(A)') '/ET { pop} def'
	end if
	end
c
c       BEGINNING AND ENDING TILES
	subroutine btile(lev)
	integer lev,lnblnk
	character cmd*20
	cmd='T'
	go to 10
c
	entry etile(lev)
	cmd='ET'
 10	write(1,100) lev,cmd(1:lnblnk(cmd))
 100	format(I4,1x,A)
	end
C
C	DRAW AN ELLIPSE CENTERED AT (X,Y) WITH MAJOR AXIS A, MINOR
C	AXIS B, AND DIRECTION THETA OF MAJOR AXIS
	subroutine sellps(x,y,a,b,theta)
	real*8 x,y,a,b,theta,factor,xmin,ymin,xmax,ymax,hoff,voff
	real*8 unitpx
	integer ix,iy,ia,ib,penwid
	common /penwbk/ unitpx,penwid
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	ia=factor*a
	ib=factor*b
	if(max(ia,ib) .le. 0.5*unitpx) then
	  call spoint(x,y)
	  return
	end if
	ix=factor*x+hoff
	iy=factor*y+voff
	write(1,100) ix,iy,ia,ib,theta
100	format(i8,1x,i8,1x,i8,1x,i8,1x,f7.2,' E')
	end
C
C	DRAW A COUNTERCLOCKWISE ARC OF RADIUS R WITH CENTER (X,Y) FROM
C	ANGLE A TO ANGLE B (IN DEGREES).
C	0 DEGREES POINTS ALONG THE POSITIVE X AXIS.
	subroutine sarc(x,y,r,a,b)
	real*8 x,y,r,a,b,unitpx
	real*8 factor,xmin,ymin,xmax,ymax, hoff,voff
	integer ix,iy,ir,penwid
	character cmd
	common /penwbk/ unitpx,penwid
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	cmd='A'
	go to 10
C
C	DRAW A CLOCKWISE ARC OF RADIUS R WITH CENTER (X,Y) FROM ANGLE A TO
C	ANGLE B (IN DEGREES). 0 DEGREES POINTS ALONG THE POSITIVE X AXIS.
	entry sarcn(x,y,r,a,b)
	cmd='N'
 10	ir=factor*r
c       IF RADIUS IS < HALF A PIXEL, JUST PLOT CENTER
	ir=max(ir,0.5d0*unitpx)
	ix=factor*x+hoff
	iy=factor*y+voff
	write(1,100) ix,iy,ir,a,b,cmd
100	format(i8,2(1x,i8),2(1x,f7.2),1X,A1)
	end
c
c	DRAW THE SMALLER ARC ON THE CIRCLE OF CENTER c AND RADIUS r
c	FROM z1 TO z2  (ASSUMED TO BE ON THE CIRCLE)
	subroutine drwarc(z1,z2,c,r)
	complex*16 z1,z2,c
	real*8 r,a,b,degrad
	parameter (degrad=57.29577951308232087679815481d0)
c	STRAIGHT LINE
	if(r .lt. 0.0) then
	   call ssegmt(dreal(z1),dimag(z1),dreal(z2),dimag(z2))
	   return
	end if
c	TRUE CIRCLE
	a= degrad*datan2(dimag(z1-c),dreal(z1-c))
	if(a .lt. 0.0) a=a+360.0
	b= degrad*datan2(dimag(z2-c),dreal(z2-c))
	if(b .lt. 0.0) b=b+360.0
	if( (b-a .lt. 180.0 .and. b-a .gt. 0.0) .or.
	1    (b-a .lt. -180.0) ) then
	   call sarc(dreal(c),dimag(c),r,a,b)
	else
	   call sarcn(dreal(c),dimag(c),r,a,b)
	end if
	end
C
C	CREATE A WINDOW OUTSIDE WHICH NO GRAPHICS WILL BE PLOTTED
	subroutine swindw(x0,y0,x1,y1)
	real*8 x0,y0,x1,y1,xwin0,ywin0,xwin1,ywin1
	integer ix0,iy0,ix1,iy1
	real*8 factor,xmin,ymin,xmax,ymax,hoff,voff
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	common /windw/ xwin0,ywin0,xwin1,ywin1
	xwin0 = x0
	ywin0 = y0
	xwin1 = x1
	ywin1 = y1
	ix0=factor*x0+hoff
	iy0=factor*y0+voff
	ix1=factor*x1+hoff
	iy1=factor*y1+voff
	write(1,100) ix0,iy0,ix0,iy1,ix1,iy1,ix1,iy0
100	format('NP ',i6,1x,i6,' M ',i6,1x,i6,' L ',/,
	1    3x,i6,1x,i6,' L ',i6,1x,i6,' L closepath clip NP')
	end
C
C	FILL THE CURRENT PATH
C	WITH THE SHADE P ( X AND Y ARE IGNORED )
c          P = 0 => BLACK
c          P = 1 => WHITE
	subroutine fill(x,y)
	real*8 x,y
	integer ix,iy
	write(1,'(A)') ' fill'
	return
C
C	FILL THE CURRENT PATH
C	WITH THE PATTERN P ( IX AND IY ARE IGNORED )
	entry ifill(ix,iy)
	write(1,'(A)') ' fill ST'
	return
C
C	FILL THE CURRENT PATH AND STROKE IT
C	( IX AND IY ARE IGNORED )
	entry fillst(ix,iy)
	write(1,'(A)') ' FS'
	end
C
C	REVERSE THE PATH
	subroutine revers
	write(1,'(A)') 'reversepath'
	end
C
C	POSITION THE CURSOR IN ABSOLUTE COORDINATES (IX,IY)
C	AND WRITE THE GIVEN STRING
	subroutine itext(ix,iy,string)
	integer ix,iy,lnblnk
	real*8 x,y
	character string*(*)
	call iposit(ix,iy)
	write(1,100) string(1:lnblnk(string))
100	format('(',A,') show')
	return
C
C	POSITION THE CURSOR IN USER COORDINATES (X,Y)
C	AND WRITE THE GIVEN STRING
	entry stext(x,y,string)
	call smove(x,y)
	write(1,100) string(1:lnblnk(string))
	end
c
c	DRAW A BOX AROUND THE SPECIFIED FRAME
	subroutine sbox(x0,y0,x1,y1,penwid)
	real*8 x0,y0,x1,y1
	integer penwid,p,ix0,iy0,ix1,iy1
	real*8 factor,xmin,ymin,xmax,ymax,hoff,voff
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	p=penwid
	call ongrf(p)
	ix0=factor*x0+hoff
	iy0=factor*y0+voff
	ix1=factor*x1+hoff
	iy1=factor*y1+voff
	write(1,100) ix0,iy0,ix0,iy1,ix1,iy1,ix1,iy0
100	format(i6,1x,i6,' M ',i6,1x,i6,' L ',/,
	1    i6,1x,i6,' L ',i6,1x,i6,' L closepath ')
	call offgrf
	end
c
c	CREATE A LABELLED COORDINATE FRAME
	subroutine sframe(xmin,ymin,xmax,ymax)
	real*8 xmin,ymin,xmax,ymax,x,y,delta,sinch
	real*8 x0,y0,x1,y1,xcen,ycen,xlab,ypos
	integer i,digit,lnblnk
	character tag*20, label*40, title*80, cenfmt*20,lines*100
	common /userin/ sinch
	x0=xmin
	y0=ymin
	x1=xmax
	y1=ymax
c	GET CENTER OF COORDINATE SYSTEM
	xcen = (xmin+xmax)/2.0d0
	ycen = (ymin+ymax)/2.0d0
	write(*,*) 'Enter xcen, ycen (center of coordinates; real): '
	write(*,'(A,4f14.10)') 'Defaults: ',xcen,ycen
	read(*,*)  xcen,ycen
c	GET TICK LENGTH
	digit = nint(dlog10(dabs(x1-x0)/40.0))
	delta = dexp(dlog(10.0d0)*digit)
	call gfxdoc('Begin Frame')
c       SET DATA FONT
	write(1,'(A)') 'setdatafont'
c       DRAW BOX AROUND FRAME
	call sbox(x0,y0,x1,y1,5)
c       LABEL CENTERS OF AXES AND GIVE TICK LENGTH
	write(cenfmt,'(A,I2,A,I2,A)')
	1    '(F',abs(digit)+4,'.',abs(digit)+1,')'
	write(tag,cenfmt) xcen
	call stext(xcen-0.1*sinch,y0-0.1*sinch,tag)
	write(tag,cenfmt) ycen
	call stext(x1+0.02*sinch,ycen,tag)
	write(label,'(A,I3)') 'One tick = 10^',digit
	call stext(x0,y0-0.1*sinch,label)
C       DRAW TICKS
	call ongrf(1)
	x = xcen
	call ssegmt(x,y0,x,y0+0.15*sinch)
	call ssegmt(x,y1,x,y1-0.15*sinch)
 2	if(x .ge. x1) go to 12
	do 10 i=1,4
	   x = x+delta
	   if(x .ge. x1) go to 12
	   call ssegmt(x,y0,x,y0+0.05*sinch)
 10	   call ssegmt(x,y1,x,y1-0.05*sinch)
	x = x+ delta
	if(x .ge. x1) go to 12
	call ssegmt(x,y0,x,y0+0.1*sinch)
	call ssegmt(x,y1,x,y1-0.1*sinch)
	go to 2
 12	x = xcen
 14	do 20 i=1,4
	   x = x-delta
	   if(x .le. x0) go to 22
	   call ssegmt(x,y0,x,y0+0.05*sinch)
 20	   call ssegmt(x,y1,x,y1-0.05*sinch)
	x = x- delta
	if(x .le. x0) go to 22
	call ssegmt(x,y0,x,y0+0.1*sinch)
	call ssegmt(x,y1,x,y1-0.1*sinch)
	go to 14
 22	y=ycen
	call ssegmt(x0,y,x0+0.15*sinch,y)
	call ssegmt(x1,y,x1-0.15*sinch,y)
 32	if(y .ge. y1) go to 42
	do 40 i=1,4
	   y = y+delta
	   if(y .ge. y1) go to 42
	   call ssegmt(x0,y,x0+0.05*sinch,y)
 40	   call ssegmt(x1,y,x1-0.05*sinch,y)
	y = y+ delta
	if(y .ge. y1) go to 42
	call ssegmt(x0,y,x0+0.1*sinch,y)
	call ssegmt(x1,y,x1-0.1*sinch,y)
	go to 32
 42	y = ycen
 44	do 50 i=1,4
	   y = y-delta
	   if(y .le. y0) go to 52
	   call ssegmt(x0,y,x0+0.05*sinch,y)
 50	   call ssegmt(x1,y,x1-0.05*sinch,y)
	y = y- delta
	if(y .le. y0) go to 52
	call ssegmt(x0,y,x0+0.1*sinch,y)
	call ssegmt(x1,y,x1-0.1*sinch,y)
	go to 44
 52	call offgrf
c	GET TITLE
	write(*,*) 'Enter name of picture: '
	read(*,'(A)') title
	call stext(xmin,ymax+0.1*sinch,title)
c       LABELS
c       We assume the labels have been stored in the file unit 2
c       This outputs them in two column format.
	xcen = (xmin+xmax)*0.5
	xlab = xcen - 3.0*sinch
	rewind(2)
	ypos = ymin - 0.15*sinch
 80	ypos = ypos - 0.12*sinch
	read(2,'(A)',END=999) lines
	call stext(xlab,ypos,lines)
	if(lnblnk(lines).gt.50) go to 80
 	read(2,'(A)',END=999) lines
	if(lnblnk(lines).gt.50) then
	   ypos = ypos - 0.12*sinch
	   call stext(xlab,ypos,lines)
	else
	   call stext(xcen,ypos,lines)
	end if
	go to 80
 999	call gfxdoc('End Frame')
	return
	end
c
c	SET SCALE WITH A MARGIN
	subroutine smargn(xmin,ymin,xmax,ymax,edge)
	real*8 xmin,ymin,xmax,ymax,edge,xmins,ymins,xmaxs,ymaxs
	integer sub
	assign 20 to sub
	go to 10
c
c	RESET SCALE WITH A MARGIN
	entry rmargn(xmin,ymin,xmax,ymax,edge)
	assign 30 to sub
 10	xmins=(1.0+edge)*xmin-edge*xmax
	xmaxs=(1.0+edge)*xmax-edge*xmin
	ymins=(1.0+edge)*ymin-edge*ymax
	ymaxs=(1.0+edge)*ymax-edge*ymin
	go to sub
 20	call sscale(xmins,ymins,xmaxs,ymaxs)
	return
 30	call rescal(xmins,ymins,xmaxs,ymaxs)
	end
c
c	DOCUMENT OUTPUT FILE
	subroutine gfxdoc(commnt)
	character commnt*(*)
	write(1,'(A,A)') '%%',commnt
	end
C
C	DRAW A LABELLED ARROW
	subroutine slabel(x,y,length,angle,label)
	real*8 x,y,length,angle,angle1,factor,xmin,ymin,xmax,ymax,hoff,voff
	character label*(*)
	integer ix,iy,lnblnk,num
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	ix=factor*x+hoff
	iy=factor*y+voff
c       NORMALIZE ANGLE BETWEEN 0 AND 360
	num=angle/360.0
	angle1 = angle -num*360.0d0
        if(angle1 .lt. 0.0) angle1 = angle1 + 360.0d0
	write(1,100) ix,iy,length,angle1,label(1:lnblnk(label))
100	format(2(i6,1x),2(f8.3,1x),'(',A,') putarrow')
	end
c
C	SUBROUTINES FOR WRITING RASTER FILES:
C
C	THE VERTICAL INTERVAL [YMIN,YMAX] WILL BE SAMPLED
C	AT JUSED POINTS: Y = YMIN + J*(YMAX-YMIN)/(JUSED-1). NOTE
C	THAT THESE ARE CHOSEN UNIFORMLY INCLUDING THE ENDPOINTS.
C	LIKEWISE, THE HORIZONTAL INTERVAL [XMIN,XMAX] WILL BE SAMPLED
C	AT IUSED POINTS: X = XMIN + I*(XMAX-XMIN)/(IUSED-1).
c
c       BITMAP = RASTER IMAGE OF FILLED CURVE (1 if in, 0 if out)
c       CROSS = 1 IF HIT AT LEAST ONCE, 0 IF NEVER HIT
	subroutine onbit(xmin,ymin,xmax,ymax)
	real*8 xmin,ymin,xmax,ymax,x0,y0,x1,y1,scale,diff
	integer iused,jused,lused,j,l,jdepth/1/
	character bitmap(512,4096),cross(512,4096),c
	common /bits/ bitmap,cross,iused,jused,lused
	common /bitwin/ x0,y0,x1,y1,scale
	iused = 256
	jused = 256
	write(*,*) 'Enter numbers of x and y bits: '
	write(*,*) 'Defaults: ',iused,' , ',jused
	read(*,*) iused,jused
	lused = iused*jdepth/8
	if(lused*8/jdepth.lt.iused) lused=lused+1
c       ENSURE THAT IUSED IS A MULTIPLE OF 8 IF JDEPTH = 1
	iused = lused*8/jdepth
c	ENLARGE WINDOW TO HAVE ASPECT RATIO (IUSED-1) BY (JUSED-1)
	diff = (xmax-xmin)*(jused-1) - (ymax-ymin)*(iused-1)
	if ( diff .gt. 0.0 ) then
	   x1 = xmax
	   x0 = xmin
	   y1 = ymax + diff/(2*iused-2)
	   y0 = ymin - diff/(2*iused-2)
	else
	   y1 = ymax
	   y0 = ymin
	   x1 = xmax - diff/(2*jused-2)
	   x0 = xmin + diff/(2*jused-2)
	end if
	scale = (x1-x0)/(iused-1)
c	INITIALIZE BITMAPS
	c=char(0)
	do 10 l=1,lused
	do 10 j=1,jused
	   bitmap(l,j)=c
	   cross(l,j)=c
 10	continue
	end
c
c	FILL RASTER IMAGE
	subroutine offbit
	integer iused,jused,lused,j,l
	character bitmap(512,4096),cross(512,4096)
	common /bits/ bitmap,cross,iused,jused,lused
C	MAKE A RUNNING SUM OF THE COLUMNS OF THE BITMAP MOD 2.
	do 10 l=1,lused
	   do 10 j=2,jused
 10	   bitmap(l,j)=
	1	   char(xor(ichar(bitmap(l,j-1)),ichar(bitmap(l,j))))
	do 20 l=1,lused
	   do 20 j=1,jused
c       FIRST CHOICE: LIMIT SET IS BLACK (FILLED)
c       SECOND CHOICE: LIMIT SET IS WHITE (UNFILLED)
c$$$ 20	   bitmap(l,j)=char(or(ichar(cross(l,j)),ichar(bitmap(l,j))))
 20	   bitmap(l,j)=char(and(not(ichar(cross(l,j))),ichar(bitmap(l,j))))
	end
c
c	WRITE POSTSCRIPT RASTER IMAGE
	subroutine offfil(inout)
	real*8 factor,xmin,ymin,xmax,ymax,hoff,voff
	real*8 x0,y0,x1,y1,scale,ss,xlen,
	1    pxmin,pymin,pxmax,pymax
	integer ix0,iy0,j,l,iused,jused,lused,jdepth/1/,inout
	character bitmap(512,4096),cross(512,4096),fmt*30
	character*5 bool(2)/'true','false'/
	common /frame/ factor,xmin,ymin,xmax,ymax,hoff,voff
	common /picwin/ pxmin,pymin,pxmax,pymax
	common /bits/ bitmap,cross,iused,jused,lused
	common /bitwin/ x0,y0,x1,y1,scale
c       CLIPPING WINDOW
	write(1,'(A)') 'gsave'
	call swindw(pxmin,pymin,pxmax,pymax)
	if(iused.ge.jused) then
		xlen=x1-x0
	else
		xlen=y1-y0
	end if
	ix0=factor*x0+hoff
	iy0=factor*y0+voff
	ss = xlen*factor
	write(1,100) jdepth,iused,jused,ss,ix0,iy0,bool(inout)
100	format(3(I5,1x),f20.5,2(1x,i6),1x,A,1x,'rasterimage')
c	DUMP RASTER
C	WRITE THIS TO THE OUTPUT FILE, COLUMN BY COLUMN.
c	      HEX OUTPUT
	write(fmt,'(A,I4,A)') '(',lused,'Z2.2)'
	write(1,fmt) ((bitmap(l,j),l=1,lused),j=1,jused)
c	WRITE GRESTORE
	write(1,'(/,A)') 'grestore'
	end
c
C	FOR EACH SEGMENT IN THE POLYGON IN THE INPUT FILE, RECORD IN
C	BIT[I][J] ALL CROSSINGS WITH THE VERTICAL LINE SEGMENT
C	(I,J-.5), (I,J+.5). TO ALLOW FOR LIMIT POINT SET OUTSIDE
C	THE WINDOW, THE ENTRY BIT[I][0] RECORDS ALL CROSSINGS WITH
C	THE SEMI-INFINITE INTERVAL (I,-INFTY),(I,0.5). */
C
	subroutine rline(xold,yold,x,y)
	real*8 xold,yold,x,y
	real*8 x0,y0,x1,y1,xmin,ymin,xmax,ymax,temp,slope,scale
	integer i0,i1,i,j0,j1,j,k,l,iused,jused,lused,cbit(8)
	integer jdepth/1/
	character bitmap(512,4096),cross(512,4096)
	common /bits/ bitmap,cross,iused,jused,lused
	common /bitwin/ xmin,ymin,xmax,ymax,scale
	data cbit/128,64,32,16,8,4,2,1/
	if(x .lt. xold) then
	   x0=x
	   y0=y
	   x1=xold
	   y1=yold
	else
	   x0=xold
	   y0=yold
	   x1=x
	   y1=y
	end if
	i0 = (x0-xmin)/scale
	if(i0*scale .lt. x0-xmin) i0=i0+1
	i0 = max(i0,0)
	i1 = (x1-xmin)/scale
	if(i1*scale .ge. x1-xmin) i1=i1-1
	i1 = min(i1,iused-1)
	if( i1 .ge. i0) then
	   slope = (y1 - y0)/(x1 - x0)
	   temp = (y0-ymin+slope*(xmin-x0))/scale+0.5
	   do 10 i = i0,i1
	      j = temp+i*slope
C	ALLOW FOR INTERCEPTS OUTSIDE THE WINDOW
	      j = max(j,0)
	      j = min(j,jused-1)
	      if(jdepth.eq.1) then
	         k = mod(i,8)
	         l = (i-k)/8
	         bitmap(l+1,j+1)=
	1	      char(xor(ichar(bitmap(l+1,j+1)),cbit(k+1)))
	         cross(l+1,j+1)=
	1	      char(or(ichar(cross(l+1,j+1)),cbit(k+1)))
	      else
	         bitmap(i+1,j+1)=
	1	      char(xor(ichar(bitmap(i+1,j+1)),255))
	         cross(i+1,j+1)=char(or(ichar(cross(i+1,j+1)),255))
	      end if
10	   continue
	else
	   i =nint((x0-xmin)/scale)
	   j0=(y0-ymin)/scale
	   if(j0*scale .lt. y0-ymin) j0=j0+1
	   j0 = max(j0,0)
	   j1 = (y1-ymin)/scale
	   if(j1*scale .ge. y1-ymin) j1=j1-1
	   j1 = min(j1,jused-1)
	   do 20 j = j0,j1
	      if(jdepth.eq.1) then
	         k = mod(i,8)
	         l = (i-k)/8
	         cross(l+1,j+1)=
	1	      char(or(ichar(cross(l+1,j+1)),cbit(k+1)))
	      else
	         cross(i+1,j+1)=char(or(ichar(cross(i+1,j+1)),255))
	      end if
20	   continue
	end if
	end
c
c       OUTPUT COLOR IMAGES
	subroutine outcbit
	real*8 factor,xwin0,ywin0,xwin1,ywin1,hoff,voff
	real*8 xmin,ymin,xmax,ymax,ssx,ssy
	integer iused,jused,lused,i,j,l,ix0,iy0
	character cbit(ISIZE,JSIZE),fmt*100
	common /frame/ factor,xwin0,ywin0,xwin1,ywin1,hoff,voff
	common /picwin/ xmin,ymin,xmax,ymax
	common /clrbit/ cbit,iused,jused,lused
c       CLIPPING WINDOW
	write(1,'(A)') 'gsave'
	call swindw(xmin,ymin,xmax,ymax)
	ix0=factor*xmin+hoff
	iy0=factor*ymin+voff
	ssx = (xmax-xmin)*factor
	ssy = (ymax-ymin)*factor
	write(1,100) iused,jused,ssx,ssy,ix0,iy0
100	format(2(I5,1x),2(f20.5,1x),2(1x,i6),1x,'mycolorimage')
c	DUMP RASTER
C	WRITE THIS TO THE OUTPUT FILE, COLUMN BY COLUMN.
c	      HEX OUTPUT
	write(fmt,'(A,I4,A)') '(',lused,'Z2.2)'
	write(1,fmt) ((cbit(l,j),l=1,lused),j=1,jused)
c	WRITE GRESTORE
	write(1,'(/,A)') 'grestore'
	end


