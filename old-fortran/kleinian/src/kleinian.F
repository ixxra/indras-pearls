c	KLEINIAN.F 
c	MENUS FOR KLEINIAN GROUPS PROGRAMS
c
	program kleinian
c       NUMFIL = NUMBER OF PICTURE FILES PRODUCED
	integer numfil,lnblnk,frame
	character numfmt*20,choice*15
	save frame
	common /numrun/ numfil,numfmt
	numfil=0
	write(*,'(A,//)') 'KLEINIAN GROUPS PROGRAMS, INDRA VERSION 1.1: January 2003'
c       SELECT A PROJECT
	write(*,'(A)') 'Select a menu option by typing the',
	1    'short command name associated to it.'
	frame = 1
c       DO ANY START UP NECESSARY
	call strtup
c
c       MAIN MENU
 10	write(*,'(/,5X,A,/)') ' MAIN MENU: '
	write(*,'(8X,A15,3X,A)') 
	1    'lset:', 'Limit Set Programs',
	2    'chain:','Circle Chain Programs',
	3    'bndy:','Boundary Programs',
	4    'gpcalc:','Calculations with generators',
	4    'bdcalc:','Boundary Point Calculations',
	5    'draw:','Draw lines, circles, labels, etc.',
	4    'noframe:','Do not plot a labelled frame',
	4    'new:','Close current picture and start new one',
	5    'quit:','Just Press Return to Exit'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'lset') then
	   call lsmenu
	else if(choice.eq.'chain') then
	   call chmenu
	else if(choice.eq.'bndy') then
	   call bdmenu
	else if(choice.eq.'gpcalc') then
	   call gpcalc
	else if(choice.eq.'bdcalc') then
	   call bdcalc
	else if(choice.eq.'draw') then
	   call drmenu
	else if(choice.eq.'noframe') then
	   frame = 0
	else if(choice.eq.'new') then
c	CLOSE OLD PICTURE AND START NEW ONE
	   call endup(frame)
	   call strtup
	else if(choice.ne.'quit'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
c       CLEAN UP FILES AND TERMINATE PROGRAM
 999	call endup(frame)
	end
c	
c	START UP
c          This sets the output filenames and x-y frame
c             grpon  = .false. -> no group set yet
c             ciron  = .false. -> no circles set yet
c             arcon  = .false. -> no arcs set yet
c             epson  = .false. -> no thresholds selected yet
c             outon  = .false. -> no output file selected yet
c             raston = .false. -> no raster started yet
c             curvon = .false. -> plot as file of points
c               "      .true.  -> plot as curve
	subroutine strtup
	logical grpon,ciron,arcon,epson,outon,raston,curvon
	integer mlev
	common /flags/ grpon,epson,outon,raston,curvon
	common /gflags/ ciron,arcon
	common /levmax/ mlev
	grpon=.false.
	ciron=.false.
	arcon=.false.
	epson=.false.
	outon=.false.
	raston=.false.
	curvon=.true.
c       DEFAULT MAX LEVEL IN TREE SEARCH
	mlev=MAXLEV
	end
c	
c	GET POSTSCRIPT FILE (UNIT=1)
	subroutine onout(x0,y0,x1,y1,prefix)
	character prefix*(*),outfil*100,curout*100,numfmt*20
	real*8 x0,y0,x1,y1,xmin,ymin,xmax,ymax
	integer numfil,l,lnblnk
	logical grpon,epson,outon,raston,curvon
	common /numrun/ numfil,numfmt
	common /picwin/ xmin,ymin,xmax,ymax
	common /flags/ grpon,epson,outon,raston,curvon
	if(outon) return
c	DEFAULT SCALES
	xmin=x0
	ymin=y0
	xmax=x1
	ymax=y1
c	SET SCALE (USED FOR CLIPPING WINDOW)
	write(*,*) 'Enter xmin,ymin,xmax,ymax: '
	write(*,'(2(A,F20.10))') 'Defaults: xmin=',xmin,' ymin=',ymin
	write(*,'(2(A,F20.10))') '          xmax=',xmax,' ymax=',ymax
	read(*,*) xmin,ymin,xmax,ymax
c       NUMBER OF PICTURE FILES PRODUCED
	numfil = numfil+1
c       FORMAT FOR FILENAMES
	l = alog(1.0*numfil)/alog(10.0)
	write(numfmt,'(A,I1,A)') '(A,I',l+1,',A)'
	write(curout,numfmt) prefix,numfil,'.ps'
	write(*,*) 'Enter name of postscript file (default: ',
	1    curout(1:lnblnk(curout)),'): '
	read(*,'(A)') outfil
	if(outfil .eq. ' ') outfil = curout
	call on(outfil)
	call smargn(xmin,ymin,xmax,ymax,0.1d0)
	outon=.true.
	end
c
c       SET THE WINDOW
	subroutine setwin(x0,y0,x1,y1)
	real*8 x0,y0,x1,y1,xmin,ymin,xmax,ymax
	common /picwin/ xmin,ymin,xmax,ymax
c	DEFAULT SCALES
	xmin=x0
	ymin=y0
	xmax=x1
	ymax=y1
c	SET SCALE (USED FOR CLIPPING WINDOW)
	write(*,*) 'Enter xmin,ymin,xmax,ymax: '
	write(*,'(2(A,F20.10))') 'Defaults: xmin=',xmin,' ymin=',ymin
	write(*,'(2(A,F20.10))') '          xmax=',xmax,' ymax=',ymax
	read(*,*) xmin,ymin,xmax,ymax
	end
c	
c	END UP
	subroutine endup(frame)
	integer frame,pen,penw
	logical grpon,epson,outon,raston,curvon
	real*8 xmin,ymin,xmax,ymax,unitpx
	common /penwbk/ unitpx,penw
	common /picwin/ xmin,ymin,xmax,ymax
	common /flags/ grpon,epson,outon,raston,curvon
	if(outon) then 
c       DRESS UP AND CLOSE POSTSCRIPT PICTURE
	   call ccolor('Black')
	   if(frame.eq.1) then
c       LABELS OUTPUT IN SFRAME
	      call sframe(xmin,ymin,xmax,ymax)
	   else 
	      pen=penw
	      call sbox(xmin,ymin,xmax,ymax,pen)
	   end if
	   call off
	end if
	grpon=.false.
	outon=.false.
	end
c
c       PLOTTING ROUTINES WITH CLIPPING AND
c       STYLE SELECTION
c
c       ROUTINE FOR PLOTTING AN (X,Y)
	subroutine plotpt(x,y)
	complex*16 wcen
	real*8 x,y,xold,yold,xclip,yclip,xmin,ymin,xmax,ymax
	real*8 x0,y0,x1,y1,windno,delwno
	real*8 bound,eps,resol,winrad
	integer numpts,maxlev,nomove
	logical inflag,inflag1,grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	common /bnds/ bound,eps,resol,wcen,winrad
	common /picwin/ x0,y0,x1,y1
	common /motion/ nomove
c       xold,yold IS THE LAST VALUES OF x,y IN A CALL TO PLOTPT
c       inflag RECORDS IF xold,yold WAS IN CLIP WINDOW
c       xclip,yclip IS THE LAST POINT OUTSIDE THE CLIP WINDOW
	common /clip/ xold,yold,xclip,yclip,windno,inflag
c	POINT PLOT
	if(.not.curvon) then
	   if(x.gt.x1.or.x.lt.x0.or.y.gt.y1.or.y.lt.y0) return
	   call spoint(x,y)
c	CURVE PLOT
	else 
	   if(numpts.eq.0) then
c	FIRST POINT
c          INITIATE PATH AND MOVE TO (x,y)
	      inflag=x.le.x1.and.x.ge.x0.and.y.le.y1.and.y.ge.y0
	      call cmove(x,y)
c       IF OUTSIDE CLIP WINDOW, SET (xclip,yclip) to (x,y) 
c       AND INITIATE WINDING NUMBER AT 0
	      if(.not.inflag) then
		 xclip=x
		 yclip=y
		 windno=0.0d0
	      end if
	   else if(nomove.eq.0) then
c       SUBSEQUENT POINTS
c          IF NOT FURTHER THAN resol FROM LAST CALL
	      if( dsqrt((x-xold)*(x-xold) + (y-yold)*(y-yold))
	1	   .le. resol) return
	      inflag1=x.le.x1.and.x.ge.x0.and.y.le.y1.and.y.ge.y0
c       LAST POINT IN; CURRENT POINT IN FRAME
c       DRAW A LINE TO (x,y)
	      if(inflag.and.inflag1) then
		 call cline(x,y)
c       LAST POINT OUT; CURRENT POINT IN FRAME
	      else if(.not.inflag.and.inflag1) then
c       DRAW PATH AROUND CLIP WINDOW WITH RECORDED WINDING NUMBER
		 call clpdrw(xclip,yclip,xold,yold,windno)
c       DRAW LINE TO CURRENT POINT
		 call cline(x,y)
c       LAST POINT IN; CURRENT POINT OUT OF FRAME
	      else if(inflag.and..not.inflag1) then
		 call cline(x,y)
c	SAVE (X,Y) AS LAST POINT PLOTTED (OUTSIDE CLIPPING BOX)
		 xclip=x
		 yclip=y
		 windno = 0.0d0
c       LAST POINT OUT; CURRENT POINT OUT OF FRAME
	      else 
c	AUGMENT WINDING NUMBER OF CURVE OUTSIDE CLIP BOX
		 windno = windno + delwno(xold,yold,x,y)
	      end if
	      inflag=inflag1
c       JUST DRAW LINE (nomove=1)
	   else
	      call cline(x,y)
	      xclip = x
	      yclip = y
	      windno = 0.0d0
	   end if
	   xold=x
	   yold=y
	end if
	xmin = dmin1(x,xmin)
	ymin = dmin1(y,ymin)
	xmax = dmax1(x,xmax)
	ymax = dmax1(y,ymax)
	numpts=numpts+1
	return
	end
c
c     FLUSH OUT UNDRAWN CURVE
	subroutine clipend
	real*8 xold,yold,xclip,yclip,windno
	logical inflag
	common /clip/ xold,yold,xclip,yclip,windno,inflag
        call clpdrw(xclip,yclip,xold,yold,windno)
	end
c
c	DRAW A LINE TO A POINT
	subroutine cline(x,y)
	real*8 x,y,xold,yold
	integer outsty
	common /outfmt/ outsty
	save xold,yold
	if(outsty.eq.1) then
	   call sline(x,y)
	else if(outsty.eq.2) then
	   call rline(xold,yold,x,y)
	   xold=x
	   yold=y
	end if
	return
c
c       MOVE TO A POINT
	entry cmove(x,y)
	if(outsty.eq.1) then
	   call smove(x,y)
	else if(outsty.eq.2) then
	   xold=x
	   yold=y
	end if
	end
c
c	DRAW AROUND THE CLIPPING FRAME
c		ASSUME: BOTH POINTS ARE OUTSIDE CLIPPING FRAME
c		        THE CURRENT POINT IS (X0,Y0)
	subroutine clpdrw(x0,y0,x1,y1,windno)
	real*8 x0,y0,x1,y1,windno,x(4),y(4),ang(4)
	real*8 xmin,ymin,xmax,ymax,u0,v0,u1,v1,wno,delwno
	integer i,imin,j,l
	common /picwin/ xmin,ymin,xmax,ymax
c	ANGLES OF CORNERS IN COUNTERCLOCKWISE ORDER
	x(1)=xmin
	y(1)=ymin
	x(2)=xmax
	y(2)=ymin
	x(3)=xmax
	y(3)=ymax
	x(4)=xmin
	y(4)=ymax
	imin=1
	do 10 i=1,4
	   ang(i) = delwno(x0,y0,x(i),y(i))
	   if(ang(i) .lt. 0.0d0) ang(i)=ang(i)+1.0d0
	   if(ang(i) .lt. ang(imin)) imin = i
10	continue
	wno=0
	u0=x0
	v0=y0
	if(windno.ge.0.0d0) then
	   do 30 l=0,int(windno)
	      do 30 i=0,3
	      j=mod(imin+i-1,4)+1
	      u1=x(j)
	      v1=y(j)
	      wno = wno + delwno(u0,v0,u1,v1)
	      if(wno.ge.windno) go to 50
	      call cline(u1,v1)
	      u0=u1
30	      v0=v1
	else
	   do 40 l=0,int(-windno)
	      do 40 i=0,3
	      j=mod(imin+2-i,4)+1
	      u1=x(j)
	      v1=y(j)
	      wno = wno + delwno(u0,v0,u1,v1)
	      if(wno.le.windno) go to 50
	      call cline(u1,v1)
	      u0=u1
40	      v0=v1
	end if
50	call cline(x1,y1)
	end
c
c	FIND THE RELATIVE ARGUMENT CHANGE
	real*8 function delwno(x0,y0,x1,y1)
	real*8 x0,y0,x1,y1
	real*8 bound,eps,resol,winrad,twopi
	complex*16 wcen,z0,z1
	common /bnds/ bound,eps,resol,wcen,winrad
	parameter (twopi=6.28318530717958647688d0)
	z0=dcmplx(x0,y0)
	z1=dcmplx(x1,y1)
	z1=(z1-wcen)/(z0-wcen)
	delwno=datan2(dimag(z1),dreal(z1))/twopi
	end
