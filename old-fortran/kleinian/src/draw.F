c 	DRAW.F
c       
c	This contains auxiliary drawing routines
c       
	subroutine drmenu
	character choice*15,col*30,lbl*100
	real*8 x,y,len,ang,unitpx
	real*8 xmin,ymin,xmax,ymax,x0,y0,x1,y1
	integer lnblnk,penw,pen
	common /penwbk/ unitpx,pen
	common /picwin/ xmin,ymin,xmax,ymax
c       INITIATE OUTPUT FILE
	call onout(-1.1d0,-1.1d0,1.1d0,1.1d0,'pic')
	call ongrf(1)
	call swindw(xmin,ymin,xmax,ymax)
 10	write(*,*) 'DRAWING MENU: '
	write(*,'(8X,A15,3X,A)')
	1    'text:','Position some text',
	2    'label:','Label a point',
	3    'circle:','Draw a circle (or line)',
	4    'disk:','Draw a filled disk',
	5    'arc:','Draw an arc (or line segment)',
	3    'box:','Draw rectangular box',
	3    'point:','Draw point(s)',
	6    'file:','Plot a file of points',
	4    'gpcalc:','Calculations with generators',
	5    'pen:','Set new penwidth',
	9    'color:','Switch to a new color',
	1    'up:','Just Press Return To Go Up To Previous Menu'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'text') then
	   write(*,*) 'Enter x, y coords., text: '
	   write(*,*) '(2 real*8, string)'
	   read(*,*) x,y,lbl
	   call stext(x,y,lbl)
	else if(choice.eq.'label') then 
	   write(*,*) 'Enter x, y coords., length, angle, text: '
	   write(*,*) '(4 real*8, string)'
	   read(*,*) x,y,len,ang,lbl
	   call slabel(x,y,len,ang,lbl)
	else if(choice.eq.'circle') then 
	   call cirmnu
	else if(choice.eq.'disk') then 
	   call dskmnu
	else if(choice.eq.'arc') then 
	   call arcmnu
	else if(choice.eq.'box') then 
	   write(*,*) 'Enter xmin,ymin,xmax,ymax'
	   read(*,*) x0,y0,x1,y1
	   penw=pen
	   call sbox(x0,y0,x1,y1,penw)
	else if(choice.eq.'point') then 
	   call ptmnu
	else if(choice.eq.'file') then 
	   call filplt
	else if(choice.eq.'gpcalc') then
c       GROUP CALCULATIONS
	   call gpcalc
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
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	call offgrf
	return
	end
c       
c	DRAWING MENUS:
c       CIRCLES OR POINTS
	subroutine cirmnu
	character choice*20,wrdlbl*MAXLEV,alabel*MAXLEN,blabel*MAXLEN,
	1    clabel*MAXLEN,reply, gentyp*20
	complex*16 z1,z2,z3,c,c2,w(4),w1,cen(MAXSIDE),y(4,MAXSIDE),
	1    begpt,endpt,midpt,eta,wa(4),wai(4),wb(4),wbi(4),
	1    wc(4),achain(MAXCHAIN),bchain(MAXCHAIN)
	real*8 r,r2,rad(MAXSIDE),zero,raddeg,twopi,den,theta,eps
	real*8 xmin,ymin,xmax,ymax
	integer side(MAXSIDE),fside(MAXSIDE),nside
	integer err,tag(MAXLEV),lev,i,outsty,p,q,numpts,maxlev,n
	logical grpon,epson,outon,raston,curvon,ciron,arcon,diskon
	common /flags/ grpon,epson,outon,raston,curvon
	common /gflags/ ciron,arcon
	common /gens/ y, side, fside,  nside, gentyp
	common /cirs/ cen,rad
        common /outfmt/ outsty
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	parameter (zero=1.0d-10,
	1    raddeg=0.01745329251994329576923690768d0,
	2    twopi=6.283185307179586476925286766d0)
	outsty = 1
	diskon=.false.
	go to 10
c
c       DISKS
	entry dskmnu
	outsty = 2
	diskon=.true.
 10	write(*,*) 'SELECT STYLE OF CIRCLE '
	write(*,'(8X,A15,3X,A)')
	1    'cenrad:','Specify center and radius',
	2    '3pt:','Circle thru 3 points',
	3    'limpts:','Circle through 3 limit points (given by words)',
	4    'parabolic:','Circle through parabolic fixed point',
	4    'isometric:','Isometric circle',
	5    'schottky:','Schottky circle'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'cenrad') then
	   write(*,*) 'Enter center, radius: (complex*16, real*8) '
	   write(*,*) '(Rad < 0 => line with angle -rad (degrees)',
	1	' thru cen) '
	   read(*,*) c,r
	   if(r.lt.0.0) r=r*raddeg
	else if(choice.eq.'3pt') then
	   write(*,*) 'Enter 3 distinct points: (3 complex*16)'
	   read(*,*) z1,z2,z3
	   call cir3pt(z1,z2,z3,c,r)
	else if(choice.eq.'limpts') then
	   if(.not.grpon) call setgrp
	   write(*,'(A$)') 'Enter first word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,z1,w1,err)
	   write(*,'(A$)') 'Enter second word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,z2,w1,err)
	   write(*,'(A$)') 'Enter third word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,z3,w1,err)
	   call cir3pt(z1,z2,z3,c,r)
	else if(choice.eq.'parabolic') then
	   if(.not.grpon) call setgrp
 23	   write(*,'(A$)') 'Enter parabolic word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,begpt,w1,err)
	   eta = (w(1)+w(4))/w(3)
	   write(*,'(A$)') 'Enter angle from parabolic axis: '
	   read(*,*) theta
	   write(*,'(A$)') 'Enter word for end point: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,endpt,w1,err)
	   eta=eta/cdabs(eta)
	   eta=eta*cdexp((0.0d0,1.0d0)*raddeg*theta)
	   call arc2pt(begpt,endpt,eta,c,midpt,r)
	else if(choice.eq.'isometric') then
	   if(.not.grpon) call setgrp
	   write(*,'(A$)') 'Enter word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   if(cdabs(w(3)).le.zero) then
	      write(*,*) 'No isometric circle '
	      return
	   end if
	   c=-w(4)/w(3)
	   r=1.0d0/cdabs(w(3))
	else if(choice.eq.'schottky') then
	   if(.not.grpon) call setgrp
	   if(.not.ciron) call setcir
	   write(*,*) 'Enter number of Schottky circle: '
	   write(*,*) '(from 1 to ',nside,')'
	   read(*,*) i
	   c=cen(i)
	   r=rad(i)
	else
	   write(*,*) 'Bad style, select again'
	   go to 10
	end if
	if(grpon) then
	   write(*,'(A$)') 'Apply any words to the circle? (y or N)'
	   read(*,'(A)') reply
c       IF NO, GO TO PLOTTING ONE CIRCLE
	   if (reply.eq.'n') go to 90
 50	   write(*,*) 'SELECT OPTION FOR WORDS: '
	   write(*,'(8X,A15,3X,A)')
	1	'list:','List of words (ended by END)',
	2	'powers:','All powers of one word',
	2	'chain:','p/q chain of words',
	3	'horo:','p/q horocycle list'
	   write(*,'(/,5X,A$)') 'SELECT: '
	   read(*,'(A)') choice
	   if(choice.eq.'list') then
 	      write(*,'(A$)') 'Enter words (END to end): '
 55	      read(*,'(A)') wrdlbl
	      if(wrdlbl.eq.'END') return
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      call lfapply(w,c,c2,r,r2)
	      if(diskon) then
		 call sdisk(dreal(c2),dimag(c2),r2)
	      else
		 call scirc(dreal(c2),dimag(c2),r2)
	      end if
	      go to 55
	   else if(choice.eq.'powers') then
	      write(*,'(A$)') 'Enter prefix word: '
	      read(*,'(A)') wrdlbl
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      call lfapply(w,c,c2,r,r2)
	      c=c2
	      r=r2
 	      write(*,'(A$)') 'Enter word: '
	      read(*,'(A)') wrdlbl
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      write(*,'(A$)') 'Enter radius threshold: '
	      read(*,*) eps
 65	      if(diskon) then
		 call sdisk(dreal(c),dimag(c),r)
	      else
		 call scirc(dreal(c),dimag(c),r)
	      end if
	      call lfapply(w,c,c2,r,r2)
	      if(r2.lt.eps) return
	      c=c2
	      r=r2
	      go to 65
	   else if(choice.eq.'chain') then
	      write(*,'(A$)') 'Enter p, q: '
	      read(*,*) p,q
	      write(*,'(A$)') 'Enter a generator word: '
	      read(*,'(A)') alabel
	      call lbl2wrd(wa,tag,lev,alabel,err)
	      write(*,'(A$)') 'Enter b generator word: '
	      read(*,'(A)') blabel
	      call lbl2wrd(wb,tag,lev,blabel,err)
	      call matinv(wb,wbi)
 	      i = 0
	      call gfxdoc('Begin Circle Chain')
 80	      if(diskon) then
		 call sdisk(dreal(c),dimag(c),r)
	      else
		 call scirc(dreal(c),dimag(c),r)
	      end if
	      if(i.ge.p+q) then
		 call gfxdoc('End Circle Chain')
		 return
	      else if(i.le.q) then
		 call lfapply(wa,c,c2,r,r2)
		 i = i + p
	      else
		 call lfapply(wbi,c,c2,r,r2)
		 i = i - q
	      end if
	      c=c2
	      r=r2
	      go to 80
	   else if(choice.eq.'horo') then
	      continue
	   else
	      write(*,*) 'Bad choice; good bye!'
	   end if
	   return
	end if
 90	if(diskon) then
	   call sdisk(dreal(c),dimag(c),r)
	else
	   call scirc(dreal(c),dimag(c),r)
	end if
	return
c
c       POINTS
	entry ptmnu
	outsty = 2
 11	write(*,*) 'SELECT POINT TO PLOT '
	write(*,'(8X,A15,3X,A)')
	1    'coords:','Specify x-y coordinates',
	4    'fixpt:','Attractive fixed point'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'coords') then
	   write(*,*) 'Enter z: (complex*16) '
	   read(*,*) z1
	else if(choice.eq.'fixpt') then
	   if(.not.grpon) call setgrp
	   write(*,'(A$)') 'Enter word for fixed point: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,z1,w1,err)
	   write(*,*) 'Fixed point is ', z1	   
	else
	   write(*,*) 'Bad style, select again'
	   go to 11
	end if
	if(grpon) then
	   write(*,'(A$)') 'Apply any words to the point? (y or N)'
	   read(*,'(A)') reply
c       IF NO, GO TO PLOTTING ONE POINT
	   if (reply.eq.'n') go to 91
 51	   write(*,*) 'SELECT OPTION FOR WORDS: '
	   write(*,'(8X,A15,3X,A)')
	1	'list:','List of words (ended by END)',
	2	'powers:','All powers of one word',
	2	'chain:','p/q chain of words',
	2	'chaincurve:','p/q chain of words, plotted as curve',
	3	'horo:','p/q horocycle list'
	   write(*,'(/,5X,A$)') 'SELECT: '
	   read(*,'(A)') choice
	   if(choice.eq.'list') then
 	      write(*,'(A$)') 'Enter words (END to end): '
 56	      read(*,'(A)') wrdlbl
	      if(wrdlbl.eq.'END') return
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      call linfrac(w,z1,z2,err)
	      call spoint(dreal(z2),dimag(z2))
	      go to 56
	   else if(choice.eq.'powers') then
	      write(*,'(A$)') 'Enter prefix word: '
	      read(*,'(A)') wrdlbl
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      call linfrac(w,z1,z2,err)
	      z1=z2
 	      write(*,'(A$)') 'Enter word: '
	      read(*,'(A)') wrdlbl
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      write(*,'(A$)') 'Enter maximum power: '
	      read(*,*) n
	      do 66 i=0,n
	         call spoint(dreal(z1),dimag(z1))
		 call linfrac(w,z1,z2,err)
		 z1=z2
 66	      continue
	   else if(choice.eq.'chain') then
	      write(*,'(A$)') 'Enter p, q: '
	      read(*,*) p,q
	      write(*,'(A$)') 'Enter a generator word: '
	      read(*,'(A)') alabel
	      call lbl2wrd(wa,tag,lev,alabel,err)
	      write(*,'(A$)') 'Enter b generator word: '
	      read(*,'(A)') blabel
	      call lbl2wrd(wb,tag,lev,blabel,err)
	      call matinv(wb,wbi)
 	      i = 0
	      call gfxdoc('Begin Point Chain')
 81	      call spoint(dreal(z1),dimag(z1))
	      if(i.ge.p+q) then
		 call gfxdoc('End Point Chain')
		 return
	      else if(i.le.q) then
		 call linfrac(wa,z1,z2,err)
		 i = i + p
	      else
		 call linfrac(wbi,z1,z2,err)
		 i = i - q
	      end if
	      z1=z2
	      go to 81
	   else if(choice.eq.'chaincurve') then
	      outsty = 1
	      if(.not.epson) call geteps
	      write(*,'(A$)') 'Enter p, q: '
	      read(*,*) p,q
	      write(*,'(A$)') 'Enter a generator word: '
	      read(*,'(A)') alabel
	      call lbl2wrd(wa,tag,lev,alabel,err)
	      call matinv(wa,wai)
	      write(*,'(A$)') 'Enter b generator word: '
	      read(*,'(A)') blabel
	      call lbl2wrd(wb,tag,lev,blabel,err)
	      call matinv(wb,wbi)
 	      achain(1) = z1
	      i = 0
 82	      if(i.ge.p+q) then
		 go to 83
	      else if(i.le.q) then
		 call linfrac(wa,z1,z2,err)
		 i = i + p
	      else
		 call linfrac(wbi,z1,z2,err)
		 i = i - q
	      end if
	      achain(i+1) = z2
	      z1 = z2
	      go to 82
 83	      z1 = achain(1)
	      bchain(1) = z1
	      call linfrac(wb,z1,z2,err)
	      i = q
	      bchain(q+1) = z2 
	      z1 = z2
 84	      if(i.ge.p+q) then
		 go to 86
	      else if(i.le.p) then
		 call linfrac(wb,z1,z2,err)
		 i = i + q
	      else
		 call linfrac(wai,z1,z2,err)
		 i = i - p
	      end if
	      bchain(i+1) = z2
	      z1 = z2
	      go to 84
 86	      write(*,'(A$)') 'Enter conjugating word: (END to end) '
	      read(*,'(A)') clabel	      
	      if(clabel.eq.'END') return
	      call lbl2wrd(wc,tag,lev,clabel,err)
	      call gfxdoc('Begin Chain Curve')
	      numpts = 0
	      curvon=.true.
	      do 89 i = 1,q+1
		 call linfrac(wc,achain(i),z1,err)
 		 call plotpt(dreal(z1),dimag(z1))
		 call linfrac(wc,bchain(i),z1,err)
 89		 call plotpt(dreal(z1),dimag(z1))
	      call gfxdoc('End Chain Curve')
	      go to 86
	   else if(choice.eq.'horo') then
	      continue
	   else
	      write(*,*) 'Bad choice; good bye!'
	   end if
	   return
	end if
 91	call spoint(dreal(z1),dimag(z1))
	return
	end
c
c       ARCS
	subroutine arcmnu
	character choice*20,wrdlbl*MAXLEV,alabel*MAXLEN,blabel*MAXLEN,
	1    reply, gentyp*20
	complex*16 begpt,midpt,endpt,begpt2,midpt2,endpt2,
	1    c,w(4),w1,eta,wa(4),wb(4),wbi(4),
	1    bp(MAXSIDE),mp(MAXSIDE),ep(MAXSIDE),y(4,MAXSIDE)
	real*8 r,zero,bang,eang,raddeg,den,theta
	integer side(MAXSIDE),fside(MAXSIDE),nside
	integer err,tag(MAXLEV),lev,i,idir,outsty,p,q
	logical grpon,epson,outon,raston,curvon,ciron,arcon
	common /flags/ grpon,epson,outon,raston,curvon
	common /gflags/ ciron,arcon
	common /gens/ y, side, fside,  nside, gentyp
	common /arcs/ bp,mp,ep
	common /outfmt/ outsty
	parameter (raddeg=0.01745329251994329576923690768d0,
	1    zero=1.0d-10)
	outsty = 3
 10	write(*,*) 'SELECT STYLE OF ARC '
	write(*,'(8X,A15,3X,A)')
	1    'cenrad:','Specify center, radius, beg. angle, end. angle',
	2    '3pt:','Arc thru 3 points',
	3    'segmt:','Line segment between two points',
	3    'limpts:','Arc thru limit points',
	4    'parabolic:','Arc through parabolic fixed point',
	5    'schottky:','Schottky arc'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'cenrad') then
	   write(*,*) 'Enter cen., rad., beg. angle, end. angle, dir:'
	   write(*,*) '(complex*16, 3 real*8, integer: - = clockwise)'
	   read(*,*) c,r,bang,eang,idir
	   begpt= c+r*cdexp(dcmplx(0.0d0,raddeg*bang))
	   endpt= c+r*cdexp(dcmplx(0.0d0,raddeg*eang))
	   midpt = cdsqrt((endpt-c)*(begpt-c))
	   if(dimag(midpt*idir) .lt. 0.0d0) midpt=-midpt
	   midpt = c+r*midpt
	else if(choice.eq.'3pt') then
	   write(*,*) 'Enter 3 distinct points: (3 complex*16)'
	   read(*,*) begpt,midpt,endpt
	else if(choice.eq.'segmt') then
	   write(*,*) 'Enter 2 distinct points: (2 complex*16)'
	   read(*,*) begpt,endpt
	   midpt=0.5d0*(begpt+endpt)
	else if(choice.eq.'limpts') then
	   if(.not.grpon) call setgrp
	   write(*,'(A$)') 'Enter word for beginning point: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,begpt,w1,err)
	   write(*,'(A$)') 'Enter word for end point: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,endpt,w1,err)
	   write(*,'(A$)') 'Enter third point on arc (complex*16): '
	   read(*,*) midpt
	else if(choice.eq.'parabolic') then
	   if(.not.grpon) call setgrp
	   write(*,'(A$)') 'Enter parabolic word: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,begpt,w1,err)
	   eta = (w(1)+w(4))/w(3)
	   write(*,'(A$)') 'Enter angle from parabolic axis: '
	   read(*,*) theta
	   write(*,'(A$)') 'Enter word for end point: '
	   read(*,'(A)') wrdlbl
	   call lbl2wrd(w,tag,lev,wrdlbl,err)
	   call fixpt(w,endpt,w1,err)
	   eta=eta/cdabs(eta)
	   eta=eta*cdexp((0.0d0,1.0d0)*raddeg*theta)
	   call arc2pt(begpt,endpt,eta,c,midpt,r)
	else if(choice.eq.'schottky') then
	   if(.not.grpon) call setgrp
	   if(.not.arcon) call setcir
	   write(*,*) 'Enter number of Schottky arc: '
	   write(*,*) '(from 1 to ',nside,')'
	   read(*,*) i
	   begpt=bp(i)
	   midpt=mp(i)
	   endpt=ep(i)
	else
	   write(*,*) 'Bad style, select again'
	   go to 10
	end if
	if(grpon) then
	   write(*,'(A$)') 'Apply any words to the arc? (y or N)'
	   read(*,'(A)') reply
c       IF NO, GO TO PLOTTING ONE CIRCLE
	   if (reply.eq.'n') go to 90
 50	   write(*,*) 'SELECT OPTION FOR WORDS: '
	   write(*,'(8X,A15,3X,A)')
	1	'list:','List of words (ended by END)',
	2	'chain:','p/q chain of words',
	3	'horo:','p/q horocycle list'
	   write(*,'(/,5X,A$)') 'SELECT: '
	   read(*,'(A)') choice
	   if(choice.eq.'list') then
	      write(*,'(A$)') 'Enter words (END to end): '
 55	      read(*,'(A)') wrdlbl
	      if(wrdlbl.eq.'END') return
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      call linfrac(w,begpt,w1,err)
	      begpt2=w1
	      call linfrac(w,midpt,w1,err)
	      midpt2=w1
	      call linfrac(w,endpt,w1,err)
	      endpt2=w1
	      call smove(dreal(begpt2),dimag(begpt2))
	      call sarcto(dreal(midpt2),dimag(midpt2),
	1	   dreal(endpt2),dimag(endpt2))
	      go to 55
	   else if(choice.eq.'chain') then
	      write(*,'(A$)') 'Enter p, q: '
	      read(*,*) p,q
	      write(*,'(A$)') 'Enter a generator word: '
	      read(*,'(A)') alabel
	      call lbl2wrd(wa,tag,lev,alabel,err)
	      write(*,'(A$)') 'Enter b generator word: '
	      read(*,'(A)') blabel
	      call lbl2wrd(wb,tag,lev,blabel,err)
	      call matinv(wb,wbi)
 	      i = 0
	      call gfxdoc('Begin Arc Chain')
 80	      if(i.le.q) then
		 call linfrac(wa,begpt,begpt2,err)
		 call linfrac(wa,midpt,midpt2,err)
		 call linfrac(wa,endpt,endpt2,err)
		 i = i + p
	      else
		 call linfrac(wbi,begpt,begpt2,err)
		 call linfrac(wbi,midpt,midpt2,err)
		 call linfrac(wbi,endpt,endpt2,err)
		 i = i - q
	      end if
	      if(i.ge.p+q) then
		 call gfxdoc('End Arc Chain')
		 return
	      end if
	      begpt=begpt2
	      midpt=midpt2
	      endpt=endpt2
	      call smove(dreal(begpt),dimag(begpt))
	      call sarcto(dreal(midpt),dimag(midpt),
	1	   dreal(endpt),dimag(endpt))
	      go to 80
	   else if(choice.eq.'horo') then
	      continue
	   else
	      write(*,*) 'Bad choice; good bye!'
	   end if
	   return
	end if
 90	call smove(dreal(begpt),dimag(begpt))
	call sarcto(dreal(midpt),dimag(midpt),dreal(endpt),dimag(endpt))
	write(*,'(A$)') 'Place an arrowhead at end or arc? (y or N)'
	read(*,'(A)') reply
	if(reply.eq.'y') then
	   call cir3pt(begpt,midpt,endpt,c,r)
	   if(r.lt.0.0d0) then
	      w1=endpt-begpt
	   else
	      w1=(c-endpt)*(0.0d0,1.0d0)
	      if(dreal((c-midpt)/w1).lt.0.0d0) w1=-w1
	   end if
	   theta=datan2(dimag(w1),dreal(w1))/raddeg
	   call sarrow(dreal(endpt),dimag(endpt),theta)
	end if
	end
c
c       PLOT A FILE OF POINTS
	subroutine filplt
	integer outsty
	real*8 xmin,ymin,xmax,ymax
	complex*16 z0,z
	character  reply*1,ptsfil*100
	logical grpon,epson,outon,raston,curvon
	common /outfmt/ outsty
	common /flags/ grpon,epson,outon,raston,curvon
	common /picwin/ xmin,ymin,xmax,ymax
c       OUTSTY = 1 MEANS DRAW AS CURVE OR SET OF POINTS
	outsty = 1
c
c       ASK IF CURVE OR SET OF POINTS
	curvon=.false.
	write(*,'(A,$)') 'Is the file a curve? (Y or n) '
	read(*,'(A)') reply
	if(reply.eq.'y') curvon = .true.
C
C       GET FILE NAME
 11	write(*,*) 'Enter name of file containing points '
	read(*,'(A)') ptsfil
	open(3,file=ptsfil,status='old',err=12)
	go to 13
 12	write(*,*) 'File does not exist. Please reenter. '
	go to 11
 13	continue
c       INITIATE PLOT WITH PENWIDTH 1
	call ongrf(1)
c       SET CLIPPING WINDOW
	call swindw(xmin,ymin,xmax,ymax)
c       PLOT POINTS ENCLOSED IN POSTSCRIPT COMMENTS
	call gfxdoc('Begin Plot')
	read(3,*) z0
	call smove(dreal(z0),dimag(z0))
 20	read(3,*,END=25) z
	call sline(dreal(z),dimag(z))
	go to 20
 25	continue
	call gfxdoc('End Plot')
c       TERMINATE THIS PLOT (BUT NOT PICTURE)
	call offgrf
	close(3)
	end
