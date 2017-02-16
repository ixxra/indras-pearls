c 	TREE.F (7/99)
c       
c	This file contains the basic subprograms for tracing out the
c	tree of words in a kleinian group. It is 
c	intended to be linked with the driver program KLEINIAN.F
c       
c       
c----------------------------------------------------------
c       
c	BASIC CONVENTIONS:
c       All two-by-two matrices are represented as vectors as follows:
c       |  X(1)   X(2)  |
c       |  X(3)   X(4)  |
c       The index indicating the position of the entry (upper-right, 
c       lower-left, etc.) is always given first. The second index, if 
c       one appears, is a counter, such as LEV for the level inside the 
c       tree or another integer to indicate which generator the matrix is 
c       out of the list of given generators.
c       
c----------------------------------------------------------------------
c       
c	The structure of the program is as follows:
c       
c	MAIN:
c       
c       CALGEN       This subroutine inputs the parameters necessary
c       to the construction of the generators. These 
c       generators are denoted Y(i,j) in COMMON /GENS/.
c       Fixed points are also computed here.
c       
c       GETEPS       This routine sets up the thresholds for deciding
c       when to truncate the tree.
c       
c       INIT
c       Set up tree:  TAG = list of indices corresp. to gens.
c       X = list of words in the group
c       LEV = current level in tree
c       The first point is plotted here.
c       
c       ENUMER  (MIN ENUMERATION ROUTINE)
c       If the current spot in the tree passes TEST, call FORWRD 
c       (advance LEV by 1)
c       If not, plot the current limit point (PLOTX) and call BCKWRD
c       (go back to the first LEV where a turn is 
c       possible and then turn.)
c       
c----------------------------------------------------------------------
c       
c	The modular routines CALGEN, TEST, PLOTX, TERMIN
c	are found at the end of the program.
c       
c	THRESHOLD TESTING
c       
c       CALGEN: 
c       This subroutine calculates the generators.  It also
c       sets up the side-pairing information.
C       
c       SPWORD:
c       This routine collects fixed points of special words which are
c       nearly parabolic in the given group. We expect sizeable stretching
c       of the limit set near these fixed points. The points are sorted
c       in lexicographical order and stored in FP(i,tag) 
c       with 1<=i<=numfp(tag).
c       
c       TEST:  
c       In this routine, the "next" points are calculated by applying the
c       current prefix X(LEV) to all the points FP(i,tag(lev)). These 
c       points are called zplot(i).
c       In the quasifuchsian case zplot(1) is the last point plotted.
c       Our main threshold requires these to be within eps of each other 
c       consecutively.
c       
c----------------------------------------------------------------------
c       
c       LIMIT SET MENU
c       
c	Menu driven limit set or tiling drawing program
c       
c	This program computes the limit set of various kleinian groups
c       based on the "infinite word algorithm" associating limit points
c       to infinite words in the generators of the groups.
c       
c	The usual output is a list of limit points, listed in the lexicographical 
c	ordering associated with the limit set.
c
c       The same program can also be used to generate a tiling, interpreted
c       as one geometric figure per word in the group.
c       
c       FAIL STATE
#define FAIL 0
c      
	subroutine lsmenu
	character choice*15,col*100,ans*15,outfil*100
	integer outsty,lnblnk,inout,penwid,mlev
	real*8 xmin,ymin,xmax,ymax
	logical grpon,epson,outon,raston,curvon,ciron,arcon,fsaon,
	2    nocurv
	common /flags/ grpon,epson,outon,raston,curvon
	common /gflags/ ciron,arcon
	common /fsflag/ fsaon
	common /outfmt/ outsty
	common /picwin/ xmin,ymin,xmax,ymax
	common /levmax/ mlev
c       INITIATE OUTPUT FILE
	call onout(-1.1d0,-1.1d0,1.1d0,1.1d0,'lim')
 10	write(*,'(/,5X,A,/)') ' LIMIT SET MENU: '
	write(*,'(8X,A15,3X,A)') 
	1    'group:', 'Choose new group structure',
	1    'change:', 'Change group structure',
	1    'gpcalc:','Calculations with generators',
	1    'plot:','Choose new plotting thresholds',
	1    'flags:','Reset plotting flags',
	1    'maxlev:','Set maximum level of tree',
	1    'draw:','Draw limit set',
	1    'file:','Store limit set in file',
	1    'fill:','Start filled limit set',
	1    'fillin:','Fill inside of limit set',
	1    'fillout:','Fill outside of limit set',
	1    'labels:','Label piece of limit set',
	1    'label:','Label limit point',
	1    'flow:','Draw flow lines',
	1    'curve:','Draw certain curves (in funcs.f)',
	1    'schottky:','Draw Schottky circles',
	1    'schottkynum:','Draw numbered Schottky circles',
	1    'disks:','Draw Schottky disks colored by level',
	1    'arcs:','Draw Schottky arcs in invariant domain',
	1    'tile:','Draw orbit of an arc configuration',
	1    'circles:','Draw orbit of circle or disk configuration',
	1    'tilefill:','Draw filled tiles',
	1    'coloropts:','Options for coloring tiles, etc.',
	1    'poincare:','Compute Poincare series',
	1    'color:','Switch to a new color',
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
	else if(choice.eq.'gpcalc') then
c       GROUP CALCULATIONS
	   call gpcalc
	else if(choice.eq.'plot') then
c	CHOOSE NEW PLOTTING THRESHOLDS
	   epson=.false.
	   call geteps
	else if(choice.eq.'flags') then
c	RESET FLAGS
	   write(*,*) 'Which flag to toggle: '
           write(*,*) 'grpon,epson,outon,raston,curvon,ciron,
	1	arcon,fsaon,nocurv'
	   read(*,*) ans
	   if(ans.eq.'grpon') then
	      grpon=.not.grpon
	   else if(ans.eq.'epson') then
	      epson=.not.epson
	   else if(ans.eq.'outon') then
	      outon=.not.outon
	   else if(ans.eq.'raston') then
	      raston=.not.raston
	   else if(ans.eq.'curvon') then
	      curvon=.not.curvon
	   else if(ans.eq.'ciron') then
	      ciron=.not.ciron
	   else if(ans.eq.'arcon') then
	      arcon=.not.arcon
	   else if(ans.eq.'fsaon') then
	      fsaon=.not.fsaon
	   else if(ans.eq.'nocurv') then
	      nocurv=.not.nocurv
	   end if
	else if(choice.eq.'maxlev') then
c	SET MAX LEVEL REACHED IN TREE
	   write(*,'(A,I6,A$)')
	1	'Enter new maximum level: (Old: ',mlev,'):'
	   read(*,*) mlev
	else if(choice.eq.'draw') then
c	DRAW LIMIT SET
c       OUTSTY = 1 MEANS DRAW AS CURVE OR SET OF POINTS
	   outsty = 1
c       INITIATE PLOT WITH PENWIDTH 1
	   call ongrf(1)
c       SET CLIPPING WINDOW
	   call swindw(xmin,ymin,xmax,ymax)
c       PLOT POINTS ENCLOSED IN POSTSCRIPT COMMENTS
	   call gfxdoc('Begin limit set')
	   call enumer(0)
	   call gfxdoc('End limit set')
c       TERMINATE THIS PLOT (BUT NOT PICTURE)
	   call offgrf
	else if(choice.eq.'file') then
c	STORE LIMIT POINTS IN A FILE
c       OUTSTY = 0 MEANS STORE IN FILE
	   outsty = 0
	   write(*,*) 'Enter name of limit points file: '
	   read(*,'(A)') outfil
	   open(3,file = outfil)
	   call enumer(0)
	   close(3)
	else if(choice.eq.'fill') then
c	PLOT FILLED LIMIT SET
c       OUTSTY = 2 MEANS POSTSCRIPT RASTER IMAGE OF FILLED CURVE
	   outsty = 2
c       INITATE RASTER MATRICES IF NOT ALREADY
	   if(.not.raston) then 
	      call onbit(xmin,ymin,xmax,ymax)
	      raston=.true.
	   end if
c       PLOT POINTS
	   call enumer(0)
	else if(choice.eq.'fillin'.or.choice.eq.'fillout') then
c       FILL AND END RASTER IMAGE
	   if(raston) call offbit
c       OUTPUT TO FILE (INOUT=1 -> INSIDE)
	   inout=2
	   if(choice.eq.'fillin') inout=1
	   call gfxdoc('Begin Filled Limit Set')
	   call offfil(inout)
	   call gfxdoc('End Filled Limit Set')
	   raston=.false.
	else if(choice.eq.'labels') then
c	LABEL A RANGE OF LIMIT POINTS
c	CHANGE RESOLUTIONS
	   epson=.false.
c       OUTSTY = 3 MEANS PLOT LABEL OF POINT
	   outsty = 3
c       INITIATE PLOT WITH PENWIDTH 2
	   call ongrf(2)
c       SET CLIPPING WINDOW
	   call swindw(xmin,ymin,xmax,ymax)
	   call gfxdoc('Begin labels')
	   call enumer(0)
	   call gfxdoc('End labels')
	   call offgrf
	else if(choice.eq.'label') then
c	LABEL A SINGLE LIMIT POINT
	   call setgrp
	   outsty = 3
	   call limpt
	else if(choice.eq.'flow') then
c	DRAW FLOW LINES FOR AN ELEMENT
	   call setgrp
	   outsty = 1
	   curvon=.true.
	   call flowln
	else if(choice.eq.'curve') then
c	DRAW CERTAIN CURVES (in funcs.f)
	   outsty = 1
	   curvon=.true.
	   call curplt
	else if(choice.eq.'schottky'.or.
	1	choice.eq.'schottkynum'.or.
	1	choice.eq.'disks'.or.
	1	choice.eq.'arcs'.or.
	1	choice.eq.'tile'.or.
	1	choice.eq.'tilefill'.or.
	1	choice.eq.'circles') then
	   penwid = 1
	   if(choice.eq.'schottky') then
c       OUTSTY = 1 MEANS DRAW CIRCLES
	      outsty = 1
	   else if(choice.eq.'schottkynum') then
c       OUTSTY = 6 MEANS DRAW NUMBERED CIRCLES
	      outsty = 6
	      penwid = 0
	   else if(choice.eq.'disks') then
c       OUTSTY = 2 MEANS DRAW DISKS
	      outsty = 2
	   else if(choice.eq.'arcs') then
c       OUTSTY = 3 MEANS DRAW ARCS
	      outsty = 3
	   else if(choice.eq.'tile') then
c       OUTSTY = 4 MEANS DRAW TILES CONSISTING OF ARCS
	      write(*,*) 'Enter penwidth: '
	      read(*,*) penwid
	      outsty = 4
	   else if(choice.eq.'tilefill') then
c       OUTSTY = 5 MEANS DRAW FILLED TILES
c       ( ASSUME THE INPUT FILE IS A CONNECTED SEQUENCE OF ARCS )
	      write(*,*) 'Enter penwidth: '
	      read(*,*) penwid
	      outsty = 5
	   else if(choice.eq.'circles') then
c       OUTSTY = 7 MEANS DRAW ORBITS OF CIRCLES
	      outsty = 7
	   end if
	   write(*,*) outsty
c       INITIATE PLOT
	   call ongrf(penwid)
c       SET CLIPPING WINDOW
	   call swindw(xmin,ymin,xmax,ymax)
c       PLOT POINTS ENCLOSED IN POSTSCRIPT COMMENTS
	   call gfxdoc('Begin '//choice)
	   call enumer(1)
	   call gfxdoc('End '//choice)
c       TERMINATE THIS PLOT (BUT NOT PICTURE)
	   call offgrf
	else if(choice.eq.'coloropts') then
	   call clrmenu
	else if(choice.eq.'poincare') then
	   call pncmenu
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
c	SUBROUTINE FOR ENUMERATING WORDS IN THE GROUP.
c       LSFLAG = 0 -> COMPUTE OR PLOT ONLY LIMIT POINTS
c       LSFLAG = 1 -> COMPUTE OR PLOT TILES FOR EACH WORD
	subroutine enumer(lsflag)
        integer lsflag
	logical yesno,fail
c	GET GROUP IF NECESSARY
	call setgrp
c	GET PLOTTING THRESHOLDS
	call geteps
c       GET TILES
	if(lsflag.eq.1) call setcir
c	INITIATE TREE
	call init
c	MAIN BRANCHING: TEST TO SEE WHETHER OR NOT TO GO FORWARD
C       IF YESNO = .TRUE. KEEP GOING FORWARD;
c       OTHERWISE, GO BACKWARDS.
 40	call test(yesno)
	if(yesno) then
c	GO FORWARD ONE VERTEX
c          IF THERE ARE NO FORWARD DIRECTIONS, RETURN FAIL =.TRUE.
c          IF TILING, MAKE PLOT
	   if(lsflag.eq.1) call vertex
	   call forwrd(fail)
	   if(.not.fail) go to 40
	else
C	STOP GOING FORWARD:
C	   PLOT LIMIT POINT CORRESPONDING TO THIS "INFINITE" WORD
	   if(lsflag.eq.0) call plotx
c	CHECK TO SEE IF THE END HAS BEEN REACHED
C       IF YESNO = .TRUE. STOP PROGRAM AND CLOSE OUTPUT FILE
	   call endchk(yesno)
	   if(yesno) go to 900
	end if
c	GO BACK TO FIRST AVAILABLE TURN AND TURN ONCE
	call bckwrd
	go to 40
c	TERMINATE LIMIT SET
 900	call termin(lsflag)
	end
c       
c	CHOOSE NEW GROUP
	subroutine setgrp
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	if(grpon) return
c	GET PARAMETERS AND CALCULATE GENERATORS
	call calgen
c	SET UP SPECIAL WORDS (I.E. NEARLY PARABOLICS)
	call spword
	grpon=.true.
	end
c       
c	CALCULATE GENERATORS AND INITIAL POINT
	subroutine calgen
c	TRACES OF a, b, a*b, [a,b], RELATED NUMBERS
	complex*16 t1,t2,t3,t4,q,r,z,mu,mu1,mu2,mu3,mu4
c	GENERATORS AND FIXED POINTS OF [a,b]
	complex*16 y(4,MAXSIDE),z0,s1(4),s2(4),ii,fac,w(4),z1,z2,eta
	complex*16 bp(MAXSIDE),mp(MAXSIDE),ep(MAXSIDE),fp,cen(MAXSIDE),
	1    delfp,bpt(MAXARC),mpt(MAXARC),ept(MAXARC),
	2    figcen,a,b,c,prelim(4)
	real*8 bang(MAXSIDE),eang(MAXSIDE),fpang,raddeg,
	1    bpr(MAXSIDE),epr(MAXSIDE),fpr
	real*8 rad(MAXSIDE),twist,den,theta,zero,pi,
	1    figx0,figy0,figx1,figy1,figh,figang,rr
	integer i,i1,j,lnblnk,index,outsty,err
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	integer tag(MAXLEN),lev,iarc
	character gentyp*20, answer*20,choice, cirtyp*20, arcfil*30,
	1    genlet*MAXSIDE, alpha*52, reply*1,wrdlbl*MAXLEN
	logical grpon,epson,outon,raston,curvon,ciron,arcon,fsaon,
	2    nocurv
	common /flags/ grpon,epson,outon,raston,curvon
	common /gflags/ ciron,arcon
	common /fsflag/ fsaon
	common /outfmt/ outsty
	common /gens/ y, side, fside,  nside, gentyp
	common /gensym/ genlet
	common /cirs/ cen,rad
c       BEG. POINTS, MIDPOINTS, ENDPOINTS OF ARCS
	common /arcs/ bp,mp,ep
	common /arclst/ bpt,mpt,ept,prelim,iarc
	save alpha, s1, s2, ii
	data t2/2.0d0/,t4/-2.0d0/,ii/(0.0d0,1.0d0)/
	data s1/(1.0d0,0.0d0),(2.0d0,0.0d0),(0.0d0,0.0d0),(1.0d0,0.0d0)/
	data s2/(1.0d0,0.0d0),(0.0d0,0.0d0),(2.0d0,0.0d0),(1.0d0,0.0d0)/
c       ALPHABET (for generator pairing information)
	data alpha
	1    /'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	parameter (raddeg=0.01745329251994329576923690768d0,
	1    zero=1.0d-10,pi=3.14159265358979323844d0)
c       
c       SELECT GENERATOR STYLE
	gentyp='twogen'
	fsaon=.false.
	nocurv=.false.
        write(*,*) 'SELECT GENERATOR STYLE: '
 10	write(*,'(8X,A15,3X,A)')
	1    'manual:','Enter generators explicitly',
	1    'onegen:','One-generator group ',      
	1    'twogen:','Standard two-generator group (DEFAULT)',      
	1    'troels:','Jorgensen pairs of punctured tori',
	2    'fuchsdisk:','Fuchsian group in unit disk',
	3    'fuchsupper:','Fuchsian group in upper half-plane',
	3    'schottky:','Classical Schottky Group',
	2    'T11:','Maskit once-punctured torus ',
	3    'riley:','Riley 4-punctured sphere ',
	4    'T04:','Kra 4-punctured sphere ',
	5    'T12:','Kra 2-punctured torus ',
	6    'T05:','Kra 5-punctured sphere ',
	7    'T13:','Kra 3-punctured torus ',
	8    'T06:','Kra 6-punctured sphere ',
	9    'T21:','Kra once-punctured genus two ',
	1    'FSA:','Set Finite State Automaton first ',
	1    'curveoff:','Always plot as points '
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') answer
	if(lnblnk(answer).gt.0) gentyp=answer
c       BY DEFAULT THE PLOT IS A CURVE UNLESS CURVON = .FALSE.
	if(.not.nocurv) curvon=.true.
c       
c       ENTER GENERATORS EXPLICITLY
	if( gentyp.eq.'manual' .or. gentyp.eq.'fuchsdisk'
	1    .or. gentyp.eq.'fuchsupper'.or.gentyp.eq.'schottky') then
	   write(*,'(A,$)') 'Enter generator word: '
	   read(*,'(A)') genlet
	   curvon=.false.
	   write(*,'(A,$)') 'Is the limit set a curve? (y or N)'
	   read(*,'(A)') reply
	   if(reply.eq.'y'.and..not.nocurv) curvon = .true.
c       
c       ENTER GENERATORS EXPLICITLY
	else if( gentyp .eq. 'onegen') then
	   curvon=.false.
	   genlet='aA'
c       
c       STANDARD TWO-GENERATOR GROUP
	else if( gentyp .eq. 'twogen') then
	   write(*,*) 'Enter traces a, b, [a,b]: (complex*16) '
	   write(*,*) 'Default: tr b = ',t2,' and tr [a,b] = ',t4
	   read(*,*) t1,t2,t4
c	IF T4=-2, THEN THE LIMIT SET IS A CURVE.
	   if(t4 .ne. -2.0d0) then
	      write(*,*) 'Is it a curve? (Y or n)'
	      read(*,'(A)') choice
	      if(choice.eq.'n') curvon=.false.
	   end if
	   if(nocurv) curvon=.false.
	   z=0.5d0*cdsqrt( t1*t1*t2*t2 -4.0d0*(t1*t1+t2*t2-t4-2.0d0) )
	   t3 = 0.5d0*t1*t2-z
	   write(*,*) 'The choices for trace(ab) are '
	   write(*,*) '    a: ',t3,'    or b: ',t3+2.0d0*z
	   write(*,*) 'Enter a or b :'
	   read(*,'(A)') choice
	   if(choice .eq. 'b') t3=t3+2.0d0*z
c	LABELS
	   write(2,100) 'trace a       =',t1,
	1	'trace b       =',t2,
	2	'trace ab   =',t3,
	3	'trace [a,b]=',t4
 100	   format(A,f14.10,'+i',f14.10)
c	ATTRACTIVE FIXED POINT OF [a,b] BEFORE NORMALIZATION TO 1
	   q = (0.0d0,1.0d0)*cdsqrt(2.0d0-t4)
	   r = cdsqrt(2.0d0+t4)
	   if (cdabs(t4+q*r) .le. 2.0d0) r=-r
	   z0 = (t3-2.0d0)*(t2+r)/((t2+q)*t3-2.0d0*t1)
	   if( cdabs(z0) .le. zero) z0=1.0d0
c       
c       SETUP side-pairings and generator letters
c       Capital letter = inverse of lower case letter
c       sides listed in clockwise order around fundamental region
	   genlet ='abAB'
c 	GENERATOR FORMULAS
c       Give formulas for each lower case letter in genlet
	   y(1,1) = t1/2.0d0
	   y(2,1) = (t1*t3-2.0d0*(t2-q))/((2.0d0*t3+4.0d0)*z0)
	   y(3,1) = z0*(t1*t3-2.0d0*(t2+q))/(2.0d0*t3-4.0d0)
	   y(4,1) = t1/2.0d0
	   y(1,2) = (y(4,1)-z0*y(2,1))*t3/2.0d0 -z0*y(2,1) 
	   y(2,2) = (y(4,1)/z0-y(2,1))*t3/2.0d0 -y(4,1)/z0
	   y(3,2) = (-y(3,1)+z0*y(1,1))*t3/2.0d0 +z0*y(1,1)
	   y(4,2) = (-y(3,1)/z0+y(1,1))*t3/2.0d0 +y(3,1)/z0
c       
c       PAIRS OF PUNTURED TORI
	else if( gentyp .eq. 'troels') then
	   write(*,*) 'Enter traces a, b: (complex*16) '
	   write(*,*) 'Default: tr b = ',t2
	   read(*,*) t1,t2
	   z=0.5d0*cdsqrt( t1*t1*t2*t2 -4.0d0*(t1*t1+t2*t2) )
	   t3 = 0.5d0*t1*t2-z
	   write(*,*) 'The choices for trace(ab) are '
	   write(*,*) '    a: ',t3,'    or b: ',t3+2.0d0*z
	   write(*,*) 'Enter a or b :'
	   read(*,'(A)') choice
	   if(choice .eq. 'b') t3=t3+2.0d0*z
c	LABELS
	   write(2,100) 'trace a       =',t1,
	1	'trace b       =',t2,
	2	'trace ab    =',t3
c       
c       SETUP side-pairings and generator letters
	   genlet ='abAB'
c 	GENERATOR FORMULAS
	   y(1,1) = t1 - t2/t3
	   y(2,1) = t1/(t3*t3)
	   y(3,1) = t1
	   y(4,1) = t2/t3
	   y(1,2) = t2 - t1/t3
	   y(2,2) = -t2/(t3*t3)
	   y(3,2) = -t2
	   y(4,2) = t1/t3
c       
c       MASKIT ONCE-PUNCTURED TORUS
        else if (gentyp .eq. 'T11') then
	   write(*,*) 'Enter mu: (complex*16) '
	   read(*,*) mu
c	LABELS
	   write(2,100) 'MU       =',mu
c       SETUP side-pairings and generator letters
	   genlet='abAB'
c 	GENERATOR FORMULAS
	   y(1,1) = -ii*mu
	   y(2,1) = -ii
	   y(3,1) = -ii
	   y(4,1) = 0.0d0
	   call matcpy(s1,y(1,2))
c       
c       
c       RILEY 4-PUNCTURED SPHERE
        else if (gentyp .eq. 'riley') then
	   write(*,*) 'Enter mu: (lower left corner; complex*16) '
	   read(*,*) mu
c       ALWAYS A SET OF POINTS
	   curvon=.false.
c	LABELS
	   write(2,100) 'MU       =',mu
c       SETUP side-pairings and generator letters
	   genlet='aAbB'
c 	GENERATOR FORMULAS
	   y(1,1) = 1.0d0
	   y(2,1) = 0.0d0
	   y(3,1) = mu
	   y(4,1) = 1.0d0
	   call matcpy(s1,y(1,3))
c       
c       
c       KRA 4-PUNCTURED SPHERE
        else if (gentyp .eq. 'T04') then
	   write(*,*) 'Enter mu: (complex*16) '
	   read(*,*) mu
c	LABELS
	   write(2,100) 'KRA Mu       =',mu
c       SETUP side-pairings and generator letters
	   genlet='abcCBA'
c 	GENERATOR FORMULAS
	   call matcpy(s1,y(1,1))
	   call matcpy(s2,y(1,2))
	   y(1,3) = 1.0d0 + 2.0d0*mu
	   y(2,3) = 2.0d0
	   y(3,3) = -2.0d0*mu*mu
	   y(4,3) = 1.0d0 - 2.0d0*mu
c       
c       
c       KRA 2-PUNCTURED TORUS
        else if (gentyp .eq. 'T12') then
	   write(*,*) 'Enter mu1, mu2: (complex*16) '
	   read(*,*) mu1, mu2
c	LABELS
	   write(2,100) 'KRA Mu1=    ',mu1,
	1	'KRA Mu2=    ',mu2
c       SETUP side-pairings and generator letters
c$$$c 	GENERATOR FORMULAS
c$$$	   genlet='abcACB'
c$$$	   y(1,1) = 1.0d0+mu1*mu2
c$$$	   y(2,1) = mu1/2.0d0
c$$$	   y(3,1) = mu2*2.0d0
c$$$	   y(4,1) = 1.0d0
c$$$	   call matcpy(s1,y(1,2))
c$$$	   call matcpy(s2,y(1,3))
c 	GENERATOR FORMULAS
	   genlet='abcBAC'
	   call matcpy(s1,y(1,1))
	   call matcpy(s2,y(1,2))
	   y(1,3) = 1.0d0
	   y(2,3) = -mu1
	   y(3,3) = -mu2
	   y(4,3) = 1.0d0+mu1*mu2
c       
c       
c       KRA 5-PUNCTURED SPHERE
        else if (gentyp .eq. 'T05') then
	   write(*,*) 'Enter mu1, mu2: (complex*16) '
	   read(*,*) mu1, mu2
c	LABELS
	   write(2,100) 'KRA Mu1=    ',mu1,
	1	'KRA Mu2=    ',mu2
c       SETUP side-pairings and generator letters
	   genlet='abcdDCBA'
c 	GENERATOR FORMULAS
	   y(1,1) = 1.0d0+2.0d0*mu1
	   y(2,1) = -2.0d0*mu1*mu1
	   y(3,1) = 2.0d0
	   y(4,1) = 1.0d0-2.0d0*mu1
	   call matcpy(s1,y(1,2))
	   call matcpy(s2,y(1,3))
	   y(1,4) = 1.0d0+2.0d0*mu2
	   y(2,4) = 2.0d0
	   y(3,4) = -2.0d0*mu2*mu2
	   y(4,4) = 1.0d0-2.0d0*mu2
c       
c       KRA 3-PUNCTURED TORUS
        else if (gentyp .eq. 'T13') then
	   write(*,*) 'Enter mu1, mu2, mu3: (complex*16) '
	   read(*,*) mu1, mu2, mu3
c	LABELS
	   write(2,100) 'KRA Mu1=    ',mu1,
	1	'KRA Mu2=    ',mu2,
	2	'KRA Mu3=    ',mu3
c       SETUP side-pairings and generator letters
	   genlet='abcdADCB'
c 	GENERATOR FORMULAS
	   y(1,1) = ii*(mu1*mu2*mu3+mu1+mu2)
	   y(2,1) = ii*(mu1*mu3+1)
	   y(3,1) = ii*(mu2*mu3+1)
	   y(4,1) = ii*mu3
	   y(1,2) = 1.0d0 +2.0d0*mu1
	   y(2,2) = -2.0d0*mu1*mu1
	   y(3,2) = 2.0d0
	   y(4,2) = 1.0d0 -2.0d0*mu1
	   call matcpy(s1,y(1,3))
	   call matcpy(s2,y(1,4))
c       
c       
c       KRA 6-PUNCTURED SPHERE
        else if (gentyp .eq. 'T06') then
	   write(*,*) 'Enter mu1, mu2, mu3: (complex*16) '
	   read(*,*) mu1, mu2, mu3
c	LABELS
	   write(2,100) 'KRA Mu1=    ',mu1,
	1	'KRA Mu2=    ',mu2,
	2	'KRA Mu3=    ',mu3
c       SETUP side-pairings and generator letters
	   genlet='abcdeEDCBA'
c 	GENERATOR FORMULAS
	   y(1,1) = -2.0d0*mu2*mu1*mu1-2.0d0*mu1+1.0d0
	   y(2,1) =  2.0d0*mu2*mu2*mu1*mu1 +4.0d0*mu2*mu1 +2.0d0
	   y(3,1) = -2.0d0*mu1*mu1
	   y(4,1) =  2.0d0*mu2*mu1*mu1 + 2.0d0*mu1 +1.0d0
	   y(1,2) = 1.0d0+2.0d0*mu2
	   y(2,2) = -2.0d0*mu2*mu2
	   y(3,2) = 2.0d0
	   y(4,2) = 1.0d0-2.0d0*mu2
	   call matcpy(s1,y(1,3))
	   call matcpy(s2,y(1,4))
	   y(1,5) = 1.0d0+2.0d0*mu3
	   y(2,5) = 2.0d0
	   y(3,5) = -2.0d0*mu3*mu3
	   y(4,5) = 1.0d0-2.0d0*mu3
c       
c       
c       KRA ONCE-PUNCTURED GENUS TWO
        else if (gentyp .eq. 'T21') then
	   write(*,*) 'Enter mu1, mu2, mu3, mu4: (complex*16) '
	   read(*,*) mu1,mu2,mu3,mu4
c	LABELS
	   write(2,100) 'KRA Mu1=    ',mu1,
	1	'KRA Mu2=    ',mu2,
	2	'KRA Mu3=    ',mu3,
	3	'KRA Mu4=    ',mu4
c       SETUP side-pairings and generator letters
	   genlet='abcdCADB'
c 	GENERATOR FORMULAS
c       y(1) = T(mu1) * T(mu3) * T(mu2)
	   y(1,1) = ii*(mu1*mu2*mu3+mu1+mu2)
	   y(2,1) = ii*(mu1*mu3+1.0d0)
	   y(3,1) = ii*(mu2*mu3+1.0d0)
	   y(4,1) = ii*mu3
	   y(1,2) = 1.0d0+2*mu1
	   y(2,2) = -2.0d0*mu1*mu1
	   y(3,2) = 2.0d0
	   y(4,2) = 1.0d0-2*mu1
	   y(1,3) = -mu1*mu4- mu4 -1.0d0
	   y(2,3) = mu1*mu4+ mu4 -mu1
	   y(3,3) = -mu4
	   y(4,3) = mu4-1.0d0
	   call matcpy(s1,y(1,4))
c       
c       SET FINITE STATE AUTOMATON AND THEN PICK GENERATORS
        else if (gentyp .eq. 'FSA') then
	   fsaon = .true.
	   go to 10
c       
c       DO NOT DRAW LIMIT SET AS A CURVE
        else if (gentyp .eq. 'curveoff') then
	   nocurv = .true.
	   curvon = .false.
	   go to 10
	else
	   write(*,*) 'Bad type: select again'
	   go to 10
        end if
c       
c       SETUP fside based on side
c       Complete all data based on genlet
	entry mkgens
	nside=lnblnk(genlet)
	do 200 i=1,nside
c       GET POSITION OF GEN IN ALPHABET
	   i1 = index(alpha,genlet(i:i))
c       LOWER CASE GEN
	   if(i1 .le. 26) then
	      side(i) = index(genlet,alpha(i1+26:i1+26))
	      j=side(i)
	      if(gentyp.eq.'manual'.or.gentyp.eq.'onegen') then
		 write(*,'(A,I2,A)')
	1	      'Enter generator ',i,' (4 complex*16):'
		 read(*,*) y(1,i),y(2,i),y(3,i),y(4,i)
		 fac=cdsqrt( y(1,i)*y(4,i)-y(2,i)*y(3,i))
		 y(1,i)=y(1,i)/fac
		 y(2,i)=y(2,i)/fac
		 y(3,i)=y(3,i)/fac
		 y(4,i)=y(4,i)/fac
c       
c       FOR FOLLOWING CASES, GENERATE STANDARD CIRCLES AND ARCS
c       
c       FUCHSIAN SCHOTTKY GROUP ON UNIT DISK
	      else if(gentyp.eq.'fuchsdisk') then
		 write(*,'(A,I2,A,I2,A/,A,A)')
	1	      'Enter angles for sides i=',i,' j=',j,
	2	      ' (4 real*8):',
	2	      'Beg. ang. side i, End ang. side i, ',
	3	      'Beg. angle side j, End ang. side j: '
		 read(*,*) bang(i),eang(i),bang(j),eang(j)
		 bp(i)=dcmplx(dcos(bang(i)*raddeg),dsin(bang(i)*raddeg))
		 ep(i)=dcmplx(dcos(eang(i)*raddeg),dsin(eang(i)*raddeg))
		 call orthog(bp(i),ep(i),(0.0d0,0.0d0),cen(i),
	1	      1.0d0,rad(i))
		 if(rad(i).le.0.0d0) then
		    mp(i)=0.5d0*(bp(i)+ep(i))
		 else
		    mp(i) = bp(i)+ep(i)-2.0d0*cen(i)
		    mp(i) = cen(i) + rad(i)*mp(i)/cdabs(mp(i))
		 end if
		 bp(j)=dcmplx(dcos(bang(j)*raddeg),dsin(bang(j)*raddeg))
		 ep(j)=dcmplx(dcos(eang(j)*raddeg),dsin(eang(j)*raddeg))
		 call orthog(bp(j),ep(j),(0.0d0,0.0d0),cen(j),
	1	      1.0d0,rad(j))
		 if(rad(j).le.0.0d0) then
		    mp(j)=0.5d0*(bp(j)+ep(j))
		 else
		    mp(j) = bp(j)+ep(j)-2.0d0*cen(j)
		    mp(j) = cen(j) + rad(j)*mp(j)/cdabs(mp(j))
		 end if
		 write(*,'(A$)')
	1	      'Enter angle of attractive fixed point: (real*8) '
		 read(*,*) fpang
		 fp = dcmplx(dcos(fpang*raddeg),dsin(fpang*raddeg))
c       GENERATOR i MAPS bp(j) -> ep(i)
c       fp   ->  fp
c       ep(j) -> bp(i)
		 call mat6pt(y(1,i),bp(j),fp,ep(j),ep(i),fp,bp(i))
c       LABELS
		 write(2,110) i,bang(i),eang(i),j,bang(j),eang(j),fpang
 110		 format('Side ',I1,' from ',f5.2,' to ',f5.2,
	1	      /,'Side ',I1,' from ',f5.2,' to ',f5.2,
	2	      '; Attr. f.p. at ',f5.2)
c       
c       FUCHSIAN SCHOTTKY GROUP GIVEN IN UPPER HALF-PLANE
	      else if(gentyp.eq.'fuchsupper') then
		 write(*,'(A,I2,A,I2,A/,A,A)')
	1	      'Enter endpoints for sides i=',i,' j=',j,
	2	      ' (4 real*8):',
	2	      'Beg. side i, End side i, ',
	3	      'Beg. side j, End side j: '
		 read(*,*) bpr(i),epr(i),bpr(j),epr(j)
		 bp(i)=dcmplx(bpr(i),0.0d0)
		 ep(i)=dcmplx(epr(i),0.0d0)
		 cen(i) = 0.5d0*(bp(i)+ep(i))
		 rad(i) = 0.5d0*cdabs(ep(i)-bp(i))
		 mp(i)=cen(i)+ii*rad(i)
		 bp(j)=dcmplx(bpr(j),0.0d0)
		 ep(j)=dcmplx(epr(j),0.0d0)
		 cen(j) = 0.5d0*(bp(j)+ep(j))
		 rad(j) = 0.5d0*cdabs(ep(j)-bp(j))
		 mp(j)=cen(j)+ii*rad(j)
		 write(*,'(A$)')
	1	      'Enter attractive fixed point: (real*8) '
		 read(*,*) fpr
		 fp = dcmplx(fpr,0.0d0)
c       GENERATOR i MAPS bp(j) -> ep(i)
c       fp   ->  fp
c       ep(j) -> bp(i)
		 call mat6pt(y(1,i),bp(j),fp,ep(j),ep(i),fp,bp(i))
c       LABELS
		 write(2,120) i,bpr(i),epr(i),j,bpr(j),epr(j),fpr
 120		 format('Side ',I1,' from ',f7.4,' to ',f7.4,
	1	      /,'Side ',I1,' from ',f7.4,' to ',f7.4,
	2	      '; Attr. f.p. at ',f7.4)
c       
c       CLASSICAL SCHOTTKY GROUP SPECIFIED BY CIRCLES
	      else if(gentyp.eq.'schottky') then
		 write(*,'(A,I2,A,I2,A/,A,A)')
	1	      'Enter circles for sides i=',i,' j=',j,
	2	      ' (2(complex*16, real*8)):',
	2	      'Center side i, Radius side i, ',
	3	      'Center side j, Radius side j: '
		 read(*,*) cen(i),rad(i),cen(j),rad(j)
		 write(*,'(A,/,A$)')
	1	      'Enter displacement of fixed point ',
	2	      '(complex*16, <1 in abs. value): '
		 read(*,*) delfp
		 write(*,'(A$)') 'Enter twisting: (real*8) '
		 read(*,*) twist
		 call schott(y(1,i),cen(i),cen(j),rad(i),rad(j),
	1	      delfp,twist,err)
c       LABELS
		 write(2,130) i,dreal(cen(i)),dimag(cen(i)),rad(i),
	1	      j,dreal(cen(j)),dimag(cen(j)),rad(j),
	2	      dreal(delfp),dimag(delfp),twist
 130		 format('Side ',I1,' cen. (',f7.4,',',f7.4,
	1	      ') rad. ',f7.4,
	1	      /,'Side ',I1,' cen. (',f7.4,',',f7.4,
	2	      ') rad. ',f7.4,
	2	      /,'Displ. (',f7.4,',',f7.4,') twist ',f5.2)
	      end if
	      call matinv(y(1,i),y(1,j))
c       UPPER CASE GEN
	   else
	      side(i) = index(genlet,alpha(i1-26:i1-26))
	   end if
	   fside(i)=side(i)-1
	   if(fside(i).eq.0) fside(i) = nside
 200	continue
	call getfsa
	return
c       
c       SET CIRCLES OR ARCS IN SCHOTTKY CASE
	entry setcir
c       ASK FOR CIRCLES OR DISKS
	if(outsty.eq.1.or.outsty.eq.2.or.outsty.eq.6) then
	   if(ciron) return
c       SELECT CIRCLES
	   write(*,*) 'SELECT CIRCLES IN ONE OF FOLLOWING STYLES: '
 20	   write(*,'(8X,A15,3X,A)')
	1	'manual:','Enter centers and radii explicitly',
	2	'standard:','Use circles given to define generators',
	3	'isometric:','Isometric circles of generators',
	4	'parabolic:','Circles thru parabolic fixed points'
	   write(*,'(/,5X,A$)') 'SELECT: '
	   read(*,'(A)') cirtyp
	   if(cirtyp.eq.'manual') then
	      write(*,'(A$)') 'Apply a Mobius transformation? (y or n) '
	      read(*,'(A)') reply
	      if(reply.eq.'y') then
		 write(*,'(A)') 'Enter matrix (4 complex*16) '
		 read(*,*) (w(i),i=1,4)
	      else
		 w(1)=1.0d0
		 w(2)=0.0d0
		 w(3)=0.0d0
		 w(4)=1.0d0
	      end if
	      do 25 i=1,nside
		 if(index(alpha,genlet(i:i)).gt.26) go to 25
		 write(*,*) 'Enter center, radius for generator '
	1	      ,genlet(i:i), '(complex*16, real*8)'
		 read(*,*) c,rr
		 call lfapply(w,c,cen(i),rr,rad(i))
		 j=side(i)
		 call lfapply(y(1,j),cen(i),cen(j),rad(i),rad(j))
 25	      continue
	   else if(cirtyp.eq.'standard') then
	      if(.not.(gentyp.eq.'fuchsdisk'.or.gentyp.eq.'fuchsupper'
	1	   .or.gentyp.eq.'schottky')) then
		 write(*,*) 'No standard circles defined'
		 go to 20
	      end if
	   else if(cirtyp.eq.'isometric') then
	      do 35 i=1,nside
		 if(index(alpha,genlet(i:i)).gt.26) go to 35
		 j=side(i)
		 if(cdabs(y(3,i)).le.zero) then
		    write(*,*) 'c=0 means no isometric circle.'
		    write(*,*) 'Enter center, radius for generator '
	1		 ,genlet(i:i), '(complex*16, real*8)'
		    read(*,*) cen(i),rad(i)
		 else
		    cen(i) = y(1,i)/y(3,i)
		    rad(i) = 1.0d0/cdabs(y(3,i))
		 end if
		 call lfapply(y(1,j),cen(i),cen(j),rad(i),rad(j))
 35	      continue
	   else if(cirtyp.eq.'parabolic') then
	      do 65 i=1,nside
		 if(index(alpha,genlet(i:i)).gt.26) go to 65
 62		 write(*,*) 'Enter first parabolic word for side ',
	1	      i,': '
		 read(*,'(A)') wrdlbl
		 call lbl2wrd(w,tag,lev,wrdlbl,err)
		 call fixpt(w,z1,z2,err)
		 if(err.ne.1) then
		    write(*,*) 'Not finite parabolic; reenter '
		    go to 62
		 end if
		 bp(i)=z1
		 write(*,'(A$)') 'Enter angle from parabolic axis: '
		 read(*,*) theta
		 eta = (1.0d0/w(3))
		 eta = eta*cdexp(dcmplx(0.0d0,raddeg*(theta-90)))
 		 write(*,'(A$)') 'Enter second word: '
		 read(*,'(A)') wrdlbl
		 call lbl2wrd(w,tag,lev,wrdlbl,err)
		 call fixpt(w,z1,z2,err)
		 ep(i) = z1
		 den= 2.0d0*dreal( (ep(i)-bp(i))*dconjg(eta) )
		 if( dabs(den) .le. zero) then
		    cen(i)=bp(i)
		    z1=ep(i)-bp(i)
		    rad(i)=-datan2(dimag(z1),dreal(z1))
		    if(rad(i) .ge. 0.0) rad(i)=rad(i)-2.0d0*pi
		 else
		    cen(i)=eta*cdabs((bp(i)-ep(i))*(bp(i)-ep(i)))/den
		    cen(i)=bp(i)+cen(i)
		    rad(i)=cdabs(bp(i)-cen(i))
		 end if
		 j=side(i)
		 call lfapply(y(1,j),cen(i),cen(j),rad(i),rad(j))
 65	      continue
	   else
	      write(*,*) 'Bad type: select again'
	      go to 20
	   end if
	   ciron = .true.
c	ASK FOR ARCS
	else if(outsty.eq.3) then
	   if(arcon) return
c       
c       SELECT ARCS
	   write(*,*) 'SELECT ARCS IN ONE OF FOLLOWING STYLES: '
 30	   write(*,'(8X,A15,3X,A)')
	1	'manual:','Enter centers and radii explicitly',
	2	'standard:','Use arcs given to define generators',
	3	'parabolic:','Pick arcs thru parabolic fixed points'
	   write(*,'(/,5X,A$)') 'SELECT: '
	   read(*,'(A)') cirtyp
	   if(cirtyp.eq.'manual') then
	      do 45 i=1,nside
		 if(index(alpha,genlet(i:i)).gt.26) go to 45
		 write(*,*) 'Enter starting point, midpoint, endpoint',
	1	      'for generator ',genlet(i:i),' (3 complex*16)'
		 read(*,*) bp(i),mp(i),ep(i)
		 j=side(i)
		 call linfrac(y(1,j),bp(i),ep(j),err)
		 call linfrac(y(1,j),mp(i),mp(j),err)
		 call linfrac(y(1,j),ep(i),bp(j),err)
 45	      continue
	   else if(cirtyp.eq.'standard') then
	      if(.not.(gentyp.eq.'fuchsdisk'
	1	   .or.gentyp.eq.'fuchsupper') )   then
		 write(*,*) 'No standard arcs defined'
		 go to 20
	      end if
	   else if(cirtyp.eq.'parabolic') then
	      do 55 i=1,nside
		 if(index(alpha,genlet(i:i)).gt.26) go to 55
 52		 write(*,*) 'Enter first parabolic word for side ',
	1	      i,': '
		 read(*,'(A)') wrdlbl
		 call lbl2wrd(w,tag,lev,wrdlbl,err)
		 call fixpt(w,z1,z2,err)
		 if(err.ne.1) then
		    write(*,*) 'Not finite parabolic; reenter '
		    go to 52
		 end if
		 bp(i)=z1
		 write(*,'(A$)') 'Enter angle from parabolic axis: '
		 read(*,*) theta
c       IT SHOULD BE LIKE THIS BELOW
		 eta = 0.5d0*cdexp(dcmplx(0.0d0,raddeg*(theta-90)))
	1	      *(w(1)+w(4))/w(3)
c$$$		 eta = cdexp(dcmplx(0.0d0,raddeg*(theta-90)))/w(3)
 		 write(*,'(A$)') 'Enter second word: '
		 read(*,'(A)') wrdlbl
		 call lbl2wrd(w,tag,lev,wrdlbl,err)
		 call fixpt(w,z1,z2,err)
		 ep(i) = z1
		 den= 2.0d0*dreal( (ep(i)-bp(i))*dconjg(eta) )
		 if( dabs(den) .le. zero) then
		    mp(i) = 0.5d0*(bp(i)+ep(i))
		 else
		    cen(i)=eta*cdabs((bp(i)-ep(i))*(bp(i)-ep(i)))/den
		    cen(i)=bp(i)+cen(i)
		    rad(i)=cdabs(bp(i)-cen(i))
		    mp(i) = cdsqrt((ep(i)-cen(i))/(bp(i)-cen(i)))
		    if(dimag(mp(i)/eta) .lt. 0.0d0) mp(i)=-mp(i)
		    mp(i) = cen(i)+mp(i)*(bp(i)-cen(i))
		 end if
		 j=side(i)
		 call linfrac(y(1,j),bp(i),ep(j),err)
		 call linfrac(y(1,j),mp(i),mp(j),err)
		 call linfrac(y(1,j),ep(i),bp(j),err)
 55	      continue
	   else
	      write(*,*) 'Bad type: select again'
	      go to 30
	   end if
	   arcon = .true.
c	ASK FOR FIGURE
	else if(outsty.eq.4.or.outsty.eq.5) then
 70	   write(*,*) 'Enter name of file containing figure '
	   read(*,'(A)') arcfil
	   open(3,file=arcfil,status='old',err=75)
	   go to 76
 75	   write(*,*) 'File does not exist. Please reenter. '
	   go to 70
 76	   iarc=0
	   read(3,*) figx0,figy0,figx1,figy1
 80	   read(3,*,end=90) bpt(iarc+1),mpt(iarc+1),ept(iarc+1)
	   iarc=iarc+1
	   go to 80
 90	   close(3)
 	   write(*,*) 'Enter center of figure, height, rotation angle: '
	   write(*,*) '(complex*16, real*8, real*8 degrees)'
	   read(*,*) figcen, figh, figang
c       PROCESS ARC POINTS BY APPROPRIATE LINEAR TRANSFORMATION
c       z -> az+b
	   a=cdexp(dcmplx(0.0d0,figang*raddeg))
	   a= a*figh/(figy1-figy0)
	   b= a*0.5d0*dcmplx(figx0+figx1,figy0+figy1)
	   b=figcen-b
	   do 95 i=1,iarc
	      bpt(i) = a*bpt(i)+b
	      mpt(i) = a*mpt(i)+b
	      ept(i) = a*ept(i)+b
 95	   continue
 	   write(*,*) 'Apply a transform during enumeration? (y or N): '
	   read(*,'(A)') reply
	   if(reply.eq.'y') then
	      write(*,'(A)') 'Enter matrix (4 complex*16) '
	      read(*,*) (w(i),i=1,4)
	      call matfix(w,prelim)
	   else
	      prelim(1)=1.0d0
	      prelim(2)=0.0d0
	      prelim(3)=0.0d0
	      prelim(4)=1.0d0
	   end if
	end if
	end
c       
c	ENTER SPECIAL WORDS
c       These are words which are either parabolic or nearly parabolic.
c       Since the most stretching occurs near these fixed points,
c       this routine ensures that they are plotted exactly.
c
	subroutine spword
	integer i,lev,err,numfp(MAXSTATE),tag(MAXLEN),lnblnk
	integer side(MAXSIDE), fside(MAXSIDE),  nside,
	1    mask(MAXSIDE)
	complex*16 fp(MAXFP,MAXSTATE),y(4,MAXSIDE),w(4)
	character fplbl(MAXFP,MAXSTATE)*MAXLEN, wlabel*MAXLEN, gentyp*20
	logical fsaon
	common /fixedp/ fp,fplbl,numfp
	common /gens/ y, side, fside,  nside, gentyp
	common /fsflag/ fsaon
c	PROCESS KNOWN SPECIAL WORDS:
c       Since we are assuming that the vertices of our fundamental region
c       are punctures, the elements that fix these vertices are parabolic.
c       We enumerate these elements by looking for the cycles in the
c       permutation i -> fside(i)
c       
c       ADD TO LABELS FILE
	write(2,'(A,$)') 'Special words: '
c       numfp(i) = number of special points starting with tag i.
c       mask(i) = 0 if we haven't come across this side yet.
c     
c       IF AN FSA IS ON, do this with states instead of sides
        if(fsaon) then
	   call spwrd2
	   go to 10
	end if
c
c       Initialize these numbers with 0
	do 3 i=1,nside
	   mask(i)=0
	   numfp(i)=0
 3	continue
	do 8 i =1, nside
	   if( mask(i) .eq. 1) go to 8
	   tag(1)=i
	   mask(i) = 1
	   do 5 lev=1,nside
	      if(fside( tag(lev) ) .eq. tag(1)) go to 6
	      tag(lev+1) = fside(tag(lev))
	      mask(tag(lev+1)) = 1
 5	   continue
 6	   continue
	   call tag2wrd(w,tag,lev,wlabel,err)
	   if(err .gt. 0) go to 8
	   write(*,*) 'Vertex word: ', wlabel(1:lnblnk(wlabel))
	   call stowrd(wlabel)
 8	continue
c
c       PROCESS SPECIAL WORDS SUPPLIED BY USER
	if(nside.le.2) then
	   write(2,'()')
	   return
	end if
 10	write(*,*) 'Enter special word, '
c$$$	write(*,*) 'Or "manual" for  specified limit point, '
	write(*,*) 'Or <return> to end list:'
	call getwrd(w,tag,lev,wlabel,err)
c       NULL WORD MEANS STOP GETTING SPECIAL WORDS
	if(lev .eq. 0) then
c       END LABEL OF SPECIAL WORDS
	   write(2,'()')
	   return
c       NOT A REDUCED WORD
	else if(err .eq. 1) then
	   write(*,*) 'Not a valid response; re-enter:'
	   go to 10
	else
c       FINITE SPECIAL WORD; STORE IN NORMAL WAY
	   call stowrd(wlabel)
	   go to 10
	end if
	end
c       
c	PROCESS AND STORE SPECIAL WORD
c       FIXED POINT DATA:
c       numfp(i) = number of special points for side i (up to 100 per)
c       fplbl(j,i) = label of j-th special word at side i
c       These are words that are to be repeated forever
c       after a finite word ending with the tag i.
c       Each word starts with i.
c       fplbl(1,i) should give the very first infinite word.
c       fplbl(numfp(i),i) the last, in lex ordering.
c       fp(j,i) = attractive fixed point of j-th special word at side i
	subroutine stowrd(wlabel)
	integer i,j,k,l,lev,err,istart,lnblnk,tag(MAXLEN),
	1    numfp(MAXSTATE),wordcmp,wlt,endtag(MAXSTATE)
	integer nstate,fsa(MAXSTATE,MAXSIDE),state(MAXLEV),st
	complex*16 fp(MAXFP,MAXSTATE),w(4),z1,z2
	character fplbl(MAXFP,MAXSTATE)*MAXLEN, wlabel*(*),
	1    wrdlbl*MAXLEN, alpha*52
	1    /'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	logical fsaon
	common /fixedp/ fp,fplbl,numfp
	common /fsflag/ fsaon
	common /fsabk/ state,fsa,nstate
	common /fsaend/ endtag
	save alpha
	wrdlbl = wlabel
	lev = lnblnk(wrdlbl)
c       DO BOTH GIVEN WORD AND ITS INVERSE
	do 75 k=1,2
c       DO ALL CYCLIC PERMUTATIONS
	   do 70 istart=1,lev
c       COMPUTE ASSOCIATED MATRIX AND TAGS
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
c	COMPUTE  FIXED POINTS (z1= attractive; z2=repelling)
	      call fixpt(w,z1,z2,err)
c       FIRST TAG OF WORD = ENDING TAG OF PREFIX WORD
	      j = tag(1)
c       ADD TO LIST FOR ANY STATE THAT HAS THAT ENDING TAG
c	SORT NEW FIXED POINT INTO PLACE
	      if(fsaon) then
		 do 35 st=2,nstate
		    if(endtag(st).ne.j) go to 35
		    do 20 i=1,numfp(st)
		       wlt = wordcmp(wrdlbl,fplbl(i,st))
		       if( wlt.eq.0 ) then
			  write(*,'(A,A)') 'Already processed: ',
	1		       wlabel(1:lev)
			  return
		       else if( wlt.lt.0 ) then 
			  go to 22
		       end if
 20		    continue
 22		    continue
c	ADD NEW FIXED POINT TO LIST
		    do 30 l=numfp(st),i,-1
		       fplbl(l+1,st) =fplbl(l,st)
		       fp(l+1,st) =fp(l,st)
 30		    continue
		    fplbl(i,st) = wrdlbl
		    fp(i,st)=z1
		    numfp(st) = numfp(st)+1
 35		 continue
	      else
		 do 40 i=1,numfp(j)
		    wlt = wordcmp(wrdlbl,fplbl(i,j))
		    if( wlt.eq.0 ) then
		       write(*,'(A,A)') 'Already processed: ',
	1		    wlabel(1:lev)
		       return
		    else if( wlt.lt.0 ) then 
		       go to 42
		    end if
 40		 continue
 42		 continue
c	ADD NEW FIXED POINT TO LIST
		 do 50 l=numfp(j),i,-1
		    fplbl(l+1,j) =fplbl(l,j)
		    fp(l+1,j) =fp(l,j)
 50		 continue
		 fplbl(i,j) = wrdlbl
		 fp(i,j)=z1
		 numfp(j) = numfp(j)+1
	      end if
c       CYCLE WORD ONE LETTER
	      wrdlbl=wrdlbl(2:lev)//wrdlbl(1:1)
 70	   continue
c       COMPUTE AND PROCESS INVERSE WORD
	   do 72 i=lev,1,-1
	      j=index(alpha,wlabel(i:i))
	      if(j.le.26) then
		 wrdlbl(lev+1-i:lev+1-i) = alpha(j+26:j+26)
	      else
		 wrdlbl(lev+1-i:lev+1-i) = alpha(j-26:j-26)
	      end if
 72	   continue
 75	continue
c       ADD WORD TO LABEL LIST
	write(2,'(1x,A,$)') wlabel(1:lev)
	end
c
c       COMPUTE MATRIX ASSUMING GENERATOR TAGS ARE GIVEN
	subroutine tag2wrd(w,tag,lev,wlabel,err)
	complex*16 w(4),temp,y(4,MAXSIDE)
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	integer tag(*),lev,lev0,l,m,err,digit,lnblnk,opt
	integer p,q,l1,l2,l3,l4,alen,blen,i,j
	character wlabel*(*), genlet*MAXSIDE, gentyp*20,
	1    aword*20,bword*20
	common /gens/ y, side, fside,  nside, gentyp
        common /gensym/ genlet
	wlabel=' '
	lev0=lev
	opt=1
	go to 10
c       GET A WORD FROM USER
	entry getwrd(w,tag,lev,wlabel,err)
	write(*,*) 'Enter tags (in letters a,b,c,...,A,B,C,...)'
	write(*,*) '   Or p/q word format: %p/q(aword,bword)'
	read(*,'(A)') wlabel
c       COMPUTE MATRIX ASSUMING GENERATOR WORD (IN ABC's) IS GIVEN
	entry lbl2wrd(w,tag,lev,wlabel,err)
	if(wlabel(1:1).ne.'%') then
	   lev0 = lnblnk(wlabel)
	   opt=2
	else 
c       p/q WORD
c       FORMAT SHOULD BE %p/q(aword,bword)
	   opt=3
	   l1 = index(wlabel,'/')
	   l2 = index(wlabel,'(')
	   l3 = index(wlabel,',')
	   l4 = index(wlabel,')')
	   read(wlabel(2:l1-1),'(I10)') p
	   read(wlabel(l1+1:l2-1),'(I10)') q
	   read(wlabel(l2+1:l3-1),'(A)') aword
	   read(wlabel(l3+1:l4-1),'(A)') bword
	   alen = lnblnk(aword)
	   blen = lnblnk(bword)
	end if
 10	w(1) = 1.0d0
	w(2) = 0.0d0
	w(3) = 0.0d0
	w(4) = 1.0d0
	err = 0
c       GETTING A WORD FROM GIVEN TAGS
	if(opt.eq.1) then
	   do 20 lev=1,lev0
	      if(tag(lev).le.0.or.tag(lev).gt.nside) go to 22
	      wlabel=wlabel(1:lev-1)//genlet(tag(lev):tag(lev))
	      temp = w(1)*y(1,tag(lev))+w(2)*y(3,tag(lev))
	      w(2) = w(1)*y(2,tag(lev))+w(2)*y(4,tag(lev))
	      w(1) = temp
	      temp = w(3)*y(1,tag(lev))+w(4)*y(3,tag(lev))
	      w(4) = w(3)*y(2,tag(lev))+w(4)*y(4,tag(lev))
	      w(3) = temp
 20	   continue
 22	   continue
	   lev = lnblnk(wlabel)
	   if(lev.eq.0) then
	      return
	   end if
c       GETTING THE TAGS FROM THE GIVEN WORD (opt=2)
	else if(opt.eq.2) then
	   do 30 lev=1,lev0
	      tag(lev)=index(genlet,wlabel(lev:lev))
	      if(tag(lev).le.0.or.tag(lev).gt.nside) go to 32
	      temp = w(1)*y(1,tag(lev))+w(2)*y(3,tag(lev))
	      w(2) = w(1)*y(2,tag(lev))+w(2)*y(4,tag(lev))
	      w(1) = temp
	      temp = w(3)*y(1,tag(lev))+w(4)*y(3,tag(lev))
	      w(4) = w(3)*y(2,tag(lev))+w(4)*y(4,tag(lev))
	      w(3) = temp
 30	   continue
 32	   continue
	   lev = lnblnk(wlabel)
	   if(lev.eq.0) then
	      return
	   end if
	else
c       CONSTRUCTING A p/q WORD
	   i = 0
	   lev = 0
 40	   if(i<q) then
c       MULTIPLY BY A
	      i = i + p
	      do 50 j=alen,1,-1
		 lev = lev + 1
		 tag(lev)=side(index(genlet,aword(j:j)))
		 wlabel=wlabel(1:lev-1)//genlet(tag(lev):tag(lev))
		 temp = w(1)*y(1,tag(lev))+w(2)*y(3,tag(lev))
		 w(2) = w(1)*y(2,tag(lev))+w(2)*y(4,tag(lev))
		 w(1) = temp
		 temp = w(3)*y(1,tag(lev))+w(4)*y(3,tag(lev))
		 w(4) = w(3)*y(2,tag(lev))+w(4)*y(4,tag(lev))
		 w(3) = temp		 
 50	      continue
	   else
c       MULTIPLY BY b
	      i = i - q
	      do 60 j=1,blen
		 lev = lev + 1
		 tag(lev)=index(genlet,bword(j:j))
		 wlabel=wlabel(1:lev-1)//genlet(tag(lev):tag(lev))
		 temp = w(1)*y(1,tag(lev))+w(2)*y(3,tag(lev))
		 w(2) = w(1)*y(2,tag(lev))+w(2)*y(4,tag(lev))
		 w(1) = temp
		 temp = w(3)*y(1,tag(lev))+w(4)*y(3,tag(lev))
		 w(4) = w(3)*y(2,tag(lev))+w(4)*y(4,tag(lev))
		 w(3) = temp		 
 60	      continue
	   end if
	   if(i.gt.0) go to 40
	end if
	end
c
c       COMPARE TWO REPEATING WORDS
c       IF WORD1 < WORD2 RETURN -1
C       IF       =       RETURN 0
C       ELSE RETURN +1
	integer function wordcmp(word1,word2)
	character word1*(*),word2*(*),wrd1*(MAXLEN),wrd2*(MAXLEN)
	complex*16 w1(4),w2(4),y(4,MAXSIDE)
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	integer tag1(MAXLEN),tag2(MAXLEN),lev1,lev2,i,l1,l2,err1,err2,
	1    i1,i2,i1old,i2old
	character gentyp*20
	common /gens/ y, side, fside,  nside, gentyp
	wrd1=word1
	wrd2=word2
	call lbl2wrd(w1,tag1,lev1,wrd1,err1)
	call lbl2wrd(w2,tag2,lev2,wrd2,err2)
	wordcmp = -1
	if(tag1(1) .lt. tag2(1)) then
	   return
	else if(tag1(1) .gt. tag2(1)) then
	   wordcmp = 1
	   return
	end if
	i1old=1
	i2old=1
	do 20 i = 2, lev1*lev2
	   i1 = mod(i-1,lev1)+1
	   i2 = mod(i-1,lev2)+1
	   l1 = mod(nside+fside(tag1(i1old))-tag1(i1),nside)
	   l2 = mod(nside+fside(tag2(i2old))-tag2(i2),nside)
	   if( l1 .lt. l2) then
	      return
	   else if( l1 .gt. l2) then
	      wordcmp = 1
	      return
	   end if
	   i1old=i1
	   i2old=i2
 20	continue
	wordcmp = 0
	return
	end
c
c	GET THRESHOLD PARAMETERS
	subroutine geteps
	real*8 eps,bound,resol,winrad,xmin,ymin,xmax,ymax
	complex*16 wcen
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	common /picwin/ xmin,ymin,xmax,ymax
	common /bnds/ eps,bound,resol,wcen,winrad
	if(epson) return
c	CLIPPING PARAMETERS
	wcen = (dcmplx(xmin,ymin)+dcmplx(xmax,ymax))*0.5d0
	winrad = cdabs( wcen - dcmplx(xmax,ymax) )
c	GET THRESHOLDS FOR TEST
	eps=0.01d0
	write(*,200) 'Enter gap between plotted points: ',eps
200	format(A,/,'Default: ',f20.15)
	read(*,*) eps
	winrad = winrad + 5.0d0*eps
	bound=20.0d0
	write(*,200) 'Enter matrix bound: ',bound
	read(*,*) bound
	resol=0.5d0*eps
	write(*,200) 'Enter plotting resolution: ',resol
	read(*,*) resol
c	LABELLING
	write(2,'(A,f15.10)') 'eps     =    ',eps
	write(2,'(A,f15.10)') 'bound  =  ',bound
	epson=.true.
	end                     
c       
c	INITIATE TREE: Assumes CALGEN has been called
	subroutine init
	external matmul
	real*8 xmin,ymin,xmax,ymax
c       GENERATORS (maximum number = MAXSIDE) AND MATRIX LIST
	complex*16 y(4,MAXSIDE), x(4,MAXLEV)
c       SIDE-PAIRING: i -> side(i), nside = number of sides
c       fside(i) = side(i)-1 modulo nside
	integer side(MAXSIDE), fside(MAXSIDE),  nside
c	LEVEL IN TREE AND TAG LIST = 1, 2, 3, ... , 2n
	integer lev,tag(MAXLEV),state(MAXLEV),maxlev,
	1    fsa(MAXSTATE,MAXSIDE),nstate
	logical fsaon
c       GENERATOR TAGS
	character genlet*MAXSIDE,begwrd*MAXLEN,endwrd*MAXLEN,gentyp*20
c	BEGINNING AND ENDING LISTS OF TAGS
	integer endtag(MAXLEN),endlev,i,numpts
	integer lnblnk,index
	common /gens/ y, side, fside,  nside, gentyp
	common /tree/ x
	common /tags/ tag,lev
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
	common /endtgs/  endtag,endlev
	common /gensym/ genlet
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	tag(1)=0
	if(fsaon) state(1) = 1
	x(1,1)=1.0d0
	x(2,1)=0.0d0
	x(3,1)=0.0d0
	x(4,1)=1.0d0
C	GET BEGINNING WORD
 5	write(*,*) 'Enter starting word:'
	read(*,'(A)') begwrd
	do 10 lev=1,lnblnk(begwrd)
	   i=index(genlet,begwrd(lev:lev))
	   if(i.le.0.or.i.gt.nside) go to 20
	   tag(lev+1)=i
	   call matmul(x(1,lev),y(1,i),x(1,lev+1))
	   if(fsaon) then
	      state(lev+1) = fsa(state(lev),i)
c       FAIL STATE: NONREDUCED WORD
	      if(state(lev+1).eq.FAIL) then
		 write(*,*) 'Non-reduced word: re-enter '
		 go to 5
	      end if
	   end if
 10	continue
 20	continue
	maxlev=lev
C	GET ENDING WORD
	endtag(1)=0
	write(*,*) 'Enter  ending word:'
	read(*,'(A)') endwrd
	do 30 endlev=1,99
	   endtag(endlev+1)=index(genlet,endwrd(endlev:endlev))
	   if(endtag(endlev+1).le.0.or.endtag(endlev+1).gt.nside)
	1	go to 900
 30	continue	
 900	continue
c	INITIATE GRAPH LIMITS
	numpts=0
	xmin=1000.0d0
	ymin=1000.0d0
	xmax=-1000.0d0
	ymax=-1000.0d0
	end
c       
c	TEST THRESHOLDS
c       yesno=.true. => GO FORWARD
	subroutine test(yesno)
	external matmul
	real*8 eps,bound,resol,winrad,neweps,xmin,ymin,xmax,ymax,
	1    rad(MAXSIDE),rad2
	complex*16 z1,zplot(MAXFP),wcen,fp(MAXFP,MAXSTATE),x(4,MAXLEV),
	1    bp(MAXSIDE),mp(MAXSIDE),ep(MAXSIDE),cen(MAXSIDE),
	2    cen2,begpt,midpt,endpt,w(4),zold,
	2    bpt(MAXARC),mpt(MAXARC),ept(MAXARC),prelim(4)
	integer tag(MAXLEV),numz,numfp(MAXSTATE),lev,i,j,numpts,
	1    maxlev,outsty,clrsty,numclr,err,iarc,
	1    mlev,infnum,lnblnk,circnt/0/
	character fplbl(MAXFP,MAXSTATE)*MAXLEN,cirlbl*MAXLEN,
	1    genlet*MAXSIDE
	integer state(MAXLEV),fsa(MAXSTATE,MAXSIDE),nstate
	logical yesno,grpon,epson,outon,raston,curvon,fsaon
	common /flags/ grpon,epson,outon,raston,curvon
	common /tree/ x
	common /tags/ tag,lev
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
	common /fixedp/ fp,fplbl,numfp
	common /gensym/ genlet
	common /bnds/ eps,bound,resol,wcen,winrad
	common /levmax/ mlev
	common /outfmt/ outsty
	common /clrfmt/ clrsty,numclr
	common /cirs/ cen,rad
	common /arcs/ bp,mp,ep
	common /arclst/ bpt,mpt,ept,prelim,iarc
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	save zplot,zold,numz,circnt
c	CHECK FOR MAXIMUM TREE DEPTH
	yesno = .false.
	if(lev .ge. min(mlev,MAXLEV)) return
C	CHECK TO SEE IF SIZE OF MATRIX EXCEEDS GIVEN BOUND
	yesno = bound .ge. cdabs(x(1,lev))+cdabs(x(2,lev))+
	1    cdabs(x(3,lev))+cdabs(x(4,lev)) 
	if(yesno) return
c	ADJUST EPS BASED ON WHERE zplot(1) IS
	if(fsaon) then
	   j=state(lev)
	else
	   j=tag(lev)
	end if
	z1=fp(1,j)
	zplot(1) = (x(1,lev-1)*z1+x(2,lev-1))/(x(3,lev-1)*z1+x(4,lev-1))
	neweps=eps*(dmax1(1.0d0,cdabs(zplot(1) - wcen)/winrad)**2)
c	CALCULATE ALL SPECIAL POINTS
	numz=numfp(j)
	do 30 i=2,numz
	   z1=fp(i,j)
	   zplot(i)=(x(1,lev-1)*z1+x(2,lev-1))/(x(3,lev-1)*z1+x(4,lev-1))
	   yesno = yesno.or.cdabs(zplot(i)-zplot(i-1)).ge.neweps
 30	continue
	return
c       
c	OUTPUT ROUTINE AT INFINITE WORD END
	entry plotx
c	PLOT zplot(1) ONLY IF SCHOTTKY OR FIRST POINT
	do 10 i=1,numz
	   if(numpts.gt.0.and.curvon.and.i.eq.1) go to 10
	   if(outsty.eq.0) then
	      if(numpts.eq.0.or.cdabs(zplot(i)-zold).ge.resol) then
		 write(3,'(F10.6,1X,F10.6)')
	1	      dreal(zplot(i)),dimag(zplot(i))
		 zold = zplot(i)
		 numpts=numpts+1
	      end if
	   else if(outsty.eq.3) then
	      call lablpt(dreal(zplot(i)),dimag(zplot(i)),i)
	   else 
	      call plotpt(dreal(zplot(i)),dimag(zplot(i)))
	   end if
c       ERROR TRACKING
c$$$	write(*,*) 'The current level is',lev
c$$$	write(*,*) 'The tags are ',(tag(j),j=1,lev)
c$$$	write(*,*) 'The states are  ', (state(j),j=1,lev)
c$$$	write(*,*) 'The numz is  ', numz
c$$$	write(*,*) 'The point to plot is  ', zplot(i)
c$$$	call pause
c       END ERROR TRACKING
 10	continue
	return
c       
c	PLOTTING AT EACH VERTEX
	entry vertex
c       SUM POINCARE SERIES
	if(outsty.eq.0) then 
	   call addterm
	else if(outsty.eq.1.or.outsty.eq.2.or.outsty.eq.6) then
c       PLOT CIRCLES
	   if(lev.le.1) return
	   call lfapply(x(1,lev-1),cen(tag(lev)),cen2,
	1	rad(tag(lev)),rad2)
	   if(outsty.eq.1) then
	      call scirc(dreal(cen2),dimag(cen2),rad2)
	   else if(outsty.eq.6) then
	      circnt = circnt+1
	      call scirc(dreal(cen2),dimag(cen2),rad2)
	      write(cirlbl,'(I3,A)') circnt,': '
  	      do 20 i=1,lev
 20	      cirlbl=cirlbl(1:lnblnk(cirlbl))//genlet(tag(i):tag(i))
	      call slabel(dreal(cen2)+rad2,dimag(cen2),
	1	   0.5d0,0.0d0,cirlbl)
	   else
c       DISKS COLORED BY LEVEL
	      call cdisk(dreal(cen2),dimag(cen2),rad2,lev,tag(lev))
	   end if
	else if(outsty.eq.3) then
c       PLOT ARCS
	   if(lev.le.1) return
	   call linfrac(x(1,lev-1),bp(tag(lev)),endpt,err)
	   call linfrac(x(1,lev-1),mp(tag(lev)),midpt,err)
	   call linfrac(x(1,lev-1),ep(tag(lev)),begpt,err)
	   call smove(dreal(begpt),dimag(begpt))
	   call sarcto(dreal(midpt),dimag(midpt),
	1	dreal(endpt),dimag(endpt))
	else if(outsty.eq.4) then
c       PLOT TILE
	   if(clrsty.eq.0) then
	      call btile(lev)
	   else if(clrsty.eq.1) then
	      call btile(tag(lev))
	   else if(clrsty.eq.2) then
	      call btile(state(lev))
	   end if
	   do 90 i=1,iarc
	      call matmul(x(1,lev),prelim,w)
	      infnum=0
	      call linfrac(w,bpt(i),begpt,err)
	      if(err.gt.0) infnum=1
	      call linfrac(w,mpt(i),midpt,err)
	      if(err.gt.0) infnum=2
	      call linfrac(w,ept(i),endpt,err)
	      if(err.gt.0) infnum=3
c       MIDPOINT AT INFINITY (WORST CASE)
	      if(infnum.eq.2) then
C       (HOPE IT DOESN'T HAPPEN FOR NOW)
c       FIRST POINT AT INFINITY
c           MAKE AN APPROXIMATION AND HPE IT WORKS
	      else if(infnum.eq.1) then
		 begpt=endpt+100d0*(midpt-endpt)
c       THIRD POINT AT INFINITY
	      else if(infnum.eq.3) then
		 endpt=begpt+100d0*(midpt-begpt)
	      end if
	      call smove(dreal(begpt),dimag(begpt))
	      call sarcto(dreal(midpt),dimag(midpt),
	1	   dreal(endpt),dimag(endpt))
 90	   continue
c       CLOSE PLOT OF TILE
	   if(clrsty.eq.0) then
	      call etile(lev)
	   else if(clrsty.eq.1) then
	      call etile(tag(lev))
	   else if(clrsty.eq.2) then
	      call etile(state(lev))
	   end if
	else if(outsty.eq.5) then
c       PLOT FILLED TILE
	   if(clrsty.eq.0) then
	      call btile(lev)
	   else if(clrsty.eq.1) then
	      call btile(tag(lev))
	   else if(clrsty.eq.2) then
	      call btile(state(lev))
	   end if
	   do 95 i=1,iarc
	      call matmul(x(1,lev),prelim,w)
	      infnum=0
	      call linfrac(w,bpt(i),begpt,err)
	      if(err.gt.0) infnum=1
	      call linfrac(w,mpt(i),midpt,err)
	      if(err.gt.0) infnum=2
	      call linfrac(w,ept(i),endpt,err)
	      if(err.gt.0) infnum=3
c       MIDPOINT AT INFINITY (WORST CASE)
	      if(infnum.eq.2) then
C       (HOPE IT DOESN'T HAPPEN FOR NOW)
c       FIRST POINT AT INFINITY
c           MAKE AN APPROXIMATION AND HOPE IT WORKS
	      else if(infnum.eq.1) then
		 begpt=endpt+100d0*(midpt-endpt)
c       THIRD POINT AT INFINITY
	      else if(infnum.eq.3) then
		 endpt=begpt+100d0*(midpt-begpt)
	      end if
	      if(i.eq.1) call smove(dreal(begpt),dimag(begpt))
	      call scurve(dreal(midpt),dimag(midpt),
	1	   dreal(endpt),dimag(endpt))
c$$$	      call sarcto(dreal(midpt),dimag(midpt),
c$$$	1	   dreal(endpt),dimag(endpt))
 95	      continue
	   call fillst(0,0)
c       CLOSE PLOT OF TILE
	   if(clrsty.eq.0) then
	      call etile(lev)
	   else if(clrsty.eq.1) then
	      call etile(tag(lev))
	   else if(clrsty.eq.2) then
	      call etile(state(lev))
	   end if
	end if
	numpts=numpts+1
	end
c
c	GO FORWARD IN TREE
	subroutine forwrd(fail)
	external matmul
	complex*16 x(4,MAXLEV),y(4,MAXSIDE)
	integer tag(MAXLEV),state(MAXLEV),lev,fsa(MAXSTATE,MAXSIDE),
	1    nturns,nstate,i
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	character gentyp*20
	logical fsaon,fail
	common /tree/ x
	common /tags/ tag,lev
	common /gens/ y, side, fside,  nside, gentyp
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
c       ASSUME THE SIDES ARE NUMBERED CLOCKWISE 1,2, .... , nside
c         Suppose the current tag is i.
c         Side i is paired to j = side(i) (defined theoretically)
c         So the next tag counterclockwise is j-1 modulo nside = fside(i)
	if(lev.gt.1) then
	   tag(lev+1) = fside(tag(lev))
	   nturns = nside - 1
	else
	   tag(lev+1) = 1
           nturns = nside
	end if
	if(fsaon) then
	   do 10 i=1,nturns
	      state(lev+1) = fsa(state(lev),tag(lev+1))
c       FAIL STATE: CHOOSE NEXT DIRECTION: SUBTRACT 1 FROM tag(lev+1)
	      if(state(lev+1) .ne. FAIL) go to 20
	      tag(lev+1)=mod(nside+tag(lev+1)-2,nside) +1
 10	   continue
	   fail=.TRUE.
	   return
	end if
C	MULTIPLY BY THE APPROPRIATE GENERATOR
 20	fail=.FALSE.
	call matmul(x(1,lev),y(1,tag(lev+1)),x(1,lev+1))
	lev=lev+1
	end
c
c	GO BACKWARD IN TREE
	subroutine bckwrd
	external matmul
	complex*16 x(4,MAXLEV), y(4,MAXSIDE)
	integer tag(MAXLEV),state(MAXLEV),lev,fsa(MAXSTATE,MAXSIDE),
	1    state2,i,nstate,j
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	character gentyp*20
	logical fsaon
	common /tree/ x
	common /tags/ tag,lev
	common /gens/ y, side, fside,  nside, gentyp
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
c	TERMINATE PROGRAM IF LEV IS 1
 10	if(lev .eq. 1) return
	do 20 i=1,nside
c       NEXT POSSIBLE TAG AT LEV
	   tag(lev) = mod( nside + tag(lev) - 2 , nside ) +1
c       CHECK TO SEE IF THIS IS BEYOND LAST POSSIBLE TURN
           if(  lev.gt.2 .and. tag(lev).eq.side(tag(lev-1))
	1	.or. lev.eq.2 .and. tag(lev).eq.1 ) then
	      lev = lev-1
	      go to 10
	   end if
c       CHECK TO SEE IF THE STATE IS ACCEPTABLE
	   if(fsaon) then
	      state2 = fsa(state(lev-1),tag(lev))
c       FAIL STATE: GO TO NEXT POSSIBLE TAG
	      if(state2 .eq. FAIL) go to 20
	      state(lev)=state2
	   end if   
c       STATE IS ACCEPTABLE: MAKE TURN AND GO ON
	   call matmul(x(1,lev-1),y(1,tag(lev)),x(1,lev))
	   return
 20	continue
c       NEVER COMPLETES THIS LOOP
	end
c
c	CHECK FOR THE END OF THE TREE
c	 yesno=.true. => END HAS OCCURRED
	subroutine endchk(yesno)
	complex*16 y(4,MAXSIDE)
	real*8 xmin,ymin,xmax,ymax
	integer numpts,maxlev,tag(MAXLEV),endtag(MAXLEN),endlev,lev,i
	integer side(MAXSIDE), fside(MAXSIDE), nside, lev2,
	1    newtag,state2,j,nstate
	integer state(MAXLEV),fsa(MAXSTATE,MAXSIDE)
	logical yesno,fsaon
	character gentyp*20
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	common /tags/ tag,lev
	common /endtgs/  endtag, endlev
	common /gens/ y, side, fside,  nside, gentyp
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
c       KEEP TRACK OF MAX LEVEL
	maxlev=max(maxlev,lev)
	yesno = .false.
C	THE PROGRAM TERMINATES AFTER REACHING THE LAST
C	"INFINITE" WORD BEGINNING WITH THE GIVEN ENDING WORD.
	if(endlev.gt.1) then
C	CHECK TO SEE IF CURRENT WORD MATCHES ENDING WORD UP TO ENDLEV
C		IF NOT THEN CONTINUE
	   do 10 i=2,endlev
	      if(tag(i).ne.endtag(i).and.i.le.lev) return
 10	   continue
	   lev2 = endlev+1
	else
	   if( tag(2) .ne. 2 ) return
	   lev2 = 3
	end if
	do 20 i=lev2,lev
c       BACK SIDE
	   newtag = side(tag(i-1))
	   do 15 j=1,nside
c       ONE STEP BACKWARDS
	      newtag = mod(newtag, nside ) +1
	      if(newtag.eq.tag(i)) go to 20
	      if(fsaon) then
		 state2 = fsa(state(i-1),newtag)
c       FAIL STATE: GO TO NEXT POSSIBLE TAG
		 if(state2 .eq. FAIL) go to 15
	      end if   
c       STATE DOES NOT MATCH ENDING WORD AND THERE IS
c       AN ACCEPTABLE ROUTE TO FOLLOW
	      return
 15	   continue
c       NEVER COMPLETES ABOVE LOOP
 20	continue
	yesno=.true.
	end
c       
c	CLOSE GROUP ENUMERATION
	subroutine termin(lsflag)
	integer lsflag,numpts,maxlev
	real*8 xmin,ymin,xmax,ymax
	character*12 msg(2)/'limit points','tiles'/
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	write(2,200) msg(lsflag+1),numpts,maxlev
 200	format('No. of ',A,': ',I9,3x,'Max. level = ', I8)
 	write(*,100) xmin,xmax,ymin,ymax,numpts,maxlev
 100	format(1x,f12.6,' < x < ', f12.6,/,1x,f12.6,' < y < ', f12.6,
	1    /,'No. of items:',I9,3x,'Max. level = ', I8)
	end
c       
c       SUPPLEMENTARY PROGRAMS
c       
c	LABEL A LIMIT POINT
c       BEGINNING OF INFINITE WORD IS IN TAGS
c       REPEATING END IS FPLBL(l,j)
	subroutine lablpt(x,y,l)
	real*8 x,y,xmin,ymin,xmax,ymax
	real*8 x0,y0,x1,y1
	integer i,j,k,l,tag(MAXLEV),lev,numfp(MAXSTATE),numpts
	integer fplen, lnblnk, index,maxlev
	complex*16 fp(MAXFP,MAXSTATE)
	character fplbl(MAXFP,MAXSTATE)*MAXLEN,
	1    wrdlbl*MAXLEN,genlet*MAXSIDE
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
        common /gensym/ genlet
	common /tags/ tag,lev
	common /gfinfo/ xmin,ymin,xmax,ymax,numpts,maxlev
	common /fixedp/ fp,fplbl,numfp
	common /picwin/ x0,y0,x1,y1
c       KEEP TRACK OF MAX LEVEL
	maxlev = max(maxlev,lev)
c	FORM PREFIX
	wrdlbl=' '
	do 10 i=1,lev-2
 	   wrdlbl=wrdlbl(1:i-1)//genlet(tag(i+1):tag(i+1))
 10	continue
	fplen = lnblnk( fplbl(l,tag(lev)) )
	wrdlbl = wrdlbl(1:lev-2)//'['//fplbl(l,tag(lev))(1:fplen)//']'
c       
c	SIMPLIFY THE LABEL: Collects as much as possible into periodic ending
c       EXAMPLE: If wrdlbl = ABC[BBC], then final newlbl = A[BCB]
c       
c	LOCATIONS OF [ AND ] IN NEWLBL
	i = index(wrdlbl,'[')
	j = index(wrdlbl,']')
 30	k=i-1
	if(k.eq.0.or.wrdlbl(k:k).ne.wrdlbl(j-1:j-1)) go to 32
	wrdlbl=wrdlbl(1:k-1)//'['//wrdlbl(k:k)//wrdlbl(i+1:j-2)//']'
	i=i-1
	j=j-1
	go to 30
 32	continue
	if(.not.curvon) then
	   call vpoint(x,y,wrdlbl)
	else if(numpts.eq.0) then
	   call vmove(x,y,wrdlbl)
	else
	   call vline(x,y,wrdlbl)
	end if
c	call slabel(x,y,0.5d0,0.0d0,wrdlbl)
	xmin = dmin1(x,xmin)
	ymin = dmin1(y,ymin)
	xmax = dmax1(x,xmax)
	ymax = dmax1(y,ymax)
	numpts=numpts+1
	end
c
c	PLOT AND LABEL A SINGLE LIMIT POINT
	subroutine limpt
	integer tag0(MAXLEN),tag1(MAXLEN),lev0,lev1,err
	complex*16 z,w(4),u(4),w1,w2
	real*8 alen,dangle
	character wlabel*MAXLEN,prefix*MAXLEN,reply
	call gfxdoc('Begin labelled limit points')
 10	write(*,*) 'Enter prefix:'
	call getwrd(u,tag0,lev0,prefix,err)
	if(err .eq. 1) go to 10
 12	write(*,*) 'Enter periodic ending:'
	call getwrd(w,tag1,lev1,wlabel,err)
	if(err .eq. 1) go to 12
	wlabel=prefix(1:lev0)//'['//wlabel(1:lev1)//']'
	call fixpt(w,w1,w2,err)
	write(*,*) 'direction angle,lght of arrow (degrees, inches):'
	read(*,*) dangle,alen
	z=(u(1)*w1+u(2))/(u(3)*w1+u(4))
	call slabel(dreal(z),dimag(z),alen,dangle,wlabel)
	write(*,*) 'Another limit point? (y or n)'
	read(*,'(A)') reply
	if(reply.eq.'y') go to 10
	call gfxdoc('End labelled limit points')
	end
c
c	PLOT A FLOW LINE
	subroutine flowln
	integer i,tag(MAXLEN),lev,err,penw,numpts,maxlev
	complex*16 z,w(4),zseed,w1,w2,zcoef
        complex*16 lambda,lampow,alpha,u(4),zderiv,c
	real*8  zstep, t, tstep,r,xmin,ymin,xmax,ymax,x0,y0,x1,y1
	character wlabel*MAXLEN, reply
	common /picwin/ xmin,ymin,xmax,ymax
	common /gfinfo/ x0,y0,x1,y1,numpts,maxlev
        write(*,*) 'Flow lines for a given word.'
        write(*,*) 'Enter line thickness (pixels): '
        read(*,*) penw
	call gfxdoc('Begin flowlines')
        call ongrf(penw)
	call swindw(xmin,ymin,xmax,ymax)
c       GET A WORD: TAG = LIST OF TAGS, LEV = LENGTH
c                   WRDLBL = WORD LABEL, W = COMPLEX MATRIX
 10	call getwrd(w,tag,lev,wlabel,err)
	call fixpt(w,w1,w2,err)
	if(err.gt.1) then
	   write(*,*)
	1	'One fixed point is infinite; choose another word.'
	   go to 10
	end if
c	MULTIPLIER (LESS THAN ONE IN ABSOLUTE VALUE)
	lambda =1.0d0/((w(3)*w1+w(4))*(w(3)*w1+w(4)))
	alpha = cdlog(lambda)
        write(*,100) dreal(w1),dimag(w1),dreal(w2),dimag(w2)
 100	format(1x,'Fixed points are:',/,
	1    f10.6,'+i ',f10.6,/,f10.6,'+i ',f10.6)
c       NEXT POINT SHOULD BE ABOUT ZSTEP AWAY
	write(*,*) 'Enter step size: (real)'
	read(*,*) zstep
c       SEED POINT FOR FLOW LINE
 20	write(*,*) 'Enter seed point: (complex)'
	read(*,*) zseed
c       ATTRACTIVE FIXED POINT IS FIRST POINT
	numpts=0
	call plotpt(dreal(w1),dimag(w1))
	if(err.eq.1) go to 50
c       HYPERBOLIC CASE
c       MATRIX TRANSFORMING 0 TO W1, INFINITY TO W2, 1 TO ZSEED
	u(1)=w2*(w1-zseed)
	u(2)=w1*(zseed-w2)
	u(3)=w1-zseed
	u(4)=zseed-w2
        t=-dlog(zstep*cdabs((w2-zseed)/(w1-zseed)/(w2-w1)))
	1    /dreal(alpha)
        lampow = cdexp( -t*alpha )
	z= (u(1)*lampow+u(2))/(u(3)*lampow+u(4))
        call plotpt(dreal(z),dimag(z))
	zcoef = cdabs((w2-w1)*(w1-zseed)*(w2-zseed)*alpha)
	do 30 i=1,1000000
	   zderiv = zcoef*cdabs(lampow/(u(3)*lampow+u(4))**2)
	   tstep = zstep/zderiv
	   t = t + tstep
	   lampow = cdexp( -t*alpha )
	   z= (u(1)*lampow+u(2))/(u(3)*lampow+u(4))
           call plotpt(dreal(z),dimag(z))
	   if(cdabs(z-w2) .le. zstep) go to 999
 30	continue
	go to 999
c       PARABOLIC CASE: TRANSLATION
c       THE FLOW LINE IS A CIRCLE THRU W1, ZSEED, W(ZSEED)
 50	z=(w(1)*zseed+w(2))/(w(3)*zseed+w(4))
	call cir3pt(w1,zseed,z,c,r)
	call scirc(dreal(c),dimag(c),r)
 999	write(*,'(A$)') 'Another seed point? (y or N) '
	read(*,'(A)') reply
	if(reply.eq.'y') go to 20
	call offgrf
	call gfxdoc('End flowlines')
	end
c       
c       FINITE STATE AUTOMATON PROGRAMS
c       
c       SELECTS OR GETS FINITE STATE AUTOMATON
c       ASSUMES THAT GENERATORS AND GENERATOR WORD HAS BEEN CHOSEN ALREADY.
	subroutine getfsa
	logical fsaon
	character gentyp*20,fsafil*100
	complex*16 y(4,MAXSIDE)
	integer side(MAXSIDE), fside(MAXSIDE),nside,nstate,
	1    lnblnk,fsa(MAXSTATE,MAXSIDE),i,j,state(MAXLEV)
	common /gens/ y, side, fside,  nside, gentyp
	common /fsabk/ state,fsa,nstate
	common /fsflag/ fsaon
c       FREE GROUPS HAVE NO FSA
	if(.not.fsaon) return
 30	write(*,'(A$)') 'Enter name of file containing fsa: '
	read(*,'(A)') fsafil
	open(3,file=fsafil,status='old',err=35)
	go to 37
 35	fsafil = FSADIR//fsafil(1:lnblnk(fsafil))
	open(3,file=fsafil,status='old',err=36)
	go to 37
 36	write(*,*) 'File does not exist. Please reenter. '
	go to 30
 37	i=0
 40	read(3,*,end=50) (fsa(i+1,j),j=1,nside)
	i=i+1
	go to 40
 50	close(3)
	nstate=i
        end
c       
c	PROCESS SPECIAL WORDS IN FSA CASE
c       
	subroutine spwrd2
	integer i,lev,err,numfp(MAXSTATE),tag(MAXLEN),lnblnk
	integer side(MAXSIDE),fside(MAXSIDE),nside
	integer nstate,fsa(MAXSTATE,MAXSIDE),j,state(MAXLEV),st,st2
	integer fstate(MAXSTATE),ftag(MAXSTATE),bstate(MAXSTATE),
	1    btag(MAXSTATE),mask(MAXSTATE),endtag(MAXSTATE)
	complex*16 fp(MAXFP,MAXSTATE),y(4,MAXSIDE),w(4),z1,z2
	character fplbl(MAXFP,MAXSTATE)*MAXLEN,wlabel*MAXLEN,gentyp*20
	common /fixedp/ fp,fplbl,numfp
	common /gens/ y, side, fside,  nside, gentyp
	common /fsabk/ state,fsa,nstate
	common /fsaend/ endtag
c
c       FIND TWO SPECIAL POINTS FOR EACH STATE:
c          ONE BY ALWAYS FOLLOWING FORWARD TURNS;
c          ONE BY ALWAYS FOLLOWING BACKWARD TURNS;
c
c       FIRST INITIALIZE THE MASK TO 0
	do 3 i=1,nstate
	   mask(i)=0
	   numfp(i)=0
 3	continue
c
c       NOW START TO WORK THROUGH THE STATES
c       ASSUME STATE 1 IS THE START STATE
c          THE ALGORITHM NEVER ENDS AT THE START STATE,
c          SO WE DON'T NEED FIXED POINTS FOR THAT STATE.
c
c       FIRST CALCULATE ENDING TAGS FOR ALL STATES
	endtag(1)=0
	mask(1)=1
	do 20 st=1,nstate
	   do 10 i=1,nside
	      st2 = fsa(st,i)
	      if(st2.ne.FAIL.and.mask(st2).eq.0) then
		 endtag(st2)=i
		 mask(st2) = 1
	      end if
 10	   continue
 20	continue
c
c       NOW CALCULATE FSTATE AND FTAG FOR ALL STATES
	do 30 st = 2, nstate
c       FORWARD SIDE FROM ENDING TAG
	   i= fside(endtag(st))
c       CALCULATE NEXT STATE
 25	   st2 = fsa(st,i)
c       IF NOT FAIL STATE, THEN RECORD THIS AS THE FORWARD STATE
	   if(st2.ne.FAIL) then
	      fstate(st)=st2
	      ftag(st)=i
c       ELSE TRY ANOTHER ROUTE
	   else
	      i=mod(nside+i-2,nside)+1
c       IF ALL ROUTES USED UP, FSTATE = FAIL
	      if(i.eq.side(endtag(st))) then
		 fstate(st)=FAIL
c       ELSE TRY NEXT ROUTE
	      else
		 go to 25
	      end if
	   end if	   
 30	continue
c
c
c       NOW CALCULATE BSTATE AND BTAG FOR ALL STATES
	do 40 st = 2, nstate
c       BACKWARD SIDE FROM ENDING TAG
	   i= mod(side(endtag(st)),nside)+1
c       CALCULATE NEXT STATE
 35	   st2 = fsa(st,i)
c       IF NOT FAIL STATE, THEN RECORD THIS AS THE BACKWARD STATE
	   if(st2.ne.FAIL) then
	      bstate(st)=st2
	      btag(st)=i
c       ELSE TRY ANOTHER ROUTE
	   else
	      i=mod(i,nside)+1
c       IF ALL ROUTES USED UP, BSTATE = FAIL
	      if(i.eq.side(endtag(st))) then
		 bstate(st)=FAIL
c       ELSE TRY NEXT ROUTE
	      else
		 go to 35
	      end if
	   end if	   
 40	continue
c
c       GET THE FORWARD FIXED POINT FOR EACH STATE
c
c       WORKING HERE NOW
	do 50 st=2,nstate
	   lev=1
	   tag(1) = endtag(st)
	   state(1)=st
 45	   state(lev+1)=fstate(state(lev))
c       IF FAIL THEN SKIP
	   if(state(lev+1).eq.-1) go to 50
	   tag(lev+1)=ftag(state(lev))
c       CHECK TO SEE IF WE HAVE A DUPLICATE STATE
	   do 46 i=1,lev
	      if(state(i).eq.state(lev+1)) go to 48
 46	   continue
c       NO DUPLICATE
	   lev = lev+1
	   go to 45
c       WE HAVE A WINNER!
c          THE PATH GOES FROM 1 TO I-1, THEN REPEATS FROM I TO LEV
c          FILL OUT THE TAGS TO LEV+I-1
 48	   do 49 j=1,i-1
 49	      tag(lev+j)=side(tag(i-j))
c       FIND THE WORD ASSOCIATED TO THIS WORD
	   call tag2wrd(w,tag,lev+i-1,wlabel,err)
	   if(err .gt. 0) go to 50
c	COMPUTE  FIXED POINTS (z1= attractive; z2=repelling)
	   call fixpt(w,z1,z2,err)
	   numfp(st)=numfp(st)+1
	   fp(numfp(st),st) = z1
	   fplbl(numfp(st),st) = wlabel
 50	continue
c
c       NOW FIND THE BACKWARD FIXED POINTS
	do 60 st=2,nstate
	   lev=1
	   tag(1) = endtag(st)
	   state(1)=st
 55	   state(lev+1)=bstate(state(lev))
c       IF FAIL THEN SKIP
	   if(state(lev+1).eq.FAIL) go to 60
	   tag(lev+1)=btag(state(lev))
c       CHECK TO SEE IF WE HAVE A DUPLICATE STATE
	   do 56 i=1,lev
	      if(state(i).eq.state(lev+1)) go to 58
 56	   continue
c       NO DUPLICATE
	   lev = lev+1
	   go to 55
c       WE HAVE A WINNER!
c          THE PATH GOES FROM 1 TO I-1, THEN REPEATS FROM I TO LEV
c          FILL OUT THE TAGS TO LEV+I-1
 58	   do 59 j=1,i-1
 59	      tag(lev+j)=side(tag(i-j))
c       FIND THE WORD ASSOCIATED TO THIS WORD
	   call tag2wrd(w,tag,lev+i-1,wlabel,err)
	   if(err .gt. 0) go to 60
c	COMPUTE  FIXED POINTS (z1= attractive; z2=repelling)
	   call fixpt(w,z1,z2,err)
	   numfp(st)=numfp(st)+1
	   fp(numfp(st),st) = z1
	   fplbl(numfp(st),st) = wlabel
 60	continue
	end
c
c       FOR ERROR TRACKING ONLY
	subroutine pause
	character ans*20
	write(*,*) 'Keep going? '
        read(*,*,END=900) ans
 900	continue
	end
