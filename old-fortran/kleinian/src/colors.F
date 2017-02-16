c----------------------------------------------------------------------
c       
c       COLOR OPTIONS MENU
c       
c	This program sets up the options for coloring tiles
c       and other objects drawn by KLEINIAN.
c      
	subroutine clrmenu
	character choice*15,col*100
	integer clrsty,numclr,lnblnk
	logical grpon,epson,outon,raston,curvon,fsaon
	integer nstate,fsa(MAXSTATE,MAXSIDE),state(MAXLEV)
	common /fsabk/ state,fsa,nstate
	common /flags/ grpon,epson,outon,raston,curvon
	common /fsflag/ fsaon
	common /clrfmt/ clrsty,numclr
c     SET THE GROUP IF NOT DONE
        if(.not.grpon) call setgrp
 10	write(*,'(/,5X,A,/)') ' COLOR OPTIONS MENU: '
	write(*,'(8X,A15,3X,A)')
     $       'onecolor:','Plot in one color only',
     $       'level:','Choose color by level in tree',
     $       'tag:','Choose color by ending tag',
     $       'FSA:','Choose color by state in FSA',
     $       'colormap:','Choose fixed colormap in psgraph.pro',
     $       'number:','Choose a fixed number of colors',
     $       'up:','Just Press Return To Go To MAIN MENU'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'onecolor') then
c	CHOOSE A FIXED COLOR
	   write(*,'(A)') 'Enter the name of a color (as ',
	1	'defined in psgraph.pro, e.g. Red):'
	   read(*,'(A)') col
	   call ccolor(col)
c     CTILE = 0 TURNS OFF COLORING
           call ctile(0)
	else if(choice.eq.'level') then
c     CTILE = 1 TURNS ON COLORING WITH CC
           call ctile(1)
           clrsty = 0
	else if(choice.eq.'tag') then
           call ctile(1)
           clrsty = 1
	else if(choice.eq.'FSA') then
c     ASSUME THE FSA IS ACTIVATED
           clrsty = 2
           numclr = nstate
           call ctile(2)
	else if(choice.eq.'colormap') then
c	CHOOSE A NEW COLORMAP
	   write(*,'(A)') 'Enter the name of a colormap (as ',
	1	'defined in psgraph.pro, e.g. cmapA):'
	   read(*,'(A)') col
	   call setcmap(col)
	else if(choice.eq.'number') then
	   write(*,'(A)') 'Enter the number of colors: '
	   read(*,*) numclr
           call ctile(2)
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c	
c	PARAMETERS FOR COLORS
	block data clrdata
	integer clrsty,numclr
	common /clrfmt/ clrsty,numclr
	data clrsty/0/,numclr/12/
	end	
