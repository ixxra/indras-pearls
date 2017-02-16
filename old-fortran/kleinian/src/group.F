c 	GROUP.F
c       
c	This contains routines for manipulating groups.
c       
c	CHANGE GROUP
	subroutine chgrp
	character choice*20
	integer lnblnk
 	write(*,*) 'SELECT ACTION '
 10	write(*,'(8X,A15,3X,A)')
	1    'conjugate:','Conjugate group',
	2    'subgroup:','Pick a subgroup',
	3    'dehn:','Dehn twist of generators',
	4    'gpcalc:','Calculations with generators',
	1    'up:','Just Press Return To Go Up To Previous Menu'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'conjugate') then
c       CONJUGATE GROUP
	   call conjug
	else if(choice.eq.'subgroup') then
c       PICK A SUBGROUP
	   call subgp
	else if(choice.eq.'dehn') then
c       DEHN TWIST
	   call dehn
	else if(choice.eq.'gpcalc') then
c       GROUP CALCULATIONS
	   call gpcalc
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c       
c       CONJUGATE GROUP
	subroutine conjug
	complex*16 y(4,MAXSIDE),ynew(4,MAXSIDE),w(4),temp,wdet
	integer side(MAXSIDE), fside(MAXSIDE),  nside
	integer nside2,i,j,lnblnk,index,tag(MAXLEN),lev,err
	character genlet*MAXSIDE,genlet2*MAXSIDE,wrdlbl*MAXLEN,reply
	character gentyp*20, alpha*52
	1    /'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'/
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	common /gens/ y, side, fside,  nside, gentyp
	common /gensym/ genlet
	write(*,*) 'Enter conjugating matrix A (G -> AGA^-1) '
	write(*,*) '   (4 complex*16): '
	read(*,*) w
	wdet = w(1)*w(4) - w(2)*w(3)
	do 10 i=1,nside
c       Y_i = A Y_i
	   temp = w(1)*y(1,i)+w(2)*y(3,i)
	   y(3,i) = w(3)*y(1,i)+w(4)*y(3,i)
	   y(1,i) = temp
	   temp = w(1)*y(2,i)+w(2)*y(4,i)
	   y(4,i) = w(3)*y(2,i)+w(4)*y(4,i)
	   y(2,i) = temp
c       Y_i = Y_i A^-1
	   temp = w(4)*y(1,i)-w(3)*y(2,i)
	   y(2,i) = (-w(2)*y(1,i)+w(1)*y(2,i))/wdet
	   y(1,i) = temp/wdet
	   temp = w(4)*y(3,i)-w(3)*y(4,i)
	   y(4,i) = (-w(2)*y(3,i)+w(1)*y(4,i))/wdet
	   y(3,i) = temp/wdet
 10	continue
	call spword
	return
c       
c       PICK A SUBGROUP
	entry subgp
	write(*,'(A$)') 'Enter new sidepairing as word: '
	read(*,'(A)') genlet2
	nside2=lnblnk(genlet2)
	do 200 i=1,nside2
c       LOWER CASE GEN
	   if(index(alpha,genlet2(i:i)) .le. 26) then
	      write(*,'(A,I2,A$)') 'Enter generator ',i,' as a word: '
	      read(*,'(A)') wrdlbl
	      call lbl2wrd(w,tag,lev,wrdlbl,err)
	      ynew(1,i)=w(1)
	      ynew(2,i)=w(2)
	      ynew(3,i)=w(3)
	      ynew(4,i)=w(4)
	   end if
 200	continue
	do 210 i=1,nside2
	   y(1,i)=ynew(1,i)
	   y(2,i)=ynew(2,i)
	   y(3,i)=ynew(3,i)
	   y(4,i)=ynew(4,i)
 210	continue
	genlet=genlet2
	gentyp='subgp'
	call mkgens
	write(*,'(A$)') 'Is this a curve? (y or N) '
	read(*,'(A)') reply
	curvon=.false.
	if(reply.eq.'y') curvon=.true.
	epson=.false.
	call spword
	return
c       
c       AUTOMORPHISM ROUTINES
c	(X,Y) -> (X,XY)
	entry dehn
	write(*,'(A$)') 'Enter first and second generator indices: '
        read(*,*) i,j
	temp = y(1,i)*y(1,j)+y(2,i)*y(3,j)
	y(3,j) = y(3,i)*y(1,j)+y(4,i)*y(3,j)
	y(1,j) = temp
	temp = y(1,i)*y(2,j)+y(2,i)*y(4,j)
	y(4,j) = y(3,i)*y(2,j)+y(4,i)*y(4,j)
	y(2,j) = temp
	call mkgens
	end
c       
c	GROUP CALCULATIONS
	subroutine gpcalc
	character choice*20, wrdlbl*MAXLEV
	integer lnblnk
	logical grpon,epson,outon,raston,curvon
	common /flags/ grpon,epson,outon,raston,curvon
	if(.not.grpon) call setgrp
 	write(*,*) 'SELECT ACTION '
 10	write(*,'(8X,A15,3X,A)')
	1    'data:','Print info about a word',
	2    'gens:','Print generator info',
	1    'up:','Just Press Return To Go Up To Previous Menu'
	write(*,'(/,5X,A$)') 'SELECT: '
	read(*,'(A)') choice
	if(choice.eq.'data') then
 15	   write(*,'(A$)') 'Enter word: (Return to quit)'
	   read(*,'(A)') wrdlbl
	   if(lnblnk(wrdlbl).eq.0) go to 10
	   call wrddat(wrdlbl)
	   go to 15
	else if(choice.eq.'gens') then
	   call prgens
	else if(choice.ne.'up'.and.lnblnk(choice).gt.0) then
	   write(*,*) 'Bad command, select again'
	else 
	   go to 999
	end if
	go to 10
 999	return
	end
c
c       DATA OF A WORD
	subroutine wrddat(wrdlbl)
	character wrdlbl*(*)
	complex*16 w(4),z1,z2
	integer tag(MAXLEV),lev,err
	call lbl2wrd(w,tag,lev,wrdlbl,err)
	write(*,100) w(1),w(2),w(3),w(4), w(1)+w(4)
 100	format(' Matrix = [',f20.15,'+i ',f20.15,4x,
	1                    f20.15,'+i ',f20.15,']',/,
	2      '          [',f20.15,'+i ',f20.15,4x,
	3                    f20.15,'+i ',f20.15,']',/,
	4      ' Trace = ',f20.15,'+i ',f20.15)
	call fixpt(w,z1,z2,err)
	write(*,200) z1,z2
 200	format(' Attr. fixed point = ',f20.15,'+i ',f20.15,/,
	1      ' Repell. fixed pt. = ',f20.15,'+i ',f20.15)
	end
c       
c       PRINT GENERATORS
	subroutine prgens
	complex*16 y(4,MAXSIDE)
	integer side(MAXSIDE), fside(MAXSIDE),  nside,i
	character genlet*MAXSIDE, gentyp*20
	common /gens/ y, side, fside,  nside, gentyp
	common /gensym/ genlet
	write(*,'(A,A)') 'Generator word is ',genlet(1:nside)
	do 10 i =1,nside
	   write(*,100) genlet(i:i),y(1,i),y(2,i),y(3,i),y(4,i)
 100	   format(' ',A1,' = [',f20.15,'+i ',f20.15,4x,
	1	                f20.15,'+i ',f20.15,']',/,
	2	       '     [',f20.15,'+i ',f20.15,4x,
	3	                f20.15,'+i ',f15.10,']')
 10	continue
	end
