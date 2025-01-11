!Subroutine for function Y, just temporarily in the main file
subroutine cubic_spline(y,z_s,n)

use glob_variables_cubicspl

implicit none

real 				y(n),z_s(n)						!Vectors
real,allocatable::	h(:),r(:),b(:),c(:),d(:),a(:),c1(:)				!
real,allocatable::	M(:,:),inv_M(:,:)
real				f_s,y0,sp1,sp2,sp								!
integer 			i,j												!Things for loops
integer 			n,n1,z													!Number of nodes

Allocate(a(n))
Allocate(M(n-2,n-2))
Allocate(inv_M(n-2,n-2))
Allocate(h(n-1))
Allocate(c(n))
Allocate(c1(n-2))
Allocate(b(n-1))
Allocate(r(n-2))
Allocate(d(n-1))

do i=1,n-1
	h(i)=y(i+1)-y(i)
enddo

!write(*,*) h
!read(*,*)

n1=n-2
a=z_s														!Values of a are equal to those of z_s

!write(*,*) a
!read(*,*)


do i=1,n-2
	r(i)=(((3/h(i+1))*(a(i+2)-a(i+1)))-((3/h(i))*(a(i+1)-a(i))))	!Equations for r
enddo

!write(*,*) r
!read(*,*)

!M=0.0
do i=1,n-2
	M(i,i)=2*(h(i)+h(i+1))
	M(i,i-1)=h(i)
	M(i,i+1)=h(i+1)
enddo	

!write(*,*) M
!read(*,*)

call inverse_matrix(M,inv_M,n1)

!write(*,*) inv_M
!read(*,*)

c(1)=0														!Boundary value zero
c(n)=0														!Boundary value zero

c1=matmul(inv_M,r)
do i=2,n-1
	c(i)=c1(i-1)
enddo
	
!write(*,*) c
!read(*,*)

do i=1,n-1
	b(i)=((1/h(i))*(a(i+1)-a(i)))-((h(i)/3)*(c(i+1)+2*c(i)))
enddo

!write(*,*) b
!read(*,*)

do i=1,n-1
	d(i)=(1/(3*h(i)))*(c(i+1)-c(i))
enddo

!write(*,*) d
!read(*,*)

!Define equidistant spacing for printed Values

sp1=y(1)
sp=(y(n)-y(1))/100
sp2=y(n)

!Open input files
open(21,file='data_f_s.txt')

!!!Print test values
do i=1,101
	y0=sp1+(sp*real(i-1))
	z=1
	do while(z.le.n-1) !!Check later if its n, n-1,
		if(y0.ge.y(z).and.y0.lt.y(z+1)) then
		z=z
		exit
		else
		z=z+1
		endif
	enddo
	an=a(z)
	bn=b(z)
	cn=c(z)
	dn=d(z)
	yn=y(z)
!	write(*,*)an,bn,cn,dn,yn
!	read(*,*)
	write(21,*) y0, f_s(y0)
!	read(*,*)
enddo

read(*,*)

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!gnuplot function
!Close input files
close(21)

!gnuplot file writing
open(22,file='plot_cubicspl.plt')
write(22,*) 'set title "f_s(y)"'
write(22,*) 'set xrange [-7:7]'
write(22,*) 'set yrange [-1:3]'
write(22,*) 'set xlabel "y [U]"'
write(22,*) 'set ylabel "z_s [U]"'					!' ' read by gnuplot as commands, "" read by gnuplot as text
write(22,*) 'plot "data_f_s.txt"' 	!!First column x values, second column y values
write(22,*) 'pause -1 "Hit ENTER to continue"'
close(22)

! Calling the wgnuplot.exe and handing over the gnuplot-input file
!**************************************************************************!
call system('binary\wgnuplot plot_cubicspl.plt')

end subroutine

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Create Module

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!Function f_s(y)
real function f_s(y0)

use glob_variables_cubicspl

implicit none

real y0      						! argument of the function
!real a(n),c(n),b(n-1),d(n-1),y(n)
!integer n,z,i

!!!Calculation of the function value
!**************************************************************************!
!z=1
!do while(z.le.n) !!Check later if its n, n-1,
!	if(y0.ge.y(z).and.y0.lt.y(z+1)) then
!	i=z
!	exit
!	else
!	z=z+1
!	endif
!enddo
	
f_s=(an)+(bn*(y0-yn))+(cn*(y0-yn)**2)+(dn*(y0-yn)**3)

end function
