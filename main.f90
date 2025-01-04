!Main Program file, where every Subroutine and function converges

program main_program

implicit none

!Variables
real, allocatable:: x(:), Z_r(:), y(:), z_s(:) 	!Variables for X and fx, that should be read from a .txt file
integer n										!Number of Points (first line of the .txt file)
integer i										!Loop Variable

open(10,file='input_points.txt')
read(10,*) n	!Read the size from the first line of the .txt file
read(10,*)		!Skipping the headings "x" and "fx" in the .txt file

!Assigning number of points
allocate(x(n))							
allocate(fx(n))							
allocate(y(n))
allocate(fy(n))

!Assign values for x and fx from the text file
do i=1,n
	read(10,*) x(i), z_r(i)			
enddo

read(10,*) !There is an empty space in the .txt, which should be skipped
read(10,*) !Skipping the headings "y" and "fy" in the .txt file

!Assign values for y and fy from the text file
do i=1,n
	read(10,*) y(i), z_s(i)			
enddo

close(10)

!Call function X

call cubic_spline(y,z_s)

!Write Check for the points
write(*,*) 'x coordinates:'
write(*,*) x
write(*,*) 'fx coordinates:'
write(*,*) fx
write(*,*) 'y coordinates:'
write(*,*) y
write(*,*) 'fy coordinates:'
write(*,*) fy

end program

!Subroutine for function Y, just temporarily in the main file
subroutine cubic_spline(y,z_s,n)

implicit none

real			 	y(:),z_s(:),a(:)						!Vectors
real,allocatable::	h(:),r(:),b(:),c(:),d(:)				!
real,allocatable::	M(:,:)									!
integer i,j													!Things for loops
integer n													!Number of nodes

Allocate y(n)
Allocate z_s(n)
Allocate a(n)
Allocate M(n-2,n-2)
Allocate h(n-1)
Allocate c(n)
Allocate b(n-1)
Allocate r(n-2)
Allocate d(n-1)

do i=1,n-1
	h(i)=y(i+1)-y(i)
enddo

a=z_s														!Values of a are equal to those of z_s
c(1)=0														!Boundary value zero
c(n)=0														!Boundary value zero

do i=1,n-2
	r(i)=(3/h(i+1))*(a(i+2)-a(i+1))-(3/h(i))*(a(i+1)-a(i))	!Equations for r
enddo

!!!!Still need to calculate C
!
!
!!!!

do i=1,n-1
	b(i)=(1/h(i))*(a(i+1)-a(i))-(h(i)/3)*(c(i+1)+2*c(i))
enddo

do i=1,n-1
	d(i)=(1/3*h(i))*(c(i+1)-c(i))
enddo
end subroutine
!!!