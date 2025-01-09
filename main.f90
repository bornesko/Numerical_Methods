!Main Program file, where every Subroutine and function converges

program main_program

implicit none

!Variables
real, allocatable:: x(:), z_r(:), y(:), z_s(:) 	!Variables for X and fx, that should be read from a .txt file
integer n										!Number of Points (first line of the .txt file)
integer i										!Loop Variable

open(10,file='input_points.txt')
read(10,*) n	!Read the size from the first line of the .txt file
read(10,*)		!Skipping the headings "x" and "fx" in the .txt file

!Assigning number of points
allocate(x(n))							
allocate(z_r(n))							
allocate(y(n))
allocate(z_s(n))

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

call cubic_spline(y,z_s,n)

!Write Check for the points
write(*,*) 'x coordinates:'
write(*,*) x
write(*,*) 'fx coordinates:'
write(*,*) z_r
write(*,*) 'y coordinates:'
write(*,*) y
write(*,*) 'fy coordinates:'
write(*,*) z_s
read(*,*)

end program

!Subroutine for function Y, just temporarily in the main file
subroutine cubic_spline(y,z_s,n)

implicit none

real 	y(n),z_s(n)						!Vectors
real,allocatable::	h(:),r(:),b(:),c(:),d(:),a(:)				!
real,allocatable::	M(:,:),inv_M(:,:)								!
integer i,j													!Things for loops
integer n													!Number of nodes

Allocate(a(n))
Allocate(M(n-2,n-2))
Allocate(inv_M(n-2,n-2))
Allocate(h(n-1))
Allocate(c(n))
Allocate(b(n-1))
Allocate(r(n-2))
Allocate(d(n-1))

do i=1,n-1
	h(i)=y(i+1)-y(i)
enddo

a=z_s														!Values of a are equal to those of z_s
c(1)=0														!Boundary value zero
c(n)=0														!Boundary value zero

do i=1,n-2
	r(i)=(3/h(i+1))*(a(i+2)-a(i+1))-(3/h(i))*(a(i+1)-a(i))	!Equations for r
enddo

M=0.0
do i=1,n-2
	M(i,i)=2*(h(i)+h(i+1))
	M(i,i-1)=h(i)
	M(i,i+1)=h(i+1)
enddo	

call inverse_matrix(M,inv_M,n)

c=matmul(inv_M,r)

do i=1,n-1
	b(i)=(1/h(i))*(a(i+1)-a(i))-(h(i)/3)*(c(i+1)+2*c(i))
enddo

do i=1,n-1
	d(i)=(1/3*h(i))*(c(i+1)-c(i))
enddo

write(*,*) a
write(*,*) ' '
write(*,*) h
write(*,*) ' '
write(*,*) M
write(*,*) ' '
write(*,*) r
write(*,*) ' '
write(*,*) c
write(*,*) ' '
write(*,*) b
write(*,*) ' '
write(*,*) d
read(*,*)

end subroutine
!!!