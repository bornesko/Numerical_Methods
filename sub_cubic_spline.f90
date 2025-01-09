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

write(*,*) a
write(*,*) ' '
write(*,*) h
read(*,*)

do i=1,n-2
	r(i)=(((3/h(i+1))*(a(i+2)-a(i+1)))-((3/h(i))*(a(i+1)-a(i))))	!Equations for r
enddo

write(*,*) r
read(*,*)

M=0.0
do i=1,n-2
	M(i,i)=2*(h(i)+h(i+1))
	M(i,i-1)=h(i)
	M(i,i+1)=h(i+1)
enddo	

write(*,*) M
read(*,*)

call inverse_matrix(M,inv_M,n)

c=matmul(inv_M,r)

write(*,*) c
read(*,*)

do i=1,n-1
	b(i)=(1/h(i))*(a(i+1)-a(i))-(h(i)/3)*(c(i+1)+2*c(i))
enddo

write(*,*) b
read(*,*)

do i=1,n-1
	d(i)=(1/3*h(i))*(c(i+1)-c(i))
enddo

write(*,*) d
read(*,*)

end subroutine