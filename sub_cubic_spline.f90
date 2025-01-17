!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! Subroutine to determine function f_s(y) !!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ***   Notes   ***   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cubic_spline(y,z_s,n)								!Subroutine calls "y" and "z_s" from input values, and "n" number of inputs (nodes) 
																!
use glob_variables												!Call global variables module file
																!
implicit none													!
																!
real 				y(n),z_s(n)									!Arrays to store input values
real,allocatable::	h(:),r(:),b(:),c(:),d(:),a(:),c1(:)			!Allocatable vector arrays needed for the equation
real,allocatable::	M(:,:),inv_M(:,:)							!Alocatable matrix M and its inverse
real				f_s,y0,sp1,sp2,sp							!Variables for function, evaluating values for f(y), and limits for print spacing 
integer 			i,j											!Loop indices
integer 			n,n1,z										!Number of input nodes "n" and additional "n1" for some subroutines
																!
!!!!!!!!!!!!!!!!!!!!! Allocate arrays sizes !!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Allocate(a(n))													!Allocate arrays
Allocate(M(n-2,n-2))											!""
Allocate(inv_M(n-2,n-2))										!""
Allocate(h(n-1))												!""
Allocate(c(n))													!""
Allocate(c1(n-2))												!""
Allocate(b(n-1))												!""
Allocate(r(n-2))												!""
Allocate(d(n-1))												!""
																!""
!!!!!!!!!! Calculate variables needed for f_s equation !!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i=1,n-1														!
	h(i)=y(i+1)-y(i)											!Equation for "h" variable
enddo															!
																!
a=z_s															!"a" variable has the same values from "z_s" input
																!
do i=1,n-2														!
	r(i)=(((3/h(i+1))*(a(i+2)-a(i+1)))-((3/h(i))*(a(i+1)-a(i))))!Equation for "r" variable
enddo															!
																!
M=0.0															!Matrix M filled with zeros
do i=1,n-2														!
	M(i,i)=2*(h(i)+h(i+1))										!Equation for diagonal elements of the matrix
	M(i,i-1)=h(i)												!Equation for elements to the left of the diagonal
	M(i,i+1)=h(i+1)												!Equation for elements to the right of the diagonal
enddo															!		
																!
a=z_s															!"a" variable has the same values from "z_s" input (NEEDED AGAIN, NOT SURE WHY)
																!
n1=n-2															!Definition of reduced "n1" needed for the matrices in det and inv subroutines
																!
call inverse_matrix(M,inv_M,n1)									!Call inverse matrix subroutine
																!
c1=matmul(inv_M,r)												!Matrix-vector multiplication to calculate c (intermediate values)
do i=2,n-1														!
	c(i)=c1(i-1)												!Loop to insert intermediate c values into array 
enddo															!
																!
c(1)=0															!c first boundary value zero
c(n)=0															!c last boundary value zero
																!
do i=1,n-1														!
	b(i)=((1/h(i))*(a(i+1)-a(i)))-((h(i)/3)*(c(i+1)+(2*c(i))))	!Equation for "b" variable
enddo															!
																!
do i=1,n-1														!
	d(i)=(1/(3*h(i)))*(c(i+1)-c(i))								!Equation for "d" variable
enddo															!
																!
!!!!!!!!!!!! Define equidistant spacing for printing !!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																!
sp1=y(1)														!Define bottom limit for spacing
sp=(y(n)-y(1))/100												!Define spacing size (total range divided by some arbitrary number)  
sp2=y(n)														!Define top limit for spacing
																!
!!!!!!!!!!!!!!!!!!!!!!! Print f_s values !!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
open(21,file='task_1/data_f_s.txt')									!Open file to store printed values
																!
do i=1,101														!Loop from 1 to the total number of equidistant spaces +1
	y0=sp1+(sp*real(i-1))										!"y0" variable that will be evaluated into function f_s( ) 
	z=1															! ____________________________________________________________________________________   
	do while(z.le.n-1)											!*|Function f_s( ) is different for each range in-between "y" inputs. Therefore, these
		if(y0.ge.y(z).and.y0.lt.y(z+1)) then					!*|two loops check if the evaluated "y0" is within a pair of values y(z) and y(z+1).
		z=z														!*|If true, z remains and it defines the variables later. If not, it increases +1 and
		exit													!*|the if-loop is checked again.
		else													!*|
		z=z+1													!*|
		endif													!*|___________________________________________________________________________________
	enddo														!
	an=a(z)														!Global variable "an" corresponding to a(z) for the range y(z) where y0 is located
	bn=b(z)														!""
	cn=c(z)														!""
	dn=d(z)														!""
	yn=y(z)														!""
	write(21,*) y0, f_s(y0)										!Print "y0" and the result of f_s( ) evaluated for that value
enddo															!
close(21)														!Close the results file
																!														!
end subroutine													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! Define function f_s(y) !!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ***   Notes   ***   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function f_s(y0)											!Initiate "function" program
																!
use glob_variables												!Call global variables module file
																!
implicit none													!
																!
real y0      													!Real number to evaluate the function
																!
f_s=(an)+(bn*(y0-yn))+(cn*(y0-yn)**2)+(dn*(y0-yn)**3)			!Equation of function f_s
																!
end function													!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
