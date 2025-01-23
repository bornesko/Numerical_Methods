!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!! Subroutine to determine function f_s(y) !!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ***   Notes   ***   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
subroutine cubic_spline(z_s)												!Subroutine calls "y" and "z_s" from input values, and "n" number of inputs (nodes) 
																			!
use glob_variables															!Call global variables module file
																			!
implicit none																!
																			!
real 				z_s(n)													!Array to store input z_s values
real,allocatable::	h(:),r(:),c1(:)											!Allocatable vector arrays needed for the equation
real,allocatable::	M(:,:),inv_M(:,:)										!Alocatable matrix M and its inverse
real				f_s,y0,sp1,sp2,sp										!Variables for function, evaluating values for f(y), and limits for print spacing 
integer 			i,j														!Loop indices
integer 			n1,z													!Number of input nodes "n" and additional "n1" for some subroutines
																			!
!!!!!!!!!!!!!!!!!!!!! Allocate arrays sizes !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
Allocate(a_cs(n))															!Allocate arrays
Allocate(M(n-2,n-2))														!""
Allocate(inv_M(n-2,n-2))													!""
Allocate(h(n-1))															!""
Allocate(c_cs(n))															!""
Allocate(c1(n-2))															!""
Allocate(b_cs(n-1))															!""
Allocate(r(n-2))															!""
Allocate(d_cs(n-1))															!""
																			!""
!!!!!!!!!! Calculate variables needed for f_s equation !!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
do i=1,n-1																	!
	h(i)=y(i+1)-y(i)														!Equation for "h" variable
enddo																		!
																			!
a_cs=z_s																	!"a" variable has the same values from "z_s" input
																			!
do i=1,n-2																	!
	r(i)=(((3/h(i+1))*(a_cs(i+2)-a_cs(i+1)))-((3/h(i))*(a_cs(i+1)-a_cs(i))))!Equation for "r" variable
enddo																		!
																			!
M=0.0																		!Matrix M filled with zeros
do i=1,n-3																	!
	M(i,i)=2*(h(i)+h(i+1))													!Equation for main diagonal elements of the matrix
	M(i,i+1)=h(i+1)															!Equation for elements of the upper diagonal
	M(i+1,i)=h(i+1)															!Equation for elements of the lower diagonal
enddo																		!
M(n-2,n-2)=2*(h(n-3)+h(n-2))												!Final value of main matrix diagonal (n-2,n-2)																		!		
																			!
n1=n-2																		!Definition of reduced "n1" needed for the matrices in det and inv subroutines
																			!
call inverse_matrix(M,inv_M,n1)												!Call inverse matrix subroutine
																			!
c1=matmul(inv_M,r)															!Matrix-vector multiplication to calculate c_cs (intermediate values)
do i=2,n-1																	!
	c_cs(i)=c1(i-1)															!Loop to insert intermediate c_cs values into array 
enddo																		!
																			!
c_cs(1)=0																	!c_cs first boundary value zero
c_cs(n)=0																	!c_cs last boundary value zero
																			!
do i=1,n-1																	!
	b_cs(i)=((1/h(i))*(a_cs(i+1)-a_cs(i)))-((h(i)/3)*(c_cs(i+1)+(2*c_cs(i))))	!Equation for "b_cs" variable
enddo																		!
																			!
do i=1,n-1																	!
	d_cs(i)=(1/(3*h(i)))*(c_cs(i+1)-c_cs(i))								!Equation for "d_cs" variable
enddo																		!
																			!
!!!!!!!!!!!! Define equidistant spacing for printing !!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
																			!
sp1=y(1)																	!Define bottom limit for spacing
sp=(y(n)-y(1))/100															!Define spacing size (total range divided by some arbitrary number)  
																			!
!!!!!!!!!!!!!!!!!!!!!!! Print f_s values !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
open(21,file='task_1/data_f_s.txt')											!Open file to store printed values
																			!
do i=1,101																	!Loop from 1 to the total number of equidistant spaces +1
	y0=sp1+(sp*real(i-1))													!"y0" variable that will be evaluated into function f_s( ) 
	write(21,*) y0, f_s(y0)													!Print "y0" and the result of f_s( ) evaluated for that value
enddo																		!
close(21)																	!Close the results file
																			!														
end subroutine																!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!! Define function f_s(y) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!   ***   Notes   ***   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real function f_s(y0)														!Initiate "function" program
																			!
use glob_variables															!Call global variables module file
																			!
implicit none																!
																			!
real y0      																!Real number to evaluate the function
integer z																	!Integer z for the loop
																			!
z=1																			! ____________________________________________________________________________________   
	do while(z.le.n-1)														!*|Function f_s( ) is different for each range in-between "y" inputs. Therefore, these
		if(y0.ge.y(z).and.y0.lt.y(z+1)) then								!*|two loops check if the evaluated "y0" is within a pair of values y(z) and y(z+1).
		z=z																	!*|If true, z remains and it defines the variables later. If not, it increases +1 and
		exit																!*|the if-loop is checked again.
		else																!*|
		z=z+1																!*|
		endif																!*|___________________________________________________________________________________
	enddo																	!
																			!
f_s=(a_cs(z))+(b_cs(z)*(y0-y(z)))+(c_cs(z)*(y0-y(z))**2)+(d_cs(z)*(y0-y(z))**3)	!Equation of function f_s
																			!
end function																!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
