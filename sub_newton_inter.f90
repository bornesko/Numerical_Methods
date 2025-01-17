subroutine newton_interpolation(x,z_r,n)

use glob_variables

implicit none

real x(n)						!Given x points
real z_r(n)						!Given fx points
real f_r						!Function to calculate the interpolation
real temp						!Temporary value to store
real, allocatable::	A(:,:)		!Matrix for the coefficients
real, allocatable:: C(:)		!Coefficients from the matrix for the function
integer n						!The number of points given 
integer i, j, k					!Loop Indices

!Assigning sizes
allocate(A(n,n))					
allocate(C(n))

!Assigning values for the first column of the matrix, which are the same as fx
do i=1,n							
	A(i,1)=z_r(i)
enddo

!Loop for the matrix for the newton method. See Excel Sheet for reference
k=1				
do j=2,n		!Column of matrix
	do i=2,n 	!Row of Matrix
	A(i,j)=(A(i-1,j-1)-A(i,j-1))/(x(i-k)-x(i))	!Equation for the triangular matrix. The k is for the x value, because we always start from x1
	enddo											!Look at the formula from the exercise and also the formula in the Excel Sheet
	k=k+1										
enddo

!Extract the coefficients of the function from the matrix -> the diagonal
do i=1,n
	C(i)=A(i,i)
enddo

!Global values needed for the function
allocate(C_f(n))
allocate(x_f(n))

C_f = C 	!The coefficients
x_f = x	!The given X points
p = n		!The number of given X points

temp=x(1)										!Lowest given point as a starting reference for the loop 

open(11,file='task_1/data_f_r.txt')					!Create a file to store all the points that are going to be calculated x and coresponding fx

!loop to gather points from the function
do
	write(11,*) temp, f_r(temp)		!Start from the lowest x = xu(1) = temp and write the x and the fx (newton result)
	temp=temp+0.1								!With and increment of 0.1
	if(temp.gt.x(n))exit						!And stop when x(n) is reached
enddo

close(11)

end subroutine



real function f_r(x0)

use glob_variables	!the global variables are x_f, C_f and p

implicit none

real x0						!The input from xu(1) to xu(n) with an increment
integer j				    !Loop index
real temp_f, temp, temp_c	!Temporary variables to strore info

temp_c = C_f(1)		!First part of the function -> Only the first coefficient. In the exercise the first coeff is C0. HERE IT IS C1
temp = 1.00			!The multiplication part (x-xu) that repeats and increases after every loop
temp_f = 0.00		!The total part of (x-xu)*C

do j=1,p	
	temp_f = ((x0-(x_f(j)))*(C_f(j+1)))*temp + temp_f	!READ AS: (x-x1)*C2 + 0  . The first loop temp_f should be 0, because no intermediate result
	temp = (x0-(x_f(j)))*temp							!(x-x1) becomes a temp 
enddo													!-> loop 2 is:	 (x-x2) *  C3  *   (x-x1)  +   Loop 1
														!                (x-xu2)*  C(3) *    temp  +   temp_f 
f_r = temp_c + temp_f						!-> loop 3 is:	 (x-x3) *  C4  *   (x-x1)*(x-x2) + 	Loop 1 + Loop 2
! Total result  =  C(1)  + All (x-xu)*C					!                (x-xu3)*  C(4) *      temp 	 +      temp_f

end function
