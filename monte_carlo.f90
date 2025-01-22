subroutine monte_carlo
use glob_variables

implicit none

!Variables for the subroutine
real f_r, f_s 				!The functions
real f_mc, final_f			!The monte_carlo sum and the final result
real length_x, length_y		!The length between x1 and xn and between y1 and yn
real x_mc, y_mc				!The monte_carlo input variables for x and y
real temp_x, temp_y			!Temporary variables to generate random_number
integer i,j					!Integers
integer points				!Number of simulated points

!Calculation of the length between x1 and xn and between y1 and yn
length_x = x(n) - x(1)		
length_y = y(n) - y(1)

!Opening file to store the results
open(10,file='task_2/convergence.txt')

!Number of starting points
points = 100

do														
	f_mc = 0									!We start with 0, because we look for the SUM of all points. Hence the first loop will give a result and there are no result beforehand
														!
	do i=1,points+1								!A loop to calculate the monte_carlo
		call random_number(temp_x)				!Simulating random points x
		call random_number(temp_y)				!Simulating random points y
		x_mc = temp_x * length_x + x(1) 		!Placing the random x in the domain of integration
		y_mc = temp_y * length_y + y(1)			!Placing the random y in the domain of integration
		f_mc = f_r(x_mc) * f_s(y_mc) + f_mc		!Calculating f(x,y) = f(x)*f(y) with the random points and SUM each result
	enddo
	final_f = (f_mc/points)*length_x*length_y	!Final Integration Volume = Height /the f(x,y) result/ * Area /length_x * length_y/
	write(10,"(I10,F10.2)") points, final_f		!Store the N of points and the result in the convergence.txt
	
	if(points.gt.10000000) exit					!REPEAT THE WHOLE LOOP IF THE POINTS ARE LESS THAN 10000000
	
	points = points*2							!INCREASE THE AMOUNT OF POINTS BY 2
	
enddo

close(10)

!Creating a file for a reference line for the visual representation
open(11,file='task_2/reference.txt')
write(11,"(I10,F10.2)") 64, final_f				!The first value starts at 64, because of the logscale. Our graph starts from 64 onwards
write(11,"(I10,F10.2)") points, final_f			!The last valuse 
close(11)

end subroutine
