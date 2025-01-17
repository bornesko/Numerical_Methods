subroutine monte_carlo
use glob_variables

implicit none

real f_r, f_s 
real f_mc, final_f
real length_x, length_y
real x_mc, y_mc
real temp_x, temp_y
integer i,j
integer points

length_x = x(n) - x(1)	
length_y = y(n) - y(1)

open(10,file='task_2/convergence.txt')

points = 10

do
	f_mc = 0

	do i=1,points+1
		call random_number(temp_x)
		call random_number(temp_y)
		x_mc = temp_x * length_x + x(1) 
		y_mc = temp_y * length_y + y(1)
		f_mc = f_r(x_mc) * f_s(y_mc) + f_mc
	enddo
	final_f = (f_mc/points)*length_x*length_y
	write(10,*) points, final_f
	
	points = points*2
	
	if(points.gt.10000000) exit
	
enddo

close(10)

end subroutine
