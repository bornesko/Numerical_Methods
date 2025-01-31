!Main Program file, where every Subroutine and function converges

program main_program

use glob_variables

implicit none

!Variables
real, allocatable::  z_r(:), z_s(:) 	!Variables for X and fx, that should be read from a .txt file
integer i, j							!Loop Variable
real temp_1, temp_2						!Temporary values for the ordering loop
real,allocatable:: temp_y(:), temp_fs(:) !Temporary values for root finder


! Assigning Plot points
plot_points=100

open(10,file='input_points.txt')
read(10,*) n									!Read the size from the first line of the .txt file
read(10,*)										!Skipping the headings "x" and "fx" in the .txt file

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

!Loop to order values from lowest to biggest -> X and Z_R
do i=1,n
	do j=1,n
		if(x(i).lt.x(j)) then
			temp_1 = x(j)
			temp_2 = z_r(j) 
			x(j) = x(i)
			z_r(j) = z_r(i)
			x(i) = temp_1
			z_r(i) = temp_2
		endif
	enddo
enddo

!Loop to order values from lowest to biggest -> Y and Z_S
do i=1,n
	do j=1,n
		if(y(i).lt.y(j)) then
			temp_1 = y(j)
			temp_2 = z_s(j) 
			y(j) = y(i)
			z_s(j) = z_s(i)
			y(i) = temp_1
			z_s(i) = temp_2
		endif
	enddo
enddo			

!Call function X
!write(*,*) y, z_s
!read(*,*)

call newton_interpolation(z_r)

!Call function Y

call cubic_spline(z_s)

open(11,file='task_1/x_points.txt')
do i=1,n
	write(11,*) x(i),z_r(i)
enddo
close(11)

open(11,file='task_1/y_points.txt')
do i=1,n
	write(11,*) y(i),z_s(i)
enddo
close(11)

! gnuplot code
open(14,file='task_1/plot_task01.plt')
write(14,*) 'set multiplot layout 2,1 rowsfirst'
write(14,*) 'set label 1 ""'
write(14,*) 'set title "Newton Interpolation"'
write(14,*) 'set xlabel "x [U]"'								
write(14,*) 'set ylabel "z_r [U]"'
write(14,*) 'set xzeroaxis'
write(14,*) 'set yzeroaxis'
write(14,*) 'set style line 1 lt rgb "#DC143C" pt 7'
write(14,*) 'set xrange [',x(1),':',x(n),']'					!https://gnuplotting.org/tag/multiplot/index.html
write(14,*) 'plot "task_1/data_f_r.txt" with line lt rgb "#00008b" title "Newton Interpolation ",' // & 
			' "task_1/x_points.txt" using 1:2:(sprintf("(%g,%g)",$1,$2)) with labels offset 1,1 font ",7" point ls 1 notitle'
write(14,*) 'set label 2 ""'
write(14,*) 'set title "Cubic Spline"'
write(14,*) 'set xlabel "y [U]"'
write(14,*) 'set ylabel "z_s [U]"'
write(14,*) 'set xzeroaxis'
write(14,*) 'set yzeroaxis'
write(14,*) 'set style line 2 lt rgb "#DC143C" pt 7'
write(14,*) 'set xrange [',y(1),':',y(n),']'								
write(14,*) 'plot "task_1/data_f_s.txt" with line lt rgb "#00008b" title "Cubic Spline ",' // &
			' "task_1/y_points.txt" using 1:2:(sprintf("(%g,%g)",$1,$2)) with labels offset 1,1 font ",7" point ls 2 notitle'
!write(14,*) 'plot "task_1/data_f_s.txt" with lines lt rgb "#00008b", "task_1/y_points.txt" with points ls 2'
write(14,*) 'unset multiplot'
write(14,*) 'pause -1 "Hit return to continue"'

close(14)														

call system('binary\wgnuplot task_1\plot_task01.plt')
 
!Call Monte Carlo

call monte_carlo

open(10,file='task_2/plot_task02.plt')
write(10,*) 'set title "Convergence"'
write(10,*) 'set logscale x 2'
write(10,*) 'set xlabel "Number of Points [n]"'								
write(10,*) 'set ylabel "Integration [U^3]"'
write(10,*) 'set style line 1 lt rgb "#DC143C" lw 2'
write(10,*) 'set style line 2 lt rgb "#00008b" dt 3 lw 2'
write(10,*) 'plot "task_2/convergence.txt" using 1:2:(sprintf("(%g)",$2)) with labels offset 1,1 point pt 2 notitle,' // &
			' "task_2/convergence.txt" with lines ls 2 title "Convergence Behaviour ",' // & 
			' "task_2/reference.txt" using 1:2:(sprintf("(%g)",$2)) with labels offset -1,-1 notitle,' // &
			' "task_2/reference.txt" with lines ls 1 title "Reference "'
write(10,*) 'pause -1 "Hit return to continue"'

close(10)														

call system('binary\wgnuplot task_2\plot_task02.plt')

!Call Bisection

allocate(temp_y(plot_points+1))	!Assigning size
allocate(temp_fs(plot_points+1)) 
allocate(a_root(plot_points))
allocate(b_root(plot_points))

open(10,file='task_1/data_f_s.txt')
do i =1,plot_points+1
	read(10,*) temp_y(i), temp_fs(i)	!Reading the results from task_1/data_f_s
enddo
close(10)

roots=0
error_root= 0.01

do i=1,plot_points
	if(temp_fs(i)*temp_fs(i+1).le.0.00) then	!Finding a change of sign
	roots=roots+1								!Root is present
	a_root(roots)=temp_y(i)					    !Remember the values of y
	b_root(roots)=temp_y(i+1)
	endif
enddo


call bisection

call secant_method

call regula_falsi

end program
