!Main Program file, where every Subroutine and function converges

program main_program

use glob_variables

implicit none

!Variables
real, allocatable::  z_r(:), z_s(:) 	!Variables for X and fx, that should be read from a .txt file
										!Number of Points (first line of the .txt file)
integer i										!Loop Variable

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


!Call function X

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

call bisection

call secant_method

call regula_falsi

end program
