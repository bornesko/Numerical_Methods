!Main Program file, where every Subroutine and function converges

program main_program

implicit none

!Variables
real, allocatable:: x(:), fx(:), y(:), fy(:) 	!Variables for X and fx, that should be read from a .txt file
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
	read(10,*) x(i), fx(i)			
enddo

read(10,*) !There is an empty space in the .txt, which should be skipped
read(10,*) !Skipping the headings "y" and "fy" in the .txt file

!Assign values for y and fy from the text file
do i=1,n
	read(10,*) y(i), fy(i)			
enddo

close(10)

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