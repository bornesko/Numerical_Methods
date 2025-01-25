subroutine secant_method

use glob_variables

implicit none

real f_s																!Function
real,allocatable:: temp_y(:), temp_fs(:), a_sec(:), b_sec(:), y_sec(:)	!temp y and fs to read the txt file. a and b for the domain. y for the calculations
real error, temp														!Error and temporary values
integer i, j, max_iter, roots											!Integers + iteration value and roots values

error = 0.01		!Error
max_iter = 1000		!Maximum iterations
roots=0				!How many roots

allocate(temp_y(120))	!Assigning size
allocate(temp_fs(120)) 
allocate(a_sec(100))
allocate(b_sec(100))
allocate(y_sec(0:max_iter))	

a_sec(1)=y(1)	!First value
b_sec(0)=y(1)

open(10,file='task_1/data_f_s.txt')
do i =1,101
	read(10,*) temp_y(i), temp_fs(i)	!Reading the results from task_1/data_f_s
enddo
close(10)

do i=1,100
	if(temp_fs(i)/temp_fs(i+1).lt.0.00) then	!Finding a change of sign
	roots=roots+1								!Root is present
	a_sec(roots)=temp_y(i)					    !Remember the values of y
	b_sec(roots)=temp_y(i+1)
	endif
enddo

write(*,*) 'SECANT METHOD'
write(*,*) 'Expected roots:', roots
read(*,*)

do i=1, roots
	j = 1
	temp = 1
	y_sec(j-1)=a_sec(i)
	y_sec(j)=b_sec(i)
	
	do while(abs(temp).gt.error .and. j.lt.max_iter)
	
		y_sec(j+1)=y_sec(j)-(((y_sec(j)-y_sec(j-1))/(f_s(y_sec(j))-f_s(y_sec(j-1))))*f_s(y_sec(j)))
		
		j = j + 1
		temp = f_s(y_sec(j))
		
	enddo
	
	write(*,*) 'Root:', y_sec(j), 0.00, 'Iterations:', j
	
enddo

end subroutine