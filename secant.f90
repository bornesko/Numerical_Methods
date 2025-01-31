subroutine secant_method

use glob_variables

implicit none

real f_s																!Function
real,allocatable:: a_sec(:), b_sec(:), y_sec(:)	!temp y and fs to read the txt file. a and b for the domain. y for the calculations
real  temp														!error_root and temporary values
integer i, iter, max_iter											!Integers + iteration value and roots values


max_iter = 1000		!Maximum iterations
!roots=0				!How many roots
!allocate(temp_y(plot_points+1))	!Assigning size
!allocate(temp_fs(plot_points+1)) 
!allocate(a_sec(100))
!allocate(b_sec(100))
allocate(y_sec(0:max_iter))	

!open(10,file='task_1/data_f_s.txt')
!do i =1,101
!	read(10,*) temp_y(i), temp_fs(i)	!Reading the results from task_1/data_f_s
!enddo
!close(10)
!
!do i=1,100
!	if(temp_fs(i)/temp_fs(i+1).lt.0.00) then	!Finding a change of sign
!	roots=roots+1								!Root is present
!	a_sec(roots)=temp_y(i)					    !Remember the values of y
!	b_sec(roots)=temp_y(i+1)
!	endif
!enddo

!	a_sec(roots)=temp_y_root(i)					    !Remember the values of y
!	b_sec(roots)=temp_y_root(i+1)
open(10,file='task_3/02_secant_roots.txt')

do i=1, roots
	iter = 1
	temp = 1
	y_sec(iter-1)=a_root(i)
	y_sec(iter)=b_root(i)
	
	do while(abs(temp).gt.error_root .and. iter.lt.max_iter)
	
		y_sec(iter+1)=y_sec(iter)-(((y_sec(iter)-y_sec(iter-1))/(f_s(y_sec(iter))-f_s(y_sec(iter-1))))*f_s(y_sec(iter)))
		
		iter = iter + 1
		temp = f_s(y_sec(iter))
		
	enddo
	
	write(10,*) y_sec(iter), f_s(y_sec(iter)),iter-1
	
	
enddo

close(10)

end subroutine