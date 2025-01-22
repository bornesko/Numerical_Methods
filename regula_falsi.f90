subroutine regula_falsi

use glob_variables

implicit none

real				f_s
real				temp_a,temp_b,y_new,a_rf,b_rf,temp_c
real,allocatable::	y_rf(:),fs_rf(:)
integer				iter,count
integer				i,j,k

iter=101 !Value coming from the total number of simulated points in cubic_spline

Allocate(y_rf(iter))
Allocate(fs_rf(iter))

open(60,file='task_1/data_f_s.txt')
do i=1,101
	read(60,*) y_rf(i),fs_rf(i)
enddo
close(60)

count=0
open(61,file='task_3/potential_roots.txt')
do i=1,iter-2
	temp_a=(fs_rf(i))/abs(fs_rf(i))
	temp_b=(fs_rf(i+1))/abs(fs_rf(i+1))

!	temp_a=(fs_rf(i+1)-fs_rf(i))/abs(fs_rf(i+1)-fs_rf(i))
!	temp_b=(fs_rf(i+2)-fs_rf(i+1))/abs(fs_rf(i+2)-fs_rf(i+1))
!	write(*,*) temp_a,temp_b
!	read(*,*)
	if (temp_a.ne.temp_b) then
	write(61,*) i
	count=1+count
!	else !stalled
!	write(*,*) "no roots"
	endif
enddo
close(61)

open(61,file='task_3/potential_roots.txt')

do k=1,count
	read(61,*) i
	a_rf=y_rf(i)
	b_rf=y_rf(i+1)
!	write(*,*) a_rf,b_rf
	do j=1,10
		y_new=(a_rf)-((b_rf-a_rf)/(f_s(b_rf)-f_s(a_rf)))*(f_s(a_rf))
		if ((f_s(a_rf)*f_s(y_new)).gt.0) then
			a_rf=y_new
			b_rf=b_rf
		else if ((f_s(b_rf)*f_s(y_new)).gt.0) then
			a_rf=a_rf
			b_rf=y_new
		else
			write(*,*) "error"
		endif
!		write(*,*) y_new
	enddo

	write(*,*) y_new, f_s(y_new)

enddo
close(61)

read(*,*)

end subroutine