subroutine regula_falsi

use glob_variables

implicit none

real				f_s
real				temp,y_rf,a_rt,b_rt
real,allocatable::	temp_y(:),temp_fs(:)
integer				i,j,max_iter

max_iter=1000						!

open(61,file='task_3/03_regula_falsi_roots.txt')
do i=1,roots
	j=1
	temp=1
	a_rt=a_root(i)
	b_rt=b_root(i)
	do while (abs(temp).gt.error_root.and.j.lt.max_iter)
		y_rf=(a_rt)-((b_rt-a_rt)/(f_s(b_rt)-f_s(a_rt)))*(f_s(a_rt))
		if ((f_s(a_rt)*f_s(y_rf)).gt.0) then
			a_rt=y_rf
			b_rt=b_rt
		else if ((f_s(b_rt)*f_s(y_rf)).gt.0) then
			a_rt=a_rt
			b_rt=y_rf
		else
			write(61,*) "error"
		endif
	j=j+1
	temp=f_s(y_rf)
	enddo

	write(61,*) y_rf, f_s(y_rf), j

enddo
close(61)

end subroutine