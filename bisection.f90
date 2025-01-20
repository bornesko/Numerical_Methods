subroutine bisection

use glob_variables	

implicit none

integer i,j
real error
real a_bi,b_bi,t_bi,mult
real fa_bi,fb_bi,ft_bi,f_s

error=0.01

open(10,file='task_3/roots.txt')

do j=1,n-1 
	mult = f_s(y(j))*f_s(y(j+1))

if (mult.le.0.0) then  !checks every interval to see if there is a change of sign
	i=0
	a_bi=y(j)
	b_bi=y(j+1)
	fa_bi = f_s(a_bi)
	fb_bi = f_s(b_bi)
	ft_bi=error+1
	
	if(fa_bi.eq.0) then !checks if f_s(y(i)) is not a root
		write(10,*) fa_bi,f_s(fa_bi),i
	
	else if(fb_bi.eq.0) then
		write(10,*) fb_bi,f_s(fb_bi),i
	
	else 
		do while(abs(ft_bi).gt.error)
			
				t_bi=(a_bi+b_bi)/2
				ft_bi=f_s(t_bi)
				if(fa_bi*ft_bi.gt.0) then !defines next a and b from testing if there was a change of sign
					a_bi=t_bi
					fa_bi=ft_bi
				else
					b_bi=t_bi
					fb_bi=ft_bi
					
				endif
				
				i=i+1
		enddo
		write(10,*) (a_bi+b_bi)/2,f_s((a_bi+b_bi)/2),i   !once a root is found, it writes in the file the y value, 
														!z value (for verification, can be deleted in final version), 					
														!and how many iterations			
	endif
	else !added this for testing, to be deleted in final version
	!write(*,*) 'no root between:', y(j),y(j+1) 
endif
	
enddo

close(10)




end subroutine
		