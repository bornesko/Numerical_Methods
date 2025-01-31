subroutine bisection

use glob_variables	

implicit none

integer iter,i

real a_bi,b_bi,t_bi
real fa_bi,fb_bi,ft_bi,f_s



open(10,file='task_3/01_bisection_roots.txt')


do i=1,roots
	
	iter=0
	a_bi=a_root(i)
	b_bi=b_root(i)
	fa_bi = f_s(a_bi)
	fb_bi = f_s(b_bi)
	ft_bi=error_root+1

do while(abs(ft_bi).gt.error_root)
	
				t_bi=(a_bi+b_bi)/2
				ft_bi=f_s(t_bi)
				if(fa_bi*ft_bi.gt.0) then !defines next a and b from testing if there was a change of sign
					a_bi=t_bi
					fa_bi=ft_bi
				else
					b_bi=t_bi
					fb_bi=ft_bi
					
				endif
				
				iter=iter+1
		enddo
		write(10,*) (a_bi+b_bi)/2,f_s((a_bi+b_bi)/2),iter   !once a root is found, it writes in the file the y value, 
														!z value (for verification, can be deleted in final version), 					
														!and how many iterations			
	
	
enddo

close(10)




end subroutine
		