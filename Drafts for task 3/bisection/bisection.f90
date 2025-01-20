subroutine bisection

use glob_variables	

implicit none

integer i,j
real error
real a_bi,b_bi,t_bi
real fa_bi,fb_bi,ft_bi,f_s

i=0
error=0.01
a_bi=y(1)
b_bi=y(2)
fa_bi = f_s(a_bi)
fb_bi = f_s(b_bi)
ft_bi=error+1

do j=1,n-1
if (y(j)*y(j+1).lt.0) then
a_bi=y(j)
b_bi=y(j+1)
do while(abs(ft_bi).gt.error)
		t_bi=(a_bi+b_bi)/2
		ft_bi=f_s(t_bi)
		if(fa_bi*ft_bi.gt.0) then
			a_bi=t_bi
			fa_bi=ft_bi
		else
			b_bi=t_bi
			fb_bi=ft_bi
			
		endif
		
		i=i+1
enddo
else
write(*,*) 'no root between:', y(j),y(j+1)
endif
ENDDO

write(*,*) (a_bi+b_bi)/2
write(*,*) i

read(*,*)

end subroutine
		