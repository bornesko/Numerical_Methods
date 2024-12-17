program bisection

implicit none

integer i
real error
real a,b,t
real fa,fb,ft

i=0
error=0.01
a=-2.0
b=2.0
fa = a**5 + (a - 2) * sin(a) + (a - 1)
fb = b**5+(b-2)*sin(b)+(b-1)
ft=error+1

do while(abs(ft).gt.error)
		t=(a+b)/2
		ft=t**5 + (t - 2) * sin(t) + (t - 1)
		if(fa*ft.gt.0) then
			a=t
			fa=ft
		else
			b=t
			fb=ft
			
		endif
		
		i=i+1
enddo

write(*,*) (a+b)/2
write(*,*) i

read(*,*)

end program
		