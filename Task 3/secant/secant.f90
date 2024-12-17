
program secant

implicit none

real a,b
real error

integer i,max_iter
real, allocatable :: x(:), fx(:)


error=0.01
a=-2.0
b=2.0

max_iter = 1000                    ! Set a maximum number of iterations
allocate(x(0:max_iter))            ! Allocate arrays with a sufficient size
allocate(fx(0:max_iter))
i=1
x(0)=a
x(1)=b
fx(0) = x(0)**5 + (x(0) - 2) * sin(x(0)) + (x(0) - 1)
fx(1) = x(1)**5 + (x(1) - 2) * sin(x(1)) + (x(1) - 1)

do while (abs(fx(i)) > error .and. i < max_iter)
	x(i+1)=x(i)-((x(i)-x(i-1))/(fx(i)-fx(i-1)))*fx(i)
	fx(i+1) = x(i+1)**5 + (x(i+1) - 2) * sin(x(i+1)) + (x(i+1) - 1)
	i=i+1
enddo

 if (i == max_iter) then
        write(*,*) "Secant method did not converge within the maximum iterations."
    else
        write(*,*), "Root:", x(i)
        write(*,*), "Iterations:", i-1
    end if

read(*,*)
end program
	
