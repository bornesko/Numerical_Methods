subroutine bisection_method
    
use glob_variables
    
implicit none
    
integer iteration, index, max_iterations
real tolerance, left_bound, right_bound, midpoint
real f_left, f_right, f_mid
real f_function

! Define the tolerance level (error threshold)
tolerance = 0.01

! Define a maximum number of iterations to prevent infinite loops
max_iterations = 1000  

iteration = 0

! Loop through each pair of consecutive values in the array y
do index = 1, n-1
    ! Check if there is a root in the interval [y(index), y(index+1)]
    if (f_function(y(index)) * f_function(y(index+1)) .lt. 0) then
    ! Set initial boundary values
    left_bound = y(index)
    right_bound = y(index+1)
    f_left = f_function(left_bound)
    f_right = f_function(right_bound)

    ! Initialize midpoint function value with a large number
        f_mid = tolerance + 1  

        ! Start the Bisection method loop
        do while (abs(f_mid) .gt. tolerance .and. iteration .lt. max_iterations)
        ! Compute the midpoint of the current interval
        midpoint = (left_bound + right_bound) / 2
        f_mid = f_function(midpoint)  ! Evaluate function at the midpoint

        ! Update the interval based on function signs
        if (f_left * f_mid .gt. 0) then
            left_bound = midpoint
            f_left = f_mid
        else
            right_bound = midpoint
            f_right = f_mid
        endif

                iteration = iteration + 1
        enddo

            ! Print the root and the number of iterations
        write(*,*) "Root found at:", midpoint
        write(*,*) "Iterations:", iteration
    else
            ! If no root is found in the interval, display a message
        write(*,*) "No root between:", y(index), y(index+1)
    endif
enddo

end subroutine bisection_method
