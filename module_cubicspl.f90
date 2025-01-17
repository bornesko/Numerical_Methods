module glob_variables

!Global variables for the newton interpolation
integer p
real, allocatable:: C_f(:), x_f(:)
!!!!!!!!!!!!!!!!!

!Global variables for the cubic spline
real an,cn,bn,dn,yn	! global variables
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module glob_variables