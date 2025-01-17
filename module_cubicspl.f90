module glob_variables

!Global variables for the newton interpolation

real, allocatable:: C_f(:), x(:)
!!!!!!!!!!!!!!!!!

!Global variables for the cubic spline
real an,cn,bn,dn,yn	! global variables
real,allocatable::	a_cs(:),c_cs(:),d_cs(:),b_cs(:)
real, allocatable:: y(:)
integer n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

end module glob_variables