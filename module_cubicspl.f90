module glob_variables

!Number of plotted points
integer plot_points

!Global variables for the newton interpolation

real, allocatable:: C_f(:), x(:)
!!!!!!!!!!!!!!!!!

!Global variables for the cubic spline
real an,cn,bn,dn,yn	! global variables
real,allocatable::	a_cs(:),c_cs(:),d_cs(:),b_cs(:)
real, allocatable:: y(:)
integer n
!!!!!!!!!!!!!!!!!!!!!!!!!!!!

real,allocatable:: a_root(:),b_root(:)

end module glob_variables