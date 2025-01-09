!**************************************************************************!
!**************************************************************************!
!***********                                                    ***********!
!***********            Fortran Programming Example             ***********!
!***********                                                    ***********!
!**************************************************************************!
!**************************************************************************!
!* The examples are for teaching purposes in Faculty of Civil Engineering *!
!* of Technische Universität Dresden and are without any guarantee of ac- *!
!* curacy, completeness and/or functionality. The usage is at the user's  *!
!* own risk.                                                              *!
!**************************************************************************!
!**************************************************************************!


! Subroutine for calculating the determinant 
!**************************************************************************!
!**************************************************************************!

subroutine determinant(A,n,det)

implicit none

real	A(n,n)		! matrix for which the determinant is calculated
real    A_tria(n,n) ! matrix A in triangular form
real    det         ! determinant of matrix A i.e. A_tria
real    m,temp      ! temporary variables
integer	n           ! size of matrix A
integer	i,j,k		! loop indexes
logical	det_exist   ! termination variable

! Default values for the variables
!**************************************************************************!
det_exist=.true.
det=1
A_tria=A

! Conversion of matrix A in triangular form (without pivoting)
!**************************************************************************!
do k=1,n-1
		if(A_tria(k,k).eq.0) then										   ! if the actual diagonal element is = 0,
                                                                           ! then the existence of the determinant is 
                det_exist=.false.                                                           ! initially suspended and the next 
                do i=k+1,n                                                 ! row is searched in which the 
                        if(A_tria(i,k).ne.0) then                          ! corresponding element is |= 0,
                                do j=1,n                                   ! in order to swap this row with row k
                                        temp=A_tria(i,j)                   !
                                        A_tria(i,j)=A_tria(k,j)            ! swapping rows i and k
                                        A_tria(k,j)=temp                   !
								enddo
                                det_exist=.true.                                           ! doubt about the existance of 
                                                                           ! the determinant is eliminated
                                det=-det                                   ! change of sign of the determinant
                                                                           ! due to row swapping
                                exit                                       ! premature termination of the loop
						endif
				enddo
                if(det_exist.eqv..false.) then                             ! all elements of the column k are null,
                        det=0.0                                            ! it means that det(A)=0
                        return                                             ! return to the calling (sub-)program
				endif
		endif

		do j=k+1,n
                m=A_tria(j,k)/A_tria(k,k)                                  ! subtracting the k-th row (with
                do i=k+1,n                                                   ! factor m) from the remaining rows
                        A_tria(j,i)=A_tria(j,i)-m*A_tria(k,i)                   ! (= diagonalization)
				enddo		
		enddo
enddo

! Calculation of the determinant as a product of the diagonal elements
!**************************************************************************!
do i=1,n
		det=det*A_tria(i,i)
enddo

end subroutine        
