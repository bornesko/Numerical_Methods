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


! Subroutine for calculating the inverse of a matrix 
!**************************************************************************!
!**************************************************************************!

subroutine inverse_matrix(A,inv_A,n)

implicit none

real	A(n,n)			! matrix for which the inverse is calculated
real	inv_A(n,n)		! inverse of matrix A
real	sub_A(n-1,n-1)	! sub-matrix of matrix A
real	det				! determinant of matrix A
real	sub_det			! determinant of sub-matrix sub_A
integer	n				! size of matrix A
integer	i,j,k,l			! loop indexes

! Calling the subroutine for calculating the determinant of A
!**************************************************************************!
call determinant(A,n,det)

! Calculating the elements of the inverse matrix
!**************************************************************************!
do i=1,n
		do j=1,n
				do k=1,n-1													! assignment of sub_A with the elements:
						do l=1,n-1
								if(k.lt.j.and.l.lt.i)then					! - left and above the position (j,i)
										sub_A(k,l)=A(k,l)					!
								else if(k.lt.j.and.l.ge.i)then				! - right and above the position (j,i)        
										sub_A(k,l)=A(k,l+1)					!
								else if(k.ge.j.and.l.lt.i)then				! - left and below the position (j,i)        
										sub_A(k,l)=A(k+1,l)					!                                        
								else										! - right and below the position (j,i)        
										sub_A(k,l)=A(k+1,l+1)				!        
								endif
						enddo
				enddo
        
				call determinant(sub_A,n-1,sub_det)							! calculating the determinant of sub_A
        
                inv_A(i,j)=(((-1)**(i+j))*(sub_det))/det					! calculating the elements (i,j)
        
		enddo
enddo

end subroutine

