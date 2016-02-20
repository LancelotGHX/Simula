!---------------------------------------------------------------------------  
!
! DESCRIPTION: 
!> Define all the helper functions independent to the simulation project
!
!---------------------------------------------------------------------------  
module helper_functions

implicit none

contains

!---------------------------------------------------------------------------  
! DESCRIPTION: 
!> @brief Function to generate random integer (include boundaries)
!> @param[in] sint: lower bound
!> @param[in] eint: upper bound
!> @return integer random number
!--------------------------------------------------------------------------- 
function rand_int(sint, eint)
  integer,intent(IN) :: sint, eint
  integer :: rand_int
  real(8) :: r
    call random_number(r)
    rand_int = floor(r*(eint-sint+1))+sint
  return
end function rand_int

!---------------------------------------------------------------------------  
! DESCRIPTION: 
!> @brief Function to generate random float
!> @param[in] spnt: lower bound
!> @param[in] epnt: upper bound
!> @return real random number
!--------------------------------------------------------------------------- 
function rand_uniform(spnt, epnt)
  real(8),intent(IN) :: spnt, epnt
  real(8) :: r, rand_uniform
  call random_number(r)
    rand_uniform = r*(epnt-spnt) + spnt
  return
end function rand_uniform

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief rotate vector once by rotational matrix
!> @param mat: matrix
!> @param vec: vector
!
! MATRIX INDEX
! (1,1) (1,2)
! (2,1) (2,2)
!---------------------------------------------------------------------------  
function rotate_once(mat, vec)
  integer, intent(in) :: mat(2,2), vec(2)
  integer :: rotate_once(2)
  rotate_once(1) = vec(1) * mat(1,1) + vec(2) * mat(1,2)
  rotate_once(2) = vec(1) * mat(2,1) + vec(2) * mat(2,2)
  return
end function rotate_once
!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief rotate vector n times by rotational matrix
!> @param mat: matrix
!> @param vec: vector
!> @param n  : number of rotation
!---------------------------------------------------------------------------  
function rotate(mat, vec, n)
  integer, intent(in) :: mat(2,2), vec(2), n
  integer :: tmp(2), rotate(2), i
  tmp = vec
  do i = 1,n
     tmp = rotate_once(mat, tmp)
  end do
  rotate = tmp
  return
end function rotate

!---------------------------------------------------------------------------  
! Description:
!> @reference https://gcc.gnu.org/onlinedocs/gfortran/RAND.html
!
!   Restarts or queries the state of the pseudorandom number generator used
!   by RANDOM_NUMBER.
!
!   If RANDOM_SEED is called without arguments, it is initialized to a default
!   state. The example below shows how to initialize the random seed based on
!   the system's time.
!
! Standard : F95 and later
!
! Class    : Subroutine
!
! Syntax   : CALL RANDOM_SEED(SIZE, PUT, GET)
!
! Arguments:
!     SIZE (Optional) Shall be a scalar and of type default INTEGER, with
!                     INTENT(OUT). It specifies the minimum size of the arrays
!                     used with the PUT and GET arguments.
!     PUT (Optional)  Shall be an array of type default INTEGER and rank one.
!                     It is INTENT(IN) and the size of the array must be larger
!                     than or equal to the number returned by the SIZE argument.
!     GET (Optional)  Shall be an array of type default INTEGER and rank one. 
!                     It is INTENT(OUT) and the size of the array must be larger
!                     than or equal to the number returned by the SIZE argument.
!---------------------------------------------------------------------------  
subroutine init_random_seed()
  integer :: i, n, clock
  integer, dimension(:), allocatable :: seed

  call random_seed(size = n)
  allocate(seed(n))

  call system_clock(count=clock)

  seed = clock + 37 * (/ (i - 1, i = 1, n) /)
  call random_seed(put = seed)

  deallocate(seed)
end subroutine init_random_seed

end module helper_functions
