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
!> @brief Function to generate random integer
!> @param[in] sint: lower bound
!> @param[in] eint: upper bound
!> @return integer random number
!--------------------------------------------------------------------------- 
function rand_int(sint, eint)
  integer,intent(IN)::sint, eint
  integer::rand_int
  real(8)::r
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
  real(8),intent(IN)::spnt, epnt
  real(8)::r, rand_uniform
  call random_number(r)
    rand_uniform = r*(epnt-spnt) + spnt
  return
end function rand_uniform

end module helper_functions
