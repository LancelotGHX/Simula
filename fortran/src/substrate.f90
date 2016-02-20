!---------------------------------------------------------------------------  
!
! DESCRIPTION: 
!> @brief Define simulation substrate
!
!> @type substrate
!
!> @function substrate_init
!> @function substrate_set
!> @function substrate_get
!
!--------------------------------------------------------------------------- 
module substrate

use user_define
implicit none

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief substrate static variable
!--------------------------------------------------------------------------- 
integer, save :: self (XSIZE,YSIZE)

contains

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Initialize a substrate
!> @param self: substrate structure
!--------------------------------------------------------------------------- 
subroutine substrate_init()
  self = 0
  return
end subroutine substrate_init

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Access substrate point at (x,y)
!> @param self: substrate structure
!> @param x: x coordinate
!> @param y: y coordinate
!--------------------------------------------------------------------------- 
elemental function get_sub( x, y)
  integer             :: get_sub
  integer, intent(in) :: x, y
  get_sub = self ( modulo(x-1,XSIZE) + 1, modulo(y-1,YSIZE) + 1 )
  return 
end function get_sub

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Assign substrate point at (x,y)
!> @param self: substrate structure
!> @param x: x coordinate
!> @param y: y coordinate
!> @param v: value
!--------------------------------------------------------------------------- 
subroutine set_sub( x, y, v)
  integer, intent(in) :: x, y, v
  self ( modulo(x-1,XSIZE) + 1, modulo(y-1,YSIZE) + 1 ) = v
  return
end subroutine set_sub

end module substrate
