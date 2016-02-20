module constants
use type_define
implicit none

!---------------------------------------------------------------------------  
! DESCRIPTION
!> module substrate
!---------------------------------------------------------------------------  
integer, parameter :: XSIZE = 100
integer, parameter :: YSIZE = 100

!---------------------------------------------------------------------------  
! DESCRIPTION
!> module molecule type
!---------------------------------------------------------------------------  
type (mtype) :: tpyp
type (mtype) :: lead

contains

!---------------------------------------------------------------------------  
! DESCRIPTION
!> initialize type definition
!---------------------------------------------------------------------------  
subroutine init ()

  !> define molecule type TPyP
  call idx_def (tpyp, 2000); !> type id should be within [1000, 9999]
  call dot_num (tpyp, 5);    !> number of components
  call dot_pos (tpyp, 0, 0, 1) !> xpos, ypos, state
  call dot_pos (tpyp, 1, 0, 2) !> xpos, ypos, state
  call dot_pos (tpyp, 0, 1, 2) !> xpos, ypos, state
  call dot_pos (tpyp,-1, 0, 2) !> xpos, ypos, state
  call dot_pos (tpyp, 0,-1, 2) !> xpos, ypos, state

  return
end subroutine

end module constants
