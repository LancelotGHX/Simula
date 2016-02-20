module user_define
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
  !> adding types into the record
  call num_of_mtypes (2)
  call add_new_mtype (tpyp)
  call add_new_mtype (lead)

  !> define molecule type TPyP
  call idx_def (tpyp, 2000);   !> type id should be within [1000, 9999]
  call eva_num (tpyp, 100);    !> evaporation number
  call dot_num (tpyp, 5);      !> number of components
  call dot_pos (tpyp, 0, 0, 1) !> xpos, ypos, state
  call dot_pos (tpyp, 1, 0, 2) !> xpos, ypos, state
  call dot_pos (tpyp, 0, 1, 2) !> xpos, ypos, state
  call dot_pos (tpyp,-1, 0, 2) !> xpos, ypos, state
  call dot_pos (tpyp, 0,-1, 2) !> xpos, ypos, state
  !> define molecule type Lead
  call idx_def (lead, 1000);   !> type id should be within [1000, 9999]
  call eva_num (lead, 100);    !> evaporation number
  call dot_num (lead, 1);      !> number of components
  call dot_pos (lead, 0, 0, 3) !> xpos, ypos, state

  !> testing
  call init_mlist()
  print *, tlist(1) % ptr %  idx_def
  print *, tlist(2) % ptr %  idx_def

  return
end subroutine

end module user_define
