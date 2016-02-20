!---------------------------------------------------------------------------  
! DESCRIPTION
!> this is a module should be handle by user for defining all constant
!  variables, molecule structures and chemical reactions
!---------------------------------------------------------------------------  
module user_define

use helper_functions
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
!> define molecule type here
!---------------------------------------------------------------------------  
type (mtype), save :: tpyp
type (mtype), save :: lead

contains

!---------------------------------------------------------------------------  
! DESCRIPTION
!> initialization
!---------------------------------------------------------------------------  
subroutine init ()
  
  call init_random_seed() !> initialize random seed

  !--------------------------------------------------
  ! handled by user
  !
  !> adding types into the record
  call num_of_mtypes (2)
  call add_new_mtype (tpyp)
  call add_new_mtype (lead)
  !
  !> define molecule type TPyP
  !
  call symm    (tpyp, 4)       !> define symmetry
  call idx_def (tpyp, 2000)    !> type id should be within [1000, 9999]
  call eva_num (tpyp, 100)     !> evaporation number
  call dot_num (tpyp, 5)       !> number of components
  call dot_pos (tpyp, 0, 0, 1) !> xpos, ypos, state
  call dot_pos (tpyp, 1, 0, 2) !> xpos, ypos, state !> reaction 2->4
  call dot_pos (tpyp, 0, 1, 3) !> xpos, ypos, state !> reaction 3->5
  call dot_pos (tpyp,-1, 0, 2) !> xpos, ypos, state
  call dot_pos (tpyp, 0,-1, 3) !> xpos, ypos, state
  
  !------------------------------------------------
  ! testing, will be clean up if it works
  typt % mov_pos (1,:) = (/0,0,1/)
  typt % mov_pos (2,:) = (/0,0,2/)
  typt % mov_pos (3,:) = (/0,0,3/)
  typt % mov_pos (4,:) = (/1,0,0/)
  typt % mov_pos (5,:) = (/0,1,0/)
  typt % mov_pos (6,:) = (/-1, 0,0/)
  typt % mov_pos (7,:) = (/0 ,-1,0/)

  tpyp % react_num = 1
  allocate ( tpyp % react(1) )
  allocate ( tpyp % react(1) % fill(2) ) !> one self, one target
  ! response from molecule itself
  tpyp % react(1) % fill(1) % type_id = 0 !< indicating it's itself
  allocate (tpyp % react(1) % fill(1) % comp_id (2)) !> component 2 & 4
  allocate (tpyp % react(1) % fill(1) % pos (3,2))
  allocate (tpyp % react(1) % fill(1) % mov (3,2))
  tpyp % react(1) % fill(1) % comp_id = (/2,4/)
  tpyp % react(1) % fill(1) % pos = 0               !> self reactant
  tpyp % react(1) % fill(1) % mov (:,1) = (/ 1,0,0/)
  tpyp % react(1) % fill(1) % mov (:,2) = (/-1,0,0/)
  tpyp % react(1) % fill(1) % state_i = 2
  tpyp % react(1) % fill(1) % state_f = 4
  ! response from other molecule
  tpyp % react(1) % fill(2) % type_id = 2000 !< also tpyp
  allocate (tpyp % react(1) % fill(2) % comp_id (2)) !> component 2 & 4
  allocate (tpyp % react(1) % fill(2) % pos (3,2))
  allocate (tpyp % react(1) % fill(2) % mov (3,2))
  tpyp % react(1) % fill(2) % comp_id = (/2,4/)
  tpyp % react(1) % fill(2) % mov = 0               !> no movement for target
  tpyp % react(1) % fill(2) % pos(:,1) = (/ 3,0,0/)
  tpyp % react(1) % fill(2) % pos(:,2) = (/-3,0,0/)
  tpyp % react(1) % fill(2) % state_i = 2
  tpyp % react(1) % fill(2) % state_f = 4
  !------------------------------------------------

  !
  !> define molecule type Lead
  !
  call symm    (lead, 1)       !> define symmetry
  call idx_def (lead, 1000)    !> type id should be within [1000, 9999]
  call eva_num (lead, 100)     !> evaporation number
  call dot_num (lead, 1)       !> number of components
  call dot_pos (lead, 0, 0, 4) !> xpos, ypos, state
  !
  ! ends here
  !--------------------------------------------------

  !> Initialize molecules
  call init_mlist()
  return
end subroutine

end module user_define
