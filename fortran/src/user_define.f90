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
  call dot_pos (tpyp, 0, 0, 1) !> xpos, ypos, comp
  call dot_pos (tpyp, 1, 0, 2) !> xpos, ypos, comp !> reaction 2->4
  call dot_pos (tpyp, 0, 1, 3) !> xpos, ypos, comp !> reaction 3->5
  call dot_pos (tpyp,-1, 0, 2) !> xpos, ypos, comp
  call dot_pos (tpyp, 0,-1, 3) !> xpos, ypos, comp
  
  !------------------------------------------------
  ! testing, will be clean up if it works
  call reaction_num (tpyp, 8) ! 7 free movement + 1 chem reaction

  ! how to check one condition
  ! 1) check if one of the relative positions matches: cond->pos
  ! 2) check if one of the relative direction matches: cond->dir
  ! 3) check if ALL components' initial states match : cond->state
  ! pass the checking and do movement and update component state
  call beg_reaction (tpyp, 2) ! 2 condition
    call set_react_info (0, 0.0, [ 0,0,0 ])
    call set_react_cond_type  (1, 2000) ! the type of itself 
    call set_react_cond_pos   (1, [ 0,0     ]) 
    ! REMARK: molecule cetner pos {x1,y1, x2,y2, ...}
    ! REMARK: it is always zero [0,0] sicne there is no relative distance
    !   with respect to molecule itself    
    call set_react_cond_dir   (1, [ 0       ]) 
    ! REMARK: molecule relative direction {d1,d2,d3,...}
    ! REMARK: again always zero for the first condition
    call set_react_cond_state (1, [ 2,0,1   ]) 
    ! REMARK: state changes {c, state_i, state_f, ...}
    call set_react_cond_type  (2, 2000) ! target molecule
    call set_react_cond_pos   (2, [ 3,0 ])
    call set_react_cond_dir   (2, [ 0,2   ])
    call set_react_cond_state (2, [ 2,0,1 ])
  call end_reaction ()
  ! ... repeat 4 times for four directions      =.=
  ! ... of course you can write a loop to do so =.=

  call beg_reaction (tpyp, 1) ! 1 condition just a movement
    call set_react_info (0, 0.0, [ 1,0,0 ]) ! move left for one step
    call set_react_cond_type  (1, 2000)     
    call set_react_cond_pos   (1, [ 0,0 ]) 
    call set_react_cond_dir   (1, [ 0   ]) ! 
    call set_react_cond_state (1, [ 1,0,0, 2,0,0, 3,0,0 ])
    ! REMARK: one molecule cannot move when it has any bonds. state zero means
    !   there is no bond. so the initial state should always be zero
  call end_reaction ()
  ! ... again you need to repead those lines =.= i'am sorry

  !------------------------------------------------
  !
  !> define molecule type Lead
  !
  call symm    (lead, 1)       !> define symmetry
  call idx_def (lead, 1000)    !> type id should be within [1000, 9999]
  call eva_num (lead, 100)     !> evaporation number
  call dot_num (lead, 1)       !> number of components
  call dot_pos (lead, 0, 0, 4) !> xpos, ypos, comp
  !
  ! ends here
  !--------------------------------------------------

  !> Initialize molecules
  call init_mlist()
  return
end subroutine

end module user_define
