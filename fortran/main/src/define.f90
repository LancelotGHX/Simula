!---------------------------------------------------------------------------  
! DESCRIPTION
!> this is a module should be handle by user for defining all constant
!  variables, molecule structures and chemical reactions
!---------------------------------------------------------------------------  
module define
  
  use class_mtype
  use class_molecule
  use func_helper
  use func_substrate
  use func_rate_kmc
  implicit none

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> define molecule type here
  !---------------------------------------------------------------------------  
  type (mtype), save, pointer :: tpyp
  type (mtype), save, pointer :: lead

contains

  subroutine def_free_move(t_obj, r, move, energy)
    type(mtype), pointer, intent (inout) :: t_obj
    integer             , intent (in) :: r 
    integer             , intent (in) :: move(3)
    real(dp)            , intent (in) :: energy

    integer, allocatable :: sta (:), pos (:)
    integer, allocatable :: tmp(:,:)
    integer, pointer     :: state_initial(:) => null()
    integer              :: i, j
    state_initial => t_obj % state_initial()

    ! new-x, new-y, pos-not-overlap, original-x, original-y
    call alloc(tmp, 5, t_obj % comp_num())

    tmp = 1
    do i = 1, t_obj % comp_num()
       tmp(1:2,i) = t_obj % translate(t_obj % comp(i), move)
       ! check overlapping
       do j = 1, t_obj % comp_num()
          tmp(4:5,j) = t_obj % xy(j)
          if ( all( tmp(1:2,i) == tmp(4:5,j) ) ) tmp(3,i) = 0
       end do
    end do

    call alloc_I1(pos, sum(tmp(3,:)) * 3)
    call alloc_I1(sta, size(tmp, 2)  * 2)

    j = 1
    do i = 1, t_obj % comp_num()
       if (tmp(3,i) == 1) then
          pos(3*j-2) = tmp(1,i)
          pos(3*j-1) = tmp(2,i)
          pos(3*j  ) = 0
          j = j + 1
       end if
       sta(2*i-1) = state_initial(i) 
       sta(2*i  ) = state_initial(i)
    end do
            
    ! basic information
    call t_obj % reac (r) % set_energy (energy)
    call t_obj % reac (r) % set_move   (move)
    ! two conditions
    call t_obj % reac (r) % alloc_cond (2)
    ! condition for molecule itself
    call t_obj % reac (r) % cond (1) % set_tp (t_obj % idx_def())
    call t_obj % reac (r) % cond (1) % alloc_opt (1)
    call t_obj % reac (r) % cond (1) % opt(1) % set_pos ([0,0,0])
    call t_obj % reac (r) % cond (1) % opt(1) % set_state (sta)
    
    ! condition for background checking (empty checking)
    call t_obj % reac (r) % cond (2) % set_tp (0)        ! background
    call t_obj % reac (r) % cond (2) % alloc_opt (1)
    call t_obj % reac (r) % cond (2) % opt(1) % set_pos (pos)
    call t_obj % reac (r) % cond (2) % opt(1) % set_state ([0,0]) 
                
    return
  end subroutine def_free_move

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> initialization
  !---------------------------------------------------------------------------  
  subroutine init ()
    type(mtype), pointer :: t_obj
    integer              :: r

    !-------------------------------------------------------------------
    !-------------------------------------------------------------------
    ! handled by user
    !
    !> adding types into the record
    
    call tlist_init (2)
    tpyp => tlist_new ()
    lead => tlist_new ()

    !
    !-------------------------------------------------------------------
    !-------------------------------------------------------------------
    !
    !> define molecule type TPyP
    !
    ! how to check one condition
    ! 1) check if one of the relative positions matches: cond->opt->pos
    ! 2) check if one of the relative direction matches: cond->opt->dir
    ! 3) check if ALL components' initial states match : cond->state
    ! pass the checking and do movement and update component state
    !
    
    call tpyp % set_symm    (4)    !> define symmetry
    call tpyp % set_idx_def (2000) !> type id should be within [1000, 9999]
    call tpyp % set_eva_num (100)  !> evaporation number
    call tpyp % alloc_comp (5)    !> number of components

    call tpyp % set_comp(1, [ 0, 0, 1]) !> x, y, i-state
    call tpyp % set_comp(2, [ 1, 0, 2]) !> x, y, i-state
    call tpyp % set_comp(3, [ 0, 1, 3]) !> x, y, i-state
    call tpyp % set_comp(4, [-1, 0, 2]) !> x, y, i-state
    call tpyp % set_comp(5, [ 0,-1, 3]) !> x, y, i-state
    call tpyp % alloc_reac (9)    

    call def_free_move(tpyp, 1, [ 1, 0, 0], 0.0_dp)
    call def_free_move(tpyp, 2, [ 0, 1, 0], 0.0_dp)
    call def_free_move(tpyp, 3, [-1, 0, 0], 0.0_dp)
    call def_free_move(tpyp, 4, [ 0,-1, 0], 0.0_dp)
    call def_free_move(tpyp, 5, [ 0, 0, 1], 0.0_dp)
    call def_free_move(tpyp, 6, [ 0, 0, 2], 0.0_dp)
    call def_free_move(tpyp, 7, [ 0, 0, 3], 0.0_dp)

    ! reaction: tpyp+tpyp
    t_obj => tpyp
    r     =  8
    call t_obj % reac (r) % set_energy (-0.5_dp)
    call t_obj % reac (r) % set_move   ([1,0,0])
    call t_obj % reac (r) % alloc_cond (3)
    ! 1st tpyp
    call t_obj % reac (r) % cond (1) % set_tp (2000)
    call t_obj % reac (r) % cond (1) % alloc_opt (1)
    call t_obj % reac (r) % cond (1) % opt(1) % set_pos ([0,0,0])
    call t_obj % reac (r) % cond (1) % opt(1) % set_state &
         ([1,1,  2,4,  3,3,  2,2,  3,3])
    ! 2nd tptp
    call t_obj % reac (r) % cond (2) % set_tp (2000)
    call t_obj % reac (r) % cond (2) % alloc_opt (4)
    call t_obj % reac (r) % cond (2) % opt(1) % set_pos ([4,0,0])
    call t_obj % reac (r) % cond (2) % opt(1) % set_state &
         ([1,1,  2,2,  3,3,  2,4,  3,3])
    call t_obj % reac (r) % cond (2) % opt(2) % set_pos ([4,0,2])
    call t_obj % reac (r) % cond (2) % opt(2) % set_state &
         ([1,1,  2,4,  3,3,  2,2,  3,3])
    call t_obj % reac (r) % cond (2) % opt(3) % set_pos ([4,0,0])
    call t_obj % reac (r) % cond (2) % opt(3) % set_state &
         ([1,1,  4,4,  3,3,  2,4,  3,3])
    call t_obj % reac (r) % cond (2) % opt(4) % set_pos ([4,0,2])
    call t_obj % reac (r) % cond (2) % opt(4) % set_state &
         ([1,1,  2,4,  3,3,  4,4,  3,3])

    ! condition for background checking (empty checking)
    call t_obj % reac (r) % cond (3) % set_tp (0)
    call t_obj % reac (r) % cond (3) % alloc_opt (1)
    call t_obj % reac (r) % cond (3) % opt(1) % set_pos &
         ([1,1,0,  1,-1,0,  2,0,0])
    call t_obj % reac (r) % cond (3) % opt(1) % set_state ([0,0]) 

    ! tpyp+tpyp
    t_obj => tpyp
    r     =  9
    call t_obj % reac (r) % set_energy (-0.5_dp)
    call t_obj % reac (r) % set_move   ([-1,0,0])
    call t_obj % reac (r) % alloc_cond (3)
    ! 1st tpyp
    call t_obj % reac (r) % cond (1) % set_tp (2000)
    call t_obj % reac (r) % cond (1) % alloc_opt (1)
    call t_obj % reac (r) % cond (1) % opt(1) % set_pos ([0,0,0])
    call t_obj % reac (r) % cond (1) % opt(1) % set_state &
         ([1,1,  2,2,  3,3,  2,4,  3,3])
    ! 2nd tptp
    call t_obj % reac (r) % cond (2) % set_tp (2000)
    call t_obj % reac (r) % cond (2) % alloc_opt (4)
    call t_obj % reac (r) % cond (2) % opt(1) % set_pos ([-4,0,0])
    call t_obj % reac (r) % cond (2) % opt(1) % set_state &
         ([1,1,  2,4,  3,3,  2,2,  3,3])
    call t_obj % reac (r) % cond (2) % opt(2) % set_pos ([-4,0,2])
    call t_obj % reac (r) % cond (2) % opt(2) % set_state &
         ([1,1,  2,2,  3,3,  2,4,  3,3])
    call t_obj % reac (r) % cond (2) % opt(3) % set_pos ([-4,0,0])
    call t_obj % reac (r) % cond (2) % opt(3) % set_state &
         ([1,1,  2,4,  3,3,  4,4,  3,3])
    call t_obj % reac (r) % cond (2) % opt(4) % set_pos ([-4,0,2])
    call t_obj % reac (r) % cond (2) % opt(4) % set_state &
         ([1,1,  4,4,  3,3,  2,4,  3,3])

    ! condition for background checking (empty checking)
    call t_obj % reac (r) % cond (3) % set_tp (0)
    call t_obj % reac (r) % cond (3) % alloc_opt (1)
    call t_obj % reac (r) % cond (3) % opt(1) % set_pos &
         ([-1,1,0,  -1,-1,0,  -2,0,0])
    call t_obj % reac (r) % cond (3) % opt(1) % set_state ([0,0]) 

    !
    !-------------------------------------------------------------------
    !-------------------------------------------------------------------
    !
    !> define type Lead
    !
    call lead % set_symm    (1)    
    call lead % set_idx_def (4000) 
    call lead % set_eva_num (100)   
    call lead % alloc_comp (1)    
    call lead % set_comp (1, [0, 0, 9]) 
    call lead % alloc_reac (4)
    call def_free_move(lead,  1, [ 1, 0,0], 0.5_dp)
    call def_free_move(lead,  2, [ 0, 1,0], 0.5_dp)
    call def_free_move(lead,  3, [-1, 0,0], 0.5_dp)
    call def_free_move(lead,  4, [ 0,-1,0], 0.5_dp)
    !
    ! ends here
    !--------------------------------------------------

    !> Auto initialization
    call init_random_seed() ! initialize random seed
    call init_substrate(25,25) ! this should be placed after type definitions
    call init_rates()
    call mlist_init()

    return
  end subroutine init

end module define
