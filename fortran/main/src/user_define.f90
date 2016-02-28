!---------------------------------------------------------------------------  
! DESCRIPTION
!> this is a module should be handle by user for defining all constant
!  variables, molecule structures and chemical reactions
!---------------------------------------------------------------------------  
module user_define
  
  use helper_functions
  use class_mtype
  use class_molecule
  use substrate
  use rate_kmc
  implicit none

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> define molecule type here
  !---------------------------------------------------------------------------  
  type (mtype), save, target :: tpyp
  type (mtype), save, target :: lead

contains

  subroutine def_free_move(mtp, r, move, energy)
    type(mtype), intent (inout) :: mtp
    integer    , intent (in)    :: r 
    integer    , intent (in)    :: move(3)
    real(dp)   , intent (in)    :: energy

    integer, allocatable :: sta (:), pos (:), tmp(:,:)
    integer              :: i, j

    ! new-x, new-y, pos-not-overlap, original-x, original-y
    call alloc_I2(tmp, 5, mtp % comp_num())

    tmp = 1
    do i = 1, mtp % comp_num()
       tmp(1:2,i) = mtp % rotate(mtp % comps(i), move(3)) + move(1:2)
       ! check overlap
       do j = 1, mtp % comp_num()
          tmp(4:5,j) = mtp % comps(j)
          if ( all( tmp(1:2,i) == tmp(4:5,j) ) ) tmp(3,i) = 0
       end do
    end do

    call alloc_I1(pos, sum(tmp(3,:)) * 2)
    call alloc_I1(sta, size(tmp, 2)  * 3)
    
    j = 1
    do i = 1, mtp % comp_num()
       if (tmp(3,i) == 1) then
          pos(2*j-1) = tmp(1,i)
          pos(2*j  ) = tmp(2,i)
          j = j + 1
       end if
       sta(3*i-2) = i
       sta(3*i-1) = 1
       sta(3*i  ) = 1
    end do

    ! basic information
    call mtp % reacs (r) % set_ene (energy)
    call mtp % reacs (r) % set_mov (move)
    ! two conditions
    call mtp % reacs (r) % alloc_conds (2)
    ! condition for molecule itself
    call mtp % reacs (r) % conds (1) % set_tar (mtp % idx_def)
    call mtp % reacs (r) % conds (1) % set_sta (sta) 
    call mtp % reacs (r) % conds (1) % alloc_opt (1)
    call mtp % reacs (r) % conds (1) % opt(1) % set ([0,0],0)
    ! condition for background checking (empty checking)
    call mtp % reacs (r) % conds (2) % set_tar (0)       ! background
    call mtp % reacs (r) % conds (2) % set_sta ([1,0,0]) ! background components
    call mtp % reacs (r) % conds (2) % alloc_opt (4)
    call mtp % reacs (r) % conds (2) % opt(1) % set (pos, 0)
    call mtp % reacs (r) % conds (2) % opt(2) % set (pos, 1)
    call mtp % reacs (r) % conds (2) % opt(3) % set (pos, 2)
    call mtp % reacs (r) % conds (2) % opt(4) % set (pos, 3)
    return
  end subroutine def_free_move

  subroutine def_tpyp_tpyp (mtp, r, energy, mov_pos, tar_dir, num)
    type(mtype) :: mtp
    real(dp) :: energy
    integer  :: mov_pos(2), tar_dir, r, num

    integer  :: mov(3)
    integer  :: ctr_sta(15)
    integer  :: tar_sta(15)
    integer  :: emp_pos(6)

    mov(1:2) = mov_pos
    mov(3)   = tar_dir
    if (all(mov == [ 1,0,0])) then
       if (num == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,2, 5,1,1]
       else if (num == 2) then
          tar_sta = [1,1,1, 2,2,2, 3,1,1, 4,1,2, 5,1,1]
       end if
       ctr_sta = [1,1,1, 2,1,2, 3,1,1, 4,1,1, 5,1,1]
       emp_pos = [2,0, 1,1, 1,-1]
    else if (all(mov == [ 1,0,2])) then
       if (num == 1) then
          tar_sta = [1,1,1, 2,1,2, 3,1,1, 4,1,1, 5,1,1]
       else if (num == 2) then
          tar_sta = [1,1,1, 2,1,2, 3,1,1, 4,2,2, 5,1,1]
       end if
       ctr_sta = [1,1,1, 2,1,2, 3,1,1, 4,1,1, 5,1,1]
       emp_pos = [2,0, 1,1, 1,-1]
    else if (all(mov == [-1,0,0])) then
       if (num == 1) then
          tar_sta = [1,1,1, 2,1,2, 3,1,1, 4,1,1, 5,1,1]
       else if (num == 2) then
          tar_sta = [1,1,1, 2,1,2, 3,1,1, 4,2,2, 5,1,1]
       end if
       ctr_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,2, 5,1,1]
       emp_pos = [-2,0, -1,1, -1,-1]
    else if (all(mov == [-1,0,2])) then
       if (num == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,2, 5,1,1]
       else if (num == 2) then
          tar_sta = [1,1,1, 2,2,2, 3,1,1, 4,1,2, 5,1,1]
       end if
       ctr_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,2, 5,1,1]
       emp_pos = [-2,0, -1,1, -1,-1]
    end if

    call mtp % reacs (r) % set_ene (energy)
    call mtp % reacs (r) % set_mov ([mov_pos(1), mov_pos(2),0])
    call mtp % reacs (r) % alloc_conds (3)
    ! self molecule type
    call mtp % reacs (r) % conds (1) % set_tar (2000)
    call mtp % reacs (r) % conds (1) % set_sta (ctr_sta) 
    call mtp % reacs (r) % conds (1) % alloc_opt (1)
    call mtp % reacs (r) % conds (1) % opt(1) % set ([0,0],0)
    ! target molecule type
    call mtp % reacs (r) % conds (2) % set_tar (2000)
    call mtp % reacs (r) % conds (2) % set_sta (tar_sta) 
    call mtp % reacs (r) % conds (2) % alloc_opt (1)
    call mtp % reacs (r) % conds (2) % opt(1) % set (4 * mov_pos, tar_dir)
    ! condition for background checking (empty checking)
    call mtp % reacs (r) % conds (3) % set_tar (0)       ! background
    call mtp % reacs (r) % conds (3) % set_sta ([1,0,0]) ! background
    call mtp % reacs (r) % conds (3) % alloc_opt (4)
    call mtp % reacs (r) % conds (3) % opt(1) % set (emp_pos, 0)
    call mtp % reacs (r) % conds (3) % opt(2) % set (emp_pos, 1)
    call mtp % reacs (r) % conds (3) % opt(3) % set (emp_pos, 2)
    call mtp % reacs (r) % conds (3) % opt(4) % set (emp_pos, 3)
    return
  end subroutine def_tpyp_tpyp


  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> initialization
  !---------------------------------------------------------------------------  
  subroutine init ()
    integer :: r
    call init_random_seed() !> initialize random seed

    !-------------------------------------------------------------------
    !-------------------------------------------------------------------
    ! handled by user
    !
    !> adding types into the record
    call tlist_set_num (2)
    call tlist_insert (tpyp)
    call tlist_insert (lead)
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
    call tpyp % alloc_comps (5)    !> number of components
    call tpyp % set_comps(1, [ 0, 0]) !> xpos, ypos, comp-id
    call tpyp % set_comps(2, [ 1, 0]) !> xpos, ypos, comp-id
    call tpyp % set_comps(3, [ 0, 1]) !> xpos, ypos, comp-id
    call tpyp % set_comps(4, [-1, 0]) !> xpos, ypos, comp-id
    call tpyp % set_comps(5, [ 0,-1]) !> xpos, ypos, comp-id
    call tpyp % alloc_reacs (16)
    call def_free_move(tpyp, 1, [ 1, 0,0], 0.5_dp)
    call def_free_move(tpyp, 2, [ 0, 1,0], 0.5_dp)
    call def_free_move(tpyp, 3, [-1, 0,0], 0.5_dp)
    call def_free_move(tpyp, 4, [ 0,-1,0], 0.5_dp)
    call def_free_move(tpyp, 5, [ 0, 0,1], 0.5_dp)
    call def_free_move(tpyp, 6, [ 0, 0,2], 0.5_dp)
    call def_free_move(tpyp, 7, [ 0, 0,3], 0.5_dp)
    call def_tpyp_tpyp(tpyp, 8, -0.5_dp, [ 1,0], 0, 1)
    call def_tpyp_tpyp(tpyp, 9, -0.5_dp, [ 1,0], 0, 2)
    call def_tpyp_tpyp(tpyp,10, -0.5_dp, [-1,0], 0, 1)
    call def_tpyp_tpyp(tpyp,11, -0.5_dp, [-1,0], 0, 2)
    call def_tpyp_tpyp(tpyp,12, -0.5_dp, [ 1,0], 2, 1)
    call def_tpyp_tpyp(tpyp,13, -0.5_dp, [ 1,0], 2, 2)
    call def_tpyp_tpyp(tpyp,14, -0.5_dp, [-1,0], 2, 1)
    call def_tpyp_tpyp(tpyp,15, -0.5_dp, [-1,0], 2, 2)
    ! bond 12 single bond tpyp + lead
    r = 16
    call def_tpyp_lead(tpyp,16, -0.5_dp, [0, 1])
    !-------------------------------------------------------------------
    !-------------------------------------------------------------------
    !
    !> define type Lead
    !
    call lead % set_symm    (1)    
    call lead % set_idx_def (4000) 
    call lead % set_eva_num (100)   
    call lead % alloc_comps (1)    
    call lead % set_comps (1, [0, 0]) 
    call lead % alloc_reacs (12)
    call def_free_move(lead,  1, [ 1, 0,0], 0.5_dp)
    call def_free_move(lead,  2, [ 0, 1,0], 0.5_dp)
    call def_free_move(lead,  3, [-1, 0,0], 0.5_dp)
    call def_free_move(lead,  4, [ 0,-1,0], 0.5_dp)
    call def_lead_tpyp( 5, -0.5_dp, [ 0, 1], 0)
    call def_lead_tpyp( 6, -0.5_dp, [ 1, 0], 0)
    call def_lead_tpyp( 7, -0.5_dp, [ 0,-1], 0)
    call def_lead_tpyp( 8, -0.5_dp, [-1, 0], 0)
    call def_lead_tpyp( 9, -0.5_dp, [ 0, 1], 1)
    call def_lead_tpyp(10, -0.5_dp, [ 1, 0], 1)
    call def_lead_tpyp(11, -0.5_dp, [ 0,-1], 1)
    call def_lead_tpyp(12, -0.5_dp, [-1, 0], 1)
  
    ! ends here
    !--------------------------------------------------

    ! !> Initialize molecules
    call init_substrate(45,45) ! this should be placed after type definitions
    call init_rates()
    call init_mlist()
    return
  end subroutine init

  subroutine def_tpyp_lead  (mtp, r, energy, mov)
    type(mtype) :: mtp
    real(dp) :: energy
    integer  :: r, mov(2)

    integer :: ctr_sta(15), emp_pos(6)

    if (all(mov == [0,1])) then
       ctr_sta = [1,1,1, 2,1,1, 3,1,3, 4,1,1, 5,1,1]
       emp_pos = [1, 1, 0, 2, -1, 1]
    else if (all(mov == [0,-1])) then
       ctr_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,1, 5,1,3]
       emp_pos = [1,-1, 0,-2, -1,-1]
    end if

    call mtp % reacs (r) % set_ene (energy)
    call mtp % reacs (r) % set_mov ([mov(1), mov(2),0])
    call mtp % reacs (r) % alloc_conds (3)
    ! self molecule type
    call mtp % reacs (r) % conds (1) % set_tar (2000)
    call mtp % reacs (r) % conds (1) % set_sta (ctr_sta) 
    call mtp % reacs (r) % conds (1) % alloc_opt (1)
    call mtp % reacs (r) % conds (1) % opt(1) % set ([0,0],0)
    ! target molecule type
    call mtp % reacs (r) % conds (2) % set_tar (4000) ! LEAD 
    call mtp % reacs (r) % conds (2) % set_sta ([1,1,3])
    call mtp % reacs (r) % conds (2) % alloc_opt (4)
    call mtp % reacs (r) % conds (2) % opt(1) % set (3 * mov,0)
    call mtp % reacs (r) % conds (2) % opt(2) % set (3 * mov,1)
    call mtp % reacs (r) % conds (2) % opt(3) % set (3 * mov,2)
    call mtp % reacs (r) % conds (2) % opt(4) % set (3 * mov,3)
    ! condition for background checking (empty checking)
    call mtp % reacs (r) % conds (3) % set_tar (0)       ! background
    call mtp % reacs (r) % conds (3) % set_sta ([1,0,0]) ! background
    call mtp % reacs (r) % conds (3) % alloc_opt (4)
    call mtp % reacs (r) % conds (3) % opt(1) % set (emp_pos, 0)
    call mtp % reacs (r) % conds (3) % opt(2) % set (emp_pos, 1)
    call mtp % reacs (r) % conds (3) % opt(3) % set (emp_pos, 2)
    call mtp % reacs (r) % conds (3) % opt(4) % set (emp_pos, 3)
    return
  end subroutine def_tpyp_lead

  subroutine def_lead_tpyp ( r, energy, mov, dir)
    type(mtype), pointer :: mtp
    real(dp) :: energy
    integer  :: r, mov(2), dir
    integer :: tar_sta(15), tar_dir

    mtp => lead

    if (all(mov == [1,0])) then
       if (dir == 0) then
          tar_sta = [1,1,1, 2,1,1, 3,1,3, 4,1,1, 5,1,1]
          tar_dir = 1
       else if (dir == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,1, 5,1,3]
          tar_dir = 3
       end if
    else if (all(mov == [0,1])) then
       if (dir == 0) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,1, 5,1,3]
          tar_dir = 0
       else if (dir == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,3, 4,1,1, 5,1,1]
          tar_dir = 2
       end if
    else if (all(mov == [-1,0])) then
       if (dir == 0) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,1, 5,1,3]
          tar_dir = 1
       else if (dir == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,3, 4,1,1, 5,1,1]
          tar_dir = 3
       end if
    else if (all(mov == [0,-1])) then
       if (dir == 0) then
          tar_sta = [1,1,1, 2,1,1, 3,1,3, 4,1,1, 5,1,1]
          tar_dir = 0
       else if (dir == 1) then
          tar_sta = [1,1,1, 2,1,1, 3,1,1, 4,1,1, 5,1,3]
          tar_dir = 2
       end if
    end if

    call mtp % reacs (r) % set_ene (energy)
    call mtp % reacs (r) % set_mov ([mov(1), mov(2),0])
    call mtp % reacs (r) % alloc_conds (3)
    ! self molecule type
    call mtp % reacs (r) % conds (1) % set_tar (4000)
    call mtp % reacs (r) % conds (1) % set_sta ([1,1,3])
    call mtp % reacs (r) % conds (1) % alloc_opt (1)
    call mtp % reacs (r) % conds (1) % opt(1) % set ([0,0],0)
    ! target molecule type
    call mtp % reacs (r) % conds (2) % set_tar (2000)
    call mtp % reacs (r) % conds (2) % set_sta (tar_sta) 
    call mtp % reacs (r) % conds (2) % alloc_opt (1)
    call mtp % reacs (r) % conds (2) % opt(1) % set (3 * mov,tar_dir)
    ! condition for background checking (empty checking)
    call mtp % reacs (r) % conds (3) % set_tar (0)       ! background
    call mtp % reacs (r) % conds (3) % set_sta ([1,0,0]) ! background
    call mtp % reacs (r) % conds (3) % alloc_opt (4)
    call mtp % reacs (r) % conds (3) % opt(1) % set (mov, 0)
    call mtp % reacs (r) % conds (3) % opt(2) % set (mov, 1)
    call mtp % reacs (r) % conds (3) % opt(3) % set (mov, 2)
    call mtp % reacs (r) % conds (3) % opt(4) % set (mov, 3)

    return
  end subroutine def_lead_tpyp

end module user_define
