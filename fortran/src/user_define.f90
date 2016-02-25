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

  implicit none

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> define molecule type here
  !---------------------------------------------------------------------------  
  type (mtype), save :: bg
  type (mtype), save :: tpyp
  type (mtype), save :: lead

contains

  subroutine def_free_move(mtp, r, move, energy)
    type(mtype), intent (inout) :: mtp
    integer    , intent (in)    :: r 
    integer    , intent (in)    :: move(3)
    real(dp)   , intent (in)    :: energy

    integer, allocatable :: sta (:), pos (:), tmp(:,:)
    integer              :: i, j, k

    ! x, y, id, pos-not-overlap, id-not-overlap, bufferx3
    call alloc_I2(tmp, 8, mtp % comp_num())

    tmp = 1
    do i = 1, mtp % comp_num()
       tmp(1:3,i) = mtp % comps(i)
       tmp(1:2,i) = mtp % rotate(tmp(1:2,i), move(3)) + move(1:2)
       do j = 1, mtp % comp_num()
          tmp(6:8,j) = mtp % comps(j)
          if ( all( tmp(1:2,i) == tmp(6:7,j) ) ) tmp(4,i) = 0
          if (j < i) then
             if ( tmp(3,i) == tmp(8,j) ) tmp(5,i) = 0
          end if
       end do
    end do

    call alloc_I1(pos, sum(tmp(4,:)) * 2)
    call alloc_I1(sta, sum(tmp(5,:)) * 3)

    print *, sum(tmp(4,:)), sum(tmp(5,:))
    j = 1
    k = 1
    do i = 1, mtp % comp_num()
       if (tmp(4,i) == 1) then
          pos(2*j-1:2*j) = tmp(1:2,i)
          j = j + 1
       end if
       if (tmp(5,i) == 1) then
          sta(3*k-2) = tmp(3,i)
          sta(3*k-1) = 0
          sta(3*k  ) = 0
          k = k + 1
       end if
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
    call mtp % reacs (r) % conds (1) % opt(1) % set ([0,0],[0])
    ! condition for background checking (empty checking)
    call mtp % reacs (r) % conds (2) % set_tar (0)       ! background
    call mtp % reacs (r) % conds (2) % set_sta ([1,0,0]) ! background components
    call mtp % reacs (r) % conds (2) % alloc_opt (4)
    call mtp % reacs (r) % conds (2) % opt(1) % set (pos, [0])
    call mtp % reacs (r) % conds (2) % opt(2) % set (pos, [1])
    call mtp % reacs (r) % conds (2) % opt(3) % set (pos, [2])
    call mtp % reacs (r) % conds (2) % opt(4) % set (pos, [3])
    return
  end subroutine def_free_move

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
    call tlist_number (2)
    call tlist_insert (tpyp)
    call tlist_insert (lead)
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
    call tpyp % set_eva_num (10)   !> evaporation number
    call tpyp % alloc_comps (5)    !> number of components
    call tpyp % set_comps(1, [ 0, 0, 1]) !> xpos, ypos, comp-id
    call tpyp % set_comps(2, [ 1, 0, 2]) !> xpos, ypos, comp-id
    call tpyp % set_comps(3, [ 0, 1, 3]) !> xpos, ypos, comp-id
    call tpyp % set_comps(4, [-1, 0, 2]) !> xpos, ypos, comp-id
    call tpyp % set_comps(5, [ 0,-1, 3]) !> xpos, ypos, comp-id
    call tpyp % alloc_reacs (1)
    call def_free_move(tpyp, 1, [ 1, 0,0], 0.5_dp)
    !call def_free_move(tpyp, 2, [ 0, 1,0], 0.5_dp)
    !call def_free_move(tpyp, 3, [-1, 0,0], 0.5_dp)
    !call def_free_move(tpyp, 4, [ 0,-1,0], 0.5_dp)
    !
    !> define type Lead
    !
    call lead % set_symm    (4)    
    call lead % set_idx_def (2000) 
    call lead % set_eva_num (10)   
    call lead % alloc_comps (1)    
    call lead % set_comps (1, [0, 0, 4]) 
    call lead % alloc_reacs (0)
    !
    ! ends here
    !--------------------------------------------------

    ! !> Initialize molecules
    call init_substrate(15,15) ! this should be placed after type definitions
    call init_mlist()
    return
  end subroutine init

end module user_define
