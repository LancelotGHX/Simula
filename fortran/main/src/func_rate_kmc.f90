!-----------------------------------------------------------------------------
!
! DESCRIPTION
!> @ module to handle KMC rate calculations and event selection
!
!-----------------------------------------------------------------------------
module func_rate_kmc

  !---------------------------------------------------------------------------
  ! used modules
  use func_helper    , only: &
       dp, alloc_I1, alloc_I2, alloc_F1, rand_uniform, binary_search
  use class_condition, only: condition
  use class_reaction , only: reaction 
  use class_mtype    , only: tlist, mtype, tlist_num
  use class_molecule , only: mlist, molecule, mlist_num
  use func_substrate , only: &
       get_sub, activated_num, convert_from_land, move_one
  implicit none
  private

  type, private :: m_data_info
     integer              :: size
     integer              :: idxs (3) ! self   [ tid, mid, rid ]
     integer, allocatable :: tars (:) ! target [ mid, mid, ... ]
  end type m_data_info

  type(m_data_info), allocatable, save :: m_data(:)

  !---------------------------------------------------------------------------
  ! private variables

  real(dp), allocatable, save :: m_rate(:) ! all value of rates
  ! STRUCTURE = 1 x (1 : No. of reactions)

  real(dp), allocatable, save :: m_sums(:) ! step sums of rates
  ! STRUCTURE = 1 x (1 : No. of reactions + 1)
  !  [                                0] #1
  !  [step sum of rate up to reaction 1] #2
  !  [step sum of rate up to reaction 2] #3
  !  [step sum of rate up to reaction 3] #4
  !  ...
  !  [step sum of rate up to reaction N] #N+1

  integer , allocatable, save :: m_reac(:,:) ! step sums of reactions
  ! STRUCTURE = 2 x (1 : No. of mtypes + 1)
  !  [                 0, total No. of reactions] #1
  !  [step sum of type 1, reaction No. of type 1] #2
  !  [step sum of type 2, reaction No. of type 2] #3
  !  [step sum of type 3, reaction No. of type 3] #4
  !  ...
  !  [step sum of type T, reaction No. of type T] #T+1
 
  !---------------------------------------------------------------------------
  ! global variables/functions
  public :: init_rates, compute_rates

contains

  !---------------------------------------------------------------------------
  ! DESCRIPTION
  !> @brief initialize rate list
  !---------------------------------------------------------------------------
  subroutine init_rates()
    integer :: t, m, r, i, status
    
    ! calculate reaction number and its step sum
    call alloc_I2 (m_reac, 2, tlist_num()+1) ! [step-sum, step]
    m_reac(1,1) = 0
    do t = 1, tlist_num()
       m_reac(2,t+1) = tlist(t) % ptr % reac_num()
       m_reac(1,t+1) = m_reac(1,t) + &
            tlist(t) % ptr % eva_num * m_reac(2,t+1)
    end do
    m_reac(2,1) = m_reac(1,t) ! note that here t=tlist_num()+1 after the loop
    
    ! just an example how to get No. of reactions
    write (*,'(" No. of reactions", I6)') m_reac_num()

    ! allocate rate sum array and data array
    call alloc_F1 (m_sums, m_reac(2,1) + 1)
    call alloc_F1 (m_rate, m_reac(2,1)    )
    allocate (m_data(m_reac(2,1)), STAT = status)
    if (status /= 0) stop "Not enough memory"

    ! initialize array
    m_rate = 0.0_dp
    m_sums = 0.0_dp

    ! loop over all reactions to get number of conditions
    do i = 1, m_reac_num()
       t = binary_search(m_reac(1,:), i)     ! type index
       r = modulo ( i-1-m_reac(1,t), m_reac(2,t+1) ) + 1 ! reaction index
       m_data (i) % idxs = [t,0,r]
       m_data (i) % size = tlist(t) % ptr % reacs(r) % cond_num()
       call alloc_I1 (m_data (i) % tars, m_data (i) % size)
       !if (i > 1600 .and. i < 1700) then
       !   print *, "test", i,  m_data (i) % size, t, r
       !end if
       m_data (i) % tars = 0
    end do

    return
  end subroutine init_rates

  !---------------------------------------------------------------------------
  ! DESCRIPTION
  !> @brief Getter to total number of reaction, just to make life easier
  !---------------------------------------------------------------------------
  function m_reac_num() result (r)
    integer :: r
    r = m_reac(2,1)
    return
  end function m_reac_num

  function select_rates () result (i)
    integer  :: i
    real(dp) :: r, p, u

    r = 0.0_dp
    if (m_sums(m_reac_num()+1) == 0.0_dp) stop "ERROR: no events"

    do while (r == 0.0_dp)
       p = rand_uniform(0.0_dp, m_sums(m_reac_num()+1))
       !print *, p
       
       !u = rand_uniform(0.0_dp, 1.0_dp)
       !print *, log(1.0_dp/u) / m_rate_stp(m_reac_num)
    
       i = binary_search(m_sums, p)
       !print *, i, m_rate(i)

       r = m_rate(i)
    end do

    return
  end function select_rates

  subroutine execute_reaction(i)
    integer, intent (in) :: i
    integer              :: t, m, r, c, o, p
    integer              :: t_p(2), t_m
    integer              :: x,y,d,pos(2),vec(3)
    type(mtype)    , pointer :: t_obj, debug_t_obj
    type(molecule) , pointer :: m_obj, debug_m_obj
    type(condition), pointer :: c_obj, debug_c_obj

    t = m_data(i) % idxs(1)
    m = m_data(i) % idxs(2)
    r = m_data(i) % idxs(3)
    !print *, t, m, r

    m_obj => mlist(m)
    t_obj => tlist(t) % ptr
    vec = m_obj % pos
    ! execute molecule movement
    EACH_CONDITION: do c = 1, m_data(i) % size
       c_obj => tlist(t) % ptr % reacs(r) % conds(c)
       o = m_data(i) % tars(c)
       EACH_POSION: do p = 1, c_obj % opt(o) % pos_num()
          ! condition position
          !> @remark need to rotate t_p first since it is define
          ! according to relative direction 
          t_p = vec(1:2) + &
               t_obj % rotate(c_obj % opt(o) % pos(p), vec(3))      
          ! target molecule index
          t_m = convert_from_land(get_sub(t_p(1), t_p(2)), 1)           
          !print *, "==>",t_m, "==>",m, mlist(t_m) % sta
          debug_m_obj => mlist(t_m)
          !print *, mlist(t_m) % sta
          call  c_obj % sta_final(mlist(t_m) % sta)
          ! update substrate
          call move_one(t_m,0,0,0)
       end do EACH_POSION
    end do EACH_CONDITION

    !> @remark here also we need to rotate 'mov' according to moelcule
    !          direction since it is define using relative position
    pos = t_obj % rotate(tlist(t) % ptr % reacs(r) % mov(1:2), vec(3)) 
    x = pos (1)
    y = pos (2)
    d = tlist(t) % ptr % reacs(r) % mov(3)
    !print *, tlist(t) % ptr % reacs(r) % mov
    call move_one(m,x,y,d)

    return
  end subroutine execute_reaction
 
  !---------------------------------------------------------------------------
  ! DESCRIPTION
  !> @brief calculate the rate array for all reactions
  !---------------------------------------------------------------------------
  subroutine compute_rates()

    ! how to check one condition
    ! 1) check if one of the relative positions matches: cond->opt->pos
    ! 2) check if one of the relative direction matches: cond->opt->dir
    ! 3) check if ALL components' initial states match : cond->state
    ! pass the checking and do movement and update component state

    ! Here I don't want to do real copying, so all pointers
    type(molecule) , pointer :: m_obj ! molecule object
    type(mtype)    , pointer :: t_obj ! type object
    type(reaction) , pointer :: r_obj ! reaction object
    type(condition), pointer :: c_obj ! condition object

    ! counter number
    integer :: m, m_range(2) ! molecule index counter
    integer :: t             ! type index counter
    integer :: r             ! reaction
    integer :: c             ! condition
    integer :: o             ! option
    integer :: p             ! option position

    integer :: t_p(2), t_t, t_d, t_m ! target properties

    integer :: vec(3)

    logical :: all_c_true
    logical :: one_o_true
    logical :: all_p_true

    real(dp) :: rate
    integer  :: reac_idx

    EACH_TYPE: do t = 1, size(tlist)-1
       ! retrieve current type
       t_obj => tlist(t) % ptr

       ! retrieve index range
       m_range(1) = t_obj % abs_id(1)
       m_range(2) = t_obj % abs_id(activated_num(t))

       !print  *, "type", t, "index range", m_range(1), m_range(2)

       EACH_MOLECULE: do m = m_range(1), m_range(2)
          ! retrieve current molecule
          m_obj => mlist(m)
          vec = m_obj % pos

          EACH_REACTION: do r = 1, t_obj % reac_num()
             reac_idx = (m-m_range(1)) * t_obj % reac_num() + m_reac(1,t) + r

             ! record data for moelcule itself
             m_data(reac_idx) % idxs = [ t, m, r ]
             !print *, reac_idx

             ! retrieve current reaction
             r_obj => t_obj % reacs(r)

             ! all conditions must be fulfilled
             all_c_true = .true.

             ! rate default value
             rate = 0.0
             
             EACH_COND: do c = 1, r_obj % cond_num()
                ! retrieve current condition
                c_obj => r_obj % conds(c)

                one_o_true = .false.
                EACH_OPTION: do o = 1, c_obj % opt_num()

                   all_p_true = .true.
                   EACH_POSION: do p = 1, c_obj % opt(o) % pos_num()
                      ! condition position
                      !> @remark need to rotate t_p first since it is define
                      ! according to relative direction 
                      
                      t_p = vec(1:2) + &
                           t_obj % rotate(c_obj % opt(o) % pos(p), vec(3))

                      ! target molecule index
                      t_m = convert_from_land(get_sub(t_p(1), t_p(2)), 1) 

                      ! target molecule defined type
                      t_t = tlist(mlist(t_m) % type) % ptr % idx_def

                      ! target molecule relative direction
                      t_d = modulo( &
                           mlist(t_m) % pos(3) - m_obj % pos(3), &
                           tlist(mlist(t_m) % type) % ptr % symm &
                           )
                      
                      !> @remark  we should never check the center
                      !           position of background                      
                      if (t_t /= 0) then 
                         !> @remark check if t_p is the central position of
                         !          molecule
                         if (.not. all(t_p == mlist(t_m) % pos(1:2))) then
                            all_p_true = .false.
                            exit EACH_POSION
                         end if
                      end if
                      ! check if molecule direction fits              
                      if (c_obj % opt(o) % dir_not_equal(t_d)) then
                         all_p_true = .false.
                         exit EACH_POSION
                      end if

                      ! check if type confirms with definition              
                      if (c_obj % tar_not_equal(t_t)) then 
                         all_p_true = .false.
                         exit EACH_POSION
                      end if

                      ! check all comp states must be the same as initial
                      if (c_obj % sta_not_equal(mlist(t_m) % sta)) then
                         all_p_true = .false.
                         exit EACH_POSION
                      end if

                   end do EACH_POSION
                   
                   !> @remark if all the option requirement are fulfilled, 
                   !          then we have at least one option to to true
                   !> @remark if more than one options are fulfilled, the last
                   !          one will be selected according to our method. 
                   !          however we should always make sure there is only
                   !          one option is valid each time
                   if (all_p_true) then
                      one_o_true = .true.
                      !print *, "--",r_obj % cond_num(),m_data (reac_idx) % idxs
                      m_data (reac_idx) % tars (c) = o
                      
                      exit EACH_OPTION
                   end if

                end do EACH_OPTION

                ! if we don't have any option passed, then the condition must 
                ! be failed
                if (.not.one_o_true) all_c_true = .false.
                !print *, t, m, r, c, all_c_true

             end do EACH_COND

             ! if the condition is passed, we calculate rate using exp 
             ! function (need more freedom). else we set rate to be zero
             ! which means the execution probability is zero
             if (all_c_true) then
                rate = 10E-10  * exp(- r_obj % ene / 0.01)          
             else 
                rate = 0.0_dp
             end if

             m_rate (reac_idx) = rate


          end do EACH_REACTION

       end do EACH_MOLECULE

    end do EACH_TYPE
    
    do reac_idx = 1, m_reac_num()
       m_sums(reac_idx+1) = sum(m_rate(1:reac_idx))
       !print *, "r=", m_sums(reac_idx+1), m_rate(reac_idx)
    end do
    
    !write (*, '(" No. of rates ",I6," rate sum = ",ES25.15)') &
    !     m_reac_num(), m_sums(m_reac_num())

    call execute_reaction(select_rates())

    return
  end subroutine compute_rates

end module func_rate_kmc
