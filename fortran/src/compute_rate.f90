!> to calculate rate array
subroutine compute_rate()
  
  use class_condition
  use class_reaction
  use class_mtype
  use class_molecule
  use substrate
  implicit none

  ! how to check one condition
  ! 1) check if one of the relative positions matches: cond->pos
  ! 2) check if one of the relative direction matches: cond->dir
  ! 3) check if ALL components' initial states match : cond->state
  ! pass the checking and do movement and update component state

  ! real(8), intent(out) :: rate(6)

  type(molecule)  :: m_obj
  type(mtype)     :: t_obj
  type(reaction)  :: r_obj
  type(condition) :: c_obj

  integer :: m, m_range(2), m_pos(3) ! molecule index counter
  integer :: t                       ! type index counter
  integer :: r                       ! reaction
  integer :: c                       ! condition
  integer :: loc (2)
  
  integer :: t_t, t_d, t_m
  integer :: e_t

  logical :: all_conds_true
  real(8) :: rate

  EACH_TYPE:  do t = 1, size(tlist)
     ! retrieve current type
     t_obj = tlist(t) % ptr
     ! retrieve index range
     m_range(1) = t_obj % abs_id(1)
     m_range(2) = t_obj % abs_id(activated_num(t))
     
     EACH_MOLECULE: do m = m_range(1), m_range(2)
        ! retrieve current molecule
        m_obj = mlist(m)
        m_pos = m_obj % pos
        
        EACH_REACTION: do r = 1, t_obj % reac_num()
           ! retrieve current reaction
           r_obj = t_obj % reacs(r)

           ! all conditions must be fulfilled
           all_conds_true = .true. 
           
           rate = 0.0
           EACH_COND: do c = 1, r_obj % cond_num()
              ! retrieve current condition
              c_obj = r_obj % conds(c)

              loc = m_pos(1:2) + c_obj % pos(1,1:2) ! condition position

              t_m = convert_from_land(get_sub(loc(1), loc(2)), 1) ! target molecule index
              t_t = tlist(mlist(t_m) % type) % ptr % idx_def      ! target molecule defined type
              t_d = mlist(t_m) % pos(3) - m_pos(3)                ! target molecule relative direction             

              ! check if type confirms with definition              
              all_conds_true = check_type(c_obj, t_t)
              ! check if molecule direction fits              
              all_conds_true = check_dir(c_obj, t_d)
              ! check all comp states must be the same as initial
              all_conds_true = check_states(c_obj, t_m)

              print *, loc, t_m, t_t, c_obj % tar

           end do EACH_COND

           if (all_conds_true) then
              rate = exp(- r_obj % ene / 0.01)
           else 
              rate = 0.0
           end if

           print *, "rate", rate, "energy", r_obj % ene

        end do EACH_REACTION
     end do EACH_MOLECULE
  end do EACH_TYPE
  return

contains

  function check_type (c_obj, t) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: t
    logical                      :: p
    p = .true.
    if (c_obj % tar /= t) then
       p = .false.
    end if
    return
  end function check_type

  function check_dir (c_obj, d) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: d
    logical                      :: p
    p = .true.
    if (all(c_obj % dir /= d)) then                 
       p = .false.
    end if
    return
  end function check_dir

  function check_states (c_obj, m) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: m
    logical                      :: p
    type(mtype)    :: t_obj
    type(molecule) :: m_obj
    integer :: c, i, s, k
    p = .true.
    m_obj = mlist(m)
    t_obj = tlist(m_obj % type) % ptr    
    EACH_COMP: do c = 1, t_obj % comp_num ()
       i = t_obj % comps (c,3) ! component index
       s = m_obj % sta (c)                         ! component current state
       EACH_COND: do k = 1, size(c_obj % sta, 1)
          if (c_obj % sta (k,1) == i) then
             if (c_obj % sta (k,2) /= s) then
                p = .false.
             end if
          end if
       end do EACH_COND
    end do EACH_COMP
    return
  end function check_states

end subroutine compute_rate
