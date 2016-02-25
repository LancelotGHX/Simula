!> to calculate rate array
subroutine compute_rate()
  
  use class_condition
  use class_reaction
  use class_mtype
  use class_molecule
  use substrate
  implicit none

  ! how to check one condition
  ! 1) check if one of the relative positions matches: cond->opt->pos
  ! 2) check if one of the relative direction matches: cond->opt->dir
  ! 3) check if ALL components' initial states match : cond->state
  ! pass the checking and do movement and update component state

  ! real(8), intent(out) :: rate(6)

  type(molecule)  :: m_obj
  type(mtype)     :: t_obj
  type(reaction)  :: r_obj
  type(condition) :: c_obj

  ! counter number
  integer :: m, m_range(2) ! molecule index counter
  integer :: t             ! type index counter
  integer :: r             ! reaction
  integer :: c             ! condition
  integer :: o             ! option
  integer :: p

  integer :: t_p(2), t_t, t_d, t_m
  integer :: e_t

  logical :: all_c_true
  logical :: one_o_true
  logical :: all_p_true

  real(8) :: rate

  EACH_TYPE:  do t = 1, size(tlist)-1
     ! retrieve current type
     t_obj = tlist(t) % ptr

     ! retrieve index range
     m_range(1) = t_obj % abs_id(1)
     m_range(2) = t_obj % abs_id(activated_num(t))
     
     !print  *, "type", t, "index range", m_range(1), m_range(2)

     EACH_MOLECULE: do m = m_range(1), m_range(2)
        ! retrieve current molecule
        m_obj = mlist(m)

        !print *, "molecule", m_obj % pos(1), m_obj % pos(2),m_obj % pos(3)

        EACH_REACTION: do r = 1, t_obj % reac_num()
           ! retrieve current reaction
           r_obj = t_obj % reacs(r)

           ! all conditions must be fulfilled
           all_c_true = .true.
           
           ! rate default value
           rate = 0.0

           EACH_COND: do c = 1, r_obj % cond_num()
              ! retrieve current condition
              c_obj = r_obj % conds(c)

              one_o_true = .false.
              EACH_OPTION: do o = 1, c_obj % opt_num()

                 all_p_true = .true.
                 EACH_POSION: do p = 1, c_obj % opt(o) % pos_num()
                    ! condition position
                    t_p = m_obj % pos(1:2) + c_obj % opt(o) % pos(p)

                    ! target molecule index
                    t_m = convert_from_land(get_sub(t_p(1), t_p(2)), 1) 

                    ! target molecule defined type
                    t_t = tlist(mlist(t_m) % type) % ptr % idx_def

                    ! target molecule relative direction
                    t_d = modulo( &
                         mlist(t_m) % pos(3) - m_obj % pos(3), &
                         tlist(mlist(t_m) % type) % ptr % symm &
                         )

                    !print *, t_t, t_p(1), t_p(2), t_d, t_m
                    !pause

                    ! check if type confirms with definition              
                    if (type_not_equal(c_obj, t_t)) all_p_true = .false.

                    ! check if molecule direction fits              
                    if (all_dir_not_equal(c_obj, o, t_d)) all_p_true = .false.  

                    ! check all comp states must be the same as initial
                    if (all_states_not_equal(c_obj, t_m)) all_p_true = .false.

                 end do EACH_POSION

                 if (all_p_true) one_o_true = .true.

              end do EACH_OPTION

              if (.not.one_o_true) all_c_true = .false.               

           end do EACH_COND

           if (all_c_true) then
              rate = exp(- r_obj % ene / 0.01)
           else 
              rate = 0.0
           end if

           print *, t, m, "rate", rate, "energy", r_obj % ene, all_c_true

        end do EACH_REACTION
     end do EACH_MOLECULE
  end do EACH_TYPE
  return

contains

  function all_dir_not_equal (c_obj, i, d) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: i, d
    logical                      :: p
    p = all(c_obj % opt(i) % dir /= d)
    return
  end function all_dir_not_equal

  function type_not_equal (c_obj, t) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: t
    logical                      :: p
    p = (c_obj % tar /= t)
    return
  end function type_not_equal

  function all_states_not_equal (c_obj, m) result (p)
    type(condition), intent (in) :: c_obj
    integer        , intent (in) :: m
    logical                      :: p
    type(mtype)    :: t_obj
    type(molecule) :: m_obj
    integer :: c, i, s, k, comp(3), stas(3)
    p = .true.
    m_obj = mlist(m)
    t_obj = tlist(m_obj % type) % ptr    
    EACH_COMP: do c = 1, t_obj % comp_num ()
       comp = t_obj % comps (c) ! component info
       i = comp(3)         ! component index
       s = m_obj % sta (c) ! component current state
       EACH_COND: do k = 1, c_obj % sta_num()
          stas = c_obj % sta (k)
          if (stas(1) == i) then
             if (stas (2) == s) then
                p = .false.
             end if
          end if
       end do EACH_COND
    end do EACH_COMP
    return
  end function all_states_not_equal

end subroutine compute_rate
