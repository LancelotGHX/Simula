!> to calculate rate array
subroutine compute_rate()
  
  use class_condition
  use class_reaction
  use class_mtype
  use class_molecule
  use substrate
  implicit none

  ! real(8), intent(out) :: rate(6)
  ! integer, intent(in)  :: mid
  type(molecule)  :: m_molec
  type(mtype)     :: m_mtype
  type(reaction)  :: m_react
  type(condition) :: m_mcond

  ! how to check one condition
  ! 1) check if one of the relative positions matches: cond->pos
  ! 2) check if one of the relative direction matches: cond->dir
  ! 3) check if ALL components' initial states match : cond->state
  ! pass the checking and do movement and update component state

  integer :: i, min_mid, max_mid
  integer :: j, j_i, j_s
  integer :: k
  integer :: nr, r
  integer :: nc, c
  integer :: s_i, s_x, s_y, s_d, s_t
  integer :: t_i, t_x, t_y, t_d, t_t
  real(8) :: m_rate
  logical :: m_conds_true

  do i = 1, size(tlist)
     m_mtype = tlist(i) % ptr
     min_mid = m_mtype % to_abs_id(1)
     max_mid = m_mtype % to_abs_id(activated_num(i))
     
     s_t = m_mtype % idx_def
     print *, "activated molecule number: ", activated_num(i)
     EACH_MOLECULE: do s_i = min_mid, max_mid
        m_molec = mlist(s_i)

        s_x = m_molec % pos(1)
        s_y = m_molec % pos(2)
        s_d = m_molec % pos(3)
        
        nr = m_mtype % react_num ! no of reactions
        EACH_REACTION: do r = 1, nr
           m_react = m_mtype % reacts(r)
           nc = m_react % cond_num ! no of conditions

           m_conds_true = .true. ! all conditions must be fulfilled
           m_rate = 0.0
           EACH_COND: do c = 1, nc
              m_mcond = m_react % conds(c)
              t_x = s_x + m_mcond % pos(1,1)
              t_y = s_y + m_mcond % pos(1,2)
              t_i = convert_from_land(get_sub(t_x, t_y), 1) ! molecule index

              t_d = mlist(t_i) % pos(3)
              t_t = m_mcond % type
              
              print *, t_x, t_y, t_i, s_i, t_t,tlist(mlist(t_i) % type)%ptr%idx_def

              ! check if type confirms with definition
              if (t_t /= tlist(mlist(t_i) % type)%ptr%idx_def) then
                 m_conds_true = .false.
              end if

              ! check if molecule direction fits
              if ( all(m_mcond % dir /= t_d- s_d) ) then                 
                 m_conds_true = .false.
              end if

              ! check all comp states must be the same as initial
              do j = 1, m_mtype % comp_num 
                 j_i = m_mtype % comps (j,3)
                 j_s = m_molec % state (j)
                 do k = 1, size(m_mcond % state, 1)
                    if (m_mcond % state(k,1) == j_i) then
                       if (m_mcond % state(k,2) /= j_s) then
                          m_conds_true = .false.
                       end if
                    end if
                 end do
              end do
              
           end do EACH_COND

           if (m_conds_true) then
              m_rate = exp(-m_react % energy / 0.01)
           else 
              m_rate = 0.0
           end if

           print *, m_rate, m_react % energy

        end do EACH_REACTION
     end do EACH_MOLECULE
  end do
  return

contains

end subroutine compute_rate
