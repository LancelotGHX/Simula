!> to calculate rate array
subroutine compute_rate()

  use class_mtype
  use class_molecule
  use substrate
  implicit none

  ! real(8), intent(out) :: rate(6)
  ! integer, intent(in)  :: mid
  type(molecule) :: mol
  type(mtype)    :: mtp

  ! how to check one condition
  ! 1) check if one of the relative positions matches: cond->pos
  ! 2) check if one of the relative direction matches: cond->dir
  ! 3) check if ALL components' initial states match : cond->state
  ! pass the checking and do movement and update component state

  integer :: i, min_id, max_id, id
  integer :: nr, r
  integer :: nc, c
  integer :: x, y, d, v

  do i = 1, size(tlist)
     print *, "activated molecule number: ", activated_num(i)
     mtp = tlist(i) % ptr
     min_id = mtp % to_abs_id(1)
     max_id = mtp % to_abs_id(activated_num(i))
     EACH_MOLECULE: do id = min_id, max_id
        mol = mlist(id)
        x = mol % pos(1)
        y = mol % pos(2)
        d = mol % pos(3)
        nr = mtp % react_num
        EACH_REACTION: do r = 1, nr
           nc = mtp % reacts(r) % cond_num
           EACH_COND: do c = 1, nc
              x = x + mtp % reacts(r) % conds(c) % pos(1,1)
              y = y + mtp % reacts(r) % conds(c) % pos(1,2)
              v = convert_from_land(get_sub(x, y), 1) ! molecule index
              print *, v, id, get_sub(x, y), x, y

           end do EACH_COND
        end do EACH_REACTION
     end do EACH_MOLECULE
  end do

  return
end subroutine compute_rate
