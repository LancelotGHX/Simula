subroutine compute_rate(rate, mid)
  use type_define
  use user_define
  use substrate
  implicit none

  real(8), intent(out) :: rate(6)
  integer, intent(in)  :: mid
  type(molecule) :: mol
  type(mtype)    :: mtp
  
  integer :: i, cpos(3)

  mol = mlist(mid)              !< molecule reference
  mtp = tlist(mol % type) % ptr !< type reference

  do i = 1, size(mtp % mov_pos, 1)
     rate(i) = 0
     cpos = mol % pos + mtp % mov_pos (i,:)
     
     




  end do

  stop
end subroutine compute_rate
