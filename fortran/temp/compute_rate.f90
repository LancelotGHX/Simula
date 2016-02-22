!> to calculate rate array
subroutine compute_rate(rate, mid)
  use type_define
  use user_define
  use substrate
  implicit none

  real(8), intent(out) :: rate(6)
  integer, intent(in)  :: mid
  type(molecule) :: mol
  type(mtype)    :: mtp

  stop
end subroutine compute_rate
