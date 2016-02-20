subroutine evaporate (tp, num)
  use helper_functions
  use type_define
  use substrate
  implicit none
  logical, save              :: first_call = .true.
  integer, save, allocatable :: curr_num (:)
  integer :: i, n, status
  integer,      intent(in) :: num
  type (mtype), intent(in) :: tp
  !> first call initialization
  if (first_call) then
     !> allocation check
     n = size(tlist)
     allocate (curr_num (n), STAT = status)
     if (status /= 0) stop "ERROR: Not enough memory!"
     !> initialize evaporated molecule number
     do i = 1, n
        curr_num(i) = 0
     end do
     first_call = .false.
  end if
  !> evaporate molecule
  do i = 1,num
     print *, i
  end do
  
  return
end subroutine

