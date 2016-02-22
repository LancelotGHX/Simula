!---------------------------------------------------------------------------  
!
! DESCRIPTION: 
!> @brief Define simulation substrate
!
!> @type substrate
!
!> @function substrate_init
!> @function substrate_set
!> @function substrate_get
!
!--------------------------------------------------------------------------- 
module substrate

use helper_functions
use type_define

implicit none

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief substrate static variable
!---------------------------------------------------------------------------
integer, save :: XSIZE, YSIZE
integer, save, allocatable :: self (:,:)
integer, save, allocatable :: activated_num (:)

contains

subroutine init_substrate(xlen, ylen)
  integer :: status, xlen, ylen
  XSIZE = xlen
  YSIZE = ylen
  call alloc_I2(self, xlen, ylen)
  allocate (activated_num(size(tlist)), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  activated_num = 0
  return
end subroutine init_substrate
!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief compress dot information into one number
!> @param mid  : molecule index
!> @param tid  : type index
!> @param comp : component
!> @param state: dot state
!--------------------------------------------------------------------------- 
function convert_to_land (mid, tid, comp, state)
  integer :: mid, tid, comp, state, convert_to_land
  !> check if numbers are all valid
  if (tid   > 99) stop "ERROR: number of types cannot be larger than 99"
  if (comp  > 99) stop "ERROR: number of dots cannot be larger than 99"
  if (state > 99) stop "ERROR: number of states cannot be larger than 99"
  !> compute values
  convert_to_land = state + 100 * comp + 10000 * tid + 1000000 * mid 
  return
end function convert_to_land

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief convert substrate value back to readable number
!> @param v: substrate value
!> @param k: 1 => molecule id, 2 => type id, 3 => component, 4 => state
!--------------------------------------------------------------------------- 
function convert_from_land(v, k)
  integer :: v, k, convert_from_land
  if (k == 1) then
     convert_from_land = v / 1000000
  else if (k == 2) then
     convert_from_land = mod(v / 10000, 100)
  else if (k == 3) then
     convert_from_land = mod(v / 100, 100)
  else if (k == 4) then
     convert_from_land = mod(v, 100)
  end if
  return
end function convert_from_land

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Initialize a substrate
!> @param self: substrate structure
!--------------------------------------------------------------------------- 
subroutine substrate_init()
  self = 0
  return
end subroutine substrate_init

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Access substrate point at (x,y)
!> @param self: substrate structure
!> @param x: x coordinate
!> @param y: y coordinate
!--------------------------------------------------------------------------- 
elemental function get_sub (x, y)
  integer             :: get_sub
  integer, intent(in) :: x, y
  get_sub = self ( modulo(x-1,XSIZE) + 1, modulo(y-1,YSIZE) + 1 )
  return 
end function get_sub

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief Assign substrate point at (x,y)
!> @param self: substrate structure
!> @param x: x coordinate
!> @param y: y coordinate
!> @param v: value
!--------------------------------------------------------------------------- 
subroutine set_sub (x, y, v)
  integer, intent(in) :: x, y, v
  self ( modulo(x-1,XSIZE) + 1, modulo(y-1,YSIZE) + 1 ) = v
  return
end subroutine set_sub

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief to land one molecule at xc, yc, dc position
!> @param 
!> @param 
!> @param 
!> @param 
!> @return true if landing 
!--------------------------------------------------------------------------- 
function land_one (mid, xc, yc, dc)

  logical :: land_one
  integer, intent(in)  :: mid, xc, yc, dc
  integer, allocatable :: vec(:,:)
  integer              :: status
  integer     :: i, x, y, v, tid, state, comp
  logical     :: empty
  
  class(mtype)    :: mtp
  type(molecule) :: obj 

  ! !> retrieve molecule type & molecule object
  ! obj = mlist(mid)
  ! mtp = tlist(obj % type) % ptr

  ! !> calculate all positions
  ! allocate(vec(2, mtp % dot_num), STAT=status)
  ! if (status /= 0) stop "ERROR: Not enough memory!"

  ! !> check position is empty by the way
  ! empty = .true.
  ! do i = 1, mtp % dot_num
  !    vec(1,i) = xc + mtp % dot_pos(i,1)
  !    vec(2,i) = yc + mtp % dot_pos(i,2)
  !    vec(:,i) = rotate(mtp % rmat, vec(:,i), dc) !> rotate molecule
  !    x = vec(1,i)
  !    y = vec(2,i)
  !    if (get_sub(x, y) /= 0) empty = .false.
  ! end do

  ! !> land molecule if the site is empty, do nothing otherwise
  ! if (empty) then
  !    mlist(mid) % pos(1) = xc
  !    mlist(mid) % pos(2) = yc
  !    mlist(mid) % pos(3) = dc
  !    !> land each dots
  !    do i = 1, mtp % dot_num
  !       !> part 1
  !       tid   = mtp % idx_gen
  !       comp  = mtp % dot_pos(i,3)
  !       state = obj%states(i)
  !       !> part 2
  !       x = vec(1,i)
  !       y = vec(2,i)
  !       v = convert_to_land(mid, tid, comp, state)
  !       !> part 3
  !       call set_sub(x, y, v)
  !    end do
  !    land_one = .true.     
  ! else
  !    print *, "  landing failed once"
  !    land_one = .false.
  ! end if

  return
end function land_one

!--------------------------------------------------------------------------- 
! DESCRIPTION
!> @brief print subsrate to screen for debuging
!--------------------------------------------------------------------------- 
subroutine print_to_screen()
  integer :: i, j  
  do j = 1, YSIZE
     write (*, FMT="(100G2.5)") (convert_from_land(self(i,j),3), i=1,XSIZE)
  end do
  return
end subroutine print_to_screen

end module substrate
