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
  use class_mtype
  use class_molecule
  implicit none
  private

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief substrate static variable
  integer, save :: m_xsize
  integer, save :: m_ysize
  integer, save, allocatable :: m_sub (:,:)

  !---------------------------------------------------------------------------
  !> public variables
  integer, public, save, allocatable :: activated_num (:)

  public :: init_substrate
  public :: get_sub, set_sub
  public :: convert_from_land
  public :: rand_subX, rand_subY, land_one
  public :: print_to_screen

contains

  subroutine init_substrate (xlen, ylen)
    integer :: xlen, ylen
    m_xsize = xlen
    m_ysize = ylen
    call alloc_I2(m_sub, xlen, ylen)
    m_sub = 0
    call alloc_I1(activated_num, size(tlist))
    activated_num = 0
    return
  end subroutine init_substrate

  function rand_subX ()
    integer :: rand_subX
    rand_subX = rand_int(1, m_xsize)
    return
  end function rand_subX

  function rand_subY ()
    integer :: rand_subY
    rand_subY = rand_int(1, m_ysize)
    return
  end function rand_subY

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
  !> @brief Access substrate point at (x,y)
  !> @param m_sub: substrate structure
  !> @param x: x coordinate
  !> @param y: y coordinate
  !--------------------------------------------------------------------------- 
  elemental function get_sub (x, y)
    integer             :: get_sub
    integer, intent(in) :: x, y
    get_sub = m_sub ( modulo(x-1,m_xsize) + 1, modulo(y-1,m_ysize) + 1 )
    return 
  end function get_sub

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief Assign substrate point at (x,y)
  !> @param m_sub: substrate structure
  !> @param x: x coordinate
  !> @param y: y coordinate
  !> @param v: value
  !--------------------------------------------------------------------------- 
  subroutine set_sub (x, y, v)
    integer, intent(in) :: x, y, v
    m_sub ( modulo(x-1,m_xsize) + 1, modulo(y-1,m_ysize) + 1 ) = v
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

    type(mtype)    :: mtp
    type(molecule) :: obj 

    !> retrieve molecule type & molecule object
    obj = mlist(mid)
    mtp = tlist(obj % type) % ptr

    !> calculate all positions
    call alloc_I2 (vec, 2, mtp % comp_num)

    !> check position is empty by the way
    empty = .true.
    do i = 1, mtp % comp_num
       vec(:,i) = mtp % comps(i,1:2)
       vec(:,i) = rotate(mtp % rmat, vec(:,i), dc) !> rotate molecule
       x = vec(1,i) + xc
       y = vec(2,i) + yc
       if (get_sub(x, y) /= 0) empty = .false.
    end do

    !> land molecule if the site is empty, do nothing otherwise
    if (empty) then
       mlist(mid) % pos(1) = xc
       mlist(mid) % pos(2) = yc
       mlist(mid) % pos(3) = dc
       !> land each dots
       do i = 1, mtp % comp_num
          !> part 1
          tid   = mtp % idx_gen
          comp  = mtp % comps (i,3)
          state = obj % state (i)
          !> part 2
          x = vec(1,i) + xc
          y = vec(2,i) + yc
          v = convert_to_land (mid, tid, comp, state)
          !> debug for center
          !if (i == 1) then
          !   print *, "at:", x, y
          !end if
          !> debug for center
          !> part 3
          call set_sub(x, y, v)
       end do
       land_one = .true.     
    else
       print *, "  landing failed once"
       land_one = .false.
    end if

    return
  end function land_one

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief print subsrate to screen for debuging
  !--------------------------------------------------------------------------- 
  subroutine print_to_screen()
    integer :: i, j, v
    do j = 0, m_ysize+1
       !----------------------------------------------------------------------
       ! in case you want to print all substrate values
       !write (*, "(100G2.5)") (convert_from_land(m_sub(i,j),3), i=1,m_xsize)
       !----------------------------------------------------------------------
       ! draw left boundary if you want
       write (*, "(A1)",advance="no") "|"
       do i = 1, m_xsize
          ! draw upper and lower boundary 
          if (j == 0 .or. j == m_ysize + 1) then
             write (*, "(A2)",advance="no") "=="
          else
             v = convert_from_land(m_sub(i,j),3)
             if (v /= 0) then
                write (*, "(I2)",advance="no") v
             else 
                write (*, "(A2)",advance="no") "  "
             end if
          end if
       end do
       ! draw right boundary if you want
       write (*, "(A1)",advance="no") "|"
       ! change a new line
       write (*,*) ""
       !----------------------------------------------------------------------
    end do
    return
  end subroutine print_to_screen

end module substrate
