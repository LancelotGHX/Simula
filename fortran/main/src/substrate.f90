!---------------------------------------------------------------------------  
!
! DESCRIPTION: 
!> @brief Define actions related to substrate
!
!> @function init_substrate
!> @function set_sub
!> @function get_sub
!> ...
!
!--------------------------------------------------------------------------- 
module substrate

  use helper_functions, only: alloc_I1, alloc_I2, rand_int
  use class_mtype     , only: tlist, mtype, tlist_num
  use class_molecule  , only: mlist, molecule
  implicit none

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief substrate static variable
  integer, private :: m_xsize
  integer, private :: m_ysize
  integer, private, allocatable :: m_sub (:,:) ! substrate data
  integer, private, allocatable :: m_num (:)   ! number of molecule activated

contains

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief Get number of activated molecules of type i
  !> @param i: ith molecule type
  !--------------------------------------------------------------------------- 
  function activated_num (i) result (r)
    integer, intent (in) :: i
    integer              :: r
    if (i > tlist_num()) stop "ERROR: index out of bound (module-substrate)"
    r = m_num(i)
    return
  end function activated_num

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief Increase activated molecules of type i by one
  !> @param i: ith molecule type
  !--------------------------------------------------------------------------- 
  subroutine activate_new (i)
    integer, intent (in) :: i
    if (i > tlist_num()) stop "ERROR: index out of bound (module-substrate)"
    m_num (i) = m_num (i) + 1
    return
  end subroutine activate_new

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief Allocate and initialize substrate
  !> @param xlen
  !> @param ylen 
  !--------------------------------------------------------------------------- 
  subroutine init_substrate (xlen, ylen)
    integer, intent (in) :: xlen, ylen
    m_xsize = xlen
    m_ysize = ylen
    call alloc_I2(m_sub, xlen, ylen)
    call alloc_I1(m_num, tlist_num())
    m_sub = 0
    m_num = 0
    return
  end subroutine init_substrate

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief generate a random X coordinate
  !--------------------------------------------------------------------------- 
  function rand_subX ()
    integer :: rand_subX
    rand_subX = rand_int(1, m_xsize)
    return
  end function rand_subX

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief generate a random Y coordinate
  !--------------------------------------------------------------------------- 
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
  !> @param m: molecule index
  !> @param xc
  !> @param yc
  !> @param dc
  !> @return true if landing 
  !--------------------------------------------------------------------------- 
  function land_one (m, xc, yc, dc)
    logical :: land_one
    logical :: empty
    integer, intent(in)  :: m, xc, yc, dc
    integer, allocatable :: vec(:,:)
    integer              :: i, x, y, v, t, s, c
    type(mtype)   , pointer :: mtp
    type(molecule), pointer :: obj 

    ! retrieve molecule type & molecule object
    obj => mlist(m)
    mtp => tlist(obj % type) % ptr

    ! calculate all positions {x,y,comp-id}
    call alloc_I2 (vec, 2, mtp % comp_num())

    ! check position is empty by the way
    empty = .true.
    do i = 1, mtp % comp_num()
       vec(:,i) = mtp % comps(i)
       vec(:,i) = mtp % rotate(vec(:,i), dc)
       x = vec(1,i) + xc
       y = vec(2,i) + yc
       if (get_sub(x, y) /= 0) empty = .false.
    end do

    ! land molecule if the site is empty, do nothing otherwise
    if (empty) then

       !mlist(m) % pos(1) = xc ! here I am directly accessing the data in order
       !mlist(m) % pos(2) = yc ! to make sure the changes are saved. Do changes
       !mlist(m) % pos(3) = dc ! on local variable does not gaurantee that

       obj % pos(1) = xc ! however this works if the variable is defined
       obj % pos(2) = yc ! as a pointer
       obj % pos(3) = modulo(dc, mtp % symm) 

       ! land each dots
       do i = 1, mtp % comp_num()
          ! part 1
          t = mtp % idx_gen() ! type index generated
          c = i               ! comp-id
          s = obj % sta(i)    ! comp-state
          ! part 2
          x = vec(1,i) + xc
          y = vec(2,i) + yc
          v = convert_to_land (m, t, c, s)
          ! part 3
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
  !> @brief to move m-th molecule from its original position to (nx,ny,nd)
  !> @param m: molecule index
  !> @param rx
  !> @param ry
  !> @param rd
  !> @remark: this function does not perform empty test ! user has the duty to
  !           make sure the destination is completely empty
  !---------------------------------------------------------------------------
  subroutine move_one (m, rx, ry, rd)
    integer, intent(in)  :: m, rx, ry, rd ! relative position
    integer              :: nx, ny, nd
    integer              :: ox, oy, od
    integer              :: comp(2), opos(2), npos(2)
    integer              :: i, t, c, s
    type(mtype)   , pointer :: mtp
    type(molecule), pointer :: obj 

    ! retrieve molecule type & molecule object
    obj => mlist(m)                ! pointing pointer to oject
    mtp => tlist(obj % type) % ptr ! this is pointer assignment !

    ox = obj % pos(1)
    oy = obj % pos(2)
    od = obj % pos(3)
    !print *, "move old", obj % pos
    
    nx = rx + ox
    ny = ry + oy
    nd = rd + od

    !mlist(m) % pos(1) = nx ! here I am directly accessing the data in order
    !mlist(m) % pos(2) = ny ! to make sure the changes are saved. Do changes
    !mlist(m) % pos(3) = nd ! on local variable does not gaurantee that

    obj % pos(1) = nx ! however this works if the variable is defined
    obj % pos(2) = ny ! as a pointer
    obj % pos(3) = modulo(nd, mtp % symm) 
    !print *, "move new", obj % pos

    ! reset old points
    do i = 1, mtp % comp_num()
       comp = mtp % comps(i)
       opos = [ox,oy] + mtp % rotate(comp, od)       
       call set_sub(opos(1), opos(2), 0)
    end do
    ! assign new points
    do i = 1, mtp % comp_num()
       comp = mtp % comps(i)
       npos = [nx,ny] + mtp % rotate(comp, nd)
       t = mtp % idx_gen() ! type index generated
       c = i               ! comp-id
       s = obj % sta(i)    ! comp-state
       call set_sub(npos(1), npos(2), convert_to_land (m, t, c, s))
       !print *, m, t, c, convert_to_land (m, t, c, s), npos
    end do

    return
  end subroutine move_one

  !--------------------------------------------------------------------------- 
  ! DESCRIPTION
  !> @brief print subsrate to screen for debuging
  !--------------------------------------------------------------------------- 
  subroutine print_to_screen()
    integer :: i, j, v
    ! skip printing when substrate is too large
    if (m_xsize > 45) return 
    ! printing
    do j = 0, m_ysize+1
       !----------------------------------------------------------------------
       ! in case you want to print all substrate values
       !!write (*, "(100G2.5)") (convert_from_land(m_sub(i,j),4), i=1,m_xsize)
       !----------------------------------------------------------------------
       ! draw left boundary if you want
       write (*, "(A1)",advance="no") "|"
       do i = 1, m_xsize
          ! draw upper and lower boundary 
          if (j == 0 .or. j == m_ysize + 1) then
             write (*, "(A2)",advance="no") "=="
          else
             !< @selection 1=> mid, 2=>tid, 3=>comp 4=> state
             v = convert_from_land(m_sub(i,j),4) 
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