!---------------------------------------------------------------------------  
!
! DESCRIPTION
!> define basic types for future use
!
!---------------------------------------------------------------------------  
module type_define
implicit none

!---------------------------------------------------------------------------  
!> changes for one reactant within one reaction (bonding)
!---------------------------------------------------------------------------
type :: condition
   integer :: type_id
   integer, allocatable :: pos   (:,:) ! (K,2) {xpos, ypos}
   integer, allocatable :: dir   (:)   ! (M,1) {direction }
   integer, allocatable :: state (:,:) ! (N,3) {comp, state_i, state_j}
end type condition

!---------------------------------------------------------------------------  
!> descriotion for one reaction
!---------------------------------------------------------------------------  
type :: reaction
   real(8) :: energy
   integer :: id      ! reaction id
   integer :: mov (3) ! {xpos, ypos, direction}
   type (condition), allocatable :: conds(:) ! no allocation means all empty
end type reaction

!---------------------------------------------------------------------------  
!> moledule type class
!---------------------------------------------------------------------------  
type :: mtype
   !-- symmetry
   integer :: symm       ! number of symmetry (rotation)
   integer :: rmat (2,2) ! rotation matrix (auto)
   !-- evaporation amount
   integer :: eva_num    ! evaporation number
   !-- indexing
   integer :: idx_def    ! user defined index for reference 
   integer :: idx_gen    ! data index in storage (handled by the program)
   integer :: idx_offset ! molecule index offset (handled by the program)
   !-- geometry
   integer              :: dot_num       ! component number
   integer, allocatable :: dot_pos (:,:) ! {x, y, componemt_id}
   !-- chemical reaction / movement
   integer                      :: react_num
   type (reaction), allocatable :: reactions (:)
end type mtype

type :: mtype_ptr
   type (mtype), pointer :: ptr
end type mtype_ptr

!---------------------------------------------------------------------------  
!> each molecule 
!---------------------------------------------------------------------------  
type :: molecule
   integer              :: id, type, pos(3)
   integer, allocatable :: states(:) 
end type molecule

!---------------------------------------------------------------------------  
!> Global variables 
!---------------------------------------------------------------------------  
type (mtype_ptr), allocatable :: tlist (:) ! type list
type (molecule),  allocatable :: mlist (:) ! molecule list

!---------------------------------------------------------------------------  
! this one should be local (handled automatically)
!---------------------------------------------------------------------------
type (reaction), pointer :: curr_react

contains

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set type id of ith condition for current reaction
!> @param idx: idicating it is the idx-th condition
!> @param num: type id number
!---------------------------------------------------------------------------  
subroutine set_react_cond_type (idx, num)
  integer :: num, idx
  if (idx > size(curr_react % conds)) stop "ERROR: Index out of bound"
  curr_react % conds(idx) % type_id = num
  return
end subroutine set_react_cond_type

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set 'pos' array for idx-th condition of current reaction
!> @param pos: value for pos array in 1D form (x1,y1,x2,y2...)
!---------------------------------------------------------------------------  
subroutine set_react_cond_pos (idx, pos)
  integer :: pos(:), idx, n, i, status
  if (idx > size(curr_react % conds)) stop "ERROR: Index out of bound"
  !> allocate array
  n = size(pos) / 2
  allocate(curr_react % conds(idx) % pos(n,2), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  !> assign matrix
  do i = 1,n
     curr_react % conds(idx) % pos(i,1) = pos(i * 2 - 1)
     curr_react % conds(idx) % pos(i,2) = pos(i * 2    )
  end do
  return
end subroutine set_react_cond_pos

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set 'dir' array for idx-th condition of current reaction
!> @param dir: value for dir array in the form of (d1, d2, d3, ...)
!---------------------------------------------------------------------------  
subroutine set_react_cond_dir (idx, dir)
  integer :: dir(:), idx, n, status
  if (idx > size(curr_react % conds)) stop "ERROR: Index out of bound"
  !> allocate array
  n = size(dir)
  allocate(curr_react % conds(idx) % dir(n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  !> assign matrix
  curr_react % conds(idx) % dir = dir
  return
end subroutine set_react_cond_dir

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set 'state' array for idx-th condition of current reaction
!> @param state: value for state array in the form of 
!    (comp_id, i_state, f_state, ...)
!---------------------------------------------------------------------------  
subroutine set_react_cond_state (idx, state)
  integer :: state(:), idx, n, i, status
  if (idx > size(curr_react % conds)) stop "ERROR: Index out of bound"
  !> allocate array
  n = size(state) / 3
  allocate(curr_react % conds(idx) % state(n,3), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  !> assign matrix
  do i = 1, n
     curr_react % conds(idx) % state (i,1) = state (3 * i - 2)
     curr_react % conds(idx) % state (i,2) = state (3 * i - 1)
     curr_react % conds(idx) % state (i,3) = state (3 * i    )
  end do
  return
end subroutine set_react_cond_state

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set information (id, energy, movement) for current reaction
!> @param id    : reaction index (any number, maybe it's useless)
!> @param energy: just the reaction energy
!> @param mov   : moving direction (including rotation)
!---------------------------------------------------------------------------  
subroutine set_react_info (id, energy, mov)
  integer, intent(in) :: id, mov (3)
  real(4), intent(in) :: energy
  call check_curr_react()  
  curr_react % id     = id
  curr_react % mov    = mov
  curr_react % energy = energy ! converted short real to long real
  return
end subroutine set_react_info

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief to check if 'curr_react' is pointing to something
!    which means 'beg_reaction' is called before
!---------------------------------------------------------------------------  
subroutine check_curr_react()
  if (.not. associated(curr_react)) then
     stop "ERROR: no reaction session found"
  end if
  return
end subroutine check_curr_react

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief begin reaction definition
!> @remark this subroutine will increase 'react_num' by one
!> @param obj: target
!---------------------------------------------------------------------------  
subroutine beg_reaction(obj, num)
  type (mtype), target :: obj
  integer :: num, status
  obj % react_num = obj % react_num + 1
  curr_react => obj % reactions (obj % react_num)
  !> allocate condition number
  allocate(curr_react % conds (num), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  return
end subroutine beg_reaction

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief end reaction definition
!---------------------------------------------------------------------------  
subroutine end_reaction()
  nullify(curr_react)
  return
end subroutine end_reaction

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief define total number of reactions
!> @param obj: target
!> @param n  : reaction number
!---------------------------------------------------------------------------  
subroutine reaction_num (obj, n)
  type (mtype) :: obj
  integer      :: n, status
  !> react_num works as a counter
  !  which will be increased when user defines reactions
  obj % react_num = 0
  !> allocate reaction list
  allocate (obj % reactions (n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  return
end subroutine reaction_num

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief define molecule rotational symmetry
!> @param obj: target
!> @param n  : symmetry number
!---------------------------------------------------------------------------  
subroutine symm (obj, n)
  type (mtype) :: obj
  integer      :: n
  obj % symm = n
  obj % rmat = 0
  if (n == 1) then
     obj % rmat (1,1) = 1
     obj % rmat (2,2) = 1
  else if (n == 2) then
     obj % rmat (1,1) = -1
     obj % rmat (2,2) = -1
  else if (n == 4) then
     obj % rmat (1,2) = -1
     obj % rmat (2,1) = 1
  else
     stop "ERROR: Invalid symmetry"
  end if
  return
end subroutine symm

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set idx_def for mtype
!> @param obj: target
!> @param idx: value
!---------------------------------------------------------------------------  
subroutine idx_def (obj, idx)
  type (mtype) :: obj
  integer      :: idx
  obj % idx_def = idx
  return
end subroutine idx_def

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set evaporation amount for mtype
!> @param obj: target
!> @param num: number
!---------------------------------------------------------------------------  
subroutine eva_num (obj, num)
  type (mtype) :: obj
  integer      :: num
  obj % eva_num = num
  return
end subroutine eva_num

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief allocate component position for mtype
!> @param obj: target
!> @param num: maximum component (dot) number
!---------------------------------------------------------------------------  
subroutine dot_num (obj, num)
  type (mtype) :: obj
  integer      :: num, status
  obj % dot_num = 0
  allocate (obj % dot_pos (num,3), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  obj % dot_pos = 0
  return
end subroutine dot_num

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set new component (dot) position and state
!> @param obj: target
!> @param x: x 
!> @param y: y
!> @param s: state
!---------------------------------------------------------------------------  
subroutine dot_pos (obj, x, y, s)
  type (mtype) :: obj
  integer      :: x, y, s
  !> array boundary check
  if (obj % dot_num > size(obj % dot_pos, 1)) then
     stop "ERROR: Index out of boundary"
  end if
  obj % dot_num = obj % dot_num + 1
  obj % dot_pos(obj % dot_num,1) = x
  obj % dot_pos(obj % dot_num,2) = y
  obj % dot_pos(obj % dot_num,3) = s
  return
end subroutine dot_pos

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set total number of molecule types
!> @param n: number
!---------------------------------------------------------------------------  
subroutine num_of_mtypes (n)
  integer :: n, status
  allocate (tlist (n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  return
end subroutine num_of_mtypes

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief add a new type (by reference) to the tlist
!> @param tp: the new molecule type to be added
!---------------------------------------------------------------------------  
subroutine add_new_mtype (tp)
  type (mtype), target :: tp
  integer, save        :: curr_size = 0
  curr_size = curr_size + 1
  tp % idx_gen = curr_size
  tlist (curr_size) % ptr => tp
end subroutine add_new_mtype

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief initialize molecule list (mlist)
!---------------------------------------------------------------------------  
subroutine init_mlist()
  integer :: n, t, s, i, status
  !---------------------------------
  !> calculate total number of molecules
  n = 0
  do t = 1, size(tlist)
     n = n + tlist(t) % ptr % eva_num
  end do
  !---------------------------------
  !> allocate list
  allocate (mlist (n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  !---------------------------------
  !> initialize molecule
  t = 0 !> molecule type index
  s = 0 !> index sum
  do i = 1, n
     !> find current molecule type
     if (i > s) then        
        t = t + 1
        tlist(t) % ptr % idx_offset = s  !> set index offset for molecule
        s = s + tlist(t) % ptr % eva_num !> set next index counter
        !> debug check result
        print *, "initalized type", t, "=>", tlist(t) % ptr % idx_def
     end if
     !> allocate state number
     allocate (mlist(i) % states (tlist(t) % ptr % dot_num), STAT = status)
     if (status /= 0) stop "ERROR: Not enough memory!"
     !> set molecule properties
     mlist(i) % id     = i
     mlist(i) % pos    = 0
     mlist(i) % type   = t
     mlist(i) % states = 0
  end do
  return
end subroutine init_mlist

end module type_define
