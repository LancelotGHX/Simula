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
type :: reactant
   integer :: type_id
   !> changes on MOLECULE CENTER position
   integer, allocatable :: pos (:,:) !> xpos, ypos, direction
   integer, allocatable :: mov (:,:) !> xpos, ypos, direction
   !> changes on COMPONENT state
   integer, allocatable :: comp_id(:)
   integer :: state_i
   integer :: state_f

end type reactant

!---------------------------------------------------------------------------  
!> descriotion for one reaction
!---------------------------------------------------------------------------  
type :: reaction
   real(8) :: energy
   integer :: id
   integer,         allocatable :: empty (:,:)
   type (reactant), allocatable :: fill  (:)
end type reaction

!---------------------------------------------------------------------------  
!> moledule type class
!---------------------------------------------------------------------------  
type :: mtype

   integer :: symm       !> number of symmetry (rotation)
   integer :: rmat (2,2) !> rotation matrix

   integer :: eva_num    !> evaporation number

   integer :: idx_def    !> user defined index for reference 
   integer :: idx_gen    !> data index in storage (handled by the program)
   integer :: idx_offset !> molecule index offset (handled by the program)

   integer              :: dot_num       !> component number
   integer, allocatable :: dot_pos (:,:) !> component geometry (x, y, state)
   
   integer                      :: react_num
   type (reaction), allocatable :: react (:)

   integer :: mov_pos (7,3) !< three rot + 4 mov (need more implementation)
end type mtype

type :: mtype_ptr
   type (mtype), pointer :: ptr
end type mtype_ptr

!---------------------------------------------------------------------------  
!> each molecule 
!---------------------------------------------------------------------------  
type :: molecule
   integer :: id
   integer :: type
   integer :: pos (3)
end type molecule

type (mtype_ptr), allocatable :: tlist (:)
type (molecule),  allocatable :: mlist (:)

contains

subroutine react_num (obj, n)
  type (mtype) :: obj
  integer      :: n, status
  obj % react_num = n
  !> allocate reaction list
  allocate (obj % react (n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"
  return
end subroutine react_num

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

  !> calculate total number of molecules
  n = 0
  do t = 1,size(tlist)
     n = n + tlist(t) % ptr % eva_num
  end do

  !> allocate list
  allocate (mlist (n), STAT = status)
  if (status /= 0) stop "ERROR: Not enough memory!"

  !> initialize molecule
  t = 0
  s = 0
  do i = 1,n
     !> find current molecule type
     if (i > s) then
        t = t + 1
        tlist(t) % ptr % idx_offset = s  !> set index offset for molecule
        s = s + tlist(t) % ptr % eva_num !> set next index counter
        !> debug check result
        print *, "initalized type", t, "=>", tlist(t) % ptr % idx_def
     end if
     mlist(i) % id   = i
     mlist(i) % pos  = 0
     mlist(i) % type = t
  end do

  return
end subroutine init_mlist

end module type_define
