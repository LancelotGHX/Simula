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
   integer :: comp_id
   !> initial condition
   integer :: sta_i
   integer :: pos_i (3) !> xpos, ypos, direction
   !> final condition
   integer :: sta_f
   integer :: pos_f (3) !> xpos, ypos, direction
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
   !integer :: symm       !> number of symmetry (rotation)
   !integer :: rmat (2,2) !> rotation matrix
   integer :: eva_num    !> evaporation number
   integer :: idx_def    !> user defined index for reference 
   integer :: idx_gen    !> data index in storage (handled by the program)
   integer :: dot_num    !> component number
   integer, allocatable :: dot_pos (:,:) !> component geometry (x, y, state)
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
end subroutine

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
end subroutine

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
end subroutine

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
end subroutine

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
        s = s + tlist(t) % ptr % eva_num
     end if
     mlist(i) % id   = i
     mlist(i) % pos  = 0
     mlist(i) % type = t
  end do
  !> check result
  do i = 1,n
     print *, mlist(i) % id, mlist(i) % type
  end do
  return
end subroutine init_mlist

end module type_define
