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
   integer :: symm       !> number of symmetry (rotation)
   integer :: rmat (2,2) !> rotation matrix
   integer :: idx_def    !> user defined index for reference 
   integer :: idx_gen    !> data index in storage (handled by the program)
   integer :: dot_num    !> component number
   integer, allocatable :: dot_pos (:,:) !> component geometry (x, y, state)
end type mtype

contains

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief set idx_def for mtype
!> @param obj: target
!> @param idx: value
!---------------------------------------------------------------------------  
subroutine idx_def(obj, idx)
  type (mtype) :: obj
  integer      :: idx
  obj % idx_def = idx
  return
end subroutine

!---------------------------------------------------------------------------  
! DESCRIPTION
!> @brief allocate component position for mtype
!> @param obj: target
!> @param num: maximum component (dot) number
!---------------------------------------------------------------------------  
subroutine dot_num(obj, num)
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
!> @param i: state
!---------------------------------------------------------------------------  
subroutine dot_pos(obj, x, y, i)
  type (mtype) :: obj
  integer      :: x, y, i
  if (i > size(obj % dot_pos, 1)) stop "ERROR: Index out of boundary"
  obj % dot_num = obj % dot_num + 1
  obj % dot_pos(obj % dot_num,1) = x
  obj % dot_pos(obj % dot_num,2) = y
  obj % dot_pos(obj % dot_num,2) = i
  return
end subroutine

end module type_define
