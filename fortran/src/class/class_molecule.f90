!---------------------------------------------------------------------------  
!
! DESCRIPTION
!> define basic types for future use
!
!---------------------------------------------------------------------------  
module class_molecule

  !---------------------------------------------------------------------------  
  !> used module
  use helper_functions
  use class_mtype
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> each molecule 
  type, public :: molecule
     integer              :: id, type, pos(3)
     integer, allocatable :: state (:) 
  end type molecule

  !---------------------------------------------------------------------------  
  !> Global variables 
  type (molecule), public,  allocatable :: mlist (:) ! molecule list

  !---------------------------------------------------------------------------  
  !> global functions
  public :: init_mlist

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief initialize molecule list (mlist)
  subroutine init_mlist()
    integer :: n, t, s, i, status
    !---------------------------------
    !> calculate total number of molecules
    n = 0
    do t = 1, size(tlist)
       n = n + tlist(t) % ptr % eva_num
    end do
    !---------------------------------
    !> not a basic type, allocate manually
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
       call alloc_I1 (mlist(i) % state, tlist(t) % ptr % comp_num)
       !> set molecule properties
       mlist(i) % id    = i
       mlist(i) % pos   = 0
       mlist(i) % type  = t
       mlist(i) % state = 0
    end do
    return
  end subroutine init_mlist

end module class_molecule