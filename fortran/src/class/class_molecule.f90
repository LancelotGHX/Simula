!-----------------------------------------------------------------------------  
!
! DESCRIPTION
!> This class defines the behaviors of one molecule
!
! FIELD
!
!-----------------------------------------------------------------------------  
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
     integer              :: idx
     integer              :: pos (3)
     integer              :: type
     integer, allocatable :: stas (:) 
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
  !---------------------------------------------------------------------------  
  subroutine init_mlist()
    integer :: n         ! total number of molecules
    integer :: s         ! index step sum 
    integer :: t         ! molecule type
    integer :: i, status

    ! calculate total number of molecules
    n = 0
    EACH_TYPE: do t = 1, size(tlist)
       n = n + tlist(t) % ptr % eva_num
    end do EACH_TYPE

    ! not a basic type, allocate manually
    allocate (mlist (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"

    ! initialize molecule
    t = 0 
    s = 0 
    EACH_MOLECULE: do i = 1, n
       ! find current molecule type
       if (i > s) then        
          t = t + 1
          call tlist(t) % ptr % set_idx_off (s)  ! index offset for molecule
          s = s + tlist(t) % ptr % eva_num       ! set next index step
       end if
       ! allocate state number
       call alloc_I1 ( mlist(i) % stas, tlist(t) % ptr % comp_num() )
       ! set molecule properties
       mlist(i) % idx  = i
       mlist(i) % pos  = 0
       mlist(i) % type = t
       mlist(i) % stas = 0
    end do EACH_MOLECULE

    return
  end subroutine init_mlist

end module class_molecule
