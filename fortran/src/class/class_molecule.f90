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
  use helper_functions, only: alloc_I1
  use class_mtype     , only: mtype, tlist
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> each molecule 
  type, public :: molecule
     ! public
     integer               :: type
     integer               :: idx
     integer               :: pos (3)
     integer, allocatable :: sta (:)
   contains
     procedure :: sta_num => m_get_sta_num
  end type molecule

  !---------------------------------------------------------------------------  
  !> Global variables 
  type (molecule), public, allocatable, target :: mlist (:) ! molecule list

  !---------------------------------------------------------------------------  
  !> global functions
  public :: init_mlist, mlist_num

contains
  ! DESCRIPTION
  !> @brief Getter for tlist size
  !---------------------------------------------------------------------------
  function mlist_num () result (r)
    integer :: r
    r = size(mlist)
    return
  end function mlist_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for state list size
  !---------------------------------------------------------------------------  
  function m_get_sta_num (this) result (r)
    class(molecule), intent (in) :: this
    integer                      :: r
    r = size(this % sta)
    return
  end function m_get_sta_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief initialize molecule list (mlist)
  !---------------------------------------------------------------------------  
  subroutine init_mlist()
    integer :: n         ! total number of molecules
    integer :: s         ! index step sum 
    integer :: t         ! molecule type
    integer :: i, status
    type(molecule),pointer :: debug_m_obj

    ! calculate total number of molecules
    n = 0
    EACH_TYPE: do t = 1, size(tlist)-1
       n = n + tlist(t) % ptr % eva_num
    end do EACH_TYPE

    ! Printing total number of molecules
    write (*,'(" No. of molecules", I6)') n

    ! not a basic type, allocate manually
    ! @remark id = 0 is researved for background type
    if (allocated(mlist)) stop "ERROR: multiple definitions"
    allocate (mlist (0:n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"

    ! initialize molecule
    t = 0
    s = 0 
    EACH_MOLECULE: do i = 0, n
       ! @remark update current molecule type. this check should always be
       !         done before other things. it will ignore the first case
       if (i > s) then 
          t = t + 1
          call tlist(t) % ptr % set_idx_off(s)   ! index offset for molecule
          s = s + tlist(t) % ptr % eva_num       ! set next index step
       end if
       ! allocate state number
       debug_m_obj => mlist(i)
       call alloc_I1 ( mlist(i) % sta, tlist(t) % ptr % comp_num() )
       ! set molecule properties
       mlist(i) % type = t
       mlist(i) % idx  = i
       mlist(i) % pos  = 0
       if (i == 0) then
          mlist(i) % sta  = 0 ! initial state for background
       else
          mlist(i) % sta  = 1 ! initial state
       end if
    end do EACH_MOLECULE

    return
  end subroutine init_mlist

end module class_molecule
