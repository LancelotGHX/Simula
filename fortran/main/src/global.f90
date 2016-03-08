!-----------------------------------------------------------------------------
!
! DESCRIPTION
!> @brief definitions for global variables and their setters
!
!-----------------------------------------------------------------------------
module global

  implicit none
  public

  !---------------------------------------------------------------------------
  !> @var it defines the substrate symmetry, which will be related to 
  !       rotational generation and substrate indexing
  integer, save :: SYS_SYMM = 4

contains

  !---------------------------------------------------------------------------
  ! DESCRIPTION
  !> @brief system symmetry setter
  !---------------------------------------------------------------------------
  subroutine set_sys_symm(s)
    integer, intent (in) :: s
    SYS_SYMM = s
    return
  end subroutine set_sys_symm

end module global
