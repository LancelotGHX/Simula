!-----------------------------------------------------------------------------
!
! DESCRIPTION
!> @brief This class defines the behaviors of one single general reaction. The
!         word general means it contains not only chemical bondings, but also
!         all free movements, bond breakings, etc. All those behaviors should
!         be explicitly defined either by hard coding or a NAMELIST.
!
! FIELD
!> @var idx: reaction index
!> @var ene: reaction energy
!> @var mov: reaction moving (rotation) direction / specification
!> @var cond_num: length of condition list
!> @var conds   : condition list, the reaction can be executed if and only if 
!                 all the conditions listed here are passed
!
!-----------------------------------------------------------------------------
module class_reaction

  !---------------------------------------------------------------------------
  ! used modules
  use helper_functions
  use class_condition
  implicit none
  private

  !---------------------------------------------------------------------------  
  !> descriotion for one reaction
  type, public :: reaction
     integer, private :: m_cond_num
     real(dp) :: ene     ! reaction energy
     integer  :: idx     ! reaction id for further reference
     integer  :: mov (3) ! action specification {x, y, d}
     type (condition), allocatable :: conds(:) ! no allocation means all empty
   contains
     procedure :: cond_num    => m_cond_num
     procedure :: set_idx     => m_set_idx
     procedure :: set_ene     => m_set_ene
     procedure :: set_mov     => m_set_mov
     procedure :: alloc_conds => m_alloc_conds
  end type reaction

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for reaction index
  !> @param i : reaction index (automatically generated)
  !---------------------------------------------------------------------------  
  subroutine m_set_idx (this, i)
    class(reaction), intent (inout) :: this
    integer        , intent (in)    :: i
    this % idx  = i
    return
  end subroutine m_set_idx

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for reaction energy
  !> @param e : energy value 
  !             since the default fortran real precision is short precision, a
  !             type conversion from reak(4) to real(dp) is performed here
  !---------------------------------------------------------------------------  
  subroutine m_set_ene (this, e)
    class(reaction), intent (inout) :: this
    real(4)        , intent (in)    :: e 
    this % ene = real(e, dp) ! converted short real to long real
    return
  end subroutine m_set_ene

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for reaction action
  !> @param m : moving specification
  !---------------------------------------------------------------------------  
  subroutine m_set_mov (this, m)
    class(reaction), intent (inout) :: this
    integer        , intent (in)    :: m (3)
    this % mov = m
    return
  end subroutine m_set_mov
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Allocator for conds list
  !> @param n : number of conditions
  !---------------------------------------------------------------------------  
  subroutine m_alloc_conds (this, n)
    class(reaction), intent (inout) :: this
    integer        , intent (in)    :: n
    integer                         :: status
    ! assign number value
    this % m_cond_num = n
    ! not a basic type, allocate manually
    allocate(this % conds(n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory! (class reaction)"
    return
  end subroutine m_alloc_conds

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for condition number
  !---------------------------------------------------------------------------  
  function m_cond_num (this) result (r)
    class(reaction), intent (inout) :: this
    integer :: r
    r = this % m_cond_num
    return
  end function m_cond_num
  
end module class_reaction
