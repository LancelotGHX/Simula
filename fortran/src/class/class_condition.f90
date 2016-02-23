!-----------------------------------------------------------------------------
! 
! DESCRIPTION
!> @brief This is a class for dealing with reaction condition checking. in
!         our definition, a condition is defined by a list of position pairs,
!         which follows narutal definition: { (x_i, y_i)_i }.
!
! FIELDS
!> @var tar: target type index (generated index)
!> @var dir: position relative direction list for target with respect to
!            current object
!> @var pos: position relative position list for target with respect to
!            current object
!> @var sta: it defines the required initial states for listed components and 
!            their corresponding final states if this reaction is executed
!
!-----------------------------------------------------------------------------
module class_condition

  !---------------------------------------------------------------------------  
  !> used modules
  use helper_functions
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> changes for one reactant within one reaction (bonding)
  type, public :: condition
     integer              :: tar       ! target type
     integer, allocatable :: dir (:  ) ! dir (M,1) {direction }
     integer, allocatable :: pos (:,:) ! pos (K,2) {xpos, ypos}
     integer, allocatable :: sta (:,:) ! sta (N,3) {comp, state_i, state_j}
   contains
     procedure :: set_tar => m_set_tar
     procedure :: set_pos => m_set_pos
     procedure :: set_dir => m_set_dir
     procedure :: set_sta => m_set_sta
  end type condition

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition target type
  !> @param t: target type of the condition instance
  !---------------------------------------------------------------------------  
  subroutine m_set_tar (this, t)
    class(condition), intent (inout) :: this
    integer         , intent (in)    :: t
    this % tar = t
    return
  end subroutine m_set_tar

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition relative fetching positions
  !> @param p: values for position array in 1D form of {x1,y1,x2,y2...}
  !---------------------------------------------------------------------------  
  subroutine m_set_pos (this, p)
    class(condition), intent (inout) :: this
    integer         , intent (in)    :: p(:)
    integer                          :: n, i
    ! allocate array
    n = size (p) / 2
    call alloc_I2 (this % pos, n, 2)
    ! assign array
    do i = 1, n
       this % pos (i,1) = p (i * 2 - 1)
       this % pos (i,2) = p (i * 2    )
    end do
    return
  end subroutine m_set_pos

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition relative direction array 
  !> @param d: values for direction array in 1D form of {d1, d2, d3, ...}
  !---------------------------------------------------------------------------  
  subroutine m_set_dir (this, d)
    class(condition), intent (inout) :: this
    integer         , intent (in)    :: d(:)
    integer                          :: n
    ! allocate array
    n = size (d)
    call alloc_I1 (this % dir, n) 
    ! assign array
    this % dir = d
    return
  end subroutine m_set_dir
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition state array
  !> @param state: values for state array in 1D form of 
  !                {component_idx, initial_state, final_state, ...}
  !---------------------------------------------------------------------------  
  subroutine m_set_sta (this, s)
    class(condition), intent (inout) :: this
    integer         , intent (in)    :: s(:)
    integer                          :: n, i
    ! allocate array
    n = size(s) / 3
    call alloc_I2 (this % sta, n, 3)
    ! assign matrix
    do i = 1, n
       this % sta (i,1) = s (3 * i - 2)
       this % sta (i,2) = s (3 * i - 1)
       this % sta (i,3) = s (3 * i    )
    end do
    return
  end subroutine m_set_sta

end module class_condition
