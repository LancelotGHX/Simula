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
!            their corresponding final states if this reaction is executed. it
!            is constructed in the form of {comp, state_i, state_j}
!-----------------------------------------------------------------------------
module class_condition

  !---------------------------------------------------------------------------  
  !> used modules
  use helper_functions, only: alloc_I1, alloc_I2
  implicit none
  private

  !---------------------------------------------------------------------------  
  !> changes for one reactant within one reaction (bonding)  
  type, private :: m_cond_info
     integer, allocatable :: dir (:  ) ! dir (1,M) {direction }
     integer, allocatable :: pos (:,:) ! pos (2,K) {xpos, ypos}
   contains
     procedure :: set_pos => m_info_set_pos
     procedure :: set_dir => m_info_set_dir
     procedure :: set     => m_info_set
  end type m_cond_info

  type, public :: condition
     ! private
     integer, private :: m_opt_num
     ! public
     integer                        :: tar       ! target type
     integer          , allocatable :: sta (:,:) ! sta (3,N)
     type(m_cond_info), allocatable :: opt (:)
   contains
     procedure :: set_tar   => m_set_tar
     procedure :: set_sta   => m_set_sta
     procedure :: opt_num   => m_get_opt_num
     procedure :: alloc_opt => m_alloc_opt
  end type condition

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for option number
  !---------------------------------------------------------------------------  
  function m_get_opt_num (this) result (r)
    class(condition), intent (inout) :: this
    integer                          :: r
    r = this % m_opt_num
    return
  end function m_get_opt_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Allocator for option list
  !> @param n: number of options 
  !---------------------------------------------------------------------------  
  subroutine m_alloc_opt (this, n)
    class(condition), intent (inout) :: this
    integer         , intent (in)    :: n
    integer                          :: status
    ! check allocation
    if (allocated(this % opt)) stop "ERROR: multiple definitions"
    ! allocate memory space
    allocate (this % opt (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory"
    ! assign values
    this % m_opt_num = n
    return
  end subroutine m_alloc_opt

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
    call alloc_I2 (this % sta, 3, n)
    ! assign matrix
    do i = 1, n
       this % sta (1,i) = s (3 * i - 2)
       this % sta (2,i) = s (3 * i - 1)
       this % sta (3,i) = s (3 * i    )
    end do
    return
  end subroutine m_set_sta

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition relative fetching positions
  !> @param p: values for position array in 1D form of {x1,y1,x2,y2...}
  !---------------------------------------------------------------------------  
  subroutine m_info_set_pos (this, p)
    class(m_cond_info), intent (inout) :: this
    integer           , intent (in)    :: p(:)
    integer                            :: n, i
    ! allocate array
    n = size (p) / 2
    call alloc_I2 (this % pos, 2, n)
    ! assign array
    do i = 1, n
       this % pos (1,i) = p (i * 2 - 1)
       this % pos (2,i) = p (i * 2    )
    end do
    return
  end subroutine m_info_set_pos

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition relative direction array 
  !> @param d: values for direction array in 1D form of {d1, d2, d3, ...}
  !---------------------------------------------------------------------------  
  subroutine m_info_set_dir (this, d)
    class(m_cond_info), intent (inout) :: this
    integer           , intent (in)    :: d(:)
    integer                            :: n
    ! allocate array
    n = size (d)
    call alloc_I1 (this % dir, n) 
    ! assign array
    this % dir = d
    return
  end subroutine m_info_set_dir

  subroutine m_info_set (this, p, d)
    class(m_cond_info), intent (inout) :: this
    integer           , intent (in)    :: p(:)
    integer           , intent (in)    :: d(:)
    call this % set_pos(p)
    call this % set_dir(d)
    return
  end subroutine m_info_set

end module class_condition
