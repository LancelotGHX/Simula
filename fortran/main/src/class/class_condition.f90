!-----------------------------------------------------------------------------
! 
! DESCRIPTION
!> @brief This is a class for dealing with reaction condition checking
!
! FIELDS
!> @var tar: target type index (generated index)
!
!> @var sta: it defines the required initial states and final states for
!            executing the reaction. it follows the defining order of 
!            molecule components
!
!> @var opt: option list. it defines all the possible conditions in detail.
!            the condition is determined to be ture if at least one of the 
!            options is observed to be true
!
!> @var opt -> pos: relative position list for targets with respect to current
!                   object. It requires all the points should be filled with
!                   identical objects. If you want to have different possible
!                   directions, add more options please
!
!> @var opt -> dir: position relative direction list for target with respect
!                   to current object. It lists all the possible directions
!                   that the target can choose in order to execute the reaction
!
!-----------------------------------------------------------------------------
module class_condition

  !---------------------------------------------------------------------------  
  !> used modules
  use func_helper, only: alloc_I1, alloc_I2
  implicit none
  private

  !---------------------------------------------------------------------------  
  ! (private)
  !> option information for condition
  type, private :: m_cond_info
     ! private
     integer, private              :: m_pos_num
     integer, private, allocatable :: m_pos (:,:) ! pos (2,K) {xpos, ypos}
     ! public
     integer, public               :: dir
   contains
     procedure :: dir_not_equal => m_info_dir_if_not_equal   
     ! getters
     procedure :: pos_num       => m_info_get_pos_num     
     procedure :: pos           => m_info_get_pos
     ! setters
     procedure :: set           => m_info_set
     procedure :: set_pos       => m_info_set_pos
     procedure :: set_dir       => m_info_set_dir
  end type m_cond_info

  !---------------------------------------------------------------------------  
  !> changes for one reactant within one reaction (bonding)  
  type, public :: condition
     ! private
     integer, private              :: m_opt_num
     integer, private              :: m_sta_num
     integer, private, allocatable :: m_sta (:,:) ! sta (3,N)
     ! public
     integer                        :: tar       ! target type
     type(m_cond_info), allocatable :: opt (:)
   contains
     procedure :: sta_not_equal => m_sta_if_not_equal
     procedure :: tar_not_equal => m_tar_if_not_equal
     ! size getter
     procedure :: sta_num       => m_get_sta_num
     procedure :: opt_num       => m_get_opt_num
     ! getter
     procedure :: sta           => m_get_sta
     procedure :: sta_final     => m_sta_final       
     ! setter
     procedure :: set_tar       => m_set_tar
     procedure :: set_sta       => m_set_sta
     ! allocator
     procedure :: alloc_opt     => m_alloc_opt
  end type condition

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for option number
  !---------------------------------------------------------------------------  
  function m_get_opt_num (this) result (r)
    class(condition), intent (in) :: this
    integer                       :: r
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
  !> @brief check if the condition's type is equal to t
  !> @param c_obj: the condition object need to be tested
  !> @param t    : type
  !---------------------------------------------------------------------------
  function m_tar_if_not_equal (this, t) result (r)
    class(condition), intent (in) :: this
    integer         , intent (in) :: t
    logical                       :: r
    r = (this % tar /= t)
    return
  end function m_tar_if_not_equal

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
    call alloc_I2 (this % m_sta, 3, n)
    ! assign matrix
    do i = 1, n
       this % m_sta (1,i) = s (3 * i - 2)
       this % m_sta (2,i) = s (3 * i - 1)
       this % m_sta (3,i) = s (3 * i    )
    end do
    ! assignment
    this % m_sta_num = n
    return
  end subroutine m_set_sta

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for condition state array
  !> @param i: ith vector
  !---------------------------------------------------------------------------  
  function m_get_sta (this, i) result (r)
    class(condition), intent (in) :: this
    integer         , intent (in) :: i
    integer                       :: r(3)
    r = this % m_sta(:, i)
    return
  end function m_get_sta

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for condition state array size
  !---------------------------------------------------------------------------  
  function m_get_sta_num (this) result (r)
    class(condition), intent (in) :: this
    integer                       :: r
    r = this % m_sta_num
    return
  end function m_get_sta_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief check if component initial states fit the requirement
  !---------------------------------------------------------------------------  
  function m_sta_if_not_equal (this, comp) result (r)
    class(condition)    , intent (in) :: this
    integer, allocatable, intent (in) :: comp(:)
    logical                           :: r
    if (this % m_sta_num /= size(comp)) then
       r = .true.
    else 
       r = .not. all(comp == this % m_sta(2,:))
    end if

    return
  end function m_sta_if_not_equal

  subroutine m_sta_final (this, comp)
    class(condition)      , intent (in)  :: this
    integer, allocatable                :: comp(:)
    if (this % m_sta_num /= size(comp)) then
       print *, "self sta ", this % m_sta
       print *, "self type", this % tar
       print *, "target ", comp
       stop "Component shape inconsistent"
    end if
    comp = this % m_sta(3,:)
    return
  end subroutine m_sta_final

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
    call alloc_I2 (this % m_pos, 2, n)
    ! assign array
    do i = 1, n
       this % m_pos (1,i) = p (i * 2 - 1)
       this % m_pos (2,i) = p (i * 2    )
    end do
    ! assignment
    this % m_pos_num = n
    return
  end subroutine m_info_set_pos

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for condition relative fetching positions
  !> @param i: the ith pair
  !---------------------------------------------------------------------------  
  function m_info_get_pos (this, i) result (r)
    class(m_cond_info), intent (in) :: this
    integer           , intent (in) :: i
    integer                         :: r(2)
    r = this % m_pos(1:2,i)
    return
  end function m_info_get_pos

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for condition relative fetching positions size
  !---------------------------------------------------------------------------  
  function m_info_get_pos_num (this) result (r)
    class(m_cond_info), intent (in) :: this
    integer                         :: r
    r = this % m_pos_num
    return
  end function m_info_get_pos_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for condition relative direction array 
  !> @param d: value for direction
  !---------------------------------------------------------------------------  
  subroutine m_info_set_dir (this, d)
    class(m_cond_info), intent (inout) :: this
    integer           , intent (in)    :: d
    this % dir = d
    return
  end subroutine m_info_set_dir

  !---------------------------------------------------------------------------
  ! DESCRIPTION
  !> @brief check if 'dir' array all do not equal to the value d
  !> @param d: direction value to be tested
  !---------------------------------------------------------------------------
  function m_info_dir_if_not_equal (this, d) result (r)
    class(m_cond_info), intent (in) :: this
    integer           , intent (in) :: d
    logical                         :: r
    r = this % dir /= d
    return
  end function m_info_dir_if_not_equal

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief General setter for position and direction
  !> @param p: values for position array in 1D form of {x1,y1,x2,y2...}
  !> @param d: values for direction
  !---------------------------------------------------------------------------  
  subroutine m_info_set (this, p, d)
    class(m_cond_info), intent (inout) :: this
    integer           , intent (in)    :: p(:)
    integer           , intent (in)    :: d
    call this % set_pos(p)
    call this % set_dir(d)
    return
  end subroutine m_info_set

end module class_condition
