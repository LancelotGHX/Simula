!-----------------------------------------------------------------------------
!
! DESCRIPTION
!> @brief This class defines all the common properties shared by the same 
!         molecule type
!
! FIELD
!  
!-----------------------------------------------------------------------------
module class_mtype

  !---------------------------------------------------------------------------  
  !> used modules
  use helper_functions
  use class_reaction
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> moledule type class
  type, public :: mtype
     ! protected fields
     integer, private :: m_rmat (2,2) ! rotation matrix (auto)
     integer, private :: m_idx_off    ! molecule index offset (auto)
     integer, private :: m_idx_gen    ! data index in storage (auto)
     integer, private :: m_comp_num   ! component number
     integer, private :: m_reac_num   ! reaction number
     ! public fields
     integer :: symm       ! number of symmetry (rotation)
     integer :: eva_num    ! evaporation number
     integer :: idx_def    ! user defined index for reference 
     integer        , allocatable :: comps (:,:) ! {x, y, component_id}
     type (reaction), allocatable :: reacs (:)   ! reaction list
   contains
     procedure :: idx_off  => m_idx_off
     procedure :: idx_gen  => m_idx_gen
     procedure :: comp_num => m_comp_num
     procedure :: reac_num => m_reac_num
     procedure :: set_symm     => m_set_symm
     procedure :: set_idx_def  => m_set_idx_def
     procedure :: set_idx_off  => m_set_idx_off
     procedure :: set_eva_num  => m_set_eva_num
     procedure :: alloc_comps  => m_alloc_comps
     procedure :: alloc_reacs  => m_alloc_reacs
     procedure :: abs_id       => m_abs_idx
     procedure :: rel_id       => m_rel_idx
     procedure :: rotate       => m_rotate
  end type mtype

  !---------------------------------------------------------------------------  
  !> derivated pointer type
  type, private :: mtype_ptr
     type (mtype), pointer :: ptr
   contains
     procedure :: alloc => m_alloc
  end type mtype_ptr
  
  !---------------------------------------------------------------------------  
  !> global type list
  type (mtype_ptr), public, allocatable, save :: tlist (:)

  !---------------------------------------------------------------------------  
  !> global functions
  public :: alloc_tlist, add_to_tlist

contains
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Rotate vector v for N times using the rotational matrix
  !> @param v: the input vector
  !> @param n: number of rotations
  !---------------------------------------------------------------------------  
  function m_rotate (this, v, n) result (r)
    class(mtype), intent (in) :: this
    integer     , intent (in) :: v(2), n
    integer                   :: r(2)
    r = rotate (this % m_rmat, v, n)
    return
  end function m_rotate
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for component number
  !---------------------------------------------------------------------------  
  function m_comp_num (this) result (r)
    class(mtype), intent (in) :: this
    integer                   :: r
    r = this % m_comp_num
    return 
  end function m_comp_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for reaction number
  !---------------------------------------------------------------------------  
  function m_reac_num (this) result (r)
    class(mtype), intent (in) :: this
    integer                   :: r
    r = this % m_reac_num
    return 
  end function m_reac_num
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for generated type index
  !---------------------------------------------------------------------------  
  function m_idx_gen (this) result (r)
    class(mtype), intent (in) :: this
    integer                   :: r
    r = this % m_idx_gen
    return 
  end function m_idx_gen

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Getter for molecule index offset
  !---------------------------------------------------------------------------  
  function m_idx_off (this) result (r)
    class(mtype), intent (in) :: this
    integer                   :: r
    r = this % m_idx_off
    return 
  end function m_idx_off
    
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief To allocate memory for pointers in tlist when importing data via
  !         an external namelist
  !---------------------------------------------------------------------------  
  subroutine m_alloc (this)
    class(mtype_ptr), intent (out) :: this
    integer                        :: status
    if (.not. associated(this  % ptr)) then
       allocate (this % ptr, STAT = status)
       if (status /= 0) stop "ERROR: Not enough memory (class mtype)!"
    else
       stop "ERROR: pointer in 'tlist' is associated to some data already!"
    end if
    return
  end subroutine m_alloc

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief function to convert relative index (index with respect to the same
  !         type) to absolute index (index with respect to all molecules)
  !---------------------------------------------------------------------------  
  function m_abs_idx (this, i) result (r)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: i
    integer                      :: r
    r = i + this % m_idx_off
    return
  end function m_abs_idx

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief function to convert absolute index (index with respect to all 
  !         molecules) to relative index (index with respect to the same
  !         type)
  !---------------------------------------------------------------------------  
  function m_rel_idx (this, i) result (r)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: i
    integer                      :: r
    r = i - this % m_idx_off
    return
  end function m_rel_idx

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for molecule rotational symmetry (range for direction, or in
  !         the other words, number of 90/30 degrees it needs for completing 
  !         one full rotation) and compute corresponding rotational matrix
  !> @param n: symmetry number
  !---------------------------------------------------------------------------  
  subroutine m_set_symm (this, n)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: n
    this % symm   = n
    this % m_rmat = 0
    if (n == 1) then
       this % m_rmat (1,1) =  1
       this % m_rmat (2,2) =  1
    else if (n == 2) then
       this % m_rmat (1,1) = -1
       this % m_rmat (2,2) = -1
    else if (n == 4) then
       this % m_rmat (1,2) = -1
       this % m_rmat (2,1) =  1
    else
       stop "ERROR: Invalid symmetry"
    end if
    return
  end subroutine m_set_symm

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for user defined type index
  !> @param i: index
  !---------------------------------------------------------------------------  
  subroutine m_set_idx_def (this, i)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: i
    this % idx_def = i
    return
  end subroutine m_set_idx_def

  !---------------------------------------------------------------------------  
  ! (private)
  ! DESCRIPTION
  !> @brief Setter for program generated type index
  !> @param i: index
  !---------------------------------------------------------------------------  
  subroutine m_set_idx_gen (this, i)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: i
    this % m_idx_gen = i
    return
  end subroutine m_set_idx_gen

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter for molecule index offset
  !> @param i: index
  !---------------------------------------------------------------------------  
  subroutine m_set_idx_off (this, i)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: i
    this % m_idx_off = i
    return
  end subroutine m_set_idx_off

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Setter type evaporation amount
  !> @param n: amount
  !---------------------------------------------------------------------------  
  subroutine m_set_eva_num (this, n)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: n
    this % eva_num = n
    return
  end subroutine m_set_eva_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Set component number and allocate comps array
  !> @param n: maximum component (dot) number
  !---------------------------------------------------------------------------  
  subroutine m_alloc_comps (this, n)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: n
    call alloc_I2 (this % comps, n, 3)
    this % m_comp_num = n
    this % comps      = 0
    return
  end subroutine m_alloc_comps

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Set total number of reactions and allocate reactions array
  !> @param n: reaction number
  !---------------------------------------------------------------------------  
  subroutine m_alloc_reacs (this, n)
    class(mtype), intent (inout) :: this
    integer     , intent (in)    :: n
    integer                      :: i, status
    ! assign value
    this % m_reac_num = n
    ! not a basic type, allocate manually
    allocate (this % reacs (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"
    ! initialization
    do i = 1, n
       call this % reacs (i) % set_idx (i)
    end do
    return
  end subroutine m_alloc_reacs

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief Allocate tlist
  !> @param n: number
  !---------------------------------------------------------------------------  
  subroutine alloc_tlist (n)
    integer, intent (in) :: n
    integer              :: status
    ! not a basic type, allocate manually
    allocate (tlist (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"
    return
  end subroutine alloc_tlist

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief add a new type (by reference) to the tlist
  !> @param tp: the new molecule type to be added
  !---------------------------------------------------------------------------  
  subroutine add_to_tlist (t)
    type (mtype), target, intent (inout) :: t
    integer     , save                   :: s = 0
    s = s + 1
    ! generate type indices
    t % m_idx_gen = s
    ! point pointer to the data
    tlist (s) % ptr => t
  end subroutine add_to_tlist

end module class_mtype
