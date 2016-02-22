module class_mtype

  !---------------------------------------------------------------------------  
  !> used modules
  use helper_functions
  use class_reaction
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> moledule type class
  !---------------------------------------------------------------------------  
  type, public :: mtype
     integer :: symm       ! number of symmetry (rotation)
     integer :: rmat (2,2) ! rotation matrix (auto)
     integer :: eva_num    ! evaporation number
     integer :: idx_def    ! user defined index for reference 
     integer :: idx_gen    ! data index in storage (handled by the program)
     integer :: idx_offset ! molecule index offset (handled by the program)
     integer              :: comp_num    ! component number
     integer, allocatable :: comps (:,:) ! {x, y, componemt_id}
     integer                      :: react_num
     type (reaction), allocatable :: reacts (:)
   contains
     procedure :: set_symm      => mtype_set_symm
     procedure :: set_id        => mtype_set_idx_def
     procedure :: set_amount    => mtype_set_eva_num
     procedure :: alloc_comps   => mtype_set_dot_num
     procedure :: alloc_reacts  => mtype_set_react_num
  end type mtype
  !> derivated pointer type
  type :: mtype_ptr
     type (mtype), pointer :: ptr
  end type mtype_ptr
  
  type (mtype_ptr), public, allocatable :: tlist (:) ! type list

  public :: num_of_mtypes, add_new_mtype

contains

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief define molecule rotational symmetry
  !> @param this: target
  !> @param n  : symmetry number
  !---------------------------------------------------------------------------  
  subroutine mtype_set_symm (this, n)
    class(mtype) :: this
    integer      :: n
    this % symm = n
    this % rmat = 0
    if (n == 1) then
       this % rmat (1,1) = 1
       this % rmat (2,2) = 1
    else if (n == 2) then
       this % rmat (1,1) = -1
       this % rmat (2,2) = -1
    else if (n == 4) then
       this % rmat (1,2) = -1
       this % rmat (2,1) =  1
    else
       stop "ERROR: Invalid symmetry"
    end if
    return
  end subroutine mtype_set_symm

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set idx_def for mtype
  !> @param this: target
  !> @param idx: value
  !---------------------------------------------------------------------------  
  subroutine mtype_set_idx_def (this, idx)
    class(mtype) :: this
    integer      :: idx
    this % idx_def = idx
    return
  end subroutine mtype_set_idx_def

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set evaporation amount for mtype
  !> @param this: target
  !> @param num: number
  !---------------------------------------------------------------------------  
  subroutine mtype_set_eva_num (this, num)
    class(mtype) :: this
    integer      :: num
    this % eva_num = num
    return
  end subroutine mtype_set_eva_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief allocate component position for mtype
  !> @param this: target
  !> @param num: maximum component (dot) number
  !---------------------------------------------------------------------------  
  subroutine mtype_set_dot_num (this, num)
    class(mtype) :: this
    integer      :: num
    call alloc_I2 (this % comps, num, 3)
    this % comp_num = num
    this % comps    = 0
    return
  end subroutine mtype_set_dot_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief define total number of reactions and allocate react array
  !> @param obj: target
  !> @param n  : reaction number
  !---------------------------------------------------------------------------  
  subroutine mtype_set_react_num (this, n)
    class(mtype) :: this
    integer      :: n, status
    this % react_num = n
    !> not a basic type, allocate manually
    allocate (this % reacts (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"
    return
  end subroutine mtype_set_react_num

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set total number of molecule types
  !> @param n: number
  !---------------------------------------------------------------------------  
  subroutine num_of_mtypes (n)
    integer :: n, status
    allocate (tlist (n), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"
    return
  end subroutine num_of_mtypes

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief add a new type (by reference) to the tlist
  !> @param tp: the new molecule type to be added
  !---------------------------------------------------------------------------  
  subroutine add_new_mtype (tp)
    type (mtype), target :: tp
    integer, save        :: curr_size = 0
    curr_size = curr_size + 1
    tp % idx_gen = curr_size
    tlist (curr_size) % ptr => tp
  end subroutine add_new_mtype

end module class_mtype
