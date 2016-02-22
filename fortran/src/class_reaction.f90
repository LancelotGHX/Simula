module class_reaction

  !---------------------------------------------------------------------------
  ! used modules
  use helper_functions
  use class_condition
  implicit none
  private

  !---------------------------------------------------------------------------  
  !> descriotion for one reaction
  !---------------------------------------------------------------------------  
  type, public :: reaction
     real(8) :: energy
     integer :: id      ! reaction id
     integer :: mov (3) ! {xpos, ypos, direction}
     integer                       :: cond_num
     type (condition), allocatable :: conds(:) ! no allocation means all empty
   contains
     procedure :: set_info    => reaction_set_info
     procedure :: alloc_conds => reaction_set_cond_num
  end type reaction

contains
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set information (id, energy, movement) for current reaction
  !> @param id    : reaction index (any number, maybe it's useless)
  !> @param energy: just the reaction energy
  !> @param mov   : moving direction (including rotation)
  !---------------------------------------------------------------------------  
  subroutine reaction_set_info (this, id, energy, mov)
    class(reaction) :: this
    integer :: id, mov (3)
    real(4) :: energy
    this % id     = id
    this % mov    = mov
    this % energy = real(energy, 8) ! converted short real to long real
    return
  end subroutine reaction_set_info

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief begin reaction definition
  !> @remark this subroutine will increase 'react_num' by one
  !> @param obj: target
  !---------------------------------------------------------------------------  
  subroutine reaction_set_cond_num(this, num)
    class(reaction) :: this
    integer :: num, status
    this % cond_num = num
    !> not a basic type, allocate manually
    allocate(this % conds(num), STAT = status)
    if (status /= 0) stop "ERROR: Not enough memory!"
    return
  end subroutine reaction_set_cond_num
  
end module class_reaction
