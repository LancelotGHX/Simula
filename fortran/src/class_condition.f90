module class_condition

  !---------------------------------------------------------------------------  
  !> used modules
  use helper_functions
  implicit none
  private
  
  !---------------------------------------------------------------------------  
  !> changes for one reactant within one reaction (bonding)
  !---------------------------------------------------------------------------
  type, public :: condition
     integer :: type
     integer, allocatable :: pos   (:,:) ! (K,2) {xpos, ypos}
     integer, allocatable :: dir   (:)   ! (M,1) {direction }
     integer, allocatable :: state (:,:) ! (N,3) {comp, state_i, state_j}
   contains
     procedure :: set_type  => condition_set_type
     procedure :: set_pos   => condition_set_pos
     procedure :: set_dir   => condition_set_dir
     procedure :: set_state => condition_set_state
  end type condition

contains
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set type id of ith condition for current reaction
  !> @param idx: idicating it is the idx-th condition
  !> @param num: type id number
  !---------------------------------------------------------------------------  
  subroutine condition_set_type (this, num)
    class(condition) :: this
    integer :: num
    this % type = num
    return
  end subroutine condition_set_type

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set 'pos' array for idx-th condition of current reaction
  !> @param pos: value for pos array in 1D form (x1,y1,x2,y2...)
  !---------------------------------------------------------------------------  
  subroutine condition_set_pos (this, pos)
    class(condition) :: this
    integer :: pos(:), n, i
    !> allocate array
    n = size(pos) / 2
    call alloc_I2(this % pos, n, 2)
    !> assign matrix
    do i = 1, n
       this % pos(i,1) = pos(i * 2 - 1)
       this % pos(i,2) = pos(i * 2    )
    end do
    return
  end subroutine condition_set_pos

  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set 'dir' array for idx-th condition of current reaction
  !> @param dir: value for dir array in the form of (d1, d2, d3, ...)
  !---------------------------------------------------------------------------  
  subroutine condition_set_dir (this, dir)
    class(condition) :: this
    integer :: dir(:), n
    !> allocate array
    n = size(dir)
    call alloc_I1 (this % dir, n) 
    !> assign matrix
    this % dir = dir
    return
  end subroutine condition_set_dir
  
  !---------------------------------------------------------------------------  
  ! DESCRIPTION
  !> @brief set 'state' array for idx-th condition of current reaction
  !> @param state: value for state array in the form of 
  !    (comp_id, i_state, f_state, ...)
  !---------------------------------------------------------------------------  
  subroutine condition_set_state (this, state)
    class(condition) :: this
    integer :: state(:), n, i
    !> allocate array
    n = size(state) / 3
    call alloc_I2 (this % state, n, 3)
    !> assign matrix
    do i = 1, n
       this % state (i,1) = state (3 * i - 2)
       this % state (i,2) = state (3 * i - 1)
       this % state (i,3) = state (3 * i    )
    end do
    return
  end subroutine condition_set_state

end module class_condition