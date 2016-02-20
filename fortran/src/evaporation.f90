!---------------------------------------------------------------------------  
!
! DESCRIPTION: 
!> @brief Define subroutine for evaporation
!> @param mtp: copy/pointer to molecule type
!> @param num: number of molecules will be evaporate
!
!--------------------------------------------------------------------------- 
subroutine evaporate (mtp, num)
  use helper_functions
  use type_define
  use substrate
  implicit none

  integer, save, allocatable :: curr_num (:)
  logical, save              :: first_call = .true.
  integer                    :: num_of_types, status
  integer    , intent(in) :: num
  type(mtype), intent(in) :: mtp
  integer :: i, t, k, x, y, d

  !> first call initialization
  if (first_call) then
     !> allocation check
     num_of_types = size(tlist)
     allocate (curr_num (num_of_types), STAT = status)
     if (status /= 0) stop "ERROR: Not enough memory!"
     !> initialize evaporated molecule number
     curr_num = 0
     first_call = .false.
  end if

  !> debug
  print *, "eva start, target number", mtp % idx_def, "=>", num

  !> evaporate new molecules
  t = mtp % idx_gen !> get molecule index

  LAND_LOOP: do i = 1, num
     !> check if next landing will exceed maximum molecule number
     if (curr_num(t) >= mtp % eva_num) exit LAND_LOOP
     !> land a new molecule 
     curr_num(t) = curr_num(t) + 1
     SEARCH_LOOP: do while (.true.)
        k = curr_num(t) + mtp % idx_offset !> calculate molecule id in storage
        x = rand_int(1, XSIZE)
        y = rand_int(1, XSIZE)
        d = rand_int(1, mtp % symm)
        !> land_one will return true if landing succeed, vice versa
        if (land_one(k, x, y, d)) exit SEARCH_LOOP
     end do SEARCH_LOOP
  end do LAND_LOOP

  !> debug print current landed molecule
  print *, "eva done, current number", mtp % idx_def, "=>", curr_num(t)

  return
end subroutine
