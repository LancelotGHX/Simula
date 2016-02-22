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
  !---------------------------------------
  Integer    , intent(in) :: num
  type(mtype), intent(in) :: mtp
  integer :: i, t, k, x, y, d
  !> debug -------------------------------
  print *, "eva start, target number", mtp % idx_def, "=>", num
  !> debug -------------------------------
  !> evaporate new molecules
  t = mtp % idx_gen !> get molecule index
  LAND_LOOP: do i = 1, num
     !> check if next landing will exceed maximum molecule number
     if (activated_num(t) >= mtp % eva_num) exit LAND_LOOP
     !> land a new molecule 
     activated_num(t) = activated_num(t) + 1
     SEARCH_LOOP: do while (.true.)
        k = activated_num(t) + mtp % idx_offset !> offset molecule id
        x = rand_int(1, XSIZE)
        y = rand_int(1, XSIZE)
        d = rand_int(1, mtp % symm)
        !> land_one will return true if landing succeed, vice versa
        if (land_one(k, x, y, d)) exit SEARCH_LOOP
     end do SEARCH_LOOP
  end do LAND_LOOP
  !> debug -------------------------------
  print *, "eva done, activated number", mtp % idx_def, "=>", activated_num(t)
  !> debug -------------------------------
  return
end subroutine