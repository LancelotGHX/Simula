program hello

  use user_define
  use substrate
  use kmc_rates
  implicit none
  logical :: tmp
  integer :: i

  print *, ">>> Simula"
  print *, ">>> Initialization"
  call init()
  !print *, ">>> Evaporation"

  call activate_new(1)
  print *, land_one(1,3,5,0)

  call activate_new(1)
  print *, land_one(2,7,5,0)

  call activate_new(1)
  print *, land_one(3,3,8,0)

  !call evaporate(tpyp, 9)
  !call evaporate(lead, 9)

  call print_to_screen()

  print *, ">>> Calculate rate"
  do i = 1, 100
     call compute_rates()
     call print_to_screen()
     pause
  end do

  !call move_one(2, 0, -1, 0)
  !call print_to_screen()
  

  stop
end program hello
