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

  ! call activate_new(1)
  ! print *, land_one(1,3,5,0)

  ! call activate_new(1)
  ! print *, land_one(2,7,5,0)

  ! call activate_new(1)
  ! print *, land_one(3,4,8,0)

  call evaporate(tpyp, 50)
  call evaporate(lead, 50)

  !call print_to_screen()

  print *, ">>> Calculate rate"
  do i = 1, 10000
     call compute_rates()
     !call print_to_screen()
  end do
  !call print_to_screen()

  !call move_one(2, 0, -1, 0)
  !call print_to_screen()
  
  stop
end program hello
