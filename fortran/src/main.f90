program hello

  use user_define
  use substrate
  implicit none
  logical :: tmp

  print *, "Simula"

  print *, "Initialization"
  call init()

  print *, "Evaporation"

  call activate_new(1)
  print *, land_one(1,3,5,0)

  !call activate_new(1)
  !print *, land_one(2,6,5,0)

  !call evaporate(tpyp, 1)
  !call evaporate(lead, 1)

  call compute_rate()
  
  call print_to_screen()

  call move_one(1, 6, 5, 0)

  call print_to_screen()

  stop
end program hello
