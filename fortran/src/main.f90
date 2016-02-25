program hello

  use user_define
  use substrate
  implicit none
  logical :: tmp

  print *, "Simula"
  print *, ""

  print *, "Initialization"
  call init()
  print *, ""

  print *, "Evaporation"

  call activate_new(1)
  print *, land_one(1,6,5,0)

  !call activate_new(1)
  !print *, land_one(2,3,8,0)

  !call evaporate(tpyp, 2)
  !call evaporate(lead, 1)
  print *, ""

  call compute_rate()
  
  call print_to_screen()
  stop
end program hello
