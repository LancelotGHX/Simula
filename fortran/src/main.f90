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

  activated_num(1) = activated_num(1) + 1
  print *, land_one(1,6,5,0)

  activated_num(2) = activated_num(2) + 1
  print *, land_one(2,3,5,0)

  !call evaporate(tpyp, 3)
  !call evaporate(lead, 1)
  print *, ""

  call compute_rate()
  
  call print_to_screen()
  stop
end program hello
