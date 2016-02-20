program hello
  use type_define
  use user_define
  use substrate

  print *, "Simula"
  print *, ""

  print *, "Initialization"
  call init()
  print *, ""

  print *, "Evaporation"
  call evaporate(tpyp, 100)
  call evaporate(lead, 100)
  print *, ""

  stop
end program hello
