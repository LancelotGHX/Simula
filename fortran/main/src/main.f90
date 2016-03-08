program main

  use FoX_dom
  use func_substrate
  use func_rate_kmc
  use define
  implicit none

  integer :: i

  !call set_root_dir("/home/qiwu/work/dev/simula/fortran/output")
  !call set_proj_dir("test-qiwu")

  print *, ">>> Simula"
  print *, ">>> Initialization"
  call init()
  print *, ">>> Evaporation"

  call activate_new(1)
  print *, land_one(1, 7,5,0)
  call activate_new(1)
  print *, land_one(2,11,5,2)
  call activate_new(1)
  print *, land_one(3, 4,5,0)

  !call evaporate(tpyp, 50)
  !call evaporate(lead, 50)
  call print_to(6, 4)

  print *, ">>> Calculate rate"
  print *, ""
  do i = 1, 1
     call compute_rates(verbose = .false.)
  !    if (modulo(i, 1000)==0) then
  !       !call start_file(90)
  !       !call print_to(90,4)
  !       !call close_file(90)
  !    end if
  end do
  print *, ""
  call print_to(6, 4)
  
  stop
end program main
