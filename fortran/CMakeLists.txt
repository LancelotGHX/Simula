project (simula-fortran)
cmake_minimum_required (VERSION 2.6)

# global variables
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${CMAKE_CURRENT_LIST_DIR}/lib)
set(SIMULA_BUILD_PATH ${PROJECT_BINARY_DIR})
set(FOX_PATH "utils/FoX-4.1.2")

# create useful directories
file(MAKE_DIRECTORY ${CMAKE_ARCHIVE_OUTPUT_DIRECTORY})

# compile fox library
add_subdirectory(${FOX_PATH})

# compile main program
add_subdirectory(main)
