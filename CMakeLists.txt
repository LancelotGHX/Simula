cmake_minimum_required(VERSION 2.8.9)
project(Simula)
# include path
include_directories("include")
include_directories("utils/rapidxml")
# source files
set (SOURCE_FILES src/main.cpp)
# set executable
add_executable(simula ${SOURCE_FILES})