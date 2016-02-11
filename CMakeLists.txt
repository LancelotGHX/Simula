cmake_minimum_required(VERSION 2.8.9)
project(Simula)
# include path
include_directories("include")
include_directories("utils/rapidxml_1_13")
include_directories("utils/boost_1_60_0")
# source files
set (SOURCE_FILES src/main.cpp)
# set executable
add_executable(simula ${SOURCE_FILES})
# copy additional files
file(COPY "convert.m" DESTINATION ${CMAKE_CURRENT_BINARY_DIR})