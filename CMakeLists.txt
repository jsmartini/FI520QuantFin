cmake_minimum_required(VERSION 3.9)
project(FI520 VERSION 1 DESCRIPTION "QFin Class Assignments")

set(BUILD_DIR ${CMAKE_SOURCE_DIR}/build)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_STANDARD 17)

# load all of the dependencies
include_directories(${PROJECT_SOURCE_DIR}/Dep/eigen)
include_directories(${PROJECT_SOURCE_DIR}/Dep/stats/include)
include_directories(${PROJECT_SOURCE_DIR}/Dep/gcem/include)

# class specific code for developing applications
include_directories(${PROJECT_SOURCE_DIR}/QLib/inc)

add_executable(BuildSysTest Main/BuildSystemTest.cpp)
#add_executable(Assignment2 Main/assignment2.cpp)
