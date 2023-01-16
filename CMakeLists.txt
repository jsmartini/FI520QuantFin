cmake_minimum_required(VERSION 3.9)
project(FI520 VERSION 1 DESCRIPTION "QFin Class Assignments")

set(BUILD_DIR ${CMAKE_SOURCE_DIR}/build)
set(CMAKE_VERBOSE_MAKEFILE ON)
set(CMAKE_CXX_STANDARD 17)

# load all of the dependencies
include_directories(${PROJECT_SOURCE_DIR}/Dep/eigen)
include_directories(${PROJECT_SOURCE_DIR}/Dep/stats/include)
include_directories(${PROJECT_SOURCE_DIR}/Dep/gcem/include)
include_directories(${PROJECT_SOURCE_DIR}/Dep/boost)

# class specific code for developing applications
include_directories(${PROJECT_SOURCE_DIR}/QLib/inc)

#pybind11 libraries
add_subdirectory(${PROJECT_SOURCE_DIR}/Dep/pybind11)
pybind11_add_module(QLib ${PROJECT_SOURCE_DIR}/src/bindings.cpp)

# make sure all of the headers are working
add_executable(BuildSysTest ${PROJECT_SOURCE_DIR}/Binaries/BuildSystemTest.cpp)

# build bindings
target_compile_definitions(QLib PRIVATE VERSION_INFO=${EXAMPLE_VERSION_INFO})
