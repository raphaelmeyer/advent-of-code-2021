cmake_minimum_required(VERSION 3.20)

project(AdventOfCode2021)

include(CTest)

add_library(settings INTERFACE)
add_library(project::settings ALIAS settings)

target_compile_options(settings INTERFACE
  "-O2"
  "-std=c++20"
  "-Wall"
  "-Wextra"
  "-Werror"
)

add_subdirectory(vendor)

foreach(day "01" "02" "04")
  add_subdirectory("src/day-${day}")
endforeach()
