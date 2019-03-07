project(DataKam)

cmake_minimum_required(VERSION 3.3)

#set(CMAKE_GENERATOR "Unix Makefiles")
#set (CMAKE_GENERATOR "Unix Makefiles" CACHE INTERNAL "" FORCE)
#set(CMAKE_VERBOSE_MAKEFILE TRUE)

set (CMAKE_GENERATOR "Ninja")

message("generator is set to ${CMAKE_GENERATOR}")

add_subdirectory(cpp)
