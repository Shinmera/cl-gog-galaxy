cmake_minimum_required(VERSION 3.16.3)
project(galaxy-c)

option(GALAXY_SDK "The Galaxy SDK root")

file(GLOB_RECURSE sources CONFIGURE_DEPENDS "*.cpp")
add_library(galaxy-c SHARED ${sources})
target_link_libraries(galaxy-c Galaxy64)
target_include_directories(galaxy-c PRIVATE ${GALAXY_SDK}/Include/)
target_link_directories(galaxy-c PRIVATE ${GALAXY_SDK}/Libraries/)
