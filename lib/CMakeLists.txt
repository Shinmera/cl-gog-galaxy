cmake_minimum_required(VERSION 3.16.3)
project(galaxy-c)
set(CMAKE_CXX_STANDARD 11)

option(GALAXY_SDK "The Galaxy SDK root")

add_library(Galaxy SHARED IMPORTED)
set_target_properties(Galaxy PROPERTIES
    IMPORTED_LOCATION "${GALAXY_SDK}/Libraries/Galaxy64.dll"
    IMPORTED_IMPLIB "${GALAXY_SDK}/Libraries/Galaxy64.lib"
    INTERFACE_INCLUDE_DIRECTORIES "${GALAXY_SDK}/Include"
)

file(GLOB sources CONFIGURE_DEPENDS "*.cpp")
add_library(galaxy-c SHARED ${sources})
target_compile_definitions(galaxy-c PUBLIC GOG_BUILD)
target_link_libraries(galaxy-c PUBLIC Galaxy)
set_target_properties(galaxy-c PROPERTIES POSITION_INDEPENDENT_CODE TRUE)
