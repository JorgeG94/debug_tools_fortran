cmake_minimum_required(VERSION 3.21)

# Project name and settings
project(
  printing
  VERSION 0.0.0
  LANGUAGES Fortran)

enable_language(Fortran)

# Set the source directory path
set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/source")

# List all source files
set(SOURCES
    ${SOURCE_DIR}/debugging_tools.f90
    ${SOURCE_DIR}/printing_utilities.f90
    ${SOURCE_DIR}/string_utilities.f90
)

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdefault-integer-8)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    add_compile_options(-integer-size 64)
endif()

# Create the static library
add_library(printing_library STATIC ${SOURCES})

# Set the library output and module directories
set_target_properties(printing_library PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)
