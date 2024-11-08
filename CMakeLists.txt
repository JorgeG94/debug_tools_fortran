cmake_minimum_required(VERSION 3.21)

# Project name and settings
project(
  printing
  VERSION 0.0.0
  LANGUAGES Fortran)

enable_language(Fortran)
find_package(BLAS)
# Set the source directory path
set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/source")

# List all source files
set(SOURCES
    ${SOURCE_DIR}/debugging_tools.f90
    ${SOURCE_DIR}/printing_utilities.f90
    ${SOURCE_DIR}/string_utilities.f90
    ${SOURCE_DIR}/output_module.f90
    ${SOURCE_DIR}/output_helpers.f90
    ${SOURCE_DIR}/math.f90
    ${SOURCE_DIR}/solvers.f90
    ${SOURCE_DIR}/tftri.f
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

add_executable(test ${SOURCE_DIR}/main.f90)
add_executable(compare ${SOURCE_DIR}/compare_math.f90)
add_executable(compare_solvers ${SOURCE_DIR}/compare_solvers.f90)
# Set the module directory for the executable
target_include_directories(test PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_include_directories(compare PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_include_directories(compare_solvers PRIVATE "${CMAKE_BINARY_DIR}/modules")

# Link the library to the executable
target_link_libraries(test PRIVATE printing_library BLAS::BLAS)
target_link_libraries(compare PRIVATE printing_library BLAS::BLAS)
target_link_libraries(compare_solvers PRIVATE printing_library BLAS::BLAS)
