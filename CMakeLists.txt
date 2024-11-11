cmake_minimum_required(VERSION 3.21)

# Project name and settings
project(
  printing
  VERSION 0.0.0
  LANGUAGES Fortran)

enable_language(Fortran)
find_package(BLAS)
find_package(LAPACK)
# Set the source directory path
set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/source")

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdefault-integer-8)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    add_compile_options(-integer-size 64)
endif()
#-Wimplicit-interface -Wimplicit-procedure
add_compile_options(-g -fbacktrace -Wall -Wextra -Wunderflow -Wconversion -Wline-truncation -Wcharacter-truncation -Warray-temporaries -fcheck=all -finit-real=snan -finit-integer=-2147483648 -finit-logical=true -fimplicit-none)

# Create the static library
set(print_lib printing_library)
set(math_lib math_library)
set(debug_exec_name debug)
set(compare_blas_name compare_blas)
set(compare_solver_name compare_solver)

add_library(${print_lib} STATIC)
add_library(${math_lib} STATIC)


# Set the library output and module directories
set_target_properties(${print_lib} PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)
set_target_properties(${math_lib} PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)

add_executable(${debug_exec_name} ${SOURCE_DIR}/main.f90)
add_executable(${compare_blas_name} ${SOURCE_DIR}/compare_math.f90)
add_executable(${compare_solver_name} ${SOURCE_DIR}/compare_solvers.f90)
# Set the module directory for the executable
target_include_directories(${debug_exec_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_include_directories(${compare_blas_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")
target_include_directories(${compare_solver_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

# Link the library to the executable
target_link_libraries(${debug_exec_name} PRIVATE ${print_lib} BLAS::BLAS LAPACK::LAPACK)
target_link_libraries(${compare_blas_name}  PRIVATE ${print_lib} ${math_lib} BLAS::BLAS LAPACK::LAPACK)
target_link_libraries(${compare_solver_name}  PRIVATE ${print_lib} ${math_lib}  BLAS::BLAS LAPACK::LAPACK)

add_subdirectory(source)
