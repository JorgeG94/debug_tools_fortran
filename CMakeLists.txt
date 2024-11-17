cmake_minimum_required(VERSION 3.21)
cmake_policy(SET CMP0074 NEW)
cmake_policy(SET CMP0083 NEW)
cmake_policy(SET CMP0104 NEW)
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()
cmake_policy(SET CMP0054 NEW)
cmake_policy(SET CMP0048 NEW)

# Project name and settings
project(
  printing
  VERSION 0.0.0
  LANGUAGES Fortran)

enable_language(Fortran)
enable_testing()
include(ExternalProject)
find_package(BLAS)
find_package(LAPACK)
# Set the source directory path
set(SOURCE_DIR "${CMAKE_CURRENT_SOURCE_DIR}/source")

if(CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")
    add_compile_options(-fdefault-integer-8)
elseif(CMAKE_Fortran_COMPILER_ID STREQUAL "Intel")
    add_compile_options(-integer-size 64)
endif()
set(test-drive_version 0.4.0)
ExternalProject_Add(
  test-drive
  URL https://github.com/fortran-lang/test-drive/archive/refs/tags/v${test-drive_version}.tar.gz
  PREFIX ${PROJECT_SOURCE_DIR}/external/test-drive
  CMAKE_ARGS -DCMAKE_INSTALL_PREFIX=${PROJECT_SOURCE_DIR}/lib/test-drive -DCMAKE_Fortran_FLAGS="-fdefault-integer-8")
#add_compile_options(-Wimplicit-interface -Wimplicit-procedure)
#add_compile_options(-pedantic -Wampersand -Wsurprising -std=f2018)
#add_compile_options(-g -fbacktrace -Wall -Wextra -Wunderflow -Wconversion -Wline-truncation -Wcharacter-truncation -Warray-temporaries -fcheck=all -finit-real=snan -finit-integer=-2147483648 -finit-logical=true -fimplicit-none)

# Create the static library
set(print_lib printing_library)
set(math_lib math_library)
set(debug_exec_name debug)


add_library(${print_lib} STATIC)
add_library(${math_lib} STATIC)

target_link_libraries(${math_lib} PRIVATE ${print_lib} ${PROJECT_SOURCE_DIR}/lib/test-drive/lib/libtest-drive.a)

# Set the library output and module directories
set_target_properties(${print_lib} PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)
set_target_properties(${math_lib} PROPERTIES
    ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}/lib"
    Fortran_MODULE_DIRECTORY "${CMAKE_BINARY_DIR}/modules"
)

include_directories(
  ${PROJECT_SOURCE_DIR}/lib/test-drive/include/test-drive/${CMAKE_Fortran_COMPILER_ID}-${CMAKE_Fortran_COMPILER_VERSION}
)
add_subdirectory(source)
add_subdirectory(tests)
add_executable(${debug_exec_name} ${SOURCE_DIR}/main.f90)
# Set the module directory for the executable
target_include_directories(${debug_exec_name} PRIVATE "${CMAKE_BINARY_DIR}/modules")

    #${PROJECT_SOURCE_DIR}/lib/test-drive/lib/libtest-drive.a)
# Link the library to the executable
target_link_libraries(${debug_exec_name} PRIVATE ${print_lib} BLAS::BLAS LAPACK::LAPACK ${PROJECT_SOURCE_DIR}/lib/test-drive/lib/libtest-drive.a)

