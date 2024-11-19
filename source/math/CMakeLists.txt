add_subdirectory(numerical_recipes)
target_sources(${math_lib} PRIVATE blas.f90 solvers.f90 math_utilities.f90 blas_interfaces.f90)  
