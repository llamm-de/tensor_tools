# CMake Configuration file for TensorTools library
# 
# Subdirectory: src
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

# Set source files
set(SOURCE  libtt.f90
            libtt_exception.f90
            libtt_common.f90
            libtt_common_lapack.f90
            libtt_products.f90
            libtt_physics_elasticity.f90
            libtt_precision.f90
            libtt_print.f90
            libtt_voigt.f90
            libtt_solvers_gauss.f90
            libtt_solvers_eigenproblem.f90
            libtt_logical.f90
            libtt_constants.f90
            libtt_physics_transformation.f90
            )

# Define library to build
add_library(libtt ${SOURCE})
target_include_directories(libtt PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(libtt ${LAPACK_LIBRARIES})