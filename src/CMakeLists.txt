# CMake Configuration file for TensorTools library
# 
# Subdirectory: src
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

if(LIBTT_LAPACK)
    add_subdirectory(lapack)
endif()

# Set source files
set(SOURCE  libtt.f90
            libtt_exception.f90
            libtt_common.f90
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
add_library(tt ${SOURCE})
target_include_directories(tt PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
if(LIBTT_LAPACK)
    target_link_libraries(tt tt_lapack)
endif()