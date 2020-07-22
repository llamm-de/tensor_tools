# CMake Configuration file for TensorTools library
# 
# Subdirectory: src
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

# Set source files
set(SOURCE  libtt_exception.f90
            libtt_common.f90
            libtt_products.f90
            libtt_tensors_physics.f90
            libtt_precision.f90
            libtt_print.f90
            libtt_voigt.f90
            libtt_solvers_gauss.f90
            )

# Define library to build
add_library(tensortools ${SOURCE})
target_include_directories(tensortools PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})