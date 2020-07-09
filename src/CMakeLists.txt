# CMake Configuration file for TensorTools library
# 
# Subdirectory: src
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

# Set source files
set(SOURCE libtt_common.f90
           libtt_products.f90)

# Define library to build
add_library(tensortools STATIC ${SOURCE})
target_include_directories(tensortools PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})