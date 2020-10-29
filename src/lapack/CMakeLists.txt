# CMake Configuration file for TensorTools library
# 
# Subdirectory: src/lapack
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

# Set source files
set(SOURCE  
            libtt_common_lapack.f90
            )

# Define library to build
add_library(libtt_lapack ${SOURCE})
target_include_directories(libtt_lapack PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(libtt_lapack ${LAPACK_LIBRARIES})