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
add_library(tt_lapack ${SOURCE})
target_include_directories(tt_lapack PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})
target_link_libraries(tt_lapack ${LAPACK_LIBRARIES})