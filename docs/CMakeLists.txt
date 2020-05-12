# CMake Configuration file for TensorTools library
# 
# Subdirectory: docs
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

# set input and output files
set(DOXYFILE_IN ${CMAKE_CURRENT_SOURCE_DIR}/Doxyfile.in)
set(DOXYFILE ${CMAKE_CURRENT_BINARY_DIR}/Doxyfile)

# request to configure the file
configure_file(${DOXYFILE_IN} ${DOXYFILE} @ONLY)

# note the option ALL which allows to build the docs together with the
# application
add_custom_target(docs
                  COMMAND ${DOXYGEN_EXECUTABLE} ${DOXYFILE}
                  WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
                  COMMENT "Generating documentation with Doxygen"
                  VERBATIM )

