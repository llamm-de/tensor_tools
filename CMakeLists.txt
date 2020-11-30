# CMake Configuration file for TensorTools library
#
# Version: 0.1.0 (04/2020)
# Author:  Lukas Lamm 

#----------------------------------------------------------------------#
# Define Cmake version
#----------------------------------------------------------------------#
cmake_minimum_required(VERSION 3.1...3.15)
if(${CMAKE_VERSION} VERSION_LESS 3.12)
    cmake_policy(VERSION ${CMAKE_MAJOR_VERSION}.${CMAKE_MINOR_VERSION})
endif()

#----------------------------------------------------------------------#
# Set default installation directory
#----------------------------------------------------------------------#
#if (CMAKE_INSTALL_PREFIX_INITIALIZED_TO_DEFAULT)
#    set (CMAKE_INSTALL_PREFIX "${CMAKE_BINARY_DIR}/install" CACHE PATH "default install path" FORCE)
#    message(STATUS "LIBTT: Setting default install prefix to ${CMAKE_INSTALL_PREFIX} \n
#                      Override with -DCMAKE_INSTALL_PREFIX=<path>.")
#endif()

#----------------------------------------------------------------------#
# Project & language definition
#----------------------------------------------------------------------#
project(TensorTools VERSION 0.1.0
                    DESCRIPTION "A small modern fortran library for tensor calculus"
                    LANGUAGES NONE)
enable_language(Fortran)

#----------------------------------------------------------------------#
# Set default compilaation to debug
#----------------------------------------------------------------------#
if(NOT DEFINED CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Debug
        CACHE STRING "Choose type of build: Debug")
else()
    set(CMAKE_BUILD_TYPE ${CMAKE_BUILD_TYPE}
        CACHE STRING "Choose type of build: ${CMAKE_BUILD_TYPE}")
endif()

#----------------------------------------------------------------------#
# Set compiler options
#----------------------------------------------------------------------#
include(cmake_config/${CMAKE_Fortran_COMPILER_ID}.cmake RESULT_VARIABLE found)

if(NOT found)
    message(FATAL_ERROR "Unrecognized Fortran compiler. Only gfortran and ifort are supperted!")
else()
    message(STATUS "${CMAKE_Fortran_COMPILER_ID} compiler selected.")
endif()

#----------------------------------------------------------------------#
# Set directory for Fortran modules
#----------------------------------------------------------------------#
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/modules)

#----------------------------------------------------------------------#
# Find LAPACK
#----------------------------------------------------------------------#
option(LIBTT_LAPACK "Enable/Disable Lapack routines" OFF)
if(LIBTT_LAPACK)
    find_package(LAPACK)
    if(NOT LAPACK_FOUND)
        message(FATAL_ERROR "LAPACK not found! Please install Lapack first!")
    endif()
else()
    message(STATUS "No Lapack routines will be build. \n   Enable with -DLIBTT_LAPACK=ON")
endif()

#----------------------------------------------------------------------#
# Add subdirectories
#----------------------------------------------------------------------#
# Source files
add_subdirectory(src)

#----------------------------------------------------------------------#
# Testing
# Examples: https://github.com/Goddard-Fortran-Ecosystem/pFUnit_demos/tree/main/Basic
#----------------------------------------------------------------------#
option(LIBTT_TESTS "Enable/disable testing framework" OFF)
if(LIBTT_TESTS)
    find_package(PFUNIT REQUIRED)
    enable_testing()
    add_subdirectory(tests)
    message(STATUS "LIBTT: Tests enabled \n   Override with -DLIBTT_TESTS=OFF")
endif()

#----------------------------------------------------------------------#
# Documentation
# Only enable documentation if it is the main package
# Needed: doxygen & graphviz
#----------------------------------------------------------------------#
option(LIBTT_DOCS "Enable/disable documentation" OFF)
if((CMAKE_PROJECT_NAME STREQUAL PROJECT_NAME) AND LIBTT_DOCS)
    find_package(Doxygen)
    if(Doxygen_FOUND)
        add_subdirectory(docs)
        message(STATUS "LIBTT: Documentation enabled \n   Override with -DLIBTT_DOCS=OFF")
    else()
        message(STATUS "LIBTT: Doxygen not found, not building docs")
    endif()
endif()



#----------------------------------------------------------------------#
# Installation
#----------------------------------------------------------------------#
#install(FILES)