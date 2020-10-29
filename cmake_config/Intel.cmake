# Compiler falgs for Intel compiler

set(CMAKE_Fortan_FLAGS_DEBUG "-Wall -Wextra -Wsurprising -00 -g"
    CACHE STRING "Compiler flags for debugging")

set(CMAKE_Fortan_FLAGS_RELEASE "-O2"
    CACHE STRING "Compiler flags for production")

set(CMAKE_Fortran_FLAGS "-traceback -check all -save-temps -diag-disable 5462")