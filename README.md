# TensorTools
[![Build Status](https://jenkins.llamm.de/buildStatus/icon?job=tensor_tools_2%2Fmaster)](https://jenkins.llamm.de/job/tensor_tools_2/job/master/)
![](https://img.shields.io/badge/license-LGPL--V2.1-blue)

A modern Fortran library for tensor calculus.

## Getting started
The following instructions will give you a copy of the project up and running on your local machine.
Notice, this library is only tested on Unix like operating systems (e.g. Linux, MacOs), Windows is not supported at the moment.

### Download
To get the latest version of this package, you can easily clone this repository by using
```
git clone https://github.com/llamm-de/tensor_tools.git
```

### Prerequisites & Dependencies
This package uses [CMake](https://cmake.org/) for the creation of build files. In order to build this package yourself, you would need to have CMake installed on your system.

If you want to run the tests for this package or generate an documentation, you need to have the following packages installed on your system:

* [**pFUnit**](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) - A unit testing framework for Fortran
* [**Doxygen**](https://www.doxygen.nl/) - Automated documentation tool

These dependencies are disabled by default. If you wish to use them, you can activate them by providing the options ```LIBTT_TESTS=ON``` and/or ```LIBTT_DOCS=ON``` when running CMake.

### Build & Installation
First create a build directory
```
mkdir build
```
within the directory you cloned TensorTools into. Next run Cmake to configure the build files, i.e.
```
cd build
cmake -DCMAKE_INSTALL_PREFIX=/path/to/install/dir ..
```
Here, you have to specify the installation path explicitly. If you did not specify the path, the executable will be build into the build directory you just created. 

Finally you can compile and install TensorTools by calling
```
make install
```
Congratulations, now you should be able to include the installed library into your projects.

## Examples
Some examples on how to use the TensorTools library in your project, are given as individual programs within the ```examples``` directory. Please notice, that these examples are not covered by test.

## Testing
If you want to run the tests for this framework, let CMake generate your build files and compile everything using
```
cmake -DLIBTT_TEST=ON ..
make
```
Now you can run the tests by calling
```
ctest
```
from the build directory.

## Documentation
If you want to create a documentation for this framework, let CMake generate your build files and compile everything using
```
cmake -DLIBTT_DOCS=ON ..
```
Now you can create the documentation by calling
```
make docs
```
from the build directory.

## Contributing
If you wish to contribute to this project, feel free to report issues on GitHub or to even fork and open a pull request.

### Main authors
* [**Lukas Lamm**](https://www.llamm.de) - Just some random guy working with computers

## License & Copyright
This project is licensed under the LGPL License - see the [LICENSE.md](LICENSE.md) file for details.

Copyright © 2020 by Lukas Lamm
