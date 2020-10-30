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

Some optional third party dependencies are:

* [**pFUnit**](https://github.com/Goddard-Fortran-Ecosystem/pFUnit) (Optional) - To run the unit test for TensorTools
* [**Doxygen**](https://www.doxygen.nl/) (Optional) - To automatically build the documentation of TensorTools
* [**LAPACK**](http://www.netlib.org/lapack/) (Optional) - To use the more advanced and performance oriented algorithms in TensorTools

These dependencies are disabled by default. If you wish to use them, you can activate them by providing the options ```LIBTT_TESTS=ON```, ```LIBTT_LAPACK=ON``` and/or ```LIBTT_DOCS=ON``` when running CMake.

### Build & Installation
First create a build directory
```
mkdir build
```
within the directory you cloned TensorTools into. Next run Cmake to configure the build files, i.e.
```
cd build
cmake -DCMAKE_INSTALL_PREFIX=<path/to/install/dir> ..
```
Here, you have to specify the installation path explicitly. If you did not specify the path, the executable will be build into the build directory you just created. 

Finally you can compile and install TensorTools by calling
```
make
```
Congratulations, now you should be able to include the installed library into your projects.

### Include TensorTools into your project
There are various possibilities to include the tensor tools library into your own project. A few of them are listed below.

#### Use CMake
The most easy way to include TensorTools to your poject is available if your project is build using CMake.

#### Link against a static labrary
After having build the static library as described above, you only have to tell your linker where to find the ```libtt.a``` file.

#### Copy source files
If you do not want to care about setting up CMake or configuring your build link against a static library, you can also use the quick and dirty way by copying the source files directly into your projects source directory.

## LAPACK routines
TensorTools offers some advanced and/or computational efficient routines which are based on the LAPACK library for linear algebra. The build of these routines is disabled by default. If you want to use them, you would need to set the corresponding option when running CMake, e.g.
```
cmake -DLIBTT_LAPACK=ON ..
```

## Testing
If you want to run the tests for this framework, let CMake generate your build files and compile everything using
```
cmake -DLIBTT_TESTS=ON -DCMAKE_PREFIX_PATH=<path/to/pfunit/install/dir> ..
make
```
Now you can run the tests by calling
```
ctest
```
from the build directory.

## Examples
You can find examples on the functionality of this library within the ```examples``` directory. To run an example, got to the directory of the desired example and run the prepared shell script, e.g.
```
./run_example.sh
```
This will build and execute the desired example for you.

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

## Versioning
We use [SemVer](http://semver.org/) for versioning.

## Contributing
If you wish to contribute to this project, feel free to report issues on GitHub or to even fork and open a pull request.

### Main authors
* [**Lukas Lamm**](https://www.llamm.de) - Just some random guy working with computers

## License & Copyright
This project is licensed under the LGPL License - see the [LICENSE.md](LICENSE.md) file for details.

Copyright © 2020 by Lukas Lamm
