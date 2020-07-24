#! /bin/bash
green=$'\e[1;32m'
end=$'\e[0m'

if [ ! -f ./common_example ]; then
    printf  "%s\n" "${green} Building example for common operations. ${end}"
    make
    printf  "%s\n" "${green} Removing temporary build files. ${end}"
    rm *.mod *.o
else
    printf  "%s\n" "${green} Executable already exists. No build needed. ${end}"
fi

printf  "%s\n" "${green} Executing example. ${end}"
./common_example

