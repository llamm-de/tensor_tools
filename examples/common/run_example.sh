#! /bin/bash
green=$'\e[1;32m'
end=$'\e[0m'

if [ ! -f ./common_example.out ]; then
    printf  "%s\n" "${green} Building example for common operations. ${end}"
    make
else
    printf  "%s\n" "${green} Executable already exists. No build needed. ${end}"
fi

printf  "%s\n" "${green} Executing example. ${end}"
./common_example.out

# Clean up
printf  "%s\n" "${green} Removing build files. ${end}"
make clean

