gfortran -Wall -c 08EX_Nicolai_MODULES.f90 08EX_Nicolai_CODE.f90  -llapack -Ofast -fcheck=all 
gfortran -Wall -o 08EX_Nicolai_CODE 08EX_Nicolai_MODULES.f90 08EX_Nicolai_CODE.f90  -llapack -Ofast  -fcheck=all 

echo -e "\n\n\nCalling program passing 0 as parameter:\n"
./08EX_Nicolai_CODE 0

echo -e "\n\n\nCalling program passing 1 as parameter:\n"
./08EX_Nicolai_CODE 1

echo -e "\n\n\nCalling program passing 2 as parameter:\n"
./08EX_Nicolai_CODE 2

echo -e "\n\n\nCalling program passing 3 as parameter:\n"
./08EX_Nicolai_CODE 3
