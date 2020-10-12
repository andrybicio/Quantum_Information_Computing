rm O1.dat O2.dat O3.dat Ofast.dat
for optimization_flag in O1 O2 O3 Ofast
do
   gfortran -Wall -${optimization_flag} -o "01_TestPerformanceFancier" "01_TestPerformanceFancier.f90"
   for matrix_size in 10 25 50 75 100, 125, 150, 250, 500, 750, 1000, 1250, 1500
   do
      echo Optimization flag ${optimization_flag} and matrix size ${matrix_size}
      ./01_TestPerformanceFancier ${matrix_size} ${optimization_flag}
   done
done

