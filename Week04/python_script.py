import numpy as np
import os
import subprocess as sub
import sys

#first arguments, if passed when calling the script N_min,N_max define the interval of matrix sizes
#third argument is the number of steps 
#if not passed, use predefined sizes
if ( len(sys.argv) >= 4):
	N_min   = int(sys.argv[1])
	N_max   = int(sys.argv[2])
	N_count = int(sys.argv[3])
else:
	N_min   = 50
	N_max   = 1500
	N_count = 50
array_sizes = np.logspace( np.log10(N_min+1), np.log10(N_max+1), num=N_count, base=10, dtype=int)


#commands to be written in shell
compile_program = 'gfortran -Wall -o "EX04_Nicolai-CODE" "EX04_Nicolai-CODE.f90"'
run_executable  = './EX04_Nicolai-CODE '
filename_input_size = "sizes.dat" 


#filenames for the execution
data_files = ['definition.dat', 'transposed.dat', 'intrinsic.dat']

#purge the folder before starting
os.system('rm ' + filename_input_size)
os.system('rm -f *.png*')
os.system('rm -f fit.log')

for single_file in data_files:
    os.system('rm '+ single_file)    

#compile the Fortran script 
os.system(compile_program)

#start the script and produce data
for ii in array_sizes:
    
    istream = open(filename_input_size, "w")
    istream.write(str(ii))
    istream.close()

    sub.call([run_executable + filename_input_size], shell = True)

#gnuplot plot function
sub.call(["gnuplot", "timeplots.gplot"])
    
#need to initialize some filenames using gnuplot format
data_files_gnuplot = ['\'definition.dat\'', '\'transposed.dat\'', '\'intrinsic.dat\'']
output_png_files_gnuplot = ['\'def.png\'', '\'tra.png\'' , '\'int.png\'']

#need to call the gnuplot fit commands and initialize some useful strings
gnuplot_call_w_arguments = "gnuplot -e \""
gnuplot_datafile_command = "datafile="
gnuplot_output_png_command = "outputname="

#gnuplot fit commands
for single_input_data, output_image in zip(data_files_gnuplot, output_png_files_gnuplot):
    sub.call([gnuplot_call_w_arguments + 
              gnuplot_datafile_command + single_input_data +"; "+
              gnuplot_output_png_command + output_image + "\" " + "fit_cubic.gplot"], shell = True)
