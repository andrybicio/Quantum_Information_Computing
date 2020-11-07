import numpy as np
import os
import subprocess as sub
import sys

#commands to be written in shell
compile_program = 'gfortran -Wall -o "05EX_Nicolai_CODE" "05EX_Nicolai_CODE.f90" -llapack "05EX_Nicolai_MODULES.f90"' 
run_executable  = './05EX_Nicolai_CODE '
#gfortran -Wall -o "%e" "%f"  -llapack 05EX_Nicolai_MODULES.f90 
#purge the folder before starting
os.system('rm -f *.dat')
os.system('rm -f *.png*')
os.system('rm -f fit.log')
os.system('rm -f results_fit.csv')

matrix_size = '1500'
number_samples = '200'
type_of_matrix = ['H','D']
aver_parameter = ['0', '1', str(int(int(matrix_size)/100)), str(int(int(matrix_size)/50)), str(int(int(matrix_size)/10))]
filenames_data = []

#compile the Fortran script 
os.system(compile_program)

index = 1
for matr_type in type_of_matrix:
    for aver in aver_parameter:
        print(index,"/",str(len(type_of_matrix)*len(aver_parameter)))    
        sub.call(['./05EX_Nicolai_CODE', matrix_size, number_samples, matr_type, aver])
        filenames_data.append("size"+matrix_size+"_samples"+number_samples+"_"+matr_type+"_aver"+aver+".dat")
        index = index + 1   

#remove the extension and append the "png" for the image
filenames_png = [i[:-3]+"png" for i in filenames_data]

parameters_strings = []
script_gnuplot_files = [' fit_plot_herm.gplot',' fit_plot_diag.gplot']

for matr_type in type_of_matrix:
    for aver in aver_parameter:
        parameters_strings.append("size='"+matrix_size+"';samples='"+number_samples+"';type_matr='"+matr_type+"';window='"+aver+"'")

temp_commands_gnuplot = ["gnuplot -e \"datafile='"+
                              data_in+"';"+param+";outputname='"+image_out+"\'\"" 
                              for data_in, image_out, param in zip(filenames_data, filenames_png, parameters_strings) ]

commands_gnuplot = []

for matr_type, script_gnuplot in zip(type_of_matrix, script_gnuplot_files):
    for command in temp_commands_gnuplot:
        if (matr_type in command): 
            commands_gnuplot.append(command+script_gnuplot)
            

for command in commands_gnuplot:
    sub.call([command], shell= True)
