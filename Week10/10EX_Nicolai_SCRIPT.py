import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import os
import sys
import subprocess as sub

#mean field energy
def MF_approx_en(lambda_par):
    y = []
    for value in lambda_par:
        if (value <= 2):
            y.append(-1 -value**2/4)
        else:
            y.append(-value)
    return y



compile_program = 'gfortran -Wall -o "10EX_Nicolai_CODE" "10EX_Nicolai_MODULES.f90" "10EX_Nicolai_CODE.f90" -llapack -lm -lfftw3 -Ofast' 
run_executable  = './10EX_Nicolai_CODE'
#purge the folder before starting
os.system('rm -f *.dat')
os.system('rm -f *.png')


#compile the Fortran script 
os.system(compile_program)
#let us start the program
sub.call([run_executable, '4', '100'])

#lambda linspacing
x = np.linspace(0, 3, 101)

#mf_energy
y = MF_approx_en(x)

n_particles = ['2','3','4']
df_names = ['n_particles  '+i+'.dat' for i in n_particles]
df_names_gs_vs_iter = ['gs_vs_iter_npart  '+i+'.dat'for i in n_particles]



df_list_iter = []
for df_name in df_names_gs_vs_iter:
    df_list_iter.append(pd.read_csv(df_name, header = None))

plt.figure(figsize=(8,6))
for single_df, n_part in zip(df_list_iter, n_particles):
    plt.plot(single_df.iloc[:,0], abs(single_df.iloc[:,1] + 1.), label = 'N = '+ n_part)    
plt.yscale('log')

plt.xlabel('Iteration #', fontsize = '15')
plt.ylabel('Relative error', fontsize = '15')
plt.title('$|\dfrac{e_0^{RSRG} - e_0^{M.F.}}{e_0^{M.F.}}|$ for different number of iterations', fontsize = '15')
plt.grid(True)    
plt.legend()
plt.savefig('01.png')
plt.show()

df_list = []
for df_name in df_names:
    df_list.append(pd.read_csv(df_name, header = None))

plt.figure(figsize=(8,6))
for single_df, n_part in zip(df_list, n_particles):
    plt.plot(x,single_df.iloc[:,1], label = "N = "+n_part)
plt.plot(x,y, lw = 2, label = 'MF prediction')    
plt.title("RSRG - Ground state energy", fontsize = '15')
plt.ylabel('Energy density', fontsize = '15')
plt.xlabel('$\lambda$', fontsize = '15')
plt.grid(True)
plt.savefig('02.png')
plt.legend()
plt.show()


plt.figure(figsize=(8,6))
for single_df, n_part in zip(df_list, n_particles):
    plt.plot(x,single_df.iloc[:,1] - y, label = "N = "+n_part)
    
plt.title("RSRG - Difference wrt MF prediction for ground state", fontsize = '15')
plt.ylabel('$\Delta e_{gs}$', fontsize = '15')
plt.xlabel('$\lambda$', fontsize = '15')
plt.grid(True)    
plt.legend()
plt.savefig('03.png')
plt.show()



