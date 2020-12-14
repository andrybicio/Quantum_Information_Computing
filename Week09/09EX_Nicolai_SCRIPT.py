import numpy as np
import pandas as pd
import os
import sys
import subprocess as sub
import matplotlib.pyplot as plt

compile_program = 'gfortran -Wall -o "09EX_Nicolai_CODE" "09EX_Nicolai_MODULES.f90" "09EX_Nicolai_CODE.f90" -llapack -lm -lfftw3 -Ofast' 
run_executable  = './09EX_Nicolai_CODE '
#purge the folder before starting
os.system('rm -f *.dat')
os.system('rm -f *.png*')

#choose the max number of particles to simulate, matrix size is 2^n_part
n_particles = '11'
sub.call(['./09EX_Nicolai_CODE', n_particles])

#mean field energy
def MF_approx_en(lambda_par):
    y = []
    for value in lambda_par:
        if (value <= 2):
            y.append(-1 -value**2/4)
        else:
            y.append(-value)
    return y

#lambda linspacing
x = np.linspace(0, 3, 60)

#mf_energy
y = MF_approx_en(x)

name_list = ['n_particles 2.dat', 'n_particles 3.dat', 'n_particles 4.dat',
             'n_particles 5.dat', 'n_particles 6.dat', 'n_particles 7.dat',
             'n_particles 8.dat', 'n_particles 9.dat', 'n_particles10.dat',
             'n_particles11.dat']

df_list = []

for fname in name_list:
    df_list.append(pd.read_csv(fname, header = None, names =['lambda', 'E_0', 'E_1', 'E_2', 'E_3']) )

fig = plt.figure(figsize = (12,8))

plt.plot(x,y, label = 'MF', color = 'black', lw = 3)
index = 2
for df in df_list:
    plt.plot(df.loc[:,'lambda'], df.loc[:,'E_0']*(index)/(index-1), label = "N = "+str(index))
    index += 1

plt.grid(True)    
plt.ylabel('$\dfrac{E_0}{N-1}$')
plt.xlabel('$\lambda$')
plt.title('Ground State Energy density for different N')
plt.legend()
plt.savefig('ground_states.png')

index = 2
for df in df_list:
    fig = plt.figure(figsize = (8,6))
    
    plt.plot(df.loc[:,'lambda'], df.loc[:,'E_0']*(index)/(index-1), label = "$E_0$")
    plt.plot(df.loc[:,'lambda'], df.loc[:,'E_1']*(index)/(index-1), label = "$E_1$")
    plt.plot(df.loc[:,'lambda'], df.loc[:,'E_2']*(index)/(index-1), label = "$E_2$")
    plt.plot(df.loc[:,'lambda'], df.loc[:,'E_3']*(index)/(index-1), label = "$E_3$")
    
    plt.ylabel('$\dfrac{E_i}{N-1}$')
    plt.xlabel('$\lambda$')
    plt.title('Energy densities wrt $\lambda$      N = '+str(index))
    plt.grid(True)    
    plt.legend()
    plt.savefig('Energies_N'+str(index)+'.png')
    
    index += 1


