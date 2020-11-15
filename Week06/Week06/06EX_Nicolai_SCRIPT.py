import numpy as np
import os
import subprocess as sub
import matplotlib.pyplot as plt
import pandas as pd
import sys

def print_UV(spacing, h_bar = 1., m = 1.):
    spacing = spacing
    print("UV energy cutoff is: " , h_bar*h_bar/(2.*m*spacing*spacing)) 
    return

def print_IR(length_interval, m = 1, h_bar = 1.):
    l_half = length_interval/2.
    print("IR energy cutoff is: " , h_bar*h_bar/(2.*m*l_half*l_half) )
    return

def print_n_IR(length_interval, x_0):
    l_half = length_interval/2.
    print("Lower bound for energy level is: " , x_0*x_0/(2.*l_half*l_half) )
    return

def print_n_UV(spac, x_0):
    print("Upper bound for energy level is: " , x_0*x_0/(2.*spac*spac) )
    return

def print_x0(omega, m = 1., h_bar = 1.):
    x0 = np.sqrt(h_bar/(m*omega))
    print("Typical lenght of the system is", x0)
    return x0

def print_parameters(len_int, spac, om, mass, num_eig):
    print("Interval length <-", len_int)
    print("Spacing <-", spac)
    print("Omega <-", om)
    print("Mass <-", mass)
    print("num eigenvalues wanted <-", num_eig)
    return
    
def eigenf0(x, m, omega, h_bar):
    return ((m*omega)/(np.pi*h_bar))**(1/4)*np.exp(-m*omega/(2*h_bar) *x**2)

def eigenf1(x, m, omega, h_bar):
    return ((m*omega)/(np.pi*h_bar))**(1/4)*x*np.sqrt((2*m*omega)/(h_bar))*np.exp(-m*omega/(2*h_bar) *x**2)

def eigenf2(x, m, omega, h_bar):
    return ((m*omega)/(np.pi*h_bar))**(1/4)*(1-2*m*omega*x**2/h_bar)*np.exp(-m*omega/(2*h_bar) *x**2)

#commands to be written in shell
compile_program = 'gfortran -Wall -o "06EX_Nicolai_CODE" "06EX_Nicolai_MODULES.f90" "06EX_Nicolai_CODE.f90" -llapack -Ofast' 
run_executable  = './06EX_Nicolai_CODE '
#purge the folder before starting
os.system('rm -f *.dat')
os.system('rm -f *.png*')

length_interval = '1e0'
mass = '1e0'
omega = '5e4'
spacings = ['5e-2' , '1e-2', '5e-3', '1e-3', '5e-4', '2.5e-4', '1.0e-4']
num_eigen = '200'
h_bar = 1.

energies_filenames_data = []
eigenvectors_filenames_data = []

#compile the Fortran script 
os.system(compile_program)


x_0 = print_x0(float(omega))

for spacing in spacings:
    print("\nFor the following choice of parameters:")
    print_parameters(length_interval, spacing, omega, mass, num_eigen)
    sub.call(['./06EX_Nicolai_CODE', length_interval, spacing, mass, omega, num_eigen])
    
    print_IR(float(length_interval))
    print_UV(float(spacing))
    print_n_IR(float(length_interval), x_0 )
    print_n_UV(float(spacing), x_0 )
    
    energies_filenames_data.append("energies_L"+length_interval+"_dx"+spacing+".dat")
    eigenvectors_filenames_data.append("eigenvectors_L"+length_interval+"_dx"+spacing+".dat")
    print("\n")

def theoretical_eigenvalues(num_values, omega, h_bar = 1.):
    eigenv = []
    for n in range(num_values):
        eigenv.append(h_bar*omega*(0.5+n))
        
    return np.array(eigenv)

import pandas as pd

theoretical_values = theoretical_eigenvalues(int(num_eigen), float(omega)) 

for file1, spac in zip(energies_filenames_data, spacings):
    temp_energy = pd.read_csv(file1, header = None, na_values = '                       NaN', dtype = float)
    plt.plot( abs(temp_energy.loc[:,0] - theoretical_values)/theoretical_values, 'o', label = spac)

plt.xscale('log')
plt.yscale('log')
plt.legend(title = 'spacing')
plt.title('Relative error of eigenvalues wrt theoretical values')
plt.ylabel('Relative error')
plt.xlabel('Energy level')
plt.savefig('Err_energies.png')
plt.figure()

for file1, spac in zip(energies_filenames_data[3:],spacings[3:]):
    temp_energy = pd.read_csv(file1, header = None, na_values = '                       NaN', dtype = float)
    plt.plot( abs(temp_energy.loc[:,0] - theoretical_values)/theoretical_values, 'x', label = spac)

plt.legend(title = 'spacing')
plt.title('Relative error of eigenvalues wrt theoretical values')
plt.ylabel('Relative error')
plt.yscale('log')
plt.xscale('log')
plt.xlabel('Energy level')
plt.savefig('Err_energies_zoom.png')
plt.figure()

for file1, spac in zip(eigenvectors_filenames_data[:-3],spacings[:-3]):
    temp_eigenf = pd.read_csv(file1, header = None, na_values = '                       NaN', dtype = float)
    eigen0_val = eigenf0(temp_eigenf.loc[:,0], float(mass), float(omega), h_bar)
    
    plt.plot( temp_eigenf.iloc[:,0], abs(temp_eigenf.iloc[:,1])  - eigen0_val, label = spac )

plt.title('Ground state eigenvector vs Theoretical one')
plt.xlabel('x')
plt.ylabel('Difference from theoretical')
plt.xlim([-0.1,0.1])
plt.legend(title = 'spacing')
plt.savefig('eigenONE.png')
plt.figure()


for file1, spac in zip(eigenvectors_filenames_data[:-3],spacings[:-3]):
    temp_eigenf = pd.read_csv(file1, header = None, na_values = '                       NaN', dtype = float)
    eigen1_val = eigenf1(temp_eigenf.loc[:,0], float(mass), float(omega), h_bar)
    plt.plot( temp_eigenf.iloc[:,0], abs(temp_eigenf.iloc[:,2])  - abs(eigen1_val), label = spac )

plt.title('First excited state eigenvectors vs Theoretical one')
plt.xlabel('x')
plt.ylabel('Difference from theoretical error')
plt.xlim([-0.15,0.15])
plt.legend(title = 'spacing')
plt.savefig('eigenTWO.png')
plt.figure()


fig, ax = plt.subplots(4,1, figsize = (10,6), sharex = True)

best_data_fname = eigenvectors_filenames_data[-1]
best_data = pd.read_csv(file1, header = None, na_values = '                       NaN', dtype = float)
legends_lab = ["Ground state", "First excited state", "Second excited state", "Third excited state"]

for i in range(4):
    ax[i].plot(best_data.iloc[:,0], best_data.iloc[:,i+1]**2, color = 'red', label = legends_lab[i])
    ax[i].set_xlim([-0.02, 0.02])
    ax[i].set_ylabel("|$\psi$|$^2$")
    ax[i].set_xlabel("x")
    ax[i].legend()
    
plt.tight_layout()
plt.savefig('square.png')
plt.figure()



tot_values_to_print = 7
x = best_data.iloc[:,0]
ener = pd.read_csv(energies_filenames_data[-1], header = None)

for i in range(tot_values_to_print):
    plt.axhline(ener.iloc[i,0], c='black', ls='--')
    plt.plot(x, 2000*best_data.iloc[:,i+1] + ener.iloc[i,0] , label='Level '+str(i))
    
plt.plot(x, 0.5*float(omega)*float(omega)*float(mass)*(x**2))
plt.ylim(0, ener.iloc[tot_values_to_print,0])
plt.xlabel('x')
plt.ylabel('V(x)')
plt.xlim([-0.025, 0.025])
plt.title('First eigenvalues and related eigenfunctions')
plt.legend(loc='upper center', bbox_to_anchor=(0.5, -0.2), shadow=True, ncol=tot_values_to_print)
plt.savefig('potential.png')





