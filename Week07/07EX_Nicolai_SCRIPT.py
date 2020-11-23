#!/usr/bin/env python
# coding: utf-8

# In[146]:


import numpy as np
import pandas as pd
import os
import sys
import subprocess as sub
#to remove a non empty folder
import shutil

def compute_avg(data, dx):
    mu = np.zeros(len(data.columns) - 1)
    for i in range(len(data.columns) - 1):
        mu[i] = np.sum(data.loc[:,0]*(data.loc[:,i+1]))
    return mu*dx

def compute_var(data, mu, dx):
    sigma = np.zeros(len(data.columns) - 1)
    for i in range(len(data.columns) - 1):
        sigma[i] = np.sum( (data.loc[:,0]**2*data.loc[:,i+1])*dx )-mu[i]**2
    return sigma

#commands to be written in shell
compile_program = 'gfortran -Wall -o "07EX_Nicolai_CODE" "07EX_Nicolai_MODULES.f90" "07EX_Nicolai_CODE.f90" -llapack -lm -lfftw3 -Ofast' 
run_executable  = './07EX_Nicolai_CODE '
#purge the folder before starting
# os.system('rm -f *.dat')
# os.system('rm -f *.png*')

x_max = '4e0'
x_min = '-4e0'
t_min = '0e0'
t_max = '5e1'
spacing_lengths = ['1e-2','1e-1']
spacing_times = ['1e-2', '1e-1']


potential_filename = []
wv_sq_filename = []
directories = []
spac_mus = []

#compile the Fortran script 
os.system(compile_program)

for spacing_dx in spacing_lengths:
    for spacing_dt in spacing_times:
        print("Omega, mass, h_bar are fixed to 1.0")
        
        sub.call(['./07EX_Nicolai_CODE', x_min, x_max, spacing_dx, t_min, t_max,  spacing_dt])
        
        length_interval = str(float(x_max)-float(x_min))
        potential_filename.append("potential_x_"+x_min+x_max+"_dx_"+spacing_dx+"time_"+t_max+"_dt_"+spacing_dt+".dat")
        wv_sq_filename.append("sqwave_x_"+x_min+x_max+"_dx_"+spacing_dx+"time_"+t_max+"_dt_"+spacing_dt+".dat")
        directories.append("L"+length_interval+"_dx"+spacing_dx+"dt_"+spacing_dt+"/")
        spac_mus.append(spacing_dx)
        print("\n")        

script_gnuplot_files = [' gif_wavefunctiont_vs_time.gplot',' gif_wavefunction_vs_potential.gplot', ' wavefunctiont_vs_time.gplot', ' mu_vs_time.gplot']

for folder in directories:
    try:
        shutil.rmtree(folder)
    except Exception:
        pass
    os.mkdir(folder)

    
    
parameters_strings = []
for sq_filename, pot_filename, single_dir, spac_dx in zip(wv_sq_filename, potential_filename, directories, spac_mus):
    
    data         = pd.read_csv(sq_filename, header = None)
    n_columns    = str(len(data.columns))
    mu   = compute_avg(data, float(spac_dx))
    var  = compute_var(data,mu, float(spac_dx))

    mu_var_dic = {'mu' : mu, 'var' : np.sqrt(var) }
    mu_var_df = pd.DataFrame(mu_var_dic).to_csv(single_dir+'mu_var.dat')
    
    parameters_strings.append("\"filename_pot='"+pot_filename+
                              "';filename_data_sq='"+sq_filename+
                              "';n_cols='"+n_columns+
                              "';directory='"+single_dir+
                              "'\"")

commands = []
for single_script in script_gnuplot_files:
    for single_param in parameters_strings:
        commands.append("gnuplot -e "+ single_param + single_script)
    
for single_script in commands:
    sub.call(single_script, shell = True)


# In[ ]:


data = pd.read_csv(sq_filename, header = None)
data.columns


# In[98]:


sq_filename
get_ipython().system('ls')


# In[ ]:


get_ipython().system('ls')

sqwave_x_-4e04e0_dx_1e-1time_3e1_dt_1e-1.dat

sqwave_x_-4e04e0_dx_1e-2time_3e1_dt_1e-2.dat


# In[119]:


sq_filename = "sqwave_x_-4e04e0_dx_1e-2time_5e1_dt_1e-2.dat"

def compute_avg(data):
    mu = np.zeros(len(data.columns) - 1)
    for i in range(len(data.columns) - 1):
        mu[i] = np.sum(x*(data.loc[:,i+1]))
    return mu


def compute_var(data, mu):
    sigma = np.zeros(len(data.columns) - 1)
    
    x = data.loc[:,0]
    
    for i in range(len(data.columns) - 1):
        sigma[i] = np.sum( x**2*(data.iloc[:,i+1] ) )  - mu[i]**2
    return sigma

data = pd.read_csv(sq_filename, header = None)
n_columns    = str(len(data.columns))
mu   = compute_avg(data)
plt.plot(mu)
print(mu)
var  = compute_var(data, mu)
# plt.plot(var)

plt.plot( mu + np.sqrt(var))
plt.plot( mu - np.sqrt(var))

# var
# mu_var_dic = {'mu': mu, 'var': var}

# mu_var_df = pd.DataFrame(mu_var_dic)
# mu_var_df


# In[122]:


data = pd.read_csv(sq_filename, header = None)
x = data.loc[:,0]
prob_x = data.loc[:,5000]
np.sum(x*prob_x)


# In[123]:


data


# In[69]:


import matplotlib.pyplot as plt
varrrr = np.var(data.loc[:,1:], axis = 0)

plt.plot(mu)
plt.plot(mu + varrrr)
plt.plot(mu - varrrr)


# In[1]:


x_max = '4e0'
x_min = '-4e0'
t_min = '0e0'
t_max = '3e1'
spacing_length = '1e-2'
spacing_time = '1e-2'
n_columns = int(float(t_max)/float(spacing_time) + 1)
n_columns


# In[ ]:




