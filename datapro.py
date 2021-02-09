# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import datetime as dt
import csv
import pandas as pd
import os
# os.chdir('/Users/Jayson/Documents/python_practice/stat6110_proj/Data')

# %%
import pygrib as pg
grbs = pg.open('temp.grib')  # 所有变量
for grb in grbs:
    print(grb)  # 每一个变量的头文件
    print(grb.keys())  # 每一个变量的keys
    print(grb.values)  # 每一个变量的值

# %%
grbs = pg.open('temp.grib')
with open('temp.txt', 'w') as f:
    for grb in grbs:
        f.write(str(grb) + '\n')

# %%
grbs = pg.open('temp.grib')
grbs.seek(0)
grb = grbs.select(name='2 metre temperature')[0]
value = grb.values
data = pd.DataFrame(value)
data.to_csv('/Users/Jayson/Documents/python_practice/stat6110_proj/Data/txt/17090100.csv',
            index=False, header=False)
lats, lons = grb.latlons()
output_list = []
for i in range(lats.shape[0]):
    zip1 = zip(lats[i], lons[i])
    output_list.append(list(zip1))

output = pd.DataFrame(output_list)
# %%
grbs = pg.open('temp.grib')
tem = []
date_valid = dt.datetime(2020, 3, 4, 6)
for grb in grbs:
    if grb.validDate == date_valid and grb.parameterName == 'Temperature' and grb.level == 2:
        tem.append(grb, values)


#%% Follow another tutorial
# Make the output of plotting commands be displayed inline within the notebook,
%matplotlib inline 
from mpl_toolkits.basemap import Basemap  # import Basemap matplotlib toolkit
import numpy as np
import matplotlib.pyplot as plt
import pygrib # import pygrib interface to grib_api








