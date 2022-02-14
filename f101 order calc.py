# -*- coding: utf-8 -*-
"""
Created on Mon Apr  5 21:19:44 2021

@author: Davit
"""

import pandas as pd
import os
import numpy as np
os.chdir(r"D:\Data center")


f101 =pd.read_excel('f101.xlsx', sheet_name = 'f101 v3')
f101 = f101.set_index('Acronym code')

while any(f101['Order'].isnull()):
    for i in f101.loc[f101['Order'].isnull()].index: 
        a = f101.loc[str(f101.loc[i, 'Internal']).replace(",", '').split(' '), 'Order']
    #    print(a)
        if not(np.isnan(a).any()):
            f101.loc[i, 'Order'] = max(a) + 1

f101.to_excel('f101_maket.xlsx')
