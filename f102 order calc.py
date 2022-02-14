# -*- coding: utf-8 -*-
"""
Created on Thu Mar 25 15:09:55 2021

@author: Davit
"""

import pandas as pd
import os
import numpy as np
os.chdir(r"D:\Data center")

f102 = pd.read_excel("f102.xlsx")
f102 = f102.set_index('Acronym code')

while any(f102['Order'].isnull()):
    for i in f102.loc[f102['Order'].isnull()].index: 
        a = f102.loc[str(f102.loc[i, 'Internal']).replace(",", '').split(' '), 'Order']
    #    print(a)
        if not(np.isnan(a).any()):
            f102.loc[i, 'Order'] = max(a) + 1



f102.to_excel('f102_1.xlsx')
