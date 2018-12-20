# -*- coding: utf-8 -*-
"""
Created on Wed Sep 17 15:40:19 2014

@author: jonas
"""

from __future__ import division
import csv
import glob
import random

baseMoney = 100  # how much is available for investment?
investFactor = 1.5

# List csv files and randomize order
csv.register_dialect('semi', delimiter=';')
files = glob.glob('/home/jonas/Documents/programmering/pggcs/data_between/*.csv')
files = random.sample(files, len(files))

for i in range(0, len(files) -1, 2):
    # Load two files
    trialList1 = list(csv.DictReader(open(files[i]), dialect='semi'))
    trialList2 = list(csv.DictReader(open(files[i + 1]), dialect='semi'))

    # Loop through trials
    for trial in range(len(trialList1)):
        trial1 = trialList1[i]
        trial2 = trialList2[i]
        
        # Payback from common pool
        invest1 = int(trial1['pggInvest'])
        invest2 = int(trial2['pggInvest'])
        commonPayback = (invest1 + invest2) * investFactor / 2
        
        # Payback from kept money
        keepPayback1 = baseMoney - invest1
        keepPayback2 = baseMoney - invest2
        
        # Total payback when multiplied with recall performance
        payback1 = (commonPayback + keepPayback1) * float(trial1['payoutFactor'])
        payback2 = (commonPayback + keepPayback2) * float(trial2['payoutFactor'])
        
        # Show results
        id1 = files[i].split(' (2014')[0]  # id entered in dialogue
        id2 = files[i + 1].split(' (2014')[0]  # id entered in dialogue
        print '%s gets %f DKK back. Competed against %s.' %(id1, payback1, id2)
        print '%s gets %f DKK back. Competed against %s.' %(id2, payback2, id1)
