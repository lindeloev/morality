# -*- coding: utf-8 -*-
"""
Created on Sat Sep 27 11:07:12 2014

@author: jonas
"""


import os
import csv
os.chdir('/media/jonas/4773705E4BCB1B13/jonas/insync/Documents/projects/work pggcs/data')

outFile = 'pggcs_all.csv'
pathFolder = 'pggcs_all'

csv.register_dialect('semi', delimiter=';')
allTrials = []

for item in os.walk(pathFolder):  # [path, folders, files]
    for filename in item[2]:
        # Move files to top directory
        pathCurrent = os.path.join(item[0], filename)
        pathNew = os.path.join(pathFolder, filename)
        os.rename(pathCurrent, pathNew)

for item in os.walk(pathFolder):  # [path, folders, files]
    for filename in item[2]:
        pathNew = os.path.join(pathFolder, filename)
        
        if filename[-4:].lower() == '.csv':
            rows = list(csv.DictReader(open(pathNew), dialect='semi'))

            # Append questions to trials-file
            if '_questions' in filename:
                id = filename.split('_')[0]
                ending = filename.split('_questions ')[1]
                trialsPath = os.path.join(pathFolder, id + '_trials ' + ending)
                
                # Load _trials csv data
                trialsRows = csv.DictReader(open(trialsPath), dialect='semi')  # just to get the keys and  fieldnames order!
                sampleTrial = list(trialsRows)[0]
                writer = csv.DictWriter(open(trialsPath, 'a'), sampleTrial.keys(), dialect='semi')
                writer.fieldnames = trialsRows.fieldnames
                
                # Now loop through questions and add them
                for row in rows:
                    row['recallRT'] = row.pop('rt')
                    row['recallAns'] = row.pop('answer')
                    row['gender'] = sampleTrial['gender']
                    row['id'] = sampleTrial['id']
                    row['age'] = sampleTrial['age']
                    if sampleTrial.has_key('stimType'): row['stimType'] = sampleTrial['stimType']
                    if sampleTrial.has_key('order'): row['order'] = sampleTrial['order']
                    row.pop('question')
                    
                    writer.writerow(row)
                
                # Now delete _questions
                os.remove(pathNew)
                print 'Q_FIX: %s' %filename
            
            # Add trials to data pool
            elif '_trials ' in filename:
                #print 'WRITE: %s, %i rows' %(filename, nrows)
                for row in rows:
                    # Change to "encode" rather than "faces" and add "stimType"
                    if not row.has_key('encode'):
                        row['encode'] = row.pop('faces')
                    if not row.has_key('stimType'):
                        row['stimType'] = 'faces'
                    
                    if row['encode']:
                        row['encode'] = row['encode'].replace('faces\\\\', '')
                    if row['recallAns']:
                        row['recallAns'] = row['recallAns'].replace('faces\\\\', '')
                allTrials += rows
            
            # Bad files
            """
            elif len(rows) < 4:
                print 'DELETED FEW ROWS: %s, %i rows' %(filename, nrows)
                #os.remove(path)
            else:
                print 'IGNOR: %s, %i rows' %(filename, nrows)
            """
        else:
            print 'DELETE NON-CSV: %s' %filename
            #os.remove(pathNew)
        
        

# Write trials
writer = csv.DictWriter(open(outFile, 'w'), allTrials[0].keys() + ['pauseRT'], dialect='excel-tab')
writer.writeheader()
writer.writerows(allTrials)
