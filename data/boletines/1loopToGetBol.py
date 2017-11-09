# -*- coding: utf-8 -*-
# Code prepared by Eric Magar, 30oct2014
# email emagar at gmail dot com

import sys # to get arguments from command line
import codecs # eases character encoding issues
import csv # handles csv files
from collections import defaultdict  # used to prepare csv data
import os.path # used to check file existence

# import id and bl codes (for use in loop to process each record internally)
columns = defaultdict(list)
with codecs.open('1id-bl.csv', mode='r', encoding='utf-8') as f:
    reader = csv.DictReader(f, delimiter=',', quotechar='"') # read rows into a dictionary format
    for row in reader:                                       # read a row as {column1: value1, column2: value2,...}
        for (k,v) in row.items():                            # go over each column name and value
            columns[k].append(v)                             # append the value into the appropriate list based on column name k
ids = columns['id']
bls = columns['bl']

# will determine if the file is present in directory where this file resides
fileExists = []
for i in bls:
    tmp = 'bol' + i + '.txt'
    fileExists.append( os.path.exists(tmp) )

# indices of records that need work
indices2work = []
for i, j in enumerate(fileExists):
    if j == False:
        indices2work.append(i)

# subset if so desired to work by chunks (recommended)
n = 5
indices2work = indices2work[0:n] # pick first n elements

# loop starts by cutting first element to work on it
while len(indices2work) > 0:
    index2work = indices2work.pop(0) # "pops" element 0 (ie. first) from indices2work 
    sys.argv = ['1getBol.py', ids[index2work], bls[index2work]]
    execfile('1getBol.py')

