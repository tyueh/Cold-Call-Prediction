import collections as cl
import pandas as pd
import os


cwd = os.getcwd()
filename = 'carInsurance_train.csv'
abs_filename = os.path.join(cwd, filename)


## open the file, read the file into a list, and close the file  
f = open(abs_filename)
data = f.readlines()
f.close()


## clean the data
clean = []
for row in data:
    if row != '\n':
        row = row.strip().replace('"', '') ## remove the double quotation 
        row = row.replace('NA', 'NotFound') ## replace NA with NotFound
        clean.append(row.split(','))
        

## get the header and transpose the data
header = clean[0]
del clean[0]
clean_t = list(zip(*clean))

## store the data into a dictionary
dic = cl.defaultdict(list)
for i in range(len(header)):
    dic[header[i]] = clean_t[i]

## using pandas to write a csv file
df = pd.DataFrame(dic)
tofile_name = 'carInsurance_clean.csv'
df.to_csv(tofile_name, index=False)

