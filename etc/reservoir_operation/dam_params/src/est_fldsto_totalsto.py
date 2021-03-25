import calendar
from datetime import datetime
import os
import numpy as np
import pandas as pd
import sys

print(os.path.basename(__file__))

#### initial setting =======================================

DAM_FILE = sys.argv[1]
TAG = sys.argv[2]

output_file = TAG+'/tmp_p03_fldsto.csv'

fldstorate = 0.37

####========================================================

damcsv = pd.read_csv(DAM_FILE)

damcsv['fldsto_mcm'] = damcsv['totalsto_mcm'].values * fldstorate
damcsv['consto_mcm'] = damcsv['totalsto_mcm']  - damcsv['fldsto_mcm']

stocsv = damcsv[['damid', 'damname', 'totalsto_mcm', 'fldsto_mcm', 'consto_mcm']]
stocsv.to_csv(output_file, index=None)

print('file outputted:', output_file)
print('###########################################')
print(' ')

sys.exit()