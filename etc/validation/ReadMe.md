# Sample script for comparison

NOTE: all sample observation data is dummy, as we cannot redistribute observation datasets.

## 1. Discharge validation
Simulated total discharge (outflw) is comapred with observed discharge values in daily scale.

```bash
sh s01-discharge_validation.sh
```
The above program will run ```bash src/discharge_validation.py``` which compares the simulated and observed dicharge. The above program will output discarge comaprison figures (```fig_{tag}/discharge```) and data files (```txt_{tag}/discharge```) for each observation locations. This program will output the Nash-Sutcliffe efficiency and Kling-Gupta efficiency.

## 2. Water surface elevation validation
Simulated water surface elevation [WSE] (sfcelv) is comapred with observed WSE values in daily scale.

```bash
sh s02-wse_validation.sh
```
The above program will run ```src/wse_validation.py``` which compares the simulated and observed WSE. The above program will output discarge comaprison figures (```fig_{tag}/wse```) and data files (```txt_{tag}/wse```) for each observation locations. This program will output the Root Mean Square Error.

## 3. Flood water extent validation
Simulated flood water extent [FWE] (fldare) is comapred with observed FWE values in monthly scale.

```bash
sh s02-flood_extent_validation.sh
```
The above program will run ```src/flood_extent_validation.py``` which compares the simulated and observed FWE. The above program will output monthly averaged FWE time series figures (```fig_{tag}/fwe```) and data files (```txt_{tag}/fwe```) for the region choose. This program will output the correlation coeffient for FWE comaprison.