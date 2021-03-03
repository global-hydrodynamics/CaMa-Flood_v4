# Sample script for comparison

NOTE: all sample observation data is dummy, as we cannot redistribute observation datasets.

## Discharge validation
Simulated total discharge (outflwYYYY) is comapred with observed discharge values in daily scale.

```bash
sh s01-discharge_validation.sh
```
The above code will run ```bash src/discharge_validation.py``` with compares the simulated and observed dicharge. The above program will output discarge comaprison figures (```bash fig_{tag}```) and data files (```bash txt_{tag}```) for each observation locations. This program will output the Nash-Sutcliffe efficiency and Kling-Gupta efficiency.

## Water surface elevation validation
Simulated water surface elevation [WSE] (sfcelvYYYY) is comapred with observed WSE values in daily scale.

```bash
sh s01-discharge_validation.sh
```
The above code will run ```bash src/discharge_validation.py``` with compares the simulated and observed dicharge. The above program will output discarge comaprison figures (```bash fig_{tag}```) and data files (```bash txt_{tag}```) for each observation locations. This program will output the Nash-Sutcliffe efficiency and Kling-Gupta efficiency.