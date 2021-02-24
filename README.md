# CaMa-Flood_v4
CaMa-Flood_v4 code is now on GitHub!

This GitHub repository is mainly used for those who want to make contributions to CaMa-Flood.
If you simply want to use CaMa-Flood, please visit the product webpage and register on Google Form. (the password for downloading is issued after registration). Then, you can download the CaMa-Flood package which contains data (map/input) to run the model.
http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/

We also provide a guideline to all users to run the CaMa-Flood obtained in different ways (e.g., internal users in U-Tokyo, download from product webpage, clone from Guihub). Please check /etc/CaMa-Flood_Guideline.md after downloading any packages. A more detailed manual for model structures, parameters and guildline fore personal settings can be accessed in /etc/Manual_CaMa-Flood_v400.docx. 

Now the codes of CaMa-Flood v4 are distributed under Apache 2.0 license (so Open Source!). 
So you can feel free to use/modify the code. (note: data is distributed under difference license).

We are also happy to collaborate with external contributers for further development of CaMa-Flood.
We are now discussing how to merge contributions from multiple developpers.

Below is a tentative idea.

## Branches
- master: main repository (latest STABLE code is here).
- release_v4.XX: the code archive released as v4.XX (Do not modify)
- develop4XX: current development branch (latest WORKING code is here, to be merged with master at some points)
- dev4XX_YYY: working branch (to be merged to 'develop4XX') for development & test a new scheme YYY


## When you make some updates

0. Please contact the CaMa developer to state that you want to make some contributions.

1. We recommend you to fork the "master" branch. (or fork "develop4XX" if you want to check the confricts with ongoing works not yet merged to "master".)

2. Make some modifications, following the below rules:

- The added scheme should be turned on/off by switch. Better if it does not affect the original codes when the new scheme is turned off.
- If the new scheme causes conflicts, please discuss with the manager.
- We appreciate if a sample script is prepared to run CaMa-Flood with the new scheme. If possible, please design the scripts ready to be run by someone else. For example, avoid absolute pass, avoid environment dependent shebang (#!/usr/local/python) and describe required library required.
- If the new scheme requires additional data to run, please prepare a sample data required by the new sheme (better if the size of the sample data is minimum). However, the data should be kept outside of GitHub. If your new scheme required data, please contact Yamazaki to discuss a better way of data management in CaMa-Flood package.

3. When your code is ready, please check the conflicts with the latest master branch. (as master might be updated during your work).
- If you find a conflict which cannot be solved, please contact the code manager.

4. If there is no conflict, please contact the CaMa-Flood manager. We will create a new branch (e.g. dev4XX_YYY) to merge your contribution.

5. Please make a pull request of your new contribution to "dev4XX_YYY" branch. We will let you know which branch you should make a pull request.

6. Your new contribution is once merged to "dev4XX_YYY". Yamazaki will perform a test simulation to confirm your contribution works well. Then, the new scheme is merged to the "deveop4XX" branch.

7. After several tests, "develop4XX" is merged to "master". 

