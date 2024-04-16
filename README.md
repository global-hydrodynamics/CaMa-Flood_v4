# CaMa-Flood_v4
CaMa-Flood_v4 code on GitHub.

This GitHub repository is mainly used for those who want to make contributions to CaMa-Flood.
If you simply want to use CaMa-Flood, please visit the product webpage and register on Google Form. (the password for downloading is issued after registration). Then, you can download the CaMa-Flood package which contains data (map/input) to run the model.
http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/

Now the codes of CaMa-Flood v4 is distributed under **Apache 2.0** license (so Open Source!). 
So you can feel free to use/modify the code. (note: data is distributed under difference license)

We are also happy to collaborate with external contributers for further development of CaMa-Flood.
We are now discussing how to merge contributions from multiple developpers.

Below is a tentative idea.

## Branches
- **master**        : main repository (latest stable code is here).
- **release_v4.XX** : the code archive released as v4.XX (Do not modify)
- **develop4XX**    : development branch based on v4.XX (please fork from here to make a contribution)
- **dev4XX_PROJ**   : working branch for merging a project (when you make pull request, we creat a specific branch for your project)


## When you make some update

0. Please contact the developper (Dai Yamazaki) in advance to make a discussion on the new scheme to be integrated to **CaMa-Food master**.

1. We recommend you to fork the **develop4XX** branch.

2. Make some modifications, following the below rules:

- The added scheme should be turned on/off by switch. Better if it does not affect the original codes when the new scheme is turned off.
- If the new scheme causes conflict, please discuss with the manager.
- We appreciate if the sample scripts is prepared to run CaMa-Flood with the new scheme. If possible, please design the scripts ready to be run by someone else. For example, avoid absolute pass, avoid environment dependent shebang (e.g. #!/usr/local/python), describe required library required.
- If the new scheme require additional data to run, please prepare a sample data required by the new sheme (better if the size of sample data is minimum). However, the data should be kept outside of GitHub. If your new scheme required data, please contact Yamazaki to discuss a better way of data management in CaMa-Flood package.

3. When your code is ready to be merged, please contact the repository manager (Dai Yamazaki). Then, we create a working branch **dev4XX_PROJ** for merging your commit. 
- Please fork **dev4XX_PROJ**, reflect your changes on this branch (only include nesessary changes).
- If you find a conflict which cannot be solved, please contact the code manager.

4. Please make a pull request to **dev4XX_PROJ** branch.
- If your commit cannot be merged by conflict, we will contact you.

5. Your new contribution is once merged to **dev4XX_PROJ**. Yamazaki will perform a test simulations to confirm your contribution works well. Then, the new scheme is merged to **develop4XX**  and **master** branch, by Yamazaki
