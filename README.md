# CaMa-Flood_v4
CaMa-Flood_v4 code on GitHub.

This GitHub repository is mainly used for those who want to make contributions to CaMa-Flood.
If you simply want to use CaMa-Flood, please visit the product webpage and register on the Google Form. (the password for downloading is issued after registration). Then, you can download the CaMa-Flood package which contains data (map/input) to run the model.
http://hydro.iis.u-tokyo.ac.jp/~yamadai/cama-flood/

The codes of CaMa-Flood v4 is distributed under the **Apache 2.0** license (so Open Source!). 
So you can feel free to use/modify the code. (note: data is distributed under difference license)

If you need to cite CaMa-Flood based on versions (rather than published papers), please refer to Zenodo repository.
https://zenodo.org/search?q=parent.id%3A4609654&f=allversions%3Atrue&l=list&p=1&s=10&sort=version

We are also happy to collaborate with external contributers for further development of CaMa-Flood.

## Change log
See the changes done in [previous versions here](Versions.md).

## Branches
- **master**        : main repository (latest stable code is here).
- **release_v4.XX** : the code archive released as v4.XX (Do not modify)
- **develop4XX**    : development branch based on v4.XX (please fork from here to make a contribution)
- **dev4XX_PROJ**   : working branch for merging a project (when you make pull request, we creat a specific branch for your project)

## Contents

- `adm/`  : Make include files for different compilers and architectures used in `src/`
- `doc/`  : Documentation
- `etc/`  : Additional workflows/scripts to prepare, test and analyse input and output
- `gosh/` : Workflows to build and run CaMa-Flood under different configurations
- `map/`  : Input data preparation code
- `src/`  : Main source code files including the Makefile
- `utl/`  : Utility functions/scripts used throughout the repo

## Contributing to this repository

0. Please contact the developper (Dai Yamazaki) in advance to make a discussion on the new scheme to be integrated to **CaMa-Food master**.

1. Create a branch from the **develop4XX** branch.

2. Make some modifications, following the below rules:

  - The added scheme should be turned on/off by a switch. Better if it does not affect the original codes when the new scheme is turned off.
  - If the new scheme causes conflicts, please discuss with the manager.
  - We would appreciate it if the sample scripts are prepared to run CaMa-Flood with the new scheme. If possible, please design the scripts ready to be run by someone else. For example, avoid absolute paths, avoid environment dependent shebangs (e.g. #!/usr/local/python), describe required library required.
  - If the new scheme requires additional data to run, please prepare sample data required by the new sheme (better if the size of sample data is kept minimum and should be kept outside of the git repo, i.e. only commit text/code files). If your new scheme requires data, please contact Yamazaki to discuss a better way of data management in the CaMa-Flood package.
  - Test and commit your changes.

3. When your code is ready to be merged, please contact the repository manager (Dai Yamazaki). Then, we create a working branch **dev4XX_PROJ** to merge your branch. 

4. Please create a pull request to the **dev4XX_PROJ** branch.
  - If your commit cannot be merged by conflict, we will contact you.

5. Your new contribution is merged into **dev4XX_PROJ**. Yamazaki will perform further test simulations to confirm your contribution works well. Then, the new scheme is merged to **develop4XX** and **master** branch, by Yamazaki
