# CaMa-Flood_v4
CaMa-Flood_v4 code on GitHUb

Now the codes of CaMa-Flood v4 is distributed under Apache 2.0 license (so Open Source!). 
So you can feel free to use/modify the code. (note: data is distributed under difference license)

We are also happy to collaborate with external contributers for further development of CaMa-Flood.
We are now discussing how to merge contributions from multiple developpers.

Below is a tentative idea.

## Branches
- master: main repository (latest code is here).
- release_v4.00: the code archive released as v4.00 (Do not modify)
- development_XXX: development & test for project XXX


## When you make some update

1. We recommend you to fork the "master" branch. (or fork "development" if you want to check the confrict with ongoing works not yet merged to "master".)

2. Make some modifications, following the below rules:

- The added scheme should be turned on/off by switch. Better if it does not affect the original codes when the new scheme is turned off.
- If the new scheme causes conflict, please discuss with the manager.
- We appreciate if the sample scripts is prepared to run CaMa-Flood with the new scheme.
- If the new scheme require additional data to run, please prepare a sample data required by the new sheme (better if the size of sample data is minimum). However, the data should be kept outside of GitHub. If your new scheme required data, please contact Yamazaki to discuss a better way of data management in CaMa-Flood package.

3. When your code is ready, please check the conflict with the latest master version. (as master might be updated during your work).
- If you find a conflict which cannot be solved, please contact the code manager.

4. If there is no conflict, please contact the CaMa-Flood manager. We will create a new branch (e.g. development_YYY) to merge your contribution.

5. Please push your new contribution to "development_YYY" branch.

6. Your new contribution is once merged to "development_YYY". Yamazaki will perform a test simulations to confirm your contribution works well. Then, the new scheme is merged to the "master" branch.
