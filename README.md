# hudson-sturgeon
Hudson River Atlantic Sturgeon Abundance Indices.


# Citation: 
Stich, D. S., D. A. Fox, A. L. Higgs, D. C. Kazyak, R. M. Pendleton, and S. A. Sethi. 2025. Reconstructing relative abundance indices for Atlantic Sturgeon using hierarchical ecological models. Transactions of the American Fisheries Society 154:134-142. [https://doi.org/10.1093/tafafs/vnae005](https://doi.org/10.1093/tafafs/vnae005)


# Files
## 2025 data update for ASMFC
Updated data, scripts, and results for ASMFC (2026) that include new (post-publication) data through 2025 are appended with `_2025` where applicable. 


## 2025 NY Bight meeting
Updated data, scripts, and results for the 2025 NY Bight Management meeting with new (post-publication) data are appeneded with `bight-meeting` where applicable.


## Rscripts
`adult-empirical-poisson.R` Runs adult Atlantic Sturgeon N-mixture models using input from `data/` folder and saves output to `results/` folder.
`adult-empirical-poisson-2025-update.R` Updated analysis through 2025 for ASMFC (2026).
`adult-data-update.R` Stand-alone script for updating adult data for NY Bight and ASMFC needs.

`juvenile-empirical-poisson.R` Runs juvenile Atlantic Sturgeon N-mixture models using input from `data/` folder and saves output to `results/` folder.
`juvenile-empirical-poisson-2025-update.R` Updated analysis through 2025 for ASMFC (2026).

`adult-simulation-R1.R` Contains code for conducting simulation-estimation validation of the statistical models based on estimates of adult Atlantic sturgeon for abundance, detection, and population growth estimated from empirical models. Also contains code for processing results and reproducing supplemental figures.
`adult-simulation-R1-bight-meeting.R` Updated script for 2025 NY Bight meeting for running/testing locally.
`adult-simulation-R1-bight-meeting-server.R` Updated script for 2025 NY Bight meeting for running on server.

`combined-plots.R` Contains code for calculating summary statistics and creating plots of parameter estimates from the full posteriors of empirical analyses for juvenile and adult Atlantic Sturgeon.
`ny-bight-plots.R` Updated plots for 2025 NY Bight meeting, including for power analysis.
`combined-plots-2025-update.R`


## Folders
`data/` Contains data files of all data for juvenile and adult Atlantic Sturgeon gill net surveys in the Hudson River, NY 2004-2022.

`models/` Contains files with JAGS code for each of the models used in `adult-empirical-analysis.R` and `juvenile-empirical-analysis.R`. 

`results/` Contains compressed ".rda" files with the results of  juvenile and adult Atlantic sturgeon models generated from the empirical analyses, as well as the output from the simulation-estimation validation study. This directory also contains figures from the manuscripts that are generated from the reproducible workflow in scripts from the `main` directory. Due to long run-sizes (500,000 iterations), the outputs from the empirical analyses (350 MB for adults and 1 GB for juveniles) are not housed in the GitHub repository because they exceed free-use limits on file size for GitHub. These files can be re-created by running the analyses in corresponding files within the `main` directory.
