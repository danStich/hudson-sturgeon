# hudson-sturgeon
Hudson River Atlantic Sturgeon Abundance Indices

## Files
`adult-empirical-analysis.R` Runs adult Atlantic Sturgeon N-mixture models using input from `data/` folder and saves output to `results/` folder.

`adult-empirical-analysis.R` Runs juvenile Atlantic Sturgeon N-mixture models using input from `data/` folder and saves output to `results/` folder.

`adult-simulation.R` Contains code for conducting simulation-estimation validation of the statistical models based on estimates of adult Atlantic sturgeon for abundance, detection, and population growth estimated from empirical models. Also contains code for processing results and reproducing supplemental figures.

`combined-plots.R` Contains code for calculating summary statistics and creating plots of parameter estimates from the full posteriors of empirical analyses for juvenile and adult Atlantic Sturgeon.


## Folders
`data/` Contains data files of all data for juvenile and adult Atlantic Sturgeon gill net surveys in the Hudson River, NY 2004-2022.

`models/` Contains files with JAGS code for each of the models used in `adult-empirical-analysis.R` and `juvenile-empirical-analysis.R`. 

`results/` Contains compressed ".rda" files with the results of  juvenile and adult Atlantic sturgeon models generated from the empirical analyses, as well as the output from the simulation-estimation validation study. This directory also contains figures from the manuscripts that are generated from the reproducible workflow in scripts from the `main` directory. Due to long run-sizes (500,000 iterations), the outputs from the empirical analyses (350 MB for adults and 1 GB for juveniles) are not housed in the GitHub repository because they exceed free-use limits on file size for GitHub. These files can be re-created by running the analyses in corresponding files within the `main` directory.