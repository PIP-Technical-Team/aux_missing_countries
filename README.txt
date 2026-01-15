This repository creates a country-year dataset, which will have all the necessary information to predict full welfare distributions for countries with no lined up data. The repository gathers covariates necessary to predict distributions and the coefficients from regression models. Jointly, this can then be used to predict distributions. The method behind is from this paper: https://hdl.handle.net/10986/42676

The repository contains three scripts. 
- 01-code/01-prepare-covariates.R: Gathers all the covariates needed to predict full distributions from various sources including WDI, the UN Population Prospects and more. It creates a country-year dataset with these covariates
- 01-code/02-run-model.R: Matches the covariates gathered to survey-level welfare data in order to run regression models that predict how these covariates relate to welfare. It stores the coefficients from this model
- 01-code/03-save-final-data.R: This script uses the file with the country-year-level covariates from the first script and the estimated coefficients from the second script to save a minimum dataset necessary to predict full distributions, which is then done further in the pipeline. It also contains some checks to make sure all makes sense and an example of how full distributions can be predicted from the output data.
- 01-code/04-update-excel-tool.R: This program downloads the latest covariates data from 03-intermediatedata/covariates.Rda and updates PIP_MissingPoverty_Calculator.xlsx with the data and properly mapped column headers.

To update this repository, it is only necessary to update the first, third, and fourth scripts. The second script should only be updated if the model and coefficients should be re-estimated. 

Note: The file 02-inputdata/wpp.xlsx is not in the GitHub repository because it is too large. It can be retrieved as follows:
Visit 
https://population.un.org/wpp/downloads?folder=Standard%20Projections&group=Most%20used
Select the first file "Compact (most used: estimates and medium projections) (XLSX)"
Save as wpp.xlsx in 02-inputdata folder
