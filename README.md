# multi-agyw

Code for the project *A multinomial spatio-temporal model for  for sexual risk behaviour*.
This repository is an [`orderly`](https://github.com/vimc/orderly) project, with directories: 

* `src`: containing all reports
* `archive`: containing versioned results of running your report
* `global`: containing global data
* `data`: copies of data used in the reports
* `tutorials`: miscellaneous code used to study models etc.

## To-do

- [ ] Format data from the most recent DHS survey in Malawi for multinomial model, and resolve any issues
- [ ] Generate simulated data to test model and allow model building concurrently
- [ ] Fit model (to simulated data) using `R-INLA` and Poisson trick or `TMB`
- [ ] Decide smoothing structure (space and age) between categories
- [ ] Decide structure for multinomial model (baseline category, nested, etc.)
- [ ] Possibility to include covariates
- [ ] Extend Malawi model by adding more surveys (PHIA and MICS). Requires adding time and temporal smoothing. Problems with multiple surveys: biases -- could use survey specific intercepts
- [ ] Extend model to more countries
