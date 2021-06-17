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
- [x] Generate simulated data to test model and allow model building concurrently
  - See `src/sim_sexbehav`
  - [ ] Add different types of simulated data e.g. spatial structure
- [ ] Figure out how to deal with survey weights for multinomial data (see if Jeff or Seth knows anything about this)
  - Example found by Kinh [Multinomial additive hazard model to assess the disability burdenusing cross-sectional data](https://core.ac.uk/download/pdf/95690175.pdf), where they put the weights in the log-likelihood
- [ ] Fit model (to simulated data) using `R-INLA` and Poisson trick or `TMB`
- [ ] Decide smoothing structure (space and age) between categories
- [ ] Decide structure for multinomial model (baseline category, nested, etc.)
- [ ] Possibility to include covariates
- [ ] Extend Malawi model by adding more surveys (PHIA and MICS). Requires adding time and temporal smoothing. Problems with multiple surveys: biases -- could use survey specific intercepts
- [ ] Extend model to more countries

## Resources

* [Which logit to use](https://stats.stackexchange.com/questions/307249/guidance-on-when-to-use-cumulative-vs-stopping-ratio-vs-continuation-ratio-vs) SE question
* [The Poisson transform for unnormalised statistical models](https://warwick.ac.uk/fac/sci/statistics/crism/workshops/estimatingconstants/chopin.pdf) slides by Chopin
* [Nested logit model](https://www.youtube.com/watch?v=5MuJ95nHISM) from EPFL MOOC
* [DHS Recode manual](https://dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm)
* [Multinomial Response Models](https://data.princeton.edu/wws509/notes/c6.pdf) by Rodriguez
* [Ordinal Regression](https://betanalpha.github.io/assets/case_studies/ordinal_regression.html) case study by Betancourt
* [Poisson GLMs and the Multinomial model](http://www.statslab.cam.ac.uk/~qz280/teaching/modelling-2020/L14.pdf) lecture notes from Cambridge
