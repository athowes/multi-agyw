# multi-agyw

Code for the project *A multinomial spatio-temporal model for sexual risk behaviour*.
This repository is an [`orderly`](https://github.com/vimc/orderly) project, with directories: 

* `src`: containing all reports
* `archive`: containing versioned results of running your report
* `global`: containing global data
* `data`: copies of data used in the reports
* `tutorials`: miscellaneous code used to study models etc.

## How to run and commit reports

After installing [`orderly`](https://github.com/vimc/orderly) (from either CRAN or Github) a report, let's say called `example`, may be run by:

`orderly::orderly_run(name = "src/example")`

The results of this run will appear in the `draft/` folder.
To commit the draft (with associated `id`) to the `archive/` folder use:

`orderly::orderly_commit(id)`

Any outputs of this report will then be available to use as dependencies within other reports.

## To-do

Numbers indicate priority.
1:6 are the highest priority items!

- [ ] Format data from the most recent DHS survey in Malawi for multinomial model, and resolve any issues
  - [x] 5. Create a table of conflicts for overlapping categories and propose a mapping
  - [ ] 6. Show differences in the outcomes using `sex12m` versus `eversex` to derive `nosex`
    - [ ] Eventually generalise 5. and 6. to other surveys, moving this analysis earlier in the pipeline. May be quite hard to have a generalised task for `parameters: iso3` and have to be country by country.
  - [ ] 1. Pull spatial data for Malawi, link the `cluster_id` to area
    - Copy code from Katie's `naomi-orderly` task for this
- [x] Generate simulated data to test model and allow model building concurrently -- see `src/sim_sexbehav`
  - [ ] Add different types of simulated data e.g. spatial structure
- Fit model (to simulated data) using `R-INLA` and Poisson trick
  - [x] Can do this for "individual" data structure -- see `src/fit_sim-multi-sexbehav-sae`
  - [ ] Do it for "aggregated" data structure as well
- Fit model to real Malawi data
  - Individual format (depends on 1)
    - [ ] 4. Ignoring weight version + BYM + age
    - [ ] Individual weighted log-likelihood (`TMB`, as `R-INLA` is not doable?)
      - Google group [discussion](https://groups.google.com/g/r-inla-discussion-group/c/Q-STkrFXR0g/m/6PWxRV4tBQAJ) about survey weights in `R-INLA` suggests that the weights scale the log-likelihood
  - Aggregate format (depends on 1)
    - [ ] 2. Ignoring weight version + BYM + age
    - [ ] 3. Kish's weighted (like Jeff's) + BYM + age
      - `xPoisson` may be useful for this, see `INLA::inla.doc("xPoisson")`
- [ ] Decide smoothing structure (space and age) between categories
- [ ] Decide structure for multinomial model (baseline category, nested, etc.)
  - How does this interplay with the Poisson trick?
- [ ] 11. Possibility to include covariates
- Extend Malawi model by considering effect of survey years
  - [ ] 9. DHS, requires adding time and temporal smoothing, could use survey specific intercepts
  - [ ] 10. Adding PHIA and MICS
- Extend model to more countries
  - [ ] 7. Fit a model for all DHS countries – separately (depend on previous steps)
  - [ ] 8. Fit a model for all DHS countries – jointly
    - Biases and variation in methodology for key population data which vary by country. Survey estimates have more comparable methodology but depending on KP features (for example proportion in households included in survey sampling frame) may have varying bias. See working paper "Laga - Mapping the population size of female sex worker in countries across sub-Saharan Africa"
    - Survey question "did you have sex in exchange for money or goods" has been critisied 

## Resources

* [Which logit to use](https://stats.stackexchange.com/questions/307249/guidance-on-when-to-use-cumulative-vs-stopping-ratio-vs-continuation-ratio-vs) SE question
* [The Poisson transform for unnormalised statistical models](https://warwick.ac.uk/fac/sci/statistics/crism/workshops/estimatingconstants/chopin.pdf) slides by Chopin
* [Nested logit model](https://www.youtube.com/watch?v=5MuJ95nHISM) from EPFL MOOC
* [DHS Recode manual](https://dhsprogram.com/publications/publication-dhsg4-dhs-questionnaires-and-manuals.cfm)
* [Multinomial Response Models](https://data.princeton.edu/wws509/notes/c6.pdf) by Rodriguez
* [Ordinal Regression](https://betanalpha.github.io/assets/case_studies/ordinal_regression.html) case study by Betancourt
* [Poisson GLMs and the Multinomial model](http://www.statslab.cam.ac.uk/~qz280/teaching/modelling-2020/L14.pdf) lecture notes from Cambridge
* [Online lecture material](https://online.stat.psu.edu/stat504/lesson/8/8.4) from PennState
* [`orderly`](https://www.vaccineimpact.org/orderly/index.html) documentation
* [Example using survey weight in multinomial model](https://core.ac.uk/download/pdf/95690175.pdf), where they put the weights in the log-likelihood
