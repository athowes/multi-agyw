# multi-agyw

Code for the project *A multinomial spatio-temporal model for sexual risk behaviour*.
This repository is an [`orderly`](https://github.com/vimc/orderly) project, with directories: 

* `src`: containing all reports
* `archive`: containing versioned results of running your report
* `global`: containing global data
* `data`: copies of data used in the reports
* `tutorials`: miscellaneous code used to study models etc.

## R packages

Note that this repository requires a particular branch of the `naomi.utils` package, which can be installed from Github via:

`devtools::install_github("athowes/naomi.utils", ref = "sexbehav-vars-adam")`

## How to run and commit reports

After installing [`orderly`](https://github.com/vimc/orderly) (from either CRAN or Github) a report, let's say called `example`, may be run by:

`orderly::orderly_run(name = "src/example")`

The results of this run will appear in the `draft/` folder.
To commit the draft (with associated `id`) to the `archive/` folder (which should be treated as "read only") use:

`orderly::orderly_commit(id)`

Any outputs of this report will then be available to use as dependencies within other reports.

Reports can be pushed to the HIV inference group sharepoint (the remote) using:

`orderly::orderly_push_archive("example")`

Or can be pulled (alongside any dependencies) from the remote using:

`orderly_pull_archive("example")`

Alternatively, just the dependencies can be pulled using `orderly::orderly_pull_dependencies("example")`.

## Misc to-do

- [ ] Analysis of the extent of the differences between the different models e.g. compute maximum difference between (mean) estimates then arrange in decreasing order
- [ ] Possibility to include covariates
- [ ] Extend Malawi model by adding more surveys (PHIA and MICS). Could use survey specific intercepts
- [ ] Fitting model jointly to multiple countries
- [ ] Should the `utils` scripts be reports? Report to run reports? Report to run report which runs reports?
  - When running an `orderly` report is it possible to create data outside of the draft folder?
  - `run_fitting` is probably best staying as external to `orderly` anyway
  - `pull_naomi_areas` might be better using `orderly_pull_dependencies`
- [ ] Add other different types of simulated data e.g. spatial structure to `sim_sexbehav` and try to recover
- [ ] Create individual data that links the `cluster_id` to area by modifying `mwi_data_survey_behav` (currently it's only the aggregate data that is output)
- [ ] Fit model to Malawi using individual format data. Individual weighted log-likelihood in `R-INLA` might not be possible, see Google group [discussion](https://groups.google.com/g/r-inla-discussion-group/c/Q-STkrFXR0g/m/6PWxRV4tBQ). Could try `TMB`

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
* [How to use `rdhs`](https://cran.r-project.org/web/packages/rdhs/vignettes/introduction.html)
* [Separable models using the `group` option](https://becarioprecario.bitbucket.io/inla-gitbook/ch-temporal.html#separable-models-with-the-group-option) from Bayesian inference with INLA by Virgilio Gómez-Rubio
* [Gaussian Kronecker product Markov random fields](https://raw.githubusercontent.com/hrue/r-inla/devel/internal-doc/group/group-models.pdf) presentation by Andrea Riebler
* [Grouped models](https://faculty.washington.edu/jonno/SISMIDmaterial/8-Groupedmodels.pdf) presentation by Daniel Simpson
* [Primer on crashing INLA models](https://avianecologist.com/2018/05/25/a-primer-on-crashing-inla-models/)
* [Thread on multinomial logit models in Stan](https://github.com/stan-dev/rstanarm/issues/20)

## Improving the estimates for FSW

- Previous estimates from workbook are based upon national estimates of FSW population size. Think about how to integrate these
* Biases and variation in methodology for key population data which vary by country. Survey estimates have more comparable methodology but depending on KP features (for example proportion in households included in survey sampling frame) may have varying bias. See working paper "Laga - Mapping the population size of female sex worker in countries across sub-Saharan Africa"Math Kernel Library
* Survey question "did you have sex in exchange for money or goods" has been critisied. Likely too broad with regard to key population of female sex workers
  * Only the most recent round of DHS has this survey question (change was made in 2013 and started to be implemented in 2015, although this may vary by country with some countries still not using this question). Alternative question regards last three sexual partners
* Other sources of data about key populations
  * The [UNAIDS Key Population Atlas](https://kpatlas.unaids.org/dashboard)
  * Johnston et al. (2021, preprint) Deriving and interpreting population size estimates for adolescent and young key populations at higher risk of HIV transmission: men who have sex with men, female sex workers and transgender persons 
    * Disaggregates the UNAIDS published population size estimates by age using proportion of sexually active adults
    * Kinh is a coauthor
      * Warns that the estimates should be seen as expert opinion rather than based on data
      * Several countries had no data
      * Rounding up when the number is too small
  * Laga et al. (2021, preprint) Mapping female sex worker prevalence (aged 15-49 years) in sub-Saharan Africa
    * Has [code and data](https://github.com/ilaga/Mapping-FSW-SSA)
    * Jeff is a coauthor
      * Variation across countries may be implausibly large
      * Uses study type random effects but implementation differences even within studies belonging to the same group likely to be large
* Possible approaches
  * Move all `sexpaid12m` into `sexnonreg`, then get the FSW estimates from other data sources. Is there a way to integrate this data in a coherent way?
  * Use the `sexpaid12m` data to learn the spatial pattern and the other data sources to learn the level
* Is there a coherent way to use existing estimates? Penalise distance from existing estimates equivalent to placing a prior on estimate? 
  * Sounds similar to Bayesian melding, but implementing Bayesian melding is intractable for all but the simplest models
  * Can we get distributions or standard errors on the existing estimates? Not for Johnston
  * Work of Jon Wakefield / Taylor Okonek on calibration of estimates?
* Other possible data source on men who paid for sex
  * See Hodgkins et al. (2021, preprint)
  * The proportion of men who pay for sex (CFSW) can be estimated from the data, and then this can be linked to the proportion of FSW by some model like `p_{CFSW} = B * p_{FSW}` where a strongly informative prior is placed on `B` (around 10 say)
* [Fully Bayesian benchmarking of small area estimation models](https://sciendo.com/article/10.2478/jos-2020-0010) (Zhang and Bryant, 2020)
  * Zhang and Bryant have quite a few [papers](https://www.bayesiandemography.com/papers) which look interesting

## Notes

* The 13 AGYW Global Fund priority countries are Botswana, Cameroon, Kenya, Lesotho, Malawi, Mozambique, Namibia, South Africa, Swaziland, Tanzania, Uganda, Zambia and Zimbabwe, from ["The Global Fund measurement framework for adolescents girls and young women programs"](https://www.theglobalfund.org/media/8076/me_adolescentsgirlsandyoungwomenprograms_frameworkmeasurement_en.pdf)
* Use same model for all countries or select to best model in each country?
  * Same model for all is a good default, unless something really stands out
* In which countries, ages or categories are there the greatest changes?
