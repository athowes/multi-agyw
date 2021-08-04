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

## To-do

### Clean-up operations

- [ ] ~5% of rows produced using `softmax` [based on samples](https://github.com/athowes/multi-agyw/blob/1581d6f6bb27fdcaf725cd0956c84b44859019c4/src/aaa_fit_multi-sexbehav-sae/functions.R#L55) using `inla.posterior.sample` from (some subset of) the models result in `NaN`. Find the cause of this issue and solve
- [x] [Local WAIC very large](https://github.com/athowes/multi-agyw/blob/1581d6f6bb27fdcaf725cd0956c84b44859019c4/src/aaa_fit_multi-sexbehav-sae/script.R#L205) for `x_eff` values of zero (this more often occurs for `sexpaid12m`). DIC output also has problems resulting in either `NA` or implausible values. Use the local values to find rows causing the problem, then try to resolve somehow. Could it be related to survey weights?
  - [ ] Have added the diagnostic output but remains to say how to solve it
- [x] [`ind` values outside [0, 1]](https://github.com/athowes/multi-agyw/blob/1581d6f6bb27fdcaf725cd0956c84b44859019c4/src/aaa_fit_multi-sexbehav-sae/script.R#L102) (possibly traced back to `survey::` functions). Try to fix
  - [x] Start by adding a diagnostic warning, if that's possible in `orderly`
  - Values seem to be a tiny amount below zero or above one (and it's occurring when all the responses are either no or yes). could trace this back to the function being used but doesn't seem that much of a priority
- [x] It's not OK to fit overlapping age categories at the same time as e.g. use some data twice to inform precision parameter estimate of age random effects. Find a way to generate 15-24 age category estimates from 15-19 and 20-24. Population size weighting?
  - Population size data can be obtained from Naomi model [outputs](https://imperiallondon.sharepoint.com/sites/HIVInferenceGroup-WP/Shared%20Documents/Forms/AllItems.aspx?csf=1&web=1&e=g7J9el&cid=1beffd0f%2D9df9%2D4a8b%2Db79c%2Df4bed7428f73&RootFolder=%2Fsites%2FHIVInferenceGroup%2DWP%2FShared%20Documents%2FData%2FSpectrum%20files%2F2021%20naomi&FolderCTID=0x0120000FA834E7B0DC9A4A865FA1C3F87255B3) and can be read in using [`spud::sharepoint`](https://github.com/mrc-ide/naomi-orderly/blob/f162d2d227a30150fa078187a8c83ddb84164be0/src/bwa_raw_survey_bwa2013bais_addsexbehav/script.R#L2)
  - In Naomi standard errors for different levels of aggregation are produced using [sparse matrix multiplication in TMB](https://github.com/mrc-ide/naomi/blob/master/src/tmb.cpp#L689). The input matrix `A_out` determining the aggregation is produced by [`naomi_output_frame`](https://github.com/mrc-ide/naomi/blob/master/R/model.R#L1-L74)
  - [ ] This is done for the `aaa_fit_multi-sexbehav-sae` but needs to be transferred to `aaa_fit_all-dhs-multi-sexbehav-sae`
  
### High priority

- [x] Create upper and lower credible estimates of probabilities using `inla.posterior.sample`. See the `multinomial_model` function
- Aggregate format using Kish weights and `xPoisson`, see `INLA::inla.doc("xPoisson")`
  - [x] Model 1: Age-category interaction
  - [x] Model 2: Age-category interaction, space-category interaction (IID)
  - [x] Model 3: Age-category interaction, space-category interaction (BYM2)
  - [x] Model 4: Age-category interaction, space-category interaction using INLA `group` argument (IID)
  - [x] Model 5: Age-category interaction, space-category interaction using INLA `group` argument (BYM2)
    - [ ] This model has crashed INLA for some occasions, try to debug
- [x] Model comparison (DIC or WAIC) among the above models
  - [x] Make model comparison an `artefact` of model fitting, then combine them together in another report
- [x] Create modified datasets for the 13 priority countries
  - [x] Create new branch `sexbehav-vars-adam` in `naomi.utils`, modify `create_sexbehav_dhs` or `extract_sexbehav_dhs` to include changes to coding, and create PR to merge into `sexbehav-vars` (avoiding making alterations to each of the `aaa_data_survey_behav` reports)
- [x] Extend model to more countries by generalising `fit_multi-sexbehav-sae` to `aaa_fit_multi-sexbehav-sae` by not including any Malawi specific analysis, taking `iso3` as parameter input
- [x] Extend Malawi model by adding more DHS surveys (will eventually require more model selection)
  - Temporal smoothing (random walk, what about interactions?)
  - Loss: current time or over all time?
- [x] Add report to calculate "fake" national-level FSW estimates from `sexpaid12m` in order to compare to Johnston et al.
  - [ ] Above is done for Johnston et al., add Laga et al. (Laga do not disaggregate by age though)
- [ ] Fix UGA, TZA, LSO and KEN data creation

### Medium priority

- [x] Add .pdf plot to `process_information-criteria`
- [x] Add standard errors to DIC results
  - [ ] To add to plot once first run with this information is processed
- Split the `aaa_data_survey_behav` tasks up: a lot going on (unclear what exactly to split into)
  - [ ] Modularise linking cluster identifiers to area (talked to Oli about this and looks like something he has done / is doing -- task will be to update relevant scripts to use his tasks once they are available)
- [ ] Analysis of the extent of the differences between the different models e.g. compute maximum difference between (mean) estimates then arrange in decreasing order
- [ ] Understand how the Poisson trick interplays with different structures for multinomial model (baseline category, nested, etc.)
- [ ] Possibility to include covariates
- [ ] Extend Malawi model by adding more surveys (PHIA and MICS). Could use survey specific intercepts
- [ ] Fitting model jointly to multiple countries
- [ ] Should the `utils` scripts be reports? Report to run reports? Report to run report which runs reports?
  - When running an `orderly` report is it possible to create data outside of the draft folder?
  - `run_fitting` is probably best staying as external to `orderly` anyway
  - `pull_naomi_areas` might be better using `orderly_pull_dependencies`
- [x] Katie [has processed](https://github.com/mrc-ide/naomi-orderly/commit/f162d2d227a30150fa078187a8c83ddb84164be0) the BWA data now. Could import this into `multi-agyw` and test, though could also wait until scripts are moved to e.g. `naomi-orderly`. Not high priority but good to check that it's OK for the different data
  - [ ] Added, but still need to test
- [ ] Move the stacked proportion plots to the spatio-temporal models. Little more challenging in that there are now 9 models and and additional variable (survey) to facet over. Starting to be a very large plot, which is OK for internal purposes but hard to share perhaps

### Low priority

- [ ] In plots where the `facet_plot` is over both estimate type and survey, find a way to highlight the surveys together e.g. using boxes. [Here](https://stackoverflow.com/questions/9847559/conditionally-change-panel-background-with-facet-grid) is one possible solution
- [x] Reorder columns of output to a more friendly format, removing things which don't need to be there like identifiers for `R-INLA`
- [ ] Add other different types of simulated data e.g. spatial structure to `sim_sexbehav` and try to recover
- [ ] Create individual data that links the `cluster_id` to area by modifying `mwi_data_survey_behav` (currently it's only the aggregate data that is output)
- [ ] Fit model to Malawi using individual format data. Individual weighted log-likelihood in `R-INLA` might not be possible, see Google group [discussion](https://groups.google.com/g/r-inla-discussion-group/c/Q-STkrFXR0g/m/6PWxRV4tBQ). Could try `TMB`
- [ ] More detailed comparison of differences in the outcomes using `sex12m` versus `eversex` to derive `nosex`
- [ ] Add age at first sex question to help deal with `NA` entries in `eversex`

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

## Improving the estimates for FSW

- Previous estimates from workbook are based upon national estimates of FSW population size. Think about how to integrate these
* Biases and variation in methodology for key population data which vary by country. Survey estimates have more comparable methodology but depending on KP features (for example proportion in households included in survey sampling frame) may have varying bias. See working paper "Laga - Mapping the population size of female sex worker in countries across sub-Saharan Africa"
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
* [Fully Bayesian benchmarking of small area estimation models](https://sciendo.com/article/10.2478/jos-2020-0010) (Zhang and Bryant, 2020)
  * Zhang and Bryant have quite a few [papers](https://www.bayesiandemography.com/papers) which look interesting

## Notes

* The 13 AGYW Global Fund priority countries are Botswana, Cameroon, Kenya, Lesotho, Malawi, Mozambique, Namibia, South Africa, Swaziland, Tanzania, Uganda, Zambia and Zimbabwe, from ["The Global Fund measurement framework for adolescents girls and young women programs"](https://www.theglobalfund.org/media/8076/me_adolescentsgirlsandyoungwomenprograms_frameworkmeasurement_en.pdf)
  * Note that Botswana doesn't have DHS: instead they do their own surveys "Botswana AIDS Impact Survey (BAIS)" (this explains the lack of `src/bwa_data_survey_behav`
* Use same model for all countries or select to best model in each country?
  * Same model for all is a good default, unless something really stands out
* ["Now there remains the question about the DIC infinite of my model that is still unresolved. Do you have any idea?"](https://groups.google.com/g/r-inla-discussion-group/c/KPjQBjpIlrI/m/w006pSqoDgAJ)
  * "This is usually an overflow issue, mainly because of an weakly indentified model. Like DIC would require an integral like \int exp(x) dx and if the marginal variance of x is large, then exp(x) at the upper limit might evaluate to infinity, giving an infinite DIC."
* "with so many hyperparameters we have to increase the number of maximum function evaluations in the derivation of the posterior marginals for the hyperparameters ... `control.inla=list(numint.maxfeval=80000000))`" from [here](https://raw.githubusercontent.com/hrue/r-inla/devel/internal-doc/group/group-models.pdf) 
