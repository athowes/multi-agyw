# multi-agyw

Code for the manuscript Howes *et al.* "Spatio-temporal estimates of HIV risk group proportions for adolescent girls and young women across 13 priority countries in sub-Saharan Africa" (preprint on [medRxiv](https://www.medrxiv.org/content/10.1101/2022.07.12.22277551v1)).

New HIV infections are disproportionately common in adolescent girls and young women.
Accelerating provision of HIV prevention programming in this key population is a top priority, but funding is insufficient to reach everyone ([HIV Prevention Coalition, 2020](https://hivpreventioncoalition.unaids.org/wp-content/uploads/2020/06/Decision-making-aide-AGYW-investment-Version-March-2020-Final.pdf)).
Small-area estimation of risk group population sizes enables countries to prioritise provision of services to those at greatest risk, taking into account both geographic and behavioral factors, as called for in the [Global AIDS Strategy 2021-2026](https://www.unaids.org/en/Global-AIDS-Strategy-2021-2026).
Using [AIS](https://dhsprogram.com/methodology/survey-types/ais.cfm), [BAIS](https://baisv20.com/), [DHS](https://dhsprogram.com/) and [PHIA](https://phia.icap.columbia.edu/) survey data, we fit multinomial logistic regressions to priority countries using the package [`R-INLA`](https://www.r-inla.org/).
To enable inference with the `R-INLA` package, each model is reformulated as an equivalent Poisson log linear model using the multinomial-Poisson transformation [(Baker, 1994)](https://www.jstor.org/stable/2348134?seq=1#metadata_info_tab_contents).
We use the resulting proportion estimates to disaggregate HIV prevalence and HIV incidence estimates from the [Naomi small-area estimation model](https://github.com/mrc-ide/naomi) according to risk group membership.

![This figure is produced by the report `plot_aids-abstract`](aids-abstract.png)

## File structure

The directories of this repository are:

| Directory   | Contains |
|-------------|--------------|
| `make`      | Scripts used to run the reports. `_make.R` runs everything in order. |
| `misc`      | Miscellaneous code, not used as part of `orderly`. |
| `src`       | All `orderly` reports. |
| `tutorials` | Miscellaneous code used to study models and get up to speed. |
| `utils`     | Helper scripts for common development tasks. |

## `orderly`

We use the [`orderly`](https://github.com/vimc/orderly) package ([RESIDE, 2020](https://reside-ic.github.io/)) to simplify the process of doing reproducible research.
After installing [`orderly`](https://github.com/vimc/orderly) (from either CRAN or Github) a report, `example`, may be run by:

```r
orderly::orderly_run(name = "src/example")
```

The results of this run will appear in the `draft/` folder (ignored on Github).
To commit the draft (with associated `id`) to the `archive/` folder (also ignored on Github, and treated as "read only") use:

```r
orderly::orderly_commit(id)
```

Any outputs of this report will then be available to use as dependencies within other reports.
Reports can be pushed to the HIV inference group sharepoint (the remote) using:

```r
orderly::orderly_push_archive("example")
```

Or can be pulled (alongside any dependencies) from the remote using:

```r
orderly_pull_archive("example")
```

Alternatively, just the dependencies can be pulled using `orderly::orderly_pull_dependencies("example")`.

## R package dependencies

This repository is supported by the [`mutli.utils`](https://github.com/athowes/multi.utils) package, which can be installed from Github via:

```r
devtools::install_github("athowes/multi.utils")
```

The `R-INLA` package is not currently available on CRAN, and instead may be installed by following [instructions](https://www.r-inla.org/download-install) from the project website.
This repository also requires a particular branch of the `naomi.utils` package, which can be installed from Github via:

```r
devtools::install_github("athowes/naomi.utils", ref = "sexbehav-vars-adam")
```

## Session information

The `sessionInfo()` used to run this analysis is:

```
R version 4.1.0 (2021-05-18)
Platform: x86_64-pc-linux-gnu (64-bit)
Running under: Ubuntu 20.04.2 LTS

Matrix products: default
BLAS:   /usr/lib/x86_64-linux-gnu/blas/libblas.so.3.9.0
LAPACK: /usr/lib/x86_64-linux-gnu/lapack/liblapack.so.3.9.0

locale:
 [1] LC_CTYPE=en_GB.UTF-8       LC_NUMERIC=C               LC_TIME=en_GB.UTF-8        LC_COLLATE=en_GB.UTF-8    
 [5] LC_MONETARY=en_GB.UTF-8    LC_MESSAGES=en_GB.UTF-8    LC_PAPER=en_GB.UTF-8       LC_NAME=C                 
 [9] LC_ADDRESS=C               LC_TELEPHONE=C             LC_MEASUREMENT=en_GB.UTF-8 LC_IDENTIFICATION=C       

attached base packages:
[1] parallel  stats     graphics  grDevices utils     datasets  methods   base     

other attached packages:
 [1] multi.utils_0.1.0 orderly_1.4.3     ggridges_0.5.3    tidyr_1.1.3       spdep_1.1-8       spData_0.3.8     
 [7] sf_0.9-8          scales_1.1.1      readr_2.1.1       naomi_2.3.15      INLA_21.11.16     sp_1.4-6         
[13] foreach_1.5.1     Matrix_1.3-4      ggplot2_3.3.5     forcats_0.5.1     dplyr_1.0.8  
```



