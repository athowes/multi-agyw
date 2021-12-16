# multi-agyw

Code for the manuscript Howes *et al.* "A multinomial spatio-temporal model for sexual risk behaviour in adolescent girls and young women" (in preparation).

New HIV infections are disproportionately common in adolescent girls and young women.
Accelerating provision of HIV prevention programming in this key population is a top priority, but funding is insufficient to reach everyone ([HIV Prevention Coalition, 2020](https://hivpreventioncoalition.unaids.org/wp-content/uploads/2020/06/Decision-making-aide-AGYW-investment-Version-March-2020-Final.pdf)).
Small-area estimation of risk group population sizes enables countries to prioritise provision of services to those at greatest risk, taking into account both geographic and behavioral factors, as called for in the [Global AIDS Strategy 2021-2026](https://www.unaids.org/en/Global-AIDS-Strategy-2021-2026).

Using [AIS](https://dhsprogram.com/methodology/survey-types/ais.cfm), [BAIS](https://baisv20.com/), [DHS](https://dhsprogram.com/) and [PHIA](https://phia.icap.columbia.edu/) survey data, we fit multinomial logistic regressions to priority countries using the package [`R-INLA`](https://www.r-inla.org/).
To enable inference with the `R-INLA` package, each model is reformulated as an equivalent Poisson log linear model using the multinomial-Poisson transformation [(Baker, 1994)](https://www.jstor.org/stable/2348134?seq=1#metadata_info_tab_contents).

## File structure

The directories of this repository are:

| Directory   | Contains |
|-------------|--------------|
| `docs`      | Presentations and other documentation. |
| `make`      | Scripts used to run the reports. `_make.R` runs everything in order. |
| `src`       | All `orderly` reports. |
| `tutorials` | Miscellaneous code used to study models and get up to speed. |
| `utils`     | Helper scripts for common development tasks. |

## `orderly`

We use the [`orderly`](https://github.com/vimc/orderly) package ([RESIDE, 2020](https://reside-ic.github.io/)) to simplify the process of doing reproducible research.
After installing [`orderly`](https://github.com/vimc/orderly) (from either CRAN or Github) a report, let's say called `example`, may be run by:

```r
orderly::orderly_run(name = "src/example")
```

The results of this run will appear in the `draft/` folder (ignored on Github).
To commit the draft (with associated `id`) to the `archive/` folder (also ignored on Github, and which should be treated as "read only") use:

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

## R packages

This repository requires a particular branch of the `naomi.utils` package, which can be installed from Github via:

```r
devtools::install_github("athowes/naomi.utils", ref = "sexbehav-vars-adam")
```
