# multi-agyw

Code for the project *A multinomial spatio-temporal model for sexual risk behaviour in adolescent girls and young women*.
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
