#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("plot-tikz_category-flowchart")
# setwd("src/plot-tikz_category-flowchart")

tools::texi2dvi("category-flowchart.tex", pdf = TRUE, clean = TRUE)
