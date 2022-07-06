# orderly::orderly_develop_start("docs_paper")
# setwd("src/docs_paper")

#' Conversion of figures from .pdf to .jpeg at specified resolution
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/opt/homebrew/bin", sep = ":"))

convert_pdf_jpeg <- function(name) {
  command <- paste0(
    "convert -density 300 depends/", name, ".pdf -scene 1 -background white",
    " -alpha remove -alpha off -quality 80 depends/", name, ".jpeg"
  )
  system(command)
}

#' For paper.Rmd
convert_pdf_jpeg("available-surveys")
convert_pdf_jpeg("3p1-continental-map")
convert_pdf_jpeg("3p1-within-between-country-variation")
convert_pdf_jpeg("infections-reached")

#' For appendix.Rmd
convert_pdf_jpeg("information-criteria")
convert_pdf_jpeg("fsw-logit-information-criteria")
convert_pdf_jpeg("coverage")

#' For tables-figures.Rmd
convert_pdf_jpeg("category-flowchart")
convert_pdf_jpeg("model-direct-benefits")
convert_pdf_jpeg("aaa-variance-proportions")
convert_pdf_jpeg("age-variation")
convert_pdf_jpeg("temporal-interpolation-ribbon")
convert_pdf_jpeg("prev-district-sexbehav-logit")
convert_pdf_jpeg("incidence-district-sexbehav")
convert_pdf_jpeg("infections-reached-country")

#' Render documents
rmarkdown::render("paper.Rmd")
rmarkdown::render("appendix.Rmd")
rmarkdown::render("tables-figures.Rmd")
