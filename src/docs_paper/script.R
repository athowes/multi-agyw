# orderly::orderly_develop_start("docs_paper")
# setwd("src/docs_paper")

#' Conversion of figures from .pdf to .jpeg at specified resolution
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/opt/homebrew/bin", sep = ":"))

convert_pdf_png <- function(name, dpi = 300) {
  command <- paste0(
    "convert -density ", dpi, " depends/", name, ".pdf -scene 1 -background white",
    " -alpha remove -alpha off -quality 80 depends/", name, ".png"
  )
  system(command)
}

#' For paper.Rmd
#' These are already .png
# convert_pdf_png("available-surveys")
# convert_pdf_png("3p1-continental-map")
# convert_pdf_png("3p1-within-between-country-variation")
# convert_pdf_png("infections-reached")

#' For appendix.Rmd
convert_pdf_png("model-comparison")
convert_pdf_png("fsw-logit-model-comparison")
convert_pdf_png("coverage")
convert_pdf_png("age-disagg-fsw-line")

#' For tables-figures.Rmd
convert_pdf_png("category-flowchart")
convert_pdf_png("model-direct-benefits")
convert_pdf_png("aaa-variance-proportions")
convert_pdf_png("age-variation")
convert_pdf_png("temporal-interpolation-ribbon", dpi = 150)
convert_pdf_png("prev-district-sexbehav-logit", dpi = 150)
convert_pdf_png("incidence-district-sexbehav", dpi = 150)
convert_pdf_png("infections-reached-country", dpi = 150)

#' Render documents
rmarkdown::render("paper.Rmd")
rmarkdown::render("appendix.Rmd")
rmarkdown::render("tables-figures.Rmd")
rmarkdown::render("cover.Rmd")
pagedown::chrome_print("cover.html")
