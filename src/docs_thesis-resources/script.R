# orderly::orderly_develop_start("docs_thesis-resources")
# setwd("src/docs_thesis-resources")

#' Conversion of figures from .pdf to .jpeg at specified resolution
Sys.setenv(PATH = paste(Sys.getenv("PATH"), "/opt/homebrew/bin", sep = ":"))

convert_pdf_png <- function(name, dpi = 300) {
  command <- paste0(
    "convert -density ", dpi, " depends/", name, ".pdf -scene 1 -background white",
    " -alpha remove -alpha off -quality 80 depends/", name, ".png"
  )
  system(command)
}

convert_pdf_png("age-disagg-fsw-line")
