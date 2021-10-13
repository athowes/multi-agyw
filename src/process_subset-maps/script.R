#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_subset-maps")
# setwd("src/process_subset-maps")

#' Want to create .pdf files containing only the results from particular models
#' (Probably the final model chosen)
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")
pdfs <- paste0("depends/", tolower(iso3), "_all-dhs-multinomial-smoothed-district-sexbehav.pdf")

if (!dir.exists("tmp")) dir.create("tmp")
pdfs_output <- paste0("tmp/", tolower(iso3), "_all-dhs-multinomial-smoothed-district-sexbehav.pdf")

#' Get the page numbers of all maps corresponding the chosen model
pages <- map(pdfs,
  ~ pdftools::pdf_text(.x) %>%
    stringr::str_detect(pattern = paste0("\\(Model ", model, "\\)")) %>%
    which()
)

#' Create the subsetted .pdf files
pmap(
  .l = list(input = pdfs, pages = pages, output = pdfs_output),
  .f = pdftools::pdf_subset
)

#' Combine them into one output artefact
pdftools::pdf_combine(input = pdfs_output, output = paste0("all-dhs-multinomial-smoothed-district-sexbehav.pdf"))

#' Delete the temporary directory
unlink("tmp", recursive = TRUE)
