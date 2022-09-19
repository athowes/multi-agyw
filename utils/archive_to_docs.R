#' Set the working directory to the project root
setwd(rprojroot::find_rstudio_root_file())

archive_to_docs <- function(report) {
  #' Artefacts to be moved
  filenames <- yaml::read_yaml(file.path(paste0("src/", report, "/orderly.yml")))$artefacts[[1]]$data$filenames

  #' Latest version in archive
  latest <- orderly::orderly_latest(report)

  #' Copy files over
  files_from <- paste0("archive/", report, "/", latest, "/", filenames)
  files_to <- paste0("docs/", filenames)

  file.copy(from = files_from, to = files_to, overwrite = TRUE)
}

#' Names of the reports to move
archive_to_docs("docs_24-09-21-new-cohort")
archive_to_docs("docs_09-09-21-three-category")
archive_to_docs("docs_30-11-21-coverage")
archive_to_docs("docs_04-01-22-update")
archive_to_docs("docs_19-01-22-stats-epi-group")
archive_to_docs("docs_03-02-22-ea-talk")
archive_to_docs("docs_12-05-22-mrc-gida")
archive_to_docs("docs_28-09-22-toronto")
archive_to_docs("docs_09-22-hdsl")
archive_to_docs("docs_14-10-22-waterloo")
archive_to_docs("docs_amazon-poster")
archive_to_docs("docs_paper")
