#' The 13 GF AGYW countries
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' Check that there are indeed 13
stopifnot(length(iso3) == 13)

sapply(iso3,
       function(x) {
         tryCatch(
           orderly::orderly_pull_archive(
             name = "aaa_scale_pop",
             remote = "fertility",
             parameters = list(iso3 = x),
             recursive = FALSE
           ),
           error = function(e) paste0("Report corresponding to country ", x, " could not be pulled!")
         )
       }
)
