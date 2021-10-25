#' The 13 GF AGYW countries
iso3 <- c("BWA", "CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE")

#' Check that there are indeed 13
stopifnot(length(iso3) == 13)

lapply(iso3,
       function(x) {
       orderly::orderly_pull_archive(
         "aaa_scale_pop",
         id = paste0('latest(parameter:iso3 == "', x, '")'),
         remote = "fertility",
         recursive = TRUE
      )
})
