#' #' Pull naomi3 data from the sharepoint
#' sharepoint <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
#' url <- "sites/HIVInferenceGroup-WP/Shared Documents/Data/Spectrum files/2021 naomi/Naomi datasets in R/naomi3.rds"
#' path <- sharepoint$download(URLencode(url))
#' naomi3 <- readRDS(path)
#'
#' #' Save space by just saving the countries, ages and sex we are interested in
#' naomi3 <- naomi3 %>%
#'   filter(
#'     iso3 %in% c("CMR", "BWA", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE"),
#'     age_group_label %in% c("15-19", "20-24", "25-29", "15-24"),
#'     #' Only female
#'     sex == "female"
#'   )
#'
#' saveRDS(naomi3, file = "global/naomi3.rds")
