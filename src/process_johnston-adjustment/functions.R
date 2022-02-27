#' A dictionary to convert from internal names to human readable names, used in the manuscript.
update_naming <- function(df) {
  df %>%
    mutate(
      age_group = fct_relevel(age_group, "Y015_024", after = 3) %>%
        fct_recode(
          "15-19" = "Y015_019",
          "20-24" = "Y020_024",
          "25-29" = "Y025_029",
          "15-24" = "Y015_024"
        ),
      indicator =
        fct_recode(indicator,
                   "No sex (past 12 months)" = "nosex12m",
                   "Cohabiting partner" = "sexcohab",
                   "Nonregular partner(s)" = "sexnonreg",
                   "YWKP" = "sexpaid12m"
        ),
      iso3 =
        fct_recode(iso3,
                   "Botswana" = "BWA",
                   "Cameroon" = "CMR",
                   "Kenya" = "KEN",
                   "Lesotho" = "LSO",
                   "Mozambique" = "MOZ",
                   "Malawi" = "MWI",
                   "Namibia" = "NAM",
                   "Eswatini" = "SWZ",
                   "Tanzania" = "TZA",
                   "Uganda" = "UGA",
                   "South Africa" = "ZAF",
                   "Zambia" = "ZMB",
                   "Zimbabwe" = "ZWE"
        )
    )
}
