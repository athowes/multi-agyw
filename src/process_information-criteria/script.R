#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_information-criteria")
# setwd("src/process_information-criteria")

cbpalette <- c("#56B4E9","#009E73", "#E69F00", "#F0E442","#0072B2","#D55E00","#CC79A7", "#999999")

#' aaa_fit_multi-sexbehav-sae

#' A single survey and four categories, so only those countries which have survey question V7191A
iso3 <- c("CMR", "MWI", "ZAF", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_information-criteria.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

df <- df %>%
  mutate(
    model = fct_recode(
      model,
      "1" = "Model 1", "2" = "Model 2", "3" = "Model 3"
    ),
    iso3 = fct_recode(
      iso3,
      "Cameroon" = "CMR",
      "Malawi" = "MWI",
      "South Africa" = "ZAF",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE"
    )
  )

write_csv(df, "model-comparison.csv", na = "")

pdf("model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df, ic = "dic")
ic_plot(df, ic = "waic")
ic_plot(df, ic = "cpo")
ic_plot(df, ic = "pit")

dev.off()

pdf("rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df)

dev.off()

create_latex_table(df, file_name = "model-comparison.txt")

#' aaa_fit_all-dhs-multi-sexbehav-sae

#' Single survey and three categories
iso3 <- c("BWA", "NAM", "SWZ", "TZA", "ZAF")
files <- paste0("depends/", tolower(iso3), "_all-dhs-information-criteria.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

df <- df %>%
  mutate(
    model = fct_recode(
      model,
      "1" = "Model 1", "2" = "Model 2", "3" = "Model 3"
    ),
    iso3 = fct_recode(
      iso3,
      "Botswana" = "BWA",
      "Namibia" = "NAM",
      "Swaziland" = "SWZ",
      "Tanzania" = "TZA",
      "South Africa" = "ZAF",
    )
  )

write_csv(df, "all-dhs-single-model-comparison.csv", na = "")

pdf("all-dhs-single-model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df, ic = "dic")
ic_plot(df, ic = "waic")
ic_plot(df, ic = "cpo")
ic_plot(df, ic = "pit")

dev.off()

pdf("all-dhs-single-rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df)

dev.off()

create_latex_table(df, file_name = "all-dhs-single-model-comparison.txt")

#' Multiple surveys and three categories
iso3 <- c("CMR", "KEN", "LSO", "MOZ", "MWI", "UGA", "ZMB", "ZWE")
files <- paste0("depends/", tolower(iso3), "_all-dhs-information-criteria.csv")
df <- bind_rows(lapply(files, function(file) read_csv(file)))

if(include_interactions) {
  df <- mutate(df, model = fct_recode(
    model,
    "5x" = "Model 5x", "6x" = "Model 6x", "8x" = "Model 8x", "9x" = "Model 9x")
  )
} else {
  #' If we're not including interactions, remove all the models ending in "x" (the interaction models)
  df <- filter(df, !stringr::str_ends(model, "x"))
}

df <- df %>%
  mutate(
    model = fct_recode(
      model,
      "1" = "Model 1", "2" = "Model 2", "3" = "Model 3",
      "4" = "Model 4", "5" = "Model 5", "6" = "Model 6",
      "7" = "Model 7", "8" = "Model 8", "9" = "Model 9"),
    iso3 = fct_recode(
      iso3,
      "Cameroon" = "CMR",
      "Kenya" = "KEN",
      "Lesotho" = "LSO",
      "Mozambique" = "MOZ",
      "Malawi" = "MWI",
      "Uganda" = "UGA",
      "Zambia" = "ZMB",
      "Zimbabwe" = "ZWE"
    )
  )

write_csv(df, "all-dhs-multi-model-comparison.csv", na = "")

pdf("all-dhs-multi-model-comparison.pdf", h = 5, w = 6.25)

ic_plot(df, ic = "dic")
ic_plot(df, ic = "waic")
ic_plot(df, ic = "cpo")
ic_plot(df, ic = "pit")

dev.off()

pdf("all-dhs-multi-rank-comparison.pdf", h = 3.5, w = 6.25)

rank_ic_plot(df)

dev.off()

create_latex_table(df, file_name = "all-dhs-multi-model-comparison.txt")
