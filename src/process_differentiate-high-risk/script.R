#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_differentiate-high-risk")
# setwd("src/process_differentiate-high-risk")

df_3_aaa <- read_csv("depends/best-3-multi-sexbehav-sae.csv")
df_3 <- read_csv("depends/multi-sexbehav-sae.csv") %>%
  filter(model == "Model 4") #' Temporary solution, should be earlier in pipeline

#' Just want one set of estimates for each country
df_prop <- read_csv("depends/best-fsw-logit-sae.csv") %>%
  filter(!(survey_id %in% c("MWI2015DHS", "ZMB2016PHIA", "ZWE2015DHS")))

df_3p1_aaa <- differentiate_high_risk(df_3_aaa, df_prop)
write_csv(df_3p1_aaa, "best-3p1-aaa-multi-sexbehav-sae.csv", na = "")

df_3p1 <- differentiate_high_risk(df_3, df_prop)
write_csv(df_3p1, "best-3p1-multi-sexbehav-sae.csv", na = "")

#' And now on the samples
#' TODO
# samples <- readRDS("depends/every-3-multi-sexbehav-sae-samples.rds")
# samples_prop <- readRDS("depends/best-fsw-logit-sae-samples.rds")
