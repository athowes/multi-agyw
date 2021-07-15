#' Uncomment and run the two line below to resume development of this script
orderly::orderly_develop_start("check_national-fsw-comparison")
setwd("src/check_national-fsw-comparison/")

#' Read in the population size estimates (PSEs) for AYKP
sabin <- read_excel("depends/aykp_pse_july17.xlsx", sheet = "FSW", range = "A3:F187")
names(sabin) <- c("region", "country", "size_15-19", "size_20-24", "size_15-24", "size_25-49")

sabin %>%
  pivot_longer(cols = contains("size"), names_prefix = "size_", names_to = "age") %>%
  filter(region == "ESA")

#' And these are the estimates from the sexpaid12m category of our model
df <- read_csv("depends/all-multinomial-smoothed-district-sexbehav.csv")

df %>%
  filter(indicator == "nosex12m")

#' TODO: Compare these (does df have the higher level)
