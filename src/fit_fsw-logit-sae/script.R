# orderly::orderly_develop_start("fit_fsw-logit-sae")
# setwd("src/fit_fsw-logit-sae")

analysis_level <- c("BWA" = 2,
                    "CMR" = 2,
                    "KEN" = 2,
                    "LSO" = 1,
                    "MOZ" = 2,
                    "MWI" = 5,
                    "NAM" = 2,
                    "SWZ" = 1,
                    "TZA" = 3,
                    "UGA" = 3,
                    "ZAF" = 2,
                    "ZMB" = 2,
                    "ZWE" = 2)

admin1_level <- c("BWA" = 1,
                  "CMR" = 1,
                  "KEN" = 1,
                  "LSO" = 1,
                  "MOZ" = 1,
                  "MWI" = 1,
                  "NAM" = 1,
                  "SWZ" = 1,
                  "TZA" = 2,
                  "UGA" = 1,
                  "ZAF" = 1,
                  "ZMB" = 1,
                  "ZWE" = 1)

iso3 <- names(analysis_level)

areas <- readRDS("depends/areas.rds")
ind <- read_csv("depends/survey_indicators_sexbehav.csv")
pop <- read_csv("depends/interpolated_population.csv")

#' Set ind$estimate > 1 to 1, as well as ind$estimate < 0 to 0
ind$estimate <- constrain_interval(ind$estimate, lower = 0, upper = 1)

#' Use only the surveys which contain a specific paid sex question
available_surveys <- read_csv("depends/available-surveys.csv")

giftsvar_surveys <- available_surveys %>%
  filter(giftsvar == 1) %>%
  pull(survey_id)

ind <- ind %>%
  filter(survey_id %in% giftsvar_surveys)

#' There are this many surveys which are suitable
length(unique(ind$survey_id))

areas <- areas %>%
  select(
    area_id, area_name, area_level, area_level_label,
    parent_area_id, area_sort_order, center_x, center_y
  ) %>%
  mutate(
    iso3 = substr(area_id, 1, 3)
  ) %>%
  #' Add column for analysis level
  left_join(
    data.frame(analysis_level) %>%
      tibble::rownames_to_column("iso3"),
    by = "iso3"
  )

#' Areas at the level of analysis
areas_model <- areas %>%
  filter(area_level == analysis_level) %>%
  #' Add area_id for admin1 observation
  left_join(
    areas %>%
      st_drop_geometry() %>%
      filter(area_level <= analysis_level) %>%
      spread_areas() %>%
      select(area_id, area_id_aggr = paste0("area_id", admin1_level)),
    by = "area_id"
  ) %>%
  #' Add an integer index for INLA
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

#' Country level area (not to be included in model)
country <- areas %>%
  filter(area_level == 0) %>%
  mutate(
    area_idx = NA,
    area_id_aggr = NA
  )

#' Create adjacency matrix for INLA
adjM <- spdep::poly2nb(areas_model)
adjM <- spdep::nb2mat(adjM, style = "B", zero.policy = TRUE)
colnames(adjM) <- rownames(adjM)

#' Create the scaffolding for the estimates
df <- crossing(
  #' Only the high and very high risk groups
  indicator = c("sexnonreg", "sexpaid12m"),
  #' Three age groups, plus aggregate category
  age_group = c("Y015_019", "Y020_024", "Y025_029", "Y015_024"),
  #' Both the areas in the model and the aggregate country
  bind_rows(areas_model, country) %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)
