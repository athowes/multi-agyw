#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("sim_sexbehav")
# setwd("src/sim_sexbehav/")

#' Create df dataframe from which to add simulated data

analysis_level <- c("MWI" = 5)

admin1_level <- c("MWI" = 1)

stopifnot(iso3 %in% names(analysis_level))
stopifnot(iso3 %in% names(admin1_level))

analysis_level <- analysis_level[iso3]
admin1_level <- admin1_level[iso3]

areas <- read_sf(paste0("depends/", tolower(iso3), "_areas.geojson"))

areas <- select(areas, area_id, area_name, area_level, area_level_label,
                parent_area_id, area_sort_order, center_x, center_y)

areas_model <- filter(areas, area_level == analysis_level)

areas_model <- areas_model %>%
  left_join(
    areas %>%
      st_drop_geometry() %>%
      filter(area_level <= analysis_level) %>%
      spread_areas() %>%
      select(area_id, area_id_aggr = paste0("area_id", admin1_level)),
    by = "area_id"
  )

#' Add an integer index for INLA
areas_model <- areas_model %>%
  arrange(area_sort_order) %>%
  mutate(area_idx = row_number())

#' Only using three age groups here
age_groups <- c("Y015_019", "Y020_024", "Y025_029")

df <- crossing(
  age_group = age_groups,
  areas_model %>%
    st_drop_geometry() %>%
    select(area_id, area_name, area_idx, area_id_aggr,
           area_sort_order, center_x, center_y)
)

#' Assume these four categories to be exhaustive and mutually exclusive
indicators <- c("nosex12m", "sexcohab", "sexnonreg", "sexpaid12m")
K <- length(indicators)

#' Make up value of the multinomial probabilities for simulation
#' No spatial structure for now, same probabilities for all areas
prob_Y015_019 <- c(0.6, 0.25, 0.12, 0.03)
prob_Y020_024 <- c(0.2, 0.4, 0.35, 0.05)
prob_Y025_029 <- c(0.05, 0.5, 0.4, 0.05)

#` Sample size for each area age combination is equal
m <- 50

#' Copy df K times, adding cat_idx, representing the category number (1, ..., K)
#' Then copy this another m times, representing the m multinomial observations
df <- df %>%
  dplyr::slice(rep(1:n(), each = K)) %>%
  dplyr::mutate(cat_idx = rep(1:K, times = n() / K)) %>%
  dplyr::slice(rep(row_number(), m))

#' Tall format: (1 x n * K) length vector
tall_rmultinomial <- function(n, prob) {
  replicate(n, rmultinom(1, 1, prob = prob), simplify = TRUE) %>%
    as.vector()
}

#' nrow(areas_model) is the number of areas
samp_Y015_019 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y015_019)
samp_Y020_024 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y020_024)
samp_Y025_029 <- tall_rmultinomial(n = m * nrow(areas_model), prob = prob_Y025_029)

#' Add multinomial samples data, and index for the observation to df
#' There are a total of m * nrow(areas_model) multinomial observations for each age category
#' and the total number of rows of df is given as follows
stopifnot(nrow(df) == nrow(areas_model) * m * length(age_groups) * K)

df <- df %>%
  arrange(age_group) %>%
  mutate(obs_idx = rep(1:(m * nrow(areas_model) * length(age_groups)), each = K),
         y = c(samp_Y015_019, samp_Y020_024, samp_Y025_029))

write_csv(df, "simulated-sexbehav.csv", na = "")
