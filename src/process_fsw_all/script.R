#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_fsw_all")
# setwd("src/process_fsw_all")

#' Could add a new indicator name like ywkp or fsw rather than sexpaid12m
#' This would help clarify that the estimate is now different
df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")
fsw <- read_csv("depends/fsw-estimates.csv")
areas <- readRDS("depends/areas.rds")

# map our area ids in df_3p1 data frame to their level 1 and 2
areas$temp_id <- areas$area_id
areas$temp_id2 <- areas$area_id
areas$level1 <- ifelse(substr(areas$temp_id,5,5)==1,areas$temp_id,NA)
areas$level2 <- ifelse(substr(areas$temp_id,5,5)==2,areas$temp_id,NA)
while(sum(is.na(areas$temp_id2))!=nrow(areas)) {
  areas$temp_id2 <- factor(areas$temp_id, levels = areas$area_id, labels = areas$parent_area_id)
  areas$temp_id2 <- ifelse(nchar(as.character(areas$temp_id2))==3,NA,as.character(areas$temp_id2))
  areas$temp_id <- ifelse(is.na(areas$temp_id2),areas$temp_id,areas$temp_id2)
  areas$level1 <- ifelse(substr(areas$temp_id,5,5)==1,areas$temp_id,areas$level1)
  areas$level2 <- ifelse(substr(areas$temp_id,5,5)==2,areas$temp_id,areas$level2)
}
areas$level0 <- substr(areas$area_id,1,3)
areas <- areas %>% select(area_id, level0, level1 , level2)

df_3p1 <- df_3p1 %>%
  left_join(areas)

fsw_level <- fsw %>%
  group_by(iso3) %>%
  summarise(area_level_fsw = mean(area_level))

fsw_analysis_level <- fsw_level$area_level_fsw
names(fsw_analysis_level) <- fsw_level$iso3

df_3p1 <- df_3p1 %>%
  left_join(
  as.data.frame(fsw_analysis_level) %>%
    tibble::rownames_to_column("iso3"),
  by = "iso3"
  ) %>%
  mutate(fsw_match_area = case_when(fsw_analysis_level == 0 ~ level0,
                                  fsw_analysis_level == 1 ~ level1,
                                  fsw_analysis_level == 2 ~ level2,
                                  TRUE ~ NA_character_))

df_3p1 <- df_3p1 %>%
  select(!(level0:fsw_analysis_level))

fsw <- fsw %>%
  select(age_group, fsw_match_area = area_id, fsw_prop) %>%
  mutate(age_group = fct_recode(age_group, "Y015_019" = "15-19", "Y020_024" = "20-24", "Y025_029" = "25-29",
                                "Y030_034" = "30-34", "Y035_039" = "35-39", "Y040_044" = "40-44", "Y045_049" = "45-49"))

df_3p1_adjusted <- df_3p1 %>%
  left_join(
    fsw,
    by = c("fsw_match_area", "age_group")
  ) %>%
  select(iso3, year, indicator, survey_id, age_group, area_id, area_name, estimate_smoothed, fsw_prop) %>%
  pivot_wider(
    names_from = indicator,
    values_from = estimate_smoothed
  ) %>%
  mutate(
    sexpaid12m = fsw_prop,
    sexnonreg = 1 - nosex12m - sexcohab - sexpaid12m
  ) %>%
  select(-c(fsw_prop)) %>%
  pivot_longer(
    cols = c(nosex12m,sexcohab,sexnonregplus,sexnonreg,sexpaid12m,),
    names_to = "indicator",
    values_to = "estimate_smoothed"
  )

write_csv(df_3p1_adjusted, "adjust-best-3p1-multi-sexbehav-sae.csv")

pdf("adjustment-comparison.pdf", h = 5, w = 6.25)

bind_rows(
  df_3p1 %>%
    filter(indicator == "sexnonregplus", year == 2018) %>%
    mutate(adjustment = "Unadjusted") %>%
    select(age_group, area_id, estimate_smoothed, adjustment),
  df_3p1_adjusted %>%
    filter(indicator == "sexnonreg", year == 2018) %>%
    mutate(adjustment = "Adjusted") %>%
    select(age_group, area_id, estimate_smoothed, adjustment)
) %>%
  pivot_wider(
    names_from = adjustment,
    values_from = estimate_smoothed
  ) %>%
  mutate(iso3 = substr(area_id, 1, 3)) %>%
  ggplot(aes(x = Unadjusted, y = Adjusted, col = iso3, shape = age_group)) +
  geom_point(alpha = 0.5) +
  lims(x = c(0, 0.1), y = c(0, 0.1)) +
  theme_minimal()

dev.off()
