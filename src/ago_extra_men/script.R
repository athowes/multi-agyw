#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("ago_extra")
# setwd("src/ago_extra")

#' Could add a new indicator name like ywkp or fsw rather than sexpaid12m
#' This would help clarify that the estimate is now different
df_3p1 <- read_csv("depends/best-3p1-multi-sexbehav-sae.csv")
areas <- readRDS("depends/areas.rds")

df_3p1_ago <- df_3p1 %>%
  filter(iso3=="AGO")

ago_areas <- filter(areas, substr(area_id,1,3)=="AGO")

df_3p1_ago <- ago_areas %>%
  st_drop_geometry() %>%
  filter(area_level==2) %>%
  select(area_id,parent_area_id, area_name, center_x, center_y) %>%
  full_join(df_3p1_ago %>%
              select(-area_name,-center_x,-center_y),
            by = join_by(parent_area_id == area_id),
            relationship = "many-to-many") %>%
  select(-parent_area_id)

df_3p1_adjusted <- df_3p1 %>%
  filter(iso3!="AGO") %>%
  bind_rows(df_3p1_ago)


write_csv(df_3p1_adjusted, "best-3p1-multi-sexbehav-sae.csv")
