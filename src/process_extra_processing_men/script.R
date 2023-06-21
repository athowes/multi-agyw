#' Uncomment and run the two line below to resume development of this script
# orderly::orderly_develop_start("process_extra_processing")
# setwd("src/process_extra_processing")

full_df <- lapply(1:39,function(x) readRDS(paste0("depends/files_",x,".rds")))

full_df <- do.call(rbind,full_df)

#' Save output
write_csv(full_df, "best-3p1-multi-sexbehav-sae.csv")

#' Visualise result
pdf("3p1-boxplots.pdf", h = 5, w = 6.25)

full_df %>%
  filter(
    year == 2018,
    indicator %in% c("sexnonregplus", "sexnonreg", "sexpaid12m")
  ) %>%
  split(.$iso3) %>%
  lapply(function(x)
  x %>%
    ggplot(aes(y = prob_mean, x = 1)) +
      geom_boxplot() +
      facet_wrap(iso3 ~ indicator) +
      theme_minimal() +
      theme(
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank()
      )
  )

dev.off()
