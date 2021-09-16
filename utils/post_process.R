id <- orderly::orderly_run("process_multinomial-smoothed-district-sexbehav")
orderly::orderly_commit(id)

id <- orderly::orderly_run("process_information-criteria")
orderly::orderly_commit(id)

id <- orderly::orderly_run("process_variance-proportions")
orderly::orderly_commit(id)

id <- orderly::orderly_run("process_coverage")
orderly::orderly_commit(id)

id <- orderly::orderly_run("plot_temporal-variation")
orderly::orderly_commit(id)

id <- orderly::orderly_run("plot_within-between-country-variation")
orderly::orderly_commit(id)

id <- orderly::orderly_run("plot_age-variation")
orderly::orderly_commit(id)
