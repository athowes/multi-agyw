tasks <- list.files("src")
lapply(tasks, orderly::orderly_develop_clean)
