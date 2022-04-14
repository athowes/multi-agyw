# orderly::orderly_develop_start("docs_aids22-poster")
# setwd("src/docs_aids22-poster")

rmarkdown::render("aids22-poster.Rmd")
pagedown::chrome_print("aids22-poster.html")
