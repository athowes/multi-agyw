# orderly::orderly_develop_start("docs_amazon-poster")
# setwd("src/docs_amazon-poster")

rmarkdown::render("amazon-poster.Rmd")
pagedown::chrome_print("amazon-poster.html")
