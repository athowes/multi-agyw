# orderly::orderly_develop_start("docs_review")
# setwd("src/docs_review")

rmarkdown::render("review.Rmd")
rmarkdown::render("cover.Rmd")
pagedown::chrome_print("cover.html")
