#' Run everything in bulk
params <- data.frame(iso3 = c("CMR", "KEN", "LSO", "MOZ", "MWI", "NAM", "SWZ", "TZA", "UGA", "ZAF", "ZMB", "ZWE"))
ids <- orderly::orderly_batch("aaa_fit_multi-sexbehav-sae", params)

#' Running individually
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "CMR"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "KEN"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "LSO"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "MOZ"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "MWI"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "NAM"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "SWZ"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "TZA"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "UGA"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZAF"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZMB"))
# id <- orderly::orderly_run("aaa_fit_multi-sexbehav-sae", parameters = list(iso3 = "ZWE"))
