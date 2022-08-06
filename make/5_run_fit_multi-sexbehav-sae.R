#' Run model for all countries

#' Options for reducing computation in testing:
#' @param `lightweight` Fit just one model rather than all (eight) considered models
#' @param `fewer_countries` Use five (BWA, MOZ, MWI, ZMB, ZWE) out of the thirteen total countries

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE, fewer_countries = TRUE))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(fewer_countries = TRUE))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(lightweight = TRUE))
orderly::orderly_commit(id) #' [x]

id <- orderly::orderly_run("fit_multi-sexbehav-sae", parameters = list(include_interactions = FALSE, fewer_countries = FALSE))
orderly::orderly_commit(id) #' [x]

#' The full version with interactions is too slow to run locally,
#' so let's try using the orderly bundles workflow

#' A. On local machine:
#' 1. Create bundle
#' 2. Upload to sharepoint somewhere with spud
#' B. On windows VM, or other drive connected to DIDEHPC:
#' 1. Pull bundle
#' 2. Run bundle
#' 3. Upload to sharepoint
#' C. On local machine
#' 1. Pull bundle
#' 2. Import into archive

#' A1.
path_bundles <- "bundle-input"
bundle <- orderly::orderly_bundle_pack(
  path = path_bundles,
  name = "fit_multi-sexbehav-sae",
  parameters = list(include_interactions = TRUE)
)

#' A2.
spud <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
folder <- spud$folder("HIVInferenceGroup-WP", "Shared Documents/orderly/multi-agyw/bundle-input", verify = TRUE)
folder$upload(path = bundle$path)

recent_bundle <- folder$list() %>%
  filter(created == max(created))

#' B1.
folder$download(
  path = recent_bundle$name,
  dest = paste0("bundle-input/", recent_bundle$name)
)

#' B2.
orderly_packages <- yaml::read_yaml(file.path("src/fit_multi-sexbehav-sae/orderly.yml"))$packages
packages <- list(loaded = c("orderly", orderly_packages))

config <- didehpc::didehpc_config(
  workdir = path_bundles,
  credentials = "ath19",
  cluster = "fi--didemrchnb"
  # "fi--dideclusthn"
  # "fi--didemrchnb"
)

src <- conan::conan_sources("athowes/multi.utils")

ctx <- context::context_save(
  "context",
  packages = packages,
  package_sources = src
)

obj <- didehpc::queue_didehpc(ctx, config = config)

t <- obj$enqueue(orderly::orderly_bundle_run(recent_bundle$name, "bundle-output"))

t$status()
t$result()
