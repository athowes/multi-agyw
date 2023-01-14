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

#' The full version with interactions is too slow to run locally, so we use the
#' orderly bundles workflow to run it on DIDEHPC instead. Roughly the method for
#' doing this is as follows:
#'
#' A. On local machine:
#' 1. Create bundle
#' 2. Upload to sharepoint somewhere with spud
#'
#' B. On windows VM, or other drive connected to DIDEHPC:
#' 1. Pull bundle
#' 2. Run bundle
#' 3. Upload to sharepoint
#'
#' C. On local machine
#' 1. Pull bundle
#' 2. Import into archive

#' A
repo <- "multi-agyw"
report <- "fit_multi-sexbehav-sae"
path_bundles <- "bundles"
param <- list(include_interactions = TRUE)

#' A1.
bundle <- orderly::orderly_bundle_pack(path = path_bundles, name = report, parameters = param)

#' A2.
spud <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
folder <- spud$folder("HIVInferenceGroup-WP", paste0("Shared Documents/orderly/", repo, "/", path_bundles), verify = TRUE)
folder$upload(path = bundle$path)
recent_bundle <- dplyr::filter(folder$list(), created == max(created))

#' B
root <- "/Volumes/ath19"
setwd(root)
repo_location <- paste0("~/Documents/phd/", repo, "/")

#' B1.
folder$download(
  path = recent_bundle$name,
  dest = file.path(root, path_bundles, recent_bundle$name)
)

#' B2.
orderly_packages <- yaml::read_yaml(
  file.path(paste0(repo_location, "src/", report, "/orderly.yml"))
)$packages

packages <- list(loaded = c("orderly", orderly_packages))

config <- didehpc::didehpc_config(
  workdir = path_bundles,
  credentials = "ath19",
  cluster = "fi--didemrchnb",
  template = "24Core",
  cores = 4,
  # "fi--dideclusthn"
  # "fi--didemrchnb"
)

#' naomi sourced from Github (not on CRAN)
#' multi.utils sourced from Github (not on CRAN)
#' INLA available from URL
src <- conan::conan_sources(
  packages = c("github::mrc-ide/naomi", "github::athowes/multi.utils"),
  repos = "https://inla.r-inla-download.org/R/stable"
)

ctx <- context::context_save(
  "context",
  packages = packages,
  package_sources = src
)

obj <- didehpc::queue_didehpc(ctx, config = config)

#' Test that queue works correctly
t <- obj$enqueue(sessionInfo())
t$status()
t$result()

#' Run larger job
path <- file.path(recent_bundle$name)
output_path <- file.path("/output")

t <- obj$enqueue(orderly::orderly_bundle_run(
  path = path,
  workdir = output_path
))

t$status()
t$result()

#' C1.
#' Not at this stage yet

#' C2.
#' Not at this stage yet

#' Test with just one country on the cluster

setwd(repo_location)
report <- "aaa_fit-multi-sexbehav-sae-minimal"
param <- list(include_interactions = TRUE, iso3 = "MWI")
path_bundles <- "bundles"

#' A1.
bundle <- orderly::orderly_bundle_pack(path = path_bundles, name = report, parameters = param)

#' A2.
spud <- spud::sharepoint$new("https://imperiallondon.sharepoint.com/")
folder <- spud$folder("HIVInferenceGroup-WP", paste0("Shared Documents/orderly/multi-agyw/", path_bundles), verify = TRUE)
folder$upload(path = bundle$path)
recent_bundle <- dplyr::filter(folder$list(), created == max(created))

#' B
setwd(root)

#' B1.
folder$download(
  path = recent_bundle$name,
  dest = file.path(root, path_bundles, recent_bundle$name)
)

#' B2.
orderly_packages <- yaml::read_yaml(
  file.path(paste0(repo_location, "src/", report, "/orderly.yml"))
)$packages

packages <- list(loaded = c("orderly", orderly_packages))

config <- didehpc::didehpc_config(
  workdir = path_bundles,
  credentials = "ath19",
  cluster = "fi--didemrchnb",
  template = "24Core",
  cores = 4,
  # "fi--dideclusthn"
  # "fi--didemrchnb"
)

#' naomi sourced from Github (not on CRAN)
#' multi.utils sourced from Github (not on CRAN)
#' INLA available from URL
src <- conan::conan_sources(
  packages = c("github::mrc-ide/naomi", "github::athowes/multi.utils"),
  repos = "https://inla.r-inla-download.org/R/stable"
)

ctx <- context::context_save(
  "context",
  packages = packages,
  package_sources = src
)

obj <- didehpc::queue_didehpc(ctx, config = config)

#' Test that queue works correctly
t <- obj$enqueue(sessionInfo())
t$status()
t$result()

#' Run larger job
path <- file.path(recent_bundle$name)
output_path <- file.path("/output")

t <- obj$enqueue(orderly::orderly_bundle_run(
  path = path,
  workdir = output_path
))

t$status()
t$result()

#' Test running locally
id <- orderly::orderly_run(report, param)
orderly::orderly_commit(id)

#' C
bundle_output_location <- file.path(root, "output", t$result()$filename)
orderly::orderly_bundle_import(path = bundle_output_location, root = repo_location)
