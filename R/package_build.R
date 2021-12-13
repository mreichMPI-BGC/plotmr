#' @import rayshader
#' @import raster
#' @import rgl
#' @importFrom purrr walk map walk2
#' @importFrom scales rescale rescale_mid
#' @import magrittr
#' @importFrom rnaturalearth ne_countries
#' @import quadmesh



usethis::use_package("glue", type = "Depends")
usethis::use_package("sf", type = "Depends")
usethis::use_package("fields", type = "Depends")
usethis::use_package("av", type = "Depends")
usethis::use_package("gifski", type = "Depends")
usethis::use_package("png", type = "Depends")
usethis::use_package("magick", type = "Depends")

usethis::use_build_ignore("package_build.R")
