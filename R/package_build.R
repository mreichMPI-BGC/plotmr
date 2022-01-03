#' @import rayshader
#' @import raster
#' @import rgl
#' @importFrom purrr walk map walk2
#' @importFrom scales rescale rescale_mid
#' @importFrom terra rast resample
#' @import magrittr
#' @importFrom rnaturalearth ne_countries
#' @import quadmesh
#' @importFrom glue glue
#' @import sf
#' @importFrom fields image.plot
#' @import av
#' @importFrom gifski gifski
#' @import png
#' @import magick


 usethis::use_package("rayshader", type = "Suggests")
 usethis::use_package("raster", type = "Suggests")
 usethis::use_package("terra", type = "Suggests")
 usethis::use_package("rgl", type = "Suggests")

# usethis::use_package("glue", type = "Depends")
# usethis::use_package("sf", type = "Depends")
# usethis::use_package("fields", type = "Depends")
# usethis::use_package("av", type = "Depends")
# usethis::use_package("gifski", type = "Depends")
# usethis::use_package("png", type = "Depends")
# usethis::use_package("magick", type = "Depends")

usethis::use_vignette("plotmr-vignette")

usethis::use_build_ignore("package_build.R")
