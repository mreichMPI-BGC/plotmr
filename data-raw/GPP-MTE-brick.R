## code to prepare `br` dataset goes here


## Generation script for br
br <- raster::brick("D:/markusr/_FLUXCOM/resample/GPP_MTE_2003.nc.0720.0360.0012.nc") %>%
  raster::readAll() %>%
  raster::flip(direction="y")


usethis::use_data(br, overwrite = TRUE)
