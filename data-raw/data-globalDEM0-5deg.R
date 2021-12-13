## code to prepare `data/globalDEM0.5deg` dataset goes here

globalDEM0.5deg <-
  raster(
    "M:/data/DataStructureMDI/DATA/grid/Global/0d50_static/ETOPO/ETOPO1/Data/ETOPO1.halfdegree.nc"
  )
altitudeInclIce  <-
  raster(
    "M:/data/DataStructureMDI/DATA/grid/Global/0d50_static/Worldclim/v1_4/Data/altitude.nc"
  )

globalDEM0.5deg[!is.na(altitudeInclIce)]  <-  altitudeInclIce[!is.na(altitudeInclIce)]


usethis::use_data(globalDEM0.5deg, overwrite = TRUE)
