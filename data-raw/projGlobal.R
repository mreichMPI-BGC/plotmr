### Projection strings

projGlobal <- list(Robinson="+proj=robin",
                   InterGoodeHomo="+proj=igh",
                   EqualEarth="+proj=eqearth",
                   Eckert4="+proj=eck4",
                   LambertAziEqual="+proj=Laea",
                   NaturalEarth="+proj=natearth",
                   NaturalEarth2="+proj=natearth2",
)

usethis::use_data(projGlobal, overwrite = TRUE)
