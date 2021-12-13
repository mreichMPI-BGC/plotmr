## code to prepare `palettes` dataset goes here

pal1 <- pals::viridis(12)
pal2 <- pals::magma(15)
my.colVec <- c(pal1[4:11],  "grey80", rev(pal2[6:13]))

pal_MR <- NULL
pal_MR$divViriMagma <- colorRampPalette(my.colVec, inter="s")(255)

pal_MR$divTurbo  <- viridisLite::turbo(11) %>%
  replace(6, "grey80") %>%
  {colorRampPalette(., interpolate="l")(255)}

pal_MR$divMPG  <- colorRampPalette(c("#006c66", "lightyellow",  "#6C0006"), inter="l")(15) %>%
  replace(8, "grey80") %>%
  {colorRampPalette(., interpolate="l")(255)}

pal_MR$divRedBlue <- pals::brewer.rdbu(255)

usethis::use_data(pal_MR, overwrite = TRUE)
