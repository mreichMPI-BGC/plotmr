
#' Maps a matrix of values (=image) onto an RGB (given a palette) to be used in rayshader
#'
#' @description
#'
#' @param values Value matrix to map
#' @param pal The palette to be used
#' @param valRange Range of values to be considered for mapping (min, max)
#' @param trimRange Should values be trimmed to that range?
#' @param na.color color for NA values
#' @param ... parameters to be further passed on
#' @param rescaleMid Should 0 be preferred to be in middle of colorscale? Default:TRUE
#'
#' @return returns array (rgba, col, row)
## @export
#'
#' @examples
values2rgb  <- function(values, pal=palette(), valRange=range(values, na.rm = TRUE, finite = TRUE),
                        trimRange=TRUE, na.color=NA, rescaleMid=T, ... ) {

  ncol <- length(pal)
  if (trimRange) {
    values  <- pmin(pmax(values, valRange[1]), valRange[2])
  }
  if (rescaleMid & (valRange[1]/valRange[2] < 0.25))
    idx <- scales::rescale_mid(values, c(1,ncol), valRange)
  else
    idx <- scales::rescale(values, c(1,ncol), valRange)

  cols <- pal[idx] %>%
    replace_na(na.color) %>% col2rgb(alpha=TRUE)
  cols  <- cols %>% array(dim=c(4, values %>% dim)) %>% aperm(c(3,2,1))
  cols/255.
}

#' Title
#'
#' @param elmat
#' @param coloring
#' @param img_overlay1
#' @param img_overlay2
#' @param water
#' @param wateralpha
#' @param alpha_over
#' @param texture
#' @param zscale
#' @param shadowintens
#' @param shadowReachFactor
#' @param windowsize
#' @param zoom
#' @param valRange
#' @param plot
#' @param ...
#'
#' @return
#' @export
#'
#' @examples ## Plot a colored surface plot of volcano data set
#' rs_surface(elmat=volcano, coloring=volcano, pal=terrain.colors(50), zscale=5)
#' ## Change e.g. coloring, so that it reflects the difference from the mean height
#' rs_surface(elmat=volcano, coloring=abs(volcano - mean(volcano)), pal=pal_MR$divTurbo, zscale=5)
#' ## Coloring now to reflect the slope
#' gg <- imager::imgradient(volcano %>% `dim<-`(c(dim(.), 1,1)))
#' volcanoGradient <- sqrt(as.matrix(gg[[1]])^2+as.matrix(gg[[2]])^2)
#' rs_surface(elmat=volcano, coloring=volcanoGradient, pal=pal_MR$divViriMagma, zscale=5)
rs_surface  <- function(elmat = volcano, coloring=volcano, img_overlay1=NULL, img_overlay2=NULL, water = FALSE,
                        wateralpha=0.5, alpha_over=0.75,
                        texture="bw", zscale=250, shadowintens=0.8, shadowReachFactor=1, windowsize=c(1920, 1080), zoom=0.5,
                        valRange = range(coloring, na.rm = TRUE, finite = TRUE), plot=TRUE, pal=palette(), ...)
{
  surface  <-
    elmat %>%
    sphere_shade(texture=texture,  colorintensity = 1, zscale = zscale) %>%
    #ambient_shade() %>%
    add_shadow(ray_shade(elmat,zscale=zscale/shadowReachFactor),max_darken = 1-shadowintens) %>%
    add_overlay( coloring %>% values2rgb(valRange = valRange, pal=pal, ...), alphalayer = alpha_over)
  if (!is.null(img_overlay1))
    surface %<>% add_overlay(img_overlay1, alphalayer = 1, alphamethod = "max")
  if (!is.null(img_overlay2))
    surface %<>% add_overlay(img_overlay2, alphalayer = 0.8, alphamethod = "max")

  if (plot) {
    rgl.clear()
    surface %T>%
      rayshader::plot_map() %T>%
      rayshader::plot_3d(zscale = zscale, heightmap = elmat, water=water, windowsize = windowsize, zoom = zoom, wateralpha = wateralpha, ...)

  }
  invisible(surface)
}



#' Render a rasterStack into 3D rgl png images and animiations
#'
#' Takes a rasterstack or a path to a NetCDF file and generates 3D vizualisations from the data,
#' offering a couple of options (e.g. flat or spherical viz; interpolation between layers).
#' The values can be either represented as colors **and** height, or the height can be
#' specified by a separate (static) raster.
#'
#' @param brick The rasterStack or brick to be visualized. Default: br brick in this package
#' @param eleRast The raster to be used for elevation. Default: globalDEM0.5deg in this package
#' @param eleRastOnly4NA Should the elevation raster only be used in case of NA in the \code{brick}? Default: TRUE
#' @param renderOcean Should the ocean be rendered (with a blue transparent)? Default: FALSE
#' @param renderSphere Should the data set be rendered as a sphere? Default: FALSE
#' @param useRayShade4Sphere Should the rayshader flat image (e.g. including shadows) be used as texture for the spherical viz? Default: TRUE
#' @param renderFakeSphere Should the rayshader flat image just wrapped around the sphere without elevation effects. Not recommended. Default: FALSE
#' @param sphereExtFac Indicates how pronouned the elevation should be. 0 = no elevation effect, 1 = range of elevations is radius of the sphere. Default: 0.2
#' @param renderCountries Should the country borders be rendered? Default: FALSE
#' @param minVal Set manually the minimum value in the brick for color and elevation scaling. If not set, will be derived from data, but it seems brick need to be in memory then!
#' @param maxVal Set manually the maximum value in the brick for color and elevation scaling. If not set, will be derived from data, but it seems brick need to be in memory then!
#' @param nudgeMin2ZeroElev If TRUE, the minimum elevation from the data will be set to zero (= Ocean level). Default= FALSE
#' @param maxFac Can be used to reduce the maximum to stretch color scale and elevation (e.g. if derived from range of data). Default: 1.0
#' @param gaussianSmoothSigma If >0, applies a spatial Gaussian smoothing with sigma. Default: 0
#' @param zscaleRatio The elevetation range to pixel size will be this ratio. Default: 40 (i.e. height is 40 times the pixel size)
#' @param col4NA COlor used for NA. Default: "white"
#' @param elevat4NA The elevation to be the used if NA. Default: 0
#' @param oceanCol COlor of the ocean. Default: "paleturquoise"
#' @param nSubSteps Substeps (Interpolation in time) per layer (to make animation smoother). 1 means no interpolation. Default: 2
#' @param rewindLoop Should, for the animation, the sequence by reversed (forth and back). Default: FALSE
#' @param loop How many loops should be rendered in the animation. Default: 0
#' @param append1st Should the first layer be appended? Helpful to make a smooth seasonal cycle. Default: TRUE
#' @param outPrefix Name of the subfolder and prefix of output file name. Default: "Animation"
#' @param outFolder Folder where results are written. Default: tempdir()
#' @param framerate Video framerate. Default: 8
#' @param titles Vector of title of length number of layers: Default: NULL
#' @param justReRenderVideo Should just the video be (re-)rendered (e.g. with different framerate; then pngs must be there already). Default: FALSE
#' @param renderVideo Should a video be rendered? Default: TRUE
#' @param renderLegend Should a color legend be rendered= Default: TRUE
#' @param leg.tit Title of the legend, intended to contain the unit Default: \code{expression(g~C~~m^{-2}~day^{-1})}
#' @param caption Caption of pngs and video. Default: ""
#' @param resume Should the process just be resumed (then files in folder will not be overwritten, but rather continued). Default: FALSE
#' @param nRounds One round is going once through all layers. Several rounds makes sense if combined with varying view angles. Default: 1
#' @param thetaStartEnd Start and end view horizontal angle. Default: C(0,0)
#' @param phiStartEnd Start and end vertical angle: Default: \code{c(0, 0) + 45*!renderSphere}
#' @param cntStart Can change where the file number counting starts. Default: 0
#' @param over2 Can be another overlay image. Default: NULL
#' @param ...  parameters passed on to other functions
#' @param over An overlay to be plotted. Can be an filepath to an image, or SpatialPolygonDataFrame from wihch an overlay image is generated with \code{rayshader::generate_line_overlay}. Default: NULL
#' @param useOnlyOver If TRUE only the overlay is used in the spherical rendering and br data ignored. Otherwise the texture is "somehow" combined with the plotcolor in \code{shade3d}. Default: FALSE
#' @param pal The palette to be used. Vector of colors. Default: \code{palette()}
#'
#' @return Returns an array of generated files (so that can used in pipe with \code{renderVideos})
#' @export
#'
#' @examples ## Create pngs for animation of maps on a sphere
#' ## Open an rgl window with defined size first
#' rgl::open3d(windowRect = 50 + c(0,0,1920,1080))
#'
#' ## Just a single frame br[[6]] for brevity (br is a brick with 12 layers)
#' ## Simple sphere without elevation model and ocean
#' brick2movie(br[[6]], eleRast = NULL, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=T, pal=rev(pal_MR$divViriMagma))
#'
#' ## Sphere with elevation model and ocean
#' brick2movie(br[[6]], eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=T, renderOcean=T, pal=rev(pal_MR$divViriMagma))
#'
#' ## Same but now a flat projection
#' brick2movie(br[[6]], eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=F, renderOcean=T, pal=rev(pal_MR$divViriMagma))
#'
#' ## Now an animation with interpolation and 10 rounds and include country borders
#' ## This creates 480 frames ==> takes a bit of time
#' brick2movie(br, eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = T, renderSphere=T, renderOcean=T, pal=rev(pal_MR$divViriMagma), renderCountries = T, thetaStartEnd = c(0,360), nSubSteps = 4, nRounds = 10)
#'
#' ## The same but flat ==> just change rendersphere to 'F'
#' brick2movie(br, eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = T, renderSphere=F, renderOcean=T, pal=rev(pal_MR$divViriMagma), renderCountries = T, thetaStartEnd = c(0,360), nSubSteps = 4, nRounds = 10)
#'
#' ## Also works for regions
#' africa  <- crop(br, extent(-23.906250,55.458984, -37.889187,39.364140) )
#' ## Flat
#' brick2movie(africa[[6]], eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=F, renderOcean = T, renderCountries = T, pal=rev(pal_MR$divViriMagma))
#'
#' ## Sphere
#' brick2movie(africa[[6]] %>% raster::disaggregate(2), eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=T, renderOcean = T, renderCountries = T, pal=rev(pal_MR$divViriMagma))
#'
#' ## Or with projections
#' prj <- projGlobal$Robinson
#' brick2movie(br[[6]] %>% aggregate(2) %>% terra::rast()  %>% terra::project(prj, mask=T) %>% raster(), eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=F, renderOcean = T, renderCountries = T, pal=rev(pal_MR$divViriMagma), phiStartEnd = c(50,50), elevat4NA = NA, title=paste("June GPP", prj ))

brick2movie <-
  function(brick = br,
           eleRast = globalDEM0.5deg,
           eleRastOnly4NA = TRUE,

           renderOcean = FALSE,
           renderSphere = FALSE,
           useRayShade4Sphere = FALSE,
           renderFakeSphere = FALSE,
           sphereExtFac = 0.2,
           renderCountries = FALSE,
           minVal = NULL,
           maxVal = NULL,
           nudgeMin2ZeroElev = F,
           maxFac = 1.0,
           gaussianSmoothSigma = 0,
           zscaleRatio = 40.,
           col4NA = "white",
           elevat4NA = 0.0,
           oceanCol = "paleturquoise",

           nSubSteps = 1,
           rewindLoop = F,
           loop = 0,
           append1st = TRUE,
           outPrefix = "Animation",
           outFolder = tempdir(),
           framerate = 8,
           titles = NULL,
           justReRenderVideo = FALSE,
           renderVideo = TRUE,
           renderLegend = TRUE,
           leg.tit=expression(g~C~~m^{-2}~day^{-1}),
           caption="",
           resume=FALSE,
           nRounds = 1,
           thetaStartEnd = c(0, 0),
           phiStartEnd=c(0, 0) + 45*!renderSphere,
           cntStart = 0,
           over = NULL,
           useOnlyOver=FALSE,
           over2 = NULL,
           pal=palette(),
           ...) {

    if (!justReRenderVideo) {
      if (is.null(brick))
        br <- eleRast
      else
        br  <- brick

      if (is.null(br)) br <- raster(vals=0.0)
      dimbr <- dim(br)

      dir.create(glue::glue("{outFolder}/{outPrefix}/"), showWarnings = F )

      eleRastClass <- class(eleRast)
      if (str_starts(eleRastClass, "Raster")) {
        #eleRast %<>% resample(br)
       # eleRast %<>% projectRaster(to=br)
       # elmat  <-  eleRast %>%  raster_to_matrix()
        #elmat  <- eleRast %>% terra::rast() %>% terra::project(crs(br) %>% as.character(), mask=T) %>% raster() %>% raster_to_matrix()
        elmat  <- eleRast %>% terra::rast() %>%   terra::project(crs(br) %>% as.character(), mask=T) %>%
          terra::resample(terra::rast(br)) %>% raster()  %>% raster_to_matrix()

              }

      #over <- generate_line_overlay(ne_coastline() %>% sf::st_as_sf(), extent(br), heightmap = elmat, linewidth = 0.5)
      if (!is.null(over)) {
        classOver <- class(over)[1]
        if (classOver == "sf" || str_detect(classOver, fixed("Spatial", ignore_case = T))) {
          over <- generate_line_overlay(
            sf::st_as_sf(over) %>% sf::st_cast("MULTILINESTRING") %>% sf::st_transform(crs(br)),
            extent(br),
            heightmap = matrix(0, nrow=dimbr[2], ncol=dimbr[1]),
            linewidth = 0.5 * sqrt(prod(dimbr[1:2])/360/720)
          )

        }

        if (classOver %in% c("glue","character")) {
          over <- imager::load.image(over) %>% imager::resize(size_x = dimbr[2], size_y=dimbr[1]) %>%
            as.array() %>% drop() %>% aperm(c(2,1,3))
        }

      }

      if (renderCountries) {
        overCountries <- generate_line_overlay(
           sf::st_as_sf(rnaturalearth::countries110) %>% sf::st_cast("MULTILINESTRING") %>% sf::st_transform(crs(br)),
          extent(br),
          heightmap = matrix(0, nrow=dimbr[2], ncol=dimbr[1]),
          linewidth = 0.5 * sqrt(prod(dimbr[1:2])/360/720)
        )
        if (is.null(over)) {
          over <- overCountries
        } else {
          alphas <- overCountries[,,4]
          for (i in 1:3) over[,,i] <- overCountries[,,i]*alphas + over[,,i]*(1-alphas)
          if (dim(over)[3]==4) over[,,4] <- pmax(over[,,4], alphas)
        }
      }




      smoothButReinsertNA <- function(r, sigma) {
        r_out <- focal(r,
                       w = focalWeight(r, sigma * sqrt(prod(res(r))), type = "Gauss"),
                       na.rm = T,
                       pad = T
        ) %>% mask(r)
      }

      # if (gaussianSmoothSigma > 0) br <- map(br %>% as.list, ~focal(.x, w=focalWeight(.x, gaussianSmoothSigma, type="Gauss"), na.rm=T)) %>% stack
      if (gaussianSmoothSigma > 0)
        br <-
        map(br %>% as.list,
            ~ smoothButReinsertNA(.x, gaussianSmoothSigma)) %>% stack

      if (minVal %>% is.null)
        minVal <- minValue(br) %>% min(na.rm=T)
      if (maxVal %>% is.null)
        maxVal <- maxValue(br) %>% max(na.rm=T)
      maxVal <- maxVal * maxFac

      if (renderLegend) {
        #ncol <- length(palette())
        if ((minVal/maxVal < 0.25))
          dummy <- scales::rescale_mid(c(minVal, maxVal), to=c(1, length(pal))) %>% as.integer()
        else
          dummy <- scales::rescale(c(minVal, maxVal), to=c(1, length(pal))) %>% as.integer()
         #dummy <- scales::rescale_mid(c(minVal, maxVal), to=c(1, length(pal))) %>% as.integer()
        png(glue::glue("{outFolder}/{outPrefix}/legend.png"),width=350, height=800)
        par(cex=2.6)
        print(fields::image.plot(legend.only = T, col=pal[seq(dummy[1], dummy[2])], legend.width = 3, zlim=c(minVal, maxVal),
                                 legend.lab = leg.tit, legend.cex = 3, legend.line = 3, legend.mar=24,  legend.args=list( cex=1.5, side=4, line=2)))
        par(cex=1)
        dev.off()

      }

      #print(maxVal)
      goodzScale <-
        (maxVal - minVal) / zscaleRatio # a zscale of (maxVal-minVal) would mean the range of GPP gets only to the height of one pixel width
      # factor 40 (zscaleRatio) means this range will be 40 times a pixel width

      cnt <-  0
      total <- (nSubSteps * (nlayers(br) - 1 + append1st) +!append1st) * nRounds
      allTheta <- approx(thetaStartEnd, n=total)$y
      allPhi <- approx(phiStartEnd, n=total)$y
      if (is.null(titles)) titles <- rep("", total)
      for (i in seq(nlayers(br) - 1 + append1st)) {
        addlast <- ifelse(i == (nlayers(br) - 1) & !append1st, 1, 0)
        for (j in 0:(nSubSteps - 1 + addlast)) {
          #theta <- cnt / (total + 1) * diff(thetaStartEnd) + thetaStartEnd[1]
          cnt <- cnt + 1
          if (resume &
              file.exists(glue::glue("{outFolder}/{outPrefix}/{outPrefix}_{str_pad(cnt+cntStart-1,5,pad='0')}.png"))) next
          theta <- allTheta[cnt]
          phi <- allPhi[cnt]
          lambda  <-  j / nSubSteps

          print(glue::glue("{cnt} of {total/nRounds} steps: layer={i}, substep={j}, theta={format(theta)}, phi={format(phi)}, title={titles[i+(addlast==1 & lambda==1)]}"))

          valOrig <- ((1 - lambda) * br[[i]] + lambda * br[[i %% nlayers(br) + 1]]) %>% raster_to_matrix()
          valFilt <- imagine::quantileFilter(valOrig, 3, 0.001)

          if (str_starts(eleRastClass, "Raster") & !eleRastOnly4NA) {
            height2Plot  <- elmat
          } else {

            height2Plot <- valOrig
            height2Plot[is.na(valOrig)]  <- valFilt[is.na(valOrig)] #* 0.0 + minVal

          }

          if (str_starts(eleRastClass, "Raster") & eleRastOnly4NA) {
            height2Plot  <- ifelse(is.na(height2Plot),
                                   elmat / diff(range(elmat, na.rm=TRUE)) * diff(range(height2Plot, na.rm=T)),
                                   height2Plot - nudgeMin2ZeroElev * min(height2Plot, na.rm=T))
          }

          goodzScale <- diff(range(height2Plot, na.rm=TRUE)) / zscaleRatio




          if (renderOcean) {
            ## Missing values of GPP are replaced by the elevation matrix for the heights, but scaled such that
            ## the non-GPP heights are too big (20000 is approx the range of elmat from ocean bottom to Mt. Everest..)
            #elmat4plot <- ifelse(elmat < 0 | is.na(height2Plot), elmat/(20000/(maxVal-minVal)), height2Plot - minVal )
            # elmat4plot <-
            #   ifelse(
            #     elmat < 0 |
            #       is.na(height2Plot),
            #     elmat / (20000 / (maxVal - minVal)),
            #     height2Plot - nudgeMin2ZeroElev * minVal
            #   )
            #elmat4plot <- height2Plot
            water <- array(dim = c(dim(elmat) %>% rev(), 4))
            wCol <- col2rgb(oceanCol)
            for (i2 in 1:3)
              water[, , i2] <- wCol[i2] / 255
            water[, , 4] <-
              (0.5 * (is.na(valFilt) & (elmat < 0))) %>% t()
            # Modified to work with Sphere
            # water[, , 4] <-
            #   (0.5 * ((elmat < 0))) %>% t()



            #rgl.clear()
            res1 <-
              rs_surface(
                height2Plot %>% replace_na(elevat4NA),
                zscale = goodzScale,
                valOrig,
                water = F, #!renderSphere,

                shadowReachFactor = 2,
                shadowintens = 0.7,
                watercolor = oceanCol,
                zoom = 0.7,
                solid = T,
                windowsize = c(1920, 1080),
                valRange = c(minVal, maxVal),
                img_overlay1 = over,
                img_overlay2 = water,
                theta = theta,
                plot = !renderSphere & !renderFakeSphere,
                na.color=col4NA,
                pal=pal,
                ...
              )
          } else {
            #rgl.clear()
            res1 <-
              rs_surface(
                height2Plot %>% replace_na(elevat4NA),
                zscale = goodzScale,
                valOrig,
                water = FALSE,
                shadowReachFactor = 2,
                shadowintens = 0.8,
                zoom = 0.7,
                windowsize = c(1920, 1080),
                valRange = c(minVal, maxVal),
                img_overlay1 = over,
                img_overlay2 = over2,
                theta = theta,
                plot = !renderSphere & !renderFakeSphere,
                na.color=col4NA,
                pal=pal,
                ...
              )
          } #else renderOcean


          #title3d(main=month.name[i])

          ## If classical projected way using rayshader, use the rayshader function to create
          ## the snapshot
          #if (!renderFakeSphere & !renderSphere)


          ## Otherwise use rgl functions
          if (renderFakeSphere) {
            png::writePNG(res1, target = "Earth.png")
            spheres3d(
              0,
              texture = "Earth.png",
              textype = "rgb",
              color = "white",
              specular = "black",
              texminfilter = "linear.mipmap.linear",
              texmagfilter = "linear",
              texmipmap = T
            )
            # title3d(main = titles[i + (addlast == 1 &
            #                              lambda == 1)], size = 9)
            #view3d(theta = theta, phi=phi)
            # snapshot3d(
            #   filename = glue::glue(
            #     "{outFolder}/{outPrefix}/{outPrefix}_{str_pad(cnt+cntStart-1,5,pad='0')}.png"
            #   )
            # )

          }

          if (renderSphere) {
            rst <- raster(t(height2Plot), template=br) %>% reclassify(cbind(NA, elevat4NA))
            rasterHasNA  <- any(is.na(rst[]))
            #png::writePNG(res, target = "Earth.png")
            tr <- NULL
            if (useRayShade4Sphere) tr <-  255*raster::stack(raster((res1[,,1]), template=br),
                                                             raster((res1[,,2]), template=br),
                                                             raster((res1[,,3]), template=br  ))
            else {
              if (!is.null(over)) tr <- 255*raster::stack(raster(over[,,1], template=br),
                                                           raster(over[,,2], template=br),
                                                           raster(over[,,3], template=br)
              )
              # if (!is.null(over2))  tr <- 255*raster::stack(raster(over2[,,1], template=br),
              #                                               raster(over2[,,2], template=br),
              #                                               raster(over2[,,3], template=br)
              # )
              colrst <- raster(t(valOrig), template=br)#; colrst[is.na(colrst)]  <- -1
              qmCol <- quadmesh(colrst, na.rm=F)
            }
            qm <- quadmesh(rst, na.rm=F, texture=tr) #, texture="Earth.png")  #%>% reclassify(cbind(NA,NA, -1), right=F) %>% aggregate()
            heightRange <- range(qm$vb[3,], na.rm=T)
            if (prod(heightRange) < 0) # if range crosses zero then have rho(0) = 1
              shiftZero <- 0.0
            else
              shiftZero  <- heightRange[1]
            rho <- 1 + (sphereExtFac) * (qm$vb[3,] - shiftZero)/diff(heightRange)
            x <-  rev(cos((qm$vb[1,]-90)/180*pi)) * sin((qm$vb[2,]-90)/180*pi) * 100 * rho # (100 + qm$vb[3,])
            z <- rev(sin((qm$vb[1,]-90)/180*pi)) * sin((qm$vb[2,]-90)/180*pi) * 100 * rho # (100 + qm$vb[3,])
            y <-  cos((qm$vb[2,]-90)/180*pi) * 100*rho
            qms <- qm
            qms$vb[1,]  <- x; qms$vb[2,] <- y; qms$vb[3,] <- z
            if (!rasterHasNA) qms  <- rgl::addNormals(qms)

            # pal[scales::rescale_mid(values, c(1,ncol), valRange)]

            rgl::clear3d()
            #clear3d(type="lights"); light3d()#0,23, specular="grey50", viewp=T)
            if (useRayShade4Sphere | useOnlyOver) {
              # light3d(-45,-15, specular="white", viewp=T)
              qms$material$color <- "white"
              shade3d(qms, specular="grey30", shininess=80)
            }
            else {
              qms$colorValue  <- scales::rescale_mid(qmCol$vb[3,qmCol$ib], c(1,length(pal)), c(minVal, maxVal))
              plotColor <- pal[qms$colorValue]
              plotColor[is.na(plotColor)] <- col4NA
              shade3d(qms, col = plotColor , specular="grey30", meshColor="legacy", shininess=30)

            }
            if (renderOcean) {
              #ocean <- is.na(qmCol$vb[3,qmCol$ib]) & (qm$vb[3,qm$ib] < 0)
              #plotColor[ocean]  <- oceanCol

              ocean <- qm
              x <-  rev(cos((qm$vb[1,]-90)/180*pi)) * sin((qm$vb[2,]-90)/180*pi) * 100
              z <- rev(sin((qm$vb[1,]-90)/180*pi)) * sin((qm$vb[2,]-90)/180*pi) * 100
              y <-  cos((qm$vb[2,]-90)/180*pi) * 100

              ocean$vb[1,]  <- x; ocean$vb[2,] <- y; ocean$vb[3,] <- z
              ocean$texcoords <- NULL

             # shade3d(ocean, col = oceanCol , specular="black", meshColor="legacy", alpha=0.8 * (plotColor == col4NA))
              shade3d(ocean, col = oceanCol , specular="black", meshColor="legacy", alpha=0.8 * is.na(qms$colorValue))


            }

          }
          for (round in 1:nRounds) {
            cnt2 <- (round-1) * (nlayers(br) - 1 + append1st) * (nSubSteps + addlast) + cnt
            theta <- allTheta[cnt2]
            phi <- allPhi[cnt2]
            view3d(theta = theta, phi=phi, zoom=0.7)
            pngOut <- glue::glue("{outFolder}/{outPrefix}/{outPrefix}_{str_pad(cnt2+cntStart-1,5,pad='0')}.png")
            render_snapshot(
              filename = pngOut,
              title_text = titles[i + (addlast == 1 & lambda == 1)],
              title_position = "North",
              title_size = 50
            )
            if (renderLegend) decorate_png(file = pngOut, legend_file = paste0(dirname(pngOut),"/legend.png"),
                                           caption = caption, overwrite = TRUE)
          } # round
        } # j
      } # i
    } # if !justrendervideo
    files <-
      list.files(
        path = glue::glue("{outFolder}/{outPrefix}/"),
        pattern = glue::glue("{outPrefix}_.*.png"),
        full.names = T
      )
    if (renderVideo) {
      renderVideos(files, rewindLoop = rewindLoop, loop=loop, framerate = framerate, ... )
    } #rendervideo
    ### WITH LOOP ONE NEEDS:
    # av::av_encode_video(files, output=glue::glue("{outFolder}{outPrefix}_animation.mp4"), framerate = 4,
    #                     vfilter = "loop=3:29:0")
    # gifski::gifski(files, gif_file = glue::glue("{outFolder}{outPrefix}_animation.gif"), delay = 0.25, width=2400, height=1200)

    if (justReRenderVideo) res1 <- "Just the video was rendered"
    if (!exists("res1") & resume)  res1 <- "No result produced with resume, everything was there!"
    invisible(files)

  }


#' Renders mp4 and gif videos from array of image files
#'
#' @param files Array of files used to create animation
#' @param rewindLoop Should animation go forth and back? Default: FALSE
#' @param loop How often should video be looped? Default: 0
#' @param framerate Framerate. Default: 16
#' @param makegif Should also a gif be created with gifski? Default: FALSE
#' @param gifShrinkage Factor by which gif should be shrinked
#'
#' @return mp4 filename
#' @export
#'
#' @examples brick2movie(br, eleRast = globalDEM0.5deg, gaussianSmoothSigma = 1, renderVideo = F, renderSphere=T, renderOcean=T, pal=rev(pal_MR$divViriMagma), renderCountries = T, thetaStartEnd = c(0,360), nSubSteps = 2, nRounds = 10) %>% renderVideos()
renderVideos <- function(files, rewindLoop=F, loop=0, framerate=16, makegif=FALSE, gifShrinkage=4, ...) {
  dir <- dirname(files[1])
  if (rewindLoop)
    files  <- c(files, rev(files[2:(length(files) - 1)]))
  if (loop > 0)
    vfilter <-
      glue::glue("loop={loop-1}:{length(files)+1}:{0}")
  else
    vfilter <- "null"
  av::av_encode_video(
    files,
    output = glue::glue("{dir}/../{basename(dir)}_animation.mp4"),
    vfilter = vfilter,
    framerate = framerate
  )
  pngDim <- png::readPNG(files[1], info=T) %>% attr("info") %>% extract2("dim")
  if (makegif) gifski::gifski(files, gif_file = glue::glue("{dir}/../{basename(dir)}_animation.gif"), delay = 1./framerate, width=pngDim[1]/gifShrinkage, height=pngDim[2]/gifShrinkage)
  return(glue::glue("{dir}/../{basename(dir)}_animation.mp4"))
}

#' Test function to show all rendering options
#'
#' Sphere or not sphere, ocean or not etc.
#'
#' @return Called for the side effect
#' @export
#'
#' @examples ## Make sure you have the right palette() in place
#'  testAllRenderOptions()
testAllRenderOptions <- function(br = br) {

  #br <- brick("D:/markusr/_FLUXCOM/resample/GPP_MTE_2003.nc.0720.0360.0012.nc") %>% readAll()

  df_ctrl <- cross_df(list(renderSphere = c(T,F),
                           renderOcean = c(T,F),
                           useRayShade4Sphere = c(T,F),
                           renderCountries = c(T,F),
                           eleRastOnly4NA = c(T,F)))
  walk(seq(nrow(df_ctrl)), ~ df_ctrl %$%
         brick2movie(br[[c(1,6)]] %>% flip("y") %>% reclassify(cbind(NA,NA)) %>% aggregate(2),
                     renderSphere = renderSphere[.x],
                     renderOcean = renderOcean[.x],
                     useRayShade4Sphere = useRayShade4Sphere[.x],
                     renderCountries = renderCountries[.x],
                     eleRastOnly4NA = eleRastOnly4NA[.x],
                     outPrefix = glue::glue("SphereWithOcean_{.x}"), gaussianSmoothSigma = 1.0,
                     sphereExtFac = 0.1, nRounds = 1, nSubSteps = 1,
                     thetaStartEnd = c(0, 360),  col4NA = "white", elevat4NA = -1,  renderVideo = T)
  )

}

#' Little demo function to create an animation of a wave
#'
#' @return called for the side effect of the mp4 created
#' @export
#'
#' @examples ## Make sure you have the right palette() in place
#' wavesAnim()
wavesAnim <- function() {
  dampedWave <- function(phase=0, xVec=seq(0,1,length.out = 100)*2*pi*10) {
    y <- cos(xVec+phase)*1/(xVec+2)
    y
  }
  rs <- map(seq(0, 4*pi, length.out = 40), ~cross_df(tibble(x=seq(-4*pi, 4*pi, length.out = 100), y=x)) %>% mutate(r=sqrt(x^2+y^2), wave=dampedWave(phase = -.x, xVec=r)) %>% reshape2::acast( x ~ y , ) %>% raster::raster()) %>% raster::stack()
  brick2movie(brick=rs, prefix="wave", renderOcean = F, outPrefix = "wave", thetaStartEnd = c(30,30), solid=F)


}

#' Decorates a png with title, caption and a legend from an image using magick package
#'
#' @param file File to decorate
#' @param title Title string
#' @param caption Caption string
#' @param size Size of Title in pix. Caption is 0.7xsize
#' @param legend_file Image file with legend will be put on the right
#' @param overwrite Should orig file be overwritten? Default: FALSE
#'
#' @return called for the side effect
#'
#'
#' @examples
decorate_png <- function(file=NULL, title="", caption="", size=60, legend_file=NULL, overwrite=F) {
  ## example usage to annotate all pngs for an animation:
  # files <- list.files("D:/anim/Global/SphereWithOcean4/", pattern = "png", full.names = T)
  # titles <- month.name %>% rep(each=6)
  # walk2(titles, files[1:72], ~decorate_png(.y, title=.x, caption="Gross primary production estimate MPI-BGC", o=T))

  require(magick)
  if (!overwrite) suffix <-  ".new.png" else suffix <- ""
  img  <- magick::image_read(file) %>%
    image_annotate(text = title, gravity = "North",size=size, location = "+0+20" ) %>%
    image_annotate(text = caption, gravity = "SouThEast", size = size*0.7, location = "+40+40" )
  if (file.exists(legend_file)) img %<>% image_composite(image_read(legend_file), gravity = "east")
  image_write(img, glue::glue("{file}{suffix}"))
}

#' Converts mp4 to gif
#'
#' @param mp4file
#' @param vfilter
#'
#' @return called for the side effect
#'
#'
#' @examples
mp4_to_gif <- function(mp4file, vfilter="scale=240:-1:flags=lanczos") { # 240 is the width, -1 means keep aspect
  av::av_encode_video(mp4file, glue::glue("{mp4file}.gif"), vfilter = vfilter)

}

#' Add lights that make the sphere shine
#'
#' @return
#'
#'
#' @examples
goodSphereLights <- function() {
  rgl::clear3d(type="lights")
  rgl::light3d(45,15, specular="white", viewp=T)
  rgl::light3d(45,-23, specular="black", viewp=T)
  rgl::light3d(45,60, specular="black", viewp=T)
}
