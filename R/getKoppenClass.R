#' Gets and summarizes Koppen Climate Classification Data for AOI
#'
#' Generates a raster, spatial point and summary of Koppen Classifications
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or vector defining centroid and bounding box height and width in miles
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#
#' @examples
#' co.koppen = getKoppenClass(state = "CA")
#'
#' @return
#'
#' Raster
#' SpatialPointsDataFrame
#' Summary Dataframe
#'
#' @author
#' Mike Johnson
#' @export
#'
################################################################################################
##                                                                                            ##
##  Climate classification after Kottek et al. (2006), downscaling after Rubel et al. (2017)  ##
##                                                                                            ##
##  Kottek, M., J. Grieser, C. Beck, B. Rudolf, and F. Rubel, 2006: World Map of the          ##
##  Köppen-Geiger climate classification updated. Meteorol. Z., 15, 259-263.                  ##
##                                                                                            ##
##  Rubel, F., K. Brugger, K. Haslinger, and I. Auer, 2017: The climate of the                ##
##  European Alps: Shift of very high resolution Köppen-Geiger climate zones 1800-2100.       ##
##  Meteorol. Z., DOI 10.1127/metz/2016/0816.                                                 ##
##                                                                                            ##
##  (C) Climate Change & Infectious Diseases Group, Institute for Veterinary Public Health    ##
##      Vetmeduni Vienna, Austria                                                             ##
##                                                                                            ##
################################################################################################

getKoppenClass = function(state = NULL, county = NULL, clip_unit = NULL, keep.basemap = FALSE){

#------------------------------------------------------------------------------#
# Define Classification, description and color data                            #
#------------------------------------------------------------------------------#


   class       <- c( 'Af',  'Am',  'As',  'Aw',
                     'BSh', 'BSk', 'BWh', 'BWk',
                     'Cfa', 'Cfb', 'Cfc', 'Csa', 'Csb', 'Csc', 'Cwa', 'Cwb', 'Cwc',
                     'Dfa', 'Dfb', 'Dfc', 'Dfd', 'Dsa', 'Dsb', 'Dsc', 'Dsd', 'Dwa', 'Dwb', 'Dwc', 'Dwd',
                     'EF',  'ET',  'Ocean')

    description <- c(
                    # A Climates
                    "Tropical rainforest",
                    "Tropical monsoon",
                    "Tropical Savanna Dry",
                    "Tropical Savanna Wet",
                    # B Climates
                    "Hot semi-arid steppe",
                    "Cold semi-arid steppe",
                    "Hot desert",
                    "Cold desert",
                    # C Climates
                    "Humid subtropical",
                    "Temperate oceanic",
                    "Subpolar oceanic",
                    "Hot-summer Mediterranean",
                    "Warm-summer Mediterranean",
                    "Cool-summer Mediterranean",
                    "Monsoon-influenced humid subtropical",
                    "Subtropical highland or temperate oceanic with dry winters",
                    "Cold subtropical highland or subpolar oceanic with dry winters",
                    # D Climates
                    "Hot-summer humid continental",
                    "Warm-summer humid continental",
                    "Subarctic",
                    "Extremely cold subarctic",
                    "Hot, dry-summer continental",
                    "Warm, dry-summer continental",
                    "Dry-summer subarctic",
                    "Extremely cold, dry-summer subarctic",
                    "Monsoon-influenced hot-summer humid continenta",
                    "Monsoon-influenced warm-summer humid continental",
                    "Monsoon-influenced subarctic",
                    "Monsoon-influenced extremely cold subarctic",
                    # E Climates
                    "Polar Tundra",
                    "Polar Ice Cap",
                    "Ocean")

    climates <-  data.frame (Abb = class, description = description, stringsAsFactors = FALSE)

    climate.colors <- c("#960000", "#FF0000", "#FF6E6E", "#FFCCCC", "#CC8D14",
                        "#CCAA54", "#FFCC00", "#FFFF64", "#007800", "#005000",
                        "#003200", "#96FF00", "#00D700", "#00AA00", "#BEBE00",
                        "#8C8C00", "#5A5A00", "#550055", "#820082", "#C800C8",
                        "#FF6EFF", "#646464", "#8C8C8C", "#BEBEBE", "#E6E6E6",
                        "#6E28B4", "#B464FA", "#C89BFA", "#C8C8FF", "#6496FF",
                        "#64FFFF", "#F5FFFF")

#------------------------------------------------------------------------------#
# Load Raster Data and Define AOI                                              #
#------------------------------------------------------------------------------#
    kopRas <- readRDS ("data/koppen_raster.rds")

    A      <- getAOI (state = state, county = county, clip_unit = clip_unit) %>%
              spTransform (kopRas@crs)

#------------------------------------------------------------------------------#
# Format and Manipulate Data for AOI                                           #
#------------------------------------------------------------------------------#

    crop.r    <-  crop(kopRas, A, snap = 'out' )
    ras.poly  <- rasterToPolygons(crop.r)
    ras.poly  <- ras.poly[!(ras.poly$layer == 32), ]
    clip.ras.poly <- ras.poly[A, ]
    r             <- rasterize(clip.ras.poly, crop.r, "layer")

    r@legend@colortable <-  climate.colors

    z <- rasterToPoints(r, spatial=T)
    z <- spTransform(z, CRS=projection(r))
    z <- as.data.frame(z);
    z <-  subset(z, z[1]!=32);

    names(z)=c('KG', 'lat', 'lon')
    pts <- data.frame(lat= format(z[2], digits=4) , lon=format(z[3], digits=7), KG=format(z[1], digits=3))
      pts$lat = as.numeric (pts$lat)
      pts$lon = as.numeric (pts$lon)
      pts$KG  = as.numeric (pts$KG)

    for(i in 1:dim(pts)[1]){ pts$class[i] = climates$Abb[as.numeric(pts$KG[i])] }
    for(i in 1:dim(pts)[1]){ pts$description[i] = climates$description[as.numeric(pts$KG[i])] }

    sp = SpatialPointsDataFrame(cbind(pts$lat, pts$lon), data = pts)
    sp@proj4string = r@crs

    freq = data.frame(freq(r, useNA= 'no'))
    freq <- freq[!(freq$value==32),]

    df <- data.frame(KG = unique(pts$KG), class = unique(pts$class), description = unique(pts$description))
      for(i in 1:dim(df)[1]){ df$freq[i] = round(freq$count[which(freq$value == df$KG[i])] / sum(freq$count),2) }
      df = df[!(df$freq == 0),]
      df = df[order(df$freq, decreasing = TRUE ),]

#------------------------------------------------------------------------------#
# Define Data to return                                                        #
#------------------------------------------------------------------------------#
    # raster 'r' is raster of the Koppen Classification
    # SpatialPointsDataFrame sp is a point feature class
    # Summary is a data frame reporting all classification, descriptions and frequecy

    return(list(raster = r, sp = sp, summary = df))
}



