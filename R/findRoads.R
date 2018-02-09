#' Find TIGER roads within Area of Interest
#'
#' Function to locate all TIGER roads within an Area of Interest from the US Census Bureau
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#'\dontrun{
#' ep.roads = find_roads(state = 'CO', county = 'el paso', keep.boundary = TRUE, keep.basemap = TRUE)
#' plot(ep.roads$basemap)
#' plot(ep.roads$boundary, add = T, lwd = 5 )
#' plot(ep.roads$roads,    add = T )
#'}
#'
#' @export
#' @author
#' Mike Johnson

findRoads = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){

  ########## 1. Define AOI ##########
    do.call(file.remove, list(list.files(tempdir(), full.names = T)))
    items =  list()
    report = vector(mode = 'character')
    A = getAOI(state = state, county = county, clip_unit = clip_unit)
    AOI = getFiatBoundary(clip_unit = A)

    FIP = AOI$FIP
    if(is.null(FIP)){ FIP = AOI$map$FIP }
    if(is.null(FIP)){ FIP = AOI$shp$FIP }
    FIP = sprintf("%05d", as.numeric(FIP))

    if (any(keep.basemap == TRUE, !is.null(clip_unit))) {A = AOI$shp} else {A = AOI}
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Loading TIGER roads database...\n")

  ########## 2. Download Data ##########

    urls = vector()
    for(i in 1:length(FIP)){urls[i] = paste0("https://www2.census.gov/geo/tiger/TIGER2017/ROADS/tl_2017_", FIP[i], "_roads.zip")}
    if(length(urls) > 1){ verb = 'are'; noun = 'files'} else {verb = 'is'; noun = 'file'}
      message(paste("There", verb, length(urls), "TIGER", noun, "in this scene."))

    temp = tempfile()
    tempd = tempdir()

    for(i in 1:length(urls)){
        message(paste0("Downloading file ", i, " of ", length(urls)))
      download.file(url = urls[i], destfile = temp, quiet = TRUE )
      unzip(temp, exdir = tempd, overwrite = TRUE)
        message(paste0("Finished downloading file ", i, " of ", length(urls)))
    }

  ########## 3. Process Data ##########

    input.shp <- lapply(list.files(tempd, pattern = ".shp$", full.names = TRUE), rgdal::readOGR)

    for(j in 1:length(input.shp)){
      input.shp[[j]] = spTransform(input.shp[[j]], A@proj4string)
      input.shp[[j]] <- input.shp[[j]][A, ]
      #input.shp[[j]] <- suppressWarnings( gClip(input.shp[[j]], A) )
      message("Shapefile number ", j," Cropped.")
    }

    df <- do.call("rbind", input.shp)


    unlink(temp)
    unlink(tempd)

    items[['roads']] = df ; report = append(report, "Returned list includes: TIGER raods shapefile")
    if (keep.boundary) {items[['boundary']] = A; report = append(report, "boundary shapefile")}
    if (keep.basemap) {items[['basemap']] = AOI$bmap ; report = append(report, "basemap raster")}
    if(length(report) > 1) {report[length(report)] = paste("and", tail(report, n = 1))}
      message(paste(report, collapse = ", "))

    if(save){
        save.file(data = items,
                  state = state,
                  county = county,
                  clip_unit = clip_unit,
                  agency  = 'USCensus',
                  source  = "TIGER",
                  dataset = "roads",
                  other   = NULL )
    }

    return(items)
}


gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}


