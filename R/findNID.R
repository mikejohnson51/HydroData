#' Locate all USACE NID Dams within Area of Interest
#'
#' Function to locate all US Army Corps dams within an Area of Interest from the National Inventory of Dams
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param keep.boundary logical. If TRUE, the AOI shapefile will be returned with gage data in a list
#' @param keep.basemap logical. If TRUE, the google basemap will be returned with gage data in a list
#' @param save logical. If TRUE, all data is written to a HydroData folder in the working directory
#'
#' @examples
#' Find all dams in Texas
#'
#' tx.dams = find_nid(state = "TX", keep.boundary = TRUE, keep.basemap = TRUE, save= TRUE)
#' plot(tx.dams$basmap)
#' plot(tx.dams$boundary, add = TRUE)
#' plot(tx.dams$dams, add = TRUE)
#'
#' @export
#' @author
#' Mike Johnson

find_nid = function(state = NULL, county = NULL, clip_unit = NULL, keep.boundary = FALSE, keep.basemap = FALSE, save = FALSE){

  items =  list()
  report = vector(mode = 'character')
  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit, get.basemap = keep.basemap)
  if (any(keep.basemap == TRUE, !is.null(clip_unit))) {A = AOI$shp} else {A = AOI}
    message("AOI defined as the ", nameAOI(state = state, county = county, clip_unit = clip_unit), ". Shapefile determined. Now loading loading NID database...")

  data(nid_cleaned)
  dams = nid_cleaned %>% filter(!State  %in% c("AK", "HI", "GU", "PR")) %>% drop_na(Longitude, Latitude)
  rm(nid_cleaned)
  sp = SpatialPointsDataFrame(cbind(dams$Longitude, dams$Latitude), data = dams)
    message("All dams in CONUS loaded: ", formatC(dim(sp)[1], format="d", big.mark=","), " dams in total")

  sp@proj4string = A@proj4string
  sp = sp[A, ]
    message(formatC(as.numeric(length(sp)), format="d", big.mark=","), " NID dams found in ", nameAOI(state = state, county = county, clip_unit = clip_unit))

  items[['dams']] = sp ; report = append(report, "Returned list includes: NID dams shapefile")
  if (keep.boundary) {items[['boundary']] = AOI$shp; report = append(report, "boundary shapefile")}
  if (keep.basemap) {items[['basemap']] = AOI$bmap ; report = append(report, "basemap raster")}

  if(length(report) > 1) {report[length(report)] = paste("and",  tail(report, n = 1))}
    message(paste(report, collapse = ", "))

    if(save){
      save.file(data = items,
                state = state,
                county = county,
                clip_unit = clip_unit,
                agency  = 'USACE',
                source  = "NID",
                dataset = "dams",
                other   = NULL )
    }


  return(items)
}


