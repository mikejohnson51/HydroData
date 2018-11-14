#' @title Find Dams in the US Army Core National Inventory (NID)
#' @description  \code{findNID} returns a \code{SpatialPointsDataFrame}
#' of all US Army Corps Dams for an Area of Interest from the National Inventory of Dams dataset.
#' The National Inventory of Dams (NID) is a congressionally authorized database documenting dams in the United States and its territories
#' This dataset is accessed through the \code{dams} R package and contains 61 attributes, perhaps most notably:
#' \itemize{
#' \item 'Dam_Name'   : \code{character}  Dam Name
#' \item 'NID_ID'   : \code{character}  Unique ID for the dam
#' \item 'River': \code{character}  Name of the river
#' \item 'Owner_Type'   : \code{character}  Type of Owner
#' \item 'Dam_Type'   : \code{character}   Type of Dam
#' \item 'Primary_Purpose'    : \code{numeric}    Primary Purpose served
#' \item 'Dam_Length'    : \code{numeric}    Length of the dam
#' \item 'Dam_Height'   : \code{numeric}  Height of the dam
#' \item 'Max_Discharge'   : \code{numeric}  Maximum Discharge
#' \item 'Max_Storage': \code{character}  Maximum Storage
#'  \item 'Normal_Storage': \code{character}  Normal Storage
#' }
#' @param AOI A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}
#' @param ids If TRUE, a vector of Dam IDs is added to retuned list (default = \code{FALSE})
#' @return a list() of minimum length 2: AOI and dams
#' @examples
#' \dontrun{
#' # Find all dams in Texas
#' tx.dams = getAOI(state = "TX") %>% findNID()
#'}
#' @author Mike Johnson
#' @export

findNID = function(AOI = NULL, ids = FALSE){

  if(!(class(AOI) %in% c("list","HydroData"))){AOI = list(AOI = AOI)}

  dams = dams::nid_cleaned
  dams = dams[!is.na(dams$Longitude),]
  dams = dams[!is.na(dams$Latitude),]

  sp = sf::st_as_sf(x = dams, coords = c('Longitude', 'Latitude'), crs = 4269) %>%  sf::as_Spatial()

  sp = sp[AOI$AOI, ]

  if (dim(sp)[1] == 0) { warning("0 dams found in AOI") } else {

  AOI[["dams"]] = sp

  report = paste(length(sp), "NID dam(s)")

  AOI = return.what(AOI, type = 'dams', report, vals = if(ids){"Dam_Name"}else{NULL})

  }

  return(AOI)

}



