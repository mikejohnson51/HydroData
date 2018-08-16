#' Explore Spatial HydroData Objects
#'
#' @description
#' \code{explore} can be used to explore spatial HydroData objects obtained from
#' Namely:
#'  findGHCN
#'  findNHD
#'  findReservoir
#'  findtiger
#'  findSnotel
#'  findUSGS
#'  findWaterbodies
#'  findWS
#'  findap
#'  getCLD
#'  getKoppen
#'  getNED
#'  getNLCD
#'  getWaterUse
#'
#' @param input a single, or list, of HydroData objects
#' @param save  (logical) should the leaflet HTML be saved to disk?
#'
#' @family HydroData 'viz' functions
#'
#' @return leaflet map object
#'
#' @examples
#' \dontrun{
#' # Find USGS station near UCSB
#'
#' ucsb.nwis = findUSGS(clip_unit = list("UCSB", 10, 10))
#' explore(ucsb.nwis)
#' }
#'
#' @export
#' @author
#' Mike Johnson
#'


explore = function(input = NULL, save = FALSE) {

  layers = NULL

# Boundaries
 fiat = NULL
 boundary = NULL

# Vector
  nwis = NULL
  ghcn = NULL
  nhd = NULL
  nid = NULL
  wb = NULL
  tiger = NULL
  snotel = NULL
  reservoirs = NULL
  WS = list()
  ap = NULL

# Raster
  lc = NULL
  elv = NULL
  crops = list()

input = unlist(input)

for (i in seq_along(input)) {

  if (any(names(input[i]) == "ap")) {
    ap = input[i][[which(names(input[i]) == "ap")]]
    layers = append(layers, "ap")
  }

  if (any(names(input[i]) == "fiat")) {
    fiat = input[i][[which(names(input[i]) == "fiat")]]
    layers = append(layers, "fiat")
  }

    if (any(names(input[i]) == "boundary")) {
      boundary = input[i][[which(names(input[i]) == "boundary")]]
      layers = append(layers, "AOI")
    }

    if (any(names(input[i]) == "nwis")) {
      nwis = input[i][[which(names(input[i]) == "nwis")]]
      layers = append(layers, "USGS")
    }

    if (any(names(input[i]) == "ghcn")) {
      ghcn = input[i][[which(names(input[i]) == "ghcn")]]
      layers = append(layers, "GHCN")
    }

    if (any(names(input[i]) == "nhd")) {
      nhd = input[i][[which(names(input[i]) == "nhd")]]
      layers = append(layers, "NHD")
    }

    if (any(names(input[i]) == "dams")) {
      nid = input[i][[which(names(input[i]) == "dams")]]
      layers = append(layers, "NID")
    }

    if (any(names(input[i]) == "waterbodies")) {
      wb = input[i][[which(names(input[i]) == "waterbodies")]]
      layers = append(layers, "Water bodies")
    }

    if (any(names(input[i]) == "tiger")) {
      tiger = input[i][[which(names(input[i]) == "tiger")]]
      layers = append(layers, "tiger")
    }

    if (any(names(input[i]) == "snotel")) {
      snotel = input[i][[which(names(input[i]) == "snotel")]]
      layers = append(layers, "Snotel")
    }

    if (any(names(input[i]) == "reservoirs")) {
      reservoirs = input[i][[which(names(input[i]) == "reservoirs")]]
      layers = append(layers, "Reservoirs")
    }

    if (any(grepl(pattern = "^huc", names(input[i])))) {
      title = names(input[i])
      WS[[title]] = input[i][[grepl(pattern = "huc", names(input[i]))]]
      layers = append(layers, 'WBD')
    }

#------------------------------------------------------------------------------#
# rasters                                                                      #
#------------------------------------------------------------------------------#

    if (any(grepl(pattern = "lc", names(input[i])))) {
      lc = input[i][[grepl(pattern = "lc", names(input[i]))]]
      layers = append(layers, "Land Cover")
    }

    if (any(grepl(pattern = "elev", names(input[i])))) {
      elv = input[i][[grepl(pattern = "elev", names(input[i]))]]
      layers = append(layers, "Elevation")
    }

    if (any(grepl(pattern = "CLD", names(input[i])))) {
      title = names(input[i])
      crops[[title]] = input[i][[grepl(pattern = "CLD", names(input[i]))]]
      layers = append(layers, title)
    }

}

#------------------------------------------------------------------------------#
# Basemap                                                                      #
#------------------------------------------------------------------------------#


  m = leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron, group = "Base") %>%
    addProviderTiles(providers$Esri.WorldImagery, group = "Imagery") %>%
    addProviderTiles(providers$Esri.NatGeoWorldMap , group = "Terrain") %>%

    addScaleBar("bottomleft") %>%
    addMiniMap(tiles = providers$OpenStreetMap.BlackAndWhite,
               toggleDisplay = TRUE,
               minimized = TRUE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "red",
      completedColor = "green"
    )

#------------------------------------------------------------------------------#
# Boundaries                                                                   #
#------------------------------------------------------------------------------#

if (!is.null(boundary)) {
    m = addPolygons(m,
                    data = boundary,
                    fillColor = "gray",
                    group = 'AOI',
                    color = 'black',
                    stroke = TRUE,
                    weight = 1,
                    opacity = .9,
                    smoothFactor = 0.7
    )
  }

if (!is.null(fiat)) {
  m = addPolygons(m,
                  data = fiat,
                  fillColor = "transparent",
                  group = 'fiat',
                  color = 'black',
                  stroke = TRUE,
                  weight = 3,
                  opacity = 1,
                  smoothFactor = 0.7
  )
}

#------------------------------------------------------------------------------#
# USGS Stations                                                                #
#------------------------------------------------------------------------------#

  if (!is.null(nwis)) {
    data = nwis

    usgsIcon = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",
      iconWidth = 40,
      iconHeight = 20,
      iconAnchorX = 20,
      iconAnchorY = 10
    )

    comid <- data$feature_id
    num <- data$site_no # site number
    nam <- data$site_name # local site name
    url <-
      sprintf("https://waterdata.usgs.gov/nwis/inventory/?site_no=%s",
              num)
    url_call = paste0('<a href=', url, '>', num, "</a>")

    pop <- paste(
      paste("<strong>Site Number:</strong>", url_call),
      paste("<strong>NHD COMID:</strong>", comid),
      paste("<strong>Site Name:</strong>", nam),
      sep = "<br/>"
    )

    m = addMarkers(
      m ,
      data = data,
      lng = data$lon_reachCent,
      lat = data$lat_reachCent,
      icon = usgsIcon,
      popup = pop,
      group = "USGS",
      clusterOptions = markerClusterOptions()
    )
  }

#------------------------------------------------------------------------------#
# GHCN Stations                                                                #
#------------------------------------------------------------------------------#

  if (!is.null(ghcn)) {
    data = ghcn

    noaaIcon = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/3/3a/NOAA.png",
      iconWidth = 20,
      iconHeight = 20,
      iconAnchorX = 20,
      iconAnchorY = 10
    )

    ID <- data$ID
    nam <- data$NAME
    para <- data$PARAMETER
    year.f = data$START_YEAR
    year.l = data$END_YEAR

    pop <- paste(
      paste("<strong>Site ID:</strong>", ID),
      paste("<strong>Site Name:</strong>", nam),
      paste("<strong>Parameter:</strong>", para),
      paste("<strong>Year Range:</strong>", year.f, " - ", year.l),
      sep = "<br/>"
    )

    m = addMarkers(
      m ,
      data = data,
      lng = data$LON,
      lat = data$LAT,
      icon = noaaIcon,
      popup = pop,
      group = "GHCN",
      clusterOptions = markerClusterOptions()
    )

  }

#------------------------------------------------------------------------------#
# NHD nhd                                                                #
#------------------------------------------------------------------------------#

  if (!is.null(nhd)) {
    COMID <- nhd$comid
    nam <- nhd$gnis_name

    label <- paste(
      paste("<strong>COMID:</strong>", COMID),
      paste("<strong>GNIS Name:</strong>", nam),
      sep = "<br/>"
    )

    m = addPolylines(
      m,
      data = nhd,
      color = 'blue',
      weight = nhd$streamorde,
      popup = label,
      group = "NHD",

      highlight = highlightOptions(
        weight = 10,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE)
      )
  }

#------------------------------------------------------------------------------#
# NID Dams                                                                     #
#------------------------------------------------------------------------------#

  if (!is.null(nid)) {
    data = nid

    usaceIcon = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/3/35/United_States_Army_Corps_of_Engineers_logo.svg/500px-United_States_Army_Corps_of_Engineers_logo.svg.png",
      iconWidth = 20,
      iconHeight = 20,
      iconAnchorX = 20,
      iconAnchorY = 10
    )

    ID <- data$NID_ID
    nam <- data$Dam_Name
    river <- data$River
    owner = data$Owner_Type
    type = data$Dam_Type
    prim_purpose = data$Primary_Purpose
    Max_storage = data$Max_Storage

    pop <- paste(
      paste("<strong>Site ID:</strong>", ID),
      paste("<strong>Site Name:</strong>", nam),
      paste("<strong>River:</strong>", river),
      paste("<strong>Owner:</strong>", owner),
      paste("<strong>Type:</strong>", type),
      paste("<strong>Primary Purpose:</strong>", prim_purpose),
      paste("<strong>Max Storage:</strong>", Max_storage, " AF"),
      sep = "<br/>"
    )

    m = addMarkers(
      m ,
      data = data,
      lng = data$Longitude,
      lat = data$Latitude,
      icon = usaceIcon,
      popup = pop,
      group = "NID",
      clusterOptions = markerClusterOptions()
    )
  }


#------------------------------------------------------------------------------#
# Water bodies                                                                 #
#------------------------------------------------------------------------------#

  if (!is.null(wb)) {
    name <- wb$gnis_name
    size <- wb$areasqkm
    feature <- wb$ftype


    label <- paste(
      paste("<strong>Name:</strong>", name),
      paste("<strong>Feature:</strong>", feature),
      paste("<strong>Area:</strong>", size, "SQKM"),
      sep = "<br/>"
    )

    m = addPolygons(
      m,
      data = wb,
      fillColor =  'lightblue',
      stroke = TRUE,
      weight = 1,
      opacity = 1,
      fillOpacity = 0.7,
      smoothFactor = 0.7,
      popup = label,
      group = "Water bodies",
      highlight = highlightOptions(
        weight = 5,
        color = "darkred",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  }

#------------------------------------------------------------------------------#
# TIGER                                                                       #
#------------------------------------------------------------------------------#

  if (!is.null(tiger)) {
    nam <- tiger$FULLNAME

    label <- paste(paste("<strong>Name:</strong>", nam),
                   sep = "<br/>")

    m = addPolylines(
      m,
      data = tiger,
      color = 'black',
      weight = 2,
      popup = label,
      group = "tiger",
      highlight = highlightOptions(
        weight = 10,
        color = "#666",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  }

#------------------------------------------------------------------------------#
# Snotel Stations                                                              #
#------------------------------------------------------------------------------#

  if (!is.null(snotel)) {
    snotelIcon = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/58/Snowflake.svg/200px-Snowflake.svg.png",
      iconWidth = 20,
      iconHeight = 20,
      iconAnchorX = 20,
      iconAnchorY = 10
    )

    ID <- snotel$ID
    nam <- snotel$NAME
    elev <- snotel$ELEV
    year.f = snotel$START.DATE


    pop <- paste(
      paste("<strong>Site ID:</strong>", ID),
      paste("<strong>Site Name:</strong>", nam),
      paste("<strong>Elevation:</strong>", elev, " feet"),
      paste("<strong>First record:</strong>", year.f),
      sep = "<br/>"
    )

    m = addMarkers(
      m ,
      data = snotel,
      lng = snotel$LONG,
      lat = snotel$LAT,
      icon = snotelIcon,
      popup = pop,
      group = "Snotel",
      clusterOptions = markerClusterOptions()
    )
  }

#------------------------------------------------------------------------------#
# Reservoirs                                                                   #
#------------------------------------------------------------------------------#

  if (!is.null(reservoirs)) {
    resIcon = makeIcon(
      iconUrl = "https://upload.wikimedia.org/wikipedia/commons/thumb/5/57/Icon_river_reservoir.svg/200px-Icon_river_reservoir.svg.png",
      iconWidth = 20,
      iconHeight = 20,
      iconAnchorX = 20,
      iconAnchorY = 10
    )

    ID <- reservoirs$RES_ID
    dam.nam <- reservoirs$DAMNAME
    res.nam  <- reservoirs$RESNAME
    riv  <- reservoirs$RIVER
    drain = reservoirs$DRAINAREA

    pop <- paste(
      paste("<strong>Reservoir ID:</strong>", ID),
      paste("<strong>Reservoir Name:</strong>", res.nam),
      paste("<strong>Dam Name:</strong>", dam.nam),
      paste("<strong>River:</strong>", riv),
      paste("<strong>Drainage Area:</strong>", drain, " square miles"),
      sep = "<br/>"
    )

    m = addMarkers(
      m ,
      data = reservoirs,
      lng = reservoirs$LONDD,
      lat = reservoirs$LATDD,
      icon = resIcon,
      popup = pop,
      group = "Reservoirs",
      clusterOptions = markerClusterOptions()
    )

  }

#------------------------------------------------------------------------------#
# Watersheds                                                                   #
#------------------------------------------------------------------------------#

  if (length(WS) > 0) {



    smallest = max(as.numeric(substring(names(WS),4,5)))
    largest = min(as.numeric(substring(names(WS),4,5)))
    ss = WS[which(as.numeric(substring(names(WS),4,5)) == smallest) ]
    ttt = unlist(ss[[1]][, grep("^huc" , names(ss[[1]]))]@data)
    t = seq(2,smallest,2)

    names(ss[[1]]) = tolower(names(ss[[1]]))

    name <- ss[[1]]$name

    for(i in seq_along(t)){
      name = cbind(name, substring(ttt, 1, t[i]))
    }

    name = as.data.frame(name)

    colnames(name) <-  c("NAME", paste0("HUC", t))
    name = data.frame(name, row.names = NULL)

    ll = paste("<strong>", "Displaying ", "HUC", smallest, "'s", "</strong>", "<br/>", sep = "")

    for(i in seq_along(name)) {
      ll = paste(ll, paste("<strong>", names(name)[i], ":</strong>", name[,i]), sep = "<br/>" )
    }

    m = addPolygons(
      m,
      data = ss[[1]],
      fillColor = "lightblue",
      color = "black",
      stroke = TRUE,
      weight = 4,
      opacity = 1,
      fillOpacity = .5,
      smoothFactor = 0.7,
      popup = ll,
      group = "WBD",
      highlight = highlightOptions(
        weight = 5,
        color = "darkred",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  }

#------------------------------------------------------------------------------#
# ap                                                                     #
#------------------------------------------------------------------------------#

if (!is.null(ap)) {

  data = ap

  apIcon = makeIcon(
    iconUrl = "https://upload.wikimedia.org/wikipedia/commons/a/a4/Map_symbol_airport_02.png",
    iconWidth = 40,
    iconHeight = 20,
    iconAnchorX = 20,
    iconAnchorY = 10
  )

  name <- data$name
  digit4 <- data$ICAO # site number
  city <- data$city # local site name

  url = paste0("https://www.wunderground.com/history/airport/", digit4, "/",  gsub("-","/", Sys.Date()), "/DailyHistory.html")

  url_call = paste0('<a href=', url, '>', name, "</a>")

  pop <- paste(
    paste("<strong>Name:</strong>", url_call),
    paste("<strong>City:</strong>", city),
    paste("<strong>4 Digit Identifier:</strong>", digit4),
    sep = "<br/>"
  )

  m = addMarkers(
    m,
    data = data,
    icon = apIcon,
    popup = pop,
    group = "ap"
  )
}

#------------------------------------------------------------------------------#
# landcover                                                                    #
#------------------------------------------------------------------------------#

  if (!is.null(lc)) {
    lc.interest = c(0, 11)

    values(lc)[(values(lc) %in% lc.interest)] = NA

    pal = colorNumeric(col_lc$color, col_lc$code , na.color = "transparent")

    m = m %>% addRasterImage(lc,
                             colors = pal,
                             opacity = .8,
                             group = "Land Cover")
  }

#------------------------------------------------------------------------------#
# Elevation                                                                    #
#------------------------------------------------------------------------------#

  if (!is.null(elv)) {
    elev.interest = 0

    values(elv)[(values(elv) %in% elev.interest)] = NA

    pal = colorNumeric(col_elev, values(elv) , na.color = "transparent")

    m = m %>% addRasterImage(elv,
                             colors = pal,
                             opacity = .7,
                             group = "Elevation")
  }

#------------------------------------------------------------------------------#
# Crops                                                                        #
#------------------------------------------------------------------------------#

  if (length(crops) > 0) {
    for (i in seq_along(crops)) {
      values(crops[[i]])[(values(crops[[i]]) == 0)] = NA
      pal = colorNumeric(col_crops$color, col_crops$values , na.color = "transparent")
      m = m %>% addRasterImage(
        crops[[i]],
        colors = pal,
        opacity = 1,
        group = names(crops[[i]])
      )

    }
  }

#------------------------------------------------------------------------------#
# Finalize                                                                    #
#------------------------------------------------------------------------------#

  m = m %>% addLayersControl(
    baseGroups = c("Base", "Imagery", "Terrain"),
    overlayGroups = layers,
    options = layersControlOptions(collapsed = T)
  )

  if (save) {
    htmlwidgets::saveWidget(m, file = "AOI.html")
  }

  print(m)
  return(list(AOI = input, map = m))
}
