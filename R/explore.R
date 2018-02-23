explore = function(input = NULL, save = FALSE) {
  layers = NULL

  fiat = NULL
  nwis = NULL
  ghcn = NULL
  nhd = NULL
  nid = NULL
  boundary = NULL
  wb = NULL
  roads = NULL
  snotel = NULL
  reservoirs = NULL

  WS = list()

  lc = NULL
  elv = NULL
  crops = list()

  input = unlist(input)

  for (i in seq_along(input)) {
    if (any(names(input[i]) == "boundary")) {
      boundary = input[i][[which(names(input[i]) == "boundary")]]
      layers = append(layers, "boundary")
    }

    if (any(names(input[i]) == "nwis")) {
      nwis = input[i][[which(names(input[i]) == "nwis")]]
      layers = append(layers, "USGS")
    }

    if (any(names(input[i]) == "ghcn")) {
      ghcn = input[i][[which(names(input[i]) == "ghcn")]]
      layers = append(layers, "GHCN")
    }

    if (any(names(input[i]) == "flowlines")) {
      nhd = input[i][[which(names(input[i]) == "flowlines")]]
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

    if (any(names(input[i]) == "roads")) {
      roads = input[i][[which(names(input[i]) == "roads")]]
      layers = append(layers, "Roads")
    }

    if (any(names(input[i]) == "snotel")) {
      snotel = input[i][[which(names(input[i]) == "snotel")]]
      layers = append(layers, "Snotel")
    }

    if (any(names(input[i]) == "reservoirs")) {
      reservoirs = input[i][[which(names(input[i]) == "reservoirs")]]
      layers = append(layers, "Reservoirs")
    }

    if (any(grepl(pattern = "huc", names(input[i])))) {
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
               toggleDisplay = TRUE) %>%
    addMeasure(
      position = "bottomleft",
      primaryLengthUnit = "feet",
      primaryAreaUnit = "sqmiles",
      activeColor = "#3D535D",
      completedColor = "#7D4479"
    )

  if (!is.null(boundary)) {
    m = addPolygons(
      m,
      data = boundary,
      fillColor = "transparent",
      group = 'Fiat',
      color = 'black'
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
  # NHD Flowlines                                                                #
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
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
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
    name <- wb$NAME
    size <- wb$SQMI
    feature <- wb$FEATURE

    label <- paste(
      paste("<strong>Name:</strong>", name),
      paste("<strong>Feature:</strong>", feature),
      paste("<strong>Area:</strong>", size, "SQMI"),
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
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  }

  #------------------------------------------------------------------------------#
  # TIGER                                                                       #
  #------------------------------------------------------------------------------#

  if (!is.null(roads)) {
    nam <- roads$FULLNAME

    label <- paste(paste("<strong>Name:</strong>", nam),
                   sep = "<br/>")

    m = addPolylines(
      m,
      data = roads,
      color = 'black',
      weight = 2,
      popup = label,
      group = "Roads",
      highlight = highlightOptions(
        weight = 10,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
    )
  }


  #------------------------------------------------------------------------------#
  # Snotel Stations                                                                #
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

    smallest = max(as.numeric(substring(names(WS),5,6)))
    largest = min(as.numeric(substring(names(WS),5,6)))
    ss = WS[which(as.numeric(substring(names(WS),5,6)) == smallest) ]
    ttt = unlist(ss[[1]][, grep("^HUC" , names(ss[[1]]))]@data)
    t = seq(2,smallest,2)

    name <- ss[[1]]$NAME

    for(i in seq_along(t)){
      name = cbind(name, substring(ttt, 1, t[i]))
    }

    colnames(name) <-  c("NAME", paste0("HUC", t))
    name = data.frame(name, row.names = NULL)

    ll = NULL

    for(i in seq_along(name)) {
      ll = paste(ll, paste("<strong>", names(name)[i], ":</strong>", name[,i]), sep = "<br/>" )
    }

    cols = unlist(WS[which(as.numeric(substring(names(WS),5,6)) == largest) ])
    cols = unlist(cols[[1]][, grep("^HUC" , names(ss[[1]]))]@data)
    df2 <- transform(cols,id=as.numeric(factor(cols)))
    df2$X_data = as.character(df2$X_data)

    test = NULL
    for(i in 1:dim(name)[1]){
      test =  append(test, df2[which(df2$X_data == name$HUC8[i]),2])
    }

    pal <- colorFactor(palette = 'RdBu',domain = test)

    m = addPolygons(
      m,
      data = ss[[1]],
      fillColor = ~pal(test),
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
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE
      )
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
  # Crops                                                                    #
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

  print(m)

  if (save) {
    htmlwidgets::saveWidget(m, file = "AOI.html")
  }

}
