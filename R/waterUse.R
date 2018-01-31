#' Get Multi-Year USGS Water Use data
#'
#' Function to download national water use data for an area of interest from 2000, 2005, 2010
#'
#' @param state a character string. Can be full name or state abbriviation
#' @param county a character string. Can be full name or state abbriviation
#' @param clip_unit can be provided as a shapefile or as a vector defineing centroid and bounding box diminsion
#' @param year define year to download. Any combination of 2000, 2005 and 2010 accepted
#'
#' @examples
#' wu10 = waterUse(state = "CO", year = 2010)
#'
#' wu.co = waterUse(statte = c("CA", "UT", "AZ", "NV"), year = c(2000, 2005, 2010))
#'
#'@return list of SpatialPolygons and Data
#'
#' @author
#' Mike Johnson

waterUse = function(state = NULL, county = NULL, clip_unit = NULL, year = 2010){

  if(!any(year %in% c(2000, 2005, 2010))){stop('USGS water use data only available for years: 2000, 2005, 2010 in HydroData Package')}
  AOI = define_AOI(state = state, county = county, clip_unit = clip_unit)
  if(is.null(clip_unit)){AOI = AOI} else {AOI = AOI$map}
  name.AOI = nameAOI(state = state, county = county, clip_unit = clip_unit)
  message("AOI defined as the ", name.AOI, ". Shapefile determined. Now (down)loading USGS water use data...")

  if(any(year == 2000)){
    message("Downloading USGS water use data for 2000")
    waterUse = fread(paste0("https://water.usgs.gov/watuse/data/2000/usco2000.txt"), sep = "\t", fill = TRUE, showProgress = FALSE)
    names(waterUse) = gsub("-", ".", names(waterUse))
    waterUse = waterUse %>%
      select(c(which(names(waterUse) %in% wu_index$ids))) %>%
      filter(waterUse$FIPS %in% AOI$FIP)
    county = vector()
    for(i in 1:dim(waterUse)[1]){county[i] =  AOI$NAME[which(waterUse$FIPS[i] == AOI$FIP)]}
    waterUse = data.frame(append(waterUse, list(COUNTY=county), after=match("STATEFIPS", names(waterUse))))

    waterUse00 = waterUse %>% gather(ID, year_2000, 6:(dim(waterUse)[2]))
    for(i in 1:dim(waterUse00)[1]){waterUse00[i,"DESCRIPTION"] = wu_index[which(waterUse00$ID[i] == wu_index$ids),2]}
    rm(waterUse)
  }
  if(any(year == 2005)){
    message("Downloading USGS water use data for 2005")
    waterUse = fread("https://water.usgs.gov/watuse/data/2005/usco2005.txt", sep = "\t", fill = TRUE, showProgress = FALSE)
    names(waterUse) = gsub("-", ".", names(waterUse))
    waterUse = waterUse %>%
      select(c(which(names(waterUse) %in% wu_index$ids))) %>%
      filter(waterUse$FIPS %in% AOI$FIP)
    county = vector()
    for(i in 1:dim(waterUse)[1]){county[i] =  AOI$NAME[which(waterUse$FIPS[i] == AOI$FIP)]}
    waterUse = data.frame(append(waterUse, list(COUNTY=county), after=match("STATEFIPS", names(waterUse))))

    waterUse05 = waterUse %>% gather(ID, year_2005, 6:(dim(waterUse)[2]))
    for(i in 1:dim(waterUse05)[1]){waterUse05[i,"DESCRIPTION"] = wu_index[which(waterUse05$ID[i] == wu_index$ids),2]}
    rm(waterUse)
  }

  if(any(year == 2010)){
    message("Downloading USGS water use data for 2010")
    waterUse = fread("https://water.usgs.gov/watuse/data/2010/usco2010.txt", sep = "\t", fill = TRUE, showProgress = FALSE)
    names(waterUse) = gsub("-", ".", names(waterUse))
    waterUse = waterUse %>%
      select(c(which(names(waterUse) %in% wu_index$ids))) %>%
      filter(waterUse$FIPS %in% AOI$FIP) %>%
      mutate(COUNTY = gsub(" County", "", COUNTY))

    waterUse10 = waterUse %>% gather(ID, year_2010, 6:(dim(waterUse)[2])) %>% mutate(year_2010 = as.numeric(year_2010))
    for(i in 1:dim(waterUse10)[1]){waterUse10[i,"DESCRIPTION"] = wu_index[which(waterUse10$ID[i] == wu_index$ids),2]}
    rm(waterUse)
  }

  message("All data downloaded. Now subsetting...")
  if(all(c(exists("waterUse00"), !exists("waterUse05"), !exists("waterUse10")))){zz = waterUse00}
  if(all(c(!exists("waterUse00"), exists("waterUse05"), !exists("waterUse10")))){zz = waterUse05}
  if(all(c(!exists("waterUse00"), !exists ("waterUse05"), exists("waterUse10")))){zz = waterUse10}
  if(sum(c(exists("waterUse00"), exists ("waterUse05"), exists("waterUse10"))) > 1){
    if(!exists("waterUse00")){ zz = merge(waterUse05, waterUse10, all = TRUE)
    }else if(!exists("waterUse05")){ zz = merge(waterUse00, waterUse10, all = TRUE)
    }else if(!exists("waterUse10")){ zz = merge(waterUse00, waterUse05, all = TRUE)
    }else { zz = merge(waterUse00, waterUse05, all = TRUE)
            zz = merge(zz, waterUse10, all= TRUE)}
  }

  message('Complete. Annual data downlaoded for ', name.AOI, "years ", paste0(year, collapse = ", "), ".")
  return(list(data = zz, map = AOI))
}

#' Plot a Water use Object
#'
#' Function to visualize waterUse data by county in both static and interactive formats.
#'
#' @param waterUse.obj a water use object from the waterUse() function
#' @param variable the water use variable to plot. Can be indexed from wu.variable$...
#' @param plot.type can be either a spplot static image of a leaflet derived interactive image. Options include "static" or "interactive"
#' @param year the year of data to plot

#'
#' @examples
#' plotWu(wu10, variable = wu.variable$`Public Supply, surface-water withdrawals, fresh, in Mgal/d`, plot.type = 'static', year = 2010)
#'
#' plotWu(wu10, variable = wu.variable$`Public Supply, surface-water withdrawals, fresh, in Mgal/d`, plot.type = 'interactive', year = 2010)
#'
#' @author
#' Mike Johnson

plotWS = function(waterUse.obj = NULL, variable = NULL, plot.type = 'static', year = NULL){

  waterUse.sp <- merge(waterUse.obj$map, waterUse.obj$data[waterUse.obj$data$DESCRIPTION %in% variable,], by.x = "NAME", by.y = "COUNTY")

  waterUse.sp@data
  if(length(year)>1){stop("Only 1 year can be ploted at a time.")} else { name =paste0("year_", year)}

  breaks_qt <- classInt::classIntervals(waterUse.sp$year_2010,n = 5)
    br <- breaks_qt$brks
    offs <- 0.0000001
    br[1] <- br[1] - offs
    br[length(br)] <- br[length(br)] + offs

  if(plot.type == 'static'){
    sp::spplot(waterUse.sp, name, col.regions = RColorBrewer::brewer.pal(5, "YlOrRd"), at = br, col = 'grey',  lty = 3, main = waterUse.sp@data$DESCRIPTION[1])
  }else if(plot.type == 'interactive'){
      pal_fun <- colorQuantile("YlOrRd", NULL, n = 5)

      p_popup <- paste(sep = "<br/>",
                       paste0("<strong>County Name </strong>", waterUse.sp$NAME),
                       paste0("<strong>", waterUse.sp@data$DESCRIPTION[1],":</strong> ", round(waterUse.sp@data[,name],0)))

      leaflet(spTransform(waterUse.sp, CRS("+init=epsg:4326"))) %>%
        addTiles() %>%
        addPolygons(
          stroke = TRUE,
          color = 'black',
          weight = 1,
          fillColor = ~pal_fun(waterUse.sp@data[,name]),
          fillOpacity = 0.8, smoothFactor = 0.5,
          popup = p_popup) %>%
        addLegend("bottomright",
                  colors =   RColorBrewer::brewer.pal(5, "YlOrRd"),
                  labels = paste0("up to ", as.character(round(breaks_qt$brks[-1]))),
                  title = paste0(waterUse.sp@data$DESCRIPTION[1], " (", year,")"))
    }
  }



wu_index = data.frame(

  ids = c("STATE",      "STATEFIPS",  "COUNTY",     "COUNTYFIPS", "FIPS",     "TP.TotPop",  "PS.GWPop",   "PS.SWPop",   "PS.TOPop",   "PS.WGWFr",
          "PS.WGWSa",   "PS.WGWTo",   "PS.WSWFr",   "PS.WSWSa",   "PS.WSWTo",   "PS.WFrTo",   "PS.WSaTo",   "PS.Wtotl",   "DO.SSPop",   "DO.WGWFr",   "DO.WSWFr",
          "DO.WFrTo",   "DO.SSPCp",   "DO.PSDel",   "DO.PSPCp",   "DO.TOTAL",   "IN.WGWFr",   "IN.WGWSa",   "IN.WGWTo",   "IN.WSWFr",   "IN.WSWSa",   "IN.WSWTo",
          "IN.WFrTo",   "IN.WSaTo",   "IN.Wtotl",   "IR.WGWFr",   "IR.WSWFr",   "IR.WFrTo",   "IR.IrSpr",   "IR.IrMic",   "IR.IrSur",   "IR.IrTot",   "IC.WGWFr",
          "IC.WSWFr",   "IC.WFrTo",   "IC.IrSpr",   "IC.IrMic",   "IC.IrSur",   "IC.IrTot",   "IG.WGWFr",   "IG.WSWFr",   "IG.WFrTo",   "IG.IrSpr",   "IG.IrMic",
          "IG.IrSur",   "IG.IrTot",   "LI.WGWFr",   "LI.WSWFr",   "LI.WFrTo",   "AQ.WGWFr",   "AQ.WGWSa",   "AQ.WGWTo",   "AQ.WSWFr",   "AQ.WSWSa",   "AQ.WSWTo",
          "AQ.WFrTo",   "AQ.WSaTo",   "AQ.WTotl",   "MI.WGWFr",   "MI.WGWSa",   "MI.WGWTo",   "MI.WSWFr",   "MI.WSWSa",   "MI.WSWTo",   "MI.WFrTo",   "MI.WSaTo",
          "MI.Wtotl",   "PT.WGWFr",   "PT.WGWSa",   "PT.WGWTo",   "PT.WSWFr",   "PT.WSWSa",   "PT.WSWTo",   "PT.WFrTo",   "PT.WSaTo",   "PT.Wtotl",  "PT.Power",
          "PO.WGWFr",   "PO.WGWSa",   "PO.WGWTo",   "PO.WSWFr",   "PO.WSWSa",   "PO.WSWTo",   "PO.WFrTo",   "PO.WSaTo",   "PO.WTotl",   "PO.Power",   "PC.WGWFr",
          "PC.WGWSa",   "PC.WGWTo",   "PC.WSWFr",   "PC.WSWSa",   "PC.WSWTo",   "PC.WFrTo",   "PC.WSaTo",   "PC.WTotl",   "PC.Power",   "TO.WGWFr",   "TO.WGWSa",
          "TO.WGWTo",   "TO.WSWFr",   "TO.WSWSa",   "TO.WSWTo",   "TO.WFrTo",   "TO.WSaTo",   "TO.WTotl"),

  full.names = c("State postal abbreviation",
                 "State FIPS code",
                 "County name",
                 "County FIPS code",
                 "Concatenated state-county FIPS code",
                 "Total population of county, in thousands",
                 "Public Supply, population served by groundwater, in thousands",
                 "Public Supply, population served by surface water, in thousands",
                 "Public Supply, total population served, in thousands",
                 "Public Supply, groundwater withdrawals, fresh, in Mgal/d",
                 "Public Supply, groundwater withdrawals, saline, in Mgal/d",
                 "Public Supply, groundwater withdrawals, total, in Mgal/d",
                 "Public Supply, surface-water withdrawals, fresh, in Mgal/d",
                 "Public Supply, surface-water withdrawals, saline, in Mgal/d",
                 "Public Supply, surface-water withdrawals, total, in Mgal/d",
                 "Public Supply, total withdrawals, fresh, in Mgal/d",
                 "Public Supply, total withdrawals, saline, in Mgal/d",
                 "Public Supply, total withdrawals, total (fresh+saline), in Mgal/d",
                 "Domestic, self-supplied population, in thousands",
                 "Domestic, self-supplied groundwater withdrawals, fresh, in Mgal/d",
                 "Domestic, self-supplied surface-water withdrawals, fresh, in Mgal/d",
                 "Domestic, total self-supplied withdrawals, fresh, in Mgal/d",
                 "Domestic self-supplied per capita use, in gallons/day [DO-WFrTo/DO-SSPop*1000]",
                 "Domestic, deliveries from Public Supply, in Mgal/d",
                 "Domestic, publicly supplied per capita use, in gallons/day [DO-PSDel/PS-TOPop]",
                 "Domestic, total use (withdrawals + deliveries)",
                 "Industrial, self-supplied groundwater withdrawals, fresh, in Mgal/d",
                 "Industrial, self-supplied groundwater withdrawals, saline, in Mgal/d",
                 "Industrial, self-supplied groundwater withdrawals, total, in Mgal/d",
                 "Industrial, self-supplied surface-water withdrawals, fresh, in Mgal/d",
                 "Industrial, self-supplied surface-water withdrawals, saline, in Mgal/d",
                 "Industrial, self-supplied surface-water withdrawals, total, in Mgal/d",
                 "Industrial, self-supplied total withdrawals, fresh, in Mgal/d",
                 "Industrial, self-supplied total withdrawals, saline, in Mgal/d",
                 "Industrial, self-supplied total withdrawals, total (fresh+saline), in Mgal/d",
                 "Irrigation, groundwater withdrawals, fresh, in Mgal/d",
                 "Irrigation, surface-water withdrawals, fresh, in Mgal/d",
                 "Irrigation, total withdrawals, fresh, in Mgal/d",
                 "Irrigation, acres irrigated, sprinkler, in thousands",
                 "Irrigation, acres irrigated, microirrigation, in thousands",
                 "Irrigation, acres irrigated, surface (flood), in thousands",
                 "Irrigation, acres irrigated, total, in thousands",
                 "Irrigation-Crop, groundwater withdrawals, fresh, in Mgal/d",
                 "Irrigation-Crop, surface-water withdrawals, fresh, in Mgal/d",
                 "Irrigation-Crop, total withdrawals, fresh, in Mgal/d",
                 "Irrigation-Crop, acres irrigated, sprinkler, in thousands",
                 "Irrigation-Crop, acres irrigated, microirrigation, in thousands",
                 "Irrigation-Crop, acres irrigated, surface (flood), in thousands",
                 "Irrigation-Crop, acres irrigated, total, in thousands",
                 "Irrigation-Golf, groundwater withdrawals, fresh, in Mgal/d",
                 "Irrigation-Golf, surface-water withdrawals, fresh, in Mgal/d",
                 "Irrigation-Golf, total withdrawals, fresh, in Mgal/d",
                 "Irrigation-Golf, acres irrigated, sprinkler, in thousands",
                 "Irrigation-Golf, acres irrigated, microirrigation, in thousands",
                 "Irrigation-Golf, acres irrigated, surface (flood), in thousands",
                 "Irrigation-Golf, acres irrigated, total, in thousands",
                 "Livestock, groundwater withdrawals, fresh, in Mgal/d",
                 "Livestock, surface-water withdrawals, fresh, in Mgal/d",
                 "Livestock, total withdrawals, fresh, in Mgal/d",
                 "Aquaculture, groundwater withdrawals, fresh, in Mgal/d",
                 "Aquaculture, groundwater withdrawals, saline, in Mgal/d",
                 "Aquaculture, groundwater withdrawals, total, in Mgal/d",
                 "Aquaculture, surface-water withdrawals, fresh, in Mgal/d",
                 "Aquaculture, surface-water withdrawals, saline, in Mgal/d",
                 "Aquaculture, surface-water withdrawals, total, in Mgal/d",
                 "Aquaculture, total withdrawals, fresh, in Mgal/d",
                 "Aquaculture, total withdrawals, saline, in Mgal/d",
                 "Aquaculture, total withdrawals, total (fresh+saline), in Mgal/d",
                 "Mining, groundwater withdrawals, fresh, in Mgal/d",
                 "Mining, groundwater withdrawals, saline, in Mgal/d",
                 "Mining, groundwater withdrawals, total, in Mgal/d",
                 "Mining, surface-water withdrawals, fresh, in Mgal/d",
                 "Mining, surface-water withdrawals, saline, in Mgal/d",
                 "Mining, surface-water withdrawals, total, in Mgal/d",
                 "Mining, total withdrawals, fresh, in Mgal/d",
                 "Mining, total withdrawals, saline, in Mgal/d",
                 "Mining, total withdrawals, total (fresh+saline), in Mgal/d",
                 "Thermoelectric, groundwater withdrawals, fresh, in Mgal/d",
                 "Thermoelectric, groundwater withdrawals, saline, in Mgal/d",
                 "Thermoelectric, groundwater withdrawals, total, in Mgal/d",
                 "Thermoelectric, surface-water withdrawals, fresh, in Mgal/d" ,
                 "Thermoelectric, surface-water withdrawals, saline, in Mgal/d",
                 "Thermoelectric, surface-water withdrawals, total, in Mgal/d",
                 "Thermoelectric, total withdrawals, fresh, in Mgal/d",
                 "Thermoelectric, total withdrawals, saline, in Mgal/d",
                 "Thermoelectric, total withdrawals, total (fresh+saline), in Mgal/d",
                 "Thermoelectric, power generated, in gigawatt-hours",
                 "Thermoelectric once-through, groundwater withdrawals, fresh, in Mgal/d",
                 "Thermoelectric once-through, groundwater withdrawals, saline, in Mgal/d",
                 "Thermoelectric once-through, groundwater withdrawals, total, in Mgal/d",
                 "Thermoelectric once-through, surface-water withdrawals, fresh, in Mgal/d",
                 "Thermoelectric once-through, surface-water withdrawals, saline, in Mgal/d",
                 "Thermoelectric once-through, surface-water withdrawals, total, in Mgal/d",
                 "Thermoelectric once-through, total withdrawals, fresh, in Mgal/d",
                 "Thermoelectric once-through, total withdrawals, saline, in Mgal/d",
                 "Thermoelectric once-through, total withdrawals, total, in Mgal/d",
                 "Thermoelectric once-through, power generated, in gigawatt-hours",
                 "Thermoelectric recirculation, groundwater withdrawals, fresh, in Mgal/d",
                 "Thermoelectric recirculation, groundwater withdrawals, saline, in Mgal/d",
                 "Thermoelectric recirculation, groundwater withdrawals, total, in Mgal/d",
                 "Thermoelectric recirculation, surface-water withdrawals, fresh, in Mgal/d",
                 "Thermoelectric recirculation, surface-water withdrawals, saline, in Mgal/d",
                 "Thermoelectric recirculation, surface-water withdrawals, total, in Mgal/d",
                 "Thermoelectric recirculation, total withdrawals, fresh, in Mgal/d",
                 "Thermoelectric recirculation, total withdrawals, saline, in Mgal/d",
                 "Thermoelectric recirculation, total withdrawals, total (fresh+saline), in Mgal/d",
                 "Thermoelectric recirculation, power generated, in gigawatt-hours",
                 "Total groundwater withdrawals, fresh, in Mgal/d",
                 "Total groundwater withdrawals, saline, in Mgal/d",
                 "Total groundwater withdrawals, total (fresh+saline), in Mgal/d",
                 "Total surface-water withdrawals, fresh, in Mgal/d",
                 "Total surface-water withdrawals, saline, in Mgal/d",
                 "Total surface-water withdrawals, total (fresh+saline), in Mgal/d",
                 "Total withdrawals, fresh, in Mgal/d",
                 "Total withdrawals, saline, in Mgal/d",
                 "Total withdrawals, total (fresh+saline), in Mgal/d"),

  stringsAsFactors = FALSE)


wu.variable = data.frame(matrix(ncol=length(wu_index$full.names), nrow = 0))
colnames(wu.variable) = wu_index$full.names
wu.variable[1,] = wu_index$full.names










