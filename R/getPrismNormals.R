## Wraps the ROpenScience PRISM package within the HydroData framework

getPrismNormals = function(state = NULL, county = NULL, clip_unit = NULL, parameters = NULL, months = NULL, annual = FALSE, res = "4km"){

#------------------------------------------------------------------------------#
# Error Handeling                                                              #
#------------------------------------------------------------------------------#

 all.param =  c("tmean", "tmin", "tmax", "ppt", "vpdmin", "vpdmax")
 if(is.null(parameters)) { parameters = all.param}
 bad.param = parameters[ !(parameters %in% all.param)]
 if(length(bad.param) > 0) { stop("'", paste(bad.param, collapse = "', '"),
                                  "' not valid PRISM normal parameters. Use '",
                                  paste(all.param, collapse = "', '"), "'")
 }

 if (all( is.null(months), !annual)) { months = 1:12 }
 bad.months = months[ !(months %in% 1:12)]
 if(length(bad.param) > 0) { stop("'", paste(bad.months, collapse = "', '"),
                                  "' not valid month identifiers. Use ",
                                  paste(1:12, collapse = "', '"), "'")
   }

 valid.res = c( "4km", "800m")
 if (is.null(res)) { res = "4km"}
 if (!(res %in% valid.res)) { stop ("'", res, "' is not valid. Resolution must be: '",
                                    paste(valid.res, collapse = "' or '"), "'")
   }

#------------------------------------------------------------------------------#
# Define AOI                                                                   #
#------------------------------------------------------------------------------#

 AOI = getAOI(state = state, county = county, clip_unit = clip_unit)

#------------------------------------------------------------------------------#
# Files Download                                                               #
#------------------------------------------------------------------------------#
options(prism.path = "~/prismtmp")

lapply(parameters, function(parameters) {
   prism::get_prism_normals(type=parameters, resolution = res, annual = annual,
                            mon = months, keepZip=F)
})


#------------------------------------------------------------------------------#
# Data Manipulation                                                            #
#------------------------------------------------------------------------------#

if(annual){ months = as.character(c(sprintf("%02d", months), "annual"))
} else {
  months = as.character(sprintf("%02d", months))
}

normals = list()

  if ('tmin'   %in% parameters) { normals$tmin   = getPrismRasters (param = 'tmin',   months = months, clip = AOI) }
  if ('tmean'  %in% parameters) { normals$tmean  = getPrismRasters (param = 'tmean',  months = months, clip = AOI) }
  if ('tmax'   %in% parameters) { normals$tmax   = getPrismRasters (param = 'tmax',   months = months, clip = AOI) }
  if ('ppt'    %in% parameters) { normals$ppt    = getPrismRasters (param = 'ppt',    months = months, clip = AOI) }
  if ('vpdmax' %in% parameters) { normals$vpdmax = getPrismRasters (param = 'vpdmax', months = months, clip = AOI) }
  if ('vpdmin' %in% parameters) { normals$vpdmin = getPrismRasters (param = 'vpdmin', months = months, clip = AOI) }


return(normals)

}

getPrismRasters = function(param, months, clip = NULL) {
    files = grep(prism::ls_prism_data(absPath = TRUE)[,2], pattern = param, value = T)

    files <- lapply(months, function(months) {
    return(grep(files, pattern = months, value = T))
    })

    files <- sapply(unlist(files), raster)
    b <- brick(files)

    final =  crop(b, clip)

    names(final) = paste0(months)

    return(final)
}


