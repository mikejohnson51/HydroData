#' Choose what to return for HydroData Calls
#'
#' @description  A function defining what should be returned in a HydroData object
#' @param items an items list
#' @param report a report vector
#' @param AOI the defined AOI
#' @param basemap the basemap call
#' @param boundary the boundary call
#' @param clip_unit the clip_unit defintion
#' @param ids the ids call
#'
#' @family HydroData 'helper' functions
#'
#' @return
#' @export
#' @author Mike Johnson

return.what = function(items, report, AOI, basemap, boundary, clip_unit, ids ){


if (!(basemap == FALSE))  {
    if (basemap == TRUE) {
      type = 't'
      name = 'terrain'
    } else {
      type = basemap
    }

    if (type == 't') { name = 'terrain'   }
    if (type == 'h') { name = 'hybrid'    }
    if (type == 's') { name = 'satellite' }
    if (type == 'r') { name = 'roads'   }

    items[['basemap']] = getBasemap(AOI = AOI, type = type)
    report = append(report, paste(name, "basemap"))
  }

  if (boundary) { items[['boundary']] = AOI
  report = append(report, "AOI boundary")

  if (!is.null(clip_unit)) { items[['fiat']] = getFiatBoundary(clip_unit = AOI)
  report = append(report, "fiat boundary")
  }
  }

  if (ids) { items[['ids']] = sp$ID
  report = append(report, "list of station IDs")
  }

  if (length(report) > 1) { report[length(report)] = paste("and",  tail(report, n = 1)) }

  message(paste(report, collapse = ", "))
  return(items)

}

