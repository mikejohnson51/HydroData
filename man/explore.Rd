% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/explore.R
\name{explore}
\alias{explore}
\title{Explore Spatial HydroData Objects}
\usage{
explore(input = NULL, save = FALSE)
}
\arguments{
\item{input}{a single, or list, of HydroData objects}

\item{save}{(logical) should the leaflet HTML be saved to disk?}
}
\value{
leaflet map object
}
\description{
\code{explore} can be used to explore spatial HydroData objects obtained from
Namely:
 findGHCN
 findNHD
 findReservoir
 findtiger
 findSnotel
 findUSGS
 findWaterbodies
 findWS
 findap
 getCLD
 getKoppen
 getNED
 getNLCD
 getWaterUse
}
\examples{
\dontrun{
# Find USGS station near UCSB

ucsb.nwis = findUSGS(clip_unit = list("UCSB", 10, 10))
explore(ucsb.nwis)
}

}
\author{
Mike Johnson
}
\concept{HydroData 'viz' functions}
