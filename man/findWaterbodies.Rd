% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/findWaterbodies.R
\name{findWaterbodies}
\alias{findWaterbodies}
\title{Find National Hydrography Dataset Waterbodies}
\usage{
findWaterbodies(AOI = NULL, area = NULL)
}
\arguments{
\item{AOI}{A Spatial* or simple features geometry, can be piped from \link[AOI]{getAOI}}

\item{ids}{If TRUE, a vector of waterbody IDs is added to retuned list (default = \code{FALSE})}
}
\value{
a list() of minimum length 2: AOI and waterboies
}
\description{
\code{findWaterbodies} returns a \code{SpatialPolgonsDataframe} of all NHDwaterbodies within an AOI.
Data comes from the USGS CIDA server and contain 23 attributes, perhaps most notably:
\itemize{
\item 'objectid'   : \code{integer}  Integer value that uniquely identifies the waterbody of each feature in the NHD
\item 'comid'   : \code{character}  The COMID draining into the feature
\item 'fdate': \code{POSITct}  Date of last feature modification
\item 'gnis_id'   : \code{character}    Unique identifier assigned by GNIS
\item 'gnis_name'   : \code{character}    Proper name, term, or expression by which a particular geographic entity is known
\item 'meandepth'    : \code{numeric}     Mean depth of the waterbody
\item 'lakevolume'    : \code{numeric}    Total waterbody volume
\item 'maxdepth'   : \code{character}     Maximum depth of waterbody
\item 'meanused'   : \code{integer}  The average amount of water used
}
}
\examples{
\dontrun{
getAOI(clip = "Tuscaloosa") \%>\% findWaterbodies()
}

}
\author{
Mike Johnson
}
