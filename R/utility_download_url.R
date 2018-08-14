#' Download data from URL
#'
#' @param url url
#' @return
#' @export
#'

download.url = function(url){

  destdir <- normalizePath(paste0(tempdir(), "/."))
  destfile <- paste0(destdir, "/", basename(url))

  if(!file.exists(destfile)){
    x = httr::GET(url = url, httr::write_disk(destfile, overwrite = T), httr::progress(type = "down"))
  } else {x = list(status_code = 200)}

  return(list(code = x$status_code, destfile = destfile))

}
