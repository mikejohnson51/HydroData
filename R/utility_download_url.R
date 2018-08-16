#' @title Download data from URL via httr::GET
#' @description Download file from URL using httr:GET.
#' File is written to the tmp directory with the basename of the URL call.
#' @param url url
#' @return a status and path to downloaded file
#' @author Mike Johnson
#' @export

download.url = function(url){

  destdir <- normalizePath(paste0(tempdir(), "/."))
  destfile <- paste0(destdir, "/", basename(url))

  if(!file.exists(destfile)){
    x = httr::GET(url = url, httr::write_disk(destfile, overwrite = T), httr::progress(type = "down"))
  } else {x = list(status_code = 200)}

  return(list(code = x$status_code, destfile = destfile))

}
