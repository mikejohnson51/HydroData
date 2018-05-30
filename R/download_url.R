download.url = function(url, mode = "curl", max.try = 5){

  destdir <- normalizePath(paste0(tempdir(), "/."))
  destfile <- paste0(destdir, "/", basename(url))

  message("Downloading file")

  code = -9999
  rep = 0


  if(mode == 'binary'){

    while(code != 0 & rep < max.try){
      rep = rep + 1
      message("using download.file")
      code = download.file(url = url, destfile = destfile, mode = "wb", quiet = TRUE)

      if(code != 0){
        Sys.sleep(2)
      }
    }

    if(code == 0){ code = 200}

  } else {

  param <- list(verbose = FALSE,
                noprogress = TRUE,
                fresh_connect = TRUE,
                ftp_use_epsv = FALSE,
                forbid_reuse = TRUE)

  handle <- curl::new_handle()
  curl::handle_setopt(handle, .list = param)

  while(code != 200 & rep < max.try){
    rep = rep + 1
    r = curl::curl_fetch_disk(url, path = destfile, handle = handle)
    code = r$status_code

    if(code != 200){
      Sys.sleep(2)
    }
  }
  }

  return(list(code = code, destfile = destfile))

}
