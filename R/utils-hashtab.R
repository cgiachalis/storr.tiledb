hashkeys = function(h) {

  val <- vector("character", utils::numhash(h))
  idx <- 0

  utils::maphash(h, function(k, v) {
    idx <<- idx + 1
    val[idx] <<- k
  })

  val
}

exists0 <- function(keys, h){
  keys %in% hashkeys(h)
}

exists1 <- function(key, h){

 v <- utils::gethash(h, key, nomatch = FALSE)

 if (isFALSE(v)) {
   return(v)
 } else {
   return(TRUE)
 }
}

clr_cache_expired_keys <- function(namespace, h) {
  if (is.null(namespace)) {
    utils::maphash(h, function(k, v) {
      if (!(is.null(v$expires_at) || is.na(v$expires_at))) {
        if (v$expires_at < Sys.time()) {
          utils::remhash(h, k)
        }
      }
    })

  } else {
    utils::maphash(h, function(k, v) {
      # Isolate namespace
      ns <- strsplit(k, ":")[[1]][2]

      if (ns %in% namespace) {
        if (!(is.null(v$expires_at) || is.na(v$expires_at))) {
          if (v$expires_at < Sys.time()) {
            utils::remhash(h, k)
          }
        }
      }
    })
  }
}
