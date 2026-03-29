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
