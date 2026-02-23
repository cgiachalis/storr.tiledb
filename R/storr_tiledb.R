#' TileDB Storr Driver
#'
#' @param uri The URI path of storr.
#' @param context Optional [tiledb::tiledb_ctx()] object.
#' @param init Should the driver be created if not exist? Defalut is  `FALSE`.
#' @param hash_algorithm Select a hash algorithm.
#' @param compression_level Set an integer value for ZSTD compression level
#' applied in data objects. (experimental).
#' @param ... Other arguments passed to driver's create method when `init=TRUE`.
#'  Valid arguments: `hash_algorithm`, `compression_level` and `keep_open`.
#'
#' @returns
#'
#'  - **storr_tiledb** : returns a TileDB storr, see [TileDBStorr].
#'  - **driver_tiledb** : return a TileDB driver, see [TileDBDriver].
#'  - **driver_tiledb_create** : returns logical `TRUE` invisibly, for
#'   successful storr creation
#'
#'
#'
#' @export
#'
#' @name storr_tiledb
#'
storr_tiledb <- function(uri,
                         default_namespace = "objects",
                         context = NULL,
                         init = FALSE, ...) {

  # check scalar namespace
  dr <- driver_tiledb(uri, context = context, init = init, ...)
  TileDBStorr$new(dr, default_namespace = default_namespace)

}



#' @param hash_algorithm Select a hash algorithm.
#' @param compression_level Set an integer value for ZSTD compression level applied
#' in data objects. (experimental).
#'
#' @export
#'
#' @rdname storr_tiledb
driver_tiledb_create <- function(uri,
                                 hash_algorithm = NULL,
                                 compression_level = -7,
                                 context = NULL) {

  dr <- TileDBDriver$new(uri, ctx = context)
  dr$create(compression_level = compression_level,
            algo = hash_algorithm,
            keep_open = FALSE)

  dr$close()

  invisible(TRUE)
}


#' @export
#' @rdname storr_tiledb
driver_tiledb <- function(uri, context = NULL, init = FALSE, ...) {

  dr <- TileDBDriver$new(uri, ctx = context)

  if (init) {

    l <- list(...)

    if (is.null(l$compression_level)) {
      l$compression_level <- -7
    }

    if (is.null(l$keep_open)) {
      l$keep_open <- TRUE
    }
    force(l)
    dr$create(compression_level = l$compression_level,
              algo =  l$algo, keep_open =  l$keep_open)

  } else {
    if (!dr$exists()) {
      cli::cli_abort("'storr' not found, please create one.", call = NULL)
    }
  }

  dr
}
