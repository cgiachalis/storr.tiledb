#' TileDB Storr Driver
#'
#' Storr driver using TileDB storage engine.
#'
#' `driver_tiledb()` returns the TileDB driver given a URI path. You can create
#' a new driver using `init` argument or with `driver_tiledb_create()`.
#'
#' Note that [storr_tiledb()] abstracts the driver creation, so it's the preferred way unless
#' you want to pass the driver onto  \link[storr:storr]{storr}.
#'
#' `driver_tiledb_copy()` copies the driver to another URI path.
#'
#' `driver_tiledb_move()` moves the driver to another URI path.
#'
#' `driver_tiledb_rename()` renames the driver's basename, i.e., 'path/oldname' to
#' 'path/newname'
#'
#' @section Data Model:
#'
#' The underlying structure is similar to \link[storr:driver_dbi]{driver_dbi()};
#' a content-addressed database that consists of two (array) tables.
#'
#' Specifically, the driver storage is a TileDB Group with two member Arrays
#' which are stored relative to Group's URI path. The next sub-sections describe
#' the group structure and array data models.
#'
#' **Group**
#'
#' *Members*
#'
#'  1. `tbl_keys` (array): maps key-namespace pairs to hashes (and to expiry and/or
#'  notes, optional)
#'  2. `tbl_data` (array): maps hashes to values (serialised R objects)
#'
#' *Metadata*
#'
#'  - `hash_algo`: The name of hash algorithm.
#'  - `type`: Group identifier, `"storr"`
#'
#' **Arrays**
#'
#' *tbl_keys*: A 2D sparse array that maps key-namespace pairs to hashes and key-metadata.
#'
#' - Dimensions: `namespace` (*ASCII*) and `key` (*ASCII*)
#' - Attributes: `hash` (*ASCII*), `expires_at` (*DATETIME_MS*) and `notes` (*UTF8*)
#'
#'
#' *tbl_data*: A 1D sparse array that maps hashes to object values.
#'
#' - Dimensions: `hash` (*ASCII*)
#' - Attributes: `value` (*ASCII*)
#'
#' TileDB datatypes in parentheses.
#'
#' @param uri The URI path of storr.
#' @param context Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object.
#' @param init Should the driver be created if not exist? Default is  `FALSE`.
#' @param ... Other arguments passed to driver's create method when `init = TRUE`.
#'  Valid arguments: `hash_algorithm`, `compression_level` and `keep_open`.
#'
#' @returns
#'  - *driver_tiledb()* returns a [TileDBDriver] object.
#'  - *driver_tiledb_create()* returns logical `TRUE` invisibly, for successful
#'  driver creation.
#'  - *driver_tiledb_copy()*, *driver_tiledb_move()* and *driver_tiledb_rename()*
#'  return the new uri path, invisibly.
#'
#' @export
#'
#' @seealso [storr_tiledb()]
#'
#' @examples
#'\dontrun{
#' # URI path
#' uri <- tempfile()
#'
#' # create driver
#' dr <- driver_tiledb(uri, init = TRUE)
#'
#' dr$print()
#' # R6Class: <TileDBDriver>
#' #  → URI Basename: file6bb0182c1362
#' #   • Arrays: "tbl_keys" and "tbl_data"
#'
#' # members
#' dr$names()
#' # "tbl_keys" "tbl_data"
#'}
#'
#' @name driver_tiledb
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
              algo =  l$hash_algorithm, keep_open =  l$keep_open)

  } else {
    if (!dr$exists()) {
      cli::cli_abort("'storr' not found, please create one.", call = NULL)
    }
  }

  dr
}

#' @param hash_algorithm Select a hash algorithm supported by \link[digest:digest]{digest}:
#'  `r sQuote(formals(digest::digest)$algo[-1])`. If not given, the  default is 'md5'.
#' @param compression_level Set an integer value for ZSTD compression level applied
#' in data objects. (experimental).
#'
#' @export
#' @rdname driver_tiledb
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

#' @param to_uri Destination URI path to copy the storr to.
#' @export
#' @rdname driver_tiledb
driver_tiledb_copy <- function(uri, to_uri, context = NULL) {


  dr <- TileDBDriver$new(uri, ctx = context)

  check_uri(to_uri)

  olduri <- dr$uri

  vfs <- tiledb::tiledb_vfs(ctx = dr$ctx)

  if (tiledb::tiledb_vfs_is_dir(to_uri, vfs = vfs)) {
    cli::cli_abort("Directory is already present with uri: {.url {to_uri}}.", call = NULL)
  }

  newuri <- .libtiledb_vfs_copy_dir(vfs@ptr, olduri, to_uri)

  invisible(newuri)

}

#' @param newuri Destination URI path to move the storr to.
#' @export
#' @rdname driver_tiledb
driver_tiledb_move <- function(uri, newuri, context = NULL) {

  dr <- TileDBDriver$new(uri, ctx = context)
  olduri <- dr$uri

  check_uri(newuri)

  vfs <- tiledb::tiledb_vfs(ctx = dr$ctx)

  if (tiledb::tiledb_vfs_is_dir(newuri, vfs = vfs)) {
    cli::cli_abort("Directory is already present with uri: {.url {newuri}}.", call = NULL)
  }

  newuri <- tiledb::tiledb_vfs_move_dir(olduri, newuri, vfs = vfs)

  invisible(newuri)

}

#' @param newname Suffix to rename storr URI path.
#' @export
#' @rdname driver_tiledb
driver_tiledb_rename <- function(uri, newname, context = NULL) {


  dr <- TileDBDriver$new(uri, ctx = context)

  if (isFALSE(rlang::is_scalar_character(newname))) {
    cli::cli_abort(
      "{.arg {deparse(substitute(newname))}} should be a character string.", call = NULL
    )
  }

  olduri <- dr$uri
  vfs <- tiledb::tiledb_vfs(ctx = dr$ctx)

  newuri <- file_path(dirname(olduri), newname)

  if (tiledb::tiledb_vfs_is_dir(newuri, vfs = vfs)) {
    cli::cli_abort("Directory is already present with uri: {.url {newuri}}.", call = NULL)
  }

  newuri <- tiledb::tiledb_vfs_move_dir(olduri, newuri, vfs = vfs)

  invisible(newuri)

}
