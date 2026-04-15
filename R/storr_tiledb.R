#' A Storr using TileDB Engine
#'
#' Create a `r sQuote("storr")` using TileDB driver for storage.
#'
#' \link[storr:storr]{‘storr’} is a content addressed key-value store
#'  with an optional caching layer. The `storr_tiledb` generates a [TileDBStorr]
#'  object with identical interface as `storr` that additionally supports
#'  metadata next to key-values (notes and expiration timestamps) as well as asynchronous
#'  writes using the [mirai](https://cran.r-project.org/web/packages/mirai/index.html)
#'  framework.
#'
#'  `storr_tiledb()` and `storr(driver_tiledb())` can not be used interchangeably
#'  if you use the extra features (i.e., expiration timestamps). The latter
#'  is the standard storr interface and the former produces a stand-alone R6 class
#'  that replicates the storr interface with additional features.
#'
#'   Another difference, but not visible to the user, is that the
#'  `storr_tiledb`'s cache layer uses hash tables via \link[utils:hashtab]{hashtab()} instead of
#'  environments.
#'
#'  ## Cache option
#'  By default, the in-memory caching layer is enabled. The global option `storr.tiledb.cache`
#'  can be used to disable it, like so: `options(storr.tiledb.cache = FALSE)`.
#'
#'  ## Buffer size
#'  The buffer allocation size is set to 3 MB per column when fetching data. Use
#'  [tiledb::set_allocation_size_preference()] to set a different limit.
#'
#' @inheritParams driver_tiledb
#' @param default_namespace The default namespace: `"objects"`.
#' @param async Should the [mirai] daemons be enabled for async
#'  functions? Default is  `FALSE`.
#' @param ... Other arguments passed to driver when `init = TRUE`.
#'  Valid arguments: `compression_level` and `keep_open`.
#'
#' @returns An object of class [TileDBStorr], R6.
#'
#' @seealso [driver_tiledb()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # URI path
#' uri <- tempfile()
#' sto <- storr_tiledb(uri, init = TRUE)
#'
#' # set key-values
#' sto$set("a", 1)
#' sto$set("b", 1, namespace = "ns1", notes = "note1")
#'
#' # listing methods
#' sto$list("ns1") # b
#' sto$list_namespaces() # "ns1"     "objects"
#' sto$list_hashes() # "632336c518ae1c89ecf26ae5fbec5860"
#'
#' # get methods
#' sto$get("a") # 1
#' sto$get("b", "ns1") # 1
#' sto$get_keymeta("b", "ns1") # list(exprires_at = NA, notes = "note1")
#'
#' #-----------------------------------------------------------------
#' #   Storr with encryption
#' #-----------------------------------------------------------------
#'
#' # Requires a TileDB Context with encryption configuration parameters
#' key <- "0123456789abcdeF0123456789abcdeF"
#' config <- tiledb::tiledb_config()
#' config["sm.encryption_type"] <- "AES_256_GCM";
#' config["sm.encryption_key"] <- key
#' ctx <- tiledb::tiledb_ctx(config)
#'
#' # Create a storr with context that encapsulates encryption configuration
#' uri_enc <- tempfile()
#' stoe <- storr_tiledb(uri_enc, init = TRUE, context = ctx)
#'
#' stoe$set("a", 1)
#' stoe$get("a") # 1
#'
#' # No access without the key
#' # stoe_new <- storr_tiledb(uri_enc) # This will fail
#'
#' # Pass the context with encryption parameters
#' stoe_new <- storr_tiledb(uri_enc, context = ctx)
#' stoe_new$get("a") # 1
#'}
#'
#'
storr_tiledb <- function(uri,
                         default_namespace = "objects",
                         context = NULL,
                         init = FALSE,
                         hash_algorithm = NULL,
                         async = FALSE, ...) {

  # check scalar namespace
  dr <- driver_tiledb(uri, context = context, init = init, hash_algorithm = hash_algorithm,...)
  TileDBStorr$new(dr, default_namespace = default_namespace, async = async)

}

#' Copy Storr to another URI
#'
#' @inheritParams storr_tiledb
#'
#' @param to_uri Destination URI path to copy the storr to.
#'
#' @export
#'
#' @returns The new uri path, invisibly.
#'
#' @family storr-utilities
#'
#' @rdname storr_copy
storr_copy <- function(uri, to_uri, context = NULL) {

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

#' Move Storr to another URI
#'
#' @inheritParams storr_tiledb
#' @param newuri Destination URI path to move the storr to.
#'
#' @export
#'
#' @returns The new uri path, invisibly.
#'
#' @family storr-utilities
#'
#' @rdname storr_move
storr_move <- function(uri, newuri, context = NULL) {

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

#' Rename Storr URI
#'
#' It renames the driver's basename, i.e., 'path/oldname' to 'path/newname'.
#'
#' @inheritParams storr_tiledb
#' @param newname Suffix to rename storr URI path.
#'
#' @export
#'
#' @returns The new uri path, invisibly.
#'
#' @family storr-utilities
#'
#' @rdname storr_rename
storr_rename <- function(uri, newname, context = NULL) {

  dr <- TileDBDriver$new(uri, ctx = context)

  if (isFALSE(.is_scalar_character(newname))) {
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
