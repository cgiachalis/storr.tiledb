#' A Storr with Time-Travel
#'
#' A `'storr'` variant with read-only class methods and time-travel support.
#'
#' @inheritParams driver_tiledb
#' @param default_namespace The default namespace: `"objects"`.
#' @param timestamp Set a `TileDB` timestamp range that
#'  the resource will be opened at. Effective only for `"READ"` mode.
#'  Valid options:
#'  - A `NULL` value (default)
#'  - An `R` object coercible to `POSIXct` with length 1 which used for end timestamp,
#'  or length 2 with start, end timestamps
#'  - An object of class `tiledb_timestamp`. See [R6.tiledb::set_tiledb_timestamp()]
#'
#' Set a new timestamp with active field `$timestamp`, see examples.
#'
#' @returns An object of class [StorrTimeTravel], R6.
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
#' t0 <- Sys.time()
#' sto$set("a", 1)
#'
#' t1 <- Sys.time()
#' sto$set("b", 2)
#'
#' t2 <- Sys.time()
#'
#' # Open storr with time-travel support at t0
#' stor <- storr_timetravel(uri, timestamp = t0)
#'
#' # Read at t0
#' stor$list_hashes() # character(0), no hashes at t0
#'
#' stor$get("a") #  key 'a' ('objects') not found
#'
#' # Read at t1
#' stor$timestamp <- t1
#' stor$get("a") # 1
#' stor$get("b") # key 'b' ('objects') not found
#'
#' # Read at t2
#' stor$timestamp <- t2
#' sto$get("b") # 2
#'
#'}
#'
#'
storr_timetravel <- function(uri,
                             default_namespace = "objects",
                             context = NULL,
                             timestamp = NULL) {
  # check scalar namespace
  dr <- TimeTravelDriver$new(uri, ctx = context, timestamp = timestamp)
  StorrTimeTravel$new(dr, default_namespace = default_namespace)

}
