#' A Storr using TileDB Engine
#'
#' A convenient wrapper for standard storr interface, e.g., `storr::storr(driver_tiledb())`
#' with key difference that corrects for serialization format if it is other than
#' `R` native serialization.
#'
#' @details
#' When the standard interface is needed with default object serialization, then
#' `storr::storr(driver_tiledb())` and `storr_tdb0()` are equivalent. On the other
#' hand, when the driver is configured with `qs2` or `qdata` serialization format,
#' the `storr_tdb0()`  will be adjusted to use the correct serialization function.
#'
#' @inheritParams storr_tiledb uri default_namespace context
#'
#' @returns A 'storr' object.
#'
#' @export
#'
#' @keywords internal
#'
#' @seealso [storr_tiledb()] and [driver_tiledb()]
#'
#' @examples
#' \dontrun{
#' # URI path
#' uri <- tempfile()
#' driver_tiledb_create(uri)
#' sto <- storr_tdb0(uri)
#' }
#'
storr_tdb0 <- function(uri,
                       default_namespace = "objects",
                       context = NULL) {

  dr <- driver_tiledb(uri, context = context)

  sto <- storr::storr(dr, default_namespace = default_namespace)

  if (dr$serial_format != "rds") {
    sto$serialize_object <- make_serialize_object(sto$traits, serial_format = dr$serial_format)
  }

  sto
}
