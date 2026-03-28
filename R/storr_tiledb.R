#' A Storr using TileDB Engine
#'
#' Create a `r sQuote("storr")` using TileDB driver for storage.
#'
#' \link[storr:storr]{‘storr’} is a content addressed key-value store
#'  with an optional caching layer. The `storr_tiledb` generates a [TileDBStorr]
#'  object, a subclass of `storr`, with identical interface as `storr`
#'  but with some of its methods to have been rewritten for
#'  speed and efficiency.
#'
#'  `storr_tiledb` also offers (1) option to store key metadata, such as key
#'  expiration date-time and notes, (2) asynchronous writes
#'  using the [mirai](https://cran.r-project.org/web/packages/mirai/index.html)
#'  framework.
#'
#'  ## Cache layer
#'
#'  `storr_tiledb` uses hash tables via [hashtab()] for caching layers (objects
#'  and key metadata) instead of environments.
#'
#'  ## storr classes
#'
#'  You can generate a key-value store in two ways:
#'
#'  ```
#'  # URI path
#'  uri <- tempfile()
#'
#'  # TileDB Storr subclass
#'  sto <- storr_tiledb(uri, init = TRUE)
#'
#'  # storr class
#'  dr <- driver_tiledb(uri)
#'  sto <- storr::storr(dr)
#'
#'  ```
#'
#'  `storr_tiledb` has faster methods; in order to  view which method has been
#'  overwritten, as well as the additional functionality, please refer to
#'  [TileDBStorr] documentation.
#'
#' ## Key Metadata
#'
#'  TODO
#'
#' ## Async
#'
#'  TODO
#'
#'
#' @inheritParams driver_tiledb
#' @param default_namespace The default namespace: `"objects"`.
#' @param async Should the [mirai] daemons be enabled for async
#'  functions? Default is  `FALSE`.
#'
#' @returns An object of class [TileDBStorr], R6.
#'
#' @seealso [driver_tiledb()]
#'
#' @export
#'
storr_tiledb <- function(uri,
                         default_namespace = "objects",
                         context = NULL,
                         init = FALSE,
                         async = FALSE, ...) {

  # check scalar namespace
  dr <- driver_tiledb(uri, context = context, init = init, ...)
  TileDBStorr$new(dr, default_namespace = default_namespace, async = async)

}
