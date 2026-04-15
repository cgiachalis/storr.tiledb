#' Storr Fragments
#'
#' Functional interface that initialises a [StorrFragments] instance to
#' work with TileDB Fragments.
#'
#' The class includes fragment consolidation and vacuuming methods but also
#' provides access to [TileDBFragments] instances for `keys` and `data` arrays
#' where you can further inspect and manage the fragments for the specify arrays.
#'
#'
#' @inheritParams driver_tiledb
#'
#' @returns An object of class `StorrFragments`, `R6`.
#'
#' @export
#'
#' @seealso [storr_consolidate()] and [storr_vacuum()]
#'
#' @examples
#' # URI path
#' uri <- tempfile()
#' sto <- storr_tiledb(uri, init = TRUE)
#'
#' fosto <- storr_fragments(uri)
#'
#' fosto$frag_num()
storr_fragments <- function(uri, context = NULL) {
  StorrFragments$new(uri, ctx = context)
}


#' Consolidate Storr Fragments
#'
#' Perform fragment consolidation for better query performance.
#'
#' The function supports selective consolidation ("keys"/"data") or full ("all").
#' The process can be run synchronously or asynchronously and has an optional
#' argument to delete old fragments afterwards (vacuum).
#'
#' @inheritParams driver_tiledb
#' @param what Which array should be consolidated? Defaults to `"all"` arrays.
#' @param vacuum Should the old fragments (consolidated) be deleted? Default is `FALSE`.
#' @param async Should it consolidate asynchronously? Default is `FALSE`.
#'
#' @return When `async = FALSE`, it returns `TRUE` for successful consolidation.
#' For `async = TRUE`, it returns a [mirai::mirai()] object immediately; once
#' resolved, it returns `TRUE` indicating consolidation success.
#'
#' @export
#'
#' @seealso [storr_vacuum()]
#'
#' @examples
#' # URI path
#' uri <- tempfile()
#' sto <- storr_tiledb(uri, init = TRUE)
#'
#' storr_consolidate(uri)
#' @rdname storr_consolidate
storr_consolidate <- function(uri, what = "all", vacuum = FALSE, async = FALSE, context = NULL) {
  sfo <- StorrFragments$new(uri, ctx = context)
  sfo$consolidate(what = what, vacuum = vacuum, async = async)
}


#' Vacuum Storr Fragments
#'
#' Remove old consolidated fragments to free space.
#'
#' The function supports selective vacuuming ("keys"/"data") or full ("all")
#' and it can be run synchronously or asynchronously.
#'
#' @inheritParams driver_tiledb
#' @param what Which array to vacuum? Defaults to `"all"` arrays.
#' @param async Should it vacuum asynchronously? Default is `FALSE`.
#'
#' @return When `async = FALSE`, it returns `TRUE` for successful vacuuming.
#' For `async = TRUE`, it returns a [mirai::mirai()] object immediately; once
#' resolved, it returns `TRUE` indicating vacuum success.
#'
#' @export
#'
#' @seealso [storr_consolidate()]
#'
#' @examples
#' # URI path
#' uri <- tempfile()
#' sto <- storr_tiledb(uri, init = TRUE)
#'
#' storr_vacuum(uri)
#' @rdname storr_vacuum
storr_vacuum <- function(uri,  what = "all", async = FALSE, context = NULL) {
  sfo <- StorrFragments$new(uri, ctx = context)
  sfo$vacuum(what = what, async = async)
}
