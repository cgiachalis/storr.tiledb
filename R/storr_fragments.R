#' Storr Fragments
#'
#' @inheritParams driver_tiledb
#'
#' @returns An object of class `StorrFragments`, `R6`.
#'
#' @export
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
