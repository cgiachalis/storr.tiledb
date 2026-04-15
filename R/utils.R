.libtiledb_vfs_copy_dir <- utils::getFromNamespace("libtiledb_vfs_copy_dir", "tiledb")
.libtiledb_array_consolidate <- utils::getFromNamespace("libtiledb_array_consolidate", "tiledb")
.libtiledb_array_vacuum <- utils::getFromNamespace("libtiledb_array_vacuum", "tiledb")
file_path <- function(..., fsep = .Platform$file.sep) {

  paths <- list(...)

  if (is_remote_uri(paths[[1]])) fsep <- "/"
  file.path(..., fsep = fsep)
}

# TODO: add azure/gcs when supported
#' Checks for remote URI
#' @noRd
is_remote_uri <- function(x) {
  .string_starts_with(x, "s3://") | .string_starts_with(x, "tiledb://")
}

.string_collapse <- function(x, sep = ", ") {
  paste0(x, collapse = sep)
}

.string_starts_with <- function(x, prefix) {
  prefix <- paste0("^", prefix)
  grepl(prefix, x)
}

squote <- function (x) {
  sprintf("'%s'", x)
}

.hash_choices <- function() {
  eval(formals(digest::digest)$algo)
}

validate_hash_algo <- function(x) {
  x <- match.arg(x, choices = .hash_choices())
}

.is_scalar <- function(x, type) {
  (typeof(x) == type) && is.atomic(x) && length(x) == 1L
}

.is_character <- function(x) {
  typeof(x) == "character"
}

.is_scalar_character <- function(x) {

  .is_scalar(x, "character")
}

.is_scalar_logical <- function(x) {

  .is_scalar(x, "logical")
}


vcapply <- function(X, FUN, ...) {
  vapply(X, FUN, character(1), ...)
}

vlapply <- function(X, FUN, ...) {
  vapply(X, FUN, logical(1), ...)
}

check_tiledb_config <- function(x) {
  if (!inherits(x, "tiledb_config")) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be of class {.help [{.fun tiledb_config}](tiledb::tiledb_config)}.", call = NULL)
  }
}

check_tiledb_ctx <- function(x) {
  if (!inherits(x, what = 'tiledb_ctx')) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be a {.help [{.fun tiledb_ctx}](tiledb::tiledb_ctx)} object.", call = NULL)
  }
}

check_character_or_null <- function(x) {
  if (!(.is_character(x) || is.null(x))) {
    cli::cli_abort("{.arg {deparse(substitute(x))}} should be a character vector or NULL.", call = NULL)
  }
}

check_uri <- function(uri) {
  if (isFALSE(.is_scalar_character(uri))) {
    cli::cli_abort(
      "{.arg {deparse(substitute(uri))}} should be a character string for URI path",
      call = NULL
    )
  }
}

check_read_only = function(x) {
  cli::cli_abort(paste0(cli::style_italic("{.val {x}}"), " is a read-only field."), call = NULL)

}

