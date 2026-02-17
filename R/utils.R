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


validate_hash_algo <- function(x) {
  hash_choices <- c(
    "md5",
    "sha1",
    "crc32",
    "sha256",
    "sha512",
    "xxhash32",
    "xxhash64",
    "murmur32",
    "spookyhash",
    "blake3",
    "crc32c",
    "xxh3_64",
    "xxh3_128"
  )
  x <- match.arg(x, choices = hash_choices)

  x
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

