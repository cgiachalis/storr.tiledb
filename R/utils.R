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
