.libtiledb_vfs_copy_dir <- utils::getFromNamespace("libtiledb_vfs_copy_dir", "tiledb")
.libtiledb_array_consolidate <- utils::getFromNamespace("libtiledb_array_consolidate", "tiledb")
.libtiledb_array_vacuum <- utils::getFromNamespace("libtiledb_array_vacuum", "tiledb")
file_path <- function(..., fsep = .Platform$file.sep) {

  paths <- list(...)

  if (is_remote_uri(paths[[1]])) fsep <- "/"
  file.path(..., fsep = fsep)
}

#' Checks for remote URI
#' @noRd
is_remote_uri <- function(x) {
  .string_starts_with(x, "s3://") | .string_starts_with(x, "azure://") |
    .string_starts_with(x, "gcs://") | .string_starts_with(x, "tiledb://")
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

.is_scalar_numeric <- function(x) {
  (typeof(x) %in% c("double", "integer")) && is.atomic(x) && length(x) == 1L
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

# Get function to un-serialize from string ('rds', 'qs2' or 'qdata')
make_unserialize_object <- function(x) {

  switch (x,
    rds = function(.s) {unserialize(charToRaw(.s)) },
    qs2 = function(.s) {qs2::qs_deserialize(qs2::base91_decode(.s)) },
    qdata = function(.s) {qs2::qd_deserialize(qs2::base91_decode(.s)) },
  )

}

# Get function to serialize to string ('rds', 'qs2' or 'qdata')
# param x A list objects with  traits
make_serialize_object <- function(x, serial_format = "rds", xdr = TRUE, r_version = getRversion()) {


  if (serial_format == "rds") {
   # NB: storr's original 'make_serialize_object' helper function
   #     'storr.tiledb' uses only string serialization
    drop_r_version <- x$drop_r_version
    string <- x$accept == "string"
    # TODO: Review cases: we don't need all
    if (string) {
      if (drop_r_version) {
        stop("Can't combine drop_r_version and string serialization")
      }
      ## I really want the ascii = NA form of string serialization
      ## because it is safer with respect to precision loss in doubles.
      ## It's the only thing I know of that depends on R between 3.1 and
      ## 3.2 and affects only the dbi driver at present.
      if (r_version < numeric_version("3.2.0")) {
        stop("Please upgrade R to at least 3.2.0")
      }
      function(object) rawToChar(serialize_to_raw(object, NA, xdr))
    } else if (drop_r_version) {
      function(object) serialize_object_drop_r_version(object, xdr)
    } else {
      function(object) serialize_to_raw(object, FALSE, xdr)
    }

  } else if (serial_format == "qs2") {

    function(object) qs2::base91_encode(qs2::qs_serialize(object))

  } else if (serial_format == "qdata") {

    function(object) qs2::base91_encode(qs2::qd_serialize(object))

  } else {
    # NB: It's not reachable; in case it does, something is broken at driver
    stop(sprintf("Unknown serialization format %s", serial_format), call. = FALSE)
  }

}
