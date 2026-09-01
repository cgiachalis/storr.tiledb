# nocov start

.base_export <- utils::getFromNamespace("storr_copy", "storr")
storr_traits <- utils::getFromNamespace("storr_traits", "storr")
make_hash_serialized_object <- utils::getFromNamespace("make_hash_serialized_object", "storr")



## This is needed to support the case where the hash must apply to the
## *entire* structure, just the relevant bytes.
STORR_R_VERSION_BE <- as.raw(c(0L, 3L, 2L, 0L))
STORR_R_VERSION_LE <- as.raw(c(0L, 2L, 3L, 0L))
serialize_object_drop_r_version <- function(object, xdr = TRUE) {
  dat <- serialize_to_raw(object, FALSE, xdr)
  dat[7:10] <- if (xdr) STORR_R_VERSION_BE else STORR_R_VERSION_LE
  dat
}


serialize_to_raw <- function(x, ascii, xdr) {
  serialize(x, NULL, ascii = ascii, xdr = xdr, version = 3L)
}


# source: https://github.com/richfitz/storr/blob/master/R/exceptions.R
KeyError <- function(key, namespace) {
  structure(list(key = key,
                 namespace = namespace,
                 message = sprintf("key '%s' ('%s') not found", key, namespace),
                 call = NULL),
            class = c("KeyError", "error", "condition"))
}

HashError <- function(hash) {
  structure(list(hash = hash,
                 message = sprintf("hash '%s' not found", hash),
                 call = NULL),
            class = c("HashError", "error", "condition"))
}


ConfigError <- function(name, prev, requested) {
  msg <- sprintf("Incompatible value for %s (existing: %s, requested: %s)",
                 name, prev, requested)
  structure(list(name = name,
                 prev = prev,
                 requested = requested,
                 message = msg,
                 call = NULL),
            class = c("ConfigError", "error", "condition"))
}

# nocov end
