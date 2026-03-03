# TileDBDriver
roxy_key <- "A character vector of keys."
roxy_namespace <- "A character vector of namespaces."
roxy_hash <- "A vector of hash values."
roxy_expires <-  "A vector of expiration datetimes."
roxy_notes <- "A character vector of notes."

# set/get
# TileDBStorr
sto_key <- function(k = 0) {
  if (k == 0) {
    "A scalar character of key name."
  } else {
    "A character vector of key names."
  }

}

sto_value <- function(n = 0) {

  if (n == 0) {
    "An R object to store."
  } else {
    "A vector of values to store."
  }

}

sto_namespace <- function(n = 0) {
  if (n == 0) {
    "A scalar character of namespace name."
  } else {
    "A character vector of namespaces."
  }
}

sto_expires <- function(n = 0) {
  if (n == 0) {
    "A date-time object of class `POSIXct` (optional)."
  } else {
    "A vector of date-times of class `POSIXct` (optional)."
  }
}
sto_notes <-function(n = 0) {
  if (n == 0) {
    "A scalar string of notes (optional)."
  } else {
    "A character vector of notes (optional)."
  }
}

sto_cache <- "Should the cache be used? Default is `TRUE`."
sto_cache_meta <- "Should the cache be used to retrieve the metadata?
    Default is `TRUE`. If a key:namespace not found in the cache, it will
    be fetched from database. Note that when setting `FALSE`, the cache
    will always be cleared for this key-namespace; this is to avoid mismatch
    between cache and database when reading back  with
    `use_cache = TRUE`."


sto_cfg <- "Pass a [tiledb::config()] object to override context's configuration."

sto_recycle_note <- "The arguments `key` and `namespace` can be recycled if any of them is a
    scalar character and the other is a vector. No other recycling rule is permitted."
