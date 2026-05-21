#' @title Generate a `StorrTimeTravel` Object
#'
#' @description A [TileDBStorr] is a time-travel variant of `TileDBStorr` designed
#' to query data at specific points in time and in read-only mode with no write
#' capabilities.
#'
#' This class is not intended to be used directly and the preferred
#' usage is through [storr_timetravel()].
#'
#' @returns A `StorrTimeTravel`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
StorrTimeTravel <- R6::R6Class(
  classname = "StorrTimeTravel",
  cloneable = FALSE,

  public = list(

    #' @field default_namespace The default namespace.
    #'
    default_namespace = NULL,

    #' @field traits Driver traits.
    #'
    traits = NULL,

    #' @description Initialise `StorrTimeTravel`.
    #'
    #' @param driver A [TimeTravelDriver] object.
    #' @param default_namespace The default namespace.
    #'
    initialize = function(driver, default_namespace) {

      if (!inherits(driver, "TimeTravelDriver")) {
        stop("Not a valid Time-Travel 'driver'. Please use a 'TTDriver' object.",
             call. = FALSE)
      }

      private$check_input(default_namespace, n = 1, type = "character")

      # We need the member's object to be available
      # e.g., driver$members$tbl_keys$object
      if (!driver$is_open() ) {
        # Ensure the members are instantiated
        driver$open(instantiate = TRUE)
      }

      if (driver$is_open() && !driver$members_instantiated) {
        driver$close()
        driver$open(instantiate = TRUE)
      }

      private$DRIVER <- driver


      self$default_namespace <- default_namespace
      self$traits <- storr_traits(driver$traits)

    },

    #' @description Get an object given a key-namespace pair.
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #'
    #' @return The `R` object if available.
    #'
    get = function(key, namespace = self$default_namespace) {
      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")
      hash <- self$get_hash(key, namespace)
      self$get_value(hash)
    },

    #' @description Get multiple objects.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param missing Value to use for missing elements.
    #'
    #' @return A list of `R` objects.
    #'
    mget = function(key, namespace = self$default_namespace,  missing = NULL) {

      # NB: storr::join_key_namespace check is performed inside $query_keys0
       hash <- self$mget_hash(key, namespace)
       self$mget_value(hash, missing)
    },

    #' @description Get hash value.
    #'
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #'
    #' @return The hash value.
    #'
    get_hash = function(key, namespace = self$default_namespace) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      if (self$traits$throw_missing) {
        tryCatch(private$DRIVER$get_hash(key, namespace), error = function(e) {
          stop(KeyError(key,namespace))
        })
      }
      else {
        if (self$exists(key, namespace)) {
          private$DRIVER$get_hash(key, namespace)
        }
        else {
          stop(KeyError(key, namespace))
        }
      }
    },

    #' @description Get hash values.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #'
    #' @return A vector of hashes.
    #'
    mget_hash = function(key, namespace = self$default_namespace) {

      private$DRIVER$mget_hash(key, namespace)
    },


    #' @description Get an object given its hash.
    #'
    #'
    #' @param hash The hash value of the object.
    #'
    #' @return The `R` object if available.
    #'
    get_value = function(hash) {

      # TODO: no need for traits
      if (self$traits$throw_missing) {
        value <- tryCatch(
          private$DRIVER$get_object(hash),
          error = function(e)
            stop(HashError(hash))
        )
      } else {
        if (!private$DRIVER$exists_object(hash)) {
          stop(HashError(hash))
        }
        value <- private$DRIVER$get_object(hash)
      }

      value
    },

    #' @description Get multiple objects given their hashes.
    #'
    #'
    #' @param hash A vector of hash values."
    #' @param missing Value to use for missing elements.
    #'
    #' @return A list of `R` objects.
    #'
    mget_value = function(hash, missing = NULL) {

      value <- vector("list", length(hash))
      cached <- logical(length(hash))
      is_missing <- is.na(hash)

      cached[is_missing] <- TRUE
      value[is_missing] <- list(missing)

      if (any(!cached)) {
          value[!cached] <- private$DRIVER$mget_object(hash[!cached])
      }

      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing)
      }
      value
    },

    #' @description Get key's metadata.
    #'
    #'
    #' @param key The key name to get metadata values from.
    #' @param namespace The namespace to look the key within.
    #'
    #' @return A named list with the key-metadata: `"expires_at"`
    #' and `"notes".`
    #'
    get_keymeta = function(key, namespace = self$default_namespace) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      keyns <- paste(key, namespace, sep = ":")

      value <- private$DRIVER$get_keymeta(key, namespace)

      value
    },

    #' @description Get multiple key metadata.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param missing Fill value for missing keys. Default is `NULL`.
    #'
    #' @return A list with key metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    mget_keymeta = function(key,
                            namespace = self$default_namespace,
                            missing = NULL) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n

      key <- p$key
      namespace <- p$namespace
      keyns <- paste(key, namespace, sep = ":")

      value <- vector("list", n)
      cached <- logical(n)

      # Everything is TRUE, so go to find them in DB
      not_cached <- !cached
      status_not_cached <- TRUE
      num_cached <- 0L

      is_missing <- FALSE

      if (status_not_cached) {

        # From not_cached find also which are truly missing
        cc <- private$DRIVER$mget_keymeta(key[not_cached],
                                       namespace[not_cached],
                                       nomatch = missing)

        value[not_cached] <- cc
        keyns_not_cached <- keyns[not_cached]

        # not_cached and not found
        keyns_missing <- keyns_not_cached[attr(cc, "missing")]

        # Truly missing key-namespace pairs
        is_missing <- keyns %in% keyns_missing
      }


      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing) #+ num_cached
      }
      value
    },

    #' @description Check a key-namespace pair exists.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #'
    #' @return A logical vector indicating which key-namespace pair exists.
    #'
    exists = function(key, namespace = self$default_namespace) {
      private$DRIVER$exists_hash(key, namespace)
    },

    #' @description Check a serialised object exists given a hash.
    #'
    #' @param hash `r roxy_hash`
    #'
    #' @return A logical vector indicating which object exists.
    #'
    exists_object = function(hash) {
      private$DRIVER$exists_object(hash)
    },

    #' @description Get the key-namespace pairs with expiration timestamps.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #' @param datetimes Should the `expires_at` column be returned?
    #' Default is `TRUE`.
    #'
    #' @return An object of class `data.table`.
    #'
    keys_with_expiration = function(namespace = self$default_namespace, datetimes = TRUE) {
      out <- private$DRIVER$keys_with_expiration(namespace, datetimes = datetimes)
      data.table::as.data.table(out)
    },

    #' @description Get the expired key-namespace pairs.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #' @param datetimes Should the `expires_at` column be returned?
    #' Default is `TRUE`.
    #'
    #' @return An object of class `data.table`.
    #'
    expired_keys = function(namespace = self$default_namespace, datetimes = TRUE) {
      out <- private$DRIVER$expired_keys(namespace, datetimes = datetimes)
      data.table::as.data.table(out)
    },

    #' @description Check for expired key-namespace pairs.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #'
    #' @return `TRUE` for expired keys, `FALSE` otherwise.
    #'
    has_expired_keys = function(namespace = self$default_namespace) {

      private$DRIVER$has_expired_keys(namespace)

    },

    #' @description List all keys stored in a namespace.
    #'
    #' @param namespace `r sto_namespace()`
    #'
    #' @return A sorted character vector with keys.
    #'
    list = function(namespace = self$default_namespace) {

      sort(private$DRIVER$list_keys(namespace))
    },

    #' @description List all hashes stored in the storr.
    #'
    #'
    #' @return A sorted character vector with hashes.
    #'
    list_hashes = function() {

      sort(private$DRIVER$list_hashes())
    },

    #' @description List all namespaces in the storr.
    #'
    #'
    #' @return A sorted character vector with namespaces.
    #'
    list_namespaces = function() {

      sort(private$DRIVER$list_namespaces())
    },

    #' @description Export objects from storr.
    #'
    #' Use list() to export to a brand new list, or use as.list(object) for a shorthand.
    #'
    #' @param dest A destination to export objects to. It can be a storr, list, or environment.
    #' @param list Names of objects to import (or `NULL` for all objects) . If given it must be a character vector.
    #'  If named, the names of the character vector will be the names of the objects as created in the storr.
    #' @param namespace  Namespace to get objects from, and to put objects into.  If `NULL`,
    #' then this will export namespaces from this (source) storr into the destination;
    #' if there is more than one namespace, this is only possible if `dest`
    #' is a storr (otherwise there will be an error).
    #' @param skip_missing  Logical, indicating if missing keys (specified in `list`)
    #' should be skipped over, rather than being treated as an error (the default).
    #'
    #'
    #' @return `dest` object, invisibly.
    #'
    export = function(dest, list = NULL, namespace = self$default_namespace,
                      skip_missing = FALSE) {

      if (is.null(namespace)) {
        namespace <- self$list_namespaces()
      }

      invisible(.base_export(dest, self, list, namespace, skip_missing)$dest)
    },

    #' @description Generate a `data.table` with an index of objects
    #' present in a storr.
    #'
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #'
    #' @return An object of class `data.table`.
    #'
    index_export = function(namespace = NULL) {

      out <- private$DRIVER$filter_keys(character(), namespace = namespace)[]

      if (nrow(out) == 0) {

        d <- data.frame(
          namespace = character(0),
          key = character(0),
          hash = character(0),
          expires_at = as.POSIXct(double()),
          notes = character(0)
        )

        out <- data.table::as.data.table(d)
      }

      out
    },

    #' @description Export objects from storr to another TileDB storr.
    #'
    #' @param key A character vector of source keys.
    #' @param namespace `r sto_namespaces_or_null`
    #' @param uri_dest The URI path of destination storr.
    #' @param context_dest Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object
    #' for destination storr.
    #'
    #' @return A logical `TRUE` indicating successful export, invisibly.
    #'
    export_tdb = function(key = character(0),
                          namespace = self$default_namespace,
                          uri_dest,
                          context_dest = NULL) {

      dest_driver <- driver_tiledb(uri_dest, context = context_dest)

      private$DRIVER$export_tdb(key, namespace = namespace, dest_driver = dest_driver)
    }

  ),

  active = list(

    #'@field timestamp Set or retrieve a `TileDB` timestamp range that
    #'  the resource will be opened at. Effective in `"READ"` mode only.
    #'
    timestamp = function(value) {

      if (!missing(value)) {
        private$DRIVER$timestamp <- value
      } else {
        private$DRIVER$timestamp
      }

    }
  ),

  private = list(

    # @field driver The TileDB driver.
    #
    DRIVER = NULL,

  # NOTE: extracted from storr:::check_length
  check_length = function(key, namespace) {

    n_key <- length(key)
    n_namespace <- length(namespace)
    if (n_key == n_namespace || n_namespace == 1) {
      n_key
    }
    else if (n_key == 1) {
      n_namespace
    }
    else {
      stop("Incompatible lengths for key and namespace", call. = FALSE)
    }
  },

  check_input = function(x, n, type = NULL) {
    name <- deparse(substitute(x))

    switch (type, 'character' = {
      if (isFALSE(.is_character(x))) {
        stop(sprintf("'%s' should be a character string, not %s", name, class(x)),
             call. = FALSE)
      }
    }, 'datetime' = {
      if (isFALSE(inherits(x, "POSIXct"))) {
        stop(sprintf("'%s' should be a date-time object, not %s", name, class(x)),
             call. = FALSE)
      }
    })

    if (length(x) != n) {
      stop(sprintf("'%s' must have %d elements (recieved %d)", name, n, length(x)),
           call. = FALSE)
    }
  }

  )
)
