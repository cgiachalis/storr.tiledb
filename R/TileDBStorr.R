
#' @title Generate a `TileDBStorr` Object
#'
#' @description An R6 class that represents a storr interface for TileDB
#' driver.
#'
#' `TileDBStorr` replicates the `storr` interface but also enhances
#' it with additional new features:
#'  - notes and key expiration timestamps
#'  - asynchronous writes
#'
#' Note that the following methods from `storr` are not supported by
#' `TileDBStorr`: `$archive_import`, `$archive_export`,
#' `$check` and `$repair`.
#'
#' This class is not intended to be used directly and the preferred
#' usage is through [storr_tiledb()].
#'
#' @returns A `TileDBStorr`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
TileDBStorr <- R6::R6Class(
  classname = "TileDBStorr",
  cloneable = FALSE,

  public = list(

    #' @field envir The object hash table.
    #'
    envir = NULL,

    #' @field envir_metadata The key metadata hash table.
    #'
    envir_metadata = NULL,

    #' @field default_namespace The default namespace.
    #'
    default_namespace = NULL,

    #' @field traits Driver traits.
    #'
    traits = NULL,

    #' @field hash_raw The hash function.
    #'
    hash_raw = NULL,

    #' @field serialize_object The serialisation function.
    #'
    serialize_object = NULL,

    #' @description Initialise `TileDBStorr`.
    #'
    #' @param driver A TileDB driver, see [driver_tiledb()].
    #' @param default_namespace The default namespace.
    #' @param async Should the [mirai] daemons be enabled for async
    #'  functions? Default is  `FALSE`.
    #'
    initialize = function(driver, default_namespace, async = FALSE) {

      if (!inherits(driver, "TileDBDriver")) {
        stop("Not a valid TileDB 'driver'. Please use a 'TileDBDriver' object.",
             call. = FALSE)
      }

      private$check_input(default_namespace, n = 1, type = "character")

      if (async) {
        private$set_daemons()
      }

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

      # Key-value: <'hash', R object>
      self$envir <- hashtab()

      # Key-value: <'key:namespace', list(expires_at, notes)>
      self$envir_metadata <- hashtab()

      self$default_namespace <- default_namespace
      self$traits <- storr_traits(driver$traits)

      self$hash_raw <- make_hash_serialized_object(driver$hash_algorithm,
                                           !self$traits$drop_r_version)
      self$serialize_object <- make_serialize_object(self$traits$drop_r_version,
                                     self$traits$accept == "string")
    },

    #' @description Destroy (delete) 'storr'.
    #'
    #' @return `NULL`, invisibly.
    #'
    destroy = function() {

      private$DRIVER$destroy()
      private$DRIVER <- NULL

      invisible(NULL)
    },

    #' @description Flush the cache of `R` objects.
    #'
    #' It removes all items from the hash tables (R objects and
    #' their metadata).
    #'
    #' @return The object, invisibly.
    #'
    flush_cache = function() {
      clrhash(self$envir)
      clrhash(self$envir_metadata)

      invisible(self)
    },

    #' @description Set a key value pair.
    #'
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The hash value, invisibly.
    #'
    set = function(key,
                   value,
                   namespace = self$default_namespace,
                   expires_at,
                   notes,
                   use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(NA_real_)
      }

      if (missing(notes)) {
        notes <- NA_character_
      }

      private$check_input(notes, n = 1, type = "character")
      private$check_input(expires_at, n = 1, type = "datetime")

      hash <- self$set_value(value, use_cache)
      private$DRIVER$set_hash(key, namespace, hash, expires_at, notes)

      km <- paste(key, namespace, sep = ":")

      if (use_cache) {
        sethash(self$envir_metadata, km, list(expires_at = expires_at,
                                              notes = notes))
      } else {
        # always remove key metadata when use_cache = FALSE
        # otherwise, when calling get_keymeta from cache
        # will retrieve the old value
        remhash(self$envir_metadata, km)
      }

      invisible(hash)
    },

    #' @description Set multiple key value pairs.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param value `r sto_value(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache`
    #'
    #' @return A vector of hash values, invisibly.
    #'
    mset = function(key,
                    value,
                    namespace = self$default_namespace,
                    expires_at,
                    notes,
                    use_cache = getOption("storr.tiledb.cache", TRUE)) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n
      key <- p$key
      namespace <- p$namespace

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(rep_len(NA, n))
      }

      if (missing(notes)) {
        notes <- rep_len(NA_character_, n)
      }

      private$check_input(notes, n, "character")
      private$check_input(expires_at, n, "datetime")
      private$check_input(value, n, "value")

      hash <- self$mset_value(value, use_cache)
      private$DRIVER$mset_hash(key, namespace, hash, expires_at, notes)

      km <- paste(key, namespace, sep = ":")

      if (use_cache) {

        for(i in seq_along(km)) {
          sethash(self$envir_metadata, km[i], list(expires_at = expires_at[i],
                                                   notes = notes[i]))
        }
      } else {
        # ensure cache for km pairs is removed.
        # See comments in set_keymeta

        for(i in seq_along(km)) {
          remhash(self$envir_metadata, km[i])
        }
      }

      invisible(hash)
    },


    #' @description Set a key value pair asynchronously.
    #'
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache`
    #' @param cfg `r sto_cfg`
    #'
    #' @return Invisibly, a named list with two elements:
    #'
    #'  - `mirai`: a named list of two [mirai()] objects, `obj` and `key`;
    #'  `obj` refers to object table and `key` to key table. Both return
    #'  logical `TRUE` if an evaluation is successful.
    #'  - `hash`: the hash value
    #'
    set_async = function(key,
                         value,
                         namespace = self$default_namespace,
                         expires_at,
                         notes,
                         use_cache = getOption("storr.tiledb.cache", TRUE),
                         cfg = NULL) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(NA_real_)
      }

      if (missing(notes)) {
        notes <- NA_character_
      }

      private$check_input(notes, n = 1, type = "character")
      private$check_input(expires_at, n = 1, type = "datetime")

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      # Step 1: store and cache object if needed
      m1 <- TRUE
      if (!(use_cache && exists0(hash, self$envir))) {

        uri <- private$DRIVER$uri

        m1 <- mirai::mirai({
          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          # Store object if needed
          if (!driver$exists_object(hash)) {
            driver$set_object(hash, value_ser)
          }

        }, uri = uri, hash = hash, value_ser = value_ser, .compute = ns)

        # Cache value using its hash
        if (use_cache) {
          sethash(self$envir, hash, value)
        }
      }

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$set_hash(key, namespace, hash, expires_at, notes)
      }, uri = uri, key = key, namespace = namespace, hash = hash,
      expires_at = expires_at, notes = notes, .compute = ns)


      km <- paste(key, namespace, sep = ":")

      if (use_cache) {
        sethash(self$envir_metadata, km, list(expires_at = expires_at,
                                              notes = notes))
      } else {
        # always remove key metadata when use_cache = FALSE
        # otherwise, when calling get_keymeta from cache
        # will retrieve the old value
        remhash(self$envir_metadata, km)
      }

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))

    },

    #' @description Set multiple key value pairs asynchronously.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param value `r sto_value(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache`
    #' @param cfg `r sto_cfg`
    #'
    #' @return Invisibly, a named list with two elements:
    #'
    #'  - `mirai`: a named list of two [mirai()] objects, `obj` and `key`;
    #'  `obj` refers to object table and `key` to key table. Both return
    #'  logical `TRUE` if an evaluation is successful.
    #'  - `hash`: a vector with hash values
    #'
    mset_async = function(key,
                          value,
                          namespace = self$default_namespace,
                          expires_at,
                          notes,
                          use_cache = getOption("storr.tiledb.cache", TRUE),
                          cfg = NULL) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n


      if (missing(expires_at)) {
        expires_at <- as.POSIXct(rep_len(NA, n))
      }

      if (missing(notes)) {
        notes <- rep_len(NA_character_, n)
      }

      private$check_input(notes, n, "character")
      private$check_input(expires_at, n, "datetime")
      private$check_input(value, n, "value")


      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      # mirai namespace compute profile
      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      }, config_params = as.vector(cfg), .compute = ns)


      # START: 'mset_value' logic for async ---

      values_ser <- lapply(value, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)
      cached <- logical(length(hash))

      envir <- self$envir
      uri <- private$DRIVER$uri

      # Step 1: store and cache object if needed
      m1 <- TRUE

      if (use_cache) {

        cached <- exists0(hash, envir)

        m1 <- mirai::mirai({

          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          upload <- logical(length(hash))
          upload[!cached] <- !driver$exists_object(hash[!cached])

          if (any(upload)) {
            driver$mset_object(hash[upload], values_ser[upload])
          }

        }, uri = uri, hash = hash, values_ser = values_ser, cached = cached, .compute = ns)


      } else {

        m1 <- mirai::mirai({

          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          upload <- !driver$exists_object(hash)

          if (any(upload)) {
            driver$mset_object(hash[upload], values_ser[upload])
          }

        }, uri = uri, hash = hash, values_ser = values_ser, cached = cached, .compute = ns)
      }

      if (use_cache) {
        for (i in which(!cached)) {
          sethash(self$envir, hash[[i]], value[[i]])
        }
      }

      # END: 'mset_value' logic for async ---

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$mset_hash(key, namespace, hash, expires_at, notes)

      },
      uri = uri,
      key = key,
      namespace = namespace,
      hash = hash,
      expires_at = expires_at,
      notes = notes,
      .compute = ns)

      km <- paste(p$key, p$namespace, sep = ":")

      if (use_cache) {

        for(i in seq_along(km)) {
          sethash(self$envir_metadata, km[i], list(expires_at = expires_at[i],
                                                   notes = notes[i]))
        }
      } else {
        # ensure cache for km pairs are removed.
        # See comments in set_keymeta

        for(i in seq_along(km)) {
          remhash(self$envir_metadata, km[i])
        }
      }

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))
    },

    #' @description Set a key value pair using its hash as key.
    #'
    #'
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The hash value, invisibly.
    #'
    set_by_value = function(value,
                            namespace = self$default_namespace,
                            expires_at,
                            notes,
                            use_cache = getOption("storr.tiledb.cache", TRUE)) {

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(NA_real_)
      }

      if (missing(notes)) {
        notes <- NA_character_
      }

      private$check_input(namespace, n = 1, type = "character")
      private$check_input(notes, n = 1, type = "character")
      private$check_input(expires_at, n = 1, type = "datetime")

      hash <- self$set_value(value, use_cache)
      private$DRIVER$set_hash(hash, namespace, hash, expires_at, notes)

      km <- paste(hash, namespace, sep = ":")

      if (use_cache) {
        sethash(self$envir_metadata, km, list(expires_at = expires_at,
                                              notes = notes))
      } else {
        remhash(self$envir_metadata, km)
      }

      invisible(hash)
    },

    #' @description Set multiple key value pairs using their
    #'  hashes as keys.
    #'
    #'
    #' @param value `r sto_value(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache`
    #'
    #' @return A vector of hash values, invisibly.
    #'
    mset_by_value = function(value,
                             namespace = self$default_namespace,
                             expires_at,
                             notes,
                             use_cache = getOption("storr.tiledb.cache", TRUE)) {

      # TODO: review length and km recycling..
      n <- length(value)

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(rep_len(NA, n))
      }

      if (missing(notes)) {
        notes <- rep_len(NA_character_, n)
      }

      private$check_input(notes, n, "character")
      private$check_input(expires_at, n, "datetime")
      private$check_input(namespace, n, "value")

      hash <- self$mset_value(value, use_cache)
      private$DRIVER$mset_hash(hash, namespace, hash, expires_at, notes)

      km <- paste(rep_len(hash, n), rep_len(namespace, n), sep = ":")
      if (use_cache) {
        for (i in seq_along(km)) {
          sethash(self$envir_metadata,
                  km[i],
                  list(expires_at = expires_at[i], notes = notes[i]))
        }
      } else {
        for (i in seq_along(km)) {
          remhash(self$envir_metadata, km[i])
        }
      }

      invisible(hash)
    },

    #' @description Set a key value pair using its hash as key,
    #' asynchronously.
    #'
    #'
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache`
    #' @param cfg `r sto_cfg`
    #'
    #' @return Invisibly, a named list with two elements:
    #'
    #'  - `mirai`: a named list of two [mirai()] objects, `obj` and `key`;
    #'  `obj` refers to object table and `key` to key table. Both return
    #'  logical `TRUE` if an evaluation is successful.
    #'  - `hash`: the hash value
    #'
    set_by_value_async = function(value,
                                  namespace = self$default_namespace,
                                  expires_at,
                                  notes,
                                  use_cache = getOption("storr.tiledb.cache", TRUE),
                                  cfg = NULL) {

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(NA_real_)
      }

      if (missing(notes)) {
        notes <- NA_character_
      }

      private$check_input(namespace, n = 1, type = "character")
      private$check_input(notes, n = 1, type = "character")
      private$check_input(expires_at, n = 1, type = "datetime")

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      # Step 1: store and cache object if needed
      m1 <- TRUE
      if (!(use_cache && exists0(hash, self$envir))) {

        uri <- private$DRIVER$uri

        m1 <- mirai::mirai({
          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          # Store object if needed
          if (!driver$exists_object(hash)) {
            driver$set_object(hash, value_ser)
          }

        }, uri = uri, hash = hash, value_ser = value_ser, .compute = ns)

        # Cache value using its hash
        if (use_cache) {
          sethash(self$envir, hash, value)
        }
      }

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$set_hash(hash, namespace, hash, expires_at, notes)
      }, uri = uri, namespace = namespace, hash = hash,
      expires_at = expires_at, notes = notes, .compute = ns)


      km <- paste(hash, namespace, sep = ":")

      if (use_cache) {
        sethash(self$envir_metadata, km, list(expires_at = expires_at,
                                              notes = notes))
      } else {
        # always remove key metadata when use_cache = FALSE
        # otherwise, when calling get_keymeta from cache
        # will retrieve the old value
        remhash(self$envir_metadata, km)
      }

      #m1[]

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))

    },

    #' @description Set multiple key value pairs using their
    #'  hashes as keys, asynchronously.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param value `r sto_value(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache`
    #' @param cfg `r sto_cfg`
    #'
    #' @return Invisibly, a named list with two elements:
    #'
    #'  - `mirai`: a named list of two [mirai()] objects, `obj` and `key`;
    #'  `obj` refers to object table and `key` to key table. Both return
    #'  logical `TRUE` if an evaluation is successful.
    #'  - `hash`: a vector with hash values
    #'
    mset_by_value_async = function(value,
                                   namespace = self$default_namespace,
                                   expires_at,
                                   notes,
                                   use_cache = getOption("storr.tiledb.cache", TRUE),
                                   cfg = NULL) {

      # TODO: review length and km recycling..
      n <- length(value)

      if (missing(expires_at)) {
        expires_at <- as.POSIXct(rep_len(NA, n))
      }

      if (missing(notes)) {
        notes <- rep_len(NA_character_, n)
      }

      private$check_input(notes, n, "character")
      private$check_input(expires_at, n, "datetime")
      private$check_input(namespace, n, "character")

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      # mirai namespace compute profile
      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      }, config_params = as.vector(cfg), .compute = ns)


      # START: 'mset_value' logic for async ---

      values_ser <- lapply(value, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)
      cached <- logical(length(hash))

      envir <- self$envir
      uri <- private$DRIVER$uri

      # Step 1: store and cache object if needed
      m1 <- TRUE

      if (use_cache) {

        cached <- exists0(hash, envir)

        m1 <- mirai::mirai({

          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          upload <- logical(length(hash))
          upload[!cached] <- !driver$exists_object(hash[!cached])

          if (any(upload)) {
            driver$mset_object(hash[upload], values_ser[upload])
          }

        },
        uri = uri,
        hash = hash,
        values_ser = values_ser,
        cached = cached,
        .compute = ns)


      } else {

        m1 <- mirai::mirai({

          driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

          upload <- !driver$exists_object(hash)

          if (any(upload)) {
            driver$mset_object(hash[upload], values_ser[upload])
          }

        },
        uri = uri,
        hash = hash,
        values_ser = values_ser,
        .compute = ns)
      }

      if (use_cache) {
        for (i in which(!cached)) {
          sethash(self$envir, hash[[i]], value[[i]])
        }
      }

      # END: 'mset_value' logic for async ---

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$mset_hash(hash, namespace, hash, expires_at, notes)

      },
      uri = uri,
      namespace = namespace,
      hash = hash,
      expires_at = expires_at,
      notes = notes,
      .compute = ns)

      km <- paste(hash, namespace, sep = ":")

      if (use_cache) {

        for(i in seq_along(km)) {
          sethash(self$envir_metadata, km[i], list(expires_at = expires_at[i],
                                                   notes = notes[i]))
        }
      } else {
        # ensure cache for km pairs are removed.
        # See comments in set_keymeta

        for(i in seq_along(km)) {
          remhash(self$envir_metadata, km[i])
        }
      }

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))
    },

    #' @description Add an R object. The key will be the hash value
    #' of the object.
    #'
    #'
    #' @param value `r sto_value()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The hash value, invisibly.
    #'
    set_value = function(value, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      if (!(use_cache && exists0(hash, self$envir))) {

        if (!private$DRIVER$exists_object(hash)) {
          private$DRIVER$set_object(hash, value_ser)
        }

        if (use_cache) {
          sethash(self$envir, hash, value)
        }
      }
      invisible(hash)
    },

    #' @description Add a vector of R objects. The keys will be the hash
    #'  values of the objects.
    #'
    #'
    #' @param values `r sto_value(1)`
    #' @param use_cache `r sto_cache`
    #'
    #' @return A vector of hash values, invisibly.
    #'
    mset_value = function(values, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      values_ser <- lapply(values, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)
      cached <- logical(length(hash))

      envir <- self$envir

      if (use_cache) {
        cached <- exists0(hash, envir) # vlapply(hash, exists0, self$envir)
        upload <- logical(length(hash))
        upload[!cached] <- !private$DRIVER$exists_object(hash[!cached])
      } else {
        upload <- !private$DRIVER$exists_object(hash)
      }

      if (any(upload)) {
        # TODO: NO NEED
        send <- if (self$traits$accept == "object") values else values_ser

        private$DRIVER$mset_object(hash[upload], send[upload])
      }

      if (use_cache) {
        for (i in which(!cached)) {
          sethash(self$envir, hash[[i]], values[[i]])
        }
      }
      invisible(hash)
    },

    #' @description Get an object given a key-namespace pair.
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The `R` object if available.
    #'
    get = function(key, namespace = self$default_namespace, use_cache = getOption("storr.tiledb.cache", TRUE)) {
      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")
      self$get_value(self$get_hash(key, namespace), use_cache)

    },

    #' @description Get multiple objects.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache`
    #' @param missing Value to use for missing elements.
    #'
    #' @return A list of `R` objects.
    #'
    mget = function(key, namespace = self$default_namespace, use_cache = getOption("storr.tiledb.cache", TRUE),
                    missing = NULL) {

      # NB: storr::join_key_namespace check is performed inside $query_keys0
      self$mget_value(self$mget_hash(key, namespace), use_cache, missing)
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
        tryCatch(private$DRIVER$get_hash(key, namespace), error = function(e) stop(KeyError(key,
                                                                                         namespace)))
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

    #' @description Create a hash digest for an R object.
    #'
    #' @param object An R object.
    #'
    #' @return A character string of a fixed length containing the
    #'  requested digest (hash) of the supplied R object.
    #'
    hash_object = function(object) {
      self$hash_raw(self$serialize_object(object))
    },

    #' @description Get an object given its hash.
    #'
    #'
    #' @param hash The hash value of the object.
    #' @param use_cache `r sto_cache`
    #'
    #' @return The `R` object if available.
    #'
    get_value = function(hash, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      envir <- self$envir

      if (use_cache && exists0(hash, envir)) {
        value <- envir[[hash]]
      } else {
        # TODO: no need for traits
        if (self$traits$throw_missing) {
          value <- tryCatch(private$DRIVER$get_object(hash),
                            error = function(e) stop(HashError(hash)))
        } else {
          if (!private$DRIVER$exists_object(hash)) {
            stop(HashError(hash))
          }
          value <- private$DRIVER$get_object(hash)
        }
        if (use_cache) {
          envir[[hash]] <- value
          # TODO: USE sethash(envir, hash, value)
        }
      }
      value
    },

    #' @description Get multiple objects given their hashes.
    #'
    #'
    #' @param hash A vector of hash values."
    #' @param use_cache `r sto_cache`
    #' @param missing Value to use for missing elements.
    #'
    #' @return A list of `R` objects.
    #'
    mget_value = function(hash, use_cache = getOption("storr.tiledb.cache", TRUE), missing = NULL) {

      envir <- self$envir
      value <- vector("list", length(hash))
      cached <- logical(length(hash))
      is_missing <- is.na(hash)

      if (use_cache) {
        i <- exists0(hash, envir)
        value[i] <- lapply(hash[i], function(h) envir[[h]])
        cached[i] <- TRUE
      }

      cached[is_missing] <- TRUE
      value[is_missing] <- list(missing)

      if (any(!cached)) {
        # TODO: REMOVE IS.NULL
        if (is.null(private$DRIVER$mget_object)) {
          value[!cached] <- lapply(hash[!cached], self$get_value, FALSE)
        } else {
          value[!cached] <- private$DRIVER$mget_object(hash[!cached])
        }

        if (use_cache) {
          for (i in which(!cached)) {
            sethash(envir, hash[[i]], value[[i]])
          }
        }
      }

      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing)
      }
      value
    },

    #' @description Set key metadata.
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache_meta`
    #'
    #'
    #' @return The `key:namespace` string, invisibly. If both arguments
    #' `"expires_at"` and `"notes"` are missing, then nothing is set and
    #'  a zero length character vector is returned.
    #'
    set_keymeta = function(key,
                           namespace = self$default_namespace,
                           expires_at,
                           notes,
                           use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      if (missing(expires_at)) {
        expires_at <- NULL
      } else {
        private$check_input(expires_at, n = 1, type = "datetime")
      }

      if (missing(notes)) {
        notes <- NULL
      } else {
        private$check_input(notes, n = 1, type = "character")
      }

      if (is.null(notes) && is.null(expires_at)) {
        return(invisible(character()))
      }

      private$DRIVER$set_keymeta(key, namespace, expires_at, notes)

      km <- paste(key, namespace, sep = ":")

      if (use_cache) {

        # Update what has changed
        val <- gethash(self$envir_metadata, km)

        if (is.null(val)) {
          val <- list(expires_at = as.POSIXct(NA),
                      notes = NA_character_)
        }

        if(!is.null(expires_at)) {
          val[[1]] <- expires_at
        }
        if(!is.null(notes)) {
          val[[2]] <- notes
        }

        sethash(self$envir_metadata, km, val)
      } else {
        # always remove key when use_cache = FALSE
        # otherwise, when calling get_keymeta from cache
        # will retrieve the old value
        remhash(self$envir_metadata, km)
      }

      invisible(km)
    },

    #' @description Set multiple key metadata.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache_meta`
    #'
    #'
    #' @return The `key:namespace` character vector of the recycled length,
    #' invisibly. If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    #'
    mset_keymeta = function(key,
                            namespace = self$default_namespace,
                            expires_at,
                            notes,
                            use_cache = getOption("storr.tiledb.cache", TRUE)) {

      p <-  storr::join_key_namespace(key, namespace)
      n <- p$n

      if (missing(expires_at)) {
        expires_at <- NULL
      } else {
        private$check_input(expires_at, n, "datetime")
      }

      if (missing(notes)) {
        notes <- NULL
      } else {
        private$check_input(notes, n, "character")
      }

      if (is.null(notes) && is.null(expires_at)) {
        return(invisible(character()))
      }

      private$DRIVER$mset_keymeta(p$key, p$namespace, expires_at, notes)
      km <- paste(p$key, p$namespace, sep = ":")

      if (use_cache) {

        lapply(seq_along(km), function(i) {

          # Update what has changed
          val <- gethash(self$envir_metadata, km[i])

          if (is.null(val)) {
            val <- list(expires_at = as.POSIXct(NA),
                        notes = NA_character_)
          }

          if(!is.null(expires_at)) {
            val[[1]] <- expires_at[i]
          }
          if(!is.null(notes)) {
            val[[2]] <- notes[i]
          }

          sethash(self$envir_metadata, km[i], val)

        })
      } else{
        # ensure cache for km pairs is removed.
        # See comments in set_keymeta
        lapply(seq_along(km), function(i) {
           remhash(self$envir_metadata, km[i])
        })
      }

      invisible(km)
    },

    #' @description Set key metadata asynchronously.
    #'
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #' @param expires_at `r sto_expires()`
    #' @param notes `r sto_notes()`
    #' @param use_cache `r sto_cache_meta`
    #' @param cfg `r sto_cfg`
    #'
    #'
    #' @return A named list with two elements (invisibly):
    #'
    #'  - `mirai`: a mirai object
    #'  - `keyns`: The `key:namespace` string
    #'
    #' If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    #'
    set_keymeta_async = function(key,
                                 namespace = self$default_namespace,
                                 expires_at,
                                 notes,
                                 use_cache = getOption("storr.tiledb.cache", TRUE),
                                 cfg = NULL) {


      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      # Perform early check
      if (!self$exists(key, namespace)) {
        stop(KeyError(key, namespace))
      }

      if (missing(expires_at)) {
        expires_at <- NULL
      } else {
        private$check_input(expires_at, n = 1, type = "datetime")
      }

      if (missing(notes)) {
        notes <- NULL
      } else {
        private$check_input(notes, n = 1, type = "character")
      }

      if (is.null(notes) && is.null(expires_at)) {
        return(invisible(character()))
      }

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)


      uri <- private$DRIVER$uri

      m1 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)
        driver$set_keymeta(key, namespace, expires_at, notes)
        },
      uri = uri,
      key = key,
      namespace = namespace,
      expires_at = expires_at,
      notes = notes,
      .compute = ns)

      km <- paste(key, namespace, sep = ":")

      if (use_cache) {

        # Update what has changed
        val <- gethash(self$envir_metadata, km)

        if (is.null(val)) {
          val <- list(expires_at = as.POSIXct(NA),
                      notes = NA_character_)
        }

        if(!is.null(expires_at)) {
          val[[1]] <- expires_at
        }
        if(!is.null(notes)) {
          val[[2]] <- notes
        }

        sethash(self$envir_metadata, km, val)
      } else {
        # always remove key when use_cache = FALSE
        # otherwise, when calling get_keymeta from cache
        # will retrieve the old value
        remhash(self$envir_metadata, km)
      }

      invisible(list(mirai = m1,
                     keyns = km))
    },

    #' @description Set multiple key metadata.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param expires_at `r sto_expires(1)`
    #' @param notes `r sto_notes(1)`
    #' @param use_cache `r sto_cache_meta`
    #' @param cfg `r sto_cfg`
    #'
    #' @return A named list with two elements (invisibly):
    #'
    #'  - `mirai`: a mirai object
    #'  - `keyns`: The `key:namespace` character vector of the recycled length
    #'
    #' If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    mset_keymeta_async = function(key,
                                 namespace = self$default_namespace,
                                 expires_at,
                                 notes,
                                 use_cache = getOption("storr.tiledb.cache", TRUE),
                                 cfg = NULL) {

      p <-  storr::join_key_namespace(key, namespace)
      n <- p$n

      # Perform early check
      status <- !self$exists(p$key, p$namespace)

      if (any(status)) {
        stop(KeyError(paste(p$key[status], collapse = ","),
                      paste(p$namespace[status], collapse = ",")))
      }

      if (missing(expires_at)) {
        expires_at <- NULL
      } else {
        private$check_input(expires_at, n, "datetime")
      }

      if (missing(notes)) {
        notes <- NULL
      } else {
        private$check_input(notes, n, "character")
      }

      if (is.null(notes) && is.null(expires_at)) {
        return(invisible(character()))
      }

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)


      uri <- private$DRIVER$uri

      m1 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)
        driver$mset_keymeta(key, namespace, expires_at, notes)
      },
      uri = uri,
      key = p$key,
      namespace = p$namespace,
      expires_at = expires_at,
      notes = notes,
      .compute = ns)

      km <- paste(p$key, p$namespace, sep = ":")

      if (use_cache) {

        lapply(seq_along(km), function(i) {

          # Update what has changed
          val <- gethash(self$envir_metadata, km[i])

          if (is.null(val)) {
            val <- list(expires_at = as.POSIXct(NA),
                        notes = NA_character_)
          }

          if(!is.null(expires_at)) {
            val[[1]] <- expires_at[i]
          }
          if(!is.null(notes)) {
            val[[2]] <- notes[i]
          }

          sethash(self$envir_metadata, km[i], val)

        })
      } else{
        # ensure cache for km pairs is removed.
        # See comments in set_keymeta
        lapply(seq_along(km), function(i) {
          remhash(self$envir_metadata, km[i])
        })
      }

      invisible(list(mirai = m1,
                     keyns = km))
    },

    #' @description Get key's metadata.
    #'
    #'
    #' @param key The key name to get metadata values from.
    #' @param namespace The namespace to look the key within.
    #' @param use_cache Should it be retrieved from cache? Default is
    #'  `TRUE`.
    #'
    #' @return A named list with the key-metadata: `"expires_at"`
    #' and `"notes".`
    #'
    get_keymeta = function(key,
                           namespace = self$default_namespace,
                           use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      if (use_cache && exists0(keyns, envir)) {
        value <- gethash(envir, keyns)
      } else {
        value <- private$DRIVER$get_keymeta(key, namespace)

        if (use_cache) {
          sethash(envir, keyns, value)
        }
      }
      value
    },

    #' @description Get multiple key metadata.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param use_cache Should it be retrieved from cache? Default is
    #'  `TRUE`.
    #' @param missing Fill value for missing keys. Default is `NULL`.
    #'
    #' @return A list with key metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    #'
    mget_keymeta = function(key,
                            namespace = self$default_namespace,
                            use_cache = getOption("storr.tiledb.cache", TRUE),
                            missing = NULL) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n

      key <- p$key
      namespace <- p$namespace
      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      value <- vector("list", n)
      cached <- logical(n)

      if (use_cache) {
        cached <- exists0(keyns, envir)
        value[cached] <- lapply(keyns[cached], function(h) gethash(envir, h))
        num_cached <- sum(cached)
        not_cached <- !cached
        status_not_cached <- any(not_cached)
      } else {
        # Everything is TRUE, so go to find them in DB
        not_cached <- !cached
        status_not_cached <- TRUE
        num_cached <- 0L
      }

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

        # Fill cache if needed
        # Indices for not_cached but existent items

        if (use_cache) {
          # Truly missing key-namespace pairs
          is_missing <- keyns_not_cached %in% keyns_missing
          idx <- which(!is_missing)
          keyns_to_cache <- keyns_not_cached[idx]
          for (i in idx) {
            sethash(envir, keyns_to_cache[i], value[not_cached][[i]])
          }
        }
        # Truly missing key-namespace pairs
        is_missing <- keyns %in% keyns_missing
      }


      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing) #+ num_cached
      }
      value
    },

    #' @description Remove key metadata.
    #'
    #' This method is a convenient  wrapper around `set_keymeta()` and `mset_keymeta()`
    #' and sets the key metadata fields to `NA` values, i.e., `as.POSIXct(NA)` and
    #' `NA_character`.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache_meta`
    #'
    #' @return The `key:namespace` character vector of the recycled length,
    #' invisibly.
    #'
    clr_keymeta = function(key,
                           namespace = self$default_namespace,
                           use_cache = getOption("storr.tiledb.cache", TRUE)) {

      n <- private$check_length(key, namespace)

      if (n > 1) {

        self$mset_keymeta(key,
                          namespace = namespace,
                          notes = rep(NA_character_, n),
                          expires_at = rep(as.POSIXct(NA), n),
                          use_cache = use_cache)

      } else {

        self$set_keymeta(key,
                         namespace = namespace,
                         notes = NA_character_,
                         expires_at = as.POSIXct(NA),
                         use_cache = use_cache)
      }

    },

    #' @description Remove key metadata asynchronously.
    #'
    #' This method is a convenient  wrapper around `set_keymeta_async()` and `mset_keymeta_async()`
    #' and sets the key metadata fields to `NA` values, i.e., `as.POSIXct(NA)` and
    #' `NA_character`.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache_meta`
    #' @param cfg `r sto_cfg`
    #'
    #' @return A named list with two elements (invisibly):
    #'
    #'  - `mirai`: a mirai object
    #'  - `keyns`: The `key:namespace` character vector of the recycled length
    #'
    clr_keymeta_async = function(key,
                           namespace = self$default_namespace,
                           use_cache = getOption("storr.tiledb.cache", TRUE),
                           cfg = NULL) {

      n <- private$check_length(key, namespace)

      if (n > 1) {

        self$mset_keymeta_async(key,
                                namespace = namespace,
                                notes = rep(NA_character_, n),
                                expires_at = rep(as.POSIXct(NA), n),
                                use_cache = use_cache,
                                cfg = cfg)

      } else {

        self$set_keymeta_async(key,
                               namespace = namespace,
                               notes = NA_character_,
                               expires_at = as.POSIXct(NA),
                               use_cache = use_cache,
                               cfg = cfg)
      }
    },

    #' @description Set one or more keys to the same value.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache`
    #'
    #' @return A vector of hash values, invisibly.
    #'
    fill = function(key, value, namespace = self$default_namespace,
                    use_cache = getOption("storr.tiledb.cache", TRUE)) {

      p <- storr::join_key_namespace(key, namespace)

      hash <- self$set_value(value, use_cache = use_cache)
      private$DRIVER$mset_hash(p$key, p$namespace, rep(hash, p$n))
      invisible(hash)
    },

    #' @description Duplicate a set of keys.
    #'
    #' @param key_src A character vector of source keys.
    #' @param key_dest A character vector of destination keys.
    #' @param namespace The namespace to copy keys within (used only of
    #'  `namespace_src` and `namespace_dest` are not provided.
    #' @param namespace_src The source namespace - use this where keys are
    #'  duplicated across namespaces.
    #' @param namespace_dest  The destination namespace - use this where keys are duplicated
    #'  across namespaces.
    #'
    #' @return `NULL`, invisibly.
    #'
    duplicate = function(key_src,
                         key_dest,
                         namespace = self$default_namespace,
                         namespace_src = namespace,
                         namespace_dest = namespace) {

      hash_src <- self$mget_hash(key_src, namespace_src)
      private$DRIVER$mset_hash(key_dest, namespace_dest, hash_src)

      invisible(NULL)
    },

    # NB: storr reports back the number of deleted keys
    #' @description Clear a storr.
    #'
    #' @param namespace A scalar character of namespace name or `NULL` to
    #' clear all namespaces.
    #'
    #' @return The number of deleted namespaces.
    #'
    clear = function(namespace = self$default_namespace){

      if (!.is_character(namespace) & !is.null(namespace)) {

        stop(sprintf("'namespace' should be a character vector, not %s",
                     class(namespace)), call. = FALSE)
      }

      private$DRIVER$delete_namespaces(namespace)
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

    #' @description Delete an object from the storr.
    #'
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #'
    #' @return A logical vector indicating which key-namespace pair was
    #' deleted, invisibly.
    #'
    del = function(key, namespace = self$default_namespace) {

      n <- storr::join_key_namespace(key, namespace)

      deleted_hashes <- private$DRIVER$del_hash(n$key, n$namespace)

      # Remove cache metadata for primary index key:namespace
      #
      #  NB 1: We do it here instead when invoking gc() because on that
      # occasion we'll have to lookup again the key, namespace pairs. Since
      # we have deleted the hashes which correspond to key:namespace, their
      # cache can safely be removed; this is because when calling get_hash(),
      # it will always go to 'tbl_keys' and checks if the hash exists for the
      # key:namespace.
      #
      # NB 2: We cannot do the same for cached hashes as they might
      # be used by another key:namespace; but we do it in $gc() instead.
      #
      km <- paste(n$key, n$namespace, sep = ":")
      status <- vlapply(km, function(.k) {
        remhash(self$envir_metadata,key = .k)
      })

      invisible(deleted_hashes)
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

    #' @description Remove the expired key-namespace pairs.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #'
    #' @return A boolean value `TRUE` indicating success, invisibly.
    #'
    clear_expired_keys = function(namespace = self$default_namespace) {
      private$DRIVER$delete_expired_keys(namespace)
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

    #' @description Garbage collect the storr.
    #'
    #' @param clear_expired Should the expired keys be deleted?
    #' Default is `FALSE`.
    #'
    #' @return A vector of unused hashes, invisibly.
    #'
    gc = function(clear_expired = FALSE) {

      if (clear_expired) {
        private$DRIVER$delete_expired_keys(NULL)
      }

      # Deletes the objects in 'tbl_data'
      unused <- private$DRIVER$delete_unused_hashes()

      # Delete unused hashes from cache; note that metadata for
      # the respective key:namespaces have been deleted by
      # del() operation. See comments in $del().
      del <- vlapply(unused, function(.k) {
        remhash(self$envir, .k)
        })

      invisible(unused)
    },

    #' @description Import objects to storr.
    #'
    #'
    #' @param src A source to import objects from. It can be a storr, list, or environment.
    #' @param list Names of objects to import (or `NULL` for all objects) . If given it must be a character vector.
    #'  If named, the names of the character vector will be the names of the objects as created in the storr.
    #' @param namespace  Namespace to get objects from, and to put objects into.
    #' If `NULL`, all namespaces from `src` will be imported. If named,
    #' then the same rule is followed as `list`; `namespace = c(a = b)` will import the
    #' contents of namespace `b` as namespace `a`.
    #' @param skip_missing  Logical, indicating if missing keys (specified in `list`)
    #' should be skipped over, rather than being treated as an error (the default).
    #'
    #'
    #' @return A vector with destination namespaces, invisibly.
    #'
    import = function(src, list = NULL, namespace = self$default_namespace,
                      skip_missing = FALSE) {

      if (is.null(namespace)) {
        if (inherits(src, "storr")) {
          namespace <- src$list_namespaces()
        } else {
          stop("If src is not a storr, namespace can't be NULL")
        }
      }
      invisible(storr_copy(self, src, list, namespace, skip_missing)$info)
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

      invisible(storr_copy(dest, self, list, namespace, skip_missing)$dest)
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

    #' @description Import an index of objects from a storr.
    #'
    #'
    #' @param index A `data.frame` with minimum required columns 'namespace', 'key'
    #' 'hash' and optionally 'expires_at' and 'notes'. It is an error if not all
    #'  hashes are present in the storr.
    #'
    #' @return `TRUE`, invisibly.
    #'
    index_import = function(index) {

      cols <- c("namespace", "key", "hash")

      nms <- colnames(index)
      msg <- setdiff(cols, nms)
      if (length(msg) > 0L) {
        stop("Missing required columns for index: ", paste(squote(msg),
                                                           collapse = ", "), call. = FALSE)
      }

      ok <- vlapply(index[, c("namespace", "key", "hash")], is.character)
      if (!all(ok)) {
        stop("Column not character: ", paste(squote(cols[!ok]),
                                             collapse = ", "), call. = FALSE)
      }

      msg <- setdiff(index$hash, self$list_hashes())
      if (length(msg) > 0L) {
        stop(sprintf("Missing %d / %d hashes - can't import",
                     length(msg), nrow(index)), call. = FALSE)
      }


      if (all(c("expires_at", "notes") %in% nms)) {
        if (!inherits(index[["expires_at"]], "POSIXct")) {
          stop("Column not datetime: ", sQuote("expires_at"), call. = FALSE)
        }

        if (!is.character(index[["notes"]])) {
          stop("Column not character: ", sQuote("notes"), call. = FALSE)
        }

      }

      private$DRIVER$mset_hash(index$key, index$namespace, index$hash, index$expires_at, index$notes)
    }
  ),

  active = list(
   # stats or num_keys/num_namespaces/db_size/report

    #' @field async_info `mirai` information
    #'
    async_info = function(value) {

      if (!missing(value)) {
        cli::cli_abort(paste0(cli::style_italic("{.val {value}}"), " is a read-only field."), call = NULL)
      }

      mirai::info(.storr_profile)
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
  },

  # Set up persistent daemons for storr compute profile
  set_daemons = function() {
    if (!mirai::daemons_set(.storr_profile)) {
      enable_mirai()
    }
  },

  # Reset daemons for storr compute profile
  finalize = function() {
    if (mirai::daemons_set(.storr_profile)) {
      disable_mirai()
    }
  }

  )
)
