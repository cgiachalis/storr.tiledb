
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

      self$hash_raw <- make_hash_serialized_object(driver$hash_algorithm, !self$traits$drop_r_version)
      self$serialize_object <- make_serialize_object(self$traits$drop_r_version, self$traits$accept == "string")
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
    #' @details
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
    #' @details
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

      ns <- private$MIRAI_PROFILE

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
      if (!(use_cache && exists1(hash, self$envir))) {

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
    #' @details
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
      ns <- private$MIRAI_PROFILE

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
      p <- storr::join_key_namespace(value, namespace)
      n <- p$n
      namespace <- p$namespace

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

      ns <- private$MIRAI_PROFILE

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
      if (!(use_cache && exists1(hash, self$envir))) {

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
    #' @details
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
      ns <- private$MIRAI_PROFILE

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

    #' @description Add an R object without key.
    #'
    #' This is used internally.
    #'
    #' @param value `r sto_value()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The hash value, invisibly.
    #'
    set_value = function(value, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      if (!(use_cache && exists1(hash, self$envir))) {

        if (!private$DRIVER$exists_object(hash)) {
          private$DRIVER$set_object(hash, value_ser)
        }

        if (use_cache) {
          sethash(self$envir, hash, value)
        }
      }
      invisible(hash)
    },

    #' @description Add a vector of R objects.
    #'
    #' This is used internally.
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
        send <- if (self$traits$accept == "object") {
          values
        } else {
          values_ser
        }

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
      hash <- self$get_hash(key, namespace)
      self$get_value(hash, use_cache)

    },

    #' @description Get multiple objects.
    #'
    #' @details
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
      hash <- self$mget_hash(key, namespace)
      self$mget_value(hash, use_cache, missing)
    },

    #' @description Get hash value.
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
          stop(KeyError(key, namespace))
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
    #' @details
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
    #' @param hash The hash value of the object.
    #' @param use_cache `r sto_cache`
    #'
    #' @return The `R` object if available.
    #'
    get_value = function(hash, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      envir <- self$envir

      if (use_cache && exists1(hash, envir)) {
        value <- gethash(envir, hash)
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
          sethash(envir, hash, value)
        }
      }
      value
    },

    #' @description Get multiple objects given their hashes.
    #'
    #' @param hash A vector of hash values.
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
          value[!cached] <- private$DRIVER$mget_object(hash[!cached])
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

    #' @description Get an object and its metadata given a key-namespace pair.
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #' @param use_cache `r sto_cache`
    #'
    #' @return The `R` object and its key-metadata, if available.
    #'
    get_all = function(key, namespace = self$default_namespace, use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      out <- list(keyval = NULL, keymeta = NULL)

      # TODO: Explore better approach w/ not touching tbl_key twice

      # get
      hash <- self$get_hash(key, namespace)
      out$keyval <- self$get_value(hash, use_cache)

      # get_keymeta
      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      if (use_cache && exists1(keyns, envir)) {
        value <- gethash(envir, keyns)
      } else {
        value <- private$DRIVER$get_keymeta(key, namespace)

        if (use_cache) {
          sethash(envir, keyns, value)
        }
      }

      out$keymeta <- value

      out
    },

    #' @description Get multiple objects and their metadata.
    #'
    #' @details
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache`
    #' @param missing Value to use for missing elements.
    #'
    #' @return A list of `R` objects with their metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    mget_all = function(key, namespace = self$default_namespace, use_cache = getOption("storr.tiledb.cache", TRUE),
                        missing = NULL) {

      # NB: storr::join_key_namespace check is performed inside $query_keys0
      hash <- self$mget_hash(key, namespace)
      kv <- self$mget_value(hash, use_cache, missing)

      km <- self$mget_keymeta(key, namespace, use_cache = use_cache, missing = missing)

      mapply(kv, km,
        FUN = function(.k, .m) {
          if (is.null(.k)) {
            NULL
          } else {
            list(keyval = .k, keymeta = .m)
          }
        },
        SIMPLIFY = FALSE
      )
    },

    #' @description Update a key value pair.
    #'
    #' @details
    #'
    #' This method updates a key-namespace value while retaining
    #' its key-metadata. If a key is not found, it raises an error by default;
    #' otherwise, set `create` argument to  `TRUE` to set a new key and optionally
    #' add key metadata with `expires_at,notes` arguments.
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param create Should the key be created, if not found. Default is `FALSE`
    #' raising an `KeyError`. Otherwise, create a new key.
    #' @param expires_at,notes A scalar string of notes and/or a date-time
    #' object of class `POSIXct`(optional). Applies only if `create = TRUE`.
    #' @param use_cache `r sto_cache`
    #'
    #' @return The hash value, invisibly.
    #'
    update = function(key,
                      value,
                      namespace = self$default_namespace,
                      create = FALSE,
                      expires_at,
                      notes,
                      use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      dat <- private$DRIVER$filter_keys(key, namespace)

      if (nrow(dat) == 0) {
        if (isFALSE(create)) {
          stop(KeyError(key, namespace))
        } else {

          #  When 'create = TRUE' construct key-namespace index mapping and
          #  add key-metadata (optional)
          if (missing(expires_at)) {
            expires_at <- as.POSIXct(NA_real_)
          }

          if (missing(notes)) {
            notes <- NA_character_
          }

          private$check_input(notes, n = 1, type = "character")
          private$check_input(expires_at, n = 1, type = "datetime")


          dat <- data.table::as.data.table(
            list(
              namespace = namespace,
              key = key,
              hash = NA_character_,
              # Will be populated later
              expires_at = expires_at,
              notes = notes
            )
          )
        }
      }

      hash <- self$set_value(value, use_cache)

      # Update hash index only
      dat$hash <- hash
      private$DRIVER$mset_hash(dat$key,
                               dat$namespace,
                               dat$hash,
                               dat$expires_at,
                               dat$notes)

      # NB: Here, we need only to set metadata cache and not to remove it when
      # use_cache is FALSE, as it happens with $set() method; because the idea
      # of 'update' is to retain the key-metadata.

      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      if (use_cache && !exists1(keyns, envir)) {
        sethash(envir, keyns, list(expires_at = dat$expires_at,
                                   notes = dat$notes))
      }

      invisible(hash)

    },

    #' @description Update multiple key value pairs.
    #'
    #' @details
    #'
    #' This works similar to `$update` but for multiple key pairs and with
    #' more control about missing keys; use `fail_fast` to abort (default) or
    #' skip with warning.
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param create Should the key be created, if not found. Default is `FALSE`
    #' raising an `KeyError`. Otherwise, create a new key.
    #' @param fail_fast Should abort on missing keys, default is `TRUE`. Use
    #' `FALSE` for skipping keys and emit a warning for missing items. The
    #' argument has no effect when upsert is used via `create = TRUE`.
    #' @param expires_at,notes A scalar string of notes and/or a date-time
    #' object of class `POSIXct`(optional). Applies only if `create = TRUE`.
    #' @param use_cache `r sto_cache`
    #'
    #' @return A vector of hash values, invisibly.
    #'
    mupdate = function(key,
                       value,
                       namespace = self$default_namespace,
                       create = FALSE,
                       fail_fast = TRUE,
                       expires_at,
                       notes,
                       use_cache = getOption("storr.tiledb.cache", TRUE)) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n
      key <- p$key
      namespace <- p$namespace

      private$check_input(value, n, "value")

      dat <- private$DRIVER$filter_keys(key, namespace)

      # Check for no hash in given key:namespace
      data.table::setkeyv(dat, c("namespace", "key"))

      dat <- dat[.(namespace, key), env = list(namespace = I(namespace),
                                               key = I(key))][]
      hash_isna <- is.na(dat[["hash"]])


      if (any(hash_isna)) {
        # NB: Case 1 - key(s) missing, no key(s) creation and fail
        #     Case 2 - key(s) missing, no key(s) creation and no-fail but warn
        #     Case 3 - key(s) missing, key(s) creation
        if (isFALSE(create) && isTRUE(fail_fast)) {

          stop(KeyError(paste(dat$key[hash_isna], collapse = ","),
                        paste(dat$namespace[hash_isna], collapse = ",")))

        } else if (isFALSE(create) && isFALSE(fail_fast)) {

          cli::cli_warn("Skipping the following missing key indexes: {.val {which(hash_isna)}}")
          dat <- dat[!hash_isna]
          value <- value[!hash_isna]

        } else {

          #  When 'create = TRUE' construct key-namespace index mapping and
          #  add key-metadata (optional)

          num_no_hash <- sum(hash_isna)

          if (missing(expires_at)) {
            expires_at <- as.POSIXct(rep_len(NA, num_no_hash))
          } else {
            private$check_input(expires_at, n = 1, type = "datetime")
            expires_at <- rep_len(expires_at, num_no_hash)
          }

          if (missing(notes)) {
            notes <- rep_len(NA_character_, num_no_hash)
          } else {
            private$check_input(notes, n = 1, type = "character")
            notes <- rep_len(notes, num_no_hash)
          }

          dat$expires_at[hash_isna] <- expires_at
          dat$notes[hash_isna] <- notes

          dat <- data.table::as.data.table(
            list(
              namespace = dat$namespace,
              key = dat$key,
              hash = NA_character_, # Will be populated later
              expires_at = dat$expires_at,
              notes = dat$notes
            )
          )
        }
      }

      hash <- self$mset_value(value, use_cache)

      # Update hash index only
      dat$hash <- hash
      private$DRIVER$mset_hash(dat$key,
                               dat$namespace,
                               dat$hash,
                               dat$expires_at,
                               dat$notes)

      # NB: Here, we need only to set metadata cache and not to remove it when
      # use_cache is FALSE, as it happens with $set() method; because the idea
      # of 'update' is to retain the key-metadata.

      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      if (use_cache) {

        for(i in seq_along(keyns)) {
          if(!exists1(keyns[i], envir)) {
            sethash(envir, keyns[i], list(expires_at = dat$expires_at[i],
                                          notes = dat$notes[i]))
          }
        }

      }

      invisible(hash)

    },

    #' @description Update a key value pair asynchronously.
    #'
    #' @details
    #'
    #' This method updates a key-namespace value while retaining
    #' its key-metadata. If a key is not found, it raises an error by default;
    #' otherwise, set `create` argument to  `TRUE` to set a new key and optionally
    #' add key metadata with `expires_at,notes` arguments.
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param create Should the key be created, if not found. Default is `FALSE`
    #' raising an `KeyError`. Otherwise, create a new key.
    #' @param expires_at,notes A scalar string of notes and/or a date-time
    #' object of class `POSIXct`(optional). Applies only if `create = TRUE`.
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
    update_async = function(key,
                            value,
                            namespace = self$default_namespace,
                            create = FALSE,
                            expires_at,
                            notes,
                            use_cache = getOption("storr.tiledb.cache", TRUE),
                            cfg = NULL) {


      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      dat <- private$DRIVER$filter_keys(key, namespace)

      if (nrow(dat) == 0) {
        if (isFALSE(create)) {
          stop(KeyError(key, namespace))
        } else {

          #  When 'create = TRUE' construct key-namespace index mapping and
          #  add key-metadata (optional)
          if (missing(expires_at)) {
            expires_at <- as.POSIXct(NA_real_)
          }

          if (missing(notes)) {
            notes <- NA_character_
          }

          private$check_input(notes, n = 1, type = "character")
          private$check_input(expires_at, n = 1, type = "datetime")


          dat <- data.table::as.data.table(
            list(
              namespace = namespace,
              key = key,
              hash = NA_character_,
              # Will be populated later
              expires_at = expires_at,
              notes = notes
            )
          )
        }
      }

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      ns <- private$MIRAI_PROFILE

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      # Update hash index only
      dat$hash <- hash

      # Step 1: store and cache object if needed
      m1 <- TRUE

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

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$mset_hash(dat$key,
                         dat$namespace,
                         dat$hash,
                         dat$expires_at,
                         dat$notes)
      }, uri = uri, dat = dat, .compute = ns)


      # NB: Here, we need only to set metadata cache and not to remove it when
      # use_cache is FALSE, as it happens with $set() method; because the idea
      # of 'update' is to retain the key-metadata.

      if (use_cache) {

        # cache hash key
        sethash(self$envir, hash, value)

        keyns <- paste(key, namespace, sep = ":")
        envir <- self$envir_metadata

        # cache key-metadata if needed
        if (!exists1(keyns, envir)) {
          sethash(envir, keyns, list(expires_at = dat$expires_at,
                                     notes = dat$notes))
        }

      }

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))

    },

    #' @description Update multiple key value pairs asynchronously.
    #'
    #' @details
    #'
    #' This works similar to `$update` but for multiple key pairs and with
    #' more control about missing keys; use `fail_fast` to abort (default) or
    #' skip with warning.
    #'
    #' @param key `r sto_key()`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace()`
    #' @param create Should the key be created, if not found. Default is `FALSE`
    #' raising an `KeyError`. Otherwise, create a new key.
    #' @param fail_fast Should abort on missing keys, default is `TRUE`. Use
    #' `FALSE` for skipping keys and emit a warning for missing items. The
    #' argument has no effect when upsert is used via `create = TRUE`.
    #' @param expires_at,notes A scalar string of notes and/or a date-time
    #' object of class `POSIXct`(optional). Applies only if `create = TRUE`.
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
    mupdate_async = function(key,
                             value,
                             namespace = self$default_namespace,
                             create = FALSE,
                             fail_fast = TRUE,
                             expires_at,
                             notes,
                             use_cache = getOption("storr.tiledb.cache", TRUE),
                             cfg = NULL) {

      p <- storr::join_key_namespace(key, namespace)
      n <- p$n
      key <- p$key
      namespace <- p$namespace

      private$check_input(value, n, "value")

      dat <- private$DRIVER$filter_keys(key, namespace)

      # Check for no hash in given key:namespace
      data.table::setkeyv(dat, c("namespace", "key"))

      dat <- dat[.(namespace, key), env = list(namespace = I(namespace),
                                               key = I(key))][]
      hash_isna <- is.na(dat[["hash"]])


      if (any(hash_isna)) {
        # NB: Case 1 - key(s) missing, no key(s) creation and fail
        #     Case 2 - key(s) missing, no key(s) creation and no-fail but warn
        #     Case 3 - key(s) missing, key(s) creation
        if (isFALSE(create) && isTRUE(fail_fast)) {

          stop(KeyError(paste(dat$key[hash_isna], collapse = ","),
                        paste(dat$namespace[hash_isna], collapse = ",")))

        } else if (isFALSE(create) && isFALSE(fail_fast)) {

          cli::cli_warn("Skipping the following missing key indexes: {.val {which(hash_isna)}}")
          dat <- dat[!hash_isna]
          value <- value[!hash_isna]

        } else {

          #  When 'create = TRUE' construct key-namespace index mapping and
          #  add key-metadata (optional)

          num_no_hash <- sum(hash_isna)

          if (missing(expires_at)) {
            expires_at <- as.POSIXct(rep_len(NA, num_no_hash))
          } else {
            private$check_input(expires_at, n = 1, type = "datetime")
            expires_at <- rep_len(expires_at, num_no_hash)
          }

          if (missing(notes)) {
            notes <- rep_len(NA_character_, num_no_hash)
          } else {
            private$check_input(notes, n = 1, type = "character")
            notes <- rep_len(notes, num_no_hash)
          }

          dat$expires_at[hash_isna] <- expires_at
          dat$notes[hash_isna] <- notes

          dat <- data.table::as.data.table(
            list(
              namespace = dat$namespace,
              key = dat$key,
              hash = NA_character_, # Will be populated later
              expires_at = dat$expires_at,
              notes = dat$notes
            )
          )
        }
      }

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$DRIVER$ctx)
      }

      check_tiledb_config(cfg)

      # mirai namespace compute profile
      ns <- private$MIRAI_PROFILE

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      }, config_params = as.vector(cfg), .compute = ns)

      uri <- private$DRIVER$uri

      # START: 'mset_value' logic for async ---

      values_ser <- lapply(value, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)

      # Update hash index only
      dat$hash <- hash

      # Step 1: store and cache object if needed
      m1 <- TRUE

      m1 <- mirai::mirai({

        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        upload <- !driver$exists_object(hash)

        if (any(upload)) {
          driver$mset_object(hash[upload], values_ser[upload])
        }

      }, uri = uri, hash = hash, values_ser = values_ser, .compute = ns)


      # END: 'mset_value' logic for async ---

      # Step 2: set key:namespace data to key table, cache if needed
      m2 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)

        # Set info to keys table
        driver$mset_hash(dat$key, dat$namespace, dat$hash, dat$expires_at, dat$notes)

      }, uri = uri, dat = dat, .compute = ns)

      # NB: Here, we need only to set metadata cache and not to remove it when
      # use_cache is FALSE, as it happens with $set() method; because the idea
      # of 'update' is to retain the key-metadata.

      if (use_cache) {

        for (i in seq_along(hash)) {
          sethash(self$envir, hash[[i]], value[[i]])
        }

        keyns <- paste(key, namespace, sep = ":")
        envir <- self$envir_metadata

        for(i in seq_along(keyns)) {
          if(!exists1(keyns[i], envir)) {
            sethash(envir, keyns[i], list(expires_at = dat$expires_at[i],
                                          notes = dat$notes[i]))
          }
        }

      }

      invisible(list(mirai = list(obj = m1, key = m2), hash = hash))

    },

    #' @description Set key metadata.
    #'
    #' @details
    #' `r sto_keymeta_note`
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
    #' @details
    #' `r sto_keymeta_note`
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
    #' @details
    #' `r sto_keymeta_note`
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

      ns <- private$MIRAI_PROFILE

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
    #' @details
    #' `r sto_keymeta_note`
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

      ns <- private$MIRAI_PROFILE

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

      if (use_cache && exists1(keyns, envir)) {
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
    #' @details
    #' `r sto_recycle_note`
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param use_cache `r sto_cache`
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
        not_cached <- !cached
        status_not_cached <- any(not_cached)
      } else {
        # Everything is TRUE, so go to find them in DB
        not_cached <- !cached
        status_not_cached <- TRUE
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
          value_not_cached <- value[not_cached]
          for (i in idx) {
            sethash(envir, keyns_to_cache[i], value_not_cached[[i]])
          }
        }
        # Truly missing key-namespace pairs
        is_missing <- keyns %in% keyns_missing
      }


      if (any(is_missing)) {
        attr(value, "missing") <- which(is_missing)
      }
      value
    },

    #' @description Get key's expiration metadata.
    #'
    #' @details
    #'
    #' An efficient method compared to `$get_keymeta()` for fetching expiration
    #' values only.
    #'
    #' Note that `use_cache` will only fetch the metadata but not cache it if
    #' retrieved from database.
    #'
    #' @param key The key name to get metadata values from.
    #' @param namespace The namespace to look the key within.
    #' @param use_cache Should it be retrieved from cache? Default is
    #'  `TRUE`.
    #'
    #' @return A scalar key-metadata value.
    #'
    get_keymeta_expires_at = function(key,
                                      namespace = self$default_namespace,
                                      use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$keymeta_unit(key, namespace, use_cache, "expires_at")
    },

    #' @description Get key's notes metadata.
    #'
    #' @details
    #'
    #' An efficient method compared to `$get_keymeta()` for fetching notes
    #' values only.
    #'
    #' Note that `use_cache` will only fetch the metadata but not cache it if
    #' retrieved from database.
    #'
    #' @param key The key name to get metadata values from.
    #' @param namespace The namespace to look the key within.
    #' @param use_cache Should it be retrieved from cache? Default is
    #'  `TRUE`.
    #'
    #' @return A scalar key-metadata value.
    #'
    get_keymeta_notes = function(key,
                                 namespace = self$default_namespace,
                                 use_cache = getOption("storr.tiledb.cache", TRUE)) {

      private$keymeta_unit(key, namespace, use_cache, "notes")
    },

    #' @description Get expiration metadata for multiple keys.
    #'
    #' @details
    #'
    #' An efficient method compared to `$mget_keymeta()` for fetching expiration
    #' values only.
    #'
    #' `r sto_recycle_note`
    #'
    #'
    #' Note that `use_cache` will only fetch the metadata but not cache it if
    #' retrieved from database.
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param use_cache `r sto_cache`
    #' @param missing Fill value for missing keys. Default is `NULL`.
    #'
    #' @return A list with  expiration metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    #'
    mget_keymeta_expires_at = function(key,
                                       namespace = self$default_namespace,
                                       use_cache = getOption("storr.tiledb.cache", TRUE),
                                       missing = NULL) {

      private$multi_keymeta_unit(key, namespace, use_cache, missing, meta_col = "expires_at")

    },

    #' @description Get notes metadata for multiple keys.
    #'
    #' @details
    #'
    #' An efficient method compared to `$mget_keymeta()` for fetching notes
    #' values only.
    #'
    #' `r sto_recycle_note`
    #'
    #'
    #' Note that `use_cache` will only fetch the metadata but not cache it if
    #' retrieved from database.
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param use_cache `r sto_cache`
    #' @param missing Fill value for missing keys. Default is `NULL`.
    #'
    #' @return A list with notes metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    #'
    mget_keymeta_notes = function(key,
                                  namespace = self$default_namespace,
                                  use_cache = getOption("storr.tiledb.cache", TRUE),
                                  missing = NULL) {

      private$multi_keymeta_unit(key, namespace, use_cache, missing, meta_col = "notes")

    },

    #' @description Remove key metadata.
    #'
    #' @details
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
    clear_keymeta = function(key,
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
    #' @details
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
    clear_keymeta_async = function(key,
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
    #' @details
    #' `r sto_recycle_note`
    #'
    #' @param key `r sto_key(1)`
    #' @param value `r sto_value()`
    #' @param namespace `r sto_namespace(1)`
    #' @param use_cache `r sto_cache`
    #'
    #' @return A hash value, invisibly.
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
    #' @param namespace The namespace to copy keys within (used only when
    #'  `namespace_src` and `namespace_dest` are not provided).
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
    #' @details
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
    #' @details
    #'
    #' This will delete only the key-namespace pointers(s) and not the underlying
    #' data. Explicit use of `$gc()` is required to remove the actual object
    #' when its hash is not associated with any key-namespace pair.
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

    #' @description Check a key-namespace for expiration.
    #'
    #' @param key `r sto_key()`
    #' @param namespace `r sto_namespace()`
    #' @param use_cache `r sto_cache`
    #' @param check Check key-namespace pair exists before query about
    #' expiration (default). Set `FALSE` to skip check; on this occasion when
    #' a key-namespace not found, it returns `FALSE` - useful when `TRUE`value
    #' only matters to user.
    #'
    #' @return
    #'
    #'  - `TRUE` - key-namespace pair has expired
    #'  - `FALSE` - (a) key has not expired, (b) has not expiration time-stamp
    #'   or (c) `check = FALSE` and no key is found
    #'
    is_key_expired = function(key, namespace = self$default_namespace,
                              use_cache = getOption("storr.tiledb.cache", TRUE),
                              check = TRUE) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      if (check) {
        if (isFALSE(private$DRIVER$exists_hash(key, namespace))) {
          stop(KeyError(key, namespace))
        }
      }

      envir <- self$envir_metadata
      km <- paste(key, namespace, sep = ":")

      if (use_cache && exists1(km, envir)) {

        value <- gethash(envir, km)

        if (is.null(value$expires_at) || is.na(value$expires_at)) {
          is_expired <- FALSE
        } else {
          is_expired <- value$expires_at < Sys.time()
        }

      } else {
        is_expired <- private$DRIVER$is_key_expired(key, namespace)

      }

      is_expired
    },

    #' @description Remove the expired key-namespace pairs.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #'
    #' @return A boolean value `TRUE` indicating success, invisibly.
    #'
    clear_expired_keys = function(namespace = self$default_namespace) {
      out <- private$DRIVER$delete_expired_keys(namespace)
      clr_cache_expired_keys(namespace, self$envir_metadata)
      out
    },

    #' @description Get the key-namespace pairs with notes.
    #'
    #' @param namespace `r sto_namespaces_or_null`
    #' @param notes Should the `notes` column be returned?
    #' Default is `TRUE`.
    #'
    #' @return An object of class `data.table`.
    #'
    keys_with_notes = function(namespace = self$default_namespace, notes = TRUE) {
      out <- private$DRIVER$keys_with_notes(namespace, notes = notes)
      data.table::as.data.table(out)
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

    #' @description List notes given a namespace.
    #'
    #' @param namespace A single character namespace.
    #' @param named Should the output be named with keys?
    #' Default is `FALSE`.
    #'
    #' @return A vector with notes metadata values.
    #'
    list_notes = function(namespace = self$default_namespace, named = FALSE) {

      private$DRIVER$list_notes(namespace, named)
    },

    #' @description List all hashes stored in the storr.
    #'
    #'
    #' @return A sorted character vector with hashes.
    #'
    list_hashes = function() {

      sort(private$DRIVER$list_hashes())
    },

    #' @description List unused hashes stored in the storr.
    #'
    #'
    #' @return A sorted character vector with unused hashes.
    #'
    list_unused_hashes = function() {

      sort(private$DRIVER$list_unused_hashes())
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
    #' @details
    #' This will delete the actual objects from store with unused hashes (i.e.,
    #' not associated with any key-namespace pair).
    #'
    #' @param clear_expired Should the expired keys be deleted?
    #' Default is `FALSE`.
    #'
    #' @return A vector of unused hashes, invisibly.
    #'
    gc = function(clear_expired = FALSE) {

      if (clear_expired) {
        self$clear_expired_keys(NULL)
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
    #' @param src A source to import objects from. It can be a storr, list, or environment.
    #' **NOTE**: for TileDB storrs use `storr(driver_tiledb())` instead of `strorr_tiledb()`.
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
      sto <- storr::storr(private$DRIVER)
      invisible(.base_export(sto, src, list, namespace, skip_missing)$info)
    },

    #' @description Export objects from storr.
    #'
    #' Use list() to export to a brand new list, or use as.list(object) for a shorthand.
    #'
    #' @param dest A destination to export objects to. It can be a storr, list, or environment.
    #'  **NOTE**: for TileDB storrs use `storr(driver_tiledb())` instead of `strorr_tiledb()`.
    #' @param list Names of objects to export (or `NULL` for all objects) . If given it must be a character vector.
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
      sto <- storr::storr(private$DRIVER)
      invisible(.base_export(dest, sto, list, namespace, skip_missing)$dest)
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
        stop("Column not a character: ", paste(squote(cols[!ok]),
                                             collapse = ", "), call. = FALSE)
      }

      if (ncol(index) > 3) {
        if (!all(c("expires_at", "notes") %in% nms)) {
          stop("TileDB Storr index requires additional columns: 'expires_at', 'notes'", call. = FALSE)
        }

        if (!inherits(index[["expires_at"]], "POSIXct")) {
          stop("Column not a datetime: ", sQuote("expires_at"), call. = FALSE)
        }

        if (!is.character(index[["notes"]])) {
          stop("Column not a character: ", sQuote("notes"), call. = FALSE)
        }

      }

      msg <- setdiff(index$hash, self$list_hashes())
      if (length(msg) > 0L) {
        stop(sprintf("Missing %d / %d hashes - can't import",
                     length(msg), nrow(index)), call. = FALSE)
      }


      private$DRIVER$mset_hash(index$key, index$namespace, index$hash, index$expires_at, index$notes)
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

    #' @field async_info `mirai` information
    #'
    async_info = function(value) {

      if (!missing(value)) {
        check_read_only("async_info")
      }

      c(mirai::info(private$MIRAI_PROFILE), profile = private$MIRAI_PROFILE)
    },

    #' @field size Return Storr size
    #'
    size = function(value) {

      if (!missing(value)) {
        check_read_only("size")
      }

      private$DRIVER$size
    }
  ),

  private = list(

    # @field driver The TileDB driver.
    #
    DRIVER = NULL,

    # @field mirai_profile Dedicated compute profile for mirai.
    #
    MIRAI_PROFILE = NULL,

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

  # Select a single metadata for a key-namespace pair
  keymeta_unit = function(key, namespace, use_cache, meta_col) {

    private$check_input(key, n = 1, type = "character")
    private$check_input(namespace, n = 1, type = "character")

    keyns <- paste(key, namespace, sep = ":")
    envir <- self$envir_metadata

    if (use_cache && exists1(keyns, envir)) {
      value <- gethash(envir, keyns)[[meta_col]]
    } else {
      value <- private$DRIVER$get_keymeta_unit(key, namespace, meta_col)

    }
    value

  },

  # Select single metadata for multiple key-namespace pairs
  multi_keymeta_unit = function(key, namespace, use_cache, nomatch, meta_col) {

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
      value[cached] <- lapply(keyns[cached], function(h) gethash(envir, h)[[meta_col]])
      not_cached <- !cached
      status_not_cached <- any(not_cached)
    } else {
      # Everything is TRUE, so go to find them in DB
      not_cached <- !cached
      status_not_cached <- TRUE
    }

    is_missing <- FALSE

    if (status_not_cached) {

      # From not_cached find also which are truly missing
      cc <- private$DRIVER$mget_keymeta_unit(key[not_cached],
                                             namespace[not_cached],
                                             meta_col = meta_col,
                                             nomatch = nomatch)

      value[not_cached] <- cc
      keyns_not_cached <- keyns[not_cached]

      # not_cached and not found
      keyns_missing <- keyns_not_cached[attr(cc, "missing")]

      # Fill cache if needed
      # Indices for not_cached but existent items

      # Truly missing key-namespace pairs
      is_missing <- keyns %in% keyns_missing
    }


    if (any(is_missing)) {
      attr(value, "missing") <- which(is_missing)
    }

    value
  },

  # Set up persistent daemons for storr compute profile
  set_daemons = function() {
    if (!mirai::daemons_set(private$MIRAI_PROFILE)) {
      private$MIRAI_PROFILE <- paste("storr", digest::digest(Sys.time(), algo = "xxhash64"), sep = "_")
      n <- getOption("storr.mirai.daemons", 2L)
      mb <- getOption("storr.mirai.memory", NULL)
      mirai::daemons(n, memory = mb,.compute = private$MIRAI_PROFILE)
    }
  },

  # Reset daemons for storr compute profile
  finalize = function() {
    if (mirai::daemons_set(private$MIRAI_PROFILE)) {
      mirai::daemons(0L, .compute = private$MIRAI_PROFILE)
    }
  }

  )
)
