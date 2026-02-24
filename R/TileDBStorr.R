#' @title Generate a `TileDBStorr` Object
#'
#' @description An R6 class that represent a TileDB storr.
#'
#' @returns A `TileDBStorr`, `R6` object.
#'
#' @export
#'
TileDBStorr <- R6::R6Class(
  classname = "TileDBStorr",
  inherit = R6_storr,
  cloneable = FALSE,

  public = list(

    driver = NULL,
    envir = NULL,
    envir_metadata = NULL,
    default_namespace = NULL,
    traits = NULL,
    hash_raw = NULL,
    serialize_object = NULL,

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

      self$driver <- driver

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



    # start ---

    # STATUS: DONE
    flush_cache = function() {
      clrhash(self$envir)
      clrhash(self$envir_metadata)
    },

    # STATUS: DONE
    # TODO: add async option (send to set_async)
    set = function(key, value, namespace = self$default_namespace,
                   expires_at, notes, use_cache = TRUE) {

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
      self$driver$set_hash(key, namespace, hash, expires_at, notes)

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

    # STATUS: DONE
    mset = function(key, value, namespace = self$default_namespace,
                    expires_at, notes, use_cache = TRUE) {

      n <- private$check_length(key, namespace)

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
      self$driver$mset_hash(key, namespace, hash, expires_at, notes)

      km <- paste(n$key, n$namespace, sep = ":")

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


    #' @description Set a key value asynchronously.
    #'
    #'
    #' @param key The key name to set a value to.
    #' @param namespace The namespace to look the key within.
    #' @param expires_at The date-time to set of class `POSIXct` (optional).
    #' @param notes A scalar string with notes to set (optional).
    #' @param use_cache Should the key value be copied into cache?
    #' Default is `TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #' @return A named list with two elements:
    #'
    #'  - `mirai`: a named list of two mirai objects
    #'  - `hash`: the hash value
    #'
    set_async = function(key,
                         value,
                         namespace = self$default_namespace,
                         expires_at,
                         notes,
                         use_cache = TRUE,
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
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

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
      m1 <- "none"
      if (!(use_cache && exists0(hash, self$envir))) {

        uri <- self$driver$uri

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

    #' @description Set multiple key values asynchronously.
    #'
    #' The arguments `key` and `namespace` can be recycled if any of them is a
    #' scalar character and the other is a vector. No other recycling rule is
    #' permitted.
    #'
    #' @param key A character vector of keys to set metadata to.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param expires_at A vector of date-times to set. Must be of class `POSIXct`.
    #' @param notes A character vector of notes to set.
    #' @param use_cache Should the key values be copied into cache?
    #' Default is `TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #' @return A named list with two elements:
    #'
    #'  - `mirai`: a named list of two mirai objects
    #'  - `hash`: a vector with hash values
    #'
    mset_async = function(key,
                          value,
                          namespace = self$default_namespace,
                          expires_at,
                          notes,
                          use_cache = TRUE,
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
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

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
      uri <- self$driver$uri

      # Step 1: store and cache object if needed
      m1 <- "none"

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

    # STATUS: DONE
    set_by_value = function(value,
                            namespace = self$default_namespace,
                            expires_at,
                            notes,
                            use_cache = TRUE) {

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
      self$driver$set_hash(hash, namespace, hash, expires_at, notes)

      km <- paste(hash, namespace, sep = ":")

      if (use_cache) {
        sethash(self$envir_metadata, km, list(expires_at = expires_at,
                                              notes = notes))
      } else {
        remhash(self$envir_metadata, km)
      }

      invisible(hash)
    },

    # STATUS: DONE
    mset_by_value = function(value,
                             namespace = self$default_namespace,
                             expires_at,
                             notes,
                             use_cache = TRUE) {

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
      self$driver$mset_hash(hash, namespace, hash, expires_at, notes)

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

    #' @description Set a value asynchronously.
    #'
    #'
    #' @param namespace The namespace to look the key within.
    #' @param expires_at The date-time to set of class `POSIXct` (optional).
    #' @param notes A scalar string with notes to set (optional).
    #' @param use_cache Should the key value be copied into cache?
    #' Default is `TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #' @return A named list with two elements:
    #'
    #'  - `mirai`: a named list of two mirai objects
    #'  - `hash`: the hash value
    #'
    set_by_value_async = function(value,
                                  namespace = self$default_namespace,
                                  expires_at,
                                  notes,
                                  use_cache = TRUE,
                                  cfg = NULL) {

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
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

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
      m1 <- "none"
      if (!(use_cache && exists0(hash, self$envir))) {

        uri <- self$driver$uri

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

    #' @description Set multiple values asynchronously.
    #'
    #' The arguments `key` and `namespace` can be recycled if any of them is a
    #' scalar character and the other is a vector. No other recycling rule is
    #' permitted.
    #'
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param expires_at A vector of date-times to set. Must be of class `POSIXct`.
    #' @param notes A character vector of notes to set.
    #' @param use_cache Should the key values be copied into cache?
    #' Default is `TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #' @return A named list with two elements:
    #'
    #'  - `mirai`: a named list of two mirai objects
    #'  - `hash`: a vector with hash values
    #'
    mset_by_value_async = function(value,
                                   namespace = self$default_namespace,
                                   expires_at,
                                   notes,
                                   use_cache = TRUE,
                                   cfg = NULL) {

      n <- length(value)

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
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

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
      uri <- self$driver$uri

      # Step 1: store and cache object if needed
      m1 <- "none"

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
        driver$mset_hash(namespace, hash, expires_at, notes)

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

    # STATUS: DONE
    set_value = function(value, use_cache = TRUE) {

      value_ser <- self$serialize_object(value)
      hash <- self$hash_raw(value_ser)

      if (!(use_cache && exists0(hash, self$envir))) {

        if (!self$driver$exists_object(hash)) {
          self$driver$set_object(hash, value_ser)
        }

        if (use_cache) {
          sethash(self$envir, hash, value)
        }
      }
      invisible(hash)
    },

    # STATUS: DONE
    mset_value = function(values, use_cache = TRUE) {

      values_ser <- lapply(values, self$serialize_object)
      hash <- vcapply(values_ser, self$hash_raw)
      cached <- logical(length(hash))

      envir <- self$envir

      if (use_cache) {
        cached <- exists0(hash, envir) # vlapply(hash, exists0, self$envir)
        upload <- logical(length(hash))
        upload[!cached] <- !self$driver$exists_object(hash[!cached])
      } else {
        upload <- !self$driver$exists_object(hash)
      }

      if (any(upload)) {
        # TODO: NO NEED
        send <- if (self$traits$accept == "object") values else values_ser

        self$driver$mset_object(hash[upload], send[upload])
      }

      if (use_cache) {
        for (i in which(!cached)) {
          sethash(self$envir, hash[[i]], values[[i]])
        }
      }
      invisible(hash)
    },


    # STATUS: DONE
    get_value = function(hash, use_cache = TRUE) {

      envir <- self$envir

      if (use_cache && exists0(hash, envir)) {
        value <- envir[[hash]]
      } else {
        # TODO: no need for traits
        if (self$traits$throw_missing) {
          value <- tryCatch(self$driver$get_object(hash),
                            error = function(e) stop(HashError(hash)))
        } else {
          if (!self$driver$exists_object(hash)) {
            stop(HashError(hash))
          }
          value <- self$driver$get_object(hash)
        }
        if (use_cache) {
          envir[[hash]] <- value
          # TODO: USE sethash(envir, hash, value)
        }
      }
      value
    },

    # STATUS: DONE
    mget_value = function(hash, use_cache = TRUE, missing = NULL) {

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
        if (is.null(self$driver$mget_object)) {
          value[!cached] <- lapply(hash[!cached], self$get_value, FALSE)
        } else {
          value[!cached] <- self$driver$mget_object(hash[!cached])
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
    #'
    #' @param key The key name to set metadata values to.
    #' @param namespace The namespace to look the key within.
    #' @param expires_at The date-time to set of class `POSIXct` (optional).
    #' @param notes A scalar string with notes to set (optional).
    #' @param use_cache Should the cache be used to retrieve the metadata?
    #' Default is `TRUE`. If a key:namespace not found in the cache, it will
    #' be fetched from database. Note that when setting `FALSE`, the cache
    #' will always be cleared for this key-namespace; this is to avoid mismatch
    #' between cache and database when reading back  with
    #' `use_cache = TRUE`.
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
                           use_cache = TRUE) {

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

      self$driver$set_keymeta(key, namespace, expires_at, notes)

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
    #' The arguments `key` and `namespace` can be recycled if any of them is a
    #' scalar character and the other is a vector. No other recycling rule is
    #' permitted.
    #'
    #' @param key A character vector of keys to set metadata to.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param expires_at A vector of date-times to set. Must be of class `POSIXct`.
    #' @param notes A character vector of notes to set.
    #' @param use_cache Should the cache be used to retrieve the metadata?
    #' Default is `TRUE`. If a key:namespace not found in the cache, it will
    #' be fetched from database. Note that when setting `FALSE`, the cache
    #' will always be cleared for this key-namespace; this is to avoid mismatch
    #' between cache and database when reading back  with
    #' `use_cache = TRUE`.
    #'
    #' @return The `key:namespace` character vector of the recycled length,
    #' invisibly. If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    #'
    mset_keymeta = function(key,
                            namespace = self$default_namespace,
                            expires_at,
                            notes,
                            use_cache = TRUE) {

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

      self$driver$mset_keymeta(p$key, p$namespace, expires_at, notes)
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
    #' @param key The key name to set metadata values to.
    #' @param namespace The namespace to look the key within.
    #' @param expires_at The date-time to set of class `POSIXct` (optional).
    #' @param notes A scalar string with notes to set (optional).
    #' @param use_cache Should the cache be used to retrieve the metadata?
    #' Default is `TRUE`. If a key:namespace not found in the cache, it will
    #' be fetched from database. Note that when setting `FALSE`, the cache
    #' will always be cleared for this key-namespace; this is to avoid mismatch
    #' between cache and database when reading back  with
    #' `use_cache = TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #'
    #' @return A named list with two elements (invisibly):
    #'
    #'  - `mirai`: a mirai object
    #'  - `keyns`: The `key:namespace` string
    #'
    #' NOTE: If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    #'
    set_keymeta_async = function(key,
                                 namespace = self$default_namespace,
                                 expires_at,
                                 notes,
                                 use_cache = TRUE,
                                 cfg = NULL) {


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

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)


      uri <- self$driver$uri

      m1 <- mirai::mirai({
        driver <- storr.tiledb::driver_tiledb(uri, context = ctx)
        driver$set_keymeta(key, namespace, expires_at, notes)
        },
      uri = uri,
      key = key,
      namespace = namespace,
      expires_at = expires_at,
      notes = notes, .compute = ns)

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
    #' The arguments `key` and `namespace` can be recycled if any of them is a
    #' scalar character and the other is a vector. No other recycling rule is
    #' permitted.
    #'
    #' @param key A character vector of keys to set metadata to.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param expires_at A vector of date-times to set. Must be of class `POSIXct`.
    #' @param notes A character vector of notes to set.
    #' @param use_cache Should the cache be used to retrieve the metadata?
    #' Default is `TRUE`. If a key:namespace not found in the cache, it will
    #' be fetched from database. Note that when setting `FALSE`, the cache
    #' will always be cleared for this key-namespace; this is to avoid mismatch
    #' between cache and database when reading back  with
    #' `use_cache = TRUE`.
    #' @param cfg Pass a [tiledb::config()]object to override context's
    #'  configuration.
    #'
    #' @return A named list with two elements (invisibly):
    #'
    #'  - `mirai`: a mirai object
    #'  - `keyns`: The `key:namespace` character vector of the recycled length
    #'
    #' NOTE: If both arguments `"expires_at"` and `"notes"` are missing,
    #' then nothing is set and a zero length character vector is returned.
    mset_keymeta_async = function(key,
                                 namespace = self$default_namespace,
                                 expires_at,
                                 notes,
                                 use_cache = TRUE,
                                 cfg = NULL) {

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

      private$set_daemons()

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

      ns <- .storr_profile

      # Export TileDB context on all connected daemons for 'storr.tiledb' profile
      #
      mirai::everywhere({
        cfg <- tiledb::tiledb_config(config_params)
        ctx <<- R6.tiledb::new_context(cfg)
      },
      config_params = as.vector(cfg), .compute = ns)


      uri <- self$driver$uri

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
    #'
    get_keymeta = function(key,
                           namespace = self$default_namespace,
                           use_cache = TRUE) {

      private$check_input(key, n = 1, type = "character")
      private$check_input(namespace, n = 1, type = "character")

      keyns <- paste(key, namespace, sep = ":")
      envir <- self$envir_metadata

      if (use_cache && exists0(keyns, envir)) {
        value <- gethash(envir, keyns)
      } else {
        value <- self$driver$get_keymeta(key, namespace)

        if (use_cache) {
          sethash(envir, keyns, value)
        }
      }
      value
    },

    #' @description Get multiple key metadata.
    #'
    #' The arguments `key` and `namespace` can be recycled if any of them is a
    #' scalar character and the other is a vector. No other recycling rule is
    #' permitted.
    #'
    #' @param key A character vector with keys to get metadata values from.
    #' @param namespace A character vector of namespaces to look the keys within.
    #' @param use_cache Should it be retrieved from cache? Default is
    #'  `TRUE`.
    #' @param misssing Fill value for missing keys. Default is `NULL`.
    #'
    #' @return A list with key metadata for each key-namespace
    #' pair. For not found pairs will return the `missing` value.
    #'
    #'
    mget_keymeta = function(key,
                            namespace = self$default_namespace,
                            use_cache = TRUE,
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
        cc <- self$driver$mget_keymeta(key[not_cached],
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

    # STATUS: DONE
    # storr reports back the number of deleted keys
    clear = function(namespace = self$default_namespace){

      if (!.is_character(namespace) & !is.null(namespace)) {

        stop(sprintf("'namespace' should be a character vector, not %s",
                     class(namespace)), call. = FALSE)
      }

      self$driver$delete_namespaces(namespace)
    },

    # STATUS: DONE
    del = function(key, namespace = self$default_namespace) {

      n <- storr::join_key_namespace(key, namespace)

      deleted_hashes <- self$driver$del_hash(n$key, n$namespace)

      # Remove cache metadata for primary index key:namespace
      #
      #  NOTE 1: We do it here instead when invoking gc() because on that
      # occasion we'll have to lookup again the key, namespace pairs. Since
      # we have deleted the hashes which correspond to key:namespace, their
      # cache can safely be removed; this is because when calling get_hash(),
      # it will always go to 'tbl_keys' and checks if the hash exists for the
      # key:namespace.
      #
      # NOTE 2: We cannot do the same for cached hashes as they might
      # be used by another key:namespace; but we do it in $gc() instead.
      #
      km <- paste(n$key, n$namespace, sep = ":")
      status <- vlapply(km, function(.k) {
        remhash(self$envir_metadata,key = .k)
      })

      invisible(deleted_hashes)
    },

    # STATUS: DONE
    gc = function() {

      # Deletes the objects in 'tbl_data'
      unused <- self$driver$delete_unused_hashes()

      # Delete unused hashes from cache; note that metadata for
      # the respective key:namespaces have been deleted by
      # del() operation. See comments in $del().
      del <- vlapply(unused, function(.k) {
        remhash(self$envir, .k)
        })

      invisible(unused)
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

  set_daemons = function() {
    if (!mirai::daemons_set(.storr_profile)) {
      enable_mirai()
    }
  }

  )
)
