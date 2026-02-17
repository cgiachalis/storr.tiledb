

# TODO: need get_note/expiry (.get_meta with cache mechanism)

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

    ## Utility things, to fill later:
    hash_raw = NULL,
    serialize_object = NULL,

    initialize = function(driver, default_namespace) {

      if (!inherits(driver, "TileDBDriver")) {
        stop("Not a valid TileDB 'driver'. Please use a 'TileDBDriver' object.",
             call. = FALSE)
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

      # Key-value: 'hash', R object
      self$envir <- hashtab()
      # Key-value: 'key:namespace', list(expires_at, notes)
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

      if (use_cache) {
        km <- paste(key, namespace, sep = ":")
        sethash(self$envir_metadata, km, list(expires_at, notes))
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

      if (use_cache) {
        km <- paste(rep_len(key, n), rep_len(namespace, n), sep = ":")
        for(i in seq_along(km)) {
          sethash(self$envir_metadata, km[i], list(expires_at[i], notes[i]))
        }
      }

      invisible(hash)
    },

    # STATUS: DONE
    set_by_value = function(value, namespace = self$default_namespace,
                            expires_at, notes, use_cache = TRUE) {

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

      if (use_cache) {
        km <- paste(hash, namespace, sep = ":")
        sethash(self$envir_metadata, km, list(expires_at, notes))
      }

      invisible(hash)
    },

    # STATUS: DONE
    mset_by_value = function(value, namespace = self$default_namespace,
                             expires_at, notes, use_cache = TRUE) {

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

      if (use_cache) {
        km <- paste(rep_len(hash, n), rep_len(namespace, n), sep = ":")
        for(i in seq_along(km)) {
          sethash(self$envir_metadata, km[i], list(expires_at[i], notes[i]))
        }
      }

      invisible(hash)
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
        cached <- logical(length(hash))
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

      n <- private$check_length(key, namespace)

      deleted_hashes <- self$driver$del_hash(key, namespace)

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
      km <- paste(rep_len(key, n), rep_len(namespace, n), sep = ":")
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
    },

    # end ---

    # Experimental
    # TODO: mirai::require_daemons(.compute = "storr.tiledb")
    # TODO: remove nextget and dont launch daemons
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

      if (is.null(cfg)) {
        cfg <- tiledb::config(self$driver$ctx)
      }

      if (!inherits(cfg, "tiledb_config")){
        stop("'cfg' should be of class 'tiledb_config'", call. = FALSE)
      }

      # mirai namespace compute profile
      ns <- "storr.tiledb"

      # Start daemons if not already started
      if (is.null(mirai::nextget("n", .compute = ns))) {
        mirai::daemons(2L, autoexit = tools::SIGINT, .compute = ns)
      }

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


      if (use_cache) {
        km <- paste(key, namespace, sep = ":")
        sethash(self$envir_metadata, km, list(expires_at, notes))
      }

     invisible(list(mirai =list(obj = m1,
                                key = m2),
                    hash = hash))

    },

    # TODO
    mset_async = function() {

    }

  ),

  active = list(
   # stats or num_keys/num_namespaces/db_size/report
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
      stop("Incompatible lengths for key and namespace")
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
