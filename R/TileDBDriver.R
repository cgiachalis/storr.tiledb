
#' @importFrom stats na.omit
#' @importFrom data.table `:=`
#'



#' @title Generate a `TileDBDriver` Object
#'
#' @description An R6 class for creating a content addressable storage.
#'
#' @returns A `TileDBDriver`, `R6` object.
#'
#' @export
#'
TileDBDriver <- R6::R6Class(
  classname = "TileDBDriver",
  inherit = CAS,
  cloneable = FALSE,
  public = list(

    #' @field traits Driver traits (**immutable**).
    #'
    traits = NULL,

    #' @field binary Binary toggle, default is `FALSE` (**immutable**).
    #'
    binary = NULL,

    #' @description Create a new `TileDBDriver` object.
    #'
    #' @param uri URI path for the `TileDBDriver` object.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #'
    initialize = function(uri, ctx = NULL) {

      super$initialize(uri, ctx = ctx)

      if (self$exists()) {
        self$open(instantiate = TRUE)
      }

      self$binary <- FALSE
      self$traits <- list(accept = "string",
                          throw_missing = TRUE)

      lockBinding("binary", self)
      lockBinding("traits", self)

    },

    #' @description Driver type.
    #'
    #' @return A character string.
    #'
    type = function() {
      "tiledb"
    },

    #' @description Get hash values.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #'
    #' @return A vector of hashes.
    #'
    get_hash = function(key, namespace) {

      result <- self$query_keys(key, namespace, "hash")

      if (is.na(result)) {
        # 'get_hash' always returns NA if missing as we need
        # this for mget_hash. Here, an error is raised to
        # support 'throw_missing' trait. Doing so, we're
        # avoiding the extra query (e.g., exists_hash) in storr
        # layer.
        stop(KeyError(key, namespace))
      }

      result
    },

    #' @description Get hash values.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #'
    #' @return A vector of hashes.
    #'
    mget_hash = function(key, namespace) {

      self$query_keys(key, namespace, "hash")

    },

    #' @description Set hash values.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #' @param hash `r roxy_hash`
    #' @param expires_at `r roxy_expires`
    #' @param notes `r roxy_notes`
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    set_hash = function(key, namespace, hash, expires_at, notes) {

      self$mset_hash(key, namespace, hash, expires_at, notes)

    },

    #' @description Set hash values.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #' @param hash `r roxy_hash`
    #' @param expires_at `r roxy_expires`
    #' @param notes `r roxy_notes`
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    mset_hash = function(key, namespace, hash, expires_at, notes) {

      if (length(hash) == 0L) {
        return()
      }

      # NOTE: 'storr' class does not support 'expires_at' and 'notes',
      # so we sanitise missing values
      if (missing(expires_at)) {
        expires_at <- NA_real_
      }

      if (missing(notes)) {
        notes <- NA_character_
      }

      dat <- data.frame(namespace = namespace,
                        key = key,
                        hash = hash,
                        expires_at = expires_at,
                        notes = notes)

      keep <- !duplicated(dat[, 1:2], fromLast = TRUE)
      dat <- dat[keep, ]

      arr <- private$keys_array()$tiledb_array()
      arr[] <- dat

      invisible(TRUE)

    },

    #' @description Get R object given a hash.
    #'
    #' @param hash A hash value.
    #'
    #' @return A de-serialized R object.
    #'
    get_object = function(hash) {

      private$check_scalar_character(hash)
      result <- self$mget_object(hash)[[1]]

      if (is.null(result)) {
        # 'get_object' always returns NULL if missing as we need
        # this for mget_object. Here, an error is raised to
        # support 'throw_missing' trait. Doing so, we're
        # avoiding the extra query (e.g., exists_object) in storr
        # layer.
        stop(hashError(hash))
      }

      result

    },

    #' @description Get a list R objects given a hash vector.
    #'
    #' @param hash `r roxy_hash`
    #'
    #' @return A list with de-serialized R objects.
    #'
    mget_object = function(hash) {

      if (length(hash) == 0L){
        return(list())
      }

      arrobj <- private$data_array()

      sp <- list(hash = hash)
      arr <- arrobj$tiledb_array(extended = TRUE,
                                 selected_points = sp,
                                 return_as = "arrow")

      # TODO: REVIEW
      # nona_hash <- hash %in% arr[]$hash$as_vector()

      x <- arrow::Array$create(hash)
      nona_hash <- arrow::call_function("is_in",
                                  x,
                                  options = list(
                                    value_set = arr[]$GetColumnByName("hash"),
                                    skip_nulls = TRUE
                                  ))

      status_nona <- arrow::call_function("all", nona_hash)
      status_nona <- status_nona$as_vector()

     if (status_nona) {
       result <- lapply(arr[]$value$as_vector(),  {
         function(.s) unserialize(charToRaw(.s)) }
       )
     } else {
       result <- vector("list", length(hash))

       idx <- which(nona_hash$as_vector())

       vals <- arr[]$value$as_vector()
       for (i in seq_along(idx)) {
         result[idx[i]] <- unserialize(charToRaw(vals[i]))
       }
     }

    result

    },

    #' @description Store serialised R objects.
    #'
    #' @param hash `r roxy_hash`
    #' @param value A vector with serialised values.
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    set_object = function(hash, value) {
      # TODO: arrow
      # arr: table_object[] <- list(hash = hash,
      #                              value = value)
      # may using narrow
      self$mset_object(hash, value)

    },

    #' @description Store serialised R objects.
    #'
    #' @param hash `r roxy_hash`
    #' @param value A vector with serialised values.
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    mset_object = function(hash, value) {

      if (length(value) == 0L) {
        return()
      }

      # storr's $mset_value doesn't simplify the list of values
      if (is.list(value)) {
        value <- unlist(value)
      }

      # TODO: arrow
      dat <- data.frame(hash = hash,
                        value = value)

      arr <- private$data_array()$tiledb_array()
      arr[] <- dat

      invisible(TRUE)
    },

    #' @description Set key-namespace metadata.
    #'
    #' Sets a pair of expiry date-time and notes.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #' @param expires_at `r roxy_expires`
    #' @param notes `r roxy_notes`
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    set_keymeta = function(key, namespace, expires_at, notes) {


      dat <- self$filter_keys(key, namespace)

      if (nrow(dat) == 0) {
        stop(KeyError(key, namespace))
      }

      if (!is.null(notes)) {
        dat$notes <- notes
      }

      if (!is.null(expires_at)) {
        dat$expires_at <- expires_at
      }

      arr <- private$keys_array()$tiledb_array()
      arr[] <- dat

      invisible(TRUE)

    },

    #' @description Set multiple key-namespace metadata.
    #'
    #' Sets a pair of expiry date-time and notes.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #' @param expires_at `r roxy_expires`
    #' @param notes `r roxy_notes`
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    mset_keymeta = function(key, namespace, expires_at, notes) {

      dat <- self$filter_keys(key, namespace)

      # Check for no hash in given key:namespace
      data.table::setkeyv(dat, c("namespace", "key"))

      dat <- dat[.(namespace, key), env = list(namespace = I(namespace),
                                             key = I(key))][]
      hash_isna <- is.na(dat[["hash"]])

      if (any(hash_isna)) {
        stop(KeyError(paste(dat$key[hash_isna], collapse = ","),
                      paste(dat$namespace[hash_isna], collapse = ",")))
      }

      if (!is.null(notes)) {
        dat[,notes := vals, env = list(vals = I(notes))]
      }

      if (!is.null(expires_at)) {
        dat[,expires_at := vals, env = list(vals = I(expires_at))]
      }

      arr <- private$keys_array()$tiledb_array()
      arr[] <- dat

      invisible(TRUE)

    },

    #' @description Get key-namespace metadata.
    #'
    #' @param key A single character key.
    #' @param namespace A single character namespace.
    #'
    #' @return A named list with key-metadata, `"expires_at"`
    #' and `"notes".`
    #'
    get_keymeta = function(key, namespace) {

      arrobj <- private$keys_array()

      sp <- list(namespace = namespace, key = key)
      arr <- arrobj$tiledb_array(extended = FALSE,
                                 attrs = c("expires_at", "notes"),
                                 selected_points = sp,
                                 return_as = "arrow")

      DT <- data.table::as.data.table(arr[])

      # TODO: Remove when TileDB fixes it
      expires_at <- NULL
      DT[expires_at < 0 , expires_at := as.POSIXct(NA)]


      if (nrow(DT) == 0) {
        stop(KeyError(key, namespace))
      }

      as.list(DT)
    },

    #' @description Get multiple key-namespace metadata.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #' @param nomatch Value to fill in case of no match.
    #'
    #' @return A list with key metadata for each key-namespace
    #' pair. For not found pairs will return the nomatch value.
    #'
    mget_keymeta = function(key, namespace, nomatch = NULL) {

      arrobj <- private$keys_array()

      # Slice array
      sp <- list(namespace = namespace, key = key)
      arr <- arrobj$tiledb_array(selected_points = sp, return_as = "arrow")

      DT <- data.table::as.data.table(arr[], key = c("namespace", "key"))

      # TODO: Remove when TileDB fixes it
      # Sanitise datetime columns
      # See:
      expires_at <- NULL
      DT[expires_at < 0 , expires_at := as.POSIXct(NA)]

      DT <- DT[.(namespace, key), env = list(namespace = I(namespace), key = I(key))]
      hash_isna <- is.na(DT[["hash"]])

      out <- vector("list", nrow(DT))

      if (is.null(nomatch)) {
        nomatch <- list(nomatch)
      }

      for (i in seq_along(out)) {

        if (!hash_isna[i]) {
          out[[i]] <- as.list(DT[i, c("expires_at", "notes")])
        } else {
          out[[i]] <- nomatch
        }

      }

      attr(out, "missing") <- which(hash_isna)

      out

    },

    #' @description Check a key-namespace pair exists.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #'
    #' @return A logical vector.
    #'
    exists_hash = function(key, namespace) {

      p <- storr::join_key_namespace(key, namespace)

      arrobj <- private$keys_array()

      sp <- list(namespace = namespace, key = key)
      arr <- arrobj$tiledb_array(attrs = "hash",
                                 selected_points = sp,
                                 return_as = "arrow")

      DT <- data.table::as.data.table(arr[])

      data.table::setkeyv(DT, c("namespace", "key"))

      key <- p$key
      namespace <- p$namespace
      i <- DT[.(namespace, key), "hash", with = FALSE,
                env = list(namespace = I(namespace),
                           key = I(key))]

      !is.na(i[["hash"]])
    },

    #' @description Check a serialised object exists.
    #'
    #' @param hash `r roxy_hash`
    #'
    #' @return A logical vector.
    #'
    exists_object = function(hash) {

      arrobj <- private$data_array()

      arr <- arrobj$tiledb_array(attrs = NA_character_,
                                 selected_points = list(hash = hash),
                                 return_as = "arrow")

      hashes <- arr[]$GetColumnByName("hash")$as_vector()

      # Requested vector vs received
      # hash %in% hashes

      #  TODO: review
      x <- arrow::Array$create(hash)
      out <- arrow::call_function("is_in",
                                  x,
                                  options = list(
                                    value_set = arr[]$GetColumnByName("hash"),
                                    skip_nulls = TRUE
                                  ))
      out$as_vector()

    },

    #' @description Delete a key/namespace pair.
    #'
    #' @param key `r roxy_key`
    #' @param namespace `r roxy_namespace`
    #'
    #' @return A logical vector.
    #'
    del_hash = function(key, namespace) {

      # might not need it,
      # TODO: check when try to delete a no key
      exists <- self$exists_hash(key, namespace)

      if (any(exists)) {

        arr <- private$keys_array()$tiledb_array()

        # Close array as we're going to submit a delete query
        if (tiledb::tiledb_array_is_open(arr)) {

          arr <- tiledb::tiledb_array_close(arr)
        }

        qc1 <- tiledb::tiledb_query_condition_create(name = "namespace",
                                                    values = namespace,
                                                    op = "IN")

        qc2 <- tiledb::tiledb_query_condition_create(name = "key",
                                                     values = key,
                                                     op = "IN")

        qc <- tiledb::tiledb_query_condition_combine(qc1, qc2, "AND")

        tiledb::query_condition(arr) <- qc

        qry <- tiledb::tiledb_query(arr, "DELETE")

        qry <- tiledb::tiledb_query_set_condition(qry, qc)

        tiledb::tiledb_query_submit(qry)

        tiledb::tiledb_query_finalize(qry)

        # Hint: Now, the array handle is opened at delete mode,
        #       reopen to previous mode
        mode <- self$mode
       self$members$tbl_keys$object$reopen(mode)
      }

      exists
    },

    #' @description Delete serialised objects.
    #'
    #' @param hash `r roxy_hash`
    #'
    #' @return A logical vector.
    #'
    del_object = function(hash) {

      exists <- self$exists_object(hash)

      if (any(exists)) {

        hash_del <- hash[exists]

        arr <- private$data_array()$tiledb_array()

        # Close array as we're going to submit a delete query
        if (tiledb::tiledb_array_is_open(arr)) {

          arr <- tiledb::tiledb_array_close(arr)
        }

        qc <- tiledb::tiledb_query_condition_create(name = "hash",
                                                     values = hash_del,
                                                     op = "IN")

        tiledb::query_condition(arr) <- qc

        qry <- tiledb::tiledb_query(arr, "DELETE")

        qry <- tiledb::tiledb_query_set_condition(qry, qc)

        tiledb::tiledb_query_submit(qry)

        tiledb::tiledb_query_finalize(qry)

        # Hint: Now, the array handle is opened at delete mode,
        #       reopen to previous mode
        mode <- self$mode
        self$members$tbl_data$object$reopen(mode)

      }

      exists
    },

    #' @description List all hash values.
    #'
    #'
    #' @return A vector of hash values.
    #'
    list_hashes = function() {

      arrobj <- private$data_array()
      arr <- arrobj$tiledb_array(attrs = NA_character_,
                                 return_as = "arrow")

      ns <- arr[]$GetColumnByName("hash")
      ns$as_vector()

    },

    #' @description List all namespace values.
    #'
    #'
    #' @return A vector of namespace values.
    #'
    list_namespaces = function() {

      arrobj <- private$keys_array()
      arr <- arrobj$tiledb_array(attrs = NA_character_,
                                 return_as = "arrow")

      ns <- arr[]$GetColumnByName("namespace")
      ns <- arrow::call_function("unique", ns)
      ns$as_vector()

    },

    #' @description List keys given a namespace.
    #'
    #' @param namespace A single character namespace.
    #'
    #' @return A vector of key values.
    #'
    list_keys = function(namespace) {

      private$check_scalar_character(namespace)

      arrobj <- private$keys_array()
      arr <- arrobj$tiledb_array(attrs = NA_character_,
                                 selected_points = list(namespace = namespace),
                                 return_as = "arrow")

      keys <- arr[]$GetColumnByName("key")
      keys$as_vector()

    },

    #' @description List unused hashes.
    #'
    #'
    #' @return A vector of hash values.
    #'
    list_unused_hashes = function() {

      # Get unique hash values from 'tbl_keys'
      arrobj <- private$keys_array()
      arr <- arrobj$tiledb_array(extended = TRUE,
                                 attrs = "hash",
                                 return_as = "arrow")

      yh <- arr[]$GetColumnByName("hash")
      yh <- arrow::call_function("unique", yh)
      yh <- yh$as_vector()

      # Get object hash values from 'tbl_data'
      xh <- self$list_hashes()

      # Find unused hashes (equiv. setdiff(x, y))
      xh[data.table::chmatch(xh, yh, 0L) == 0L]

    },

    #' @description Delete unused hashes.
    #'
    #'
    #' @return A vector of deleted hash values, invisibly.
    #'
    delete_unused_hashes = function() {

      unused <- self$list_unused_hashes()

      if (length(unused) != 0) {
        arr <- private$data_array()$tiledb_array()

        # Close array as we're going to submit a delete query
        if (tiledb::tiledb_array_is_open(arr)) {
          arr <- tiledb::tiledb_array_close(arr)
        }

        qc <- tiledb::tiledb_query_condition_create(name = "hash",
                                                    values = unused,
                                                    op = "IN")

        tiledb::query_condition(arr) <- qc

        qry <- tiledb::tiledb_query(arr, "DELETE")

        qry <- tiledb::tiledb_query_set_condition(qry, qc)

        tiledb::tiledb_query_submit(qry)

        tiledb::tiledb_query_finalize(qry)

        # Hint: Now, the array handle is opened at delete mode,
        #       reopen to previous mode
        mode <- self$mode
        self$members$tbl_data$object$reopen(mode)
      }

      invisible(unused)
    },

    #' @description Delete namespaces.
    #'
    #' @param ns A character vector of namespaces. If `NULL` all
    #' namespaces will be cleared.
    #'
    #' @return A logical vector indicating successful deletion or not.
    #' `FALSE` means the namespace was not found in database.
    #'
    delete_namespaces = function(ns) {

     namespaces <- self$list_namespaces()

     if (length(namespaces) == 0) {
       return(invisible(NULL))
     }

     # TODO decide what to return
     if (is.null(ns)) {
       exists <- !logical(length(namespaces)) # TRUEs
     } else {
       exists <- ns %in% namespaces
       namespaces <- ns[exists]
     }

     arr <- private$keys_array()$tiledb_array()

     # Close array as we're going to submit a delete query
     if (tiledb::tiledb_array_is_open(arr)) {
       arr <- tiledb::tiledb_array_close(arr)
     }

     qc <- tiledb::tiledb_query_condition_create(name = "namespace",
                                                 values = namespaces,
                                                 op = "IN")

     tiledb::query_condition(arr) <- qc

     qry <- tiledb::tiledb_query(arr, "DELETE")

     qry <- tiledb::tiledb_query_set_condition(qry, qc)

     tiledb::tiledb_query_submit(qry)

     tiledb::tiledb_query_finalize(qry)

     # Hint: Now, the array handle is opened at delete mode,
     #       reopen to previous mode
     mode <- self$mode
     self$members$tbl_keys$object$reopen(mode)

     exists

    }
  ),

  private = list(

  )
)
