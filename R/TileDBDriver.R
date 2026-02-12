
#' @importFrom stats na.omit
#' @importFrom data.table `:=`
#'


driver_tiledb_create <- function(uri,
                                 hash_algorithm = NULL,
                                 compression_level = -7,
                                 context = NULL) {

  dr <- TileDBDriver$new(uri, ctx = context)
  dr$create(compression_level = compression_level,
            algo = hash_algorithm,
            keep_open = FALSE)

  invisible(TRUE)
}



driver_tiledb <- function(uri, context = NULL) {

  dr <- TileDBDriver$new(uri, ctx = context)

  if (!dr$exists()) {
    cli::cli_abort("'storr' not found, please create one.", call = NULL)
  }

  dr
}

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
        self$open("WRITE")
      }

      self$binary <- FALSE
      self$traits <- list(accept = "string")

      lockBinding("binary", self)
      lockBinding("traits", self)

    },


    # TODO instantiate?
    #' @description Open CAS store.
    #'
    #' @param mode The mode to open, either `"READ"` or `"WRITE"`.
    #'
    #' @return The object, invisibly.
    #'
    open = function(mode) {
      # note that will affect members active binding
      super$open(mode, instantiate = TRUE)
    },

    #' @description Driver type.
    #'
    #' @return A character string.
    #'
    type = function() {
      self$class()
    },

    #' @description Get hash values.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
    #'
    #' @return A vector of hashes.
    #'
    get_hash = function(key, namespace) {

      self$query_keys(key, namespace, "hash")
    },

    #' @description Get hash values.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
    #'
    #' @return A vector of hashes.
    #'
    mget_hash = function(key, namespace) {

      self$query_keys(key, namespace, "hash")

    },

    #' @description Set hash values.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
    #' @param hash A vector with hash values.
    #' @param expires_at A vector with expiration datetimes.
    #' @param notes A character vector with notes.
    #'
    #' @return `TRUE` for successful operation, invisibly.
    #'
    set_hash = function(key, namespace, hash, expires_at, notes) {

      # TODO: Need to asset character scalar
      # ASCII / UTF8
      self$mset_hash(key, namespace, hash, expires_at, notes)

    },

    #' @description Set hash values.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
    #' @param hash A vector with hash values.
    #' @param expires_at A vector with expiration datetimes.
    #' @param notes A character vector with notes.
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
      self$mget_object(hash)[[1]]
      },

    #' @description Get a list R objects given a hash vector.
    #'
    #' @param hash A vector with hash values.
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

      nona_hash <- hash %in% arr[]$hash$as_vector()

     if (all(nona_hash)) {
       result <- lapply(arr[]$value$as_vector(),  {
         function(.s) unserialize(charToRaw(.s)) }
       )
     } else {
       result <- vector("list", length(hash))

       idx <- which(nona_hash)

       vals <- arr[]$value$as_vector()
       for (i in seq_along(idx)) {
         result[idx[i]] <- unserialize(charToRaw(vals[i]))
       }
     }

    result

    },

    #' @description Store serialised R objects.
    #'
    #' @param hash A vector with hash values.
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
    #' @param hash A vector with hash values.
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


    #' @description Check a key/namespace pair exists.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
    #'
    #' @return A logical vector.
    #'
    exists_hash = function(key, namespace) {

      qo <- private$query_keys0(key, namespace, "hash")

      dat.recv <- data.table::as.data.table(qo$arr[])
      dat.req <- qo$dat.req
      # TODO: in memory merge
      id.recv <- merge(dat.req, dat.recv)$id

      # Requested id vector vs received
      dat.req$id %in% id.recv

    },

    #' @description Check a serialised object exists.
    #'
    #' @param hash A character vector with hash values.
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
      hash %in% hashes

    },

    #' @description Delete a key/namespace pair.
    #'
    #' @param key A character vector with keys.
    #' @param namespace A character vector with namespaces.
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
    #' @param hash A character vector with hash values.
    #'
    #' @return A logical vector.
    #'
    del_object = function(hash) {

      exists <- self$exists_object(hash)

      if (any(exists)) {

        hash_del <- hash[exists] # DO we need it?

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
    #' @param namespace A namespace value.
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

    }
  ),

  private = list(

  )
)
