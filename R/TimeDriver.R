#' @title Generate a `TimeDriver` Object
#'
#' @description A [TileDBDriver] variant with read only class methods and
#' time-travel support.
#'
#' This class is intended for usage into [StorrTimeTravel].
#'
#' @returns A `TimeDriver`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
TimeDriver <- R6::R6Class(
  inherit = TileDBGroup,
  cloneable = FALSE,
  classname = "TimeDriver",

  public = list(

    #' @field traits Driver traits (**immutable**).
    #'
    traits = NULL,

    #' @description Instantiate a new `TimeDriver` object.
    #'
    #' @param uri URI path for the `TimeBDriver` object.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #' @param timestamp Set a `TileDB` timestamp range that
    #'  the resource will be opened at. Effective in `"READ"` mode only.
    #'  Valid options:
    #'  - A `NULL` value (default)
    #'  - An `R` object coercible to `POSIXct` with length 1 which is used for end timestamp,
    #'  or length 2 with start, end timestamps
    #'  - An object of class `tiledb_timestamp`. See [R6.tiledb::set_tiledb_timestamp()]
    #'
    initialize = function(uri, ctx = NULL, timestamp = NULL) {

      super$initialize(uri, ctx = ctx, tiledb_timestamp = timestamp)

      if (self$exists()) {
        self$open(instantiate = TRUE)
      }

      self$traits <- list(accept = "string",
                          throw_missing = TRUE)

      lockBinding("traits", self)

    },

    #' @description Driver type.
    #'
    #' @return A character string.
    #'
    type = function() {
      "tiledb"
    },

   #' @description Open `TimeDriver` object for read or write.
   #'
   #' Setting`instantiate` argument to `TRUE`, all members will be instantiated
   #' and cached on opening. They can be accessed via `members` active field, i.e., using
   #' `<member>$object` element.
   #'
   #' @param mode Mode to open : either `"READ"` or `"WRITE"`. Default is `"READ"`.
   #' @param instantiate Should be all members be instantiated at opening?
   #'  Default is `FALSE`.
   #'
   #' @return The object, invisibly.
   #'
   open = function(mode = c("READ", "WRITE"), instantiate = FALSE) {

     super$open(mode = mode)

     type <- self$get_metadata("type")

     if (type != "storr" || is.null(type)) {

       cli::cli_abort("Not a {.arg TileDB Storr} at URI: {.url {self$uri}}", call = NULL)

     }

     if (instantiate) {
       private$instantiate_members()
       private$.members_instantiated <- TRUE
     }

     invisible(self)

   },

   #' @description Close the group object.
   #'
   #' All instantiated group members will be closed if opened, and before
   #' closing the group object.
   #'
   #' @return The object, invisibly.
   #'
   close = function() {

     super$close()
     private$.members_instantiated <- NULL

     invisible(self)
   },

   #' @description Filter `tbl_keys` by key and namespace
   #'
   #' @param key `r roxy_key`
   #' @param namespace `r roxy_namespace`
   #' @param attrnames A character vector with tiledb attributes (columns).
   #'
   #' @return A `data.table.
   #'
   filter_keys = function(key, namespace, attrnames = character()) {

     arrobj <- private$keys_array()

     sp <- list(namespace = namespace, key = key)

     arr <- arrobj$object
     tiledb::attrs(arr) <- attrnames
     tiledb::selected_points(arr) <- sp
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = attrnames,
     #                            selected_points = sp,
     #                            return_as = "arrow",
     #                            ctx = self$ctx)

     dt <- data.table::as.data.table(arr[])

     # TODO: Remove when TileDB fixes it
     if (attrnames == "expires_at" || length(attrnames) == 0) {
       expires_at <- NULL
       dt[expires_at <= 0 , expires_at := as.POSIXct(NA)]
     }

     dt
   },

   #' @description Get hash values.
   #'
   #' @param key `r roxy_key`
   #' @param namespace `r roxy_namespace`
   #'
   #' @return A vector of hashes.
   #'
   get_hash = function(key, namespace) {

     result <- private$query_hash(key, namespace)

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

     private$query_hash(key, namespace)

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
       # NB: 'get_object' always returns NULL if missing as we need
       # this for mget_object. Here, an error is raised to
       # support 'throw_missing' trait. Doing so, we're
       # avoiding the extra query (e.g., exists_object) in storr
       # layer.
       stop(HashError(hash))
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
     arr <- arrobj$object
     tiledb::selected_points(arr) <- sp
     tiledb::return_as(arr) <- "arrow"
     # arr <- arrobj$tiledb_array(extended = TRUE,
     #                            selected_points = sp,
     #                            return_as = "arrow")

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

     # NB: The read query gives unordered results, so we
     #     extract them in order to match the requested order.
     names(result) <- arr[]$hash$as_vector()
     unname(result[hash])

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

     arr <- arrobj$object
     tiledb::extended(arr) <- FALSE
     tiledb::attrs(arr) <- c("expires_at", "notes")
     tiledb::selected_points(arr) <- sp
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(extended = FALSE,
     #                            attrs = c("expires_at", "notes"),
     #                            selected_points = sp,
     #                            return_as = "arrow")

     DT <- data.table::as.data.table(arr[])

     # TODO: Remove when TileDB fixes it
     expires_at <- NULL
     DT[expires_at <= 0 , expires_at := as.POSIXct(NA)]


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
     arr <- arrobj$object
     tiledb::selected_points(arr) <- sp
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(selected_points = sp, return_as = "arrow")

     DT <- data.table::as.data.table(arr[], key = c("namespace", "key"))

     # TODO: Remove when TileDB fixes it
     # Sanitise datetime columns
     expires_at <- NULL
     DT[expires_at <= 0 , expires_at := as.POSIXct(NA)]

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

     arr <- arrobj$object
     tiledb::attrs(arr) <- "hash"
     tiledb::selected_points(arr) <- sp
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = "hash",
     #                            selected_points = sp,
     #                            return_as = "arrow")

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

     arr <- arrobj$object
     tiledb::attrs(arr) <- NA_character_
     tiledb::selected_points(arr) <- list(hash = hash)
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = NA_character_,
     #                            selected_points = list(hash = hash),
     #                            return_as = "arrow")

     hashes <- arr[]$GetColumnByName("hash")$as_vector()

     x <- arrow::Array$create(hash)
     out <- arrow::call_function("is_in",
                                 x,
                                 options = list(
                                   value_set = arr[]$GetColumnByName("hash"),
                                   skip_nulls = TRUE
                                 ))
     out$as_vector()

   },

   #' @description List all hash values.
   #'
   #'
   #' @return A vector of hash values.
   #'
   list_hashes = function() {

     arrobj <- private$data_array()

     arr <- arrobj$object
     tiledb::attrs(arr) <- NA_character_
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = NA_character_,
     #                            return_as = "arrow")

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

     arr <- arrobj$object
     tiledb::attrs(arr) <- NA_character_
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = NA_character_,
     #                            return_as = "arrow")

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

     arr <- arrobj$object
     tiledb::attrs(arr) <- NA_character_
     tiledb::selected_points(arr) <- list(namespace = namespace)
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(attrs = NA_character_,
     #                            selected_points = list(namespace = namespace),
     #                            return_as = "arrow")

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

     arr <- arrobj$object
     tiledb::attrs(arr) <- "hash"
     tiledb::return_as(arr) <- "arrow"

     # arr <- arrobj$tiledb_array(extended = TRUE,
     #                            attrs = "hash",
     #                            return_as = "arrow")

     yh <- arr[]$GetColumnByName("hash")
     yh <- arrow::call_function("unique", yh)
     yh <- yh$as_vector()

     # Get object hash values from 'tbl_data'
     xh <- self$list_hashes()

     # Find unused hashes (equiv. setdiff(x, y))
     xh[data.table::chmatch(xh, yh, 0L) == 0L]

   },

   #' @description Get the key-namespace pairs with expiration timestamps.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #' @param datetimes Should the `expires_at` column be returned?
   #' Default is `TRUE`.
   #'
   #' @return An `ArrowObject` object.
   #'
   keys_with_expiration = function(namespace, datetimes = TRUE) {

     check_character_or_null(namespace)

     arrobj <- private$keys_array()
     arr <- arrobj$object

     # Ignore NA datetimes
     qc <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                               value = as.POSIXct(NA),
                                               dtype = "DATETIME_MS",
                                               op = "NE")

     sp <- list()

     if (!is.null(namespace)) {
       sp <- list(namespace = namespace)
     }

     if (datetimes) {
       attrs <- "expires_at"
     } else {
       attrs <- NA_character_
     }

     tiledb::attrs(arr) <- attrs
     tiledb::selected_points(arr) <- sp
     tiledb::query_condition(arr) <- qc
     tiledb::return_as(arr) <- "arrow"
     arr[]
     # arrobj$tiledb_array(attrs = attrs,
     #                     selected_points = sp,
     #                     query_condition = qc,
     #                     return_as = "arrow")[]
   },

   #' @description Get the key-namespace pairs without expiration timestamps.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #' @param datetimes Should the `expires_at` column be returned?
   #' Default is `TRUE`.
   #'
   #' @return An `ArrowObject` object.
   #'
   keys_without_expiration = function(namespace, datetimes = TRUE) {

     check_character_or_null(namespace)

     arrobj <- private$keys_array()
     arr <- arrobj$object

     # Ignore NA datetimes
     qc <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                               value = as.POSIXct(NA),
                                               dtype = "DATETIME_MS",
                                               op = "EQ")

     sp <- list()

     if (!is.null(namespace)) {
       sp <- list(namespace = namespace)
     }

     if (datetimes) {
       attrs <- "expires_at"
     } else {
       attrs <- NA_character_
     }

     tiledb::attrs(arr) <- attrs
     tiledb::selected_points(arr) <- sp
     tiledb::query_condition(arr) <- qc
     tiledb::return_as(arr) <- "arrow"
     arr[]
     #
     # arrobj$tiledb_array(attrs = attrs,
     #                     selected_points = sp,
     #                     query_condition = qc,
     #                     return_as = "arrow")[]
   },

   #' @description Get the expired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #' @param datetimes Should the `expires_at` column be returned?
   #' Default is `TRUE`.
   #'
   #' @return An `ArrowObject` object.
   #'
   expired_keys = function(namespace, datetimes = TRUE) {

     check_character_or_null(namespace)

     arrobj <- private$keys_array()
     arr <- arrobj$object

     # Ignore NA datetimes
     qc_dttm1 <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                                     value = as.POSIXct(NA),
                                                     dtype = "DATETIME_MS",
                                                     op = "NE")
     # Expired datetimes (now > index)
     qc_dttm2 <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                                     value = Sys.time(),
                                                     dtype = "DATETIME_MS",
                                                     op = "LT")

     qc <- tiledb::tiledb_query_condition_combine(qc_dttm1, qc_dttm2, "AND")

     sp <- list()

     if (!is.null(namespace)) {
       sp <- list(namespace = namespace)
     }

     if (datetimes) {
       attrs <- "expires_at"
     } else {
       attrs <- NA_character_
     }

     tiledb::attrs(arr) <- attrs
     tiledb::selected_points(arr) <- sp
     tiledb::query_condition(arr) <- qc
     tiledb::return_as(arr) <- "arrow"
     arr[]

     # arrobj$tiledb_array(attrs = attrs,
     #                     selected_points = sp,
     #                     query_condition = qc,
     #                     return_as = "arrow")[]
   },

   #' @description Get the unexpired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #' @param datetimes Should the `expires_at` column be returned?
   #' Default is `TRUE`.
   #'
   #' @return An `ArrowObject` object.
   #'
   unexpired_keys = function(namespace, datetimes = TRUE) {

     check_character_or_null(namespace)

     arrobj <- private$keys_array()
     arr <- arrobj$object

     # Ignore NA datetimes
     qc_dttm1 <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                                     value = as.POSIXct(NA),
                                                     dtype = "DATETIME_MS",
                                                     op = "NE")
     # Expired datetimes (now > index)
     qc_dttm2 <- tiledb::tiledb_query_condition_init(attr = "expires_at",
                                                     value = Sys.time(),
                                                     dtype = "DATETIME_MS",
                                                     op = "GT")

     qc <- tiledb::tiledb_query_condition_combine(qc_dttm1, qc_dttm2, "AND")

     sp <- list()

     if (!is.null(namespace)) {
       sp <- list(namespace = namespace)
     }

     if (datetimes) {
       attrs <- "expires_at"
     } else {
       attrs <- NA_character_
     }

     tiledb::attrs(arr) <- attrs
     tiledb::selected_points(arr) <- sp
     tiledb::query_condition(arr) <- qc
     tiledb::return_as(arr) <- "arrow"
     arr[]

     # arrobj$tiledb_array(attrs = attrs,
     #                     selected_points = sp,
     #                     query_condition = qc,
     #                     return_as = "arrow")[]
   },

   #' @description Get the number of expired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #'
   #' @return A numeric value.
   #'
   num_expired_keys = function(namespace) {

     arr <- self$expired_keys(namespace, datetimes = FALSE)
     arr[]$num_rows
   },

   #' @description Get the number of unexpired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #'
   #' @return A numeric value.
   #'
   num_unexpired_keys = function(namespace) {

     arr <- self$unexpired_keys(namespace, datetimes = FALSE)
     arr[]$num_rows
   },

   #' @description Check for expired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #'
   #' @return `TRUE` for expired keys, `FALSE` otherwise.
   #'
   has_expired_keys = function(namespace) {

     arr <- self$expired_keys(namespace, datetimes = FALSE)
     arr[]$num_rows != 0
   },

   #' @description Check for unexpired key-namespace pairs.
   #'
   #' @param namespace `r sto_namespaces_or_null`
   #'
   #' @return `TRUE` for unexpired keys, `FALSE` otherwise.
   #'
   has_unexpired_keys = function(namespace) {

     arr <- self$unexpired_keys(namespace, datetimes = FALSE)
     arr[]$num_rows != 0
   }

  ),

  active = list(

    #'@field timestamp Set or retrieve a `TileDB` timestamp range that
    #'  the resource will be opened at. Effective in `"READ"` mode only.
    #'
    timestamp = function(value) {

      if (!missing(value)) {
      super$tiledb_timestamp <- value

      private$instantiate_members()

      } else {
        super$tiledb_timestamp
      }

    },

    #' @field members_instantiated Have the members been instantiated?
    #'
    members_instantiated = function(value) {

      private$check_object_exists()

      if (!missing(value)) {
        private$check_read_only("members_instantiated")
      }

      if (is.null(private$.members_instantiated)) {
        private$.members_instantiated <- FALSE
      }

      private$.members_instantiated

    }
  ),

  private = list(

    # @field Query for instantiated members
    #
    .members_instantiated = NULL,

    # @description Instantiate group members.
    #
    instantiate_members = function() {

      members <- private$.member_cache

      dev_null <- lapply(members, function(.m) {

        obj <- if (is.null(.m$object)) {
          private$log_debug0("instantiate_members", "Constructing member '{}' type {}", .m$name, .m$type)
          obj <- private$construct_member(.m$uri, .m$type)
        } else {
          .m$object
        }

        if (!obj$is_open()) {
          obj$open(self$mode)

        } else {
          obj$reopen(self$mode)
        }

        private$log_debug0("instantiate_members", "Adding cached member '{}' type {}", .m$name, .m$type)

        # Explicitly add the new member to member_cache
        private$add_cache_member(.m$name, obj)
      })

      invisible(NULL)
    },

    # @description Get cached 'tbl_keys' array object
    #
    keys_array = function() {
    # TODO: should we check for cached first?
      self$members$tbl_keys$object
    },

    # @description Get cached 'tbl_data' array object
    #
    data_array = function() {
      self$members$tbl_data$object
    },

    # @description Query 'tbl_keys' array by single attribute
    #
    query_hash = function(key, namespace) {

      p <- storr::join_key_namespace(key, namespace)
      arrobj <- private$keys_array()

      # Slice array
      sp <- list(namespace = namespace, key = key)
      arr <- arrobj$object
      tiledb::attrs(arr) <- "hash"
      tiledb::selected_points(arr) <- sp
      tiledb::return_as(arr) <- "arrow"

      # arr <- arrobj$tiledb_array(attrs = "hash",
      #                            selected_points = sp,
      #                            return_as = "arrow",
      #                            ctx = self$ctx)


      dta <- data.table::as.data.table(arr[], key = c("namespace", "key"))
      dta[.(namespace, key), "hash", with = FALSE,
             env = list(namespace = I(namespace), key = I(key))][[1]]

    }
  ) # private
)
