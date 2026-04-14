#' @title Generate a `StorrFragments` Object
#'
#' @description
#' A class for working with Storr TileDB Fragments.
#'
#' @returns An object of class `StorrFragments`, `R6`.
#'
#' @export
#'
#' @keywords internal
#'
StorrFragments <- R6::R6Class(
  classname = "StorrFragments",
  cloneable = FALSE,
  public = list(

    #' @description Create a new `StorrFragments` instance.
    #'
    #' @param uri URI path for TileDB Storr.
    #' @param ctx Optional [tiledb::tiledb_ctx()] object.
    #'
    initialize = function(uri, ctx = NULL) {

      if (missing(uri)) {
        cli::cli_abort("{.arg uri} argument is missing.", call = NULL)
      }

      dr <- driver_tiledb(uri, context = ctx)

      private$.tiledb_uri <- dr$uri
      uri_keys <- dr$members$tbl_keys$uri
      uri_data <- dr$members$tbl_data$uri
      dr$close()

      # Set context
      if (is.null(ctx)) {
        ctx <- R6.tiledb::new_context()
      }

      check_tiledb_ctx(ctx)

      private$.tiledb_ctx <- ctx
      private$.storr_uris <- list(uri_keys = uri_keys,
                                  uri_data = uri_data)

    },

    #' @description Consolidates the 'storr' fragments.
    #'
    #' Consolidation in TileDB merges multiple array fragments into a single
    #' fragment to improve query performance by reducing the number of files that
    #' need to be read during queries.
    #'
    #' The consolidation process is not deleting the old fragments.
    #' To clear the consolidated fragments after the process, set
    #' `vacuum = TRUE` which will invoke the vacuum process. Alternatively,
    #' use the class method `$vacuum()`.
    #'
    #'
    #' @param what Which array should be consolidated? Defaults to `"all"` arrays.
    #' @param cfg A configuration object [tiledb::tiledb_config()] to override context
    #'  configuration.
    #'  When `NULL` (default) the configuration parameters will be retrieved from
    #'  object's context.
    #' @param vacuum Should the old fragments (consolidated) be deleted? Default is `FALSE`.
    #' @param async Should it consolidate asynchronously? Default is `FALSE`.
    #'
    #' @return When `async = FALSE`, it returns `TRUE` for successful consolidation.
    #' For `async = TRUE`, it returns a [mirai::mirai()] object immediately; once
    #' resolved, it returns `TRUE` indicating consolidation success.
    #'
    consolidate = function(what = c("all", "keys", "data"),
                           cfg = NULL,
                           vacuum = FALSE,
                           async = FALSE) {

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$.tiledb_ctx)
      }

      check_tiledb_config(cfg)

      what  <- match.arg(what)

      if (!async) {

        ctxptr <- private$.tiledb_ctx@ptr

        switch(what,
               all = {

                 uri_keys <- private$.storr_uris$uri_keys
                 uri_data <- private$.storr_uris$uri_data

                .libtiledb_array_consolidate(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                .libtiledb_array_consolidate(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                if (vacuum) {
                  .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                  .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)
                }

               },
               keys = {

                 uri_keys <- private$.storr_uris$uri_keys

                 .libtiledb_array_consolidate(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)

                 if (vacuum) {
                   .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                 }
               },
               data = {

                 uri_data <- private$.storr_uris$uri_data

                 .libtiledb_array_consolidate(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                 if (vacuum) {
                  .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)
                 }
               })

        private$.fragments_keys <- NULL
        private$.fragments_data <- NULL

        return(TRUE)

      } else {

        m <- mirai::mirai({

          cfg <- tiledb::tiledb_config(config_params)
          ctx <- R6.tiledb::new_context(cfg)
          ctxptr <- ctx@ptr

          # switch which array
          switch(what,
                 all = {

                   uri_keys <- uris$uri_keys
                   uri_data <- uris$uri_data

                   storr.tiledb:::.libtiledb_array_consolidate(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                   storr.tiledb:::.libtiledb_array_consolidate(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                   if (vacuum) {
                     storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                     storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)
                   }

                 },
                 keys = {

                   uri_keys <- uris$uri_keys

                   storr.tiledb:::.libtiledb_array_consolidate(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)

                   if (vacuum) {
                    storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                   }
                 },
                 data = {

                   uri_data <- uris$uri_data

                   storr.tiledb:::.libtiledb_array_consolidate(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                   if (vacuum) {
                     storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)
                   }
                 })

          return(TRUE)

        }, uris = private$.storr_uris,
           config_params = as.vector(cfg),
           what = what,
           vacuum = vacuum,
           .compute = NULL) # Ephemeral

        private$.fragments_keys <- NULL
        private$.fragments_data <- NULL

        return(m)

      }
    },

    #' @description Vacuum Storr fragments
    #'
    #' This operation deletes the old fragments (consolidated).
    #'
    #' @param what Which array to vacuum? Defaults to `"all"` arrays.
    #' @param cfg A configuration object [tiledb::tiledb_config()] to override context
    #'  configuration.
    #'  When `NULL` (default) the configuration parameters will be retrieved from
    #'  object's context.
    #' @param async Should it vacuum asynchronously? Default is `FALSE`.
    #'
    #' @return When `async = FALSE`, it returns `TRUE` for successful vacuuming.
    #' For `async = TRUE`, it returns a [mirai::mirai()] object immediately; once
    #' resolved, it returns `TRUE` indicating vacuum success.
    #'
    vacuum = function(what = c("all", "keys", "data"),
                      cfg = NULL,
                      async = FALSE) {

      if (is.null(cfg)) {
        cfg <- tiledb::config(private$.tiledb_ctx)
      }

      check_tiledb_config(cfg)

      what  <- match.arg(what)

      if (!async) {

        ctxptr <- private$.tiledb_ctx@ptr

        switch(what,
               all = {

                 uri_keys <- private$.storr_uris$uri_keys
                 uri_data <- private$.storr_uris$uri_data

                .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

               },
               keys = {

                 uri_keys <- private$.storr_uris$uri_keys

                .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)

               },
               data = {

                 uri_data <- private$.storr_uris$uri_data

                 .libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

               })

        private$.fragments_keys <- NULL
        private$.fragments_data <- NULL

        return(TRUE)

      } else {

        m <- mirai::mirai({

          cfg <- tiledb::tiledb_config(config_params)

          ctx <- R6.tiledb::new_context(cfg)
          ctxptr <- ctx@ptr

          # switch which array
          switch(what,
                 all = {

                   uri_keys <- uris$uri_keys
                   uri_data <- uris$uri_data

                  storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)
                  storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                 },
                 keys = {

                   uri_keys <- uris$uri_keys

                   storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_keys, cfgptr = cfg@ptr)

                 },
                 data = {

                   uri_data <- uris$uri_data

                   storr.tiledb:::.libtiledb_array_vacuum(ctx = ctxptr, uri = uri_data, cfgptr = cfg@ptr)

                })

          return(TRUE)

        }, uris = private$.storr_uris,
        config_params = as.vector(cfg),
        what = what,
        .compute = NULL) # Ephemeral

        private$.fragments_keys <- NULL
        private$.fragments_data <- NULL

        return(m)
      }
    },

    #' @description Get the number of fragments.
    #'
    #' @return A numeric value.
    #'
    frag_num = function() {

      nk <- self$fob_keys$frag_num()
      nd <- self$fob_data$frag_num()

      nk + nd

    },

    #' @description Return the number of fragments to vacuum.
    #'
    #' @return A numeric value.
    #'
    to_vacuum_num = function() {

      nk <- self$fob_keys$to_vacuum_num()
      nd <- self$fob_data$to_vacuum_num()

      nk + nd

    },

    #' @description Refresh the Storr's Fragment Info objects.
    #'
    #' @return The object, invisibly.
    #'
    reload_finfo = function() {

      self$fob_keys$reload_finfo()
      self$fob_data$reload_finfo()

      invisible(self)
    },

    #' @description Print Fragments class.
    #'
    print = function() {
      res <- cli::cli_fmt({
        cli::cli_inform("R6Class: {.cls {class(self)[1]}}")
        cli::cli_bullets(c(">" = "URI Basename: {.emph {basename(private$.tiledb_uri)}}"))
      }, collapse = TRUE)

      cat(res)

      invisible(self)
    }

  ),

  active = list(

    #' @field fob_keys Retrieve the [TileDBFragments] instance for `keys`
    #' array.
    #'
    fob_keys = function(value) {

      if (!missing(value)) {
        check_read_only("fob_keys")
      }

      if (is.null(private$.fragments_keys)) {
        uri <- private$.storr_uris$uri_keys
        private$.fragments_keys <- R6.tiledb::TileDBFragments$new(uri, ctx = private$.tiledb_ctx)
      }

      private$.fragments_keys
    },

    #' @field fob_data Retrieve the [TileDBFragments] instance for `data`
    #' array.
    #'
    fob_data = function(value) {

      if (!missing(value)) {
        check_read_only("fob_data")
      }

      if (is.null(private$.fragments_data)) {
        uri <- private$.storr_uris$uri_data
        private$.fragments_data <- R6.tiledb::TileDBFragments$new(uri, ctx = private$.tiledb_ctx)
      }

      private$.fragments_data
    },

    #' @field size Return directory size.
    #'
    size = function(value) {

      if (!missing(value)) {
        check_read_only("size")
      }

      R6.tiledb::vfs_size(private$.tiledb_uri, vfs = private$vfs())
    }
  ),

  private = list(

    .tiledb_uri = NULL,
    .tiledb_ctx = NULL,
    .storr_uris  = NULL,

    # Cache 'TileDBFragments'
    .fragments_keys = NULL,
    .fragments_data = NULL,

    # @field Cache a 'tiledb_vfs' object
    #
    .vfs = NULL,

    # @description Get the 'tiledb_vfs' object
    #
    vfs = function() {
      if (is.null(private$.vfs)) {
        private$.vfs <- tiledb::tiledb_vfs(ctx = private$.tiledb_ctx)
      }
      private$.vfs
    }

  ) # private
)
