
#  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#   R6 Classes - CAS Schemas
#  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

#' @title Generate a `SchemaBase` Object
#'
#' @description A virtual class to be inherited by specialised R6 schema classes.
#'
#' This class should not be used directly.
#'
#' @returns A `SchemaBase`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
SchemaBase <- R6::R6Class(
  classname = "SchemaBase",
  active = list(

    #' @field coords_flist Get or set a filter list.
    #'
    coords_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("COORDS_FLIST", value)
      } else {
        private$COORDS_FLIST
      }
    },

    #' @field offsets_flist Get or set a filter list.
    #'
    offsets_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("OFFSETS_FLIST", value)
      } else {
        private$OFFSETS_FLIST
      }
    },

    #' @field validity_flist Get or set a filter list.
    #'
    validity_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("VALIDITY_FLIST", value)
      } else {
        private$VALIDITY_FLIST
      }
    },

    #' @field capacity Get or set capacity of each tile.
    #'
    capacity = function(value) {
      if (!missing(value)) {
        private$CAPACITY <- value
        private$.update_schema()
      } else {
        private$CAPACITY
      }
    },

    #' @field cell_order Get or set cell order layout.
    #'
    cell_order = function(value) {
      if (!missing(value)) {
        private$CELL_ORDER <- value
        private$.update_schema()
      } else {
        private$CELL_ORDER
      }
    },

    #' @field tile_order Get or set tile order layout.
    #'
    tile_order = function(value) {
      if (!missing(value)) {
        private$TILE_ORDER <- value
        private$.update_schema()
      } else {
        private$TILE_ORDER
      }
    }
  ),

  public = list(

    #' @description Create a new `SchemaBase` object.
    #'
    #' @param uri Optional URI path to `TileDB` driver.
    #' @param ctx Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object.
    #' @param none_filter  `TRUE` for no filters, `FALSE` for default filters.
    #' Applied on default schemas and not on schemas extracted from uri path.
    #'
    initialize = function(uri = NULL, ctx = NULL, none_filter = FALSE) {

      if (is.null(ctx)) {
        ctx <- R6.tiledb::new_context()
      }

      private$CTX <- ctx

      if (is.null(uri)) {
        if(none_filter) {
          compression_level <- NULL
        } else {
          compression_level <- -7
        }

        if (inherits(self, "SchemaData")) {
          private$SCHEMA <- schema_data(ctx = private$CTX, compression_level = compression_level)
        } else if (inherits(self, "SchemaKeys")) {
          private$SCHEMA <- schema_keys(ctx = private$CTX, compression_level = compression_level)
        } else{
          stop("Invalid schema class.",call. = FALSE)
        }

      } else {
        private$SCHEMA <- tiledb::schema(uri)
      }

      sch_flist <- tiledb::filter_list(private$SCHEMA)

      private$COORDS_FLIST <- sch_flist$coords
      private$OFFSETS_FLIST <- sch_flist$offsets
      private$VALIDITY_FLIST <- sch_flist$validity

      private$CAPACITY <- tiledb::capacity(private$SCHEMA)
      private$CELL_ORDER <- tiledb::cell_order(private$SCHEMA)
      private$TILE_ORDER <- tiledb::tile_order(private$SCHEMA)

    },

    #' @description Return TileDB schema.
    #'
    schema = function() {
      private$SCHEMA
    }

    # print = function() {
    #   # tiledb::tiledb_schema_get_names(sch)
    #   # tiledb::tiledb_schema_get_types(sch)
    # }

  ),

  private = list(

    COORDS_FLIST = NULL,
    OFFSETS_FLIST = NULL,
    VALIDITY_FLIST = NULL,

    CAPACITY = NULL,
    CELL_ORDER = NULL,
    TILE_ORDER = NULL,

    CTX = NULL,
    SCHEMA = NULL,

    .set_filter_list = function(key, value, update_schema = TRUE) {
      is_sch_flist <- key %in% c("COORDS_FLIST", "OFFSETS_FLIST", "VALIDITY_FLIST")

      if (inherits(value, "tiledb_filter_list")) {
        if (!is_sch_flist) {
          tiledb::filter_list(private[[key]]) <- value
        } else {
          private[[key]] <- value
        }

      } else if (is.null(value) || is.na(value)) {
        flist_none <- tiledb::tiledb_filter_list(tiledb::tiledb_filter("NONE", ctx = private$CTX))

        if (!is_sch_flist) {
          tiledb::filter_list(private[[key]]) <- flist_none
        } else {
          private[[key]] <- flist_none
        }

      } else {
        stop("Not a filter list", call. = FALSE)
      }

      if (update_schema) {
        private$.update_schema()
      }

      return(NULL)
    }

  )
)


#' @title Generate a `SchemaKeys` Object
#'
#' @description An R6 class that represents the 'keys' schema of CAS storage.
#'
#' This class should not be used directly.
#'
#' @returns A `SchemaKeys`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
SchemaKeys <- R6::R6Class(
  classname = "SchemaKeys",
  inherit = SchemaBase,
  active = list(

    #' @field dim_namespace Get or set a filter list.
    #'
    dim_namespace = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("DIM_NS", value)
      } else {
        tiledb::filter_list(private$DIM_NS)
      }
    },

    #' @field dim_key Get or set a filter list.
    #'
    dim_key = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("DIM_KEY", value)
      } else {
        tiledb::filter_list(private$DIM_KEY)
      }
    },

    #' @field attr_hash Get or set a filter list.
    #'
    attr_hash = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("ATTR_HASH", value)
      } else {
        tiledb::filter_list(private$ATTR_HASH)
      }
    },

    #' @field attr_expiry_at Get or set a filter list.
    #'
    attr_expiry_at = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("ATTR_EXP", value)
      } else {
        tiledb::filter_list(private$ATTR_EXP)
      }
    },

    #' @field attr_note Get or set a filter list.
    #'
    attr_note = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("ATTR_NOTE", value)
      } else {
        tiledb::filter_list(private$ATTR_NOTE)
      }
    }

  ),

  public = list(

    #' @description Create a new `SchemaKeys` object.
    #'
    #' @param uri Optional URI path to `TileDB` driver.
    #' @param ctx Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object.
    #' @param none_filter  `TRUE` for no filters, `FALSE` for default filters.
    #' Applied on default schemas and not on schemas extracted from uri path.
    #'
    initialize = function(uri = NULL, ctx = NULL, none_filter = FALSE) {

      super$initialize(uri = uri, ctx = ctx, none_filter = none_filter)

      dom <- tiledb::domain(private$SCHEMA)
      dims <- tiledb::dimensions(dom)

      for (i in seq_along(dims)) {
        if (tiledb::name(dims[[i]]) == "namespace") {
          private$DIM_NS <- dims[[i]]
        } else if (tiledb::name(dims[[i]]) == "key") {
          private$DIM_KEY <- dims[[i]]
        }
      }

      attrs <- tiledb::attrs(private$SCHEMA)

      private$ATTR_HASH <- attrs$hash
      private$ATTR_EXP <- attrs$expires_at
      private$ATTR_NOTE <- attrs$notes
    }
  ),

  private = list(

    DIM_NS = NULL,
    DIM_KEY = NULL,
    ATTR_HASH = NULL,
    ATTR_EXP = NULL,
    ATTR_NOTE = NULL,

    # NB: Technically, we're re-creating the schema
    .update_schema = function() {
      dom <- tiledb::tiledb_domain(c(private$DIM_NS, private$DIM_KEY))

      attrs <-  c(private$ATTR_HASH, private$ATTR_EXP, private$ATTR_NOTE)

      sch <- tiledb::tiledb_array_schema(
        domain = dom,
        attrs = attrs,
        cell_order = private$CELL_ORDER,
        tile_order = private$TILE_ORDER,
        capacity = private$CAPACITY,
        sparse = TRUE,
        allows_dups = FALSE,
        coords_filter_list = private$COORDS_FLIST,
        offsets_filter_list = private$OFFSETS_FLIST,
        validity_filter_list = private$VALIDITY_FLIST,
        ctx = private$CTX
      )

      if (!tiledb::schema_check(sch)) {
        stop('Failed to update schema', call. = FALSE)
      }

      private$SCHEMA <- sch

      return(NULL)
    }

  )
)


#' @title Generate a `SchemaData` Object
#'
#' @description An R6 class that represents the 'data' schema of CAS storage.
#'
#' This class should not be used directly.
#'
#' @returns A `SchemaData`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
SchemaData <- R6::R6Class(
  classname = "SchemaData",
  inherit = SchemaBase,
  active = list(

    #' @field dim_hash Get or set a filter list.
    #'
    dim_hash = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("DIM_HASH", value)
      } else {
        tiledb::filter_list(private$DIM_HASH)
      }
    },

    #' @field attr_value Get or set a filter list.
    #'
    attr_value = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("ATTR_VALUE", value)
      } else {
        tiledb::filter_list(private$ATTR_VALUE)
      }
    }

  ),

  public = list(

    #' @description Create a new `SchemaData` object.
    #'
    #' @param uri Optional URI path to `TileDB` driver.
    #' @param ctx Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object.
    #' @param none_filter  `TRUE` for no filters, `FALSE` for default filters.
    #' Applied on default schemas and not on schemas extracted from uri path.
    #'
    initialize = function(uri = NULL, ctx = NULL, none_filter = FALSE) {

      super$initialize(uri = uri, ctx = ctx, none_filter = none_filter)

      dom <- tiledb::domain(private$SCHEMA)
      dims <- tiledb::dimensions(dom)

      private$DIM_HASH <- dims[[1]]

      attrs <- tiledb::attrs(private$SCHEMA)

      private$ATTR_VALUE <- attrs$value

    }
  ),

  private = list(

    DIM_HASH = NULL,
    ATTR_VALUE = NULL,

    # NB: Technically, we're re-creating the schema
    .update_schema = function() {

      dom <- tiledb::tiledb_domain(private$DIM_HASH)
      attrs <-  private$ATTR_VALUE

      sch <- tiledb::tiledb_array_schema(
        domain = dom,
        attrs = attrs,
        cell_order = private$CELL_ORDER,
        tile_order = private$TILE_ORDER,
        capacity = private$CAPACITY,
        sparse = TRUE,
        allows_dups = FALSE,
        coords_filter_list = private$COORDS_FLIST,
        offsets_filter_list = private$OFFSETS_FLIST,
        validity_filter_list = private$VALIDITY_FLIST,
        ctx = private$CTX
      )

      if (!tiledb::schema_check(sch)) {
        stop('Failed to update schema', call. = FALSE)
      }

      private$SCHEMA <- sch

      return(NULL)
    }

  )
)


#' @title Generate a `StorrSchemas` Object
#'
#' @description An R6 class that represents the storr's CAS schemas.
#'
#'
#' @returns A `StorrSchemas`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
StorrSchemas <- R6::R6Class(
  classname = "StorrSchemas",
  active = list(

    #' @field SchemaKeys Get [SchemaKeys()] object.
    #'
    SchemaKeys = function(value) {

      if (!missing(value)) {
        check_read_only("SchemaKeys")
      }

      if (is.null(private$SCH_KEYS)) {
        private$SCH_KEYS <- SchemaKeys$new(private$URI_KEYS,
                                           ctx = private$CTX,
                                           none_filter = private$NONE_FILTER)
      }

      private$SCH_KEYS

    },

    #' @field SchemaData Get [SchemaData()] object.
    #'
    SchemaData = function(value) {

      if (!missing(value)) {
        check_read_only("SchemaData")
      }

      if (is.null(private$SCH_DATA)) {
        private$SCH_DATA <- SchemaData$new(private$URI_DATA,
                                           ctx = private$CTX,
                                           none_filter = private$NONE_FILTER)
      }

      private$SCH_DATA

    }
  ),

  public = list(

    #' @description Create a new `StorrSchemas` object.
    #'
    #' @param uri Optional URI path to `TileDB` driver.
    #' @param ctx Optional \link[tiledb:tiledb_ctx]{tiledb_ctx} object.
    #' @param none_filter `TRUE` for no filters, `FALSE` for default filters.
    #' Applied on default schemas and not on schemas extracted from uri path.
    #'
    initialize = function(uri = NULL, ctx = NULL, none_filter = FALSE) {


      private$CTX <- ctx

      if (!is.null(uri)) {
        check_uri(uri)

        private$URI_KEYS <- file_path(uri, "tbl_keys")
        private$URI_DATA <- file_path(uri, "tbl_data")
      }

      private$NONE_FILTER <- none_filter
    }
  ),

  private = list(

    SCH_KEYS = NULL,
    SCH_DATA = NULL,

    URI_KEYS = NULL,
    URI_DATA = NULL,
    NONE_FILTER = NULL,
    CTX = NULL
  )
)
