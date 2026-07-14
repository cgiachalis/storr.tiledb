
#  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
#   R6 Classes - Storr Schemas
#  -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --

#' @title Generate a `SchemaBase` Object
#'
#' @description A virtual base class to be inherited by specialised R6 schema classes.
#'
#' Provides common schema functionality for 'keys' and 'data' schemas for CAS
#' storage. This class manages filter lists for coordinates, offsets, and validity
#' data and controls tile capacity, cell order, and tile order settings.
#'
#' The schema creation comes either from existing CAS storage URI path or the
#' default templates.
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

    #' @field coords_flist Get or set the filter list for the array's coordinates.
    #'
    coords_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("COORDS_FLIST", value)
      } else {
        private$COORDS_FLIST
      }
    },

    #' @field offsets_flist Get or set the filter list for the array's variable-
    #' length attribute offsets.
    #'
    offsets_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("OFFSETS_FLIST", value)
      } else {
        private$OFFSETS_FLIST
      }
    },

    #' @field validity_flist Get or set the filter list for the array's validity.
    #'
    validity_flist = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("VALIDITY_FLIST", value)
      } else {
        private$VALIDITY_FLIST
      }
    },

    #' @field capacity Get or set the array capacity of each tile.
    #'
    capacity = function(value) {
      if (!missing(value)) {

        if (!.is_scalar_numeric(value)) {
          stop("'capacity' should be a scalar numeric value.", call. = FALSE)
        }

        private$CAPACITY <- value
        private$.update_schema()
      } else {
        private$CAPACITY
      }
    },

    #' @field cell_order Get or set the cell order layout of the array.
    #'
    cell_order = function(value) {
      if (!missing(value)) {
       if (!value %in% c("ROW_MAJOR", "COL_MAJOR")) {
         stop("'cell_order' should be a either 'ROW_MAJOR' or 'COL_MAJOR'.", call. = FALSE)
       }
        private$CELL_ORDER <- value
        private$.update_schema()
      } else {
        private$CELL_ORDER
      }
    },

    #' @field tile_order Get or set the tile order layout of the array.
    #'
    tile_order = function(value) {
      if (!missing(value)) {
        if (!value %in% c("ROW_MAJOR", "COL_MAJOR")) {
          stop("'tile_order' should be a either 'ROW_MAJOR' or 'COL_MAJOR'.", call. = FALSE)
        }
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
    #' @param uri `r sch_uri`
    #' @param ctx `r sch_ctx`
    #' @param none_filter `r sch_none_filter`
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


    #' @description Get TileDB Schema.
    #'
    schema = function() {

      private$SCHEMA
    },

    #' @description Print Schema class.
    #'
    print = function() {
      cat(paste0("R6Class: <", class(self)[1], ">"))
      invisible(self)
    }

  ),

  private = list(

    # Schema-level filters (COORDS, OFFSETS, VALIDITY) are stored here and intended
    # for getters/setters (can't modify in-place as for Dims/Attrs).
    #
    # NB: Dimension/attribute filters are stored on the dimension/attribute objects themselves.
    # So, child classes will store dims/attrs in private fields and their filters will be retrieved
    # or modified in-place from.
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
        stop("Not a filter list.", call. = FALSE)
      }

      if (update_schema) {
        private$.update_schema()
      }

      return(NULL)
    },

    # NB: Technically, we're re-creating the schema
    .update_schema = function() {


      if (inherits(self, "SchemaData")) {
        dom <- tiledb::tiledb_domain(private$DIM_HASH)
        attrs <-  private$ATTR_VALUE
      } else if (inherits(self, "SchemaKeys")) {
        dom <- tiledb::tiledb_domain(c(private$DIM_NS, private$DIM_KEY))
        attrs <-  c(private$ATTR_HASH, private$ATTR_EXP, private$ATTR_NOTE)
      } else{
        stop("Invalid schema class.",call. = FALSE)
      }

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


#' @title Generate a `SchemaKeys` Object
#'
#' @description An R6 class that represents the 'keys' schema for CAS storage
#' and provides active fields to get/set filter lists for each dimension/attribute.
#'
#' This class should not be used directly, but it can be accessed via [driver_schemas()].
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

    #' @field attr_expires_at Get or set a filter list.
    #'
    attr_expires_at = function(value) {
      if (!missing(value)) {
        private$.set_filter_list("ATTR_EXP", value)
      } else {
        tiledb::filter_list(private$ATTR_EXP)
      }
    },

    #' @field attr_notes Get or set a filter list.
    #'
    attr_notes = function(value) {
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
    ATTR_NOTE = NULL

  )
)


#' @title Generate a `SchemaData` Object
#'
#' @description An R6 class that represents the 'data' schema for CAS storage
#' and provides active fields to get/set filter lists for each dimension/attribute.
#'
#' This class should not be used directly, but it can be accessed via [driver_schemas()].
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
    ATTR_VALUE = NULL
  )
)


#' @title Generate a `TileDBDriverSchemas` Object
#'
#' @description An R6 class that represents the storr's CAS schemas and provides
#' access to both [SchemaKeys] and [SchemaData] objects.
#'
#' Users can use the schema objects to access and modify individual schemas' filter
#' lists and configuration via active fields.
#'
#' For creating a `TileDBDriverSchemas` object, use the convenient wrapper
#' [driver_schemas()].
#'
#' ## Structure
#'
#' `TileDBDriverSchemas` holds `SchemaKeys` and `SchemaData` as active bindings
#' that can be accessed and modifiled in-place. Then, the modified `TileDBDriverSchemas`
#' can be passed to driver creation method.
#'
#' ```
#' SchemaBase (abstract foundation)
#' ├── SchemaKeys (keys/index schema)
#' └── SchemaData (data/payload schema)
#'
#' TileDBDriverSchemas (factory/container)
#'
#' ```
#'
#' @returns A `TileDBDriverSchemas`, `R6` object.
#'
#' @export
#'
#' @keywords internal
#'
TileDBDriverSchemas <- R6::R6Class(
  classname = "TileDBDriverSchemas",
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

    #' @description Create a new `TileDBDriverSchemas` object.
    #'
    #' @param uri Optional URI path to `TileDB` driver. If not given,  the default
    #' schemas array will be used.
    #' @param ctx `r sch_ctx`
    #' @param none_filter `r sch_none_filter`
    #'
    initialize = function(uri = NULL, ctx = NULL, none_filter = FALSE) {

      private$CTX <- ctx

      if (!is.null(uri)) {
        check_uri(uri)

        private$URI_KEYS <- file_path(uri, "tbl_keys")
        private$URI_DATA <- file_path(uri, "tbl_data")
      }

      private$NONE_FILTER <- none_filter
    },

    #' @description Print class.
    #'
    print = function() {
      cat(paste0("R6Class: <", class(self)[1], ">"))
      invisible(self)
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



#' TileDB Driver Schemas
#'
#' Tune TileDB's performance and storage characteristics: compression algorithms,
#' compression levels, tile capacity, cell order, and tile order settings.
#'
#' For example, use `driver_schemas()` to:
#'
#' - Create schemas with optional filters
#' - Dynamically apply compression to individual attributes
#' - Persist those customizations into the underlying TileDB arrays
#'
#' This is useful for creating a storage driver for use cases that need
#' different trade-offs (speed vs. compression, memory vs. disk).
#'
#' @param uri Optional URI path to `TileDB` driver. If not given, the default
#' template array schemas will be used.
#' @param ctx `r sch_ctx`
#' @param none_filter `r sch_none_filter`
#'
#' @returns An object of class [TileDBDriverSchemas].
#'
#' @export
#'
#' @examples
#' ctx <- new_context()
#' sto_sch <- driver_schemas(ctx = ctx, none_filter = TRUE)
#'
#' # 'data' schema
#' data_sch <- sto_sch$SchemaData
#'
#' # Set up ZSTD filter with high compression
#' flt <- tiledb::tiledb_filter("ZSTD", ctx = ctx)
#' flt <- tiledb::tiledb_filter_set_option(flt,"COMPRESSION_LEVEL", 22)
#' fl_list <- tiledb::tiledb_filter_list(flt, ctx = ctx)
#'
#' # Apply filter list to 'value' attribute (CAS storage data)
#' data_sch$attr_value <- fl_list
#'
#' # Now, 'sto_sch' has been modified
#' sto_sch$SchemaData$schema()
#'
driver_schemas <- function(uri = NULL, ctx = NULL, none_filter = FALSE) {

  TileDBDriverSchemas$new(uri = uri, ctx = ctx, none_filter = none_filter)

}
