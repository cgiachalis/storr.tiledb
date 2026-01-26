
#' @importFrom R6.tiledb TileDBGroup TileDBArray

#' @title Generate a `CAS` Object
#'
#' @description An R6 class for creating a content addressable storage.
#'
#' @returns A `CAS` object.
#'
#' @export
CAS <- R6::R6Class(
  inherit = TileDBGroup,
  classname = "CAS",

  public = list(
   #' @description Open `CAS` object for read or write.
   #'
   #' @param compression_level Set an integer value for ZSTD compression level applied
   #' in data objects. (experimental)
   #' @param algo Select a hash algorithm to be used.
   #'
   #' @return The object, invisibly
   #'
   create = function(compression_level = -7, algo = NULL) {

     if (self$exists()) {
       cli::cli_abort("R6Class: {.cls {self$class()}} object already exists.", call = NULL)
     }

     uri_root <- self$uri
     uri_keys <- file_path(uri_root, "tbl_keys")
     uri_data <- file_path(uri_root, "tbl_data")

     # Default to 'md5' hash algorithm
     if (is.null(algo)) {
       algo <- "md5"
     }

     algo <- validate_hash_algo(algo)

     super$create(mode = "WRITE")

     ok1 <- tiledb::tiledb_array_create(uri_keys,
                                        schema = schema_keys())
     ok2 <- tiledb::tiledb_array_create(uri_data,
                                        schema = schema_data(compression_level))

     arr1 <- TileDBArray$new(uri_keys)
     arr2 <- TileDBArray$new(uri_data)

     self$set_member(arr1)
     self$set_member(arr2)

     self$set_metadata(list(hash_algo = algo))
     private$.hash_algo <- algo

     super$reopen("WRITE")

     invisible(self)
   },

   #' @description Open `CAS` object for read or write.
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

     algo <- self$get_metadata("hash_algo")

     # Case where 'hash_algo' key is not  present
     #
     if (is.null(algo)) {

       warning("Hash algorithim not found, defaulting to 'md5'")
       algo <- "md5"

       if (mode != "WRITE") {
         self$reopen("WRITE")
       }

       self$set_metadata(list(hash_algo = algo))

       # Flush metadata
       self$reopen(mode)

     }

     private$.hash_algo <- algo

     if (instantiate) {
       private$instantiate_members()
     }

     invisible(self)

   },

   #' @description Delete CAS.
   #'
   #' @return The object, invisibly.
   #'
   delete_cas = function() {

     self$close()

     duri <- tiledb::tiledb_object_rm(self$uri, ctx = self$ctx)

     private$.mode <- NULL
     private$.object_type <- NULL

     invisible(self)
   },

   #' @description Print directory contents.
   #'
   #' @return A character vector with file paths, invisibly.
   #'
   dir_tree = function() {
     R6.tiledb::vfs_dir_tree(self$uri, vfs = private$vfs())
   },

   #' @description Dump the Storr structure to string.
   #'
   #'
   #' @return A `character` string, invisibly.
   #'
   dump = function() {
     super$dump("Storr Directory")
   }
  ),

  active = list(

    #' @field hash_algorithm Hash algorithm
    #'
    hash_algorithm = function(value) {

      private$check_object_exists()

      if (!missing(value)) {

        private$check_scalar_character(value)
        value <- validate_hash_algo(value)

        mode <- self$mode

        if (mode != "WRITE") {
          on.exit({self$reopen(mode)})
          self$reopen("WRITE")
        }

        self$set_metadata(list(hash_algo = value))
        private$.hash_algo <- value

      }

      private$.hash_algo
    },

    #' @field size Return directory size
    #'
    size = function(value) {

      private$check_object_exists()

      if (!missing(value)) {
        private$check_read_only("size")
      }

      R6.tiledb::vfs_size(self$uri, vfs = private$vfs())
    }
  ),

  private = list(

    # @field Hash algorithm to be used
    #
    .hash_algo = NULL,

    # @field Cache a 'tiledb_vfs' object
    #
    .vfs = NULL,

    # @description Get the 'tiledb_vfs' object
    #
    vfs = function() {
      if (is.null(private$.vfs)) {
        private$.vfs <- tiledb::tiledb_vfs(ctx = self$ctx)
      }
      private$.vfs
    },

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
        private$add_cached_member(.m$name, obj)
      })

      invisible(NULL)
    },

    # @description Get cached 'tbl_keys' array object
    #
    keys_array = function() {

      self$members$tbl_keys$object

    },

    # @description Get cached 'tbl_data' array object
    #
    data_array = function() {

      self$members$tbl_data$object

    }

  ) # private
)
