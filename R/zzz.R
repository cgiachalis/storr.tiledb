# nocov start
.onLoad <- function(libname, pkgname) {
  tiledb::set_allocation_size_preference(3 * 1024 * 1024)
}
# nocov end
