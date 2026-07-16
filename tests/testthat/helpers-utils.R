# helper to get array/schema filters
.schema_filters <- function(sch) {

  dom <- tiledb::domain(sch)
  dims <- tiledb::dimensions(dom)
  names(dims) <- sapply(dims, tiledb::name)
  cols <- c(dims, tiledb::attrs(sch))


  out <- vector("list", length(cols))
  names(out) <- names(cols)

  for (i in seq_along(cols)) {

    flist <- tiledb::filter_list(cols[[i]])
    n <- tiledb::nfilters(flist)

    out[i] <- lapply(0:c(n-1), function(.f) {
      flt <- flist[.f]
      flt_type <- tiledb::tiledb_filter_type(flt)

      lvl <- tryCatch( tiledb::tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"),
                       error = function(e) NULL)

      c(flt_type, lvl)
    })

  }

  sch_filters <- tiledb::filter_list(sch)
  out_sch <- vector("list", length(sch_filters))
  names(out_sch) <- names(sch_filters)

  for (i in seq_along(sch_filters)) {

    flist <- sch_filters[[i]]
    n <- tiledb::nfilters(flist)
    out_sch[i] <- lapply(0:c(n-1), function(.f) {
      flt <- flist[.f]
      flt_type <- tiledb::tiledb_filter_type(flt)

      lvl <- tryCatch( tiledb::tiledb_filter_get_option(flt, "COMPRESSION_LEVEL"),
                       error = function(e) NULL)

      c(flt_type, lvl)
    })

  }

  data.frame(c(out, out_sch))
}
