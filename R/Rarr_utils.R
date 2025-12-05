#' create_zarr_group
#'
#' create zarr groups
#'
#' @param store the location of (zarr) store
#' @param name name of the group
#' @param version zarr version
#' @importFrom utils tail
#' @examples
#' store <- tempfile(fileext = ".zarr")
#' create_zarr(store)
#' create_zarr_group(store, "gp")
#' @export
#' @return `NULL`
create_zarr_group <- function(store, name, version = "v2") {
  split.name <- strsplit(name, split = "\\/")[[1]]
  if (length(split.name) > 1) {
    split.name <- vapply(
      seq_along(split.name),
      function(x) paste(split.name[seq_len(x)], collapse = "/"),
      FUN.VALUE = character(1)
    )
    split.name <- rev(tail(split.name, 2))
    if (!dir.exists(file.path(store, split.name[2]))) {
      create_zarr_group(store = store, name = split.name[2])
    }
  }
  dir.create(file.path(store, split.name[1]), showWarnings = FALSE)
  switch(
    version,
    v2 = {
      write(
        "{\"zarr_format\":2}",
        file = file.path(store, split.name[1], ".zgroup")
      )
    },
    v3 = {
      stop("Currently only zarr v2 is supported!")
    },
    stop("only zarr v2 is supported. Use version = 'v2'")
  )
}

#' create_zarr
#'
#' create zarr store
#'
#' @param store the location of zarr store
#' @param version zarr version
#' @examples
#' store <- tempfile(fileext = ".zarr")
#' create_zarr(store)
#' @export
#' @return `NULL`
create_zarr <- function(store, version = "v2") {
  prefix <- basename(store)
  dir <- gsub(paste0(prefix, "$"), "", store)
  create_zarr_group(store = dir, name = prefix, version = version)
}

#' create_zarr
#'
#' create zarr store
#'
#' @param store the location of zarr store
#' @examples
#' store <- tempfile(fileext = ".zarr")
#' create_zarr(store)
#' is_zarr_empty(store)
#' @export
#' @return returns TRUE if zarr store is not empty
is_zarr_empty <- function(store) {
  files <- list.files(store, recursive = FALSE, full.names = FALSE)
  all(files %in% c(".zarray", ".zattrs", ".zgroup"))
}
