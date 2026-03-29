skip_if_no_anndata_py()
skip_if_no_zarr()

library(reticulate)

ad <- reticulate::import("anndata", convert = FALSE)
zr <- reticulate::import("zarr", convert = FALSE)
bi <- reticulate::import_builtins()

known_issues <- read_known_issues()

# first generate a python zarr
adata_py <- ad$AnnData()

name <- "empty"

# create a couple of paths
file_py <- withr::local_file(
  tempfile(paste0("anndata_py_", name), fileext = ".zarr")
)
file_r <- withr::local_file(
  tempfile(paste0("anndata_r_", name), fileext = ".zarr")
)

# write to file
adata_py$write_zarr(file_py)
# Read it back in to get the version as read from disk
adata_py <- ad$read_zarr(file_py)

test_that(paste0("Reading an AnnData with layer '", name, "' works"), {
  msg <- message_if_known(
    backend = "ZarrAnnData",
    slot = c("none"),
    dtype = name,
    process = "read",
    known_issues = known_issues
  )
  skip_if(!is.null(msg), message = msg)

  adata_r <- read_zarr(file_py, as = "ZarrAnnData")
  expect_equal(
    adata_r$shape(),
    unlist(reticulate::py_to_r(adata_py$shape))
  )

  # check that the print output is the same (normalize class names)
  str_r <- capture.output(print(adata_r))
  str_py <- capture.output(print(adata_py))
  str_r <- gsub("[^ ]*AnnData", "AnnData", str_r)
  expect_equal(str_r, str_py)
})

gc()

test_that(paste0("Writing an AnnData with layer '", name, "' works"), {
  msg <- message_if_known(
    backend = "ZarrAnnData",
    slot = c("none"),
    dtype = name,
    process = c("read", "write"),
    known_issues = known_issues
  )
  skip_if(!is.null(msg), message = msg)

  adata_r <- read_zarr(file_py, as = "InMemoryAnnData")
  write_zarr(adata_r, file_r)

  # read from file
  adata_py2 <- ad$read_zarr(file_r)

  # check that the print output is the same
  expect_equal(
    unlist(reticulate::py_to_r(adata_py2$shape)),
    unlist(reticulate::py_to_r(adata_py$shape))
  )

  # check that the print output is the same
  str_py2 <- capture.output(print(adata_py2))
  str_py <- capture.output(print(adata_py))
  expect_equal(str_py2, str_py)
})
