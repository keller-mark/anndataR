#' Write H5AD
#'
#' Write an H5AD file
#'
#' @param object The object to write, either a "SingleCellExperiment" or a
#' "Seurat" object
#' @param path Path of the file to write to
#' @param compression The compression algorithm to use when writing the
#'  HDF5 file. Can be one of `"none"`, `"gzip"` or `"lzf"`. Defaults to
#' `"none"`.
#' @param mode The mode to open the HDF5 file.
#'
#'   * `a` creates a new file or opens an existing one for read/write.
#'   * `r+` opens an existing file for read/write.
#'   * `w` creates a file, truncating any existing ones
#'   * `w-`/`x` are synonyms creating a file and failing if it already exists.
#'
#' @return `path` invisibly
#' @export
#'
#' @examples
#' adata <- AnnData(
#'   X = matrix(1:5, 3L, 5L),
#'   layers = list(
#'     A = matrix(5:1, 3L, 5L),
#'     B = matrix(letters[1:5], 3L, 5L)
#'   ),
#'   obs = data.frame(row.names = LETTERS[1:3], cell = 1:3),
#'   var = data.frame(row.names = letters[1:5], gene = 1:5)
#' )
#' h5ad_file <- tempfile(fileext = ".h5ad")
#' adata$write_h5ad(h5ad_file)
#'
#' # Write a SingleCellExperiment as an H5AD
#' if (requireNamespace("SingleCellExperiment", quietly = TRUE)) {
#'   ncells <- 100
#'   counts <- matrix(rpois(20000, 5), ncol = ncells)
#'   logcounts <- log2(counts + 1)
#'
#'   pca <- matrix(runif(ncells * 5), ncells)
#'   tsne <- matrix(rnorm(ncells * 2), ncells)
#'
#'   sce <- SingleCellExperiment::SingleCellExperiment(
#'     assays = list(counts = counts, logcounts = logcounts),
#'     reducedDims = list(PCA = pca, tSNE = tsne)
#'   )
#'
#'   adata <- from_SingleCellExperiment(sce)
#'   h5ad_file <- tempfile(fileext = ".h5ad")
#'   adata$write_h5ad(h5ad_file)
#' }
#'
#' # Write a Seurat as a H5AD
#' if (requireNamespace("Seurat", quietly = TRUE)) {
#'   library(Seurat)
#'
#'   counts <- matrix(1:15, 5L, 3L)
#'   dimnames(counts) <- list(
#'     LETTERS[1:5],
#'     letters[1:3]
#'   )
#'   cell.metadata <- data.frame(
#'     row.names = letters[1:3],
#'     cell = 1:3
#'   )
#'   obj <- CreateSeuratObject(counts, meta.data = cell.metadata)
#'   gene.metadata <- data.frame(
#'     row.names = LETTERS[1:5],
#'     gene = 1:5
#'   )
#'   obj[["RNA"]] <- AddMetaData(GetAssay(obj), gene.metadata)
#'
#'   adata <- from_Seurat(obj)
#'   h5ad_file <- tempfile(fileext = ".h5ad")
#'   adata$write_h5ad(h5ad_file)
#' }
write_h5ad <- function(
  object,
  path,
  compression = c("none", "gzip", "lzf"),
  mode = c("w-", "r", "r+", "a", "w", "x")
) {
  mode <- match.arg(mode)
  adata <-
    if (inherits(object, "SingleCellExperiment")) {
      from_SingleCellExperiment(
        object,
        output_class = "HDF5AnnData",
        file = path,
        compression = compression,
        mode = mode
      )
    } else if (inherits(object, "Seurat")) {
      from_Seurat(
        object,
        output_class = "HDF5AnnData",
        file = path,
        compression = compression,
        mode = mode
      )
    } else if (inherits(object, "AbstractAnnData")) {
      object$to_HDF5AnnData(
        path,
        compression = compression,
        mode = mode
      )
    } else {
      cli_abort("Unable to write object of class {.cls {class(object)}}")
    }
  adata$close()
  rm(adata)
  gc()

  invisible(path)
}
