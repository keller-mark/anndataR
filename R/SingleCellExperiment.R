# nolint start: line_length_linter
# Sources used to understand how to convert between SingleCellExperiment and AnnData
# https://bioconductor.org/packages/release/bioc/vignettes/SingleCellExperiment/inst/doc/intro.html#2_Creating_SingleCellExperiment_instances
# https://bioconductor.org/packages/3.20/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html#column-sample-data
# https://github.com/ivirshup/sc-interchange/issues
# https://github.com/ivirshup/sc-interchange/issues/2
# https://www.bioconductor.org/packages/devel/bioc/vignettes/SingleCellExperiment/inst/doc/intro.html#3_Adding_low-dimensional_representations
# nolint end: line_length_linter

#' Convert an AnnData object to a SingleCellExperiment object
#'
#' `to_SingleCellExperiment()` converts an AnnData object
#'   to a SingleCellExperiment object.
#'
#' @param adata an AnnData object, e.g., InMemoryAnnData
#'
#' @param assays_mapping A named list mapping `layers` in `adata` to
#' `assay` names in the created SingleCellExperiment object.
#' The names of the list should be the names of the `assays` in the
#' resulting SingleCellExperiment object, and the values should be the names
#' of the `layers` in `adata`, and should include the `X` matrix as well.
#' If `X` is not in the list, it will be added as `counts` or `data`.
#' @param colData_mapping a named list mapping `obs` in `adata` to
#' `colData` in the created SingleCellExperiment object.
#' The names of the list should be the names of the `colData` columns in the
#' resulting SingleCellExperiment object. The values of the list should be the
#' names of the `obs` columns in `adata`.
#' @param rowData_mapping a named list mapping `var` names in `adata` to
#' `rowData` in the created SingleCellExperiment object. The names of the list
#' should be the names of the `rowData` columns in the resulting SingleCellExperiment
#' object. The values of the list should be the names of the `var` columns in `adata`.
#' @param reduction_mapping a named list mapping reduction names in `adata` to
#' reduction names in the created SingleCellExperiment object.
#' The names of the list should be the names of the `reducedDims` in the resulting
#' SingleCellExperiment object.
#' The values of the list should also be a named list with the following keys:
#' - `obsm`: the name of the `obsm` slot in `adata`
#' - `varm`: the name of the `varm` slot in `adata`
#' - `uns`: the name of the `uns` slot in `adata`
#' @param colPairs_mapping a named list mapping obsp names in `adata` to
#' colPairs names in the created SingleCellExperiment object.
#' The names of the list should be the names of the `colPairs` in the resulting
#' SingleCellExperiment object. The values of the list should be the names of the
#' `obsp` in `adata`.
#' @param rowPairs_mapping a named list mapping varp names in `adata` to
#' rowPairs names in the created SingleCellExperiment object.
#' The names of the list should be the names of the `rowPairs` in the resulting
#' SingleCellExperiment object. The values of the list should be the names of the
#' `varp` in `adata`.
#' @param metadata_mapping a named list mapping uns names in `adata` to
#' metadata names in the created SingleCellExperiment object.
#' The names of the list should be the names of the `metadata` in the resulting
#' SingleCellExperiment object. The values of the list should be the names of the
#' `uns` in `adata`.
#'
#' @return `to_SingleCellExperiment()` returns a SingleCellExperiment
#'   representing the content of `adata`.
#'
#' @examples
#' if (interactive()) {
#'   ## useful when interacting with the SingleCellExperiment !
#'   library(SingleCellExperiment)
#' }
#' ad <- AnnData(
#'   X = matrix(1:5, 3L, 5L),
#'   layers = list(
#'     A = matrix(5:1, 3L, 5L),
#'     B = matrix(letters[1:5], 3L, 5L)
#'   ),
#'   obs = data.frame(row.names = LETTERS[1:3], cell = 1:3),
#'   var = data.frame(row.names = letters[1:5], gene = 1:5)
#' )
#'
#' ## construct a SingleCellExperiment from an AnnData object
#' sce <- to_SingleCellExperiment(ad)
#' sce
#' @export
# nolint start: cyclocomp_linter
to_SingleCellExperiment <- function(
  # nolint end: cyclocomp_linter
  adata,
  assays_mapping = NULL,
  colData_mapping = NULL, # nolint
  rowData_mapping = NULL, # nolint
  reduction_mapping = NULL,
  colPairs_mapping = NULL, # nolint
  rowPairs_mapping = NULL, # nolint
  metadata_mapping = NULL
) {
  check_requires(
    "Converting AnnData to SingleCellExperiment",
    "SingleCellExperiment",
    "Bioc"
  )

  if (!(inherits(adata, "AbstractAnnData"))) {
    cli_abort(
      "{.arg adata} must be a {.cls AbstractAnnData} but has class {.cls {class(adata)}}"
    )
  }

  # guess mappings if not provided
  if (is.null(assays_mapping)) {
    assays_mapping <- to_SCE_guess_assays(adata)
  }
  if (is.null(colData_mapping)) {
    colData_mapping <- to_SCE_guess_all(adata, "obs") # nolint
  }
  if (is.null(rowData_mapping)) {
    rowData_mapping <- to_SCE_guess_all(adata, "var") # nolint
  }
  if (is.null(reduction_mapping)) {
    reduction_mapping <- to_SCE_guess_reduction(adata)
  }
  if (is.null(colPairs_mapping)) {
    colPairs_mapping <- to_SCE_guess_all(adata, "obsp") # nolint
  }
  if (is.null(rowPairs_mapping)) {
    rowPairs_mapping <- to_SCE_guess_all(adata, "varp") # nolint
  }
  if (is.null(metadata_mapping)) {
    metadata_mapping <- to_SCE_guess_all(adata, "uns")
  }

  # trackstatus: class=SingleCellExperiment, feature=get_X, status=done
  # trackstatus: class=SingleCellExperiment, feature=get_layers, status=done
  sce_assays <- vector("list", length(assays_mapping))
  names(sce_assays) <- names(assays_mapping)
  for (i in seq_along(assays_mapping)) {
    from <- assays_mapping[[i]]
    to <- names(assays_mapping)[[i]]
    if (from != "X") {
      sce_assays[[to]] <- to_R_matrix(adata$layers[[from]])
    } else {
      sce_assays[[to]] <- to_R_matrix(adata$X)
    }
  }

  # construct colData
  # FIXME: probably better way to make a dataframe from a list of vectors
  # trackstatus: class=SingleCellExperiment, feature=get_obs, status=done
  # trackstatus: class=SingleCellExperiment, feature=get_obs_names, status=done
  col_data <- .to_SCE_process_simple_mapping(adata, colData_mapping, "obs")

  # construct rowData
  # trackstatus: class=SingleCellExperiment, feature=get_var, status=done
  # trackstatus: class=SingleCellExperiment, feature=get_var_names, status=done
  row_data <- .to_SCE_process_simple_mapping(adata, rowData_mapping, "var")

  # construct reducedDims
  # trackstatus: class=SingleCellExperiment, feature=get_reductions, status=wip
  reduceddims <- vector("list", length(reduction_mapping))
  names(reduceddims) <- names(reduction_mapping)
  for (i in seq_along(reduction_mapping)) {
    name <- names(reduction_mapping)[[i]]
    reduction <- reduction_mapping[[i]]

    obsm_key <- reduction$obsm
    varm_key <- reduction$varm
    uns_key <- reduction$uns

    reduceddims[[name]] <- .to_SingleCellExperiment_process_reduction(
      adata,
      name,
      obsm_key,
      varm_key,
      uns_key
    )
  }

  # construct colPairs
  # trackstatus: class=SingleCellExperiment, feature=get_obsp, status=done
  col_pairs <- .to_SCE_process_simple_mapping(adata, colPairs_mapping, "obsp")

  # construct rowPairs
  # trackstatus: class=SingleCellExperiment, feature=get_varp, status=done
  row_pairs <- .to_SCE_process_simple_mapping(adata, rowPairs_mapping, "varp")

  # construct metadata
  # trackstatus: class=SingleCellExperiment, feature=get_uns, status=done
  metadata <- .to_SCE_process_simple_mapping(adata, metadata_mapping, "uns")

  arguments <- list(
    assays = sce_assays,
    colPairs = col_pairs,
    rowPairs = row_pairs,
    metadata = metadata,
    checkDimnames = TRUE
  )
  # add col_data if not empty list
  if (length(col_data) > 0) {
    arguments$colData <- as(col_data, "DataFrame")
  }
  # add row_data if not empty list
  if (length(row_data) > 0) {
    arguments$rowData <- as(row_data, "DataFrame")
  }

  # construct output object
  sce <- do.call(SingleCellExperiment::SingleCellExperiment, arguments)
  rownames(sce) <- rownames(adata$var)
  colnames(sce) <- rownames(adata$obs)

  SingleCellExperiment::reducedDims(sce) <- reduceddims # only here to ensure that the dimensions are right

  sce
}

# nolint start: object_length_linter object_name_linter
to_SCE_guess_assays <- function(adata) {
  # nolint end: object_length_linter object_name_linter
  if (!(inherits(adata, "AbstractAnnData"))) {
    cli_abort(
      "{.arg adata} must be a {.cls AbstractAnnData} but has class {.cls {class(adata)}}"
    )
  }

  layers <- list()

  if (!is.null(adata$X)) {
    layer_name_for_x <-
      if (!"counts" %in% names(adata$layers)) {
        # could expand checks, to check if integers
        "counts"
      } else {
        "data"
      }
    layers[[layer_name_for_x]] <- "X"
  }

  for (layer_name in names(adata$layers)) {
    layers[[layer_name]] <- layer_name
  }

  layers
}

# nolint start: object_length_linter object_name_linter
to_SCE_guess_all <- function(adata, slot) {
  # nolint end: object_length_linter object_name_linter
  if (!(inherits(adata, "AbstractAnnData"))) {
    cli_abort(
      "{.arg adata} must be a {.cls AbstractAnnData} but has class {.cls {class(adata)}}"
    )
  }

  mapping <- names(adata[[slot]])
  names(mapping) <- names(adata[[slot]])

  mapping
}

# nolint start: object_length_linter object_name_linter
to_SCE_guess_reduction <- function(adata) {
  # nolint end: object_length_linter object_name_linter
  .to_Seurat_guess_reductions(adata) # nolint object_usage_linter
}

# nolint start: object_length_linter object_name_linter
.to_SCE_process_simple_mapping <- function(adata, mapping, slot) {
  # nolint end: object_length_linter object_name_linter
  # check if mapping contains all columns of slot
  if (length(setdiff(names(adata[[slot]]), names(mapping))) == 0) {
    adata[[slot]]
  } else {
    mapped <- lapply(seq_along(mapping), function(i) {
      adata[[slot]][[mapping[[i]]]]
    })
    names(mapped) <- names(mapping)
    mapped
  }
}

# nolint start: object_length_linter object_name_linter
.to_SingleCellExperiment_process_reduction <- function(
  # nolint end: object_length_linter object_name_linter
  adata,
  key,
  obsm_key,
  varm_key,
  uns_key
) {
  # nolint
  embedding <- adata$obsm[[obsm_key]]

  if (is.null(embedding)) {
    cli_abort(
      c(
        "{.val {obsm_key}} is not an item in {.code adata$obsm}",
        "i" = "{.code adata$obsm_keys()}: {.val {adata$obsm_keys()}}"
      )
    )
  }

  rownames(embedding) <- adata$obs_names

  if (!is.null(varm_key) && varm_key %in% names(adata$varm)) {
    loadings <- adata$varm[[varm_key]]
    rownames(loadings) <- colnames(embedding)

    metadata <- list()
    if (!is.null(uns_key) && uns_key %in% names(adata$uns)) {
      metadata <- adata$uns[[uns_key]]
    }

    lem <- SingleCellExperiment::LinearEmbeddingMatrix(
      sampleFactors = embedding,
      featureLoadings = loadings,
      metadata = metadata
    )
    rownames(lem) <- rownames(embedding)
    colnames(lem) <- colnames(loadings)
    lem
  } else {
    embedding
  }
}

#' Convert a SingleCellExperiment object to an AnnData object
#'
#' `from_SingleCellExperiment()` converts a
#'   SingleCellExperiment to an AnnData object.
#'
#' @param sce An object inheriting from SingleCellExperiment.
#'
#' @param output_class Name of the AnnData class. Must be one of `"HDF5AnnData"`
#' or `"InMemoryAnnData"`.
#'
#' @param x_mapping Name of the assay in `sce` to use as the `X` matrix in the AnnData object.
#' @param layers_mapping A named list mapping `assay` names in `sce` to `layers` in the created AnnData object.
#' The names of the list should be the names of the `layers` in the resulting AnnData object, and the values should be
#' the names of the `assays` in the `sce` object.
#' @param obs_mapping A named list mapping `colData` in `sce` to `obs` in the created AnnData object.
#' The names of the list should be the names of the `obs` columns in the resulting AnnData object. The values of the
#' list should be the names of the `colData` columns in `sce`.
#' @param var_mapping A named list mapping `rowData` in `sce` to `var` in the created AnnData object.
#' The names of the list should be the names of the `var` columns in the resulting AnnData object. The values of the
#' list should be the names of the `rowData` columns in `sce`.
#' @param obsm_mapping A named list mapping `reducedDim` in `sce` to `obsm` in the created AnnData object.
#' The names of the list should be the names of the `obsm` in the resulting AnnData object. The values of the list
#' should be a named list with as key the name of the `obsm` slot in the resulting AnnData object, and as value a list
#' with the following elements
#' - `reducedDim`
#' - the name of the `reducedDim` in `sce`
#' @param varm_mapping A named list mapping `reducedDim` in `sce` to `varm` in the created AnnData object.
#' The names of the list should be the names of the `varm` in the resulting AnnData object. The values of the list
#' should be a named list with as key the name of the `varm` slot in the resulting AnnData object, and as value a
#' list with the following elements
#' - `reducedDim`
#' - the name of the `reducedDim` in `sce`, that is `LinearEmbeddingMatrix` of which you want the featureLoadings to
#' end up in the `varm` slot
#' @param obsp_mapping A named list mapping `colPairs` in `sce` to `obsp` in the created AnnData object.
#' The names of the list should be the names of the `obsp` in the resulting AnnData object. The values of the list
#' should be the names of the `colPairs` in `sce`.
#' @param varp_mapping A named list mapping `rowPairs` in `sce` to `varp` in the created AnnData object.
#' The names of the list should be the names of the `varp` in the resulting AnnData object. The values of the list
#' should be the names of the `rowPairs` in `sce`.
#' @param uns_mapping A named list mapping `metadata` in `sce` to `uns` in the created AnnData object.
#' The names of the list should be the names of the `uns` in the resulting AnnData object. The values of the list
#' should be the names of the `metadata` in `sce`.
#' @param ... Additional arguments to pass to the generator function.
#'
#' @return `from_SingleCellExperiment()` returns an AnnData object
#'   (e.g., InMemoryAnnData) representing the content of `sce`.
#'
#' @examples
#' ## construct an AnnData object from a SingleCellExperiment
#' library(SingleCellExperiment)
#' sce <- SingleCellExperiment(
#'   assays = list(counts = matrix(1:5, 5L, 3L)),
#'   colData = DataFrame(cell = 1:3, row.names = paste0("Cell", 1:3)),
#'   rowData = DataFrame(gene = 1:5, row.names = paste0("Gene", 1:5))
#' )
#' from_SingleCellExperiment(sce, "InMemory")
#'
#' @export
# nolint start: object_name_linter
from_SingleCellExperiment <- function(
  # nolint end: object_name_linter
  sce,
  output_class = c("InMemory", "HDF5AnnData"),
  x_mapping = NULL,
  layers_mapping = NULL,
  obs_mapping = NULL,
  var_mapping = NULL,
  obsm_mapping = NULL,
  varm_mapping = NULL,
  obsp_mapping = NULL,
  varp_mapping = NULL,
  uns_mapping = NULL,
  ...
) {
  check_requires(
    "Converting SingleCellExperiment to AnnData",
    "SingleCellExperiment",
    "Bioc"
  )

  output_class <- match.arg(output_class)

  if (!(inherits(sce, "SingleCellExperiment"))) {
    cli_abort(
      "{.arg sce} must be a {.cls SingleCellExperiment} but has class {.cls {sce}}"
    )
  }

  # For any mappings that are not set, using the guessing function
  layers_mapping <- layers_mapping %||% .from_SCE_guess_layers(sce, x_mapping)
  obs_mapping <- obs_mapping %||%
    .from_SCE_guess_all(
      sce,
      SingleCellExperiment::colData
    )
  var_mapping <- var_mapping %||%
    .from_SCE_guess_all(
      sce,
      SingleCellExperiment::rowData
    )
  obsm_mapping <- obsm_mapping %||% .from_SCE_guess_obsm(sce)
  varm_mapping <- varm_mapping %||% .from_SCE_guess_varm(sce)
  obsp_mapping <- obsp_mapping %||%
    .from_SCE_guess_obspvarp(
      sce,
      SingleCellExperiment::colPairs
    )
  varp_mapping <- varp_mapping %||%
    .from_SCE_guess_obspvarp(
      sce,
      SingleCellExperiment::rowPairs
    )
  uns_mapping <- uns_mapping %||% .from_SCE_guess_all(sce, S4Vectors::metadata)

  generator <- get_anndata_constructor(output_class)
  adata <- generator$new(shape = rev(dim(sce)), ...)

  # Fill in slots in the object
  .from_SCE_process_obs(adata, sce, obs_mapping)

  .from_SCE_process_var(adata, sce, var_mapping)

  # trackstatus: class=SingleCellExperiment, feature=set_X, status=wip
  if (!is.null(x_mapping)) {
    adata$X <- .from_SCE_convert(
      SummarizedExperiment::assay(sce, x_mapping, withDimnames = FALSE)
    )
  }

  .from_SCE_process_layers(adata, sce, layers_mapping)

  .from_SCE_process_obsm(adata, sce, obsm_mapping)

  .from_SCE_process_varm(adata, sce, varm_mapping)

  .from_SCE_process_obsp(adata, sce, obsp_mapping)

  .from_SCE_process_varp(adata, sce, varp_mapping)

  .from_SCE_process_uns(adata, sce, uns_mapping)

  adata
}

# nolint start: object_length_linter object_name_linter
.from_SCE_guess_all <- function(sce, slot) {
  # nolint end: object_length_linter object_name_linter
  mapping <- names(slot(sce))
  names(mapping) <- names(slot(sce))

  mapping
}

# nolint start: object_length_linter object_name_linter
.from_SCE_guess_layers <- function(sce, x_mapping) {
  # nolint end: object_length_linter object_name_linter
  layers_mapping <- list()

  for (assay_name in names(SummarizedExperiment::assays(sce))) {
    if (is.null(x_mapping) || assay_name != x_mapping) {
      layers_mapping[[assay_name]] <- assay_name
    }
  }

  layers_mapping
}

# nolint start: object_length_linter object_name_linter
.from_SCE_guess_obsm <- function(sce) {
  # nolint end: object_length_linter object_name_linter
  if (!inherits(sce, "SingleCellExperiment")) {
    return(list())
  }
  obsm_mapping <- list()

  for (reduction_name in names(SingleCellExperiment::reducedDims(sce))) {
    obsm_mapping[[reduction_name]] <- c("reducedDim", reduction_name)
  }

  obsm_mapping
}

# nolint start: object_length_linter object_name_linter
.from_SCE_guess_varm <- function(sce) {
  # nolint end: object_length_linter object_name_linter
  if (!inherits(sce, "SingleCellExperiment")) {
    return(list())
  }
  varm_mapping <- list()

  for (reduction_name in names(SingleCellExperiment::reducedDims(sce))) {
    reduction <- SingleCellExperiment::reducedDim(sce, reduction_name)
    if (inherits(reduction, "LinearEmbeddingMatrix")) {
      varm_mapping[[reduction_name]] <- c("reducedDim", reduction_name)
    }
  }

  varm_mapping
}

# If sce is a SummarizedExperiment, return an empty mapping
# nolint start: object_length_linter object_name_linter
.from_SCE_guess_obspvarp <- function(sce, slot) {
  # nolint end: object_length_linter object_name_linter
  if (!inherits(sce, "SingleCellExperiment")) {
    return(list())
  }
  .from_SCE_guess_all(sce, slot)
}

# Convert BioConductor-specific objects to base R objects
# Convert matrices
# Convert dgCMatrix to dgRMatrix
# nolint start: object_length_linter object_name_linter
.from_SCE_convert <- function(object, transpose = TRUE) {
  # nolint end: object_length_linter object_name_linter
  if (inherits(object, "DataFrame")) {
    as.data.frame(object)
  } else if (inherits(object, "SimpleList")) {
    as.list(object)
  } else if (inherits(object, "matrix") || inherits(object, "Matrix")) {
    if (inherits(object, "denseMatrix")) {
      object <- as.matrix(object)
    }
    if (transpose) {
      to_py_matrix(object)
    } else {
      object
    }
  } else {
    object
  }
}

# trackstatus: class=SingleCellExperiment, feature=set_obs_names, status=wip
# trackstatus: class=SingleCellExperiment, feature=set_obs, status=wip
# nolint start: object_name_linter
.from_SCE_process_obs <- function(adata, sce, obs_mapping) {
  # nolint end: object_name_linter

  if (!rlang::is_empty(obs_mapping)) {
    adata$obs <- SummarizedExperiment::colData(sce) |>
      as.data.frame() |>
      setNames(names(obs_mapping))
  } else {
    # Store an empty data.frame to keep the obs names
    if (is.null(colnames(sce))) {
      adata$obs <- data.frame(matrix(nrow = ncol(sce), ncol = 0))
    } else {
      adata$obs <- data.frame(row.names = colnames(sce))
    }
  }
}

# trackstatus: class=SingleCellExperiment, feature=set_var_names, status=wip
# trackstatus: class=SingleCellExperiment, feature=set_var, status=wip
# nolint start: object_name_linter
.from_SCE_process_var <- function(adata, sce, var_mapping) {
  # nolint end: object_name_linter

  if (!rlang::is_empty(var_mapping)) {
    adata$var <- SummarizedExperiment::rowData(sce) |>
      as.data.frame() |>
      setNames(names(var_mapping))
  } else {
    # Store an empty data.frame to keep the var names
    if (is.null(rownames(sce))) {
      adata$var <- data.frame(matrix(nrow = nrow(sce), ncol = 0))
    } else {
      adata$var <- data.frame(row.names = rownames(sce))
    }
  }
}

# trackstatus: class=SingleCellExperiment, feature=set_layers, status=wip
# nolint start: object_length_linter object_name_linter
.from_SCE_process_layers <- function(adata, sce, layers_mapping) {
  # nolint end: object_length_linter object_name_linter
  if (rlang::is_empty(layers_mapping)) {
    return(invisible())
  }

  adata$layers <- purrr::map(layers_mapping, function(.layer) {
    .from_SCE_convert(SummarizedExperiment::assay(sce, .layer))
  })
}

# trackstatus: class=SingleCellExperiment, feature=set_obsm, status=wip
# nolint start: object_name_linter
.from_SCE_process_obsm <- function(adata, sce, obsm_mapping) {
  # nolint end: object_name_linter
  if (rlang::is_empty(obsm_mapping)) {
    return(invisible())
  }

  adata$obsm <- purrr::imap(obsm_mapping, function(.mapping, .idx) {
    if (!is.character(.mapping) || length(.mapping) != 2) {
      cli_abort(c(
        paste(
          "Each item in {.arg obsm_mapping} must be a {.cls character}",
          "vector of length 2"
        ),
        "i" = paste(
          "{.code obsm_mapping[[{.val { .idx }}]]} is",
          "{.obj_type_friendly { .mapping }}"
        )
      ))
    }

    slot <- .mapping[1]
    key <- .mapping[2]

    if (slot != "reducedDim") {
      cli_abort(c(
        "The first item in each {.arg obsm_mapping} must be {.val reducedDim}",
        "i" = "{.code obsm_mapping[[{.val { .idx }}]][1]}: {.val {slot}}"
      ))
    }

    reduction <- SingleCellExperiment::reducedDim(sce, key)
    if (inherits(reduction, "LinearEmbeddingMatrix")) {
      SingleCellExperiment::sampleFactors(reduction)
    } else {
      reduction
    }
  })
}

# trackstatus: class=SingleCellExperiment, feature=set_varm, status=wip
# nolint start: object_name_linter
.from_SCE_process_varm <- function(adata, sce, varm_mapping) {
  # nolint end: object_name_linter
  if (rlang::is_empty(varm_mapping)) {
    return(invisible())
  }

  adata$varm <- purrr::imap(varm_mapping, function(.mapping, .idx) {
    if (!is.character(.mapping) || length(.mapping) != 2) {
      cli_abort(c(
        paste(
          "Each item in {.arg varm_mapping} must be a {.cls character}",
          "vector of length 2"
        ),
        "i" = paste(
          "{.code varm_mapping[[{.val { .idx }}]]} is",
          "{.obj_type_friendly { .mapping }}"
        )
      ))
    }

    slot <- .mapping[1]
    key <- .mapping[2]

    if (slot != "reducedDim") {
      cli_abort(c(
        "The first item in each {.arg varm_mapping} must be {.val reducedDim}",
        "i" = "{.code varm_mapping[[{.val { .idx }}]][1]}: {.val {slot}}"
      ))
    }

    reduction <- SingleCellExperiment::reducedDim(sce, key)
    if (!inherits(reduction, "LinearEmbeddingMatrix")) {
      cli_abort(paste(
        "{.code reducedDim(sce, {.val key}} must be a {.cls LinearEmbeddingMatrix}",
        "but has class {.cls {reduction}}"
      ))
    }

    SingleCellExperiment::featureLoadings(reduction)
  })
}

# trackstatus: class=SingleCellExperiment, feature=set_obsp, status=wip
# nolint start: object_length_linter object_name_linter
.from_SCE_process_obsp <- function(adata, sce, obsp_mapping) {
  # nolint end: object_length_linter object_name_linter
  if (rlang::is_empty(obsp_mapping)) {
    return(invisible())
  }

  adata$obsp <- purrr::map(obsp_mapping, function(.obsp) {
    .from_SCE_convert(
      SingleCellExperiment::colPair(sce, .obsp, asSparse = TRUE),
      transpose = FALSE
    )
  })
}

# trackstatus: class=SingleCellExperiment, feature=set_varp, status=wip
# nolint start: object_length_linter object_name_linter
.from_SCE_process_varp <- function(adata, sce, varp_mapping) {
  # nolint end: object_length_linter object_name_linter
  if (rlang::is_empty(varp_mapping)) {
    return(invisible())
  }

  adata$varp <- purrr::map(varp_mapping, function(.varp) {
    .from_SCE_convert(
      SingleCellExperiment::rowPair(sce, .varp, asSparse = TRUE),
      transpose = FALSE
    )
  })
}

# trackstatus: class=SingleCellExperiment, feature=set_uns, status=wip
# nolint start: object_length_linter object_name_linter
.from_SCE_process_uns <- function(adata, sce, uns_mapping) {
  # nolint end: object_length_linter object_name_linter
  if (rlang::is_empty(uns_mapping)) {
    return(invisible())
  }

  adata$uns <- purrr::map(uns_mapping, function(.uns) {
    S4Vectors::metadata(sce)[[.uns]]
  })
}
