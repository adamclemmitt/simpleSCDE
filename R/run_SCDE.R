#' Run single-cell differential expression testing on a specified data frame of
#' cells and genes
#'
#' @param df A data frame
#' @param labels A vector containing labels indicating if certain cells
#'   positively or negatively express personal genes
#' @param write_to_txt Boolean indicating whether results should be written to
#'   an external text file (default: FALSE)
#' @param file_name Full file name to use for document containing test results
#' @param ncores Number of cores to utilize (default: 1)
#' @param min_genes_detected Minimum number of genes detected in a cell. Cells
#'   with fewer genes will be removed (default: 1.8e3)
#' @param min_reads_per_gene Minimum number of reads per gene. Genes with fewer
#'   reads will be removed (default: 10)
#' @param min_cells_gene_in Minimum number of cells a gene must be seen in.
#'   Genes not seen in a sufficient number of cells will be removed (default: 5)
#' @return data frame containing upper bound(ub), maximum likelihood
#'   estimate(mle), upper bound(ub), conservative estimate(ce), Z-score(Z), and
#'   cZ-score(cZ) values
#' @examples
#' slit_posneg <- pos_neg.label(df = es.mef.small,
#'                              rowname = "dd_Smed.v4_12111_0_1")
#' run_SCDE(df = Fincher, labels = slit_posneg, file_name = "slitResults.txt",
#'          min_genes_detected = 10)
#'
#' @export
run_SCDE <- function(df, labels, write_to_txt = FALSE,
                     file_name = "results.txt", ncores = 1,
                     min_genes_detected = 1800, min_reads_per_gene = 10,
                     min_cells_gene_in = 5) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("scde")
  library(scde)

  # Issue with newer bioconductor scde releases and compatibility with 'flexmix'
  # solved here from online forums:
  # from https://github.com/hms-dbmi/scde/issues/40

  # In R
  require(devtools)
  install_version("flexmix", version = "2.3-13",
                  repos = "http://cran.us.r-project.org")
  require(devtools)
  devtools::install_github("hms-dbmi/scde", build_vignettes = FALSE)

  # Factor determining cell types
  sg <- as.factor(labels)
  # Ensure that two groups of cells were generated
  if (nlevels(sg) != 2) {
    stop("Entered labels did not specify two groups. Enter labels for cells of
         two differing groups.")
  }
  print(names(sg))
  # Calculate new data frame in order of sg(colnames)
  message("Calculating new data frame . . .")
  adjusted_df <- colselect(df = df, colchars = names(sg), df_order = FALSE)
  message("df is as follows: ")
  # Clean up the dataset
  cd <- clean.counts(adjusted_df, min.lib.size = min_genes_detected,
                     min.reads = min_reads_per_gene,
                     min.detected = min_cells_gene_in)

  # Check if clean.counts parameters are too harsh
  if (ncol(cd) < ncol(df)) {
    stop("Input for min_genes_detected is too strict(high). Please
         readjust(lower) min_genes_detected.")
  }
  if (nrow(cd) == 0) {
    stop("Parameters min_reads_per_gene and min_cells_gene_in are too
         strict(high) and have eliminated all rows. Please readjust(lower) one
         or both of these parameters.")
  }

  # Calculate models
  o.ifm <- scde.error.models(counts = cd, groups = sg, n.cores = ncores,
                             threshold.segmentation = TRUE,
                             save.crossfit.plots = FALSE,
                             save.model.plots = FALSE, verbose = 1)
  # Filter out cells that don't show positive correlation with the expected
  # expression magnitudes (very poor fits)
  valid.cells <- o.ifm$corr.a > 0
  o.ifm <- o.ifm[valid.cells, ]
  # Estimate gene expression prior
  o.prior <- scde.expression.prior(models = o.ifm, counts = cd,
                                   length.out = 400, show.plot = FALSE)

  # Define two groups of cells
  groups <- as.factor(labels)
  names(groups) <- row.names(o.ifm)
  # Run differential expression tests on all genes
  ediff <- scde.expression.difference(o.ifm, cd, o.prior, groups = groups,
                                      n.randomizations = 100, n.cores = ncores,
                                      verbose = 1)
  # Check if results should be written to external file
  if (write_to_txt) {
    # write out a table with all the results, showing most significantly
    # different genes (in both directions) on top
    write.table(ediff[order(abs(ediff$Z), decreasing = TRUE), ],
                file = file_name, row.names = TRUE, col.names = TRUE,
                sep = "\t", quote = FALSE)
  }
  return(ediff)
}
