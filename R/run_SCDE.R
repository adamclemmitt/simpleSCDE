#' Run single-cell differential expression testing on a specified data frame of cells and genes
#'
#' @param df A data frame
#' @param labels A vector containing labels indicating if certain cells positively or negatively express personal genes
#' @param file.name Full file name to use for document containing test results
#' @param ncores Number of cores to utilize
#' @param clean.counts_min.lib.size  Minimum number of genes detected in a cell. Cells with fewer genes will be removed (default: 1.8e3)
#' @param clean.counts_min.reads
#' @param clean.counts_min.detected
#' @return ediff
#' @examples
#' slit.pos_neg <- pos_neg.label(df = es.mef.small, rowname = 'dd_Smed.v4_12111_0_1')
#' run_SCDE(df = planaria_cells, labels = slit.pos_neg, file.name = 'slitediff.txt')
run_SCDE <- function(df, labels, file.name, ncores = 1, min_genes_detected = 1000, min_reads_per_gene = 5, min_cells_gene_in = 10) {
  source("https://bioconductor.org/biocLite.R")
  biocLite("scde")
  library(scde)

  # issue with newer bioconductor scde releases and compatibility with 'flexmix' solved here from online forums: from
  # https://github.com/hms-dbmi/scde/issues/40

  # in R
  require(devtools)
  install_version("flexmix", version = "2.3-13", repos = "http://cran.us.r-project.org")
  require(devtools)waww
  devtools::install_github("hms-dbmi/scde", build_vignettes = FALSE)

  # factor determining cell types
  sg <- as.factor(labels)

  # calculate new data frame in order of sg(colnames)
  message("Calculating new data frame . . .")
  adjusted_df <- colselect(df = es.mef.small, colchars = names(sg), df_order = FALSE)

  # clean up the dataset
  cd <- clean.counts(adjusted_df, min.lib.size = min_genes_detected, min.reads = min_reads_per_gene,
                     min.detected = min_cells_gene_in)

  # calculate models
  o.ifm <- scde.error.models(counts = cd, groups = sg, n.cores = ncores, threshold.segmentation = TRUE, save.crossfit.plots = FALSE,
                             save.model.plots = FALSE, verbose = 1)

  # filter out cells that don't show positive correlation with the expected expression magnitudes (very poor fits)
  valid.cells <- o.ifm$corr.a > 0
  o.ifm <- o.ifm[valid.cells, ]

  # estimate gene expression prior
  o.prior <- scde.expression.prior(models = o.ifm, counts = cd, length.out = 400, show.plot = FALSE)

  # define two groups of cells
  groups <- as.factor(labels)
  names(groups) <- row.names(o.ifm)

  # run differential expression tests on all genes.
  ediff <- scde.expression.difference(o.ifm, cd, o.prior, groups = groups, n.randomizations = 100, n.cores = ncores, verbose = 1)

  # write out a table with all the results, showing most significantly different genes (in both directions) on top
  write.table(ediff[order(abs(ediff$Z), decreasing = TRUE), ], file = file.name, row.names = TRUE, col.names = TRUE, sep = "\t", quote = FALSE)

  return(ediff)
}
