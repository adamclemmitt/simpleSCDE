% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/run_SCDE.R
\name{run_SCDE}
\alias{run_SCDE}
\title{Run single-cell differential expression testing on a specified data frame of
cells and genes}
\usage{
run_SCDE(df, labels, write_to_txt = FALSE, file_name = "results.txt",
  ncores = 1, min_genes_detected = 1800, min_reads_per_gene = 10,
  min_cells_gene_in = 5)
}
\arguments{
\item{df}{A data frame}

\item{labels}{A vector containing labels indicating if certain cells
positively or negatively express personal genes}

\item{write_to_txt}{Boolean indicating whether results should be written to
an external text file (default: FALSE)}

\item{file_name}{Full file name to use for document containing test results}

\item{ncores}{Number of cores to utilize (default: 1)}

\item{min_genes_detected}{Minimum number of genes detected in a cell. Cells
with fewer genes will be removed (default: 1.8e3)}

\item{min_reads_per_gene}{Minimum number of reads per gene. Genes with fewer
reads will be removed (default: 10)}

\item{min_cells_gene_in}{Minimum number of cells a gene must be seen in.
Genes not seen in a sufficient number of cells will be removed (default: 5)}
}
\value{
data frame containing upper bound(ub), maximum likelihood
  estimate(mle), upper bound(ub), conservative estimate(ce), Z-score(Z), and
  cZ-score(cZ) values
}
\description{
Run single-cell differential expression testing on a specified data frame of
cells and genes
}
\examples{
slit_posneg <- pos_neg.label(df = es.mef.small,
                             rowname = "dd_Smed.v4_12111_0_1")
run_SCDE(df = Fincher, labels = slit_posneg, file_name = "slitResults.txt",
         min_genes_detected = 10)

}
