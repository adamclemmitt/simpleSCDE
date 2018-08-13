#' Label a group of cells positive or negative for a single gene based on a user-defined number of hits
#'
#' @param df A data frame
#' @param rowname The name of the gene(row) to be measured for hits in provided cells
#' @param x The minimum number of hits required to label a cell positive for the expression of a particular gene (default: 100)
#' @param x.limit The maximum number of hits of the gene defined by \code{rowname} that is allowed to be present in a negative cell (default: 0)
#' @param posname The string to label cells that are determined to be positive (default: "pos")
#' @param negname The string to label cells that are determined to be negative (default: "neg")
#' @return A named character vector that contains \code{posname} and \code{negname}, and all cells that do not qualify as positive or negative are omitted
#' @examples
#' pos_neg.label(df = Fincher_muscle, rowname = "dd_Smed_v4_12111_0_1", x = 100, x.limit = 10, posname = "slit_pos", negname = "slit_neg")
#'
#' @export
pos_neg.label <- function(df, rowname, x = 100, x.limit = 0, posname = "pos", negname = "neg") {
  # Make sure inputs are valid
  if (is.null(df)) {
    stop("Assign df to a valid data frame.")
  } else if (x < 0 || x.limit < 0) {
    stop("Enter x and x.limit values greater than or equal to zero.")
  } else if (x.limit >= x) {
    stop("Enter a value for x.limit that is less than x.")
  }
  # Find the row number that contains the specified gene name
  rownumb <- 0
  row.names <- rownames(df)
  for (i in 1:nrow(df)) {
    if (rownames(df)[i] == rowname) {
      rownumb <- i
      break
    }
  }
  if (rownumb == 0) {
    stop("Row name not found. Enter a valid row name.")
  }
  # Label each cell positive or negative based on hits of specified genes
  output <- c()
  posnumbs <- c()
  posoutput <- c()
  negnumbs <- c()
  negoutput <- c()
  for (i in 1:ncol(df)) {
    if (df[[rownumb, i]] >= x) {
      posnumbs <- append(posnumbs, i)
      posoutput <- append(posoutput, posname)
    } else if (df[[rownumb, i]] >= x.limit) {
      negnumbs <- append(negnumbs, i)
      negoutput <- append(negoutput, negname)
    }
  }
  output <- c(posoutput, negoutput)
  totalnumbs <- c(posnumbs, negnumbs)
  output_names <- colnames(df)[totalnumbs]
  names(output) <- output_names
  return(output)
}

#' Label a group of cells positive for one of two genes based on a user-defined number of hits for each gene
#'
#' @param df A data frame
#' @param rowname.x The name of a gene(row) to be measured for hits in provided cells
#' @param rowname.y The name of a gene(row) to be measured for hits in provided cells
#' @param x The minimum number of hits required to label a cell positive for the expression of a particular gene (default: 100)
#' @param y The minimum number of hits required to label a cell positive for the expression of a particular gene (defualt: 100)
#' @param x.limit The maximum number of hits of the gene defined by \code{rowname} that is allowed to be present in a negative cell (default: 0)
#' @param y.limit The maximum number of hits of the gene defined by \code{rowname} that is allowed to be present in a negative cell (default: 0)
#' @param posname.x The string to label cells that are determined to be positive for \code{rowname.x} (default: "pos_x")
#' @param posname.y The string to label cells that are determined to be positive for \code{rowname.y} (default: "pos_y")
#' @return A named character vector that contains \code{posname.x} and \code{posname.y}, and all cells that do not qualify as positive or negative are omitted
#' @examples
#' pos_neg.label(df = Fincher, rowname.x = "dd_Smed_v4_12111_0_1", rowname.y = "dd_Smed_22585_0_1", x = 100, y = 150 x.limit = 10, y.limit = 20, posname.x = "slit_pos", posname.y = "zic_pos")
#'
#' @export
pos_pos.label <- function(df, rowname.x, rowname.y, x = 100, y = 100, x.limit = 0, y.limit = 0, posname.x = "pos_x", posname.y = "pos_y") {
  # Make sure inputs are valid
  if (is.null(df)) {
    stop("Assign df to a valid data frame.")
  } else if (x < 0 || x.limit < 0 || y < 0 || y.limit < 0) {
    stop("Enter x, y, x.limit, and y.limit values greater than or equal to zero.")
  } else if (x.limit >= x) {
    stop("Enter a value for x.limit that is less than x.")
  } else if (y.limit >= y) {
    stop("Enter a value for y.limit that is less than y.")
  }
  # Find the row number that contains the specified gene name
  rownumb.x <- 0
  rownumb.y <- 0
  row.names <- rownames(df)
  for (i in 1:nrow(df)) {
    if (rownames(df)[i] == rowname.x) {
      rownumb.x <- i
    } else if (rownames(df)[i] == rowname.y) {
      rownumb.y <- i
    }
  }
  if (rownumb.x == 0 || rownumb.y == 0) {
    stop("At least one row name not found. Enter valid row names.")
  }
  # Label each cell positive or negative based on hits of specified genes
  output <- c()
  posnumbs.x <- c()
  posoutput.x <- c()
  posnumbs.y <- c()
  posoutput.y <- c()
  for (i in 1:ncol(df)) {
    if (df[[rownumb.x, i]] >= x && df[[rownumb.y, i]] <= y.limit) {
      posnumbs.x <- append(posnumbs.x, i)
      posoutput.x <- append(posoutput.x, posname.x)
    } else if (df[[rownumb.y, i]] >= y && df[[rownumb.x, i]] <= x.limit) {
      posnumbs.y <- append(posnumbs.y, i)
      posoutput.y <- append(posoutput.y, posname.y)
    }
  }
  output <- c(posoutput.x, posoutput.y)
  totalnumbs <- c(posnumbs.x, posnumbs.y)
  output_names <- colnames(df)[totalnumbs]
  names(output) <- output_names
  return(output)
}

#' Label a group of cells positive or negative for a group of two genes based on a user-defined number of hits for each gene
#'
#' @param df A data frame
#' @param rowname.x1 The name of a gene(row) to be measured for hits in provided cells
#' @param rowname.x2 The name of a gene(row) to be measured for hits in provided cells
#' @param x1 The minimum number of hits required to label a cell positive for the expression of a particular gene (default: 100)
#' @param x2 The minimum number of hits required to label a cell positive for the expression of a particular gene (default: 100)
#' @param x1.limit The maximum number of hits of the gene defined by \code{rowname.x1} that is allowed to be present in a negative cell (default: 0)
#' @param x2.limit The maximum number of hits of the gene defined by \code{rowname.x2} that is allowed to be present in a negative cell (default: 0)
#' @param posname The string to label cells that are determined to be positive (default: "pos")
#' @param negname The string to label cells that are determined to be negative (default: "neg")
#' @return A named character vector that contains \code{posname} and \code{negname}, and alls cells that do not qualify as either are omitted
#' @examples
#' pos_neg.label(df = Fincher, rowname = "dd_Smed_v4_12111_0_1", x1 = 110, x2 = 90, x1.limit = 10, x2.limit = 10, posname = "slit_pos", negname = "slit_neg")
#'
#' @export
pospos_neg.label <- function(df, rowname.x1, rowname.x2, x1 = 100, x2 = 100, x1.limit = 0, x2.limit = 0, posname = "pos", negname = "neg") {
  # Make sure inputs are valid
  if (is.null(df)) {
    stop("Assign df to a valid data frame.")
  } else if (x1 < 0 || x2 < 0 || x1.limit < 0 || x2.limit < 0) {
    stop("Enter x1, x2, x1.limit, and x2.limit values greater than or equal to zero.")
  } else if (x1.limit >= x1) {
    stop("Enter a value for x1.limit that is less than x1.")
  } else if (x2.limit >= x2) {
    stop("Enter a value for x2.limit that is less than x2.")
  }
  # Find the row number that contains the specified gene name
  rownumb.x1 <- 0
  rownumb.x2 <- 0
  row.names <- rownames(df)
  for (i in 1:nrow(df)) {
    if (rownames(df)[i] == rowname.x1) {
      rownumb.x1 <- i
    } else if (rownames(df)[i] == rowname.x2) {
      rownumb.x2 <- i
    }
  }
  if (rownumb.x1 == 0 || rownumb.x2 == 0) {
    stop("At least one row name not found. Enter valid row names.")
  }
  # Label each cell positive or negative based on hits of specified genes
  output <- c()
  posnumbs <- c()
  posoutput <- c()
  negnumbs <- c()
  negoutput <- c()
  for (i in 1:ncol(df)) {
    if (df[[rownumb.x1, i]] >= x1 && df[[rownumb.x2, i]] >= x2) {
      posnumbs <- append(posnumbs, i)
      posoutput <- append(posoutput, posname)
    } else if (df[[rownumb.x1, i]] >= x1.limit && df[[rownumb.x2, i]] >= x2.limit) {
      negnumbs <- append(negnumbs, i)
      negoutput <- append(negoutput, negname)
    }
  }
  output <- c(posoutput, negoutput)
  totalnumbs <- c(posnumbs, negnumbs)
  output_names <- colnames(df)[totalnumbs]
  names(output) <- output_names
  return(output)
}

#' Label a group of cells positive for one of two two-gene groupings based on a user-defined number of hits for each gene
#'
#' @param df A data frame
#' @param rowname.x1 The name of a gene(row) to be measured for hits in provided cells
#' @param rowname.x2 The name of a gene(row) to be measured for hits in provided cells
#' @param rowname.y1 The name of a gene(row) to be measured for hits in provided cells
#' @param rowname.y2 The name of a gene(row) to be measured for hits in provided cells
#' @param x1 The minimum number of hits required to label a cell positive for the expression of a particular gene(\code{rowname.x1}) (default: 100)
#' @param x2 The minimum number of hits required to label a cell positive for the expression of a particular gene(\code{rowname.x2}) (defualt: 100)
#' @param y1 The minimum number of hits required to label a cell positive for the expression of a particular gene(\code{rowname.y1}) (default: 100)
#' @param y2 The minimum number of hits required to label a cell positive for the expression of a particular gene(\code{rowname.y2}) (defualt: 100)
#' @param x1.limit The maximum number of hits of the gene defined by \code{rowname.x1} that is allowed to be present in a y-positive cell (default: 0)
#' @param x2.limit The maximum number of hits of the gene defined by \code{rowname.x2} that is allowed to be present in a y-positive cell (default: 0)
#' @param y1.limit The maximum number of hits of the gene defined by \code{rowname.y1} that is allowed to be present in a x-positive cell (default: 0)
#' @param y2.limit The maximum number of hits of the gene defined by \code{rowname.y2} that is allowed to be present in a x-positive cell (default: 0)
#' @param posname.x The string to label cells that are determined to be positive for both \code(rowname.x1) and \code(rowname.x2) (default: "pos_x")
#' @param posname.y The string to label cells that are determined to be positive for both \code(rowname.y1) and \code(rowname.y2) (default: "pos_y")
#' @return A named character vector that contains \code{posname.x}, \code{posname.y}, or 'NA' labels for each cell
#' @examples
#' pospos_pospos.label(df = Fincher, x = 100, rowname.x1 = 'dd_Smed_v4_12111_0_1', rowname.x2 = 'dd_Smed_v4_23400_0_2', rowname.y1 = "dd_Smed_v4_840_0_1", rowname.y2 = "dd_Smed_v4_702_0_1", x1 = 10, x2 = 5, y1 = 12, y2 = 20, posname.x = 'slit_23400.pos', posname.y = '840_collagen.pos')
#'
#' @export
pospos_pospos.label <- function(df, rowname.x1, rowname.x2, rowname.y1, rowname.y2, x1 = 100, x2 = 100, y1 = 100, y2 = 100,
                                x1.limit = 0, x2.limit = 0, y1.limit = 0, y2.limit = 0, posname.x = "pos_x", posname.y = "pos_y") {
  # Make sure data frame input is valid
  if (is.null(df)) {
    stop("Assign df to a dataframe value.")
  } else if (x1 < 0 || x2 < 0 || y1 < 0 || y2 < 0 || x1.limit < 0 || x2.limit < 0 || y1.limit < 0 || y2.limit < 0) {
    stop("Enter x1, x2, y1, y2, x1.limit, x2.limit, y1.limit, and y2.limit values greater than or equal to zero.")
  }
  # Find the row number that contains the specified gene name
  rownumb.x1 <- 0
  rownumb.x2 <- 0
  rownumb.y1 <- 0
  rownumb.y2 <- 0
  row.names <- rownames(df)
  for (i in 1:nrow(df)) {
    if (rownames(df)[i] == rowname.x1) {
      rownumb.x1 <- i
    }
    if (rownames(df)[i] == rowname.x2) {
      rownumb.x2 <- i
    }
    if (rownames(df)[i] == rowname.y1) {
      rownumb.y1 <- i
    }
    if (rownames(df)[i] == rowname.y2) {
      rownumb.y2 <- i
    }
  }
  if (rownumb.x1 == 0 || rownumb.x2 == 0 || rownumb.y1 == 0 || rownumb.y2 == 0) {
    stop("One of the four row names inputted was not found. Please enter valid row names.")
  }
  # Label cells based on the genes they contain
  output <- c()
  posnumbs.x <- c()
  posoutput.x <- c()
  posnumbs.y <- c()
  posoutput.y <- c()
  for (i in 1:ncol(df)) {
    if (df[[rownumb.x1, i]] >= x1 && df[[rownumb.x2, i]] >= x2 && df[[rownumb.y1, i]] <= y1.limit && df[[rownumb.y2, i]] <= y2.limit) {
      posnumbs.x <- append(posnumbs.x, i)
      posoutput.x <- append(posoutput.x, posname.x)
    } else if (df[[rownumb.y1, i]] >= y1 && df[[rownumb.y2, i]] >= y2 && df[[rownumb.x1, i]] <= x1.limit && df[[rownumb.x2, i]] <= x2.limit) {
      posnumbs.y <- append(posnumbs.y, i)
      posoutput.y <- append(posoutput.y, posname.y)
    }
  }
  output <- c(posoutput.x, posoutput.y)
  if (length(output) == 0) {
    stop("No cells matched the entered specifications. Adjust inputted parameters in order to return valid results.")
  }
  totalnumbs <- c(posnumbs.x, posnumbs.y)
  output_names <- colnames(df)[totalnumbs]
  names(output) <- output_names
  return(output)
}

#' Label two groups of cells based on the contents of their names
#'
#' @param df A data frame
#' @param x.start Index of \code{col.names} at which to start first group of cells
#' @param x.stop Index of \code{col.names} at which to stop first group of cells
#' @param y.start Index of \code{col.names} at which to start first group of cells
#' @param y.stop Index of \code{col.names} at which to stop first group of cells
#' @param x.name String label for first cell group (default: "group_x")
#' @param y.name String label for second cell group (default: "group_y")
#' @return A named character vector that contains \code{x.name} and \code{y.name}, and all cells that do not qualify as positive or negative are omitted
#' @examples
#' label_by_colnames(df = Fincher, x.start = 1, x.stop = 11, y.start = 12, y.stop = length(colnames(planaria_cells))
#'
#' @export
label_by_colnames <- function(df, x.start, x.stop, y.start, y.stop, x.name = "group_x", y.name = "group_y") {
  if (x.start < 1 || x.stop < 1 || y.start < 1 || y.stop < 1) {
    stop("x.start, x.stop, y.start, and y.stop must be equal to or greater than 1.")
  } else if (x.start > length(colnames(df)) || x.stop > length(colnames(df)) || y.start > length(colnames(df)) || y.stop > length(colnames(df))) {
    stop("x.start, x.stop, y.start, and y.stop must have values less than or equal to the number of columns in the
         specified data frame.")
  } else if (x.start >= x.stop || y.start >= y.stop) {
    stop("x.stop must be greater than x.start, and y.stop must be greater than y.start.")
  }
  output <- c()
  difference_x <- x.stop - x.start + 1
  difference_y <- y.stop - y.start + 1
  total_difference <- difference_x + difference_y
  if (total_difference < ncol(df)) {
    stop("One or more column(s) were not selected. Include all columns between x.start, x.stop, y.start, and y.stop.")
  } else if (total_difference > ncol(df)) {
    stop("One or more column(s) were selected more than once. The range between x.start and x.stop may not overlap the
         range between y.start and y.stop.")
  }
  if (x.start < y.start) {
    for (i in 1:difference_x) {
      output <- append(output, x.name)
    }
    for (j in (difference_x + 1):total_difference) {
      output <- append(output, y.name)
    }
    names(output) <- colnames(df)
  } else {
    for (i in 1:difference_x) {
      output <- append(output, x.name)
    }
    for (j in (difference_x + 1):total_difference) {
      output <- append(output, y.name)
    }
    names(output) <- append(colnames(df)[x.start:x.stop], colnames(df)[y.start:y.stop])
  }
  return(output)
}
