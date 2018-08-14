#' Select rows from a data frame using a character vector or a numeric vector
#'
#' @param df A data frame
#' @param rowchars A character vector of row names (default: c())
#' @param rownumbs A vector containing the row numbers of the rows to be
#'   chosen (default: c())
#' @param complete_names Boolean determining if the entered strings are exact
#'   row names (rather than fragments of row names) (default: TRUE)
#' @param df_order Boolean that determines if the rows selected are returned in
#'   the order they appear in \code{df} (rather than the order they appear in
#'     \code{rowchars}) (default: TRUE)
#' @param print.rownames Boolean determining if selected row names should be
#'   printed upon completion (default: FALSE)
#' @param print.rownumbs Boolean determining if selected row numbers should be
#'   printed upon completion (default: FALSE)
#' @return The rows from \code{df} whose names match the character
#'   vector \code{rowchars}
#' @examples
#' rowselect(df = Fincher, rowchars = c("dd_Smed_v4_12", "dd_Smed_v4_22"),
#'           complete_names = FALSE, print.rownumbs = TRUE)
#'
#' @export
rowselect <- function(df, rowchars = c(), rownumbs = c(), complete_names = TRUE,
                      df_order = TRUE, print.rownames = FALSE,
                      print.rownumbs = FALSE) {
  output <- c()
  complete_rownames <- c()
  find_rownumbs <- c()
  row.names <- rownames(df)
  # Check if both rowchars and rownumbs entered
  if (length(rowchars) != 0 && length(rownumbs) != 0) {
    stop("Values may only be entered for rowchars or rownumbs. Rows are
         identified and selected based on one input type.")
    # Check if neither rowchars nor rownumbs entered
  } else if (length(rowchars) == 0 && length(rownumbs) == 0) {
    stop("Both rowchars and rownumbs are empty. Please enter a value for either
         rowchars or rownumbs.")
    # Select rows based on inputted character vector
  } else if (length(rowchars) != 0) {
    # Return columns in the order they appear in the inputted data frame
    if (df_order) {
      # Inputted strings treated as fragments of inputted column names
      if (!complete_names) {
        for (i in 1:nrow(df)) {
          for (j in 1:length(rowchars)) {
            if (grepl(pattern =
                      paste(rowchars[j], collapse = "|"), x = row.names[[i]])) {
              find_rownumbs <- append(find_rownumbs, i)
              complete_rownames <- append(complete_rownames, row.names[[i]])
            }
          }
        }
        output <- df[find_rownumbs, ]
      } else {
        # Inputted strings treated as exact column names
        for (i in 1:nrow(df)) {
          for (j in 1:length(rowchars)) {
            if (rowchars[j] == row.names[[i]]) {
              find_rownumbs <- append(find_rownumbs, i)
              complete_rownames <- append(complete_rownames, row.names[[i]])
              break
            }
          }
        }
      }
      output <- df[find_rownumbs, ]
    } else {
      # Return columns in the order they appear in inputted colchars
      # Inputted strings treated as fragments of inputted column names
      if (!complete_names) {
        for (j in 1:length(rowchars)) {
          for (i in 1:nrow(df)) {
            if (grepl(pattern =
                      paste(rowchars[j], collapse = "|"), x = row.names[[i]])) {
              find_rownumbs <- append(find_rownumbs, i)
              complete_rownames <- append(complete_rownames, row.names[[i]])
            }
          }
        }
        output <- df[find_rownumbs, ]
      } else {
        # Inputted strings treated as exact column names
        for (i in 1:length(rowchars)) {
          for (j in 1:nrow(df)) {
            if (rowchars[i] == row.names[[j]]) {
              find_rownumbs <- append(find_rownumbs, j)
              complete_rownames <- append(complete_rownames, row.names[[j]])
            }
          }
        }
      }
      output <- df[find_rownumbs, ]
    }
  } else {
    # Select columns based on inputted numeric vector
    # Return columns in the order they appear in the original data frame
    if (df_order) {
      sorted_rownumbs <- sort(rownumbs)
      output <- df[sorted_rownumbs, ]
    } else {
      # Return columns in the order they are entered/specificied in colnumbs
      output <- df[rownumbs, ]
    }
  }
  # Check if user has requested to print information
  if (print.rownames) {
    if (length(rowchars) != 0) {
      message("The names of the selected row(s) are as follows: ")
      print(complete_rownames)
    } else {
      for (i in 1:length(rownumbs)) {
        complete_rownames <- append(complete_rownames, row.names[(rownumbs[i])])
      }
      message("The names of the selected row(s) are as follows: ")
      print(complete_rownames)
    }
  }
  if (print.rownumbs) {
    if (length(rowchars) != 0) {
      message("The numbers of the selected row(s) are as follows: ")
      print(find_rownumbs)
    } else {
      message("The numbers of the selected row(s) are as follows: ")
      print(rownumbs)
    }
  }
  return(output)
}

#' Select columns from a data frame using a character vector or numeric vector
#'
#' @param df A data frame
#' @param colchars A character vector of column names (default: c())
#' @param colnumbs A numeric vector of column numbers (default: c())
#' @param complete_names Boolean determining if the entered strings are exact
#'   column names (rather than fragments of column names) (default: TRUE)
#' @param df_order Boolean that determines if the columns selected are returned
#'   in the order they appear in \code{df} (rather than the order they appear in
#'   \code{colchars}) (default: TRUE)
#' @param print.colnames Boolean determining if selected column names should be
#'   printed upon completion (default: FALSE)
#' @param print.colnumbs Boolean determining if selected column numbers should
#'   be printed upon completion (default: FALSE)
#' @return A data frame consisting of the columns from \code{df} whose names
#'   match the character vector \code{colchars} in the order that the appear in
#'   the column names of \code{df}
#' @examples
#' colselect(df = Fincher, colchars = c("dd_Smed_v4_12", "dd_Smed_v4_22"),
#'           complete_names = FALSE, df_order = FALSE, print.colnumbs = TRUE)
#'
#' @export
colselect <- function(df, colchars = c(), colnumbs = c(), complete_names = TRUE,
                      df_order = TRUE, print.colnames = FALSE,
                      print.colnumbs = FALSE) {
  output <- c()
  complete_colnames <- c()
  find_colnumbs <- c()
  col.names <- colnames(df)
  j <- 1
  # Check if both colchars and colnumbs entered
  if (length(colchars) != 0 && length(colnumbs) != 0) {
    stop("Values may only be entered for colchars or colnumbs. Columns are
         identified and selected based on one input type.")
    # Check if neither colchars nor colnumbs entered
  } else if (length(colchars) == 0 && length(colnumbs) == 0) {
    stop("Both colchars and colnumbs are empty. A value must be entered for
         either colchars or colnumbs.")
    # Select columns based on inputted character vector
  } else if (length(colchars) != 0) {
    # Return columns in the order they appear in the inputted data frame
    if (df_order) {
      # Inputted strings treated as fragments of inputted column names
      if (!complete_names) {
        for (i in 1:ncol(df)) {
          for (j in 1:length(colchars)) {
            if (grepl(pattern =
                      paste(colchars[j], collapse = "|"), x = col.names[[i]])) {
              find_colnumbs <- append(find_colnumbs, i)
              complete_colnames <- append(complete_colnames, col.names[[i]])
            }
          }
        }
        output <- df[, ..find_colnumbs]
      } else {
        # Inputted strings treated as exact column names
        for (i in 1:ncol(df)) {
          for (j in 1:length(colchars)) {
            if (colchars[j] == col.names[[i]]) {
              find_colnumbs <- append(find_colnumbs, i)
              complete_colnames <- append(complete_colnames, col.names[[i]])
              break
            }
          }
        }
        output <- df[, ..find_colnumbs]
      }
    } else {
      # Return columns in the order they appear in inputted colchars
      # Inputted strings treated as fragments of inputted column names
      if (!complete_names) {
        for (k in 1:length(colchars)) {
          for (i in 1:ncol(df)) {
            if (grepl(pattern =
                      paste(colchars[k], collapse = "|"), x = col.names[[i]])) {
              find_colnumbs <- append(find_colnumbs, i)
              complete_colnames <- append(complete_colnames, col.names[[i]])
            }
          }
        }
        output <- df[, ..find_colnumbs]
      } else {
        # Inputted strings treated as exact column names
        for (i in 1:length(colchars)) {
          for (j in 1:ncol(df)) {
            if (colchars[i] == col.names[[j]]) {
              find_colnumbs <- append(find_colnumbs, j)
              complete_colnames <- append(complete_colnames, col.names[[j]])
            }
          }
        }
        output <- df[, ..find_colnumbs]
      }
    }
  } else {
    # Select columns based on inputted numeric vector
    # Return columns in the order they appear in the original data frame
    if (df_order) {
      sorted_colnumbs <- sort(colnumbs)
      output <- df[, ..sorted_colnumbs]
    } else {
      # Return columns in the order they are entered/specificied in colnumbs
      output <- df[, ..colnumbs]
    }
  }
  # Check if user has requested to print information
  if (print.colnames) {
    if (length(colchars) != 0) {
      message("The names of the selected column(s) are as follows: ")
      print(complete_colnames)
    } else {
      for (i in 1:length(colnumbs)) {
        complete_colnames <- append(complete_colnames, col.names[(colnumbs[i])])
      }
      message("The names of the selected column(s) are as follows: ")
      print(complete_colnames)
    }
  }
  if (print.colnumbs) {
    if (length(colchars) != 0) {
      message("The numbers of the selected column(s) are as follows: ")
      print(find_colnumbs)
    } else {
      message("The numbers of the selected column(s) are as follows: ")
      print(colnumbs)
    }
  }
  return(output)
}

#' Delete repeated rows in a data frame
#'
#' @param df A data frame
#' @return The data frame \code{df} excluding all duplicated rows
#' @examples
#' delete_duplicate_row(df = Fincher)
#'
#' @export
delete_duplicate_row <- function(df) {
  if (is.null(df)) {
    stop("Argument df is null. Assign df to a valid data frame.")
  }
  # Locate rows that are not duplicated
  is_unique <- c()
  for (i in 1:length(rownames(df))) {
    if (!duplicated(rownames(df))[i]) {
      is_unique <- append(is_unique, i)
    }
  }
  output <- df[is_unique, ]
}

#' Delete repeated columns in a data frame
#'
#' @param df A data frame
#' @return The data frame \code{df} excluding all duplicated columns
#' @examples
#' delete_duplicate_col(df = Fincher)
#'
#' @export
delete_duplicate_col <- function(df) {
  if (is.null(df)) {
    stop("Argument df is null. Assign df to a valid data frame.")
  }
  # Locate columns that are not duplicated
  is_unique <- c()
  for (i in 1:length(colnames(df))) {
    if (!duplicated(colnames(df))[i]) {
      is_unique <- append(is_unique, i)
    }
  }
  output <- df[ , ..is_unique]
  return(output)
}
