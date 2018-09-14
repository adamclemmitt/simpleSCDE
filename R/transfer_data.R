#' Import data from a csv file
#'
#'@param file_path File path to csv file
#'@param colnames Boolean indicating whether first row should be treated as
#'  column names (default: FALSE)
#'@param rownames Boolean indicating whther first column should be treated as
#'  row names  (default: FALSE)
#'
#'@examples
#'lat_muscle <- import_csv(file_path =
#'                         "C:/Users/exampleUser/Desktop/planaria.xlsx",
#'                         colnames = TRUE)
#'
#'@export
import_csv <- function(file_path, colnames = FALSE, rownames = FALSE) {
  output <- read.csv(file = file_path, sep = ",")
  # Check if dimnesions are large enough to support column and row names
  if (nrow(output) < 2 && colnames) {
    stop("There must be 2 or more rows present in specified csv file to assign
         a column to column names.")
  } else if (ncol(output) < 2 && rownames) {
    stop("There must be 2 or more columns present in specified csv file to
         assign a column to row names.")
  } else if (is.null(file_path)) {
    stop("Entered file_path is null. Set paramter file_path to a valid excel
         file path.")
  }
  # Account for lack of column names (read.csv implies row names)
  if (!colnames) {
    print("ah")
    firstRow <- colnames(output)
    output <- rbind(firstRow, output)
    colnames(output) <- NULL
  }
  # Account for row names
  if (rownames) {
    rownames(output) <- output[,1]
    output <- output[,c(-1)]
  }
  return(output)
}

#' Import data from an excel file
#'
#'@param file_path Name of excel file
#'@param sheet_number Index of sheet in provided excel file
#'@param sheet_name Name of the excel sheet to be imported (default: "Sheet1")
#'@param row_start Number of first row to import
#'@param row_stop Number of last row to iomport
#'@param col_start Number of first column to import
#'@param col_stop Number of last column to import
#'@param rownames Boolean indicating whther first column should become row
#'  names (default: FALSE)
#'@param colnames Boolean indicating whether first row should become column
#'  names (default: FALSE)
#'
#'@examples
#'import_excel(file_path = "C:/Users/adamc/Desktop/Fincher_muscle.xlsx",
#'             sheet_number = 2, sheet_name = "LatMuscle", row_start = 1,
#'             row_stop = 3250, col_start = 5, col_stop = 55, rownames = TRUE,
#'             colnames = TRUE)
#'
#'@export
import_excel <- function(file_path, sheet_number, sheet_name = "Sheet1",
                         row_start, row_stop, col_start, col_stop,
                         rownames = FALSE, colnames = FALSE) {
  if (is.null(file_path)) {
    stop("Entered file_path is null. Set paramter file_path to a valid excel
         file path.")
  } else if (is.null(sheet_number) || sheet_number == 0) {
    stop("Set parameter sheet_number to an existing sheet number for the
         provided excel file.")
  } else if (row_start == 0 || row_stop == 0 || col_start == 0
             || col_stop == 0) {
    stop("Parameters row_start, row_stop, col_start, and col_stop cannot be
         equal to zero.")
  } else if (row_stop < row_start) {
    stop("Value of parameter row_stop cannot be less than value of parameter
         row_start.")
  } else if (col_stop < col_start) {
    stop("Value of parameter col_start cannot be less than value of parameter
         col_start.")
  }
  output <- xlsx::read.xlsx(file = file_path, sheetIndex = sheet_number,
                      sheetName = sheet_name, rowIndex = c(row_start:row_stop),
                      colIndex = c(col_start:col_stop, header = colnames))
  if (rownames) {
    rownames(output) <- output[,1]
    output[,1] <- NULL
  }
  message("Data was successfully imported from selected excel sheet.")
  return(output)
}

#' Import data from a text file
#'
#'@param file_path Full file path of text file
#'@param rownames Boolean indicating whther first column should become row
#'  names (default: FALSE)
#'@param colnames Boolean indicating whether first row should become column
#'  names (default: FALSE)
#'
#'@examples
#'import_excel(file_path = "C:/Users/adamc/Desktop/Fincher_muscle.xlsx",
#'             rownames = TRUE, colnames = TRUE)
#'
#'@export
import_txt <- function(file_path, colnames, rownames) {
  output <- read.table(file = file_path)
  # Check if dimnesions are large enough to support column and row names
  # Check if file_path input is null
  if (nrow(output) < 2 && colnames) {
    stop("There must be 2 or more rows present in specified csv file to assign
         a column to column names.")
  } else if (ncol(output) < 2 && rownames) {
    stop("There must be 2 or more columns present in specified csv file to
         assign a column to row names.")
  } else if (is.null(file_path)) {
    stop("Entered file_path is null. Set paramter file_path to a valid excel
         file path.")
  }
  # Account for lack of column names
  if (colnames) {
    colnames(output) = NULL
    names <- output[1,]
    colnames(output) <- names
    output <- output[-1, ]
  } else if (!colnames) {
    colnames(output) <- NULL
  }
  # Account for row names
  if (rownames) {
    rownames(output) <- output[,1]
    output <- output[,c(-1)]
  }

  return (output)
}

#' Export data to csv file
#'
#'@param data Content to export to csv file
#'@param file_path Name of csv file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be
#'  written as first row (default: TRUE)
#'@param rownames Boolean indicating whther row names should be
#'  written as first column (default: TRUE)
#'
#'@examples
#'data(Fincher_lateral_muscle)
#'export_csv(data = Fincher_lateral_muscle,
#'             file_path = "C:/Users/exampleUser/Desktop/planaria.csv")
#'
#'@export
export_csv <- function(data, file_path, colnames = TRUE, rownames = TRUE) {
  if (is.null(data)) {
    stop("Entered data is null. Enter valid data.")
  } else if (is.null(file_path)) {
    stop("Entered file_path is null. Enter valid file_path.")
  } else if (colnames != TRUE && colnames != T && colnames != F &&
             colnames != FALSE) {
    stop("Argument colnames must be a TRUE or FALSE.")
  } else if (rownames != TRUE && rownames != T && rownames != FALSE &&
             rownames != F) {
    stop("Argument rownames must be TRUE or FALSE.")
  }
  # Notify user that writing to csv file will delete current data in file
  message("In order to export data to the specified csv file, any existing data
          in the file must be deleted. Is this acceptable?(y/n)")
  confirm <- readline("")
  if (colnames) {
    colnames = NA;
  }
  if (confirm == "y") {
    # Write data to csv file
    write.table(x = data, file = file_path, sep = ",", col.names = colnames,
                row.names = rownames)
    message("Data was successfully written to specified csv file.")
  } else if (confirm == "n") {
    message("Process has been terminated. No data was written.")
  } else {
    message("Response must be y or n. Call function again and answer y or n in
            response to the question.")
  }
}

#' Export data to excel file
#'
#'@param data Content to export to excel file
#'@param file_path Name of excel file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be
#'  written as first row (default: TRUE)
#'@param rownames Boolean indicating whther row names should be
#'  written as first column (default: TRUE)
#'@param name_sheet String indicating what is to be the name of the sheet
#'  containing the newly imported data (default: "Sheet1")
#'@param add_to_file Boolean indicating whether data should be added as a
#'  new excel sheet to a current excel file or if data should replace all
#'  existing excel sheets (default: FALSE)
#'
#'@examples
#'data(Fincher_lateral_muscle)
#'export_excel(data = Fincher_lateral_muscle,
#'             file_path = "C:/Users/exampleUser/Desktop/planaria.xlsx",
#'             name_sheet = "LatMuscle", add_to_file = TRUE)
#'
#'@export
export_excel <- function(data, file_path, colnames = TRUE, rownames = TRUE,
                         name_sheet = "Sheet1", add_to_file = FALSE) {
  # Notify user that add_to_file can result in the deletion of excel data
  if (!add_to_file) {
    message("Becuase add_to_file is FALSE, existing data in selected file
            destination will be deleted and overwritten. Is this
            acceptable?(y/n)")
    confirm <- readline("")
    if (confirm == "y") {
      xlsx::write.xlsx(x = data, file = file_path, col.names = colnames,
                 row.names = rownames, sheetName = name_sheet,
                 append = add_to_file)
      message("Data was successfully written to specified excel sheet.")
    } else if (confirm == "n") {
      message("Data was not written to specified excel sheet. Change value of
              add_to_file in order to avoid deletion of existing
              file information.")
    } else {
      message("Response must be y or n. Call function again and
              enter y or n in response to question.")
    }
  } else {
    # Write data to excel sheet
    xlsx::write.xlsx(x = data, file = file_path, col.names = colnames,
               row.names = rownames, sheetName = name_sheet,
               append = add_to_file)
    message("Data was successfully written to specified excel sheet.")
  }
}

#' Export data to txt file
#'
#'@param data Content to export to txt file
#'@param file_path Name of txt file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be
#'  written as first row (default: TRUE)
#'@param rownames Boolean indicating whther row names should be
#'  written as first column (default: TRUE)
#'
#'@examples
#'data(Fincher_lateral_muscle)
#'export_txt(data = Fincher_lateral_muscle,
#'             file_path = "C:/Users/exampleUser/Desktop/planaria.txt")
#'
#'@export
export_txt <- function(data, file_path, colnames = TRUE, rownames = TRUE) {
  if (is.null(data)) {
    stop("Entered data is null. Enter valid data.")
  } else if (is.null(file_path)) {
    stop("Entered file_path is null. Enter valid file_path.")
  } else if (colnames != TRUE && colnames != T && colnames != F &&
             colnames != FALSE) {
    stop("Argument colnames must be a TRUE or FALSE.")
  } else if (rownames != TRUE && rownames != T && rownames != FALSE &&
             rownames != F) {
    stop("Argument rownames must be TRUE or FALSE.")
  }
  # Notify user that writing to text file will delete current text in file
  message("In order to export data to the specified txt file, any existing data
           in the file must be deleted. Is this acceptable?(y/n)")
  confirm <- readline("")
  if (confirm == "y") {
    # Write data to text file
    write.table(x = data, file = file_path, col.names = colnames,
                row.names = rownames)
    message("Data was successfully written to specified txt file.")
  } else if (confirm == "n") {
    message("Process has been terminated. No data was written.")
  } else {
    message("Response must be y or n. Call function again and answer y or n in
            response to the question.")
  }
}

