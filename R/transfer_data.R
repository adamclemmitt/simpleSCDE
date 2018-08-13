#' Import data from a csv file
#'
#'@param file_path File path to csv file
#'@param colnames Boolean indicating whether first row should be treated as column names (default: FALSE)
#'@param rownames Boolean indicating whther first column should be treated as row names  (default: FALSE)
#'
#'@examples
#'lat_muscle <- import_csv(file_path = "C:/Users/adamc/Desktop/planaria.xlsx", colnames = TRUE)
import_csv <- function(file_path, colnames = FALSE, rownames =FALSE) {
  output <- readr::read_csv(file = file_path)
  print(output)
  # Check if dimnesions are large enough to support column and row names
  if (nrow(output) < 2 && colnames) {
    stop("There must be 2 or more rows present in specified csv file to assign a column to column names.")
  } else if (ncol(output) < 2 && rownames) {
    stop("There must be 2 or more columns present in specified csv file to assign a column to row names.")
  }
  # Account for potsntial column names
  if (colnames) {
    colnames(output) <- output[1,]
    output <- output[c(-1),]
  }
  # Account for potential row names
  if (rownames) {
    print(length(rownames(output))
    print(length(output[,1]))
    rownames(output) <- output[,1]
    output <- output[,c(-1)]
  }
  return(output)

}


#' Export data to excel file
#'
#'@param data Content to export to excel file
#'@param file_path Name of excel file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be written (default: TRUE)
#'@param rownames Boolean indicating whther row names should be written (default: TRUE)
#'@param name_sheet String indicating what is to be the name of the sheet containing the newly imported data (default: "Sheet1")
#'@param add_to_file Boolean indicating whether data should be added as a new excel sheet to a current excel file or if data should replace all existing excel sheets (default: FALSE)
#'
#'@examples
#'data(Fincher_lateral_muscle)
#'export_excel(data = Fincher_lateral_muscle, file_path = "C:/Users/adamc/Desktop/planaria.xlsx", name_sheet = "LatMuscle", add_to_file = TRUE)
export_excel <- function(data, file_path, colnames = TRUE, rownames = TRUE, name_sheet = "Sheet1", add_to_file = FALSE) {
  # Notify user that add_to_file can result in the deletion of current excel sheets and respond accordingly
  if (!add_to_file) {
    message("Becuase add_to_file is FALSE, existing data in selected file destination will be deleted and overwritten. Is this acceptable?(y/n)")
    confirm <- readline("")
    if (confirm == "y") {
      write.xlsx(x = data, file = file_path, col.names = colnames, row.names = rownames, sheetName = name_sheet, append = add_to_file)
      message("Data was successfully written to specified excel sheet.")
    } else if (confirm == "n") {
      message("Data was not written to specified excel sheet. Change value of add_to_file in order to avoid deletion of existing file information.")
    } else {
      message("Response must be y or n. Please call function again and enter y or n in response to question.")
    }
  } else {
    # Write data to excel sheet
    write.xlsx(x = data, file = file_path, col.names = colnames, row.names = rownames, sheetName = name_sheet, append = add_to_file)
    message("Data was successfully written to specified excel sheet.")
  }
}
