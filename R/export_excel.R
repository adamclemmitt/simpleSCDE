#' Export data to excel file
#'
#'@param data Content to export to excel file
#'@param excel_file_name Name od excel file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be written
#'@param rownames Boolean indicating whther row names should be written
#'@param name_sheet String indicating what the name of the sheet containing the newly imported data should be
#'@param add_to_file Boolean indicating whether data should be added as a new excel sheet to a current excel file or if data should replace all existing excel sheets
#'
#'@examples
#'data(Fincher_lateral_muscle)
#'export_excel(data = Fincher_lateral_muscle, excel_file_path = "C:/Users/adamc/Desktop/planaria.xlsx", name_sheet = "LatMuscle", add_to_file = TRUE)
export_excel <- function(data, excel_file_path, colnames = TRUE, rownames = TRUE, name_sheet = "Sheet1", add_to_file = FALSE) {
  # Notify user that add_to_file can result in the deletion of current excel sheets and respond accordingly
  if (!add_to_file) {
    message("Becuase add_to_file is FALSE, existing data in selected file destination will be deleted and overwritten. Is this acceptable?(y/n)")
    confirm <- readline("")
    if (confirm == "y") {
      write.xlsx(x = data, file = excel_file_path, col.names = colnames, row.names = rownames, sheetName = name_sheet, append = add_to_file)
      message("Data was successfully written to specified excel sheet.")
    } else if (confirm == "n") {
      message("Data was not written to specified excel sheet. Change value of add_to_file in order to avoid deletion of existing file information.")
    } else {
      message("Response must be y or n. Please call function again and enter y or n in response to question.")
    }
  } else {
    # Write data to excel sheet
    write.xlsx(x = data, file = excel_file_path, col.names = colnames, row.names = rownames, sheetName = name_sheet, append = add_to_file)
    message("Data was successfully written to specified excel sheet.")
  }
}
