#' Export data to excel file
#'
#'@param data Content to export to excel file
#'@param excel_file_name Name od excel file that is the destination for \code{data}
#'@param colnames Boolean indicating whether column names should be written
#'@param rownames Boolean indicating whther row names should be written
#'@examples
#'export_excel(export = planaria_cells, "planarian.xlsx")
export_excel <- function(data, excel_file_path, colnames = TRUE, rownames = TRUE) {
  library(xlsx)
  write.xlsx(x = data, file = excel_file_path, col.names = colnames, row.names = rownames)
}
