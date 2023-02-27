
#' Title Compare tro excel files and gives the differences in Column names and rows
#' @param old_version_path File path to old version
#' @param new_version_path File path to new version
#' @param output_file Path to wite the output file with xlsx extension
#' @param OVER_WRITE TRUE if need to overwrite the old version of output file
#' @return
#' @export
#' @import openxlsx
#' @examples
#' \dontrun old_version_path <- "K:/QILT/GOS/2023/Overall/10. Outputs/Data files/Specs/archive/GOS 2022 Master Output Spec.xlsx"
# new_version_path <- "K:/QILT/GOS/2023/Overall/10. Outputs/Data files/Specs/GOS 2023 Master Output Spec.xlsx"
# compare_excel_files (old_version_path, new_version_path, output_file = "misc/example.xlsx", OVER_WRITE = TRUE)
compare_excel_files <- function(old_version_path, new_version_path, output_file, OVER_WRITE = TRUE) {
  # Read in all sheet names from the old Excel version
  old_sheet_names <- getSheetNames(old_version_path)

  # Read in all sheet names from the new Excel version
  new_sheet_names <- getSheetNames(new_version_path)

  # Get a vector of sheet names that are present in both versions
  identical_sheet_names <- intersect(old_sheet_names, new_sheet_names)

  # Create a new output workbook
  output_workbook <- createWorkbook()

  # Iterate over the identical sheet names and compare corresponding sheets
  for (sheet_name in identical_sheet_names) {
    # Read in the corresponding sheet from the old Excel version
    old_sheet <- read.xlsx(old_version_path, sheet = sheet_name)

    # Read in the corresponding sheet from the new Excel version
    new_sheet <- read.xlsx(new_version_path, sheet = sheet_name)

    # Compare column names
    col_diffs <- setdiff(colnames(new_sheet), colnames(old_sheet))

    # Compare row values
    row_diffs <- anti_join(new_sheet, old_sheet, by = colnames(old_sheet)) %>%
      distinct()

    # Truncate sheet name if it's too long
    sheet_name_truncated <- substr(sheet_name, 1, 22)

    # Add worksheets to the output workbook and write data
    addWorksheet(output_workbook, paste0(sheet_name_truncated, "-ColDiff"))
    writeData(output_workbook, paste0(sheet_name_truncated, "-ColDiff"), col_diffs)
    addWorksheet(output_workbook, paste0(sheet_name_truncated, "-RowDiff"))
    writeData(output_workbook, paste0(sheet_name_truncated, "-RowDiff"), row_diffs)

    print(paste0("Comparison results for ", sheet_name, " written to output workbook"))
  }

  # Save the output workbook to a file
  saveWorkbook(output_workbook, output_file, overwrite = OVER_WRITE)
  print(paste0("Comparison results saved to file: ", output_file))
}





