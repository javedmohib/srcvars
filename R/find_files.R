#' To find a file in QILT project folders
#' @param project Specify the QILT project (GOS, SES, ESS, GOS-L)
#' @param year Specify the collection year
#' @param drive Enter the drive to search for (K or Z)
#' @param subfolder_path Sub-folder name of the file path in any order
#' @param file_extension File extension for example xlsx or txt
#' @param file_name One or more keywords from the name of the file
#' @param match_all_name TRUE if all the values in file_name must be there in the output file.
#' @param match_all_path If TRUE only gives results when all values in subfolder_path match
#' @return
#' @export
#'@examples
#' \dontrun{find_files(project = "GOS", year = "2023", subfolder_path = c("sample", "feb"), file_name = c("Spec", "sample"), file_extension = "xlsx", match_all_name = TRUE, match_all_path = TRUE)}

  find_files <- function(project, year, drive = "", subfolder_path = "", file_name = "", file_extension = "", match_all_name = TRUE, match_all_path = FALSE) {
  if (tolower(project) == "gosl") project <- "GOS-L"
  if (tolower(drive) == "k") {
    folder_paths <- glue::glue("K:/QILT/{project}/{year}")
  } else if (tolower(drive) == "z") {
    folder_paths <- glue::glue("//srcentre.local/drives/z/Consulting/Jobs/QILT/{project}/{year}")
  } else {
    folder_paths <- c(
      glue::glue("K:/QILT/{project}/{year}"),
      glue::glue("//srcentre.local/drives/z/Consulting/Jobs/QILT/{project}/{year}")
    )
  }

  matching_files <- character(0)

  for (folder_path in folder_paths) {
    all_files <- dir(folder_path, recursive = TRUE)

    for (file in all_files) {
      if (!grepl("^[\\.\\$~]", file) && # remove the files that start with "., ~, $"  (i.e. hidden files)
        grepl(paste0(".", file_extension), file, ignore.case = TRUE) && # select only those with desired extension
        ((match_all_name && all(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))) || # Runs when match_all_name = TRUE

          (!match_all_name && any(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))))) { # Runs when match_all_name = FALSE

        if (all(subfolder_path == "")) {
          matching_files <- c(matching_files, file.path(folder_path, file))
        } else {
          matching_subfolder_path <- sub("/[^/]+$", "", file.path(folder_path, file))

          if ((!match_all_path && any(sapply(subfolder_path, function(arg) grepl(arg, matching_subfolder_path, ignore.case = TRUE)))) ||
            (match_all_path && all(sapply(subfolder_path, function(arg) grepl(arg, matching_subfolder_path, ignore.case = TRUE))))) {
            matching_files <- c(matching_files, file.path(folder_path, file))
          }
        }
      }
    }
  }

  while (TRUE) {
    if (length(matching_files) == 0) {
      cat(paste0("There is no such file in the folder: ", folder_paths, "\n\n"))
      break
    }

    cat("The following files were found:\n")
    for (i in 1:length(matching_files)) {
      cat(paste0(i, ": ", matching_files[i], "\n"))
    }

    file_index <- readline("Which file do you want to open? (Enter 0 to escape) ")
    while (TRUE) {
      if (file_index == "") {
        cat("Invalid input. Enter a number between 1 and", length(matching_files), "or 0 to escape: ")
        file_index <- readline("")
      } else if (as.numeric(file_index) == "0") {
        return()
      } else {
        file_index <- as.numeric(file_index)
        if (file_index < 0 || file_index > length(matching_files)) {
          cat("Invalid input. Enter a number between 1 and", length(matching_files), "or 0 to escape: ")
          file_index <- readline("")
        } else {
          shell.exec(matching_files[file_index])
          file_index <- readline("Which file do you want to open? (Enter 0 to escape) ")
        }
      }
    }
  }
}
