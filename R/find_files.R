#' Title Find a file in a folder
#' @param folder_path Path to the folder in which the file exists
#' @param subfolder_path Sub-folder name in any order
#' @param file_extension File extension for example xlsx or txt
#' @param file_name A string from the name of the file
#' @param match_all TRUE if all the values in file_name must be there in the output file.
#' @param strict If TRUE only gives results when all values in subfolder_path match
#' @return
#' @export
#'@examples
#' find_files(folder_path = "K:/QILT/GOS/2023", subfolder_path = c("sample", "feb"), file_extension = "xlsx", file_name = c("Spec", "custom"), match_all = TRUE, strict = TRUE)
find_files <- function(folder_path, subfolder_path, file_name, file_extension, match_all = TRUE, strict = FALSE){
  all_files <- dir(folder_path, recursive = TRUE)
  matching_files <- character(0)

  for (file in all_files){
    if (!grepl("^[\\.\\$~]", file) && # remove the files that start with "., ~, $"  (i.e. hidden files)
        grepl(paste0(".", file_extension), file, ignore.case = TRUE) && # select only those with desired extension
        ((match_all && all(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))) || # Runs when match_all = TRUE
         (!match_all && any(sapply(file_name, function(arg) grepl(arg, basename(file), ignore.case = TRUE)))))){ # Runs when match_all = FALSE
      if (subfolder_path == ""){
        matching_files <- c(matching_files, file.path(folder_path, file))
      } else {
        subfolder_path <- sub("^.*/", "", file.path(folder_path, file))
        subfolder_path <- sub("/[^/]+$", "", subfolder_path)
        subfolder_path_components <- strsplit(subfolder_path, "/")[[1]]
        if ((!strict && any(sapply(subfolder_path, function(arg) grepl(arg, subfolder_path_components, ignore.case = TRUE)))) ||
            (strict && all(sapply(subfolder_path, function(arg) grepl(arg, subfolder_path_components, ignore.case = TRUE))))){
          matching_files <- c(matching_files, file.path(folder_path, file))
        }
      }
    }
  }

  if (length(matching_files) == 0){
    return(paste0("There is no such file in the folder: ", folder_path))
  }

  cat("The following files were found:\n")
  for (i in 1:length(matching_files)){
    cat(paste0(i, ": ", matching_files[i], "\n"))
  }

  file_index <- as.numeric(readline("Which file do you want to open? \n"))
  while (file_index < 1 || file_index > length(matching_files)) {
    cat("Invalid input. Enter a number between 1 and", length(matching_files), ": ")
    file_index <- as.numeric(readline(""))
  }

  shell.exec(matching_files[file_index])
}




# tidyverse way
# library(purrr)
#
# find_files_tidyverse <- function(folder_path, subfolder_path = "", file_name, file_extension, match_all = FALSE){
#
#   all_files <- dir(folder_path, recursive = TRUE)
#
#   matching_files <- all_files %>%
#     keep(~ !grepl("^\\.", .x) &
#            grepl(paste0(".", file_extension), .x, ignore.case = TRUE) &
#            ((match_all & all(sapply(file_name, function(arg) grepl(arg, basename(.x), ignore.case = TRUE)))) |
#               (!match_all & any(sapply(file_name, function(arg) grepl(arg, basename(.x), ignore.case = TRUE)))))) %>%
#     map(~ if (subfolder_path == ""){
#       file.path(folder_path, .x)
#     } else {
#       subfolder_path <- sub("^.*/", "", file.path(folder_path, .x))
#       subfolder_path <- sub("/[^/]+$", "", subfolder_path)
#       subfolder_path_components <- strsplit(subfolder_path, "/")[[1]]
#       if (all(sapply(subfolder_path, function(arg) grepl(arg, subfolder_path_components, ignore.case = TRUE)))){
#         file.path(folder_path, .x)
#       } else {
#         NULL
#       }
#     })
#
#   matching_files <- matching_files[!is.na(matching_files)]
#
#   if (length(matching_files) == 0){
#     return(NULL)
#   }
#
#   cat("The following files were found:\n")
#   for (i in 1:length(matching_files)){
#     cat(paste0(i, ": ", matching_files[i], "\n"))
#   }
#
#   file_index <- as.numeric(readline("Enter the number of the file to open: "))
#   while (file_index < 1 || file_index > length(matching_files)){
#     file_index <- as.numeric(readline("Invalid input. Enter a number between 1 and ", length(matching_files), ": "))
#   }
#
#   shell.exec(matching_files[file_index])
# }

find_files_snip <- function(){
  string <- "find_files(folder_path = 'K:/QILT/GOS/2023', subfolder_path = c('sample', 'feb'), file_extension = 'xlsx', file_name = c('Spec', 'custom'), match_all = TRUE, strict = TRUE)"
  cat(string)
}
