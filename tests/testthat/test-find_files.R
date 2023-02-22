#' Test function for find_files
#'
#' @test find_files with default parameters
#' find_files("tests/testthat/folder", file_extension = "xlsx", file_name = "spec", match_all = TRUE, strict = FALSE)
#'
#' @test find_files with subfolder_path and strict = TRUE
#' find_files("tests/testthat/folder", subfolder_path = "sample", file_extension = "xlsx", file_name = "spec", match_all = TRUE, strict = TRUE)
#'
#' @test find_files with match_all = FALSE
#' find_files("tests/testthat/folder", file_extension = "xlsx", file_name = "sample", match_all = FALSE, strict = FALSE)
#'
#' @test find_files with no matching files
#' find_files("tests/testthat/folder", file_extension = "csv", file_name = "spec", match_all = TRUE, strict = FALSE)


  # # Test 1: find_files with default parameters
  # expected_output <- c("tests/testthat/folder/Jan/5. Sample/2. Sample specs and design/GOS 2023 Operational Sample Spec.xlsx",
  #                      "tests/testthat/folder/Jan/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx",
  #                      "tests/testthat/folder/Feb/5. Sample/2. Sample specs and design/GOS 2023 Operational Sample Spec.xlsx",
  #                      "tests/testthat/folder/Feb/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx")
  #
  # result <- find_files("tests/testthat/folder", file_extension = "xlsx", file_name = "spec", match_all = TRUE, strict = FALSE)
  # expect_equal(result, expected_output)
  #
  # # Test 2: find_files with subfolder_path and strict = TRUE
  # expected_output <- c("tests/testthat/folder/Feb/5. Sample/2. Sample specs and design/GOS 2023 Operational Sample Spec.xlsx",
  #                      "tests/testthat/folder/Feb/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx")
  #
  # result <- find_files("tests/testthat/folder", subfolder_path = "sample", file_extension = "xlsx", file_name = "spec", match_all = TRUE, strict = TRUE)
  # expect_equal(result, expected_output)
  #
  # # Test 3: Check if the function returns only files that match the file extension
  # test_that("Function returns files with matching extension", {
  #   folder_path <- "path/to/folder"
  #   create_files_in_folder(folder_path, file_extension = "csv", file_names = c("file1", "file2", "file3"))
  #   result <- find_files(folder_path, file_name = c("file1", "file2"), file_extension = "csv", match_all = TRUE, strict = FALSE)
  #   expect_true(all(grepl(".csv", result)))
  #
  #   remove_folder(folder_path)
  # })
  #
  # # Test 4: Check if the function returns files that match both the file name and the subfolder path
  # test_that("Function returns files that match both file name and subfolder path", {
  #   folder_path <- "path/to/folder"
  #   create_files_in_folder(folder_path, file_extension = "csv", file_names = c("file1", "file2", "file3"))
  #   subfolder_path <- "subfolder"
  #   create_files_in_folder(file.path(folder_path, subfolder_path), file_extension = "csv", file_names = c("file4", "file5", "file6"))
  #   result <- find_files(folder_path, subfolder_path = subfolder_path, file_name = c("file4", "file5"), file_extension = "csv", match_all = TRUE, strict = FALSE)
  #   expect_true(all(grepl(subfolder_path, result)))
  #   expect_true(all(grepl(".csv", result)))
  #
  #   remove_folder(folder_path)
  # })
  #
  # # Test 3: find_files with match_all = FALSE
  # expected_output <- c("tests/testthat/folder/Jan/5. Sample/2. Sample specs and design/GOS 2023 Operational Sample Spec.xlsx",
  #                      "tests/testthat/folder/Jan/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx",
  #                      "tests/testthat/folder/Nov/5. Sample/2. Sample specs and design/GOS 2023 Operational Sample Spec.xlsx",
  #                      "tests/testthat/folder/Nov/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx")
  # result <- find_files(folder_path = "tests/testthat/folder", subfolder_path = c("Jan", "Nov"), file_extension = "xlsx", file_name = c("Spec", "op"), match_all = FALSE, strict = FALSE)
  # test_that("find_files with match_all = FALSE returns correct files", {
  #   expect_equal(result, expected_output)
  # })
  #
  # # Test 5: Check if the function returns NULL when no matching files are found
  # test_that("Function returns NULL when no matching files are found", {
  #   folder_path <- "path/to/folder"
  #   create_files_in_folder(folder_path, file_extension = "csv", file_names = c("file1", "file2", "file3"))
  #   result <- find_files(folder_path, file_name = c("file4", "file5"),
  #                        match_all = TRUE, search_recursive = TRUE, return_full_path = TRUE)
  #   expect_null(result)
  # })
  #
  #
