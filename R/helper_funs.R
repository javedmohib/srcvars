#' Helper to check email mis-sort in sample processing
#' @param df A dataframe to extract variables from (Sample file)
#' @param E402 Last name
#' @param E403 First name
#' @param EMAIL1 Email1
#' @param EMAIL2 Email2
#' @param EMAIL3 Email3
#' @param ... Any other variable you want to display
#' @param n Number of samples
#' @return
#' @export
#' @import dplyr
#' @import magrittr
#' @examples
#' \dontrun{sample_emails (df, n = 10)}
sample_emails <- function(df, ..., n = 10) {
  df %>%
    filter(!E402 %in% c(NA, ""), !EMAIL1 %in% c(NA, "")) %>%
    select(E313, E402, E403, EMAIL1, EMAIL2, EMAIL3, ...) %>%
    sample_n(n)
}



#' Changes windows path to R path
#' @return
#' @export
#' @import stringr

paste_pathr <- function() {

  if (str_count(str_replace_all(readClipboard(), "\\\\", "/"), "\"") == 2) { # checking if the path is copied as a path
    x <- str_replace_all(readClipboard(), "\\\\", "/") %>%
      str_sub(2) # removes the backslash at the start
    str_sub(x , -1, -1) <- "" # removes backslash at the end
    x

  } else {
    str_replace_all(readClipboard(), "\\\\", "/")
  }

}


#' Open K drive from R session
#'
#' @param path
#'
#' @return
#' @export
#'
#' @examples
goto_k <- function(path = getwd()) {

  # Replace the "Z:" drive with "K:"
  path <- gsub("Z:/Consulting/Jobs", "K:", path)

  # Run the shell.exec command using the current working directory
  shell.exec(path)
}
