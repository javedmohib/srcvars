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
#' @examples eval = FALSE
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
#' @example eval = FALSE
#' paste_pathr()
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


#' QILT TCSI Website Functions
#' @param elm A TCSI element
#' @return Opens TCSI website for the element
#' @export
#' @importFrom stringr str_sub
#' @import glue
#' @examples
#' tcsi_web("470")
tcsi_web <- function(elm) {
  if (str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- str_sub(elm, 2, nchar(elm))
  }
  url <- glue::glue("https://www.tcsisupport.gov.au/element/", elm)
  utils::browseURL(url, browser = getOption("browser"),
                   encodeIfNeeded = FALSE)
}
