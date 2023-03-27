#' QILT TCSI functions
#' These functions provide format checks for TCSI variables
#' @param elm
#' @import stringr
#' @import glue
#' @return
#' @export

tcsi_var <- function(elm) {

  if (!stringr::str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- paste0("e", elm)

  }
  if (!is_tcsi(elm)) {
    stop(getOption("srcqilt.name"), ": `", elm, "` is not a valid tcsi element",
         call. = FALSE)
  } else{

    tcsi_spec <- openxlsx::read.xlsx("lookup/tcsi_spec.xlsx")

    var_def <- tcsi_spec %>%
      filter(tolower(element) %in% {{elm}}) %>%
      select(element, "Variable Definition" = label, type, width)

    var_val <- tcsi_spec %>%
      filter(tolower(element) %in% {{elm}}) %>%
      select(label, description)

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val)

    return(var_info)

  }

}

#' @export
is_tcsi <- function(elm) {
  if (!stringr::str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- paste0("e", elm)
  }
  tcsi_vars <- openxlsx::read.xlsx("data/1 tcsi_spec.xlsx")
  tolower(elm) %in% tcsi_vars$element
}

#' @export

tcsi_web <- function(elm) {
  if (str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- str_remove(elm,"e")
  }
  # elm <- as.character(substitute(elm)) %>% tolower()
  if (!is_tcsi(elm)) {
    stop(getOption("srcqilt.name"), ": `", elm, "` is not a valid tcsi element",
         call. = FALSE)
  } else{
    url <- glue::glue("https://www.tcsisupport.gov.au/element/", elm)
    utils::browseURL(url, browser = getOption("browser"),
                     encodeIfNeeded = FALSE)
  }
}



