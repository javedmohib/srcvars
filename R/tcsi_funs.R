#' @title QILT TCSI functions
#' @description These functions provide format checks for TCSI variables
#'
#' @param elm A TCSI element
#'
#' @import stringr
#' @import glue
#'
#' @return
#' @rdname tcsi
#' @export


tcsi_var <- function(elm) {

  if (!stringr::str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- paste0("e", elm)

  }
  if (!is_tcsi(elm)) {
    stop(getOption("srcqilt.name"), ": `", elm, "` is not a valid tcsi element",
         call. = FALSE)
  } else{

    tcsi_variables <- openxlsx::read.xlsx("lookup/1_tcsi_spec.xlsx")
    tcsi_values <-  openxlsx::read.xlsx("lookup/1_tcsi_spec.xlsx", sheet = 2)

    var_def <- tcsi_variables %>%
      filter(tolower(element) %in% {{elm}}) %>%
      select(element, "Variable Definition" = label, type, width)

    var_val <- tcsi_values %>%
      filter(tolower(element) %in% {{elm}}) %>%
      select(value, label) %>%
      as_tibble()

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val)

    return(var_info)

  }

}

#' @rdname tcsi
#' @export
is_tcsi <- function(elm) {
  if (!stringr::str_sub(elm, 1, 1) %in% c("e", "E")) {
    elm <- paste0("e", elm)
  }

  tcsi_vars <- openxlsx::read.xlsx("lookup/1_tcsi_spec.xlsx")
  tolower(elm) %in% tcsi_vars$element
}


#' @rdname tcsi
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
    url <- str_glue("https://www.tcsisupport.gov.au/element/", elm)
    utils::browseURL(url, browser = getOption("browser"),
                     encodeIfNeeded = FALSE)
  }
}



