#' Sample varaibles, its definition and values
#'
#' @param var A QILT variable name
#'
#' @return The definition of a variable, its values and labels
#' @export
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select left_join
#' @import magrittr
#' @examples
#' lkp_var("DOMINT")

lkp_var <- function(var) {

  label <-  value <-  variable <-  variable.label <- NULL

  ifelse(suppressWarnings(is.na(as.numeric(var))), var <- tolower(var), var <- paste0("e", var))

  pop_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2022/May/5. Sample/2. Sample specs and design/GOS 2022 Population File Spec.xlsx")
  pop_spec_val <- openxlsx::read.xlsx("K:/QILT/GOS/2022/May/5. Sample/2. Sample specs and design/GOS 2022 Population File Spec.xlsx", sheet = 2)

  op_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2022/May/5. Sample/2. Sample specs and design/GOS 2022 Operational Sample Spec.xlsx")

  colnames(pop_spec_var) <-  tolower(colnames(pop_spec_var))
  colnames(pop_spec_val) <-  tolower(colnames(pop_spec_val))
  pop_spec_var$variable <- tolower(pop_spec_var$variable)
  pop_spec_val$variable <- tolower(pop_spec_val$variable)

  colnames(op_spec_var) <-  tolower(colnames(op_spec_var))
  op_spec_var$variable <- tolower(op_spec_var$variable)

  if (var %in% pop_spec_var$variable) {

    var_def <- pop_spec_var %>%
      filter(variable %in% {{var}}) %>%
      select(variable, label)

    var_val <- pop_spec_val %>%
      filter(variable %in% {{var}}) %>%
      select(variable, value, label)

    temp_df <- dplyr::left_join(var_def, var_val, by = "variable")
    colnames(temp_df) <- c("Variable", "Variable Definition", "Valid Values", "Value Labels")

    if (nrow(temp_df) > 1) {
      temp_df [2:nrow(temp_df), c(1, 2)] <- ""
    }

    return(as_tibble(temp_df))

  } else {

    var_def <- op_spec_var %>%
      filter(variable %in% {{var}}) %>%
      select(variable, "Variable Definition" = variable.label)
    return(as_tibble(var_def))

  }
}


#' QILT TCSI Website Functions
#'
#' @param elm A TCSI element
#'
#' @return OPen TCSI website for the element
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
