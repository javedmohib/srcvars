#' Sample varaibles, its definition and values
#'
#' @param var A GOS variable name
#'
#' @return The definition of a variable, values and labels
#' @export
#'
#' @importFrom openxlsx read.xlsx
#' @importFrom dplyr filter select left_join
#' @import magrittr
#' @examples
#' lkp_var("DOMINT")

lkp_var <- function(var) {

  ifelse(suppressWarnings(is.na(as.numeric(var))), var <- tolower(var), var <- paste0("e", var))

  pop_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2022/Feb/5. Sample/2. Sample specs and design/GOS 2022 Population File Spec.xlsx")
  pop_spec_val <- openxlsx::read.xlsx("K:/QILT/GOS/2022/Feb/5. Sample/2. Sample specs and design/GOS 2022 Population File Spec.xlsx", sheet = 2)

  op_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2022/Feb/5. Sample/2. Sample specs and design/GOS 2022 Operational Sample Spec.xlsx")

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
    return(temp_df)

  } else {

    var_def <- op_spec_var %>%
      filter(variable %in% {{var}}) %>%
      select(variable, "Variable Definition" = variable.label)
    return(var_def)

  }


}

