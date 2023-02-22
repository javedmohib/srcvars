#' Sample variables, its definition and values
#' @param var A QILT variable name
#' @return The definition of a variable, its values and labels
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select left_join
#' @import magrittr
#' @examples eval = FALSE
#' \dontrun{lkp_var("e329")}


lkp_var <- function(var) {

  label <-  value <-  variable <-  variable.label <- NULL

  ifelse(suppressWarnings(is.na(as.numeric(var))), var <- tolower(var), var <- paste0("e", var))

  pop_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2023/Nov/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx") %>% rename_all(tolower)
  pop_spec_val <- openxlsx::read.xlsx("K:/QILT/GOS/2023/Nov/5. Sample/2. Sample specs and design/GOS 2023 Population File Spec.xlsx", sheet = 2) %>% rename_all(tolower)

  op_spec_var <- openxlsx::read.xlsx("K:/QILT/GOS/2022/May/5. Sample/2. Sample specs and design/GOS 2022 Operational Sample Spec.xlsx") %>% rename_all(tolower)

  mos_var <- openxlsx::read.xlsx("K:/QILT/GOS/2023/Overall/10. Outputs/Data files/Specs/GOS 2023 Master Output Spec.xlsx") %>% rename_all(tolower)
  mos_val <- openxlsx::read.xlsx("K:/QILT/GOS/2023/Overall/10. Outputs/Data files/Specs/GOS 2023 Master Output Spec.xlsx", sheet = 2) %>% rename_all(tolower)

  response_var <-  openxlsx::read.xlsx("lookup/qilt_vars_misc.xlsx", sheet = "Variable")
  response_val <-  openxlsx::read.xlsx("lookup/qilt_vars_misc.xlsx", sheet = "Value")
  # colnames(pop_spec_var) <-  tolower(colnames(pop_spec_var))
  # colnames(pop_spec_val) <-  tolower(colnames(pop_spec_val))
  #
  # colnames(op_spec_var) <-  tolower(colnames(op_spec_var))

  if (var %in% tolower(pop_spec_var$variable)) {

    var_def <- pop_spec_var %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(variable, label, format)

    var_val <- pop_spec_val %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(value, label)

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val
                     )

    return(var_info)

  } else if (var %in% tolower(op_spec_var$variable)) {

    var_def <- op_spec_var %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(variable, "Variable Definition" = variable.label, format)
    return(as.list(var_def))

  } else if (var %in% tolower(mos_var$variable)) {

    var_def <- mos_var %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(variable, "Variable Definition" = label, format)

    var_val <- mos_val %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(value, label)

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val
    )

    return(var_info)

  } else if (var %in% tolower(response_var$variable)) {



    var_def <- response_var %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(variable, label)

    var_val <- response_val %>%
      filter(tolower(variable) %in% {{var}}) %>%
      select(value, label)

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val)

    return(var_info)


  } else {
    stop("Variable not found in any of the lookup files.")
  }
}



