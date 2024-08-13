#' Sample variables, its definition and values
#' @param var A QILT variable name
#' @return The definition of a variable, its values and labels
#' @export
#' @importFrom openxlsx read.xlsx
#' @importFrom tibble as_tibble
#' @importFrom dplyr filter select left_join
#' @import magrittr
#' @import stringr
#' @examples eval = FALSE
#' \dontrun{lkp_var("e329")}


lkp_var <- function(var, project = "GOS", tolerance = 0.5) {

  # Function to find similar variables
  find_similar_variables <- function(variable, variables) {
    similar_vars <- variables[stringdist::stringdist(variables, variable, method = "jaccard") < tolerance]  # Adjust the tolerance as needed
    return(as_tibble(similar_vars))
  }

  project <- toupper(project)
  if(project == "GOSL") project <- "GOS-L"

  label <-  value <-  variable <-  variable.label <- NULL
  FILE_PATH <- file.path(system.file(package = "srcvars"))
  FILE_PATH <- glue::glue("{FILE_PATH}/extdata/{project}_variable_lookup.xlsx")



  ifelse(suppressWarnings(is.na(as.numeric(var))), var <- tolower(var), var <- paste0("e", var))

  variables <- openxlsx::read.xlsx(FILE_PATH, sheet = "Variables") %>%
    mutate(Variable = tolower(Variable)) %>%
    rename_all(tolower)
  values <- openxlsx::read.xlsx(FILE_PATH, sheet = "Values") %>%
    mutate(Variable = tolower(Variable)) %>%
    rename_all(tolower)

  # Check if the exact match exists
  exact_match <- var %in% variables$variable


  if (exact_match) {

    var_def <- variables %>%
      filter(variable %in% {{var}}) %>%
      select(variable, label, format, spec)

    var_val <- values %>%
      filter(variable %in% {{var}}) %>%
      select(value, label, spec)

    var_info <- list(
      Variable = as.list(var_def),
      Value = var_val
    )

    return(var_info)


  } else {
    # Find similar variables
    similar_vars <- find_similar_variables(var, variables$variable)
    if (length(similar_vars) > 0) {
      message(paste("Variable not found in any of the lookup files. But similar variable(s) found:\n", paste(similar_vars, collapse = ", ")))
    } else {
      stop("Variable not found in any of the lookup files.")
    }
  }
}



