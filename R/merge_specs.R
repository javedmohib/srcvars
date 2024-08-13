#' ---
#' title: R/merge_specs.R
#' author: jmohib
#' created: 2023-11-27
#' output:
#'     html_document:
#'         toc: true
#'
#' overview: |
#'     This scripts merges Pop, OP and MOS specs
#' ---

# library(openxlsx)
# library(dplyr)
# library(magrittr)
#
# # Spec Paths ----
# POP_PATH <- "K:/QILT/GOS/2024/Nov/5. Sample/2. Sample specs and design/GOS 2024 Population File Spec.xlsx"
#
# OP_FILE_PATH <- "K:/QILT/GOS/2024/Overall/10. Outputs/Data files/Specs/GOS 2024 Operational file spec.xlsx"
#
# MOS_PATH <- "K:/QILT/GOS/2024/Overall/10. Outputs/Data files/Specs/GOS 2024 Master Output Spec.xlsx"
#
# VAR_KEYS <- c("Variable", "Label", "Format")
# VAL_KEYS <- c("Variable", "Value" ,"Label")
#
# # Read specs----
# pop_vars <- read.xlsx(POP_PATH, sheet = "Variables") %>% select(all_of(VAR_KEYS)) %>% mutate(spec = "POP")
# pop_vals <- read.xlsx(POP_PATH, sheet = "Values") %>% select(all_of(VAL_KEYS)) %>% mutate(spec = "POP")
#
# op_vars <- read.xlsx(OP_FILE_PATH, sheet = "Variables") %>% select(all_of(VAR_KEYS)) %>% mutate(spec = "OP")
# op_vals <- read.xlsx(OP_FILE_PATH, sheet = "Values") %>% select(all_of(VAL_KEYS)) %>% mutate(spec = "OP")
#
# mos_vars <- read.xlsx(MOS_PATH, sheet = "Variables") %>% select(all_of(VAR_KEYS)) %>% mutate(spec = "MOS")
# mos_vals <- read.xlsx(MOS_PATH, sheet = "Values") %>% select(all_of(VAL_KEYS)) %>% mutate(spec = "MOS")
#
# # Combine ----------
# vars_comb <- bind_rows(pop_vars, op_vars, mos_vars)
# vals_comb <- bind_rows(pop_vals, op_vals, mos_vals)
#
# # Remove duplicates
#
# vars_comb_dist <- vars_comb %>%
#   mutate(Variable = tolower(Variable)) %>%
#   group_by(Variable, Label, Format) %>%
#   summarise(spec = paste(spec, collapse = ",")) %>%
#   distinct() %>%
#   ungroup()
#
# vals_comb_dist <- vals_comb %>%
#   mutate(Variable = tolower(Variable)) %>%
#   group_by(Variable, Value, Label) %>%
#   summarise(spec = paste(spec, collapse = ",")) %>%
#   distinct() %>%
#   arrange(Variable, as.numeric(Value)) %>%
#   ungroup()
#
# # Write out
#
#
# wb <- createWorkbook()
#
# addWorksheet(wb, "Variables")
# writeData(wb, sheet = "Variables", x = vars_comb_dist)
#
# addWorksheet(wb, "Values")
# writeData(wb, sheet = "Values", x = vals_comb_dist)
#
# # Save the workbook to a file
# saveWorkbook(wb, file = "inst/extdata//gos_variable_lookup.xlsx", overwrite = TRUE)


library(openxlsx)
library(dplyr)
library(glue)

merge_specs <- function(POP_PATH, OP_FILE_PATH, MOS_PATH, PROJECT) {
  VAR_KEYS <- c("Variable", "Label", "Format")
  VAL_KEYS <- c("Variable", "Value", "Label")

  # Read specs----
  pop_vars <- read.xlsx(POP_PATH, sheet = "Variables") %>%
    select(all_of(VAR_KEYS)) %>%
    mutate(spec = "POP")

  pop_vals <- read.xlsx(POP_PATH, sheet = "Values") %>%
    select(all_of(VAL_KEYS)) %>%
    mutate(spec = "POP")

  op_vars <- read.xlsx(OP_FILE_PATH, sheet = "Variables") %>%
    select(all_of(VAR_KEYS)) %>%
    mutate(spec = "OP")

  op_vals <- read.xlsx(OP_FILE_PATH, sheet = "Values") %>%
    select(all_of(VAL_KEYS)) %>%
    mutate(spec = "OP")

  mos_vars <- read.xlsx(MOS_PATH, sheet = "Variables") %>%
    select(all_of(VAR_KEYS)) %>%
    mutate(spec = "MOS")

  mos_vals <- read.xlsx(MOS_PATH, sheet = "Values") %>%
    select(all_of(VAL_KEYS)) %>%
    mutate(spec = "MOS")

  # Combine ----------
  vars_comb <- bind_rows(pop_vars, op_vars, mos_vars)
  vals_comb <- bind_rows(pop_vals, op_vals, mos_vals)

  # Remove duplicates
  vars_comb_dist <- vars_comb %>%
    mutate(Variable = tolower(Variable)) %>%
    group_by(Variable, Label, Format) %>%
    summarise(spec = paste(spec, collapse = ",")) %>%
    distinct() %>%
    ungroup()

  vals_comb_dist <- vals_comb %>%
    mutate(Variable = tolower(Variable)) %>%
    group_by(Variable, Value, Label) %>%
    summarise(spec = paste(spec, collapse = ",")) %>%
    distinct() %>%
    arrange(Variable, as.numeric(Value)) %>%
    ungroup()

  # Write out
  wb <- createWorkbook()

  addWorksheet(wb, "Variables")
  writeData(wb, sheet = "Variables", x = vars_comb_dist)

  addWorksheet(wb, "Values")
  writeData(wb, sheet = "Values", x = vals_comb_dist)

  # Save the workbook to a file
  file_name <- glue::glue("C:/managed/R/win-library/srcvars/extdata/{PROJECT}_variable_lookup.xlsx")
  saveWorkbook(wb, file = file_name, overwrite = TRUE)
}


merge_specs(
  "K:/QILT/GOS/2024/Nov/5. Sample/2. Sample specs and design/GOS 2024 Population File Spec.xlsx",
  "K:/QILT/GOS/2024/Overall/10. Outputs/Data files/Specs/GOS 2024 Operational file spec.xlsx",
  "K:/QILT/GOS/2024/Overall/10. Outputs/Data files/Specs/GOS 2024 Master Output Spec.xlsx",
  "GOS")


merge_specs(
  "K:/QILT/SES/2023/5. Sample/2. Sample specs and design/SES 2023 Population File Spec.xlsx",
  "K:/QILT/SES/2023/5. Sample/2. Sample specs and design/SES 2023 Population File Spec.xlsx",
  "K:/QILT/SES/2023/10. Outputs/Data files/Specs/archive/Copy of SES 2023 Master Output Spec.xlsx",
  "SES"
)

merge_specs(
  "K:/QILT/GOS-L/2023/5. Sample/2. Sample specs and design/GOS-L 2023 Population File Spec.xlsx",
  "K:/QILT/GOS-L/2023/5. Sample/2. Sample specs and design/GOS-L 2023 Population File Spec.xlsx",
  "K:/QILT/GOS-L/2023/10. Outputs/Data files/Specs/GOSL 2023 Master Outputs Spec.xlsx",
  "GOS-L"
)



#---
#EOF
#---
