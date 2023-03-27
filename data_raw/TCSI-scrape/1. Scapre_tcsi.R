#' ---
#' title: Scrape TCSI
#' author: Javed Mohib
#' created: 2022-05-12
#' output:
#'     html_document:
#'         toc: true
#'
#' overview: |
#'     Scrapes https://www.tcsisupport.gov.au/elements for element information.
#' ---

srcproj::load_settings()
library(purrr)
library(xml2)
library(rvest)
# Functions --------------------------------------------------------------------

scrape_tcsi <- function(element) {
  # Read html (allow for possible 404 error)
  element_html <- tryCatch(
    read_html(str_c("https://www.tcsisupport.gov.au/element/", element)),
    error = function(e) NULL
  )

  if (is.null(element_html)) return(NULL) # Skip if 404'd

  # Pull variable label
  element_label <- html_nodes(element_html, ".element-heading") %>%
    html_text() %>%
    str_trim()

  # Pull description
  element_desc <- html_nodes(element_html, ".element-description p") %>%
    html_text() %>%
    str_replace_all("\\s+", " ") %>%
    str_remove("Description") %>%
    str_trim()

  # Pull format
  element_format <- html_nodes(element_html, "div.element-summary-right") %>%
    html_text() %>%
    str_replace_all("\\s+", " ")

    # Extract information using regular expressions
  type <- sub('.*Element Type:\\s*(\\w+).*', '\\1', element_format)
  width <- sub('.*Width:\\s*(\\d+).*', '\\1', element_format)
  version <- sub('.*Version:\\s*([0-9]+(\\.[0-9]+)?).*', '\\1', element_format)

  # Create dataframe with the extracted information
  element_format <- data.frame(type = type, width = width, version = version)


  # element_format <- element_format[str_detect(element_format, "(Element Type|Width|Version \\d)")] %>%
  #   str_remove("(Element Type|Width|Version)") %>%
  #   str_trim() %>%
  #   set_names(c("type", "width", "version"))

  # Pull allowable values
  element_values <- html_nodes(element_html, "div.element-allowable-value") %>%
    html_table()

  if (length(element_values) > 0) {
    element_values <- map(element_values, ~mutate_all(.x, as.character))
    element_values <- bind_rows(element_values)
  } else {
    element_values <- NULL
  }

  # Check if retired
  element_retired <- html_nodes(element_html, "#archived-message") %>%
    html_text() %>%
    str_trim()

  # Return
  list(
    label = element_label,
    desc = element_desc,
    format = element_format,
    values= element_values,
    retired = element_retired
  )
}

with_delay <- function(f, delay = runif(1, 0.5, 1)) {
  force(f)
  force(delay)
  function(...) {
    Sys.sleep(delay)
    f(...)
  }
}

with_progress <- function(f, pb) {
  force(f)
  force(pb)
  function(...) {
    out <- f(...)
    pb$tick()
    out
  }
}

# Scrape -----------------------------------------------------------------------

# Scrape with random 0.5-1s delay between elements
all_elements <- 306:791

out <- map(
  all_elements,
  safely(with_progress(
    with_delay(scrape_tcsi),
    progress::progress_bar$new(total = length(all_elements))
  ))
) %>% set_names(str_c("e", all_elements))

# Check errors
map(out, "error") %>% compact()

out <- map(out, "result")

# Create data specifications ---------------------------------------------------

tcsi_var <- imap_dfr(
  compact(out),
  function(out, element) {
    tibble(!!!out$format) %>%
      mutate(
        label = out$label,
        description = out$desc,
        element = element,
        retired = ifelse(length(out$retired) == 0, "", out$retired)
      ) %>%
      select(element, label, description, everything())
  }
)

tcsi_val <- imap_dfr(
  compact(map(out, "values")),
  function(out, element) {
    out %>%
      select(value = 1, label = 2) %>%
      transmute(element = element, value = as.character(value), label = label)
  }
)

tcsi_val %<>%
  mutate(
    # Clean up values
    value = str_remove_all(value, "\\p{Cf}"),
    value = str_remove_all(value, "<U+200B>"),
    value = str_replace_all(value, "\\s+", " "),
    value = str_trim(value),

    # Clean up labels
    label = str_remove_all(label, "\\p{Cf}"),
    label = str_remove_all(label, "<U+200B>"),
    label = str_replace_all(label, "\\s+", " "),
    label = str_trim(label)
  ) %>%
  filter(label != "MEANING") %>%
  as_tibble()



# Output -----------------------------------------------------------------------
wb <- createWorkbook()
addWorksheet(wb, "Variable")
addWorksheet(wb, "Value")
writeData(wb, "Variable", x = tcsi_var)
writeData(wb, "Value", x = tcsi_val)
saveWorkbook(wb, "data/1 tcsi_spec.xlsx", overwrite = TRUE)


#---
#EOF
#---
