#' data_range_month
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
dateRangeMonthsInput <- function(inputId, label, start = NULL, end = NULL,
                                 min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                                 minviewmode = "months", # added manually
                                 weekstart = 0, language = "en", separator = " to ", width = "90%") {

  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date")) start <- format(start, "%Y-%m-%d")
  if (inherits(end, "Date")) end <- format(end, "%Y-%m-%d")
  if (inherits(min, "Date")) min <- format(min, "%Y-%m-%d")
  if (inherits(max, "Date")) max <- format(max, "%Y-%m-%d")

  htmltools::attachDependencies(
    div(
      id = inputId,
      class = "shiny-date-range-input form-group shiny-input-container",
      style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
      controlLabel(inputId, label),
      # input-daterange class is needed for dropdown behavior
      div(
        class = "input-daterange input-group",
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode, # added manually
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = start
        ),
        span(class = "input-group-addon", separator),
        tags$input(
          class = "input-sm form-control",
          type = "text",
          `data-date-language` = language,
          `data-date-weekstart` = weekstart,
          `data-date-format` = format,
          `data-date-start-view` = startview,
          `data-date-min-view-mode` = minviewmode, # added manually
          `data-min-date` = min,
          `data-max-date` = max,
          `data-initial-date` = end
        )
      )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x)) {
    if (!is.null(y) && !any(is.na(y))) {
      return(y)
    }
  }
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
 (function() {
 var datepicker = $.fn.datepicker.noConflict();
 $.fn.bsDatepicker = datepicker;
 })();
 </script>"
)
