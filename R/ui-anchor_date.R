#' UI Helper Functions
#'
#' These functions wrap or assist shiny UI functions.
#'
#' @param id ID assigned to the module calling the function
#'
#' @param inputId ID to give to this instance of the function
#'
#' @param label A user-facing label for the UI element
#'
#' @param date A date to use for determining the year; defaults to
#'   \code{\link[lubridate:today]{today()}}
#'
#' @name ui-utils
NULL

#' @rdname ui-utils
anchor_date <- function(id) {
  shiny::dateInput(
    shiny::NS(id, "anchor_date"),
    shiny::h4("Anchor Date"),
    value = year_first_monday()
  )
}

#' @rdname ui-utils
year_first_monday <- function(date = lubridate::today()) {
  date <- lubridate::as_date(date)

  candidates <- year_start(date) + seq_len(7L) - 1L

  is_monday <- weekdays(candidates) == "Monday"

  candidates[is_monday][[1L]]
}
