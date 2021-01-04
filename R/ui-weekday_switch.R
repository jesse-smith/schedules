#' Create a Material Switch for a Day of the Week
#'
#' `weekday_switch()` creates a switch using
#' \code{\link[shinyWidgets:materialSwitch]{materialSwitch()}} for a given day
#' of the week. It labels the switch on the right side with the full weekday
#' name and lowercases the 3-day abbreviation for the `inputId`.
#'
#' @inherit shinyWidgets::materialSwitch return
#'
#' @param day A day of the week, as a 3-letter abbreviation in title case
weekday_switch <- function(
  day = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
) {

  # Match day argument - client-side, so using `arg_match0()` for speed
  day <- rlang::arg_match0(
    day,
    values = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  )[[1L]]

  # Using an explicit `switch()` expression, rather than `parse_weekday()`,
  # b/c the latter would be much more computationally expensive,
  # which is problematic for a client-side UI function.

  # Convert to lowercase for inputId
  day_lwr <- stringr::str_to_lower(day)

  # Convert to full day for label
  day_full <- switch(
    day,
    "Sun" = "Sunday",
    "Mon" = "Monday",
    "Tue" = "Tuesday",
    "Wed" = "Wednesday",
    "Thu" = "Thursday",
    "Fri" = "Friday",
    "Sat" = "Saturday"
  )

  # Create material switch
  shinyWidgets::materialSwitch(
    inputId = day_lwr,
    label = day_full,
    status = "primary",
    right = TRUE
  )
}
