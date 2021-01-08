#' Create a Material Switch for a Day of the Week
#'
#' `weekday_switch()` creates a switch using
#' \code{\link[shinyWidgets:materialSwitch]{materialSwitch()}} for a given day
#' of the week. It labels the switch on the right side with the full weekday
#' name and lowercases the 3-day abbreviation for the `inputId`. This
#' is designed to work inside Shiny modules.
#'
#' @inherit shinyWidgets::materialSwitch params return
#'
#' @param id The id of the Shiny module calling this function
#'
#' @param day A day of the week, as a 3-letter abbreviation in title case
weekday_switch <- function(
  id,
  day = c("sun", "mon", "tue", "wed", "thu", "fri", "sat"),
  value = FALSE
) {

  # Match day argument - client-side, so using `arg_match0()` for speed
  day <- rlang::arg_match0(
    day,
    values = c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
  )[[1L]]

  # Using an explicit `switch()` expression, rather than `parse_weekday()`,
  # b/c the latter would be much more computationally expensive,
  # which is problematic for a client-side UI function.

  # Convert to lowercase for inputId
  day_lwr <- stringr::str_to_lower(day)

  # Convert to full day for label
  day_full <- switch(
    day,
    "sun" = "Sunday",
    "mon" = "Monday",
    "tue" = "Tuesday",
    "wed" = "Wednesday",
    "thu" = "Thursday",
    "fri" = "Friday",
    "sat" = "Saturday"
  )

  # Create material switch
  shinyWidgets::materialSwitch(
    shiny::NS(id, day_lwr),
    day_full,
    value = value,
    status = "primary",
    right = TRUE
  )
}
