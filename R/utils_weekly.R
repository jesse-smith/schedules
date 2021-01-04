weekly_ui <- function() {
  shiny::sidebarLayout(
    sidebarPanel = weekly_input(),
    mainPanel = weekly_output()
  )
}

weekly_input <- function() {

  help_text <- paste(
    "This schedule repeats weekly.",
    "To create a calendar, select the days of the week",
    "that an individual on this schedule will work."
  )

  shiny::sidebarPanel(
    shiny::titlePanel("Weekly Schedule"),
    shiny::helpText(help_text),
    calendar_title("weekly_title"),
    date_range("weekly_dates"),
    shiny::h4("Working Days"),
    weekday_switch("Sun"),
    weekday_switch("Mon", value = TRUE),
    weekday_switch("Tue", value = TRUE),
    weekday_switch("Wed", value = TRUE),
    weekday_switch("Thu", value = TRUE),
    weekday_switch("Fri", value = TRUE),
    weekday_switch("Sat")
  )
}

weekly_output <- function() {
  shiny::mainPanel(shiny::plotOutput("weekly_calendar", height = "800px"))
}

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
#'
#' @param value The default button value, `TRUE` or `FALSE`
weekday_switch <- function(
  day = c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"),
  value = FALSE
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
    value = value,
    status = "primary",
    right = TRUE
  )
}

year_start <- function(date = lubridate::today()) {

  date <- lubridate::as_date(date)

  year <- lubridate::year(date)

  lubridate::as_date(paste0(year, "-01-01"))
}

year_end <- function(date = lubridate::today()) {

  date <- lubridate::as_date(date)

  year <- lubridate::year(date)

  lubridate::as_date(paste0(year, "-12-31"))
}
