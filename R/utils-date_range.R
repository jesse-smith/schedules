date_range <- function(id) {
  shiny::dateRangeInput(
    id,
    label = shiny::h4("Date Range"),
    start = year_start(),
    end   = year_end()
  )
}

calendar_title <- function(id) {
  shiny::textInput(
    id,
    label = shiny::h4("Calendar Title"),
    value = "Team Schedule",
    placeholder = "Team Schedule"
  )
}
