calendar_title <- function(id) {
  shiny::textInput(
    shiny::NS(id, "calendar_title"),
    label = shiny::h4("Calendar Title"),
    value = "Team Schedule",
    placeholder = "Team Schedule"
  )
}