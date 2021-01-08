anchor_date <- function(id) {
  shiny::dateInput(
    shiny::NS(id, "anchor_date"),
    shiny::h4("Anchor Date"),
    value = year_first_monday()
  )
}

year_first_monday <- function(date = lubridate::today()) {
  date <- lubridate::as_date(date)
  
  candidates <- year_start(date) + seq_len(7L) - 1L
  
  is_monday <- weekdays(candidates) == "Monday"
  
  candidates[is_monday][[1L]]
}