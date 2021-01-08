#' @rdname ui-utils
date_range <- function(id) {
  shiny::dateRangeInput(
    shiny::NS(id, "date_range"),
    label = shiny::h4("Date Range"),
    start = year_start(),
    end   = year_end()
  )
}

#' @rdname ui-utils
year_start <- function(date = lubridate::today()) {

  date <- lubridate::as_date(date)

  year <- lubridate::year(date)

  lubridate::as_date(paste0(year, "-01-01"))
}

#' @rdname ui-utils
year_end <- function(date = lubridate::today()) {

  date <- lubridate::as_date(date)

  year <- lubridate::year(date)

  lubridate::as_date(paste0(year, "-12-31"))
}
