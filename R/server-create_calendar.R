#' Create a Scheduling Calendar
#'
#' `create_calendar()` creates a calendar from a given `schedule`, `start`
#' date, `end` date, and (if needed) `anchor` date. Custom schedules can be used
#' by setting `schedule = "custom"` and providing the scheduling cycle via
#' `cycle`. The schedule can be saved to pdf with `.pdf = TRUE`; the file name
#' is a standardized version of the title.
#'
#' @inheritParams schedule
#'
#' @param title The title of the calendar
#'
#' @param schedule The schedule to use
#'
#' @return A list defining the `calendR` object
create_calendar <- function(
  title = "Work Schedule",
  schedule = c("weekdays", "42", "5623", "custom"),
  start = "2021-01-01",
  end = "2021-12-31",
  anchor = start,
  cycle = NULL,
  save_as = NULL
) {

  # Create schedule
  schedule <- schedule_predefined(
    schedule = schedule,
    start = start,
    end = end,
    anchor = anchor,
    cycle = cycle
  )

  # Convert the schedule to a vector of "special days" for calendR
  scheduled_days <- schedule %>%
    dplyr::mutate(row = vctrs::vec_seq_along(.)) %>%
    dplyr::filter(.data[["scheduled"]]) %>%
    dplyr::pull(.data[["row"]])

  start_date <- min(schedule[["date"]], na.rm = TRUE)
  end_date <- max(schedule[["date"]], na.rm = TRUE)

  calendR::calendR(
    start_date = start_date,
    end_date = end_date,
    special.days = scheduled_days,
    title = title,
    font.style = "bold",
    title.col     = "#2C3E50",
    mbg.col       = "#2C3E50",
    weeknames.col = "#2C3E50",
    # bg.col     = "#f0f0f0",
    months.col = "white",
    # low.col    = "#f0f0f0",
    days.col = "#2C3E50",
    col      = "#2C3E50",
    special.col = "#2C3E5040",
    papersize = "A5",
    lwd = 1/2,
    pdf = !is.null(save_as),
    doc_name = if (is.null(save_as)) "" else save_as
  )
}
