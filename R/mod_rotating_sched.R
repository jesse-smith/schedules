#' Rotating Schedule Module UI
#'
#' @param id The ID to assign to the module
#'
#' @noRd
mod_rotating_sched_ui <- function(id) {
  shiny::sidebarLayout(
    sidebarPanel = mod_rotating_sched_input(id),
    mainPanel = mod_rotating_sched_output(id)
  )
}

#' Rotating Schedule Inputs
#'
#' UI inputs for the rotating schedule module. These are displayed in the
#' sidebar.
#'
#' @inheritParams mod_rotating_sched_ui
#'
#' @noRd
mod_rotating_sched_input <- function(id) {
  help_text <- paste(
    "This schedule repeats on a user-defined basis.",
    "To create a calendar, enter one or more 'on-off' periods",
    "that an individual on this schedule will work,",
    "plus an 'anchor' date to use as a start date for calculations.",
    "The periods should be separated by dashes and",
    "start with the 'on' phase (i.e. 4-2-5-3 means",
    "4 days on, 2 days off, 5 days on, 3 days off, repeat)."
  )

  shiny::sidebarPanel(
    shiny::titlePanel("Rotating Schedule"),
    shiny::helpText(help_text),
    calendar_title(id),
    date_range(id),
    anchor_date(id),
    rotations(id),
    shiny::br(),
    download_dropdown(id)
  )
}


#' Rotating Schedule Inputs
#'
#' UI output for the rotating schedule module. This is displayed in the
#' main panel.
#'
#' @inheritParams mod_rotating_sched_ui
#'
#' @noRd
mod_rotating_sched_output <- function(id) {
  shiny::mainPanel(
    shiny::plotOutput(
      shiny::NS(id, "calendar"),
      height = "800px"
    )
  )
}

#' Rotating Schedule Module Server
#'
#' `moduleServer()` wrapper for rotating schedule module server
#'
#' @inheritParams mod_rotating_sched_ui
#'
#' @noRd
mod_rotating_sched_server <- function(id) {
  shiny::moduleServer(
    id,
    mod_rotating_sched_server_function
  )
}

#' Server Function for Rotating Schedule Module
#'
#' Back-end for rotating schedule module
#'
#' @inheritParams mod_rotating_sched_ui
#'
#' @noRd
mod_rotating_sched_server_function <- function(input, output, session) {
  # Create reactive cycle
  cycle <- shiny::reactive(
    rotations_to_cycle(input$rotations)
  )

  # Create calendar viz
  calendar <- shiny::reactive(
    create_calendar(
      title = input$calendar_title,
      schedule = "custom",
      cycle = cycle(),
      start = input$date_range[[1L]],
      end = input$date_range[[2L]],
      anchor = input$anchor_date
    )
  ) %>%
    # Prevent premature invalidation
    shiny::debounce(1e3L)

  # Assign viz to output
  output$calendar <- shiny::renderPlot(calendar())

  # Reactive title used in filename for download
  title <- shiny::reactive(input$calendar_title) %>% shiny::debounce(1000L)

  # React to download
  output$png  <- download_handler("png", title())
  output$pdf  <- download_handler("pdf", title())
  output$html <- download_handler("html", title())
  output$svg  <- download_handler("svg", title())
}
