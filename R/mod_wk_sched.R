#' weekly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_wk_sched_ui <- function(id){

  shiny::sidebarLayout(
    sidebarPanel = mod_wk_sched_input(id),
    mainPanel = mod_wk_sched_output(id)
  )
}

mod_wk_sched_input <- function(id) {

  help_text <- paste(
    "This schedule repeats weekly.",
    "To create a calendar, select the days of the week",
    "that an individual on this schedule will work."
  )

  shiny::sidebarPanel(
    shiny::titlePanel("Weekly Schedule"),
    shiny::helpText(help_text),
    calendar_title(id),
    date_range(id),
    shiny::h4("Working Days"),
    weekday_switch(id, "sun"),
    weekday_switch(id, "mon", value = TRUE),
    weekday_switch(id, "tue", value = TRUE),
    weekday_switch(id, "wed", value = TRUE),
    weekday_switch(id, "thu", value = TRUE),
    weekday_switch(id, "fri", value = TRUE),
    weekday_switch(id, "sat")
  )
}

mod_wk_sched_output <- function(id) {
  shiny::mainPanel(
    shiny::plotOutput(
      shiny::NS(id, "calendar"),
      height = "800px"
    )
  )
}

#' weekly Server Function
#'
#' @noRd
mod_wk_sched_server <- function(id) {
  shiny::moduleServer(id, mod_weekly_server_function)
}

mod_weekly_server_function <- function(input, output, session) {
  
  # Create reactive weekly cycle
  cycle <- shiny::reactive(
    c(
      "sun" = input$sun,
      "mon" = input$mon,
      "tue" = input$tue,
      "wed" = input$wed,
      "thu" = input$thu,
      "fri" = input$fri,
      "sat" = input$sat
    )
  )
  
  # Create reactive calendar viz
  calendar <- shiny::reactive(
    create_calendar(
      title = input$calendar_title,
      schedule = "custom",
      cycle = cycle(),
      start = input$date_range[[1L]],
      end = input$date_range[[2L]]
    )
  ) %>%
  # Prevent updating too frequently
  shiny::debounce(1000L)
  
  # Assign viz to output
  output$calendar <- shiny::renderPlot(calendar())
}