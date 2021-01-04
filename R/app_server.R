#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
app_server <- function(input, output, session) {

  weekly_cycle <- rlang::expr(
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

  weekly_calendar <- rlang::expr(
    create_calendar(
      title = input$weekly_title,
      schedule = "custom",
      cycle = eval(weekly_cycle),
      start = input$weekly_dates[[1L]],
      end = input$weekly_dates[[2L]]
    )
  )

  output$weekly_calendar <- shiny::renderPlot(eval(weekly_calendar))
}
