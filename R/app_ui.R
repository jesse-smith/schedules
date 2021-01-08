#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#'
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # List the first level UI elements here
    shiny::navbarPage(
      title = "CRU Schedules",
      shiny::tabPanel(title = "Weekly Schedule", mod_wk_sched_ui("wk_sched")),
      shiny::tabPanel(title = "Rotating Schedule", mod_rotating_sched_ui("rotating_sched")),
      theme = shinythemes::shinytheme("flatly")
    )
  )
}
