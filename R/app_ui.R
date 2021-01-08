#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here
    shiny::navbarPage(
      title = "CRU Schedules",
      shiny::tabPanel(title = "Weekly Schedule", mod_wk_sched_ui("wk_sched")),
      shiny::tabPanel(title = "Rotating Schedule", mod_rotating_sched_ui("rotating_sched")),
      theme = shinythemes::shinytheme("flatly")
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  golem::add_resource_path(
    'www', app_sys('app/www')
  )

  tags$head(
    golem::favicon(),
    golem::bundle_resources(
      path = app_sys('app/www'),
      app_title = 'schedules'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
