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
      shiny::tabPanel(title = "Weekly Schedule", mod_weekly_ui("weekly_ui_1")),
      shiny::tabPanel(title = "Rotating Schedule", rotating_layout()),
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

rotating_input <- function() {

}

rotating_output <- function() {
  shiny::mainPanel(shiny::plotOutput("rotating_calendar"))
}

rotating_layout <- function() {
  shiny::sidebarLayout(
    sidebarPanel = rotating_input(),
    mainPanel = rotating_output()
  )
}
