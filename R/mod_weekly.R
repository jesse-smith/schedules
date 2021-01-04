#' weekly UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
mod_weekly_ui <- function(id){
  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::sidebarLayout(
      sidebarPanel = weekly_input(),
      mainPanel = weekly_output()
    )
  )
}

#' weekly Server Function
#'
#' @noRd
mod_weekly_server <- function(input, output, session){
  ns <- session$ns
}

## To be copied in the UI
# mod_weekly_ui("weekly_ui_1")
