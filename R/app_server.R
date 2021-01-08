#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#'
#' @noRd
app_server <- function(input, output, session) {
  mod_wk_sched_server("wk_sched")
  mod_rotating_sched_server("rotating_sched")
}
