#' @rdname ui-utils
rotations <- function(id) {
  shiny::textInput(
    shiny::NS(id, "rotations"),
    shiny::h4("Rotations"),
    value = "5-2"
  )
}
