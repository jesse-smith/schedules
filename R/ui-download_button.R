#' @rdname ui-utils
download_dropdown <- function(id, inputId = "download_dropdown") {
  shinyWidgets::dropdownButton(
    download_button(id, "png"),
    shiny::br(),
    download_button(id, "pdf"),
    shiny::br(),
    download_button(id, "svg"),
    circle = FALSE,
    status = "primary",
    label = "Download"
  )
}

#' @rdname ui-utils
download_button <- function(
  id,
  inputId = "download",
  label = toupper(gsub("_", " ", inputId))
) {

  css <- paste0("
  .download{
    color: white !important;
    background-color: #2C3E50;
    border-color: white !important;
  }
  .download:hover{
    border-color: #2C3E50 !important;
  }
  ")

  shiny::downloadButton(
    shiny::NS(id, inputId),
    label,
    class = "download",
    shiny::tags$head(shiny::tags$style(css))
  )
}
