download_button <- function(id, inputId = "download") {
  shiny::downloadButton(
    shiny::NS(id, inputId),
    "Download"
  )
}