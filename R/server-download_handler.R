#' `downloadHandler()` Wrapper for schedules Package
#'
#' `download_handler()` is paired with
#' \code{\link[schedules:download_button]{download_button()}}; it creates a
#' temporary file for the user to download.
#'
#' @param id The ID to assign to the handler and button
#'
#' @param title The title of the calendar, via
#'   `shiny::reactive(input$calendar_title)`
#'
#' @param w The width of the output, in inches, when content is left as default
#'
#' @param h The height of the output, in inches, when content is left as default
#'
#' @inheritParams shiny::downloadHandler
#'
#' @export
download_handler <- function(
  id,
  title,
  w = 8.5,
  h = 11,
  content = function(file) ggsave(file, device = id, width = w, height = h)
) {

  ext <- id %>%
    stringr::str_remove("^[.]") %>%
    stringr::str_squish() %>%
    stringr::str_to_lower()

  filename <- shiny::reactive({
    title %>%
      janitor::make_clean_names(
        use_make_names = FALSE,
        transliterations = c("Any-Latin", "Latin-ASCII")
      ) %>%
      paste0(".", ext)
  })

  shiny::downloadHandler(filename, content)
}
