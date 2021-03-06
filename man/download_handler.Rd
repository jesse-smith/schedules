% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/server-download_handler.R
\name{download_handler}
\alias{download_handler}
\title{`downloadHandler()` Wrapper for schedules Package}
\usage{
download_handler(
  id,
  title,
  w = 8.5,
  h = 11,
  content = function(file) ggsave(file, device = id, width = w, height = h)
)
}
\arguments{
\item{id}{The ID to assign to the handler and button}

\item{title}{The title of the calendar, via
`shiny::reactive(input$calendar_title)`}

\item{w}{The width of the output, in inches, when content is left as default}

\item{h}{The height of the output, in inches, when content is left as default}

\item{content}{A function that takes a single argument \code{file} that is a
file path (string) of a nonexistent temp file, and writes the content to
that file path. (Reactive values and functions may be used from this
function.)}
}
\description{
`download_handler()` is paired with
\code{\link[schedules:download_button]{download_button()}}; it creates a
temporary file for the user to download.
}
