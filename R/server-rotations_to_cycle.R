#' Convert Rotations Input to an Expanded Shift Cycle
#'
#' `rotations_to_cycle()` converts a string like `"4-2"` to a vector of `TRUE`
#' and `FALSE` values to pass to
#' \code{\link[schedules:schedule_by_cycle]{schedule_by_cycle()}}
#'
#' @param rotations Character. A string of alternating numbers and dashes
#'   representing the "on" and "off" periods of a rotating schedule. This
#'   must contain an even number of numerals and dashes between each numeral
#'   (i.e. "4-2-3-3" is a valid input; "4-2-3" or "4 2 3" is not).
#'
#' @export
rotations_to_cycle <- function(rotations) {
  rotations %>%
    std_rotations() %>%
    split_rotations() %>%
    validate_rotations() %>%
    as.integer() %>%
    expand_rotations()
}

#' \code{\link[schedules:rotations_to_cycle]{rotations_to_cycle}} Helpers
#'
#' @description
#' These functions assist `rotations_to_cycle()`:
#'
#' \itemize{
#'   \item{`std_rotations()` standardizes the `rotations` string}
#'   \item{`split_rotations()` splits the string into a vector of numbers}
#'   \item{`validate_rotations()` validates expectations for `rotations`}
#'   \item{`expand_rotations()` expands the numeric vector to a logical one}
#' }
#'
#' `std_rotations()` has helper functions of its own:
#'
#' \itemize{
#'   \item{`remove_whitespace()` removes all whitespace from a string}
#'   \item{`dash_to_space()` converts all dashes to spaces}
#'   \item{`keep_numeric_space()` removes all non-numeric/space characters}
#'   \item{`space_to_dash()` converts all spaces back to dashes}
#' }
#'
#' @param rtns The `rotations` vector in various formats
#'
#' @param string A character string
#'
#' @name rotations_to_cycle-helpers
NULL

#' @rdname rotations_to_cycle-helpers
#'
#' @export
expand_rotations <- function(rtns) {

  n <- vctrs::vec_size(rtns)

  on <- vctrs::vec_rep(c(TRUE, FALSE), n)

  scheduled_list <- list()

  for (i in seq_len(n)) {
    scheduled_list[[i]] <- vctrs::vec_rep(on[[i]], rtns[[i]])
  }

  unlist(scheduled_list)
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
validate_rotations <- function(rtns) {
  n <- vctrs::vec_size(rtns)

  nonzero <- n != 0L

  even <- n %% 2L == 0L

  error_msg <- paste(
   "`Rotations` must be an alternating sequence of numerals",
   "and dashes (i.e. 4-2-6-3) and must have an even number",
   "of numerals (i.e. 4-2-6 is not valid)."
  )

  shiny::validate(
    shiny::need(
      nonzero && even,
      message = error_msg
    )
  )

  rtns
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
split_rotations <- function(rtns) {
  rtns %>%
    stringr::str_split(pattern = "-") %>%
    unlist()
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
std_rotations <- function(rtns) {
  rtns %>%
    remove_whitespace() %>%
    dash_to_space() %>%
    keep_numeric_space() %>%
    space_to_dash()
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
remove_whitespace <- function(string) {
 stringr::str_remove_all(string, pattern = "[\t\n\r ]")
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
dash_to_space <- function(string) {
  stringr::str_replace_all(
    string,
    pattern = "-",
    replacement = " "
  )
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
keep_numeric_space <- function(string) {
  stringr::str_remove_all(string, pattern = "[^0-9 ]")
}

#' @rdname rotations_to_cycle-helpers
#'
#' @export
space_to_dash <- function(string) {
  string %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(
      pattern = " ",
      replacement = "-"
    )
}
