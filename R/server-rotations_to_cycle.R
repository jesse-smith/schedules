rotations_to_cycle(rotations) {
  rotations %>%
    std_rotations() %>%
    split_rotations() %>%
    validate_rotations() %>%
    as.integer() %>%
    expand_rotations()
}

expand_rotations <- function(rtns) {
  
  n <- vctrs::vec_size(rtns)
  
  on <- vctrs::vec_rep(c(TRUE, FALSE), n)
  
  scheduled_list <- list()
  
  for (i in seq_len(n)) {
    scheduled_list[[i]] <- vctrs::vec_rep(on[[i]], rtns[[i]])
  }
  
  unlist(scheduled_list)
}

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

split_rotations <- function(rtns) {
  rtns %>%
    stringr::str_split(pattern = "-") %>%
    unlist()
}

std_rotations <- function(rtns) {
  rtns %>%
    remove_whitespace() %>%
    dash_to_space() %>%
    keep_numeric_space() %>%
    space_to_dash()
}


remove_whitespace <- function(string) {
 stringr::str_remove_all(string, pattern = "[\t\n\r ]")
}

dash_to_space <- function(string) {
  stringr::str_replace_all(
    string,
    pattern = "-",
    replacement = " "
  )
}

keep_numeric_space <- function(string) {
  stringr::str_remove_all(string, pattern = "[^0-9 ]")
}

space_to_dash <- function(string) {
  string %>%
    stringr::str_squish() %>%
    stringr::str_replace_all(
      pattern = " ",
      replacement = "-"
    )
}