#' Extracting first and last digit from a digit-containing string
#' The function is used to solve first part of 20231201 puzzle
#' from https://adventofcode.com/2023/day/1
#'
#' @param trebuchet_string a string
#'
#' @return calibration_value a number formed by first and last string from trebuchet_string
#' @export
#'
extract_first_last_digit <- function(trebuchet_string){
  # Extract all digits
  matches <- regmatches(trebuchet_string, gregexpr("[[:digit:]]+", trebuchet_string))
  all_digits <- as.numeric(unlist(matches))
  if (length(all_digits) == 0){
    calibration_value <- 0
  } else {
    # Combine all the digits
    digit_string <- paste0(all_digits, collapse = "")
    # Take first and last digit
    calibration_value <- as.numeric(paste0(substr(digit_string,1,1),
                                           substr(digit_string,
                                                  nchar(digit_string),
                                                  nchar(digit_string))))
    }
  return(calibration_value)
}

