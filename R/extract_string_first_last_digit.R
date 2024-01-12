#' Extracting first and last digit from a digit-containing string. Digit can be
#' either the digit itself or string version of it (e.g. "one" or 1 etc).
#' This is a modified version of extract_first_last_digit() to solve second part
#'  of 20231201 puzzle from https://adventofcode.com/2023/day/1
#'
#' @param trebuchet_string a string
#'
#' @return calibration_value, a number formed by first and last string from trebuchet_string
#' @export
#'
extract_string_first_last_digit <- function(trebuchet_string){
  my_dict <- c(c(1:9),c(1:9))
  names(my_dict) <- c("one","two","three","four","five","six","seven","eight","nine",
                      as.character(c(1:9)))
  search_values <- names(my_dict)
  hits <- c()
  order <- c()
  # Create digit list
  for (i in c(1:length(search_values))){
    for (j in c(1:nchar(trebuchet_string))){
      string_part <- substr(trebuchet_string,j,j+nchar(search_values[i])-1)
      if (string_part %in% search_values[i]) {

        hits <- c(hits,my_dict[string_part])
        order <- c(order,j)
      }
    }
  }
  # Rearrange the order
  digit_hits <- data.frame(digits = hits, digit_order = order)
  digit_hits <- digit_hits[order(digit_hits$digit_order),]
  calibration_value <- paste0(digit_hits$digits[1],digit_hits$digits[length(digit_hits$digits)])
  return(as.numeric(calibration_value))

}
