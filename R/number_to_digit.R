#' Conversion of a number to a list of its digits
#'
#' @param a_number a string or a number
#'
#' @return a list of a_number digits
#' @export
#'
number_to_digit <- function(a_number){
  a_number <- as.numeric(a_number)
  list_of_digits <- c()
  # do till num greater than  0
  while(a_number > 0) {
    # split last digit from number
    mod_of_number = a_number %% 10
    list_of_digits <- c(list_of_digits,mod_of_number)
    # integer division by 10
    a_number = a_number %/% 10
  }
  return(rev(list_of_digits))
}
