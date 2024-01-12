#' Search for a number set in another number set and return the number of hits
#'
#' @param winning_set a numeric vector containing number set to look for
#' @param number_set_to_check a numeric vector containing number set to search from
#'
#' @return number of hits from winning_set in number_set_to_check
#' @export
#'
search_for_numbers_cards <- function(winning_set, number_set_to_check){
  # Order the sets
  winning_set <- sort(winning_set)
  number_set_to_check <- sort(number_set_to_check)

  hits <- c()
  for (i in winning_set){
    hits <- c(hits, binary_search(number_set_to_check, length(number_set_to_check), i))
  }

  return (length(hits[!is.na(hits)]))
}
