#' Classic binary search
#'
#' @param a_vector numeric vector
#' @param len_vector numeric, length of A
#' @param target_number numeric, target number
#'
#' @return index of the target number if it is in the vector, otherwise NA
#' @export
binary_search <- function (a_vector, len_vector, target_number){
  L <- 1
  R <- len_vector
  while (L <= R){
    m <- floor((L + R) / 2)
    if (a_vector[m] < target_number){
      L <- m + 1
    } else if (a_vector[m] > target_number){
      R <- m - 1
    } else {
      return (m)
    }
  }
  return (NA)
}
