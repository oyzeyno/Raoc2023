q1_trebuchet <- function(input_file){
  # input_file <- "inst/extdata/20231201/input.txt"
  # 20231201: Extract calibration codes
  # 20231201 - First part, digits only
  # Read input
  tt <- readLines(con = input_file)
  part_one <- sum(sapply(tt,function(x) extract_first_last_digit(x)))
  # > 56506

  # 20231201 - Second part with strings also as digits
  part_two <- sum(sapply(tt,function(x) extract_string_first_last_digit(x)))
  # > 56017

  return(c(part_one,part_two))

}
