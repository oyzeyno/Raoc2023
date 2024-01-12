q6_wait_for_it <- function(input_file){
  # input_file <- "inst/extdata/20231206/time_distance.txt"
  # 20231206: Wait for it - time/distance
  # Read input
  time_distance <- t(read.table(file = input_file))
  time_distance <- time_distance[2:dim(time_distance)[1],]
  colnames(time_distance) <- c("Time","Distance")
  time_distance <- as.data.frame(time_distance)
  time_distance$Time <- as.numeric(time_distance$Time)
  time_distance$Distance <- as.numeric(time_distance$Distance)

  # First part - Return number of ways of winning
  number_of_ways <- apply(time_distance, 1, function(x) calculate_number_of_winning_ways(x[1],x[2]))
  multiplication_result <- number_of_ways[1] * number_of_ways[2] * number_of_ways[3] * number_of_ways[4]
  #> 505494
  #>
  # Second part: time and distance as one number
  time_input <- as.numeric(paste0(time_distance$Time, collapse = ""))
  distance_input <- as.numeric(paste0(time_distance$Distance, collapse = ""))

  part_two <- calculate_number_of_winning_ways(time_input,distance_input)
  #>[1] 23632299
  return(c(multiplication_result,part_two))
}
