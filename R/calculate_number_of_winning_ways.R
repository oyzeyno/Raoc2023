calculate_number_of_winning_ways <- function(time, distance){

  # Total time: time
  # Record distance : distance
  hold_time <- c(0:time)
  speed <- hold_time
  travel_distance <- (time - hold_time) * speed
  number_of_ways <- length(travel_distance[travel_distance > distance])
  return(number_of_ways)
}
