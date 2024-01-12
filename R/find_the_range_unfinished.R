find_the_range <- function(source_range_vector, target_map){

  source_start <- as.numeric(source_range_vector[1])
  source_range <- as.numeric(source_range_vector[2])
  # Source end value
  target_map[,4] <- as.numeric(target_map[,2]) + as.numeric(target_map[,3]) - 1
  # Destination end value
  target_map[,5] <- as.numeric(target_map[,1]) + as.numeric(target_map[,3]) - 1

  tt <- apply(target_map, 1, function(x, y) {

    # check the start of the source
    if (y[1] >= x[2] && y[1] <= x[4]){
      index <- as.numeric(y[1]) - as.numeric(x[2])
      destination_value <- as.numeric(x[1]) + index
      # check the range
      #destination_range <-
    } else{
      destination_value <- y
    }

    return(destination_value)
  }, y = source_range_vector)

  hit <- unique(unlist(tt))
  if (length(hit) > 1){
    hit <- setdiff(hit,source_value)
  }

  return(hit)

}
