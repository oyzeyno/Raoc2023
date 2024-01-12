find_the_link <- function(source_value, target_map){

  source_value <- as.numeric(source_value)
  # Source end  value
  target_map[,4] <- as.numeric(target_map[,2]) + as.numeric(target_map[,3]) - 1

  tt <- apply(target_map, 1, function(x, y) {

    if (y >= x[2] && y <= x[4]){
      index <- as.numeric(y) - as.numeric(x[2])
      destination_value <- as.numeric(x[1]) + index
    } else{
      destination_value <- y
    }

    return(destination_value)
  }, y = source_value)

  hit <- unique(unlist(tt))
  if (length(hit) > 1){
    hit <- setdiff(hit,source_value)
  }

  return(hit)

}
