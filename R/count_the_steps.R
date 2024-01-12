count_the_steps <- function(instruction_vector, node_map){
  number_of_steps <- 0
  current_node <- node_map[node_map$node == "AAA",]
  # Flag in case of need of repeating the instruction_vector
  flag <- 1
  while(flag){
    for (i in instruction_vector){
      if (i == "L"){
        node_to_select <- current_node$L
      }
      if (i == "R"){
        node_to_select <- current_node$R
      }
      current_node <- node_map[node_map$node == node_to_select,]
      number_of_steps <- number_of_steps + 1

      # Check if current_node is ZZZ
      if (current_node$node == "ZZZ"){
        flag <- 0
        break
      }
    }
  }

  return(number_of_steps)
}
