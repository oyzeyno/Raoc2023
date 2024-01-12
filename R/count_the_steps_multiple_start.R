count_the_steps_multiple_start <- function(instruction_vector, node_map){
  number_of_steps <- 0
  current_nodes <- node_map[substr(node_map$node,3,3) == "A",]
  current_nodes$number_steps <- apply(current_nodes, 1, function(x,y,z){
    number_of_steps <- 0
    current_node <- x
    # Flag in case of need of repeating the instruction_vector
    flag <- 1
    while(flag){
      for (i in y){
        if (i == "L"){
          node_to_select <- current_node[2]
        }
        if (i == "R"){
          node_to_select <- current_node[3]
        }
        current_node <- z[z$node %in% node_to_select,]

        number_of_steps <- number_of_steps + 1

        # Check if current_node ends with Z
        if (substr(current_node$node,3,3) == "Z"){
          flag <- 0
          break
        }
      }
    }

    return(number_of_steps)
  }, y = instruction_vector, z = node_map)
  # find lcm
  lcm_all <- numbers::LCM(current_nodes$number_steps[1],current_nodes$number_steps[2])
  for (i in c(1:length(current_nodes$number_steps))){
    lcm_all <- numbers::LCM(lcm_all, current_nodes$number_steps[i])
  }
  return(lcm_all)

}
