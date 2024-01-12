q8_haunted_wasteland <- function(input_file){
  raw_input <- readLines(con = input_file)
  instructions <-  unlist(strsplit(raw_input[1], ""))

  nodes_LR <- raw_input[3:length(raw_input)]
  nodes_LR <- nodes_LR[order(nodes_LR)]

  # Convert it to data frame
  nodes_df <- as.data.frame(do.call(rbind,strsplit(nodes_LR,"=")))
  nodes_df$V1 <- trimws(nodes_df$V1)
  LR <- as.data.frame(do.call(rbind, strsplit(nodes_df$V2, ",")))
  LR$V1 <- trimws(LR$V1)
  LR$V1 <- gsub("\\(","",LR$V1)
  LR$V2 <- trimws(LR$V2)
  LR$V2 <- gsub("\\)","",LR$V2)
  colnames(nodes_df) <- c("node","dummy")
  colnames(LR) <- c("L","R")
  nodes_df <- cbind(nodes_df,LR)
  nodes_df$dummy <- NULL

  # First part: number of steps to reach from AAA to ZZZ
  first_part <- count_the_steps(instructions, nodes_df)
  # 22199

  # Second part: number of steps to reach from multiple XXA to multiple XXZ
  second_part <- count_the_steps_multiple_start(instructions, nodes_df)
  # 13334102464297
  return(c(first_part,second_part))
}
