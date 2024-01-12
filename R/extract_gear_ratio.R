extract_gear_ratio <- function(engine_schema,all_part_ids){

  # Determine which characters to search for
  tt<- as.character(engine_matrix)
  search_for_these <- "*"
  digits <- c(0:9)


  # get all "search_for_these" positions
  symbol_positions <- do.call(rbind, lapply(search_for_these, function(x){
    which(engine_matrix == x, arr.ind = T)
  }))

  # get all digit positions
  digit_positions <- do.call(rbind, lapply(digits, function(x){
    which(engine_matrix == x, arr.ind = T)
  }))
  digit_positions <- digit_positions[order(digit_positions[,"row"],digit_positions[,"col"]),]

  # Combine digit positions with the part_ids
  part_id_positions <- cbind(all_part_ids, as.data.frame(digit_positions))
  symbol_part_id_list <- list()

  for (i in c(1:nrow(symbol_positions))){
    part_ids <- c()
    row_index <- symbol_positions[i,"row"]
    col_index <- symbol_positions[i,"col"]
    hit_part_id_prev <- -99

    for (j in c(-1:1)){
      for (k in  c(-1:1)){
        if (any(engine_matrix[row_index-j,col_index-k] %in% digits)){
          # Get part id from part_id_positions
          hit_part_id <- part_id_positions$value[part_id_positions$row == row_index-j &
                                                   part_id_positions$col == col_index-k]
          if (hit_part_id != hit_part_id_prev){
            part_ids <- c(part_ids,hit_part_id)
          }
          hit_part_id_prev <- hit_part_id
        }
      }
    }
    symbol_part_id_list <- c(symbol_part_id_list,list(part_ids))
  }

  gear_ratio <- unlist(lapply(symbol_part_id_list,
                       function(x){
                         if (length(x) == 2) {
                           return(x[1]*x[2])
                         } else {
                           return(0)
                         }
                       }
    ))


  return(gear_ratio)
}
