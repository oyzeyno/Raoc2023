q2_cube_conundrum <- function(input_file){
  #input_file <- "inst/extdata/20231202/games.csv"
  # 20231202: Find picks of a set of r/g/b cubes
  # Read input
  games_df <- readLines(con = input_file)

  # Data wrangling
  # Get rid of Game ids
  games_df <- sapply(games_df, function(x) trimws(strsplit(x,":")[[1]][2]))
  # Separate picks
  picks <- sapply(games_df, function(x) strsplit(x,";")[[1]])
  # ids
  names(picks) <- c(1:100)
  # Separate picks and group them based on red green blue
  picks_dt <- data.table::as.data.table(transpose(picks))
  picks_dt$id <- names(picks)
  final_dt <- setorder(data.table::melt(picks_dt,
                            id.vars = "id",
                            na.rm = TRUE), "id", variable)[]
  # 20231202 - First part: Find if the game is possible or not
  final_dt$not_possible <- sapply(final_dt$value, function(x) rgb_limit_pass_fail(x))
  results_dt <- unique(final_dt[,score:=sum(not_possible),by=id][,.(id,score)])
  part_one <- sum(as.numeric(results_dt$id[results_dt$score == 0]))
  # > 2541

  # 20231202 - Second part: Sum of power of sets
  rgb_values <- do.call(rbind,lapply(final_dt$value, function(x) extract_rgb_values(x)))
  final_dt_rgb_values <- data.table::as.data.table(cbind(final_dt,rgb_values)[,c("id","green_value","blue_value","red_value")])
  final_dt_rgb_values <- final_dt_rgb_values[, lapply(.SD, max, na.rm=TRUE), by=id]
  final_dt_rgb_values <- final_dt_rgb_values[,power_set:=green_value*blue_value*red_value]
  part_two<- sum(final_dt_rgb_values$power_set)
  # > 66016

  return(c(part_one,part_two))
}
