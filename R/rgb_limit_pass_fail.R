#' Determine if a pick is possible or not based on the number of "green",
#' "blue", "red" cubes in the "pick" string. The limit for green/red/blue cubes
#' are defined as 13/12/14, respectively.
#'
#' This function is used to solve first part of 20231202 puzzle
#' from https://adventofcode.com/2023/day/2
#'
#' @param pick a string, with red, green, blue cube values in a pick.
#' Ex: "2 green, 3 blue, 1 red"
#'
#' @return TRUE if the pick is not possible. Otherwise it is FALSE.
#' @export
#'
rgb_limit_pass_fail <- function(pick){
  # 12 red cubes, 13 green cubes, and 14 blue cubes
  red_limit <- 12
  green_limit <- 13
  blue_limit <- 14

  flag <- FALSE

  values <- trimws(unlist(strsplit(pick,",")))
  for (color_cube in values){
    if (endsWith(color_cube,"green")){
      g_value <- as.numeric(gsub(" green","", color_cube))
      if (g_value > green_limit){
        flag <- TRUE
      }
    }
    if (endsWith(color_cube,"red")){
      r_value <- as.numeric(gsub(" red","", color_cube))
      if (r_value > red_limit){
        flag <- TRUE
      }
    }
    if (endsWith(color_cube,"blue")){
      b_value <- as.numeric(gsub(" blue","", color_cube))
      if (b_value > blue_limit){
        flag <- TRUE
      }
    }
  }
  return(flag)
}
