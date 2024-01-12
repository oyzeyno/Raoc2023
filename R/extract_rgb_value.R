#' Extract the number for "green", "blue", "red" cubes from the "pick" string
#' This function is used to solve second part of 20231202 puzzle
#' from https://adventofcode.com/2023/day/2
#'
#' @param pick a string, with red, green, blue cube values in a pick.
#' Ex: "2 green, 3 blue, 1 red"
#'
#' @return a data frame of green, red, blue cube values
#' @export
#'
extract_rgb_values <- function(pick){
  g_value <- 0
  r_value <- 0
  b_value <- 0
  values <- trimws(unlist(strsplit(pick,",")))
  for (color_cube in values){
    if (endsWith(color_cube,"green")){
      g_value <- as.numeric(gsub(" green","", color_cube))
    }
    if (endsWith(color_cube,"red")){
      r_value <- as.numeric(gsub(" red","", color_cube))
    }
    if (endsWith(color_cube,"blue")){
      b_value <- as.numeric(gsub(" blue","", color_cube))
    }
  }
  return(data.frame(green_value = g_value,
                    red_value = r_value,
                    blue_value = b_value))
}
