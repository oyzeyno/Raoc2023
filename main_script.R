# 20231201: Extract calibration codes
# 20231201 - First part, digits only
# Read input
tt <- readLines(con = "inst/extdata/20231201/input.txt")
sum(sapply(tt,function(x) extract_first_last_digit(x)))
# > 56506

# 20231201 - Second part with strings also as digits
sum(sapply(tt,function(x) extract_string_first_last_digit(x)))
# > 56017


# 20231202: Find picks of a set of r/g/b cubes
# Read input
games_df <- readLines(con = "inst/extdata/20231202/games.csv")

# Data wrangling
# Get rid of Game ids
games_df <- sapply(games_df, function(x) trimws(strsplit(x,":")[[1]][2]))
# Separate picks
picks <- sapply(games_df, function(x) strsplit(x,";")[[1]])
# ids
names(picks) <- c(1:100)
# Separate picks and group them based on red green blue
library(data.table)
picks_dt <- as.data.table(transpose(picks))
picks_dt$id <- names(picks)
final_dt <- setorder(melt(picks_dt,
              id.vars = "id",
              na.rm = TRUE), "id", variable)[]
# 20231202 - First part: Find if the game is possible or not
final_dt$not_possible <- sapply(final_dt$value, function(x) rgb_limit_pass_fail(x))
results_dt <- unique(final_dt[,score:=sum(not_possible),by=id][,.(id,score)])
sum(as.numeric(results_dt$id[results_dt$score == 0]))
# > 2541

# 20231202 - Second part: Sum of power of sets
rgb_values <- do.call(rbind,lapply(final_dt$value, function(x) extract_rgb_values(x)))
final_dt_rgb_values <- as.data.table(cbind(final_dt,rgb_values)[,c("id","green_value","blue_value","red_value")])
final_dt_rgb_values <- final_dt_rgb_values[, lapply(.SD, max, na.rm=TRUE), by=id]
final_dt_rgb_values <- final_dt_rgb_values[,power_set:=green_value*blue_value*red_value]
sum(final_dt_rgb_values$power_set)
# > 66016


# 20231203: Gear Ratios
# Read input as character matrix
engine_parts <- scan(file = "inst/extdata/20231203/engine_parts.txt", sep = "", what = character())
# To extract numbers:matches <- regmatches(trebuchet_string, gregexpr("[[:digit:]]+", trebuchet_string))
all_part_ids <- lapply(engine_parts,function(x) {
  matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
  all_digits <- as.numeric(unlist(matches))
})
all_part_ids_dt <- as.data.table(transpose(all_part_ids))
all_part_ids_dt$id <- c(1:length(all_part_ids))
final_dt <- setorder(melt(all_part_ids_dt,
                          id.vars = "id",
                          na.rm = TRUE), "id", variable)[]
final_dt$char_len <- sapply(final_dt$value, function(x) nchar(x))
# Replicate rows of final_dt, so that it will be straight-forward to combine
# with digit row-col ids
final_dt <- final_dt[rep(seq(nrow(final_dt)), final_dt$char_len),]
engine_matrix <- do.call(rbind, type.convert(strsplit(engine_parts, ""), as.is = TRUE))
# First part
part_ids_all <- extract_engine_part_id(engine_matrix, final_dt)
sum(part_ids_all)
#> 535078

# Second part
gear_ratio_sum <- sum(extract_gear_ratio(engine_matrix, final_dt))
#>[1] 75312571

# 20231204: Winning numbers in cards
# Read input as character vector
cards <- scan(file = "inst/extdata/20231204/cards.txt", sep = "|", what = character())
winning_numbers <- cards[grepl("Card",cards)]
card_to_check <- trimws(setdiff(cards, winning_numbers))
winning_numbers <- trimws(unlist(strsplit(winning_numbers, ":"))[!grepl("Card",unlist(strsplit(winning_numbers, ":")))])

winning_numbers_matrix <- do.call(rbind,lapply(winning_numbers, function(x){
  matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
  all_digits <- as.numeric(unlist(matches))
}))

card_to_check_matrix <- do.call(rbind,lapply(card_to_check, function(x){
  matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
  all_digits <- as.numeric(unlist(matches))
}))

# First part: How many points
how_many_points <- c()
for (i in c(1:length(winning_numbers))) {
  how_many_points <- c(how_many_points, search_for_numbers_points(winning_numbers_matrix[i,],
                                                           card_to_check_matrix[i,]))
}

sum(how_many_points[how_many_points != 0.5])
#>15205

# Second part: How many cards
how_many_cards <- c()
for (i in c(1:length(winning_numbers))) {
  how_many_cards <- c(how_many_cards, search_for_numbers_cards(winning_numbers_matrix[i,],
                                                                  card_to_check_matrix[i,]))
}

# Create counter array for the cards
card_counter <- rep(1, length(how_many_cards))
k <- 1
for (i in c(1:length(card_counter))){
  card_info <- how_many_cards[i]
  print(paste("i",i))
  print(paste("card_counter[i]",card_counter[i]))
  if (card_info > 0){
    while (k <= card_counter[i]){
      print(paste("k",k))
      for (j in c((i+1):(i+card_info))){
        card_counter[j] <- card_counter[j]+1
      }
      k <- k+1
    }
  }
  k <- 1

}
sum(card_counter)
# Took very long, but the answer is:
#> 6189740

# 20231205: Seed-fertilizer etc maps
# Read input maps
# Seeds
seed_numbers <- scan(file = "inst/extdata/20231205/seeds.txt", what = character())

# Seed-to-soil map
seed_to_soil <- read.table(file = "inst/extdata/20231205/seed-to-soil-map.txt",
                           skip = 1,
                           sep = " ")
colnames(seed_to_soil) <- c("destination","source","range")

# Soil-to-fertilizer map
soil_to_fertilizer <- read.table(file = "inst/extdata/20231205/soil-to-fertilizer-map.txt",
                                 skip = 1,
                                 sep = " ")
colnames(soil_to_fertilizer) <- c("destination","source","range")

# Fertilizer-to-water map
fertilizer_to_water <- read.table(file = "inst/extdata/20231205/fertilizer-to-water-map.txt",
                                  skip = 1,
                                  sep = " ")
colnames(fertilizer_to_water) <- c("destination","source","range")

# Water-to-light map
water_to_light <- read.table(file = "inst/extdata/20231205/water-to-light-map.txt",
                             skip = 1,
                             sep = " ")
colnames(water_to_light) <- c("destination","source","range")

# Light-to-temperature map
light_to_temperature <- read.table(file = "inst/extdata/20231205/light-to-temperature-map.txt",
                                   skip = 1,
                                   sep = " ")
colnames(light_to_temperature) <- c("destination","source","range")

# Temperature-to-humidity map
temperature_to_humidity <- read.table(file = "inst/extdata/20231205/temperature-to-humidity-map.txt",
                                      skip = 1,
                                      sep = " ")
colnames(temperature_to_humidity) <- c("destination","source","range")

# Humidity-to-location map
humidity_to_location <- read.table(file = "inst/extdata/20231205/humidity-to-location-map.txt",
                                   skip = 1,
                                   sep = " ")
colnames(humidity_to_location) <- c("destination","source","range")

# First part: Find min(location)
seed_list <- seed_numbers[2:length(seed_numbers)]
soil_list <- c()
fertilizer_list <- c()
water_list <- c()
light_list <- c()
temperature_list <- c()
humidity_list <- c()
location_list <- c()
for (i in seed_list){
  # Soil
  soil_number <- find_the_link(i, seed_to_soil)
  soil_list <- c(soil_list, soil_number)

  # fertilizer
  fertilizer_number <- find_the_link(soil_number, soil_to_fertilizer)
  fertilizer_list <- c(fertilizer_list, fertilizer_number)

  # water
  water_number <- find_the_link(fertilizer_number, fertilizer_to_water)
  water_list <- c(water_list, water_number)

  # light
  light_number <- find_the_link(water_number, water_to_light)
  light_list <- c(light_list, light_number)

  # temperature
  temperature_number <- find_the_link(light_number, light_to_temperature)
  temperature_list <- c(temperature_list, temperature_number)

  # humidity
  humidity_number <- find_the_link(temperature_number, temperature_to_humidity)
  humidity_list <- c(humidity_list, humidity_number)

  # location
  location_number <- find_the_link(humidity_number, humidity_to_location)
  location_list <- c(location_list, location_number)

}

min(location_list)
#> 278755257

# Second part: Find min location with seeds as a range
# Reshape seed number array
seeds_as_a_range <- data.frame(seed_start = as.numeric(seed_numbers[seq(2, (length(seed_numbers)-1), by = 2)]),
                               seed_range = as.numeric(seed_numbers[seq(3, length(seed_numbers), by = 2)]))
seeds_as_a_range$seed_end <- seeds_as_a_range$seed_start + seeds_as_a_range$seed_range - 1

location_list <- c()

# come up with a smarter scheme -> calculate sub-ranges

for (i in c(1:length(seeds_as_a_range))){
  seed_long_list <- c(seeds_as_a_range[i,"seed_start"]:seeds_as_a_range[i,"seed_end"])
  for (i in seed_long_list){
    # Soil
    soil_number <- find_the_link(i, seed_to_soil)

    # fertilizer
    fertilizer_number <- find_the_link(soil_number, soil_to_fertilizer)

    # water
    water_number <- find_the_link(fertilizer_number, fertilizer_to_water)

    # light
    light_number <- find_the_link(water_number, water_to_light)

    # temperature
    temperature_number <- find_the_link(light_number, light_to_temperature)

    # humidity
    humidity_number <- find_the_link(temperature_number, temperature_to_humidity)

    # location
    location_number <- find_the_link(humidity_number, humidity_to_location)
    location_list <- c(location_list, location_number)

  }


}


# 20231206: Wait for it - time/distance
# Read input
time_distance <- t(read.table(file = "inst/extdata/20231206/time_distance.txt"))
time_distance <- time_distance[2:dim(time_distance)[1],]
colnames(time_distance) <- c("Time","Distance")
time_distance <- as.data.frame(time_distance)
time_distance$Time <- as.numeric(time_distance$Time)
time_distance$Distance <- as.numeric(time_distance$Distance)

# First part - Return number of ways of winning
number_of_ways <- apply(time_distance, 1, function(x) calculate_number_of_winning_ways(x[1],x[2]))
multiplication_result <- number_of_ways[1] * number_of_ways[2] * number_of_ways[3] * number_of_ways[4]
#> 505494
#>
# Second part: time and distance as one number
time_input <- as.numeric(paste0(time_distance$Time, collapse = ""))
distance_input <- as.numeric(paste0(time_distance$Distance, collapse = ""))

calculate_number_of_winning_ways(time_input,distance_input)
#>[1] 23632299

# 20231207: Camel cards
# Read input
