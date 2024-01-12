q4_scratchcards <- function(input_file){
  #input_file <- "inst/extdata/20231204/cards.txt"
  # 20231204: Winning numbers in cards
  # Read input as character vector
  cards <- scan(file = input_file, sep = "|", what = character())
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

  part_one <- sum(how_many_points[how_many_points != 0.5])
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
  part_two <- sum(card_counter)
  # Took very long, but the answer is:
  #> 6189740
  return(c(part_one,part_two))
}
