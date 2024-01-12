label_hand <- function(hand_cards, joker = FALSE){
#   Five of a kind, where all five cards have the same label: AAAAA
#   Four of a kind, where four cards have the same label and one card has a different label: AA8AA
#   Full house, where three cards have the same label, and the remaining two cards share a different label: 23332
#   Three of a kind, where three cards have the same label, and the remaining two cards are each different from any other card in the hand: TTT98
#   Two pair, where two cards share one label, two other cards share a second label, and the remaining card has a third label: 23432
#   One pair, where two cards share one label, and the other three cards have a different label from the pair and each other: A23A4
#   High card, where all cards' labels are distinct: 23456

  the_hand <- unlist(strsplit(hand_cards, ""))

  distinct_cards <- table(the_hand)
  if (joker && hand_cards != "JJJJJ"){
    if (any(the_hand %in% "J")){
      distinct_cards <- table(the_hand[the_hand != "J"])
      the_hand[the_hand %in% "J"] <- names(which.max(distinct_cards))
    }
    distinct_cards <- table(the_hand)
  }
  if (length(the_hand) == length(distinct_cards)){
    the_hand_label <- "high card"
  } else if (length(distinct_cards) == 4){
    the_hand_label <- "one pair"
  } else if (length(distinct_cards) == 3){
    if (max(distinct_cards) == 3){
      the_hand_label <- "three of a kind"
    } else {
      the_hand_label <- "two pair"
    }
  } else if (length(distinct_cards) == 2){
    if (max(distinct_cards) == 4){
      the_hand_label <- "four of a kind"
    } else {
      the_hand_label <- "full house"
    }
  } else {
    the_hand_label <- "five of a kind"
  }
  return(the_hand_label)
}
