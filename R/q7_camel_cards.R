q7_camel_cards <- function(input_file){
  hand_bid <- utils::read.table(input_file)

  # First part
  hand_bid$hand_label <- sapply(hand_bid$V1, function(x) label_hand(x))
  card_strength <- c("A","K","Q","J","T","9","8","7","6","5","4","3","2")
  hand_strength <- c("five of a kind", "four of a kind", "full house", "three of a kind",
                     "two pair","one pair","high card")
  hand_bid$hand_label <- factor(hand_bid$hand_label, levels = hand_strength)
  hand_bid <- cbind(hand_bid, do.call(rbind,strsplit(hand_bid$V1, "")))
  hand_bid$`1` <- factor(hand_bid$`1`, levels = card_strength)
  hand_bid$`2` <- factor(hand_bid$`2`, levels = card_strength)
  hand_bid$`3` <- factor(hand_bid$`3`, levels = card_strength)
  hand_bid$`4` <- factor(hand_bid$`4`, levels = card_strength)
  hand_bid$`5` <- factor(hand_bid$`5`, levels = card_strength)
  ordered <- hand_bid[order(hand_bid$hand_label,
                            hand_bid$`1`,
                            hand_bid$`2`,
                            hand_bid$`3`,
                            hand_bid$`4`,
                            hand_bid$`5`, decreasing = T),]

  total_winnings_first <- 0
  for (i in c(1:length(hand_bid$V2))){
    total_winnings_first <- total_winnings_first + i*ordered$V2[i]
  }
  #246424613

  # Second part
  hand_bid <- utils::read.table(input_file)
  hand_bid$hand_label <- sapply(hand_bid$V1, function(x) label_hand(x, joker = T))
  card_strength <- c("A","K","Q","T","9","8","7","6","5","4","3","2","J")
  hand_strength <- c("five of a kind", "four of a kind", "full house", "three of a kind",
                     "two pair","one pair","high card")
  hand_bid$hand_label <- factor(hand_bid$hand_label, levels = hand_strength)
  hand_bid <- cbind(hand_bid, do.call(rbind,strsplit(hand_bid$V1, "")))
  hand_bid$`1` <- factor(hand_bid$`1`, levels = card_strength)
  hand_bid$`2` <- factor(hand_bid$`2`, levels = card_strength)
  hand_bid$`3` <- factor(hand_bid$`3`, levels = card_strength)
  hand_bid$`4` <- factor(hand_bid$`4`, levels = card_strength)
  hand_bid$`5` <- factor(hand_bid$`5`, levels = card_strength)
  ordered <- hand_bid[order(hand_bid$hand_label,
                            hand_bid$`1`,
                            hand_bid$`2`,
                            hand_bid$`3`,
                            hand_bid$`4`,
                            hand_bid$`5`, decreasing = T),]

  total_winnings_second <- 0
  for (i in c(1:length(hand_bid$V2))){
    total_winnings_second <- total_winnings_second + i*ordered$V2[i]
  }
  # 248256639
  return(c(total_winnings_first, total_winnings_second))
}
