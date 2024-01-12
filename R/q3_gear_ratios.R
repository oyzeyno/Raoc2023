q3_gear_ratios <- function(input_file){
  #input_file <- "inst/extdata/20231203/engine_parts.txt"
  # 20231203: Gear Ratios
  # Read input as character matrix
  engine_parts <- scan(file = input_file, sep = "", what = character())
  # To extract numbers:matches <- regmatches(trebuchet_string, gregexpr("[[:digit:]]+", trebuchet_string))
  all_part_ids <- lapply(engine_parts,function(x) {
    matches <- regmatches(x, gregexpr("[[:digit:]]+", x))
    all_digits <- as.numeric(unlist(matches))
  })
  all_part_ids_dt <- data.table::as.data.table(transpose(all_part_ids))
  all_part_ids_dt$id <- c(1:length(all_part_ids))
  final_dt <- data.table::setorder(data.table::melt(all_part_ids_dt,
                            id.vars = "id",
                            na.rm = TRUE), "id", variable)[]
  final_dt$char_len <- sapply(final_dt$value, function(x) nchar(x))
  # Replicate rows of final_dt, so that it will be straight-forward to combine
  # with digit row-col ids
  final_dt <- final_dt[rep(seq(nrow(final_dt)), final_dt$char_len),]
  engine_matrix <- do.call(rbind, type.convert(strsplit(engine_parts, ""), as.is = TRUE))
  # First part
  part_ids_all <- extract_engine_part_id(engine_matrix, final_dt)
  part_one <- sum(part_ids_all)
  #> 535078

  # Second part
  part_two <- gear_ratio_sum <- sum(extract_gear_ratio(engine_matrix, final_dt))
  #>[1] 75312571

  return(c(part_one,part_two))
}
