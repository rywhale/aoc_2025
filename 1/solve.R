library(dplyr)
library(stringr)
library(tidyr)
library(readr)

calc_end <- function(start, dir, amount){
  
  loop_count <- 0
  
  if(amount > 100){
    loop_count <- loop_count + as.integer(amount / 100)
    amount <- amount  %% 100
  }
  
  amount <- ifelse(
    dir == "R",
    amount,
    -amount
  )
  
  tot <- start + amount
  
  did_loop <- FALSE
  
  if(tot > 100){
    out <- tot - 100
    did_loop <- TRUE
  }else if(tot < 1){
    out <- tot + 100
    did_loop <- TRUE
  }else{
    out <- tot
  }
  
  if(out == 100){
    out <- 0
  }
  
  if(did_loop & out != 0 & start != 0){
    loop_count <- loop_count + 1
  }
  
  list(
    loop_count = loop_count,
    end_pos = out
  )
}

inst <- read_lines(
  "1/input"
)

pos <- 50
count <- 0
part2_count <- 0

for(ins in inst){
  
  dir <- ins |>
    str_extract("[A-Z]")
  
  amount <- ins |>
    str_extract("\\d+")
  
  move_calc <- calc_end(
    start = pos,
    dir = dir,
    amount = as.numeric(amount)
  )
  
  pos <- move_calc$end_pos
  
  if(pos == 0){
    count <- count + 1
  }
  
  part2_count <- part2_count + move_calc$loop_count
}

message("Part 1: ", count)
message("Part 2: ", part2_count + count)