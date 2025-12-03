library(purrr)
library(stringr)

input <- readLines(
  "3/input.txt"
)

find_next <- function(bank, min_index, max_index){
  
  if(min_index != 1){
    bank[1:(min_index - 1)] <- 0
  }

  first <- which.max(bank)
  
  while(first > max_index){
    bank[first:length(bank)] <- 0
    first <- which.max(bank)
  }
  
  first
}

find_all <- function(bank, size = 2){
  
  next_num <- find_next(
    bank, 
    min_index = 1,
    max_index = length(bank) - size + 1
  )
  
  out <- c()
  out[[1]] <- bank[next_num]
  
  while(length(out) < size){
    next_num <- find_next(
      bank,
      min_index = next_num + 1,
      max_index = length(bank) - (size - length(out) - 1)
    )
    
    out <- append(out, bank[next_num])
  }
  
  paste(out, collapse = "")
}

input_sp <- input |> 
  str_split("") |> 
  map(as.double)

pt1 <- input_sp |>
  map_chr(find_all) |>
  as.double()

message("Part 1: ", sum(pt1))

pt2 <- input_sp |>
  map_chr(find_all, size = 12) |>
  as.double()

message("Part 2: ", sum(pt2))
