library(stringr)
library(purrr)
library(dplyr)

input <- readLines("2/input.txt")

all_ids <- input |> 
  str_split_1(",") |> 
  str_split("-") |> 
  map(\(x) x[[1]]:x[[2]]) |> 
  unlist()

# Part 1
id_check <- tibble(
  og = all_ids
) |>
  filter(
    nchar(og) %% 2 == 0
  ) |>
  mutate(
    pt1 = substr(og, 1, nchar(og) / 2),
    pt2 = substr(og, nchar(og) / 2 + 1, nchar(og))
  ) |>
  filter(
    pt1 == pt2
  ) |>
  pull(og) |>
  sum()

message("Part 1: ", sum(id_check))

# Part 2
is_bad_pt2 <- function(id){
  max_length <- as.integer(nchar(id) / 2)
  
  id_sp <- id |>  
    as.character() |> 
    charToRaw()
  
  if(length(unique(id_sp)) == 1){
    return(TRUE)
  }
  
  if(length(unique(id_sp)) > max_length){
    return(FALSE)
  }
  
  all_lengths <- max_length:2
  all_lengths <- all_lengths[nchar(id) %% all_lengths == 0]
  
  for(size in all_lengths){
    combos <- split(
      id_sp, 
      ceiling(seq_along(id_sp) / size)
    )
    
    if(length(unique(combos)) == 1){
      return(TRUE)
    }
  }
  
  FALSE
}

id_check_2 <- all_ids |> 
  map_lgl(is_bad_pt2, .progress = TRUE)

message("Part 2: ", sum(all_ids[id_check_2 & !all_ids %in% 1:9]))