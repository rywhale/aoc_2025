library(stringr)
library(purrr)
library(dplyr)

sub_mat <- function(input_mat, row, col){
  xmin <- ifelse(
    row == 1,
    1,
    row -1
  )
  
  xmax <- ifelse(
    row == nrow(input_mat),
    nrow(input_mat),
    row + 1
  )
  
  ymin <- ifelse(
    col == 1,
    1,
    col -1
  )
  
  ymax <- ifelse(
    col == ncol(input_mat),
    ncol(input_mat),
    col + 1
  )
  
  input_mat[xmin:xmax, ymin:ymax]
}

check_roll <- function(input_matrix, row, col){
  input_matrix[row, col] <- "X"
  
  relev_mat <- input_matrix |> 
    sub_mat(row, col)

  sum(relev_mat == "@") < 4
}

update_matrix <- function(input_matrix, roll_positions){
  roll_positions |>
    nrow() |>
    seq_len() |> 
    walk(
      \(pos){
        input_matrix[roll_positions$row[[pos]], roll_positions$col[[pos]]] <<- "X"
      }
    )
  
  input_matrix
}

calc_rolls <- function(input_matrix, keep_goin = FALSE){
  rolls_checked <- which(
    input_matrix == "@",
    arr.ind = TRUE
  ) |>
    as.data.frame() |> 
    rowwise() |>
    mutate(
      is_good = check_roll(input_matrix, row, col)
    ) |> 
    filter(
      is_good
    )
  
  good_rolls <- nrow(rolls_checked)

  if(!keep_goin){
    return(good_rolls)
  }
  
  input_matrix <- input_matrix |> 
    update_matrix(rolls_checked)
  
  while(any(input_matrix == "@")){

    rolls_checked <- which(
      input_matrix == "@",
      arr.ind = TRUE
    ) |>
      as.data.frame() |> 
      rowwise() |>
      mutate(
        is_good = check_roll(input_matrix, row, col)
      ) |> 
      filter(
        is_good
      )
    
    if(nrow(rolls_checked) == 0){
      return(good_rolls)
    }else{
      good_rolls <- good_rolls + nrow(rolls_checked)
      
      input_matrix <- input_matrix |> 
        update_matrix(rolls_checked)
    }
  }
  
  good_rolls
}

input <- readLines("4/input.txt") |>
  str_split("", simplify = TRUE)

pt1 <- calc_rolls(input)
message("Part 1: ", sum(pt1))

pt2 <- calc_rolls(input, keep_goin = TRUE)
message("Part 2: ", sum(pt2))