library(stringr)
library(dplyr)
library(purrr)

input <- readLines("5/input.txt")

fresh <- input[str_detect(input, "-")] |> 
  map_df(
    \(fresh_line){
      line_sp <- str_split_1(fresh_line, "-")
      
      tibble(
        from = as.numeric(line_sp[[1]]),
        to = as.numeric(line_sp[[2]])
      )
    }
  ) |> 
  arrange(
    from, to
  )

available <- input[!str_detect(input, "-") & input != ""] |> 
  as.numeric()

is_good <- available |>
  map_lgl(
    \(ing){
      relev <- fresh |> 
        filter(
          from <= ing & to >= ing 
        )
      
      nrow(relev) != 0
    }
  )

message("Part 1: ", sum(is_good))

part2_init <- fresh[1, ]

2:nrow(fresh) |> 
  walk(
    \(ing_row_count){
      
      ing_row <- fresh[ing_row_count, ]
      
      check_from <- which(
        part2_init$from <= ing_row$from & 
          part2_init$to >= ing_row$from
      )
      
      if(length(check_from)){
        part2_init$to[check_from] <<- max(
          c(part2_init$to[check_from], ing_row$to)
        )
        
      }else{
        part2_init <<- part2_init |> 
          add_row(ing_row)
      }
      
    }
  )

part2 <- part2_init |> 
  mutate(
    count = to - from + 1
  ) |>
  pull(count)

message("Part 2: ", sum(part2))