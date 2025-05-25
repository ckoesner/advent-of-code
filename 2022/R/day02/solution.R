
library(readr)
library(dplyr)

result_points <- function(player1, player2) {
  res <- data.frame(player1_input = c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'), 
                    player2_input = c('X', 'Y', 'Z', 'X', 'Y', 'Z', 'X', 'Y', 'Z'),
                    outcome = c(3, 6, 0, 0, 3, 6, 6, 0, 3))
  
  res[res$player1_input == player1 & res$player2_input == player2, ]$outcome
}

choice_points <- function(player1){
  return(switch (player1,
          'X' = 1,
          'Y' = 2,
          'Z' = 3
  ))
}

myData = read_lines("day2/input")
myDataAsList <- strsplit(myData, " ")

myTibble <- tibble(
  player1 = sapply(myDataAsList, function(x) x[1]), 
  player2 = sapply(myDataAsList, function(x) x[2])
)


result <- myTibble %>% 
  select(player1, player2) %>%
  mutate(result = mapply(result_points, player1, player2),
         choice = sapply(player2, choice_points)) %>%
  mutate(points = result + choice)

sum(result$points)



winning_choice <- function(player1, player2) {
  res <- data.frame(player1_input = c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'), 
                    player2_input = c('X', 'Y', 'Z', 'X', 'Y', 'Z', 'X', 'Y', 'Z'),
                    outcome = c('C', 'A', 'B', 'A', 'B', 'C', 'B', 'C', 'A'))
  
  res[res$player1_input == player1 & res$player2_input == player2, ]$outcome
}


result_points <- function(player1, player2) {
  res <- data.frame(player1_input = c('A', 'A', 'A', 'B', 'B', 'B', 'C', 'C', 'C'), 
                    player2_input = c('A', 'B', 'C', 'A', 'B', 'C', 'A', 'B', 'C'),
                    outcome = c(3, 6, 0, 0, 3, 6, 6, 0, 3))
  
  res[res$player1_input == player1 & res$player2_input == player2, ]$outcome
}

choice_points <- function(player1){
  return(switch (player1,
                 'A' = 1,
                 'B' = 2,
                 'C' = 3
  ))
}
  
result <- myTibble %>% 
  select(player1, player2) %>%
  mutate(winning_choice = mapply(winning_choice, player1, player2)) %>%
  mutate(result = mapply(result_points, player1, winning_choice)) %>%
  mutate(choice = sapply(winning_choice, choice_points)) %>%
  mutate(points = result + choice)

sum(result$points)

  