library(readr)

instructions <- strsplit(read_lines("day10/input"), " ")

num_cycles <- sum(sapply(instructions, function(x) ifelse(x[1] == "addx", 2, 1)))
value_cycle <- rep(0, num_cycles)

current_cycle <- 1
current_value <- 1

for(instruction in instructions){
  if(instruction[1] == "noop"){
    value_cycle[current_cycle] <- current_value
    current_cycle <- current_cycle + 1
  }
  if(instruction[1] == "addx"){
    value_cycle[current_cycle] <- current_value
    value_cycle[current_cycle+1] <- current_value
    current_cycle <- current_cycle + 2
    current_value <- current_value + strtoi(instruction[2])
  }
}

cycles <- c(20, 60, 100, 140, 180, 220)
print(sum(value_cycle[cycles] * cycles))


sprite_overlaps <- function(cycle, sprite_pos){
  crt_pos <- ((cycle - 1) %% 40) + 1
  return(crt_pos >= sprite_pos && crt_pos <= sprite_pos + 2)
}

print(
  matrix(data = sapply(1:240, function(i) ifelse(sprite_overlaps(i, value_cycle[i]), "x", "")), 
         nrow = 6, byrow = TRUE))