library(readr)


extract_params <- function(line) {
  params <- strsplit(line, " ")[[1]]
  return(strtoi(params[(1:3)*2]))
}

move_crates <- function(crates, num, from, to, reorder){
  crates_to_move <- crates[[from]][1:num]
  if(reorder){
    crates_to_move <- crates_to_move[length(crates_to_move):1]
  }
  crates[[from]] <- na.omit(crates[[from]][(num+1) : length(crates[[from]])])
  crates[[to]] <- c(crates_to_move, crates[[to]])
  return(crates)
}


instructions = read_lines("day5/instructions")
crates <- read.csv("day5/crates.csv", sep = ";")
crates_list <- lapply(crates, c)


for(instruction in instructions){
  instruction_vec <- extract_params(instruction)
  crates_list <- move_crates(crates_list, instruction_vec[1], instruction_vec[2], instruction_vec[3], reorder=TRUE)
}

for(i in 1:length(crates_list)){
  cat(crates_list[[i]][1])
}





instructions = read_lines("day5/instructions")
crates <- read.csv("day5/crates.csv", sep = ";")
crates_list <- lapply(crates, c)


for(instruction in instructions){
  instruction_vec <- extract_params(instruction)
  crates_list <- move_crates(crates_list, instruction_vec[1], instruction_vec[2], instruction_vec[3], reorder=FALSE)
}

for(i in 1:length(crates_list)){
  cat(crates_list[[i]][1])
}
