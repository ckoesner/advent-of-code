library(readr)

instructions <- strsplit(read_lines("day14/input"), " -> ")
rock_formations <- list()

for(instruction in instructions){
  rock_formations <- c(rock_formations, list(strsplit(instruction, ",")))
}

rock_formations <- lapply(rock_formations, function(x) t(sapply(x, strtoi)))

borders <- sapply(rock_formations, function(x) c(min(x[,1]), max(x[,1]), min(x[,2]), max(x[,2])))
borders <- c(min(borders[1,]), max(borders[2,]), min(borders[3,]), max(borders[4,]))


draw_rock_formation <- function(rock_formation, offset = 0){
  start_rock <- rock_formation[1,] + c(offset, 0)
  for(i in 2:nrow(rock_formation)){
    end_rock <- rock_formation[i,]  + c(offset, 0)
    field[start_rock[2]:end_rock[2], start_rock[1]:end_rock[1]] <<- TRUE
    start_rock <- end_rock
  }
  return(field)
}

sand_falling <- function(x = 500){
  for(y in 0:(dim(field)[1]-1)){
    # print(paste("y = ", y, ", x = ", x))
    if(field[y+1, x]){
      if(!field[y+1, x-1]){
        x <- x-1
        next
      }else if(!field[y+1, x+1]){
        x <- x+1
        next
      }else{
        field[y,x] <<- TRUE
        break
      }
    }
  }
  return(y)
}



field <- matrix(FALSE, nrow = borders[4]+1, ncol = borders[2]+1)
for(rock_formation in rock_formations){
  draw_rock_formation(rock_formation)
}

number_stones <- sum(field)
while(sand_falling() != borders[4]){}
number_sand <- sum(field)-number_stones
print(paste("fallen sand: ", number_sand))


field <- matrix(FALSE, nrow = borders[2]+10, ncol = 1000)

for(rock_formation in rock_formations){
  draw_rock_formation(rock_formation)
}
field[borders[4]+2, 1:1000] <- TRUE
number_stones <- sum(field)

sand_falling <- function(y, x){
  # print(paste("y = ", y, ", x = ", x))
  if(!field[y+1, x]){
    field[y+1, x] <<- TRUE
    sand_falling(y+1, x)
  }
  if(!field[y+1, x+1]){
    field[y+1, x+1] <<- TRUE
    sand_falling(y+1, x+1)
  }
  if(!field[y+1, x-1]){
    field[y+1, x-1] <<- TRUE
    sand_falling(y+1, x-1)
  }
  return(y)
}

sand_falling(0, 500)
number_sand <- sum(field)-number_stones + 1
print(paste("fallen sand: ", number_sand))

      