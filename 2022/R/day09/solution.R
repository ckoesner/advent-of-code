library(readr)

single_head_move <- function(head_coordinates, dir){
  return(head_coordinates + switch(dir,
                                   L = c(0,-1), 
                                   D = c(-1,0), 
                                   R = c(0,1),
                                   U = c(1,0)))
}

single_tail_move <- function(head_coordinates, tail_coordinates){
  if(max(abs(head_coordinates - tail_coordinates)) <= 1){
    return(tail_coordinates)
  }else{
    return(tail_coordinates + sign(head_coordinates - tail_coordinates))
  }
}
moves <- list(
  c("R", 4),
  c("U", 4),
  c("L", 3),
  c("D", 1),
  c("R", 4),
  c("D", 1),
  c("L", 5),
  c("R", 2)
)

moves <- strsplit(read_lines("day9/input"), " ")


coordinate_list <- list()
head_coordinates <- c(0,0)
tail_coordinates <- c(0,0)
for(single_move in moves){
  for(i in 1:strtoi(single_move[2])){
    head_coordinates <- single_head_move(head_coordinates, single_move[1])
    tail_coordinates <- single_tail_move(head_coordinates, tail_coordinates)
    coordinate_list <- append(coordinate_list, list(tail_coordinates))
  }
}
print(length(unique(coordinate_list)))



coordinate_list <- list()
head_coordinates <- c(0,0)
tail_coordinates <- c(0,0)

num_knots <- 10
knot_list <- rep(list(c(0,0)), num_knots)

for(single_move in moves){
  for(i in 1:strtoi(single_move[2])){
    knot_list[[1]] <- single_head_move(knot_list[[1]], single_move[1])
    for(j in 2:num_knots){
      knot_list[[j]] <- single_tail_move(knot_list[[j-1]], knot_list[[j]])
    }
    coordinate_list <- append(coordinate_list, list(knot_list[[num_knots]]))
  }
}

print(length(unique(coordinate_list)))
