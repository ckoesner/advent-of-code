library(readr)

field <- strsplit(read_lines("day12/input2"), "")
field <- do.call(rbind, field)

field <- strsplit(read_lines("day12/input"), "")
field <- do.call(rbind, field)

start_coord <- which(field == "S", arr.ind = TRUE) 
end_coord <- which(field == "E", arr.ind = TRUE) 

field[start_coord[1], start_coord[2]] <- "a"
field[end_coord[1], end_coord[2]] <- "z"

visited <- matrix(FALSE, ncol = ncol(field), nrow = nrow(field))
distances <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
previous_x <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
previous_y <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
distances[start_coord[1], start_coord[2]] <- 0


get_next <- function(){
  if(all(dim(visited) != dim(distances))){
    print("matrices have different dimensions")
  }
  coord <- c(-1,-1)
  min_dist <- Inf
  for(i in 1:nrow(visited)){
    for(j in 1:ncol(visited)){
      if(!visited[i,j] && distances[i,j] != Inf
         && distances[i,j] < min_dist){
        coord <- c(i,j)
        min_dist <- distances[i,j]
      }
    }
  }
  return(coord)
}

update_node <- function(ev_coord, next_coord){
  # do not go out of bounds
  if(next_coord[1] < 1 || next_coord[2] < 1 ||
     next_coord[1] > nrow(distances) || next_coord[2] > ncol(distances)){
    return(FALSE)
  }
  # can't climb higher than one difference
  if(utf8ToInt(field[ev_coord[1], ev_coord[2]]) < utf8ToInt(field[next_coord[1], next_coord[2]]) - 1){
    return(FALSE)
  }
  # don't update if there is already shorter path
  if(distances[next_coord[1], next_coord[2]] < distances[ev_coord[1], ev_coord[2]] + 1){
    return(FALSE)
  }
  distances[next_coord[1], next_coord[2]] <<- distances[ev_coord[1], ev_coord[2]] + 1
  previous_x[next_coord[1], next_coord[2]] <<- ev_coord[1]
  previous_y[next_coord[1], next_coord[2]] <<- ev_coord[2]
  return(TRUE)
}

djikstra_step <- function(){
  node_to_ev <- get_next();
  update_node(node_to_ev, node_to_ev + c(1,0))
  update_node(node_to_ev, node_to_ev + c(-1,0))
  update_node(node_to_ev, node_to_ev + c(0,1))
  update_node(node_to_ev, node_to_ev + c(0,-1))
  visited[node_to_ev[1], node_to_ev[2]] <<- TRUE
  # print(node_to_ev)
  return(node_to_ev)
}

while(all(djikstra_step() != c(-1,-1))){}

print(distances[end_coord[1], end_coord[2]])



field <- strsplit(read_lines("day12/input"), "")
field <- do.call(rbind, field)

start_coord <- which(field == "E", arr.ind = TRUE) 
end_coord <- which(field == "S", arr.ind = TRUE) 

field[start_coord[1], start_coord[2]] <- "z"
field[end_coord[1], end_coord[2]] <- "a"

visited <- matrix(FALSE, ncol = ncol(field), nrow = nrow(field))
distances <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
previous_x <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
previous_y <- matrix(Inf, ncol = ncol(field), nrow = nrow(field))
distances[start_coord[1], start_coord[2]] <- 0

update_node <- function(ev_coord, next_coord){
  # do not go out of bounds
  if(next_coord[1] < 1 || next_coord[2] < 1 ||
     next_coord[1] > nrow(distances) || next_coord[2] > ncol(distances)){
    return(FALSE)
  }
  # can't climb higher than one difference
  if(utf8ToInt(field[ev_coord[1], ev_coord[2]]) > utf8ToInt(field[next_coord[1], next_coord[2]]) + 1){
    return(FALSE)
  }
  # don't update if there is already shorter path
  if(distances[next_coord[1], next_coord[2]] < distances[ev_coord[1], ev_coord[2]] + 1){
    return(FALSE)
  }
  distances[next_coord[1], next_coord[2]] <<- distances[ev_coord[1], ev_coord[2]] + 1
  previous_x[next_coord[1], next_coord[2]] <<- ev_coord[1]
  previous_y[next_coord[1], next_coord[2]] <<- ev_coord[2]
  return(TRUE)
}


while(all(djikstra_step() != c(-1,-1))){}

min_dist <- Inf
for(i in 1:nrow(field)){
  for(j in 1:ncol(field)){
    if(field[i,j]=="a" && distances[i,j] < min_dist){
      min_dist <- distances[i,j]
    }
  }
}

print(min_dist)


