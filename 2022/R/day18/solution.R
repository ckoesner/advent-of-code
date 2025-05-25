library(readr)
cube_data <- read_lines("day18/input")
# cube_data <- read_lines("day18/input2")
cube_coordinates <- sapply(strsplit(cube_data, ","), strtoi)

grid_additional_size <- c(2,2,2)

max_values <- apply(cube_coordinates, 1, max)
grid <- rep(FALSE, prod(max_values + grid_additional_size))
dim(grid) <- max_values + grid_additional_size

check_neighbor <- function(coordinates){
  neighbors <- list(c(1,0,0),
       c(-1,0,0),
       c(0,1,0),
       c(0,-1,0),
       c(0,0,1),
       c(0,0,-1))
  
  neighbor_there <- 0
  # print("neighbor")
  for(neighbor in neighbors){
    x_offset <- coordinates + neighbor
    # print(x_offset)
    if(any(x_offset <= 0) || any(x_offset > dim(grid))){
      next
    }
    if(grid[x_offset[1], x_offset[2], x_offset[3]]){
      neighbor_there <- neighbor_there + 1
    }
  }
  return(neighbor_there)
}

number_of_hidden_sided <- 0
for(i in 1:ncol(cube_coordinates)){
  cube <- cube_coordinates[,i]  + c(1,1,1)
  if(grid[cube[1], cube[2], cube[3]]){
    next
  }
  
  number_of_hidden_sided <- number_of_hidden_sided+2*check_neighbor(cube)
  grid[cube[1], cube[2], cube[3]] <- TRUE
}

number_visible_sides <- ncol(cube_coordinates) * 6 - number_of_hidden_sided
print(number_visible_sides)

number_of_hidden_sided <- 0
for(i in 1:dim(grid)[1]){
  for(j in 1:dim(grid)[2]){
    for(k in 1:dim(grid)[3]){
      if(grid[i,j,k]){
        number_of_hidden_sided <- number_of_hidden_sided + check_neighbor(c(i,j,k))
      }
    }
  }
}
print(sum(grid)*6 - number_of_hidden_sided)


grid2 <- rep(TRUE, prod(max_values + grid_additional_size))
dim(grid2) <- max_values + grid_additional_size

grid2[1,,] <- grid[1,,] 
grid2[,1,] <- grid[,1,] 
grid2[,,1] <- grid[,,1] 


grid2[dim(grid)[1],,] <- grid[dim(grid)[1],,] 
grid2[,dim(grid)[2],] <- grid[,dim(grid)[2],] 
grid2[,,dim(grid)[3]] <- grid[,,dim(grid)[3]] 

for(i in 1:40){
  for(i in 2:dim(grid)[1]){
    for(j in 1:dim(grid)[2]){
      for(k in 1:dim(grid)[3]){
        if(!grid[i,j,k] && !grid2[i-1,j,k]){
          grid2[i,j,k] <- FALSE
        }
        n <- dim(grid)[1]
        if(!grid[n-i+1,j,k] && !grid2[n-i+2,j,k]){
          grid2[n-i+1,j,k] <- FALSE
        }
      }
    }
  }
  for(j in 2:dim(grid)[2]){
    for(i in 1:dim(grid)[1]){
      for(k in 1:dim(grid)[3]){
        if(!grid[i,j,k] && !grid2[i,j-1,k]){
          grid2[i,j,k] <- FALSE
        }
        n <- dim(grid)[2]
        if(!grid[i,n-j+1,k] && !grid2[i,n-j+2,k]){
          grid2[i,n-j+1,k] <- FALSE
        }
      }
    }
  }
  for(k in 2:dim(grid)[3]){
    for(i in 1:dim(grid)[1]){
      for(j in 1:dim(grid)[2]){
        if(!grid[i,j,k] && !grid2[i,j,k-1]){
          grid2[i,j,k] <- FALSE
        }
        n <- dim(grid)[1]
        if(!grid[i,j,n-k+1] && !grid2[i,j,n-k+2]){
          grid2[i,j,n-k+1] <- FALSE
        }
      }
    }
  }
}
sum(grid)
sum(grid2)

check_neighbor <- function(coordinates){
  neighbors <- list(c(1,0,0),
                    c(-1,0,0),
                    c(0,1,0),
                    c(0,-1,0),
                    c(0,0,1),
                    c(0,0,-1))
  
  neighbor_there <- 0
  # print("neighbor")
  for(neighbor in neighbors){
    x_offset <- coordinates + neighbor
    # print(x_offset)
    if(any(x_offset <= 0) || any(x_offset > dim(grid2))){
      next
    }
    if(grid2[x_offset[1], x_offset[2], x_offset[3]]){
      neighbor_there <- neighbor_there + 1
    }
  }
  return(neighbor_there)
}


number_of_hidden_sided <- 0
for(i in 1:dim(grid)[1]){
  for(j in 1:dim(grid)[2]){
    for(k in 1:dim(grid)[3]){
      if(grid2[i,j,k]){
        number_of_hidden_sided <- number_of_hidden_sided + check_neighbor(c(i,j,k))
      }
    }
  }
}
print(sum(grid2)*6 - number_of_hidden_sided)
