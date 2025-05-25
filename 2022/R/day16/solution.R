library(readr)
valve_data <- read_lines("day16/input")

extract_flow <- function(input_string){
  pattern <-"([0-9])+"
  m <- gregexec(pattern, input_string)
  return(strtoi(regmatches(input_string, m)[[1]][1,]))
}
extract_connected_valves <- function(input_string){
  pattern <-"to (.)*"
  m <- gregexec(pattern, input_string)
  matches <- regmatches(input_string, m)[[1]][1,]
  matches_split <- strsplit(matches, " ")[[1]]
  matches_split <- sapply(matches_split, function(x) substr(x, 1,2))
  return(matches_split[3:length(matches_split)])
}

valve_names <- sapply(valve_data, function(x) substr(x, 7, 8))
flows <- sapply(valve_data, extract_flow)
connected_valves <- lapply(valve_data, extract_connected_valves)

n <- length(valve_names)

dist_mat <- matrix(0, nrow = n, ncol = n)
colnames(dist_mat) <- valve_names
rownames(dist_mat) <- valve_names
names(valve_names) <- NULL
names(flows) <- valve_names

for(i in 1:n){
  for(valve_name in connected_valves[[i]]){
    # print(valve_name)
    j <- which(valve_names == valve_name, arr.ind=TRUE)
    # print(paste("i= ",i,"j= ", j))
    dist_mat[i, j] <- 1
    dist_mat[j, i] <- 1
  }
}
rm(i, valve_name)
start_index <- which(valve_names =="AA", arr.ind = TRUE)

valves__without_flow_ind <- which(flows == 0, arr.ind = TRUE)
valves__without_flow_ind <- valves__without_flow_ind[names(valves__without_flow_ind) != "AA"]

for(i in valves__without_flow_ind){
  # print(which(dist_mat[,i] != 0, arr.ind = TRUE))
  connected_valves_ind <- which(dist_mat[,i] != 0, arr.ind = TRUE)
  for(j in connected_valves_ind){
    for(k in connected_valves_ind){
      if(j != k){
        new_dist <- dist_mat[j,i] + dist_mat[k,i]
        if(dist_mat[j,k] == 0 || new_dist < dist_mat[j,k]){
          dist_mat[j,k] <- new_dist
          dist_mat[k,j] <- new_dist
        }
      }
    }
  }
}
rm(i,j,k)

flows2 <- flows[-valves__without_flow_ind]
dist_mat_for_flow <- dist_mat[-valves__without_flow_ind, -valves__without_flow_ind]
