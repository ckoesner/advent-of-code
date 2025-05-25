library(readr)
library(rjson)

# field <- strsplit(read_lines("day13/input2"), "")
# field <- do.call(rbind, field)

input_lines <- read_lines("day13/input")
# field <- do.call(rbind, field)

line_pointer <- 1

compare_list <- function(x, y, nested_level = 0){
  if(length(x) == 0 || length(y) == 0){
    return(sign(length(y) - length(x)))
  }
  # print(paste("nested_level: ", nested_level))
  
  for(i in 1:min(length(x), length(y))){
    if(typeof(x[[i]]) == "double" && typeof(y[[i]]) == "double"){
      res <- sign(y[[i]] - x[[i]])
      if(res != 0){
        return(res)
      }
    }else{
      if(typeof(y[[i]]) == "double"){
        y[[i]] <- list(y[[i]])
      }
      if(typeof(x[[i]]) == "double"){
        x[[i]] <- list(x[[i]])
      }
      compare_res <- compare_list(x[[i]], y[[i]], nested_level+1)
      if(compare_res != 0){
        return(compare_res)
      }
    }
  }
  return(sign(length(y) - length(x)))
}


line_pointer <- 0
index_sum <- 0
i <- 0


while(line_pointer < length(input_lines)-2){
  line_pointer <- i*3 + 1
  x <- fromJSON(input_lines[line_pointer], simplify=FALSE)
  y <- fromJSON(input_lines[line_pointer + 1], simplify=FALSE)
  if(compare_list(x,y) == 1){
    index_sum <- index_sum + i + 1
  }
  i <- i+1
}
print(index_sum)

packets <- list()
for(i in 1:length(input_lines)){
  if(input_lines[[i]] != ""){
    packets <- c(packets, list(fromJSON(input_lines[[i]], simplify=FALSE)))
  }
}
packets <- c(packets, list(list(list(2))))
packets <- c(packets, list(list(list(6))))

# packets <- c(packets, list(list(2)))
# packets <- c(packets, list(list(6)))

for(i in 1:(length(packets)-1)){
  for(j in 1:(length(packets)-1)){
    if(compare_list(packets[[j]], packets[[j+1]]) <= 0){
      packet <- packets[[j]]
      packets[[j]] <- packets[[j+1]]
      packets[[j+1]] <- packet
    }
  }
}


find_element <- function(x, y){
  if(typeof(x) == "list" && length(x) == 1){
    if(typeof(x[[1]]) == "list" && length(x[[1]]) == 1){
      return(typeof(x[[1]][[1]]) == "double" && x[[1]][[1]] == y)
    }
  }
  return(FALSE)
}

# find_element <- function(x, y){
#   if(typeof(x) == "list" && length(x) == 1){
#     return(typeof(x[[1]]) == "double" && x[[1]] == y)
#   }
#   return(FALSE)
# }

ind_6 <- 0
ind_2 <- 0
for(i in 1:length(packets)){
  if(find_element(packets[[i]], 2)){
    ind_2 <- i
    # print(i)
  }
  if(find_element(packets[[i]], 6)){
    ind_6 <- i
    # print(i)
  }
}
print(ind_6 * ind_2)
