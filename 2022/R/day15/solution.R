
library(readr)

manhattan_distance <- function(x, y){
  return(abs(y[1] - x[1]) + abs( y[2] - x[2]))
}

extract_coordinates <- function(input_string){
  pattern <-"(-?[0-9])+"
  m <- gregexec(pattern, input_string)
  coordinates <- strtoi(regmatches(input_string, m)[[1]][1,])
  return(list(sensor = c(coordinates[1], coordinates[2]), 
              beacon = c(coordinates[3], coordinates[4])))
}

compute_sensor_reach <- function(y, sensor, manhattan){
  manhattan_difference <- manhattan - abs(sensor[2] - y)
  if(manhattan_difference < 0){
    return(c())
  }else{
    return((sensor[1] - manhattan_difference) : (sensor[1] + manhattan_difference))
  }
}

field <- read_lines("day15/input")
y <- 2000000

# field <- read_lines("day15/input2")
# y <- 10

formatted_input <- lapply(field, extract_coordinates)
for(i in 1:length(formatted_input)){
  formatted_input[[i]]$manhattan <- manhattan_distance(formatted_input[[i]]$sensor, 
                                                       formatted_input[[i]]$beacon)
}
rm(i)


difference_list <- lapply(formatted_input, function(x) compute_sensor_reach(y, x$sensor, x$manhattan))
all_sender_candidates <- c()

for(difference in difference_list){
  all_sender_candidates <- c(all_sender_candidates, difference)
}
rm(difference)
all_sender_candidates <- unique(all_sender_candidates)


x_indeces_to_remove <- which(y == sapply(formatted_input, function(x) x$beacon[2]))
x_values_to_remove <- sapply(formatted_input[x_indeces_to_remove], function(x) x$beacon[1])

# print(length(all_sender_candidates) - sum(unique(x_values_to_remove) %in% all_sender_candidates))


compute_sensor_reach <- function(y, sensor, manhattan){
  manhattan_difference <- manhattan - abs(sensor[2] - y)
  if(manhattan_difference < 0){
    return(c())
  }else{
    return(c(sensor[1] - manhattan_difference, sensor[1] + manhattan_difference))
  }
}

find_position <- function(y, left_border = 0, right_border = 4000000){
  difference_list <- list()
  for(input in formatted_input){
    sensor_reach <- compute_sensor_reach(y, input$sensor, input$manhattan)
    if(!is.null(sensor_reach)){
      difference_list <- c(difference_list, list(sensor_reach))
    }
  }
  if(is.null(difference_list)){
    return(Inf)
  }
  difference_list <- difference_list[sort(sapply(difference_list, function(x) x[1]), index.return = TRUE)$ix]
  if(difference_list[[1]][1] > left_border){
    return(0)
  }
  right <- difference_list[[1]][2]
  for(i in 2:length(difference_list)){
    if(difference_list[[i]][1] > right + 1){
      return(difference_list[[i]][1] - 1)
    }
    if(difference_list[[i]][2] > right){
      right <- difference_list[[i]][2]
    }
    # print(right)
  }
  if(right < right_border){
    return(right+1)
  }
  return(-1)
}


begin_time <- Sys.time()
for(i in 0:4000000){
  pos <- find_position(i)
  if(pos != -1){
    x <- pos
    y <- i
    print(x)
    print(y)
  }
}
end_time <- Sys.time()
print(end_time - begin_time)

