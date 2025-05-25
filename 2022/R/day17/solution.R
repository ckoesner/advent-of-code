

sprites <- list(
  matrix(c(TRUE, TRUE, TRUE, TRUE), nrow = 1, ncol = 4),
  matrix(c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE), nrow = 3, byrow = TRUE),
  matrix(c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE), nrow = 3, byrow = TRUE),
  matrix(c(TRUE, TRUE, TRUE, TRUE), nrow = 4),
  matrix(c(TRUE, TRUE, TRUE, TRUE), nrow = 2)
)

m <- 7
n <- 10000

field <- matrix(0, nrow = n, ncol = m)

highest_point <- nrow(field)

rock_appearing <- function(highest_point, shape){
  upper_left_corner <- c(3, highest_point - ncol(shape))
}

rock_push <- function(upper_left_corner, dir){
  if(dir == "<" && upper_left_corner[2] - 1 <= 0){
    return(FALSE)
  }
  if(dir == ">" && upper_left_corner[2] + ncol(shape) + 1 >= 7){
    return(FALSE)
  }
  possible_overlap <- 
    field[upper_left_corner[1] + 1:ncol(shape) - 1,
          upper_left_corner[2] + 1:nrow(shape) - 1]
  
  dim(shape) <- c(1, prod(dim(shape)))
  dim(possible_overlap) <- c(1, prod(dim(possible_overlap)))
  
  if(any(shape && possible_overlap)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

rock_falling <- function(upper_left_corner, shape){
  upper_left_corner <- upper_left_corner - c(1,0)
  possible_overlap <- 
    field[upper_left_corner[1] + 1:ncol(shape) - 1,
          upper_left_corner[2] + 1:nrow(shape) - 1]
  
  
  dim(shape) <- c(1, prod(dim(shape)))
  dim(possible_overlap) <- c(1, prod(dim(possible_overlap)))
  
  if(any(shape && possible_overlap)){
    return(FALSE)
  }else{
    return(TRUE)
  }
}

