library(readr)

compute_visible_trees <- function(x){
  max_value <- -1
  visible_elements <- rep(0, length(x))
  for(i in 1:length(x)){
    if(x[i] > max_value){
      visible_elements[i] <- 1
      max_value <- x[i]
    }
  }
  return(visible_elements)
}

compute_visible_trees_matrix_from_direction <- function(x, direction){
  n <- dim(x)[1]
  output_matrix <- matrix(0, nrow = dim(x)[1], ncol = dim(x)[1])

  x <- switch(direction,
    x,
    t(x),
    x[,n:1],
    t(x[n:1,]))
  
  for(i in 1:dim(x)[1]){
    output_matrix[i,] <- compute_visible_trees(x[i,])
  }
  
  output_matrix <- switch(direction,
    output_matrix,
    t(output_matrix),
    output_matrix[,n:1],
    t(output_matrix)[n:1,])
  return(output_matrix)
}

example_matrix <- matrix(sample(0:5, 25, replace = TRUE), 5, 5)
print(example_matrix)
example_result <- compute_visible_trees_matrix_from_direction(example_matrix, 4)
print(example_result)

signal <- read_lines("day8/input")

length(signal[1])
input_matrix <- t(sapply(signal, function(x) strtoi(strsplit(x, split="")[[1]])))
output_matrix <- matrix(0, nrow = dim(input_matrix)[1], ncol = dim(input_matrix)[1])

for(i in 1:4){
  output_matrix <- output_matrix + compute_visible_trees_matrix_from_direction(input_matrix, i)
}


sum(output_matrix > 0)



# first element is tree
tree_score_to_border <- function(tree_to_border_vector){
  if(length(tree_to_border_vector) == 1){
    return(0)
  }
  blocking_num <- tree_to_border_vector[1]
  current_max <- -1
  score <- 0
  for(i in 2:length(tree_to_border_vector)){
    if(tree_to_border_vector[i] > current_max){
      score <- score + 1
    }
    if(tree_to_border_vector[i] >= blocking_num){
      return(score)
    }
  }
  return(score)
}


tree_score <- function(input_matrix, i, j){
  n <- dim(input_matrix)[1]
  left <- input_matrix[i, j:1]
  right <- input_matrix[i, j:n]
  up <- input_matrix[i:1, j]
  down <- input_matrix[i:n, j]
  
  left_score <- tree_score_to_border(left)
  right_score <- tree_score_to_border(right)
  up_score <- tree_score_to_border(up)
  down_score <- tree_score_to_border(down)
  
  return(left_score * right_score * up_score * down_score)
}

example <- matrix(c(3, 0, 3, 7, 3, 
                    2, 5, 5, 1, 2,
                    6, 5, 3, 3, 2,
                    3, 3, 5, 4, 9,
                    3, 5, 3, 9, 0), nrow = 5, byrow = TRUE)

tree_score(example, 4, 3)
tree_score(example, 2, 3)


output_matrix <- matrix(0, nrow = dim(input_matrix)[1], ncol = dim(input_matrix)[1])
for(i in 1:dim(input_matrix)[1]){
  for(j in 1:dim(input_matrix)[2]){
    output_matrix[i,j] = tree_score(input_matrix, i, j)
  }
}
print(max(output_matrix))
