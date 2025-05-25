library(readr)



compute_priorities <- function(letter) {
  asciiCode <- utf8ToInt(letter)
  acode <- utf8ToInt("a")
  Acode <- utf8ToInt("A")
  
  if(asciiCode >= acode){
    return(asciiCode - acode + 1)
  }else{
    return(asciiCode - Acode + 27)
  }
}

find_doubles <- function(input_text){
  single_letters <- strsplit(input_text, "")[[1]]
  num_items <- length(single_letters)
  first_part <- single_letters[1:(num_items/2)]
  second_part <- single_letters[(num_items/2+1) : num_items]
  
  for(i in first_part){
    for(j in second_part){
      if(i == j){
        return(compute_priorities(i))
      }
    }
  }
}

myData = read_lines("day3/input")
res <- sapply(myData, find_doubles)
sum(res)

find_item_type = function(x, y, z){
  for(i in line1){
    for(j in line2){
      if(i == j){
        for(k in line3){
          if(i == k){
            return(k)
          }
        }
      }
    }
  }
}


counter <- 0
res <- NULL

for(line_number in seq(from = 1, to = 300, by = 3)){
  line1 <- strsplit(myData[line_number], "")[[1]]
  line2 <- strsplit(myData[line_number + 1], "")[[1]]
  line3 <- strsplit(myData[line_number + 2], "")[[1]]
  res <- c(res, find_item_type(line1, line2, line3))
}

sum(sapply(res, compute_priorities))
