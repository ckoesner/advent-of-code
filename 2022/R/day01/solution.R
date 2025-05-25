
library(readr)

myData = read_lines("day1/input")
num <- sapply(myData, strtoi)

sums <- c()
single_sum <- 0
for(i in num){
  if(is.na(i)){
    sums <- c(sums, single_sum)
    single_sum <- 0
  }else{
    single_sum <- single_sum + i
  }
}

max(sums)
sum(sort(sums, decreasing = TRUE)[1:3])
