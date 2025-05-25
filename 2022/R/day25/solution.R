library(readr)

snafu <- read_lines("day25/input", )

snafu_chars <- strsplit(snafu, "")

calc_value <- function(x){
  n <- length(x)
  j <- 0;
  sum <- 0
  for(i in seq(from = n, to = 1, by = -1)){
    val <- switch(x[i], "0" = 0, "1" = 1, "2" = 2, "-" = -1, "=" = -2)
    sum <- sum + val * 5^j
    j <- j+1
  }
  return(sum)
}

calc_value(snafu_chars[[1]])

sum(sapply(snafu_chars, calc_value))
