library(readr)
library(stringr)

my_data = read_lines("day21/input")

vars <- strsplit(my_data, ": ")

find_term_index_by_name <- function(x){
  for(i in 1:length(vars)){
    if(vars[[i]][1] == x){
      return(i)
    }
  }
}

find_term_by_name <- function(x){
  for(var in vars){
    if(var[[1]] == x){
      return(var[[2]])
    }
  }
}

length(unique(sapply(vars, function(x) x[1])))

find_term_index_by_name("root")
find_term_by_name("root")


recursive_step <- function(x){
  term <- find_term_by_name(x)
  terms <- strsplit(term, " ")[[1]]
  if(length(terms) > 1){
    val1 <- recursive_step(terms[1])
    val2 <- recursive_step(terms[3])
    return(switch(terms[2],
                  "*" = val1 * val2,
                  "/" = val1 / val2,
                  "+" = val1 + val2,
                  "-" = val1 - val2))
  }else{
    return(strtoi(terms[1]))
  }
}
res <- recursive_step("root")

recursive_step <- function(x){
  term <- find_term_by_name(x)
  terms <- strsplit(term, " ")[[1]]
  if(length(terms) > 1){
    val1 <- recursive_step(terms[1])
    val2 <- recursive_step(terms[3])
    if(is.integer(val1) && is.integer(val2)){
      return(switch(terms[2],
                    "*" = val1 * val2,
                    "/" = val1 / val2,
                    "+" = val1 + val2,
                    "-" = val1 - val2))
    }else{
      return(paste("(", val1, terms[2], val2, ")"))
    }
  }else{
    return(strtoi(terms[1]))
  }
}

find_term_by_name("pgtp")
find_term_by_name("root")

repl_ind <- find_term_index_by_name("humn")

vars[[repl_ind]][2] <- "a"

recursive_step <- function(x){
  term <- find_term_by_name(x)
  terms <- strsplit(term, " ")[[1]]
  if(length(terms) > 1){
    res1 <- recursive_step(terms[1])
    val1 <- as.double(res1)
    res2 <- recursive_step(terms[3])
    val2 <- as.double(res2)
    print(paste("val1", val1, "val2", val2))
    
    if(!is.na(val1) && !is.na(val2)){
      return(switch(terms[2],
                    "*" = val1 * val2,
                    "/" = val1 / val2,
                    "+" = val1 + val2,
                    "-" = val1 - val2))
    }else if(is.na(val1) && !is.na(val2)){
      return(paste0("(", res1, terms[2], val2, ")"))
    }else{
      return(switch(terms[2],
                    "+" = paste0("(", val1, terms[2], res2, ")"),
                    "-" = paste0("(", val1, terms[2], res2, ")"),
                    "*" = paste0(val1, terms[2], res2),
                    "/" = paste0(val1, terms[2], res2)
      ))
    }
  }else{
    return(terms[1])
  }
}

res1 <- recursive_step("pgtp")
res2 <- recursive_step("vrvh")

print(paste(res1, "=", res2)) # wolfram alpha
