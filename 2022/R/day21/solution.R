library(readr)
library(stringr)

my_data = read_lines("day21/input")

vars <- strsplit(my_data, ": ")

terms <- lapply(vars, function(x) x[2])

single_terms <- vars[!is.na(strtoi(terms))]
compound_terms <- vars[is.na(strtoi(terms))]

to_replace_list <- list()

for(y in single_terms){
  to_replace <- sapply(compound_terms, 
                       function(x) substr(x[2], 1,4)==y[1] || substr(x[2], 8, 11)==y[1])
  to_replace_list <- c(to_replace_list, list(to_replace))
}

to_replace_list_3 <- list()
for(y in single_terms){
  to_replace <- sapply(compound_terms, 
                       function(x) str_count(x[2],y[1]) >= 1)
  to_replace_list_3 <- c(to_replace_list_3, list(to_replace))
}

to_replace_list_2 <- list()
for(y in compound_terms){
  to_replace <- sapply(compound_terms, 
                       function(x) substr(x[2], 1,4)==y[1] || substr(x[2], 8, 11)==y[1])
  to_replace_list_2 <- c(to_replace_list_2, list(to_replace))
}

compound_terms_replaced <- compound_terms


for(i in 1:length(single_terms)){
  single_term <- single_terms[[i]]
  compound_terms_replaced[to_replace_list[[i]]] <- lapply(compound_terms_replaced[to_replace_list[[i]]],
                                                          function(x) { x[2] <- 
                                                            str_replace(x[2], 
                                                               single_terms[[i]][1], 
                                                               single_terms[[i]][2])
                                                          return(x)
                                                          })
}

compound_terms_fully_evaluated <- compound_terms_replaced
for(i in 1:length(compound_terms_fully_evaluated)){
  single_term <- compound_terms_fully_evaluated[[i]]
  compound_terms_fully_evaluated[to_replace_list_2[[i]]] <- lapply(compound_terms_fully_evaluated[to_replace_list_2[[i]]],
                                                          function(x) { x[2] <- 
                                                            str_replace(x[2], 
                                                                        single_term[1], 
                                                                        single_term[2])
                                                          return(x)
                                                          })
  
}



for(i in 1:length(compound_terms_fully_evaluated)){
  if(compound_terms_fully_evaluated[[i]][1] == "root"){
    print(compound_terms_fully_evaluated[[i]][2])
  }
}

