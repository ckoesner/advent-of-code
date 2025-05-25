
monkeys <-
  list(
    list(items = c(92, 73, 86, 83, 65, 51, 55, 93),
         operation = function(x) x * 5,
         test = function(x) ifelse(x %% 11 == 0, 3, 4),
         inspections = 0),
    
    list(items = c(99, 67, 62, 61, 59, 98),
         operation = function(x) x * x,
         test = function(x) ifelse(x %% 2 == 0, 6, 7),
         inspections = 0),
    
    list(items = c(81, 89, 56, 61, 99),
         operation = function(x) x * 7,
         test = function(x) ifelse(x %% 5 == 0, 1, 5),
         inspections = 0),
    
    list(items = c(97, 74, 68),
         operation = function(x) x + 1,
         test = function(x) ifelse(x %% 17 == 0, 2, 5),
         inspections = 0),
    
    list(items = c(78, 73),
         operation = function(x) x + 3,
         test = function(x) ifelse(x %% 19 == 0, 2, 3),
         inspections = 0),
    
    list(items = c(50),
         operation = function(x) x + 5,
         test = function(x) ifelse(x %% 7 == 0, 1, 6),
         inspections = 0),
    
    list(items = c(95, 88, 53, 75),
         operation = function(x) x + 8,
         test = function(x) ifelse(x %% 3 == 0, 0, 7),
         inspections = 0),
    
    list(items = c(50, 77, 98, 85, 94, 56, 89),
         operation = function(x) x + 2,
         test = function(x) ifelse(x %% 13 == 0, 4, 0),
         inspections = 0)
  )


# monkeys <-
#   list(
#     list(items = c(79, 98),
#          operation = function(x) x * 19,
#          test = function(x) ifelse(x %% 23 == 0, 2, 3),
#          inspections = 0),
# 
#     list(items = c(54, 65, 75, 74),
#          operation = function(x) x + 6,
#          test = function(x) ifelse(x %% 19 == 0, 2, 0),
#          inspections = 0),
# 
#     list(items = c(79, 60, 97),
#          operation = function(x) x * x,
#          test = function(x) ifelse(x %% 13 == 0, 1, 3),
#          inspections = 0),
# 
#     list(items = c(74),
#          operation = function(x) x + 3,
#          test = function(x) ifelse(x %% 17 == 0, 0, 1),
#          inspections = 0)
#     )


print_worry_levels <- function(monkeys){
  for(i in 1:length(monkeys)){
    print(paste("Worry Level for Monkey", i, ":"))
    print(monkeys[[i]]$items)
  }
}


monkey_round <- function(monkeys){
  for(j in 1:length(monkeys)){
    print(paste("Monkey", j, ":"))
    print("Worry level before inspection: ")
    print(monkeys[[j]]$items)
    
    worry_level <- sapply(monkeys[[j]]$items, monkeys[[j]]$operation)
    print("Worry level during inspection : ")
    print(worry_level)
    
    worry_level <- sapply(worry_level, function(x) floor(x/3))
    print("Worry level after inspection: ")
    print(worry_level)
    
    send_to_monkey <- sapply(worry_level, function(x) monkeys[[j]]$test(x) + 1)
    print("Send items to monkey")
    print(send_to_monkey)
    for(k in unique(send_to_monkey)){
      monkeys[[k]]$items <- c(monkeys[[k]]$items, worry_level[send_to_monkey == k])
    }
    monkeys[[j]]$items <- c()
    monkeys[[j]]$inspections <- monkeys[[j]]$inspection + length(worry_level)
  }
  return(monkeys)
}


monkey_round <- function(monkeys){
  mymod <- 11 * 2 * 5 * 17 * 19 * 7 * 3 * 13
  # mymod <- 23 * 19 * 13 * 17
  
  for(j in 1:length(monkeys)){
    # print(paste("Monkey", j, ":"))
    # print("Worry level before inspection: ")
    # print(monkeys[[j]]$items)
    
    worry_level <- sapply(monkeys[[j]]$items, monkeys[[j]]$operation)
    # print("Worry level during inspection : ")
    # print(worry_level)
    
    worry_level <- sapply(worry_level, function(x) x %% mymod)
    send_to_monkey <- sapply(worry_level, function(x) monkeys[[j]]$test(x) + 1)
    # print("Send items to monkey")
    # print(send_to_monkey)
    
    for(k in unique(send_to_monkey)){
      monkeys[[k]]$items <- c(monkeys[[k]]$items, worry_level[send_to_monkey == k])
    }
    monkeys[[j]]$items <- c()
    monkeys[[j]]$inspections <- monkeys[[j]]$inspection + length(worry_level)
  }
  return(monkeys)
}


for(i in 1:10000){
  monkeys <- monkey_round(monkeys)
}
inspections <- sapply(monkeys, function(x) x$inspections)
print(inspections)
most_active_monkeys <- sort(inspections, decreasing = TRUE)[1:2]
print(prod(most_active_monkeys))

