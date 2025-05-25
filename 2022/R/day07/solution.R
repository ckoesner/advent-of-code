
library(readr)
console_text <- strsplit(read_lines("day7/input"), " ")

setClass("Command", slots = list(name="character", input="character", output="list"))
setClass("File", slots=list(name="character", size="integer"))
setClass("Directory", slots=list(name="character", 
                                 files = "list", 
                                 directories = "list",
                                 upper_dir = "Directory"))

current_output_list <- list()
command_list <- list()
for(i in 1:length(console_text)){
  current_line <- console_text[[i]]
  print(current_line)
  if(current_line[1] == "$"){
    current_command <- new("Command", name = current_line[2], input = current_line[3])
    command_list <- append(command_list, current_command)
  }else if(current_line[1] == "dir"){
    current_command@output <- append(current_command@output, new("Directory", name = current_line[2]))
    command_list[[length(command_list)]] <- NULL
    command_list <- append(command_list, current_command)
  }else{
    current_command@output <- append(current_command@output, new("File", name = current_line[2], size = strtoi(current_line[1])))
    command_list[[length(command_list)]] <- NULL
    command_list <- append(command_list, current_command)
  }
}


file1 <- new("File", name = "a", size = 10L)
file2 <- new("File", name = "b", size = 13L)

root_dir <- new("Directory", name = "", files = list(file1, file2))
current_dir <- root_dir

evaluate_command <- function(command_stack, pointer){
  if(command_stack[[1]][2] == "cd"){
    if(command_stack[[1]][3] == "/"){
      current_dir <- root_dir
    }
    if(command_stack[[1]][3] == ".."){
      current_dir <- current_dir$upper_dir
    }
  }
}