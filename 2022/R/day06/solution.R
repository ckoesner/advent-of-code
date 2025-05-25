
library(readr)
signal <- read_lines("day6/input")
signal <- strsplit(signal, "")[[1]]


give_first_marker <- function(signal, marker_length){
  for(i in marker_length:length(signal)){
    if(length(unique(signal[(i-marker_length+1):i])) == marker_length){
      return(i)
    }
  }
}

signal[1195:1198]
print(give_first_marker(signal, 4))
print(give_first_marker(signal, 14))
