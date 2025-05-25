library(readr)


myData = read_lines("day4/input")


calc <- function(line) {
  sections_per_elf <- strsplit(line, ",")[[1]]
  section_num_elf1 <- strtoi(strsplit(sections_per_elf[1], "-")[[1]])
  section_num_elf2 <- strtoi(strsplit(sections_per_elf[2], "-")[[1]])
  
  sections_elf1 <- section_num_elf1[1]:section_num_elf1[2]
  sections_elf2 <- section_num_elf2[1]:section_num_elf2[2]
  
  return(
    all(sections_elf1 %in% sections_elf2) || 
      all(sections_elf2 %in% sections_elf1))
}

res <- sum(sapply(myData, calc))




calc <- function(line) {
  sections_per_elf <- strsplit(line, ",")[[1]]
  section_num_elf1 <- strtoi(strsplit(sections_per_elf[1], "-")[[1]])
  section_num_elf2 <- strtoi(strsplit(sections_per_elf[2], "-")[[1]])
  
  sections_elf1 <- section_num_elf1[1]:section_num_elf1[2]
  sections_elf2 <- section_num_elf2[1]:section_num_elf2[2]
  
  return(any(sections_elf1 %in% sections_elf2))
}

sum(sapply(myData, calc))
