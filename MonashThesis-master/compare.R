setwd("/Volumes/5550/turk2")

file <- read.csv("file.csv")
txt <- vector()
name <- vector()
txt <- as.character(file$txt)
name <- as.character(file$name)

readata <- function(i) {
  txt_i <- txt[i]
  name_i <- paste(name[i])
  data_i <- read.delim(txt_i, header = TRUE, sep = "", dec = ".")
  assign(name_i, data_i)
  return(name_i)
}

mapply(readata, 1:70)

readata(4)

