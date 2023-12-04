require(pdftools)
require(tm)
files<-list.files(pattern="pdf$")
opinions<-lapply(files,pdf_text)

setwd()


setwd("D:/Education/Semester 4/Prob and Stats/project2")
library(pdftools)
text <- pdf_text("normal distribution table.pdf")
text
write(text,"usaid.txt")


require(pdftools)
require(tidyverse)
require(ggplot2)

url <- c("http://www.cicad.oas.org/oid/pubs/JamaicaNationalHouseholdDrugSurvey2017ENG.pdf")

raw_text <- map(url, pdf_text)

clean_table1 <- function(raw) {
  raw <- map(raw, ~ str_split(.x, "\\n") %>% unlist())
  raw <- reduce(raw, c)
  table_start <- stringr::str_which(tolower(raw), "alcohol use pattern")
  table_end <- stringr::str_which(tolower(raw), "never used")
  table_end <- table_end[min(which(table_end > table_start))]
  
  #Build the table  and remove special characters
  table <- raw[(table_start):(table_end)]
  table <- str_replace_all(table, "\\s{2,}", "|")
  text_con <- textConnection(table)
  data_table <- read.csv(text_con, sep = "|")
  
  #Create a list of column names 
  colnames(data_table) <- c("x","Alcohol Use Pattern", "Males","Females","Total")
  data_table
}

results <- map_df(raw_text, clean_table1) 
head(results)

pdf_text("normal distribution table.pdf")

v =pdf_text("https://www.math.arizona.edu/~rsims/ma464/standardnormaltable.pdf")
print(v)
