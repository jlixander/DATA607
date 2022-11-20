#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(magrittr)
library(readtext)
library(caret)

#Create functions---------------------------------------------------------------
#Clean html from string
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Load data----------------------------------------------------------------------
spam_dir <- "C:/Users/jlixa/Desktop/CUNY/Fall22/DATA607_Rep/Project4/Data/spam_2"
#spam_dir
filenames_spam <- list.files(spam_dir, pattern = "*.*", full.names = TRUE)
length(filenames_spam)

ham_dir <- "C:/Users/jlixa/Desktop/CUNY/Fall22/DATA607_Rep/Project4/Data/easy_ham_2"
#ham_dir
filenames_ham <- list.files(ham_dir, pattern = "*.*", full.names = TRUE)
length(filenames_ham)

#HTML clean up function---------------------------------------------------------
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}

#Parse emails-------------------------------------------------------------------
spam <- filenames_spam |>
  as.data.frame() |>
  magrittr::set_colnames("file") |>
  mutate(text = lapply(filenames_spam, readLines)) |>      #read emails
  mutate(text = lapply(text, cleanFun)) |>                 #clean html tags
  unnest(c(text)) |>                                       #expand text by line
  mutate(spam = "yes") |>                                #create column labeling as spam
  group_by(file) |>                                        
  mutate(text = paste(text, collapse = " ")) |>            #use group() and paste() to combine strings         
  ungroup() |>
  distinct()

ham <- filenames_ham |>
  as.data.frame() |>
  magrittr::set_colnames("file") |>
  mutate(text = lapply(filenames_ham, readLines)) |>       #read emails
  mutate(text = lapply(text, cleanFun)) |>                 #clean html tags
  unnest(c(text)) |>                                       #expand text by line
  mutate(spam = "no") |>                                #create column labeling as spam
  group_by(file) |>                                        
  mutate(text = paste(text, collapse = " ")) |>            #use group() and paste() to combine strings         
  ungroup() |>
  distinct()

#Combine ham and spam-----------------------------------------------------------
data <- rbind(spam, ham)



write.csv(data,"C:/Users/jlixa/Desktop/CUNY/Fall22/DATA607_Rep/Project4/Data/data.csv", row.names = FALSE)
