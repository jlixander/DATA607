library(tidyverse)
library(dplyr)
library("anytime")
library(RCurl)

#load in data into a dataframe and parse columns
urlfile <- 'https://raw.githubusercontent.com/fivethirtyeight/data/master/college-majors/majors-list.csv'
majors <- read_csv(url(urlfile))

#View df
majors

#Exercise 1
df1 <- majors |> 
  filter(str_detect(majors$Major, regex("DATA|STATISTICS")))
print(df1)


#Exercise 2
var_str <- '[1] "bell pepper"  "bilberry"     "blackberry"   "blood orange"

[5] "blueberry"    "cantaloupe"   "chili pepper" "cloudberry"  

[9] "elderberry"   "lime"         "lychee"       "mulberry"    

[13] "olive"        "salal berry"'


#Remove all white spaces
var_str <-  str_replace_all(var_str,'((?![a-z])\\s+(?![a-z]))', '') #remove all whitespace
var_str

#Remove all brackets and all digits inside
var_str <- str_replace_all(var_str,'(\\[\\d*\\])', '') #replace all brackets along w/ all digits inside
print(var_str)

#Remove all "\" after a fruit name
var_str <- str_replace_all(var_str, fixed("\\"), "") #remove all backslashes
print(var_str)


#Exercise 3
#(.)\1\ =====================> Looks for any character followed by two of the same.Any character that repeats 3 times. Cannot be new line.

#"(.)(.)\\2\\1"==============> Looks for something like this: '"ab\2\1"' where a and b can be any character excluding new line.

#(..)\1 =====================> Looks for any 2 characters except new line. Continues to look for 2 more within the capture. Ex: abab and grgr.

#"(.).\\1.\\1" ==============> Looks for something like this: "je\1\\1" where j and e can be any character excluding new line.
#The tail needs to be \1(anycharacter)1. string must be enclosed in quotations.

#"(.)(.)(.).*\\3\\2\\1" =====> Looks for something like this: "degaaaaaa\3\2\1" where the first four characters can be anything but new line.
#The fifth character matches the 4th character an unlimited number of times. followed by literal \3\2\1. String must be enclosed in quotations.


#Exercise 4
#Start and end with the same character.
#^([a-z])(([a-z]+\\1$)|\\1?$)
words <- c("car", "nun", "room", "bob", "picture", "racecar", 'whenever', "mama","church", "mississippi", "eleven")
rgx1 ="^([a-z])(([a-z]+\\1$)|\\1?$)"
ss1 <- str_subset(words, rgx1)
ss1

#Contain a repeated pair of letters (e.g. "church" contains "ch" repeated twice.)
rgx2 ="([A-Za-z][A-Za-z]).*\\1"
ss2 <- str_subset(words, rgx2)
ss2

#Contain one letter repeated in at least three places (e.g. "eleven" contains three "e"s.)
rgx3 ="([A-Za-z]).*\\1.*\\1"
ss3 <- str_subset(words, rgx3)
print(ss3)
