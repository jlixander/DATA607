#Load libraries-----------------------------------------------------------------
library(tidyverse)
library(readtext)
library(caret)
library(tm)
library(caTools)
library(wordcloud)
library(e1071)
library(gmodels)



#Load data----------------------------------------------------------------------
df <- read.csv("C:/Users/jlixa/Desktop/CUNY/Fall22/DATA607_Rep/Project4/Data/data.csv",colClasses=c("NULL",NA,NA))

# shuffle the dataframe by rows
df= df[sample(1:nrow(df)), ]

df$spam <- factor(df$spam)


#Further data cleaning:---------------------------------------------------------
corpus <- VCorpus(VectorSource(df$text)) %>%
  tm_map(content_transformer(tolower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("english")) %>%
  tm_map(removePunctuation) |>
  tm_map(stemDocument) |>
  tm_map(stripWhitespace)

lapply(corpus[1:3], as.character) #check

dtm <- DocumentTermMatrix(corpus)
inspect(dtm)

findFreqTerms(dtm,5) #Find top freq words. Sparsity: 90%

dtm = removeSparseTerms(dtm, 0.7)

#split data
train <- dtm[1:1958, ] # 70% for training
test <- dtm[1959:2796, ] #30% for training
train_type <- df[1:1958, ]$spam
test_type <- df[1959:2796, ]$spam


tbl_train <- prop.table(table(train_type))
tbl_train

tbl_test <- prop.table(table(test_type))
tbl_test

#Build wordclouds---------------------------------------------------------------
spamText <- subset(df, spam == "yes") 
wordcloud(spamText$text, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2") )

hamText <- subset(df, spam =="no") # selecting ham texts
wordcloud(hamText$text, max.words = 50, scale = c(5, 0.3),random.order = FALSE, rot.per = 0.15, colors = brewer.pal(8, "Dark2"))

freq_words <- findFreqTerms(train, 5) 

freq_words_train <- train[ , freq_words]
freq_words_test <- test[ , freq_words]

# creating a function for conversion
convert <- function(x) {x <- ifelse(x > 0, "y", "n")} 
train <- apply(freq_words_train, MARGIN = 2, convert)
test <- apply(freq_words_test, MARGIN = 2, convert)
str(train) # verifying the conversion

#Training Model-----------------------------------------------------------------
# Creating a Naive Bayes classifier
email_classifier <- naiveBayes(train, train_type)

# Making prediction & evaluation with the classifier
test_prediction <- predict(email_classifier, test)
CrossTable(test_prediction, test_type, 
           prop.chisq = FALSE, prop.t = FALSE,
           dnn = c('predicted', 'actual'))

#model is 90% accurate!

#Resources:---------------------------------------------------------------------
#https://www.pluralsight.com/guides/building-classification-models-in-r
#https://www.linkedin.com/pulse/detailed-naive-bayes-spam-filter-r-leonardo-anello/
#https://www.r-bloggers.com/2013/07/document-classification-using-r/