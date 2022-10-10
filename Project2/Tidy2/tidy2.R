#Load Libraries
library("tidyverse")
library("caret")
library('caTools')


#load in data into a dataframe
urlfile <- 'https://raw.githubusercontent.com/jlixander/DATA607/main/Project2/Tidy2/brain_stroke.csv'
df <- read_csv(url(urlfile))

#Give each observation a unique ID - helps keep data group after a pivot_longer
df$ID <- seq.int(nrow(df))

#####Data Wrangling/tidying
#Remove encoded values from columns
df <- df |>
  janitor::clean_names() |>
  mutate(hypertension = recode(hypertension, "0" = "Negative", "1" = "Positive"),
         stroke = recode(stroke, "0" = "Negative", "1" = "Positive"),
         heart_disease = recode(heart_disease, "0" = "Negative", "1" = "Positive")
         )

#Set column data types
df$age<-round(as.numeric(df$age), 0)

#Check for na values in each column
df %>%
  summarise_all(funs(sum(is.na(.))))

#Consolidate all condition columns
df_longer <- df |>
  pivot_longer(cols = hypertension:heart_disease, 
               names_to = "condition", 
               values_to = "condition_status")

#####Data Analysis
#create dataframe without id
df_class <- select(df, -id)

#remove chr values to factor
names <- c(1,3,4,5,6,7, 10,11) #create list of columns to turn into factor type
df_class[,names] <- lapply(df_class[,names] , factor)
glimpse(df_class)

#partition and split data
spl = sample.split(df_class$stroke, SplitRatio = 0.80)
train = subset(df_class, spl==TRUE)
test = subset(df_class, spl==FALSE)
print(dim(train)); print(dim(test))

#build, predict and evaluate the model
model_glm = glm(stroke ~ . , family="binomial", data = train)
summary(model_glm)

#Check Baseline Accuracy
prop.table(table(train$stroke))

# Predictions on the training set
predictTrain = predict(model_glm, data = train, type = "response")

# Confusion matrix on training data
table(train$stroke, predictTrain >= 0.5)
(3786+1)/nrow(train)

#Predictions on the test set
predictTest = predict(model_glm, newdata = test, type = "response")

# Confusion matrix on test set
table(test$stroke, predictTest >= 0.5)
957/nrow(test)
