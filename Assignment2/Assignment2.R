#Load Libraries
library(DBI)
library(RPostgres)
library(tcltk) #library to handle input prompts to hide password
sleep_default <- 3

#Create input prompt for database password
psswd <- .rs.askForPassword("Database Password:") #password is "postgres"


#Create database connection
Sys.sleep(sleep_default)
con <- dbConnect(
  RPostgres::Postgres(),
  # without the previous and next lines, some functions fail with bigint data 
  #   so change int64 to integer
  bigint = "integer",  
  host = "localhost",
  port = 5432,
  user = "postgres",
  password = psswd,
  dbname = "Data607_Assignment2")

#List tables in database
dbListTables(con)

#Run query to load movies data from the database into a dataframe
df <- dbGetQuery(con, "select * from movies") #Returns the result of a query as a data frame
print(df)

#Disconnect from database
dbDisconnect(con)


