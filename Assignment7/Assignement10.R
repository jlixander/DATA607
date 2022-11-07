#Load libraries
library(jsonlite)
library(httr)
library(curl)
library(tidyverse)

#http basics GEt/POST/PUT/DELETE etc.
#https://code.tutsplus.com/tutorials/http-the-protocol-every-web-developer-must-know-part-1--net-31177

#What is an API
#cran.r-project.org/web/packages/httr/vignettes/quickstart.html

#As well as GET(), you can also use the HEAD(), POST(), PATCH(), PUT() and DELETE() verbs.

#status types: #https://www.flickr.com/photos/girliemac/sets/72157628409467125
#200: successful
#404: file not found
#403: permission denied
#500: generic failure

#Request Connection Example:---------------------------------------------------------------
r <- GET("http://httpbin.org/get")

r #view response

status_code(r) #Get status code

headers(r) #print header info

str(content(r)) #print content

http_status(r) #get API response status code
r$status_code


#automatically throw a warning or raise an error if a request did not succeed:
warn_for_status(r)
stop_for_status(r)

##The body-------------------
content(r, "text") #accesses the body as a character vector:
content(r, "text", encoding = "ISO-8859-1") #Unfortunately you can’t always trust what the server tells you, so you can override encoding if needed:
stringi::stri_enc_detect(content(r, "raw")) #If you’re having problems figuring out what the correct encoding should be

content(r, "raw") #For non-text requests, you can access the body of the request as a raw vector:

bin <- content(r, "raw") 
writeBin(bin, "myfile.txt") #this is the highest fidelity way of saving files to disk:


str(content(r, "parsed")) # JSON automatically parsed into named list

##The headers--------------------
headers(r)

headers(r)$date

##Cookies---------------
r <- GET("http://httpbin.org/cookies/set", query = list(a = 1))
cookies(r)

#The request--------------------------------------------------------------------
#Like the response, the request consists of three pieces: a status line, headers 
#and a body. The status line defines the http method (GET, POST, DELETE, etc) and
#the url. You can send additional data to the server in the url (with the query 
#string), in the headers (including cookies) and in the body of POST(), PUT() and
#PATCH() requests.

##The url query string
#A common way of sending simple key-value pairs to the server is the query string:
#e.g. http://httpbin.org/get?key=val.

r <- GET("http://httpbin.org/get", 
         query = list(key1 = "value1", key2 = "value2")
)
content(r)$args

##Custom headers-----------------
#You can add custom headers to a request with add_headers():
r <- GET("http://httpbin.org/get", add_headers(Name = "Hadley"))
str(content(r)$headers) #Note that content(r)$header retrieves the headers that httpbin received


#Cookies--------------------------
r <- GET("http://httpbin.org/cookies", set_cookies("MeWant" = "cookies")) #To send your own cookies to the server, use set_cookies():
content(r)$cookies

#Request body-------------------------------------------------------------------
#When POST()ing, you can include data in the body of the request.
r <- POST("http://httpbin.org/post", body = list(a = 1, b = 2, c = 3)) 


#You can use the encode argument to determine how this data is sent to the server:
url <- "http://httpbin.org/post"
body <- list(a = 1, b = 2, c = 3)

# Form encoded
r <- POST(url, body = body, encode = "form")
# Multipart encoded
r <- POST(url, body = body, encode = "multipart")
# JSON encoded
r <- POST(url, body = body, encode = "json")

#To see exactly what’s being sent to the server, use verbose()
POST(url, body = body, encode = "multipart", verbose()) # the default
POST(url, body = body, encode = "form", verbose())
POST(url, body = body, encode = "json", verbose())

#To send files
POST(url, body = upload_file("mypath.txt"))
POST(url, body = list(x = upload_file("mypath.txt")))

#Assignment10------------------------------------------------------------------
#Set API key in a vector
api_key <- "GXydLE1KSp8UEvtev8gdeDh8ojwOVZIV"

#GET result from url and check for list name of best sellers
r1 <- GET("https://api.nytimes.com/svc/books/v3/lists/names.json?api-key=GXydLE1KSp8UEvtev8gdeDh8ojwOVZIV") #get name of best sellers list
status_code(r1)
str(content(r1))

#Build API endpoint using parameters and key. Send GET request.
url <- "https://api.nytimes.com/svc/books/v3/lists/current/culture.json?api-key=GXydLE1KSp8UEvtev8gdeDh8ojwOVZIV"
r2 <- GET(url) 
warn_for_status(r2)
status_code(r2)


#Transform the data into a dataframe
data <- content(r2, as="text")
newdata <- fromJSON(data, flatten=TRUE)
books_df <- as.data.frame(newdata$results$books)
