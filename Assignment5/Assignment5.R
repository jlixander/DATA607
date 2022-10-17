library(tidyverse)
library(jsonlite)
library(RJSONIO)
library(RCurl)
library(httr)
library(XML)
library(xml2)
library(rvest)

#Load json file-----------------------------------------------------------------
url_json = "https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/Assignment5.json"
json_df = jsonlite::fromJSON((url_json), simplifyDataFrame = TRUE) %>% 
  as.data.frame()


#Load xml file------------------------------------------------------------------
url_xml = GET("https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/assignment5.xml") #retreive xml file
xml_doc <- xmlParse(url_xml) #parse xml file
xml_df <- xmlToDataFrame(xml_doc, nodes=getNodeSet(xml_doc, "//TopBooks")) #Select which level to unlist.


#Load html file-----------------------------------------------------------------
html_url <- "https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/Assignment5.html"
html_doc <- xml2::read_html(html_url)
tables = html_doc |> html_table()
table_one = tables[[1]]
html_df <- head(table_one, - 1)  
