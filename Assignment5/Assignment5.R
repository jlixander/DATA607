library(tidyverse)
library(rjson)
library(jsonlite)
library(RJSONIO)
library(RCurl)
library(httr)
library(XML)
library(xml2)
library(rvest)

#Load json file-----------------------------------------------------------------
url_json = "https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/Assignment5.json"
json_df = fromJSON((url_json)) %>% 
  as.data.frame


#Load xml file------------------------------------------------------------------
url_xml = GET("https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/assignment5.xml") #retreive xml file
xml_doc <- xmlParse(url_xml) #parse xml file
xml_df <- xmlToDataFrame(doc, nodes=getNodeSet(doc, "//TopBooks")) #Select which level to unlist.


#Load html file-----------------------------------------------------------------
html_url <- "https://raw.githubusercontent.com/jlixander/DATA607/main/Assignment5/Assignment5.html"

html_doc <- xml2::read_html(html_url)
html_doc <- xmlParse(html_doc) #parse xml file
html_df <- xmlToDataFrame(html_doc, nodes=getNodeSet(html_doc, "//TopBooks")) #Select which level to unlist.
