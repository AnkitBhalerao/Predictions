
setwd("C:\\R Data - Udemy")

# Read from CSV
resp1<-read.csv("Resp1.csv", header=T)
head(resp1)
str(resp1)

# Read from txt
resp2<-readresp1<-read.table("Resp1.txt", header=T)
head(resp2)

# read comma sep
winer<-read.csv("winequality-red.csv", header=T, sep=";")
head(winer)
summary(winer)

install.packages("readxl")
library("readxl")

dfb<-read_excel("boston1.xls")
head(dfb)
summary(dfb)

install.packages("RSQLite")
install.packages("DBI")

library("RSQLite")
library("DBI")

db=RSQLite::datasetsDb()
dbListTables(db)

dbReadTable(db, "CO2")

dbGetQuery(db, "Select * from co2 where conc < 100")

dbDisconnect(db)

setwd("C:\\R Data - Udemy\\Code-n-data\\Code_n_data\\section2\\database.sqlite")

db = dbConnect(dbDriver("SQLite"), "database.sqlite")

alltables = dbListTables(db)

head(alltables)

reviews <- dbGetQuery(db, "Select * from reviews limit 100")

head(reviews)

setwd("C:\\R Data - Udemy\\Code-n-data\\Code_n_data\\section2\\ipl\\database.sqlite")

db = dbConnect(dbDriver("SQLite"), "database.sqlite")

alltables = dbListTables(db)

head(alltables)

dbReadTable(db, "country")

head(dbReadTable(db, "Batsman_Scored"))

x = dbReadTable(db, "Batsman_Scored")

summary (x)

str(x)

dbDisconnect(db)

install.packages("rjson")
library(rjson)

setwd("C:\\R Data - Udemy\\Code-n-data\\Code_n_data\\section2")

json_file <- "http://api.worldbank.org/country?per_page=10&region=OED&lendingtype=LNX&format=json"

#json data is stored in json_data
json_data <- fromJSON(file=json_file)

#you can see that this json file has two objects in the outer most list
json_data[[1]]
json_data[[2]]

#you can access any particular object from the json data as shown below
d3 <- lapply(json_data[[2]], function(x) c(x["id"], x["iso2Code"]))

d3 <- do.call(rbind, d3)
d3
d4 <- lapply(json_data[[2]], function(x) c(x["id"], x["iso2Code"], x$region["id"], x$region["value"], x["capitalCity"]))
d4 <- do.call(rbind, d4)
d4

#other example

json_file <- "skorea.json"
#json data is stored in json_data

json_data <- fromJSON(file=json_file)

#you can access your data as simply shown below
json_data[[1]]
json_data[[2]]
json_data[[5]]

#or you can extract some usefull data
d <- lapply(json_data, function(x) c(x['Image'], x['Criteria'], x['Site'], x['Area ha (acre)']))
d <- do.call(rbind, d)
d

#you can access any data you want from this table like shown below
col(d)
d[,1]
d[1,]

install.packages("pdftools")
install.packages("gsubfn")
library(pdftools)
library(gsubfn)

download.file("http://arxiv.org/pdf/1403.2805.pdf", "1403.2805.pdf", mode = "wb")
txt <- pdf_text("1403.2805.pdf")

# first page text
cat(txt[3])

# Table of contents
toc <- pdf_toc("1403.2805.pdf")

txt <- pdf_text("http://arxiv.org/pdf/1406.4806.pdf")

# some tables
cat(txt[18])
cat(txt[19])


### read in data & clean
txt2 = pdf_text("https://goo.gl/wUXvjk")

head(txt2)

pat = "([0-9]{4} [M\\.|Mme|Mlle]{1}.*?, [né|née]{1}.*?)\\."
data = unlist(gsubfn::strapply(txt2, pattern = pat))

head(data)

install.packages("devtools")
library(devtools)

install.packages(ghit)

ghit::install_github(c("leeper/tabulizerjars", "leeper/tabulizer"), INSTALL_opts = "--no-multiarch", dependencies = c("Depends", "Imports"))
library(tabulizerjars)
devtools::install_github(c("ropensci/tabulizer"),args= "--no-multiarch")


library(tabulizer)
library(tabulizerjars)

tabs <- extract_tables("https://goo.gl/BMfgkS")

head(tabs)

tabs[[6]]

tabs2=extract_tables("http://oro.open.ac.uk/39349/1/Singh-et-al-2014_JARS.pdf")

tail(tabs2)
class(tabs2)

tabs2=as.data.frame(tabs2)


#------------------------------------------#

##################################################################
######### read in data from HTML tables with XML

install.packages("XML")
install.packages("RCurl")

library(XML)
library(RCurl)

url="https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"
##webpage we are intersted in

urldata <- getURL(url) #get data from this URL

data <- readHTMLTable(urldata, stringsAsFactors = FALSE)
#read the hHTML table

#medal tally
names(data)
head(data)

x=data$`2016 Summer Olympics medal table`


head(x)

#------------------------------------
##################################################################
### Read in data from Wikipedia HTML tables
install.packages("rvest")
library(rvest)
#Summer olympics medal tally

url <- "https://en.wikipedia.org/wiki/2016_Summer_Olympics_medal_table"



medal_tally <- url %>% read_html() %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[2]') %>% html_table(fill=TRUE)
## copy xpath
## //*[@id="mw-content-text"]/div/table[2]
# //*[@id="mw-content-text"]/div/table[2]

medal_tally <- medal_tally[[1]]
head(medal_tally)

#WHS Sites in the UK

url2="https://en.wikipedia.org/wiki/List_of_World_Heritage_Sites_in_the_United_Kingdom_and_the_British_Overseas_Territories"


whsuk <- url2 %>% read_html() %>% 
  html_nodes(xpath='//*[@id="mw-content-text"]/div/table[3]') %>% html_table(fill=TRUE)

whsuk <- whsuk[[1]]
head(whsuk)

########## Cleaning Tables Extracted from Webpages

library(rvest)
library(stringr)
install.packages("tidyr")
library(tidyr)

##Access the webpage with the tabular data

url = 'http://espn.go.com/nfl/superbowl/history/winners'
webpage =read_html(url)

sb_table = html_nodes(webpage, 'table')
sb = html_table(sb_table)[[1]] ##acces the first table on the page
head(sb)

## preliminary processing:remove the first two rows, and set the column names

sb = sb[-(1:2), ]#row,column
names(sb) = c("number", "date", "site", "result")
head(sb)

#divide between winner and losers
sb = separate(sb, result, c('winner', 'loser'), sep=', ', remove=TRUE)
head(sb)

## we split off the scores from the winner and loser columns.
##The function str_extract from the stringr package finds a 
##substring matching a pattern

pattern =" \\d+$"
sb$winnerScore = as.numeric(str_extract(sb$winner, pattern))
sb$loserScore =as.numeric(str_extract(sb$loser, pattern))
sb$winner = gsub(pattern, "", sb$winner)
sb$loser =gsub(pattern, "", sb$loser)
head(sb)


####### More on rvest

#### Read in and glean info from text data

library(rvest)
library(stringr)

url="https://www.washingtonpost.com/news/the-fix/wp/2016/09/26/the-first-trump-clinton-presidential-debate-transcript-annotated/?utm_term=.76c25871d72c"

t_link=read_html(url)

#to extract out the relevant html tag for the transcript 
transcript = t_link %>% html_nodes("#main-content") %>% html_text()
head(transcript)
##unstructured text

# We have 3 different patterns/speakers to search for 
#which will include in our expression using the or operator '|'
# str_locate_all of stringr will give the index (start and end position) of all the matches
markers <- str_locate_all(transcript, pattern = "CLINTON|TRUMP|HOLT")
head(markers)

# This returns a list with one component - we extract out that component
markers = markers[[1]]
# Now markers is a matrix indicating the start and end positions
#  extract start positions
markers =markers[,1]
#substr pulls out text chunks

##text chunks relating to Trump, clinton and holt

# Initialize a vector to store the results
res = vector(mode = "character", length = length(markers) - 1)
for (i in 1:(length(markers)-1)) {res[i] <- substr(transcript,markers[i],markers[i+1]-1)}
head(res)
#identfiy and store chunks spoken by Trump and Clinton 

clinton = res[sapply(res,function(x) grepl("CLINTON",x))]
clinton
trump = res[sapply(res,function(x) grepl("TRUMP",x))]
trump
head(res)

tot_words_t = unlist(sapply(trump, function(x) str_split(x, " ")))

# exclude blank values  
tot_words_t = tot_words_t[tot_words_t != ""]
length(tot_words_t)

#most common words?
library(plyr)

w_freq=count(tot_words_t) %>% arrange(desc(freq))

head(w_freq,n=20)






















































































