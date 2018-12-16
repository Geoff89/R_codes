library(rio)
credit <- import("C:\\Users\\jeffnerd\\Desktop\\credit.csv")
str(credit)
export(credit, "C:\\Users\\jeffnerd\\Desktop\\credit.xlsx")
convert("C:\\Users\\jeffnerd\\Desktop\\credit.csv","C:\\Users\\jeffnerd\\Desktop\\credit.dta")

#reading from databases
library(RODBC)
RODBC::
conect <- odbcConnect("Local instance MYSQL57", uid = "root", pwd = "Mog#67sag")
ch = odbcConnect("Local instance MySQL57", uid = "root",pwd = "Mog#67sag" )
sqlTables(ch)
sqlPrimaryKeys(ch, "department")
account <- sqlQuery(ch, "select * from account")
head(account)
is.data.frame(account)
mean(account$account_id)
cov(account$account_id, account$cust_id)

odbcClose(ch)

#reading online data 
library(RCurl)
packt_page <- ("https://www.packtpub.com/")
str(packt_page)

library(httr)
packt_page <- GET("https://www.packtpub.com")
str(packt_page, max.level = 1)
str

#for webscraping we use rvest package
str(content(packt_page, type="text"), nchar.max=200)

library(rvest)
packt_page <- html("https://www.packtpub.com")
html_node(packt_page, "title")
html_node(packt_page, "title") %>% html_text()

cran_ml <- html("http://cran.r-project.org/web/views/MachineLearning.html")
ml_packages <- html_nodes(cran_ml, "a")
head(ml_packages, n = 7)