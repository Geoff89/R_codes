# RODBC Example
# import 2 tables (Crime and Punishment) from a DBMS
# into R data frames (and call them crimedat and pundat)

library(ROBDC)
library(dbConnect)
myconn <-odbcConnect("mydsn", uid="Rob", pwd="aardvark")
crimedat <- sqlFetch(myconn, "Crime")
pundat <- sqlQuery(myconn, "select * from Punishment")
close(myconn) 
#bank database
myconn <-odbcConnect("bank", uid="jeff", pwd="root")
dbconnection <- odbcDriverConnect("Driver=ODBC Driver 11 for SQL Server;Server=Your_Server_Name\\SQLEXPRESS; Datab
odbcDriverConnect("SERVERNAME=;DRIVER={};DATABASE=;UID=;PWD=;")
Hostname = localhost
username = root/jeff
SERVERNAME = MySQL57
password = root

#worked.Should create a ODBC conectio at control panel and then test to see if it worked
ch = odbcConnect("Local instance MySQL57", uid = "root",pwd = "Mog#67sag" )
sqlTables(ch) 
sqlQuery(ch, "SELECT * FROM branch")



con <- dbConnect(MySQL(),user = "root", password = "Mog#67sag",dbname = "bank", host = "localhost")                                  