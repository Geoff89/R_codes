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
Hostname = localhost
username = root
SERVERNAME = MySQL57
password = root

#worked.Should create a ODBC conectio at control panel and then test to see if it worked
ch = odbcConnect("Local instance MySQL57", uid = "root",pwd = "Mog#67sag" )
sqlTables(ch) 
sqlQuery(ch, "SELECT * FROM branch")
#making this data to be a data frame
branch <-sqlQuery(ch, "SELECT * FROM branch")
branch.dataframe <- as.data.frame(branch)
branch.dataframe
mean(branch.dataframe$branch_id)
var(branch.dataframe$zip)
summary(branch.dataframe$zip)
