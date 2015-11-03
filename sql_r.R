install.packages("RMySQL")
library(RMySQL)

dbcon <- dbConnect(MySQL(), user="hanjy", password="hanjy",
                   dbname="kdd_algebra", host="147.47.123.220")

#2. List tables and fields in a table:

dbListTables(dbcon)
dbListFields(dbcon, "89t")


#3. Import and export data.frames: RMySQL-package 3
d <- dbReadTable(dbcon, "89t")
dbWriteTable(dbcon, "test", index) ## table from a data.frame

summary(d)
class(d)
nrow(d)

