
library(DBI)
# Get table in schema
dbReadTable(con, name = Id(schema = "SCHEMA", table = "TABLE"))


## List schemas
# Dremio
dbGetQuery(con, "select SCHEMA_NAME from INFORMATION_SCHEMA.SCHEMATA")

# SQL Server (?)
DBI::dbGetQuery(conn, "SELECT SCHEMA_NAME FROM INFORMATION_SCHEMA.SCHEMATA")


