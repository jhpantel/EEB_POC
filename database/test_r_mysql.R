#Test R MySQL. Code from shiny.rstudio.com 

library(RMySQL)

options(mysql = list(
  # "host" = "https://github.com/eeb-bipoc-db/EEB_POC",
  # "port" = 9418,
  # "user" = "jusinowicz",
  # "password" = "Klimtb@be18"
  "host" = "127.0.0.1",
  "port" = 3306,
  "user" = "myuser",
  "password" = "mypassword"
))
databaseName <- "myshinydatabase"
table <- "responses"

saveData <- function(data) {
  # Connect to the database
  db <- dbConnect(MySQL(), dbname = databaseName, host = options()$mysql$host, 
      port = options()$mysql$port, user = options()$mysql$user, 
      password = options()$mysql$password)
  # Construct the update query by looping over the data fields
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    table, 
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  # Submit the update query and disconnect
  dbGetQuery(db, query)
  dbDisconnect(db)
}