require(RPostgreSQL)
library(shiny)
library(shinymanager)
library(DBI)
library(glue)

dbname = "*****"
host = "localhost"
port = *****
  user = "*****"
password = "******"

con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname , host = host, port = port ,
                 user = user, password = password )


DBI::dbWriteTable(con, "my_table", data.frame(
  user = c("test"),
  password = c("123"),
  stringsAsFactors = FALSE
))

# or a config .yml file or others arguments
my_custom_check_creds <- function(dbname, host, port, db_user, db_password) {
  
  # finally one function of user and password
  function(user, password) {
    
    con <- dbConnect(dbDriver("PostgreSQL"), dbname = dbname, 
                     host = host, port = port,
                     user = db_user, password = db_password)
    
    on.exit(dbDisconnect(con))
    
    req <- glue_sql("SELECT * FROM my_table WHERE \"user\" = ({user}) AND \"password\" = ({password})", 
                    user = user, password = password, .con = con
    )
    
    req <- dbSendQuery(con, req)
    res <- dbFetch(req)
    if (nrow(res) > 0) {
      list(result = TRUE, user_info = list(user = user, something = 123))
    } else {
      list(result = FALSE)
    }
  }
}

ui <- fluidPage(
  tags$h2("My secure application"),
  verbatimTextOutput("auth_output")
)
ui <- secure_app(ui)


server <- function(input, output, session) {
  res_auth <- secure_server(
    check_credentials = my_custom_check_creds(
      dbname = "******",
      host = "*****",
      port = ****,
      db_user = "*****",
      db_password = "*******"
    )
  )  
  auth_output <- reactive({
    reactiveValuesToList(res_auth)
  })
  
  # access info
  observe({
    print(auth_output())
  })
}

shinyApp(ui, server)