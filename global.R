library(shiny)
library(shinydashboard)
library(shinyMobile)
library(dplyr)
library(glue)
library(sendmailR)
library(plotly)
library(logger)
library(config)
library(odbc)


if(Sys.info()['sysname'] == 'Windows') Sys.setenv(R_CONFIG_ACTIVE = "test")

#REMOVE THIS LATER
#Sys.setenv(R_CONFIG_ACTIVE = "test")

config <- config::get()

source('lib/database_operations.R')
source('lib/utils.R')
#source('lib/custom_components.R')

# if(config$is_test){
#   conn <- get_sqlite_connection(config$alerts_db)
# }else{
#   conn <- get_db_connection()
# }
 
# Credentials data
credentials <- get_credentials_from_auth_db()



claim_refs <- get_claim_refs()
print(head(claim_refs))

claim_refs <- claim_refs[1:10]
# 
# OPTS <- sapply(1:10, function(i) paste0(sample(letters, 9), collapse=""))