library(odbc)
library(DBI)
library(RSQLite)


get_db_connection <- function() {
  
  conn <- NULL
  
  tryCatch({
    if(Sys.info()['sysname'] == 'Windows'){
      
      conn <- dbConnect(odbc::odbc(),
                       Driver="SQL Server",
                       Server="prod-claimsmi-db.hiscox.com",
                       Database="ClaimsMI_Analysis",
                       Trusted_Connection="yes",
                       timeout = 60,
                       port=1433
      )
      
    } else if (grepl('azure', Sys.info()['release'])){
      
      conn <- get_sqlite_connection(config$alerts_db)
      
    } else{
      
      conn <- dbConnect(odbc::odbc(),
                       Driver="/var/sqlserver/bin/lib/libsqlserverodbc_sb64.so",
                       Server="prod-claimsmi-db.hiscox.com",
                       Database="ClaimsMI_Analysis",
                       Trusted_Connection="yes",
                       timeout = 60,
                       port=1433
      )
    }
  },error=function(e){
    logger::log_info("Error in connecting to the SQL Server database {e}")
  })

  if(is.null(conn)) conn <- get_sqlite_connection(config$alerts_db)
  
  return(conn)
  
}


get_claim_details_for_claim_ref <- function(claim_ref, is_test=FALSE) {
  
  if (is_test | grepl('azure', Sys.info()['release'])){
    
    conn <-get_sqlite_connection(config$alerts_db)
    
    claim_details <- dbGetQuery(conn, glue::glue("select * from claims_table where claim_number = '{claim_ref}'"))
    
    dbDisconnect(conn)
    
  }else{
    claim_details <- dbGetQuery(conn, 
                                glue::glue("select
                        policy_number = cm.PolicyReference,
                        claim_number = cm.OriginalClaimRef,
                        risk_address = cm.FullRiskAddress,
                        cause_code = cm.ClaimCauseGroupLevel1Name,
                        type_of_claim = case when cm.BusinessDivisionNameEU = 'Art and Private Clients' then 'APC'
                        when cm.BusinessDivisionNameEU = 'Professional Indemnity and Speciality Commercial' then 'PSC'
                        else cm.BusinessDivisionNameEU end,
                        line_of_business = cm.LineOfBusinessName,
                        claim_country = case when SalesUnitGroupLevel2Name = 'Iberia' then
                        case when SalesUnitGroupLevel1Name like 'Madrid%' then 'Spain'
                        when SalesUnitGroupLevel1Name like 'Lisbon%' then 'Portugal' end
                        when SalesUnitGroupLevel2Name = 'Benelux' then
                        case when SalesUnitGroupLevel1Name like 'Amsterdam%' then 'Netherlands'
                        when SalesUnitGroupLevel1Name like 'Brussels%' then 'Belgium' end
                        when SalesUnitGroupLevel2Name = 'Hiscox Special Risks Europe' then 'SR Europe'
                        when SalesUnitGroupLevel2Name = 'Underwriting Partnerships' then 'UWP'
                        else SalesUnitGroupLevel2Name end
                        from ClaimsMI_Analysis.dbo.Insight_All_CurrentMonth cm 
                        where cm.OriginalClaimRef='{claim_ref}'")
    )
  }
  
  return(claim_details)
  
}


# @Mai - Save an alert to database

get_sqlite_connection <- function(dbName){
  tryCatch(conn <- dbConnect(RSQLite::SQLite(), dbName ),error=function(e){
    logger::log_error("Error in connecting to the SQL Server database {e}")
    })
  return(conn)
}

save_alert_to_database <- function(claim_details) {
  
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  
  logger::log_info("Trying to write data to the table:")
  
  # Write to table
  tryCatch(
    dbWriteTable(conn, config$alerts_table, claim_details, append=TRUE, overwrite=FALSE),
    error=function(e){
    logger::log_error("Error in writing data to the table {e} Claims Details {claim_details} ")
    }, finally = {
      dbDisconnect(conn)
    })
  
  logger::log_info('Writing...Done! Close database connection.')
  
}


load_alert_from_database <- function(claim_ref){
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  
  # Construct the fetching query
  query <- glue::glue("SELECT * FROM {config$alerts_table} WHERE claim_number = {claim_ref} ")
  
  # Write to table
  claim_details <- dbGetQuery(conn, query)
  
  # Disconnect from database
  dbDisconnect(conn)
  
  claim_details
}


#' Funtion to read alerts from database
#' 
#' @param table name of the alers table
#' 
#' @return alerts dataframe with all alerts.
read_alerts_table <- function(table){
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  
  alerts <- NULL
  tryCatch({
    alerts <- dbReadTable(conn, table)
  },error=function(e){
    logger::log_info("Error in while fetching alerts from database : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  return(alerts)
  
  
}

#' Funtion to read alerts from database
#' 
#' @param table name of the alers table
#' @param role userrole which can be creator/coordinator/uw
#' @param status status of the alert
#' 
#' @return alerts filtered for user and alert status
read_alerts_for_user <- function(username, table="alerts_table", role="creator", status=config$to_be_assigned){
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  alerts <- NULL
  tryCatch({
    
    alerts <- dbGetQuery(conn, glue::glue("SELECT * FROM {table} WHERE {role} = '{username}' AND status = '{status}'"))
  },error=function(e){
    logger::log_info("Error in while fetching alerts from database for user {username} : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  return(alerts)
  
}

#' Funtion to update alert for a given alert id 
#' 
#' @param alert_id id of the alert to update
#' @param updates_dict a named list / dict of field value pairs to be updated.
#' 
update_alert_for_alertid <- function(alert_id, updates_dict){
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  
  tryCatch({
    
    updates = paste(names(updates_dict), glue("'{unlist(updates_dict, use.names = F)}'"), sep="=", collapse = ",")
    logger::log_info("UPDATE {config$alerts_table} SET {updates} WHERE alert_id = '{alert_id}' ")
    dbExecute(conn, glue::glue("UPDATE {config$alerts_table} SET {updates} WHERE alert_id = '{alert_id}' "))
    
  },error=function(e){
    logger::log_error("Error in while updating alerts table {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
}


#' Funtion to get a given info (column) for a given alert id 
#' 
#' @param alert_id id of the alert to update
#' @param col the name of the column want to extract, e.g. creator, status, uw, coordinator...
#' 
get_info_for_alertid <- function(alert_id, col){
  
  # combine multiple columns into one variable
  col <- paste(col, collapse = ",")
  
  # Connect to the database
  conn <- get_sqlite_connection(config$alerts_db)
  
  tryCatch({
    logger::log_info("SELECT {col} FROM {config$alerts_table} WHERE alert_id = '{alert_id}' ")
    info = dbGetQuery(conn, glue::glue("SELECT {col} FROM {config$alerts_table} WHERE alert_id = '{alert_id}'"))
    
    
    
  },error=function(e){
    logger::log_info("Error in while retreiving the alert information {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  return(info)
}


#' Funtion to refresh data 
#' 
#' @param database_name 
#' @param table_name 
#'
#' @return TRUE if successful else FALSE
refresh_local_database <- function(){
  
  query <- "SELECT 
              policy_number = cm.PolicyReference,
              claim_number = cm.OriginalClaimRef,
              risk_address = cm.FullRiskAddress,
              cause_code = cm.ClaimCauseGroupLevel1Name,
              type_of_claim = case when cm.BusinessDivisionNameEU = 'Art and Private Clients' then 'APC'
              when cm.BusinessDivisionNameEU = 'Professional Indemnity and Speciality Commercial' then 'PSC'
              else cm.BusinessDivisionNameEU end,
              line_of_business = cm.LineOfBusinessName,
              claim_country = case when SalesUnitGroupLevel2Name = 'Iberia' then
              case when SalesUnitGroupLevel1Name like 'Madrid%' then 'Spain'
              when SalesUnitGroupLevel1Name like 'Lisbon%' then 'Portugal' end
              when SalesUnitGroupLevel2Name = 'Benelux' then
              case when SalesUnitGroupLevel1Name like 'Amsterdam%' then 'Netherlands'
              when SalesUnitGroupLevel1Name like 'Brussels%' then 'Belgium' end
              when SalesUnitGroupLevel2Name = 'Hiscox Special Risks Europe' then 'SR Europe'
              when SalesUnitGroupLevel2Name = 'Underwriting Partnerships' then 'UWP'
              else SalesUnitGroupLevel2Name end
          from ClaimsMI_Analysis.dbo.Insight_All_CurrentMonth cm
          where SalesUnitBusinessUnitName = 'Europe'
          and AsAtClaimStatusGroupName <> 'Closed'
          "
  
  conn <- get_db_connection()
  
  claims_data <- NULL
  
  tryCatch({
    claims_data <- DBI::dbGetQuery(conn, query)
  },error=function(e){
    logger::log_info("Error in while fetching claims from database : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  if(is.null(claims_data)) return(FALSE)
  
  conn <- get_sqlite_connection(config$alerts_db)
  success <- FALSE
  tryCatch({
    DBI::dbWriteTable(conn, "claims_table", claims_data, overwrite=TRUE)
    success <- TRUE
  },error=function(e){
    logger::log_info("Error in while writing claims data to local db : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  if(success)  return(nrow(claims_data))
  
  return(success)
  
}


get_credentials_from_auth_db <- function(){
  
  conn <- get_sqlite_connection(config$auth_db)
  creds <- NULL
  tryCatch({
    creds <- DBI::dbReadTable(conn, "credentials")
  },error=function(e){
    logger::log_info("Error in while writing claims data to local db : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  return(creds)
  
}

get_claim_refs <- function(){
  
  conn <- get_sqlite_connection(config$alerts_db)
  claim_refs <- NULL
  tryCatch({
    claim_refs <- DBI::dbGetQuery(conn, "select distinct claim_number from claims_table")
  },error=function(e){
    logger::log_info("Error in while getting claim refs from claims data : {e}")
  }, finally = {
    dbDisconnect(conn)
  })
  
  return(claim_refs)
}


