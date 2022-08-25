
server = function(input, output, session) {
  
  reactive.values <- reactiveValues()
  
  reactive.values$username <- NULL 
  reactive.values$authorised <- FALSE
  reactive.values$claim_details <- NULL
  reactive.values$alerts <- NULL
  reactive.values$authorised <- TRUE
  print("ASDASDADADS ADASDADAS ASDASD")
  loginData <- callModule(f7LoginServer, id = "loginPage")
  
  observeEvent(input[['loginPage-login']], {
      print('INSIDE LOGIN PAGE')
      username <- loginData$user()
      password <- loginData$password()
      
      reactive.values$authorised <- shinymanager::check_credentials(credentials)(username, password)$result
      
      reactive.values$username <- username
      
      if(reactive.values$authorised) {
        
        output$current_user <- renderUI({HTML(glue::glue("      Logged in as : <b>{username}</b>"))})
        
      }else{
        session$reload()
      }
      
  })
  
  
  observeEvent(input$get_claim_details, {
    browser()
    req(reactive.values$authorised)
    
    claim_details <- withProgress({
                        get_claim_details_for_claim_ref(input$claim_number, config$is_test)
                      },message = "Fetching Claims details"
                      )
    
    claim_details_block <- get_claim_details_block(claim_details)
    
    output$claims_details_block <- renderUI({
                                      claim_details_block
                                    })
    
  })
  
  
  observeEvent(input$submit_alert, {
    
    req(reactive.values$authorised)

    # Validate mandatory fields
       
    f7ValidateInput(inputId = "alert_raised", info = "Alert Raised is mandatory.")
    f7ValidateInput(inputId = "alert_description", info = "Alert Description is mandatory.")
    
    if (is_null_or_empty(input$alert_raised) | is_null_or_empty(input$alert_description)) {
      
      f7Dialog(
        title = "Validation Error",
        text = glue::glue("Please select the Alert Raised and enter Alert Description!")
      )
      
      return()
    }
     
      

 
    #' Get alert details from user input
    
    alert_id = generate_alert_id(input$claim_number)
    
    alert_details_for_claim_ref = data.frame(alert_id = alert_id,
                                             claim_number = input$claim_number,
                                             policy_number = input$policy_number,
                                             last_policy_uw = input$last_policy_uw,
                                             cause_code = input$claim_cause,
                                             claim_country = input$claim_country,
                                             risk_address = input$risk_address,
                                             type_of_claim = input$type_of_claim,
                                             line_of_business = input$line_of_business,
                                             creator = reactive.values$username,
                                             coordinator = unlist(config$uw_coordinators[input$claim_country]),
                                             uw = "",
                                             status = config$to_be_assigned,
                                             alert_raised = input$alert_raised ,
                                             alert_description = input$alert_description,
                                             alert_created_date = format(Sys.time(), "%Y-%m-%d %H:%M"),
                                             alert_assigned_date = "",
                                             alert_completed_date = "",
                                             alert_action = "",
                                             action_description =""
                                             )
    
    
    
    #' save alert to database
    save_alert_to_database(alert_details_for_claim_ref)
    
    
    output$claims_details_block <- renderUI(NULL)
    
    f7Dialog(
      title = "Alert Submitted",
      text = glue::glue(config$create_alert_popup_message)
    )

    # Send alert creation to the underwriter coordinator
    send_email(from_email = config$from_email, 
               to_recepients = unlist(config$uw_coordinators[input$claim_country]), 
               mail_subject = glue(("EU Feedback Loop | Claim {input$claim_number} ")), 
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_created_email_to_coordinator), collapse = " "))))
    
    # Send alert confirmation to the claim underwriter ( the alert creator)
    send_email(from_email = config$from_email, 
               to_recepients = reactive.values$username, 
               mail_subject = glue("EU Feedback Loop | Claim {input$claim_number} created"), 
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_created_email_to_creator), collapse = " "))))
    
    
  })
  
  
  
  #'Observer for assing buttons on alerts.
  observe({
    
    last_button <- input$last_button
    
    if (is.null(last_button)) return()
    
    if(! startsWith(last_button, config$assign_button_prefix) ) return()
    
    alert_id <- gsub(config$assign_button_prefix, '', last_button)
    
    selected_cuw <- isolate(input[[glue("{config$select_uw_prefix}{alert_id}")]])
    
    #'update alert record in database
    update_alert_for_alertid(alert_id, 
                             c('uw'=selected_cuw, 
                               'status'=config$active, 
                               'alert_assigned_date'=format(Sys.time(), "%Y-%m-%d %H:%M")))
    
    reactive.values$alerts <- read_alerts_for_user(isolate(reactive.values$username), status = config$to_be_assigned, role = config$coordinator_role)
    
    # Send alert confirmation to the assigned underwriter
    send_email(from_email = config$from_email, 
               to_recepients = selected_cuw, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id}"), 
               get_html_message_body(glue(paste(readLines(config$alert_assigned_email_to_uw), collapse=" "))))
    
    #' Send email to claims underwriter about assignment
    alert_info <- get_info_for_alertid(alert_id, "creator")
    send_email(from_email = config$from_email,
               to_recepients = alert_info$creator,
               mail_subject = glue("EU Feedback Loop | Claim {alert_id}"),
               get_html_message_body(glue(paste(readLines(config$alert_assigned_email_to_creator), collapse=" "))))
    
  })
  
  
  output$alerts_to_be_assigned <- shiny::renderUI({
    
    #get current user
    username <- reactive.values$username
    
    role <- get_user_role(username)
    
    if(is.null(username)) return()
    
    input$last_button
    input$submit_alert
    
    #get alerts created by current user
    alerts <- read_alerts_for_user(username, status = config$to_be_assigned, role=config$creator_role)
    
    # If user is coordinator get alerts where this user is coordinator
    if( role == config$coordinator_role ){
      alerts_to_assign <- read_alerts_for_user(username, status = config$to_be_assigned, role=config$coordinator_role)
      alerts <- rbind(alerts, alerts_to_assign)
    }
    
    
      
    alerts <- alerts %>% distinct(alert_id, .keep_all= TRUE) %>% arrange(desc(alert_created_date))
    
    #get accordion component of alerts
    if(is.null(alerts)) return()
    
    if(nrow(alerts) < 1){
      acc <- glue("No Alerts Found for user {username}")
    }else{
      
      acc <- get_alerts_accordion(alerts, role)
    }
    
    return(acc)
    
  })
  
  
  #'Observer for the complete button to close the alert.
  observe({
    
    last_button <- input$last_button
    
    if (is.null(last_button)) return()
    
    
    if(! startsWith(last_button, config$action_button_prefix) ) return()
    
    alert_id <- gsub(config$action_button_prefix, '', last_button)
    
    selected_action <- isolate(input[[glue("{config$action_radio_prefix}{alert_id}")]])
    
    action_desc <- isolate(input[[glue("{config$action_text_area_prefix}{alert_id}")]])
    
    #'update alert record in database
    update_alert_for_alertid(alert_id, 
                             c(alert_action = selected_action,
                               action_description = action_desc,
                               'status'=config$completed, 
                               'alert_completed_date'=format(Sys.time(), "%Y-%m-%d %H:%M")))
    
    #get current user
    username <- isolate(reactive.values$username)
    # get alerts for current user
    reactive.values$alerts <- read_alerts_for_user(username=username, role = 'uw', status = config$active)
    
    
    
    #' Get the creator of the alert
    alert_info = get_info_for_alertid(alert_id, c('creator', 'coordinator', 'uw'))
    
    alert_raised = get_info_for_alertid(alert_id, 'alert_raised')
    
    alert_description = get_info_for_alertid(alert_id, 'alert_description')
    
    #' Send email notifying the completion of the alert to the creator
    from_email = config$from_email
    
    
    # Send alert confirmation to the assigned underwriter
    send_email(from_email = config$from_email, 
               to_recepients = alert_info$creator, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id} Completed"), 
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_completed_email_to_creator), collapse=" "))))
    
    # Send alert confirmation to the coordinator
    send_email(from_email = config$from_email, 
               to_recepients = alert_info$creator, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id} Completed"),
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_completed_email_to_coordinator), collapse=" "))))
    
    
    
  })
  
  
  
  #'Observer for the forward alert button to forward the alert to another underwriter
  observe({
    
    last_button <- input$last_button
    
    if (is.null(last_button)) return()
    
    
    if(! startsWith(last_button, config$forward_uw_button_prefix) ) return()
    
    alert_id <- gsub(config$forward_uw_button_prefix, '', last_button)
    
    selected_action <- isolate(input[[glue("{config$forward_uw_button_prefix}{alert_id}")]])
    
    selected_uw_to_forward <- isolate(input[[glue("{config$forward_uw_select_prefix}{alert_id}")]])
    
    #'update alert record in database
    update_alert_for_alertid(alert_id, 
                             c(uw = selected_uw_to_forward
                               #,'alert_assigned_date'=format(Sys.time(), "%Y-%m-%d %H:%M") #@TODO: do we update assigned date to current date ?
                               )
                             )
    
    #get current user
    username <- isolate(reactive.values$username)
    # get alerts for current user
    reactive.values$alerts <- read_alerts_for_user(username=username, role = 'uw', status = config$active)
    
    #' Get the creator of the alert
    alert_info = get_info_for_alertid(alert_id, col=c('creator', 'coordinator', 'uw'))
    
    #' Send email notifying the completion of the alert to the creator
    from_email = config$from_email
    
    
    # Send alert confirmation to the assigned underwriter
    send_email(from_email = config$from_email, 
               to_recepients = alert_info$creator, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id} Forwarded"), 
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_forwarded_email_to_creator), collapse=" "))))
    
    # Send alert confirmation to the coordinator
    send_email(from_email = config$from_email, 
               to_recepients = alert_info$coordinator, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id} Forwarded"),
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_forwarded_email_to_coordinator), collapse=" "))))
    
    # Send alert confirmation to the coordinator
    send_email(from_email = config$from_email, 
               to_recepients = alert_info$uw, 
               mail_subject = glue("EU Feedback Loop | Claim {alert_id} Forwarded"),
               mail_body = get_html_message_body(glue(paste(readLines(config$alert_forwarded_email_to_uw), collapse=" "))))
    
    
  })
  
  
  
  
  
  output$active_alerts <- shiny::renderUI({
    
    last_button <- input$last_button
    
    #get current user
    username <- reactive.values$username
    
    if(is.null(username)) return()
    
    role <- get_user_role(username)
    
    #get alerts for current user
    alerts <- read_alerts_for_user(username=username, role = role, status = config$active)
    
    if(is.null(alerts)) return()
    
    #get accordion component of alerts
    if(nrow(alerts) < 1){
      acc <- glue("No Active alerts found for user {username}")
    }else{
      acc <- get_alerts_accordion(alerts, role)
      reactive.values$alerts <- alerts
    }
    
    return(acc)
    
  })
  
  
  
  output$alerts_analysis<- shiny::renderUI({
    
    #get current user
    username <- isolate(reactive.values$username)
    
    #get alerts for current user
    #alerts <- read_alerts_for_user(username=username, role = 'uw', status = config$active)
    alerts <- read_alerts_table(config$alerts_table)
    
    #get accordion component of alerts
    x <-f7Page( f7Row(
      f7Col(
        
        fig <- plot_ly(
          labels = names(table(alerts$claim_country)),
          values = table(alerts$claim_country),
          name = "Countries",
          type = "pie"
        )
        
      ),
      f7Col(
        
        fig <- plot_ly(
          x = names(table(alerts$status)),
          y = table(alerts$status),
          name = "Status",
          type = "bar"
        )
        
      )
      ),
      
      f7Row(
        
        f7Col(
          fig <- plot_ly(
            labels = names(table(alerts$creator)),
            values = table(alerts$creator),
            name = "Created By",
            type = "pie"
          )
          
        ),
        
        f7Col(
          
          fig <- plot_ly(
            labels = names(table(alerts$uw)),
            values = table(alerts$uw),
            name = "Underwriter",
            type = "pie"
          )
          
        )
        
      )
      
    )
    
    return(x)
    
  })
  
  
  output$alerts_datatable <- shiny::renderUI({
    browser()
    input$refresh_claims_data
    
    alerts <- read_alerts_table(config$alerts_table)
    
    f7Table(alerts[,c(config$columns_to_show)])
    
  })
  
  # Downloadable csv of selected dataset ----
  
  output$export_alerts_data = downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      alerts <- read_alerts_table(config$alerts_table)
      write.csv(alerts, file, row.names = FALSE)
    }
  )
  
  
  # observeEvent(input$refresh_claims_data, {
  # 
  #   n <- withProgress({
  #     refresh_local_database()
  #   },message = "Fetching Claims details"
  #   )
  # 
  #   output$claims_count <- renderText(glue("Refreshed Database {n} Claims "))
  # 
  # })
  
  # observe({
  # 
  #   if(is_null_or_empty(input$claim_number)) return()
  #   if(nchar(input$claim_number) < 3) return()
  # 
  #   print(input$claim_number)
  # 
  #   claim_refs <- claim_refs %>% filter(str_detect(claim_number, glue("^{input$claim_number}")))
  # 
  #   updateSelectizeInput(session, inputId = "claim_number", label="", choices = claim_refs$claim_number)
  # 
  # })
  
}