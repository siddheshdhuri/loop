# Helper funtions
get_accordion_footer_badge <- function(status, created_date, assigned_date) {
  
  if(status == config$to_be_assigned) {
    days_diff = as.integer(difftime(Sys.Date(), created_date, units = "days"))
    if(days_diff < (config$coordinator_sla-2)){
      badge_text = glue::glue("{days_diff} days since created")
      badge_color= "green"
    }else if(days_diff <= config$coordinator_sla){
      badge_text = glue::glue("Due Soon: {days_diff} days since created")
      badge_color= "orange"
    }else {
      badge_text = glue::glue("Overdue: {days_diff} days since created")
      badge_color= "red"
    }
  }else if(status == config$active){
    days_diff = as.integer(difftime(Sys.Date(), assigned_date, units = "days"))
    if(days_diff < (config$uw_sla-2)){
      badge_text = glue::glue("{days_diff} since alert created")
      badge_color= "green"
    }else if(days_diff <= config$coordinator_sla){
      badge_text = glue::glue("Due Soon: {days_diff} days since created")
      badge_color= "orange"
    }else {
      badge_text = glue::glue("Overdue: {days_diff} days since created")
      badge_color= "red"
    }
  }
  
  return(f7Badge(badge_text, color = badge_color))
  
}


get_accordion_footer <-  function(alert_id, status, country, role, created_date, assigned_date){
  footer = NULL
  footer_badge = get_accordion_footer_badge(status, created_date, assigned_date)
  
  if(status == config$to_be_assigned & role == config$coordinator_role){
    
    
    footer = tagList(
      f7Select(glue("{config$select_uw_prefix}{alert_id}"),
               label = 'Underwriter',
               choices = config$uw_list[[country]],
      ),
      tags$br(),
      actionButton(label = "Assign",
                   inputId = glue::glue("{config$assign_button_prefix}{alert_id}"),
                   class="dynamic_id_button",
                   width = '25%'),
      footer_badge, 
    )
  }else if(status == config$active & role == config$uw_role){
    footer = tagList(
      
      # tags$p(f7Radio(inputId = glue::glue("{config$action_radio_prefix}{alert_id}"),
      #                label = config$action_radio_label,
      #                choices = config$action_radio_choices)),
      # 
      # tags$p(f7TextArea(inputId = glue::glue("{config$action_text_area_prefix}{alert_id}"),
      #                   label="Description of why that action needs to be taken:")),
      # 
      # actionButton(label = "Complete",
      #              inputId = glue::glue("{config$action_button_prefix}{alert_id}"),
      #              class="dynamic_id_button",
      #              width = '25%'),
      # footer_badge
      
      tags$div( style = "width: 100%;", 
                HTML("<b> Take Action </b>"),
                f7Row(
                  tags$p(f7Radio(inputId = glue::glue("{config$action_radio_prefix}{alert_id}"),
                                 label = config$action_radio_label,
                                 choices = config$action_radio_choices)),
                  
                  tags$p(f7TextArea(inputId = glue::glue("{config$action_text_area_prefix}{alert_id}"),
                                    label="Description of why that action needs to be taken:")),
                  
                  actionButton(label = "Complete",
                               inputId = glue::glue("{config$action_button_prefix}{alert_id}"),
                               class="dynamic_id_button",
                               width = '25%'),
                  footer_badge
                ),
                HTML("<b> OR Forward Alert: </b>"),
                f7Row(
                  f7Select(glue("{config$forward_uw_select_prefix}{alert_id}"),
                           label = 'Select Underwriter to forward to:',
                           choices = config$uw_list[[country]],
                  ),
                  
                  actionButton(label = "Forward",
                               inputId = glue::glue("{config$forward_uw_button_prefix}{alert_id}"),
                               class="dynamic_id_button",
                               width = '25%')
                )
      )
      
      
    )
  }else if(status == config$completed){

  }else{
    footer_badge
  }

}


get_claim_details_block <- function(claim_details){
  
  claims_details_block <- HTML("<b>Claim not found. Please check if you've enterred correct claim ref. Only Active European claims are available for creating alerts.</b>")
  
  if(nrow(claim_details) > 0){
    
    claims_details_block <- f7Accordion(
                              id = "claim_details_accordion",
                              f7AccordionItem(
                                title = "Claim Details",
                                
                                f7Block(f7Text(inputId = 'claim_number', label = 'Claim Reference', value = claim_details$claim_number),
                                        f7Text(inputId = 'policy_number', label = 'Policy Reference', value = claim_details$policy_number),
                                        f7AutoComplete(inputId = 'last_policy_uw', label= 'Last UW to work on policy', 
                                                       choices = c("Unknown", "Not in List", config$uw_list[[claim_details$claim_country]]), 
                                                       placeholder = "If known, add underwriter that touched the policy last",
                                                       openIn = "dropdown",
                                                       value = "Unknown"
                                                       ),
                                        f7Text(inputId = 'claim_cause', label = 'Claim Cause Code from CMS', value = claim_details$cause_code),
                                        f7Text(inputId = 'claim_country', label = 'Claim Country', value = claim_details$claim_country),
                                        f7Text(inputId = 'risk_address', label = 'Risk Address', value = stringr::str_replace_all(claim_details$risk_address, "[[:punct:]]", " ")),
                                        f7Text(inputId = 'type_of_claim', label = 'Type of Claim', value = claim_details$type_of_claim),
                                        f7Text(inputId = 'line_of_business', label = 'Line of Business', value = claim_details$line_of_business),
                                        f7Radio(inputId = 'alert_raised', label='Alert Raised', choices = config$alerts_raised),
                                        f7TextArea(inputId = 'alert_description', label="Add a short description of the alert (DON'T add sensitive information)"),
                                        
                                        f7Button(inputId = "submit_alert", label = "Submit Alert")
                                ),
                                
                                open = TRUE
                              ),
                              
                            )
    
  }
  
  return(claims_details_block)
  
}


get_alerts_accordion <- function(alerts_list, role){
  
  accordion_items_list <- list()
  
  if(nrow(alerts_list) < 1) return()
  
  for(i in 1:nrow(alerts_list)) {
    
    alert_id <- alerts_list[i, 'alert_id']
    claim_country <- alerts_list[i, 'claim_country']
    
    accordion_item <- f7AccordionItem(
      title =glue("Alert: {alert_id} raised by {alerts_list[i, 'creator']}"), 
      open = FALSE,
      f7Card(
        #title = f7checkBox(glue::glue("test{i}"), label = "select to assign"),
        tagList(
          tags$p(tags$b('Policy Number: '), alerts_list[i, 'policy_number']),
          tags$p(tags$b('Last UW to work on policy: '), alerts_list[i, 'last_policy_uw']),
          tags$p(tags$b('Claim Number'), alerts_list[i, 'claim_number']),
          tags$p(tags$b('Cause Code'), alerts_list[i, 'cause_code']),
          tags$p(tags$b('Type of Claim'), alerts_list[i, 'type_of_claim']),
          tags$p(tags$b('Line of Business'), alerts_list[i, 'line_of_business']),
          tags$p(tags$b('Country'), alerts_list[i, 'claim_country']),
          tags$p(tags$b('Risk Address'), alerts_list[i, 'risk_address']),
          tags$p(tags$b('Description'), alerts_list[i, 'alert_description']),
          tags$p(tags$b('Alert Raised'), alerts_list[i, 'alert_raised']),
          tags$p(tags$b('Cooridnator'), alerts_list[i, 'coordinator'])
        ),
        footer = get_accordion_footer(alert_id, alerts_list[i, 'status'], alerts_list[i, 'claim_country'], role,
                                      alerts_list[i, 'alert_created_date'],
                                      alerts_list[i, 'alert_assigned_date']
                                      )
        
      )
    )
    
    accordion_items_list[[i]] <- accordion_item
    
  }
  
  acc <- f7Accordion(id = "alerts_accordion", accordion_items_list)
  
  return(acc)
  
}

generate_alert_id <- function(claim_number){
  #@TODO Mai to add functionality
  return(paste(claim_number,format(Sys.time(), '%d%m%Y-%H%M%S'),sep="-"))
  
}

## Package sendmailR
send_email <- function(from_email, to_recepients, mail_subject, mail_body){
  
  to_recepients = ifelse(grepl("@hiscox.com", to_recepients, ignore.case = T), to_recepients, paste0(to_recepients, "@hiscox.com"))
  
  print(glue::glue('Start sending email from: {from_email} to :{to_recepients} '))
  
  sendmailR::sendmail(from = from_email,
           to = to_recepients,
           subject= mail_subject,
           msg = mail_body,
           control = list(smtpServer="mail.hiscox.com")
           )

}


is_null_or_empty = function(object){
  if(is.null(object)) return(TRUE)
  
  if(trim(object) == "") return(TRUE)
  
  if(length(object) < 1) return(TRUE)
  
  else return(FALSE)
}


Sys.chown <- function(paths, owner = NULL, group = NULL) {
  
  # create string for user:owner
  if (!is.null(group)) {
    og <- paste0(owner, ":", group)
  } else {
    og <- owner
  }
  
  # create string with files
  files <- paste(paths, collapse = " ")
  
  # run command
  system(paste("chown", og, files))
  
  invisible()
}


get_html_message_body = function(message_body){
  
  
  
  html_message_body =  sendmailR::mime_part(glue::glue('<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0
                                            Strict//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
                                            <html xmlns="http://www.w3.org/1999/xhtml">
                                            <head>
                                              <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
                                              <meta name="viewport" content="width=device-width, initial-scale=1.0"/>
                                              <title>EU Feedback loop</title>
                                              <style type="text/css">
                                              </style>
                                            </head>
                                            <body>
                                           {message_body}
                                            </body>
                                            </html>'))
  
  ## Set content type.
  html_message_body[["headers"]][["Content-Type"]] <- "text/html"
  
  html_message_body <- list(html_message_body)
  
  
  return(html_message_body)
}


get_user_role <- function(username){
  
  if(is.null(username)) return()
  
  if(username %in% unlist(config$uw_list))
    return(config$uw_role)
  
  if(username %in% config$uw_coordinators)
    return(config$coordinator_role)
  
  return(config$creator_role)
  
}


get_recepient_email <- function(receipient){
  return(ifelse(grepl("@hiscox.com", receipient, ignore.case = T), receipient, paste0(receipient, "@hiscox.com")))
}


##########################################################################################
##########################################################################################
##########################################################################################

f7LoginCustom <- function(..., id, title, label = "Sign In", footer = NULL,
                    startOpen = TRUE) {
  
  ns <- shiny::NS(id)
  
  submitBttn <- f7Button(inputId = ns("login"), label = label)
  submitBttn[[2]]$attribs$class <- "item-link list-button f7-action-button"
  submitBttn[[2]]$name <- "button"
  
  shiny::tags$div(
    id = ns(id),
    `data-start-open` = jsonlite::toJSON(startOpen),
    class = "login-screen",
    shiny::tags$div(
      class = "view",
      shiny::tags$div(
        class = "page",
        shiny::tags$div(
          class = "page-content login-screen-content",
          shiny::tags$div(class = "login-screen-title", title),
          
          # inputs
          shiny::tags$form(
            shiny::tags$div(
              class = "list", shiny::tags$ul(
                f7Text(
                  inputId = ns("login_user"),
                  label = "",
                  placeholder = "username@hiscox.com"
                ),
                f7Password(
                  inputId = ns("login_password"),
                  label = "",
                  placeholder = "Your password"
                ),
                ...
              )
            ),
            shiny::tags$div(
              class = "list",
              shiny::tags$ul(shiny::tags$li(submitBttn)),
              if (!is.null(footer)) {
                shiny::tags$div(class = "block-footer", footer)
              }
            )
          )
        )
      )
    )
  )
}


#' Framework7 login server module
#'
#' \code{f7LoginServer} is a useful server elements to fine tune the
#' \link{f7Login} page.
#'
#' @param input Shiny input object.
#' @param output Shiny output object.
#' @param session Shiny session object.
#' @param ignoreInit If TRUE, then, when this observeEvent is first
#' created/initialized, ignore the handlerExpr (the second argument),
#' whether it is otherwise supposed to run or not. The default is FALSE.
#' @param trigger Reactive trigger to toggle the login page state. Useful, when
#' one wants to set up local authentication (for a specific section). See example 2.
#'
#' @export
#' @rdname authentication
f7LoginServerCustom <- function(input, output, session, ignoreInit = FALSE,
                          trigger = NULL) {
  
  ns <- session$ns
  # module id
  modId <- strsplit(ns(""), "-")[[1]][1]
  
  # this is needed if we have local authentication (not on all pages)
  # and the login page is not visible at start.
  # This reactiveVal ensures that we run authentication only once.
  authenticated <- shiny::reactiveVal(FALSE)
  # open the page if not already (in case of local authentication)
  shiny::observeEvent({
    shiny::req(!is.null(trigger))
    trigger()
  }, {
    if (!authenticated()) {
      if (!input[[modId]]) updateF7Login(id = modId)
    }
  }, once = TRUE)
  
  # toggle the login only if not authenticated
  shiny::observeEvent(input$login, {
    if (!authenticated()) {
      updateF7Login(
        id = modId,
        user = input$login_user,
        password = input$login_password
      )
      authenticated(TRUE)
    }
  }, ignoreInit = ignoreInit)
  
  # useful to export the user name outside the module
  return(
    list(
      user = shiny::reactive(input$login_user),
      password = shiny::reactive(input$login_password)
    )
  )
  
}


#' Activates Framework7 login.
#'
#' \code{updateF7Login} toggles a login page.
#'
#' @param id \link{f7Login} unique id.
#' @param user Value of the user input.
#' @param password Value of the password input.
#' @param session Shiny session object.
#' @export
#' @rdname authentication
updateF7LoginCustom <- function(id, user = NULL, password = NULL, session = shiny::getDefaultReactiveDomain()) {
  message <- dropNulls(
    list(
      user = user,
      password = password
    )
  )
  session$sendInputMessage(id, message)
}
