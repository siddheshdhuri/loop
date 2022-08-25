ui <- f7Page(
  
  tags$head(tags$script(HTML("$(document).on('click', '.dynamic_id_button', function () {
                                Shiny.onInputChange('last_button',this.id);
                             });")),
            ),
  
  # tags$head(tags$script(src="autocomplete-binding.js")
  # ),
  # 
  # tags$head(
  #   tags$link(rel = "stylesheet", type = "text/css", href = "autocomplete.css")
  # ),
  
  tags$head(tags$style(
    HTML(".shiny-notification {position:fixed;top: 30%;left: 0%;right: 0%;}"))),
  
  
  
  
  title = "Insert a tab Before the target",
  icon = f7Icon("airplane"),
  
  options = list(dark = TRUE),
  
  #f7Login(id = "loginPage", title = "Get in the Loop"),
  
  f7TabLayout(
    panels = tagList(
      f7Panel(title = "Left Panel", side = "left", theme = "light", f7Button(inputId = "stayside", label = "Please StaySide"), effect = "cover"),
      f7Panel(title = "Right Panel", side = "right", theme = "dark", "Blabla", effect = "cover")
    ),

    navbar = f7Navbar(
      title = HTML('<img src="https://upload.wikimedia.org/wikipedia/commons/0/0f/Loop_logo_-_Terracycle.png" height="95px;"></img>'),
      hairline = TRUE,
      shadow = TRUE,
      leftPanel = TRUE,
      rightPanel = TRUE,
      transparent = TRUE
    ),

    f7Tabs(
      animated = TRUE,
      id = "tabs",
      
      f7Tab(
        tabName = "Create Alert",
        icon = f7Icon("exclamationmark_bubble_fill"),
        uiOutput("current_user"),
        
        #autocomplete_input("auto1", "Unnamed:", OPTS, create = TRUE),
        
        #f7Text(inputId = "claim_number", label="", placeholder = "Enter Claims Number"),
        # selectizeInput(inputId = "claim_number", label="", 
        #                choices = claim_refs$claim_number[1:100], 
        #                multiple = FALSE,  options = list(create = TRUE, maxItems = 100),
        #                width = '100%'
        #                ),
        f7AutoComplete(
          inputId = "claim_number",
          placeholder = "Start Typing a Claim Reference",
          #dropdownPlaceholderText = "Try to type Apple",
          label = "Claim Number:",
          openIn = "dropdown",
          choices = claim_refs$claim_number[1:3000]
        ),
        f7Button(inputId = "get_claim_details", label = "Get Claim Details"),

        
        uiOutput("claims_details_block"),
        
      ),


    f7Tab(
      tabName = "Assign Alert",
      icon = f7Icon("tag_fill"),

      uiOutput("alerts_to_be_assigned")
    ),
    
    f7Tab(
      tabName = "Active Alerts",
      icon = f7Icon("asterisk_circle_fill"),
      
      uiOutput("active_alerts")
    ),

    f7Tab(
      tabName = "Analysis",
      icon = f7Icon("chart_bar"),
      
     uiOutput("alerts_analysis")
      
    ),
    
    f7Tab(
      tabName = "Alerts Table",
      icon = f7Icon("table"),
      uiOutput("alerts_datatable"),
      actionButton("refresh_datatable", "", icon=icon("refresh")),
      tags$br(),
      f7DownloadButton("export_alerts_data", "Export Alerts Data"),
      tags$br(),
      # HTML("REFRESH DATABASE: "),
      # textOutput("claims_count"),
      actionButton("refresh_claims_data", "Refresh Data"),
    )

    )
  ) # end tablayout
) # end page


#ui <- shinymanager::secure_app(ui, enable_admin = TRUE)
