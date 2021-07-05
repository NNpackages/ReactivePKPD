
dashboardPage(
  dashboardHeader(title = tags$img(src='reactive_pkpd_logo.png', height = 38, width = 180) , titleWidth = "350px",
                  #conditionalPanel("input.sideMenu == 'plot_profile'",
                    #dropdownMenuOutput("review_status_ui"),
                  #),
                  #conditionalPanel("input.sideMenu == 'plot_pd_profile'",
                   # dropdownMenuOutput("pd_review_status_ui"),
                  #),
                  dropdownMenuOutput("review_status_ui"),
                  uiOutput("Online_users_UI"),
                  tags$li(plotOutput("Online_users", height = "30px", inline = TRUE), class = "dropdown")),
  dashboardSidebar(
    # Custom CSS to hide the default logout panel
    tags$head(
      # Custom CSS to hide the default logout panel
      tags$style(HTML('.shiny-server-account { display: none; }')),
      # Custom JavaScript -- just to log in when a user hits "enter".
      includeScript("www/sendOnEnter.js"), includeCSS("www/comment.css"),

      tags$script('
                            Shiny.addCustomMessageHandler("unbinding_table_elements", function(x) {
                            Shiny.unbindAll($(document.getElementById(x)).find(".dataTable"));
                            });'
      )
    ), shinyalert::useShinyalert(),shinyjs::useShinyjs(),
    # The dynamically-generated user panel
    uiOutput("userpanel"),
    conditionalPanel("input.sideMenu == 'plot_profile'",
      
      uiOutput("profile_select_ui"),

      uiOutput("review_status_user_ui"),
      uiOutput("pk_profile_buttons"),
      hr(),
      shinyWidgets::prettyRadioButtons(inputId = "show_profiles", label = "Selectable profiles:",
                           choices = c("All" = "All", "Only non-reviewed" = "non-reviewed", "Only with comments" = "with comments", "Only with non-final comments" = "non-final", "Only with awaiting comments" = "Awaiting approval", "Only with re-evaluate comments" = "Re-evaluate"),
                           selected = "All", status = "primary",
                           icon = icon("ok", lib = "glyphicon")),
      uiOutput("profile_select_ui_order"),
      hr(),
      
      tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
      ),
    conditionalPanel("input.sideMenu == 'plot_pd_profile'",
                     
                     uiOutput("pd_profile_select_ui"),
                     
                     uiOutput("pd_review_status_user_ui"),
                     uiOutput("pd_profile_buttons"),
                     hr(),
                     shinyWidgets::prettyRadioButtons(inputId = "pd_show_profiles", label = "Selectable profiles:",
                                                      choices = c("All" = "All", "Only non-reviewed" = "non-reviewed", "Only with comments" = "with comments", "Only with non-final comments" = "non-final", "Only with awaiting comments" = "Awaiting approval", "Only with re-evaluate comments" = "Re-evaluate"),
                                                      selected = "All", status = "primary",
                                                      icon = icon("ok", lib = "glyphicon")),
                     uiOutput("pd_profile_select_ui_order"),
                     hr(),
                     
                     tags$head(tags$style(HTML(".shiny-split-layout > div {overflow: visible;}")))
    ),

    sidebarMenu(id = "sideMenu",
      menuItem("Select trial", tabName = "select_trial", selected = TRUE,  icon = icon("binoculars")),
      menuItem("Setup trial",  tabName = "setup_trial", selected = FALSE, icon = icon("cogs"), expandedName = "setup_trial_exp",
        menuSubItem("Trial info",       tabName = "trial_info"),
        menuSubItem("Upload PK data",     tabName = "load_pk_data"),
        menuSubItem("Upload PD data",     tabName = "load_pd_data"),
        menuSubItem("Setup PD plot",    tabName = "setup_pd_plot")
      ),
      menuItem("Pharmacokinetic profile", tabName = "plot_profile", icon = icon("chart-line")),
      menuItem("Pharmacodynamic profile", tabName = "plot_pd_profile", icon = icon("chart-line")),
      menuItem("Overview", tabName = "Overview", icon = icon("table")),
      menuItem("Minutes", tabName = "minutes", icon = icon("file-word")),
      menuItem("Online user", tabName = "review_overview",selected = FALSE, icon = icon("globe")),
      hr(),
      conditionalPanel(condition = "input.sideMenu == 'plot_profile'",
        uiOutput("pk_commentstimeline"), style = "padding: 20px;"
      ),
      # conditionalPanel(condition = "input.sideMenu == 'plot_pd_profile'",
      #   uiOutput("pd_commentstimeline"), 
      #   shinyWidgets::prettyRadioButtons(inputId = "pd_select_times",
      #                                    label = "Select:",
      #                                    choices  =  c("Time interval", "Points"),
      #                                    selected = "Time interval", inline = TRUE),
      #   style = "padding: 20px;"
      # ),
      conditionalPanel(condition = "input.sideMenu == 'trial_info'",
                       shiny::fileInput(inputId = "new_trial_upload_zip", "Upload PKPD zip file"),
                       verbatimTextOutput("zipupload") %>% shinycssloaders::withSpinner(color = "#0dc5c1",size = 0.5), style = "padding: 5px;"
      )
    ),
    width = "350px"
  ),
  dashboardBody(
    # shinyjs::extendShinyjs(text = "shinyjs.activateTab = function(name){
    #               setTimeout(function(){
    #               $('a[href$=' + '\"#shiny-tab-' + name + '\"' + ']').closest('li').addClass('active')
    #               }, 200);
    #               }"
    # ),
    tabItems(
      tabItem(tabName = "select_trial",
       # uiOutput("select_trial_UI"),
        fluidRow(
          box(solidHeader = TRUE, collapsible = TRUE, width = 12,
              title = tagList("PK/PD reviews - only ongoing", 
              shinyWidgets::prettyCheckbox(inputId = "only_ongoing",
              label = "Yes", value = TRUE, inline = TRUE)), 
          status = "primary",

              DT::dataTableOutput("setup_all_available") %>% shinycssloaders::withSpinner(color = "#0dc5c1",size = 0.5),
              uiOutput("setup_reload_data_ui")
              #actionButton("setup_reload_data", "Update based on globeshare")
          )
        ),
        uiOutput("time_range_x")
    ),

    tabItem(tabName = "trial_info",
      fluidRow(
        box(solidHeader = TRUE, collapsible = TRUE, width = 12,
            title = "Settings of trial", status = "primary",
            h3("Trial info"),
            splitLayout(cellWidths = c("10%","10%","80%"),
                        textInput("new_trial_project", "Project - 4 digits"),
                        textInput("new_trial_trial", "Trial"),
                        textInput("new_trial_title", "Protocol Title")
            ),
            # if (PKPDEnv("data_storage") == "trial_specific") {
            #   tagList(
            #     h3("Paths"),
            #     splitLayout(
            #       pathInput("new_trial_datapath", "Path to original data"),
            #       pathInput("new_trial_appdata", "Path to application data")
            #     )
            #   )
            # }
            h3("Users"),
            splitLayout(
              selectizeInput("new_trial_superusers", "Superusers", choices = list(""), multiple = TRUE,
                             options = list(create = TRUE)),
              selectizeInput("new_trial_users", "Users", choices = list(""), multiple = TRUE,
                             options = list(create = TRUE))
            ),
            h3("Ongoing"),
            checkboxInput("new_trial_ongoing", "ongoing", value = TRUE),
            
            #column(width = 6,
              tags$span(style = "float:right",
                splitLayout(cellWidths = c(150, 150),
                  uiOutput("new_trial_update_ui"),
                  uiOutput("new_trial_save_ui")
                )
              )
            #)
        ),
        
        uiOutput("new_trial_save_success")
      )
           

    ), 
    tabItem(tabName = "load_pk_data",
      fluidRow(
        uiOutput("new_trial_upload_file")
      )
    ),
    tabItem(tabName = "load_pd_data",
            fluidRow(
              uiOutput("new_pd_trial_upload_file")
            )
    ),
    tabItem(tabName = "setup_pd_plot",
            fluidRow(
              uiOutput("setup_pd_plot_ui")
            )
    ),
    tabItem(tabName = "minutes",
            fluidRow(
              uiOutput("minutes_ui")
              )
             
    ),
      # First tab content
      tabItem(tabName = "plot_profile",
              uiOutput("plot_profile_ui")
      ),
      tabItem(tabName = "plot_pd_profile",
            uiOutput("plot_pd_profile_ui")
      ),
      tabItem(tabName = "Overview",
              fluidRow(
                box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                    title = "PK Comments", status = "primary",
                    DT::dataTableOutput("all_trial_comments")
                ),
                box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                    title = "PD Comments", status = "primary",
                    DT::dataTableOutput("pd_all_trial_comments")
                ),
                box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                    title = "Downloads", status = "primary",
                      shiny::downloadButton("download_trial_zip", "Download trial data"),
                      shiny::downloadButton("download_trial_comments", "Download PK comments"),
                      shiny::downloadButton("pd_download_trial_comments", "Download PD comments")
                    )
              )
      ),
      tabItem(tabName = "review_overview",
              fluidRow(
                box(solidHeader = TRUE, collapsible = TRUE, width = 12,
                    title = "Online users", status = "primary",
                    DT::dataTableOutput("review_overview")
                )

              )
      )
    )
  )
)
