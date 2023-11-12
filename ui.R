library(shiny)
library(shinythemes)
library(rhandsontable)

suppressMessages(library(SOMbrero))
library(cluster)
library(rhandsontable)

library(Hmisc)
library(GGally)
library(network)
library(sna)
library(ggplot2)
library(igraph)
library(intergraph)
library(tibble)
library(tidyr)
library(tidyverse)
library(shiny)
library(shinyjs)
library(visNetwork)
library(shinyalert)
library(htmltools)
library(crayon)
library(shinydashboard)
library(zip)
library(rintrojs)
library(fresh)
library(plotly)
library(DT)
library(shinycssloaders)

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#bce7fa"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#FFFFFF",
    dark_hover_bg = "#EEEEEE",
    dark_color = "#000000", 
    dark_hover_color = "#000000", 
    dark_submenu_color = "#000000", 
    dark_submenu_hover_color = "#000000"
  ),
  adminlte_global(
    content_bg = "#FAFAFA",
    box_bg = "#FFFFFF", 
    info_box_bg = "#FFFFFF"
  )
)

options(spinner.type = 1, spinner.color = "#bce7fa", size = 2)

# here is the first option using shiny themes:  
ui <- dashboardPage(
  
  dashboardHeader(
    title = 'COMPLEX-IT 1.0.1 Beta',
    titleWidth = 375),
  
  dashboardSidebar(
    
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu .treeview-menu li.active a {background-color: #FFFFFF !important;}")),
    tags$style(HTML(".main-sidebar .sidebar .sidebar-menu .treeview-menu li:hover a {background-color: #EEEEEE !important;}")),
    
    width = 375,
    
    sidebarMenu(

      HTML('<img src="Complexit_LOGO4.png" style="margin-bottom: 20px;margin-top: 10px; display: block; margin-left: auto; margin-right: auto;">'),
      
      id = 'tabs',
      
      menuItem("Import Your Cases",
               menuSubItem("Import Cases", tabName = "importing")
      ), 
      
      menuItem("Build, Confirm and Explore Your Model",
               menuSubItem("Cluster Your Cases", tabName = "cluster_cases"),
               menuSubItem("Use AI to Confirm Clusters", tabName = "AI_clusters"),
               menuSubItem("Compare and Visualise Your Results", tabName = "compare_and_visualise")
      ), 
      
      menuItem("Run Scenario Simulations",
               menuSubItem("Simulate Your Scenarios, Policies, or Interventions", tabName = "scenarios")
      ), 
      
      menuItem("Data Forecasting",
               menuSubItem("Use AI to Predict the Cluster Membership of New Cases", tabName = "forecasting")
      ), 
      
      menuItem("Systems Mapping",
               menuSubItem("Use Systems Mapping to Explore Cluster Variables", tabName = "systems_mapping")
      ), 
      
      menuItem("Export Your Results",
               menuSubItem("Generate Your Report", tabName = "generate_report")
      ), 
      
      menuItem("Help",
               menuSubItem("Help Using COMPLEX-IT", tabName = "help")
      ) 
    )
  ),
  dashboardBody(
    
    useShinyjs(),
    
    use_theme(mytheme),
    
    tags$head(tags$style(HTML('
.box {margin-top: 2px;margin-left: 0px; margin-right: 0px; margin-bottom:2px;padding:-10px}'
    ))),
    
    tags$head(tags$style(HTML('
        .skin-blue .main-header .navbar .sidebar-toggle {
          color: #000000;
        }
      '))),
    
    tags$head(tags$style(HTML('
        .skin-blue .main-header .logo {
          color: #000000;
        }
      '))),
    
    
    # tags$style("#varchoice ~ .selectize-control .select-input {
    # max-height: 150px;
    # overflow-y: auto;
    #                 }"),
    
    # tags$head(
    #   tags$style(HTML("#trainnotice_advanced_trigger {text-align: center;}"))),
    # 
    # tags$head(
    #   tags$style(HTML("#trainnotice_advanced_info {text-align: center;}"))),
    
    # tags$head(
    #   tags$style(HTML("#kmeans_title {text-align: center;}"))),
    
    tags$style(HTML(".full-width-button { width: 100%; }")),
    
    tags$style("
#varchoice ~ .selectize-control .selectize-input {
  max-height: 100px;
  overflow-y: auto;
}
"),

    introjsUI(),

    tabItems(
      
      ##### IMPORT TAB #####
      tabItem("importing",
              
              tags$h3("STEP 1: IMPORTING YOUR DATABASE", style = "text-align: center;"),
              
              tags$h4(HTML("Here you will upload your data"), style = "text-align: center;"),
              
              
              p(HTML('For TUTORIALS on preparing and importing your data for COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                      target="_blank">CLICK HERE</a>'), style = "text-align: center;"),
              
              p(HTML('Your data must be in the form of a csv file. For more on creating csv files <a href= 
                                                    "https://www.wikihow.com/Create-a-CSV-File"
                                      target="_blank">CLICK HERE</a>'), style = "text-align: center;"),
              
              br(), 
              
              
              fluidRow(
                
                column(3, 
                       
                       box(width = 12, 
                           
                           fileInput('file1', 'Choose CSV File', buttonLabel='Browse',accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                           ),
                           checkboxInput('header', ' Header?', TRUE),
                           selectInput('sep', 'Separator:',
                                       c("Comma","Semicolon","Tab","Space"), 'Comma'),
                           
                           uiOutput("varchoice"),
                           # numericInput('nrow.preview','Number of rows in the preview:',20, min = 1, max = 100),
                           # numericInput('ncol.preview', 'Number of columns in the preview:',
                           #              10,min = 1, max = 100),
                           helpText("Note: Even if the preview only shows a restricted
                                        number of observations, the map will be based on the full dataset.")
                           
                           )
                       
                       
                       
                ), 
                
                column(9, 
                       
                       DTOutput("view")
                       
                )
              )
      ),
      
      ##### CLUSTER CASES TAB #####
      tabItem("cluster_cases",
              
              tags$h3("STEP 2: CLUSTER YOUR CASES USING K-MEANS", style = "text-align: center;"), 
              
              tags$h4(HTML("Here we will use the k-means clustering algorithm to group your cases into self-similar 'clusters'"), style = "text-align: center;"),
              
              br(),
              
              #verbatimTextOutput("kmean_warning"),
              
              fluidRow(
                
                
                column(3, 
                       
                       
                       box(width = 12,
                         
                         actionButton(inputId = "init_kmeans", label="Get Clusters", class = "full-width-button",
                                      style = "foreground-color:white; 
                                                     background-color:darksalmon;
                                                     color:black;
                                                     float:center;
                                                     height: 50px;
                                                     text-align:center;
                                                     border-color:black;
                                                     border-radius: 5px;
                                                     border-width: 5px;
                                                     margin-bottom: 5px;
                                                     margin-top: 5px;"),
                       
                       actionButton("infoButton_kmean", "Info", class = "full-width-button",
                                    style = "margin-bottom: 5px;
                                                     margin-top: 5px;"),
                         
                         #helpText("Select display options."),
                         
                         #checkboxInput('silhouette', 'Silhouette?'),
                         #checkboxInput('pseudo_f', 'Pseudo F?'),
                         
                         numericInput(inputId = "clusters", label = "Select the number of clusters", value = 2, min = 2),
                         
                         radioButtons("setrandseedkmean", HTML("Do you want to set a seed for reproducible results?"),c("Yes", "No"), selected = "No"),
                         conditionalPanel("input.setrandseedkmean == 'Yes'",
                                          numericInput("randseedkmean",
                                                       HTML("Set a random seed."), sample(1:9999, size= 1), 
                                                       min = 1, max = 9999))
                         
                         
                       )
                       
                       ), 
                
                
                column(9, align="center",
                       
                       
                       tabsetPanel(type = 'tabs', 
                                   
                                   tabPanel('K-Means Clusters', 
                                            uiOutput("kmeans_title"), #title for the table
                                           DTOutput("kmeans_tab")), 
                                   
                                   tabPanel('Additional Statistics', 
                                            textOutput("pseudoF"),
                                            br(),
                                            plotOutput(outputId = "kmeans_silh", inline=TRUE))
                                   )
                       
                       # uiOutput("kmeans_title"), #title for the table
                       # tableOutput("kmeans_tab"),
                       # br(),
                       # textOutput("pseudoF"),
                       # br(),
                       # plotOutput(outputId = "kmeans_silh", inline=TRUE)
                       
                       )
                       
                       
                       
                       )
              ),
      
      
      ##### AI CLUSTERS TAB #####
      tabItem("AI_clusters",
              tags$h3("STEP 3: USING 'AI' TO CONFIRM YOUR CLUSTER SOLUTION", style = "text-align: center;"),
              
              tags$h4(HTML("Here we will use the Self-Organising Map AI to explore further your k-means cluster solution"), style = "text-align: center;"),
              
              #verbatimTextOutput("som_warning"),
              
              br(),
              
              
              fluidRow(
                
                column(3,
                       
                       box(width = 12, 
                           
                           actionButton("trainbutton","Train SOM", class = "full-width-button",
                                        style = "foreground-color:white; 
                                                     background-color:darksalmon;
                                                     color:black;
                                                     float:center;
                                                     height: 50px;
                                                     text-align:center;
                                                     border-color:black;
                                                     border-radius: 5px;
                                                     border-width: 5px;
                                                     margin-bottom: 5px;
                                                     margin-top: 5px;"),
                           
                           actionButton("infoButton_som", "Info", class = "full-width-button",
                                        style = "margin-bottom: 5px;
                                                     margin-top: 5px;"),
                           
                           h4("Map Dimensions", style = "text-align: center;"),
                           
                           numericInput("dimx", "Map dimension X:", 5, min= 3, max= 15),
                           numericInput("dimy", "Map dimension Y:", 5, min= 3, max= 15),
                           
                           h4("Advanced Options", style = "text-align: center;"),
                           uiOutput("initproto"),
                           numericInput("maxit", "Max. iterations:", 500),
                           uiOutput("scaling"), 
                           numericInput("eps0", "Scaling value for gradient descent", 1,
                                        min= 0.01, step= .01),
                           radioButtons("setrandseed", HTML("Do you want to set a seed for reproducible results?"),c("Yes", "No"), selected = "No"),
                           conditionalPanel("input.setrandseed == 'Yes'",
                                            numericInput("randseed",
                                                         HTML("Set a random seed."), sample(1:9999, size= 1), 
                                                         min = 1, max = 9999))
                           
                           )
                       
                       ),
                
                column(9, align="center", 
                       
                       uiOutput("trainnotice_header"),
                       
                       br(),
                       
                       uiOutput("trainnotice_advanced_trigger"),
                       
                       uiOutput("trainnotice_advanced_info")
                       
                       #plotOutput(outputId = "som_3Dplot", width = "50%", height = "500px")
                       
                       )
                
                
              )
              
              
              ),
      
      
      ##### COMPARE AND VISUALISE TAB #####
      tabItem("compare_and_visualise",
              tags$h3("STEP 4: VISUALISING AND EXPLORING YOUR CLUSTER SOLUTIONS", style = "text-align: center;"),
              
              tags$h4(HTML("Here we visualize the results of both your k-means and SOM AI  cluster solutions"), style = "text-align: center;"),
              
              br(),

              
              fluidRow(
                
                column(3, 
                       
                       selectInput("somplotwhat", "Plot what?", 
                                   choices= list("Observations"= "obs",
                                                 "Prototypes"= "prototypes"))
                       ), 
                
                
                column(3, 
                       
                       selectInput("somplottype", "Type of plot:", 
                                   choices= c("color", "barplot", 
                                              "names", "boxplot"))
                ), 
                
                
                column(3, 
                       
                       conditionalPanel("input.somplottype == 'color' ||
                                                input.somplottype == '3d'",
                                        selectInput("somplotvar", 
                                                    "Variable:", 
                                                    choices= "(Not Available)"))
                       
                ),
                
                
                column(3, 
                       
                       column(6, 
                              
                              conditionalPanel("input.trainbutton > 0", 
                                               actionButton("save_som", "Save SOM Results")), 
                              
                              uiOutput("save_som_notice") # SHOULD SAVE SOM BE IN THE PRIOR TAB?
                              
                              ), 
                       
                       column(6, 
                              
                              actionButton("infoButton_plot_map", "Info", class = "full-width-button")
                              
                              )
                       )
                
              ),
              # conditionalPanel("input.somplottype == 'boxplot'",
              #                  selectInput("somplotvar2", 
              #                              "Variable: (hold Ctrl to select
              #                              multiple variables)", 
              #                              choices= "(Not Available)", 
              #                              multiple= TRUE)),
             
              column(12, align="center",
                     
                     
                     conditionalPanel(
                       condition = "input.somplottype == 'boxplot'",
                       withSpinner(plotlyOutput("somplot_box", height = "600px"))  #, width = "1000px"
                     ),
                     conditionalPanel(
                       condition = "input.somplottype == 'names'",
                       withSpinner(plotOutput("somplot_names", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplottype == 'color'",
                       withSpinner(plotOutput("somplot_color", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplotwhat == 'obs' && input.somplottype == 'barplot'",
                       withSpinner(plotlyOutput("somplot_obs_bar", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplotwhat == 'prototypes' && input.somplottype == '3d'",
                       withSpinner(plotlyOutput("somplot_3d", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplotwhat == 'prototypes' && input.somplottype == 'smooth.dist'",
                       withSpinner(plotlyOutput("somplot_smooth_dist", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplotwhat == 'prototypes' && input.somplottype == 'barplot'",
                       withSpinner(plotlyOutput("somplot_prototypes_bar", height = "600px"))
                     ),
                     conditionalPanel(
                       condition = "input.somplotwhat == 'prototypes' && input.somplottype == 'umatrix'",
                       withSpinner(plotOutput("somplot_umatrix", height = "600px"))
                     )
                   )
              ),
      
      ##### SCENARIOS TAB #####
      tabItem("scenarios",
              tags$h3("STEP 5: USING YOUR MODEL TO RUN SCENARIO SIMULATIONS", style = "text-align: center;"), 
              
              tags$h4("Here we will use your model to explore different scenarios, policies, and interventions", style = "text-align: center;"),
                      
              tags$h4("To do that, we will be using your k-means clusters and your SOM AI solution and grid.", style = "text-align: center;"),                             
              
              br(),
              
              #verbatimTextOutput("Agent_Warning"),
              fluidRow(
                column(3,
                       
                       box(width = 12, 
                           
                           actionButton("infoButton_scenarios", "Info", class = "full-width-button",
                                        style = "margin-bottom: 5px;
                                                     margin-top: 5px;"),
                           
                           h4("Run Model", style = "text-align: center;"),
                           
                           actionButton(inputId = "Agent_Setup", label="Model Setup", class = "full-width-button",
                                        style = "foreground-color:white; 
                                                     background-color:khaki;
                                                     color:black;
                                                     float:center;
                                                     height: 50px;
                                                     text-align:center;
                                                     border-color:black;
                                                     border-radius: 5px;
                                                     border-width: 5px;
                                                     margin-bottom: 5px;
                                                     margin-top: 5px;"),
                           
                           actionButton(inputId = "Agent_Run_Clusters", label="Run Clusters", class = "full-width-button",
                                        style = "foreground-color:white;
                                                     background-color:lavender;
                                                     color:black;
                                                     height: 50px;
                                                     text-align:center;
                                                     border-color:black;
                                                     border-radius: 5px;
                                                     border-width: 5px;
                                                     margin-bottom: 20px;
                                                     margin-top: 5px;"),
                           
                           h4("Sensitivity Analysis", style = "text-align: center;"),
                           
                           actionButton(inputId = "SensitivityAnalysis", label="Sensitivity", class = "full-width-button",
                                        style = "foreground-color:white; 
                                                    background-color:darksalmon; 
                                                    color:black;
                                                    float:center;
                                                    height: 50px;
                                                    text-align:center;
                                                    border-color:black;
                                                    border-radius: 5px;
                                                    border-width: 5px;
                                                    margin-bottom: 5px;
                                                    margin-top: 5px;"),
                           
                           uiOutput('cluster_sensitivity')
                           
                           ),
                       
                       
                ),
                column(9,
                       conditionalPanel("input.Agent_Setup > 0",
                                        
                                        
                                        tabsetPanel(type = 'tabs', 
                                                    
                                                    tabPanel('SOM Plot Grid', 
                                                             plotOutput("somplotagent", width ="700px", height = "600px"),
                                                             br(),
                                                             rHandsontableOutput("clusters_editable_table"),
                                                             actionButton("back_cluster", "<<"),
                                                             actionButton("forward_cluster", ">>")), 
                                                    
                                                    tabPanel('Sensitivity Bar Plot', plotOutput("sensitivity_barplot")), 
                                                    
                                                    tabPanel('Agent SOM Plot', plotOutput("agent_somplot"))
                                                    
                                                    )
                       ) #Could comment this out so it by default shows the options
                                        
                       ))
              
      ),
      
      ##### FORECASTING TAB #####
      tabItem("forecasting",
              tags$h3("STEP 6: USING YOUR SOM AI TO PREDICT THE CLUSTER MEMBERSHIP OF NEW CASES", style = "text-align: center;"), 
              
              tags$h4(HTML("Here we will use your trained SOM GRID (TAB 4) to predict the cluster profile(s) that best represent a new set of cases"), style = "text-align: center;"),

              #verbatimTextOutput("Predict_Warning"),
              
              br(),
              
              fluidRow(

                column(3,
                       
                       box(width = 12,
                           
                           actionButton(inputId = "classify_prof", label="Classify Profiles", class = "full-width-button",
                                        style = "foreground-color:white; 
                                                    background-color:darksalmon; 
                                                    color:black;
                                                    float:center;
                                                    height: 50px;
                                                    text-align:center;
                                                    border-color:black;
                                                    border-radius: 5px;
                                                    border-width: 5px;
                                                    margin-bottom: 5px;
                                                    margin-top: 5px;"),
                           
                           actionButton("infoButton_new_prediction", "Info", class = "full-width-button",
                                        style = "margin-bottom: 5px;
                                                     margin-top: 5px;"),
                           
                           fileInput('file_pred', 'Choose CSV File', accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")),
                           
                           selectInput('sep_pred', 'Separator:',
                                       c("Comma","Semicolon","Tab","Space"), 'Comma'),
                           
                           checkboxInput('load_prev_som', 'Use Previous SOM Solution? If unchecked it will use SOM solution from this session.'),
                           
                           #numericInput("nrow.result_pred","Number of rows in the results:" ,20, min = 1, max = 100)
                           
                           )
                       
                       
                       ), 
                
                
                column(9, 
                       
                       
                       tabsetPanel(type = 'tabs', 
                                   
                                   tabPanel('Table of Predictions', DTOutput("view_predict")), 
                                   
                                   tabPanel('Prediction SOM Plot', plotOutput("predict_somplot"))
                                   
                       )
                       
                       
                       )


              )
              
              
      ),
      
      ##### SYSTEMS MAPPING TAB #####
      tabItem("systems_mapping",
              
              # App title ----
              #titlePanel("STEP 8: USING SYSTEMS MAPPING TO EXPLORE CLUSTER VARIABLES"),

              tags$h3("STEP 7: USING SYSTEMS MAPPING TO EXPLORE CLUSTER VARIABLES", style = "text-align: center;"), 
              
              tags$h4(HTML("Here we will use Systems Mapping to visually explore the configuration of variables you used to cluster your data. <br>
    
                 The map is generated using the <a href='https://dictionary.apa.org/zero-order-correlation'>zero-order correlations</a> amongst your variables."), style = "text-align: center;"),
              
              #verbatimTextOutput("network_warning"),
              
              
              fluidRow(
                
                column(10, 
                       
                       # Output: Histogram ----
                       
                       
                       introBox(
                         visNetworkOutput("networkPlot", height = '600px'), 
                         data.step = 25,
                         data.intro = "Here is your network. You can interact with it, including zooming in and out, moving nodes, and drawing in your own nodes and connections."),
                       
                       br(), 
                       br(), 
                       br() #Unless a better solution is found, this must be kept at three br() tags, as any less and the footer gets cut off. 
                ), 
                
                column(2, 
                       
                       introBox(actionButton(inputId = "initialise_button", label="Initialise Network", class = "full-width-button",
                                             style = "foreground-color:white; 
                                                     background-color:darksalmon;
                                                     color:black;
                                                     float:center;
                                                     height: 50px;
                                                     text-align:center;
                                                     border-color:black;
                                                     border-radius: 5px;
                                                     border-width: 5px;
                                                     margin-bottom: 5px;
                                                     margin-top: 5px;"),
                                data.step = 1,
                                data.intro = "Clicking this button initialises your systems mapping network."
                       ),
                       
                       
                       introBox(actionButton("infoButton", "Info", class = "full-width-button",
                                             style = "margin-bottom: 5px;
                                                     margin-top: 5px;"),
                                data.step = 2,
                                data.intro = "Clicking this button lets you re-read the information on the pop-up when first entering the Systems Mapping tab."
                       ),
                       
                       actionButton("tour_systems_mapping", "Guided Tour of Inputs", class = "full-width-button",
                                    style = "margin-bottom: 20px;
                                             margin-top: 5px;"),
                       
                       # Input: Node selection
                       
                       introBox(
                         selectInput("chosen_node", "Examine node:",
                                     c(NULL), 
                                     selected = F), 
                         
                         htmlOutput("text", inline = T), 
                         data.step = 26,
                         data.intro = "Here you can examine facts about your network and any particular nodes in your network."),
                       
                       
                       
                       
                       br(),
                       
                       
                       introBox(
                         # Size of Nodes
                         numericInput('node_size', 'Node Size:', 
                                      value = 25, 
                                      min = 1, 
                                      max = 100), 
                         data.step = 27,
                         data.intro = "Here you can change the size of your nodes."),
                       

                       
                )
                
              ), 
              
              
              
              
              
              fluidRow(
                
                column(4,
                       
                       shinyjs::useShinyjs(),
                       
                       ######################## MAIN OPTIONS BOX START ########################
                       box(id = "intro_box", width = "800px", 
                           
                           
                           introBox(selectInput(inputId = "cluster",
                                                label = "What Cluster would you like to analyse?",
                                                choices = NULL, 
                                                selected = 'All', 
                                                multiple = FALSE),
                                    data.step = 3,
                                    data.intro = "If you clustered your data using K-Means in tab 2, you can choose to examine any cluster that has more than three members in that cluster."
                                    ),
                           
                           
                           introBox(
                             
                             # Adding information panel
                             helpText("For these two sliders, values below the threshold will be 
                     excluded when making the network. For example, setting the correlation threshold
                     to 0.7 excludes correlations below 0.7 from the network."),
                             
                             # Input: Slider for minimum correlation ----
                                    sliderInput(inputId = "neg_corr",
                                                label = "Threshold for Negative Correlations:",
                                                min = 0,
                                                max = 1,
                                                value = 0.7,
                                                step = 0.05),
                                    
                                    # Input: Slider for maximum correlation ----
                                    sliderInput(inputId = "pos_corr",
                                                label = "Threshold for Positive Correlations:",
                                                min = 0,
                                                max = 1,
                                                value = 0.5, 
                                                step = 0.05),
                                    data.step = 4,
                                    data.intro = "Here you can filter for what threshold of positive or negative correlation must be achieved for a connection to be drawn between your nodes."),
                           
                           
                           introBox(# Adding dropdown to change network ---- 
                                    selectInput("layout", "Choose layout algorithm:",
                                                c("Circle" = "layout_in_circle",
                                                  "Random" = "layout_randomly",
                                                  "Davidson-Harel" = "layout_with_dh", 
                                                  "Fruchterman-Reingold" = "layout_with_fr", 
                                                  "Sugiyama" = "layout_with_sugiyama"), 
                                                selected = T),
                                    data.step = 5,
                                    data.intro = "Here you can choose the layout of your network." ),
                           
                           introBox(# Adding dropdown to change network ----
                                    radioButtons(inputId = "remove_unconnecteds", 
                                                 label = "Remove Nodes with No Connections?", 
                                                 c("No", "Yes")),
                                    data.step = 6,
                                    data.intro = "Here you remove any nodes from your network that do not have links to any other nodes." )),
                       
                       
                       ######################## MAIN OPTIONS BOX START ########################
                       
                       br(),
                       br(),
                       br(),
                       br()
                       
                ), 
                
                column(4, 
                       
                       shinyjs::useShinyjs(),
                       
                       ######################## ADVANCED OPTIONS CHECKBOX START ########################
                       box(id = "advancedOptionsBox", title = "Advanced Options", width = "800px",
                           
                           introBox(
                             # Choose how to present line thickness
                             radioButtons(inputId = "LineThickness", 
                                          label = "How to Determine Line Thickness", 
                                          c("Threshold"="binary", "Gradation"="bins")),
                                    data.step = 8,
                                    data.intro = "Here you decide if you would like your lines to be represented as a threshold of 'stronger' and 'weaker' correlations (default), or a gradation increasingly thicker lines as the correlations get stronger." ),
                           
                           introBox(
                             # Adding information panel ----
                             helpText("Note: the threshold for 'minor' correlations determines at what value 
                     correlations between nodes will have a thinner dashed line or a thicker
                     solid line. For example, a setting the threshold at 0.5 will designate
                     correlations below that as 'minor'."), 
                             
                             # Adding slider for when dotted line ----
                             sliderInput(inputId = "minor_threshold",
                                         label = "Threshold for 'Minor' Correlations:",
                                         min = 0,
                                         max = 1,
                                         value = 0.7, 
                                         step = 0.05),
                             data.step = 9,
                             data.intro = "If your lines are decided by a threshold, this determines at what value you consider a correlation to be weaker and thus represented with a thinner line." ),
                           

                           
                           introBox(
                             numericInput(inputId = "seed",
                                          label = "Set Seed (for reproducible results)",
                                          value = 1, 
                                          min = 1, 
                                          max = 10000),
                             data.step = 10,
                             data.intro = "Here you can set a seed for reproducible results in graphing your network." )
                           
                           
                           ),
                       
                       introBox(actionButton(inputId = "advancedOptions", label = "Show / Hide"),
                                data.step = 7,
                                data.intro = "You can toggle whether to see the a box of options using this button." ),
                       
                       br(),
                       br(),
                       
                       ######################## ADVANCED OPTIONS CHECKBOX END ########################
                       
                       ######################## EGO NETWORK CHECKBOX START ########################
                       
                       introBox(
                         
                         box(id = "egoNetworkBox", title = "Ego Network", width = "800px",
                             
                             introBox(
                               # Adding checkbox for egonetwork ----    
                               checkboxInput(inputId = "ego_network", 
                                             label = "Make ego-network?", 
                                             value = FALSE),
                               data.step = 14,
                               data.intro = "Selecting this will show your ego-network. Only select this when you are ready to visualise your network and deselect when making changes."),
                             
                             
                             introBox(
                               # Adding target node ----
                               selectInput("target_node", "Target Node ID:",
                                           c(NULL), 
                                           selected = F),
                               data.step = 12,
                               data.intro = "Here you can set which node will be the focus of your ego-network" ),
                             
                             introBox(
                               # Adding slider for degrees ----
                               numericInput(inputId = "degree",
                                            label = "No. Degrees:",
                                            min = 1,
                                            max = 100,
                                            value = 1, 
                                            step = 1),
                               data.step = 13,
                               data.intro = "Here you can set how many degrees away from your ego-network node you will show in your network.")),
                         
                         data.step = 11,
                         data.intro = "Here you can create an ego-network of as many desired degrees of separation from a chosen node." ),
                       
                       
                                              actionButton(inputId = "egoNetwork", label = "Show / Hide"),
                       
                       br(),
                       br(),
                       ######################## EGO NETWORK CHECKBOX END ########################
                       
                       ######################## EGO NETWORK CHECKBOX START ########################
                       
                       
                       introBox(
                         
                         box(id = "shortestPathsBox", title = "Shortest Paths", width = "800px",
                             
                             
                             introBox(# Adding checkbox for egonetwork ----    
                                      checkboxInput(inputId = "shortest_paths_toggle", 
                                                    label = "Show Shortest Paths?", 
                                                    value = FALSE),
                                      data.step = 16,
                                      data.intro = "Selecting this will show your shortest paths network. Only select this when you are ready to visualise your network and deselect when making changes."),
                             
                             
                             introBox(
                               # Adding target node ----
                               selectInput("from_node", "What node to start at?",
                                           c(NULL), 
                                           selected = F), 
                               
                               # Adding target node ----
                               selectInput("to_node", "What node to go to?",
                                           c(NULL), 
                                           selected = F),
                                      data.step = 15,
                                      data.intro = "Here you can select what nodes you want to show a shortest path to and from. If you add weights these will be considered when showing you your shortest paths." )),
                         
                                data.step = 14,
                                data.intro = "Here you can show the shortest path between two nodes, if a path exists." ),
                       
                       actionButton(inputId = "shortestPaths", label = "Show / Hide"),
                       ######################## EGO NETWORK CHECKBOX END ########################
                       
                       br(),
                       br(),
                       br(),
                       br()
                       
                ), 
                
                column(4, 
                       
                       shinyjs::useShinyjs(),
                       
                       ######################## EGO NETWORK CHECKBOX START ########################
                       
                       
                       introBox(
                         box(id = "weightsBox", title = "Edge Weights Options", width = "800px",
                             
                             
                             
                             introBox(# Adding information panel ----
                                      helpText("Reminder: These are all the possible edges between every node. Only put
                         your user defined weights for the edges you are interested in and upload these below. Remember, 
                         the network map is undirected, to from A to B and from B to A are synonymous."), 
                                      
                                      # Adding checkbox for egonetwork ----    
                                      downloadButton("downloadData", label = "Download"),
                                      data.step = 18,
                                      data.intro = "To populate your network weights download the template from here."),
                             
                             
                             
                             
                             
                             introBox(
                               checkboxInput(inputId = "include_weights", 
                                             label = "Include Edge Weights?", 
                                             value = FALSE),
                               data.step = 20,
                               data.intro = "Selecting this will include weights to your network. You can deselect and reselect this without needing to reupload your weights. You only need to reupload weights if you wish to change them."),
                             
                             
                             introBox(
                               fileInput(inputId = "weights_values", multiple = F, accept = ".csv", label = "Weights Values"),
                                      data.step = 19,
                                      data.intro = "Upload your network weights from here, using the template you downloaded.")),
                         
                         
                                data.step = 17,
                                data.intro = "Here you can add weights to the connections between your nodes, which will be factored into any shortest path calculations."),
                       
                       actionButton(inputId = "weightsOptions", label = "Show / Hide"),
                       
                       br(),
                       br(),
                       br(),
                       ######################## EGO NETWORK CHECKBOX END ########################
                       
                       ######################## EXPORT OPTIONS CHECKBOX START ########################
                       
                       
                       introBox(
                         
                         box(id = "exportOptionsBox", title = "Export Options", width = "800px",
                             
                             
                             introBox(
                               # Adding action buttons for various outputs to change network ----
                               textInput(inputId = "title", label = "Add title", value = NULL), 
                               textInput(inputId = "subtitle", label = "Add subtitle", value = NULL), 
                               textInput(inputId = "footer", label = "Add footer", value = NULL),
                               data.step = 22,
                               data.intro = "Here you can annotate your network with a title, subtitle, and footer."),
                             
                             
                             introBox(
                               downloadButton('htmlSave', 'Download', label = "Download your network as an HTML file",
                                              style = "margin-bottom: 5px;
                                                     margin-top: 5px;
                                              width: 100%;"),
                               data.step = 23,
                               data.intro = "Here you can download your network as a .html file, which can be shared offline independent of COMPLEX-IT."),
                             
                             
                             introBox(
                               downloadButton("nodesDownload", "Download", label = "Download your network's nodes",
                                              style = "margin-bottom: 5px;
                                                     margin-top: 5px;
                                              width: 100%;"),
                               downloadButton("edgesDownload", "Download", label = "Download your network's edges",
                                              style = "margin-bottom: 5px;
                                                     margin-top: 5px;
                                              width: 100%"),
                               data.step = 24,
                               data.intro = "Here you can download your network's nodes and edges."),
                             
                             
                             
                             # downloadButton('pngSave', 'Download', label = "Download your network as an PNG file"),
                             ),
                         
                         data.step = 21,
                         data.intro = "Here you can add annotations to your network and export the network as an interactive HMTL file or export the underlying data"),
                       
                       
                       
                       actionButton(inputId = "exportOptions", label = "Show / Hide"),
                       
                       ######################## EXPORT OPTIONS CHECKBOX END ########################
                       
                       br(),
                       br(),
                       br(),
                       br()
                       
                )
              )
              
      ),
      
      ##### GENERATE REPORT TAB #####
      tabItem("generate_report",
              tags$h3("STEP 8: GENERATING A REPORT FROM YOUR VARIOUS ANALYSES", style = "text-align: center;"), 
              
              tags$h4(HTML("Here you be able to create and download a report of all your key statistical and visual information."), style = "text-align: center;"),
              
              p(HTML('For TUTORIALS on what is contained in a COMPLEX-IT report, <a href= 
                                    "https://www.art-sciencefactory.com/tutorials.html"
                                    target="_blank">CLICK HERE</a>')), 
              
              p("NOTE: The Files from your report will be placed in a zip file
                               and downloaded to your default download directory. Please also note, you will
                               only receive results from the sections you have used in this session. And, finally, only your
                               most recent analysis -- kmeans, SOM or policy prediction, etc -- will be downloaded.", style = "color:green"),
              downloadButton('downloadReport', 'Download Report')
              
      ),
      
      ##### HELP TAB #####
      tabItem("help",
              
              h3("Help Documentation"),
              p(HTML('To view TUTORIALS and USER HELP FILES for COMPLEX-IT report, <a href= 
                                    "https://www.art-sciencefactory.com/tutorials.html"
                                  target="_blank">CLICK HERE</a> (Note internet connection required)'))
              
      )
    )
  )
)