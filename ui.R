library(shiny)
library(shinythemes)
library(shinyFiles)


ui <- fluidPage(theme=shinytheme("spacelab"),
                
                titlePanel("Complex-It 0.1.6"),
                
                sidebarLayout(
                  
                  sidebarPanel(
                    imageOutput("complexit_logo", inline=TRUE),
                    width = 2
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                      
                      tabPanel("Import Data",
                               h3("Import and Prepare Data"),
                               
                               p(HTML("To start an analysis session, import your data set using the
                                      'Browse...' button below. Your data must be in the form of a csv file. Note that when you 
                                      download Complex-It from GitHub, several datasets  will be downloaded with it. Explore 
                                      the '/data' folder inside the Complex-It folder for further analysis opportunities. 
                                      If the data is successfully imported, a preview of your data will be displayed. You may
                                      also subset the data by deselecting variables and pressing the 'Subset data' button.
                                      ")),
                               p(HTML('The Complex-It team extends a big thanks to the SOMbrero team from which our
                                      package draws on SOM training and visualization functions. For more SOM analysis
                                      visit the SOMbrero <a href= 
                                      "https://cran.r-project.org/web/packages/SOMbrero/index.html"
                                      target="_blank">package site.</a>')),
                               br(), 
                               fileInput('file1', 'Choose CSV File', accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                               checkboxInput('header', ' Header?', TRUE),
                               selectInput('sep', 'Separator:',
                                           c("Comma","Semicolon","Tab","Space"), 'Comma'),
                               selectInput('quote', 'Quote:',
                                           c("None","Double Quote","Single Quote"), 
                                           'Double Quote'),
                               uiOutput("varchoice"),
                               numericInput('nrow.preview','Number of rows in the preview:',20, min = 1, max = 100),
                               numericInput('ncol.preview', 'Number of columns in the preview:',
                                            10,min = 1, max = 100),
                               helpText("Note: Even if the preview only shows a restricted
                                        number of observations, the map will be based on the full dataset."),
                               tableOutput("view")
                               
                               ),
                      
                      tabPanel("Cluster Data",
                               h3("Cluster Data"),
                               p(HTML("In this tab you can use the kmeans clustering algorithm to group the cases in your
                                      data into self-similar 'clusters'. The Pseudo F provides a measure of how distinct
                                      and well defined the clusters are. Silhouette plots display how well each case fits
                                      within its respective cluster, where 1 is a perfect fit with its clusterand -1 is a 
                                      perfect fit with a different, neighboring cluster")),
                               br(),
                               helpText("Select display options."),
                               checkboxInput('silhouette', 'Silhouette?'),
                               checkboxInput('pseudo_f', 'Pseudo F?'),
                               numericInput(inputId = "clusters", label = "Select the number of clusters", value = 2, min = 2),
                               actionButton(inputId = "init_kmeans", label="Get Clusters"),
                               conditionalPanel("input.init_kmeans > 0", shinySaveButton("save", "Save results", "Save file as", 
                                                                                         filetype=list(csv="csv"))),
                               tableOutput("kmeans_tab"),
                               textOutput("pseudoF"),
                               plotOutput(outputId = "kmeans_silh", inline=TRUE)
                               ),
                      
                      tabPanel("Self-Organize",
                               h3("Train the Self-Organizing Map"),
                               p(HTML("In this tab you can train the Self-Organizing Map (SOM) to project the cases in your
                                      data onto an X * Y dimensional map, where each XiYi 'neuron' on the map represents
                                      a different vector of values for all the data variables. Set the SOM dimensions
                                      with the 'Map Dimension X & Y sliders and set a random seed between 1-9999 to
                                      to set reproducible SOM results")),
                               
                               actionButton("trainbutton","Train SOM"),
                               #verbatimTextOutput("summary") #prints the output of a reactive variable, can show status of uploaded
                               #data, check the original SOMbrero 
                               br(), br(),
                               
                               uiOutput("trainnotice"),
                               
                               br(), 
                               h4("Options"),
                               
                               numericInput("dimx", "Map dimension X:", 5, min= 1),
                               numericInput("dimy", "Map dimension Y:", 5, min= 1),
                               h4("Advanced options"),
                               uiOutput("initproto"),
                               numericInput("maxit", "Max. iterations:", 500),
                               uiOutput("scaling"),
                               numericInput("randseed",
                                            HTML("Set a random seed for reproducible results"), sample(1:1e4, size= 1), 
                                            min = 1, max = 9999),
                               numericInput("eps0", "Scaling value for gradient descent", 1,
                                            min= 0.01, step= .01),
                               
                               
                               plotOutput(outputId = "som_3Dplot", width = "50%", height = "500px")
                               ),
                      tabPanel("Plot Map",
                               h3("Plot the Self-Organizing Map"),
                               p("In this panel you can visualize the computed 
                                 self-organizing map. This panel contains several plots to analyze the map. The names plot
                                 under 'Obs' will map previous kmeans labels to cases on their respective part of the SOM"),
                               
                               h4("Options"),
                               selectInput("somplotwhat", "Plot what?", 
                                           choices= list("Observations"= "obs",
                                                         "Prototypes"= "prototypes")),
                               selectInput("somplottype", "Type of plot:", 
                                           choices= c("color", "barplot", 
                                                      "names", "boxplot")),
                               conditionalPanel("input.somplottype == 'color' ||
                                                input.somplottype == '3d'",
                                                selectInput("somplotvar", 
                                                            "Variable: (only used for '3d',
                                                            'color' and 'boxplot' plots if available)", 
                                                            choices= "(Not Available)")),
                               conditionalPanel("input.somplottype == 'boxplot'",
                                                selectInput("somplotvar2", 
                                                            "Variable: (hold Ctrl to select
                                                            multiple variables)", 
                                                            choices= "(Not Available)", 
                                                            multiple= TRUE)),
                               conditionalPanel("input.trainbutton > 0", 
                                                actionButton("save_som", "Save SOM Results")),
                               uiOutput("save_som_notice"),
                               plotOutput("somplot")),
                      
                      tabPanel("Agent-Model", 
                               h3("  "),
                               p("   "),
                               br(),
                               hr(),
                               fluidRow(
                                 column(3,
                                        actionButton(inputId = "Agent_Setup", label="Setup",
                                                     style = "foreground-color:white; 
                                                     background-color:blue;
                                                     float:center;
                                                     height: 50px;
                                                     width: 250px;
                                                     text-align:center;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        actionButton(inputId = "Agent_Run_Cases", label="Run Cases",
                                        style = "foreground-color:white; 
                                                     background-color:blue; 
                                                     height: 50px;
                                                     width: 250px;
                                                     text-align:center;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        actionButton(inputId = "Agent_Run_Clusters", label="Run Clusters",
                                                     style = "foreground-color:white; 
                                                     background-color:blue; 
                                                     height: 50px;
                                                     width: 250px;
                                                     text-align:center;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        actionButton(inputId = "Agent_Use_Prev_SOM", label="Use Previous SOM Results",
                                                     style = "foreground-color:white; 
                                                     background-color:blue; 
                                                     height: 50px;
                                                     width: 250px;
                                                     text-align:center;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        selectInput("Agent_somplottype", "Type of Grid Plot:", 
                                                    choices= c("color", "barplot", 
                                                               "names", "boxplot")),
                                        br(),
                                        hr(),
                                        fileInput('file-acl', 'Upload Clusters', accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                                        checkboxInput('header-acl', ' Header?', TRUE),
                                        selectInput('sep-acl', 'Separator:',
                                                    c("Comma","Semicolon","Tab","Space"), 'Comma'),
                                        selectInput('quote-acl', 'Quote:',
                                                    c("None","Double Quote","Single Quote"), 
                                                    'Double Quote'),
                                        #uiOutput("varchoiceacl"),
                                        numericInput('nrow.preview-acl','Number of rows in the preview:',20, min = 1, max = 100),
                                        numericInput('ncol.preview-acl', 'Number of columns in the preview:',
                                                     10,min = 1, max = 100),
                                        helpText("Note: Even if the preview only shows a restricted
                                        number of observations, the map will be based on the full dataset."),
                                        hr(),
                                        br(),
                                        fileInput('file-acs', 'Upload Cases', accept = c(
                                          "text/csv",
                                          "text/comma-separated-values,text/plain",
                                          ".csv")),
                                        checkboxInput('header-acs', ' Header?', TRUE),
                                        selectInput('sep-acs', 'Separator:',
                                                    c("Comma","Semicolon","Tab","Space"), 'Comma'),
                                        selectInput('quote-acs', 'Quote:',
                                                    c("None","Double Quote","Single Quote"), 
                                                    'Double Quote'),
                                        #uiOutput("varchoiceacs"),
                                        numericInput('nrow.preview-acs','Number of rows in the preview:',20, min = 1, max = 100),
                                        numericInput('ncol.preview-acs', 'Number of columns in the preview:',
                                                     10,min = 1, max = 100),
                                        helpText("Note: Even if the preview only shows a restricted
                                                 number of observations, the map will be based on the full dataset.")
                                        ),
                                 column(4,
                                       h4("Case-Based Multi-Agent Grid"),
                                       imageOutput("dummy_som_graphic", inline=TRUE),
                                       br(),
                                       br(),
                                       br(),
                                       hr(),
                                       imageOutput("dummy_cluster_graphic", inline=TRUE),
                                       #tableOutput("kmeans_tab"),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       br(),
                                       hr(),
                                       imageOutput("dummy_case_graphic", inline=TRUE)
                                       #tableOutput("view")
                                       ))
                               
                               #uiOutput("save_som_notice"),
                               #plotOutput(outputId = "som_3Dplot", width = "50%", height = "500px"),
                               #plotOutput("somplot"),
                              # fileInput('file_pred', 'Choose CSV File', accept = c(
                              #   "text/csv",
                              #   "text/comma-separated-values,text/plain",
                              #   ".csv")),
                              # checkboxInput('header_pred', ' Header?', TRUE),
                              # checkboxInput('load_prev_som', 'Use Previous SOM Results?'),
                              # selectInput('sep_pred', 'Separator:',
                              #             c("Comma","Semicolon","Tab","Space"), 'Comma'),
                              # selectInput('quote_pred', 'Quote:',
                              #             c("None","Double Quote","Single Quote"), 
                              #             'Double Quote'),
                              # actionButton('classify_prof', 'Classify Profiles'),
                              # uiOutput("prof_rec_error"),
                              # conditionalPanel("input.classify_prof > 0",
                              #                 br(),
                              #                 numericInput("nrow.result_pred","Number of rows in the results:" ,20, min = 1, max = 100),
                              #                 tableOutput("view_predict"))
                              # 
                              ),
                              
                      
                      tabPanel("Help",
                               h3("Under Construction")
                               
                      )
                      
                      )
                      )
                               )
                
                
                               )




