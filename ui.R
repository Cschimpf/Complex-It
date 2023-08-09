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



# here is the first option using shiny themes:  
ui <- fluidPage(theme=shinytheme("cosmo"),


# here is the next option embedding the tag in native html,
# ----------
# tags$head(
# tags$link(rel = "stylesheet", type = "text/css", href = "sketchy.css")
 #tags$style("#nrow.preview {font-size:10px;height:10px;}"),
# ),
# ----------
               

                titlePanel("COMPLEX-IT 1.0.1 Beta - exploring complex data from a case-based perspective"),
                
              
                  navlistPanel(
                    id = "my_navlist",
                    selected = "1. Import Your Database",
                    HTML("<img src='Complexit_LOGO4.jpg'>"),
                    HTML("<br><br><br><br>"), HTML("<br><br><br><br>"), HTML("<br><br><br><br>"), HTML("<br>"),
                    "Build Your Model",
           
                      tabPanel("1. Build database and import your cases",
                               h3("STEP 1: IMPORTING YOUR DATABASE", style = "color:purple"),
                               
                               p(HTML('For TUTORIALS on preparing and importing your data for COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                      target="_blank">CLICK HERE</a>')),
                               p(HTML('To start an analysis session, import your data set using the BROWSE button below.')),
                               p(HTML('Your data must be in the form of a csv file. For more on creating csv files <a href= 
                                                    "https://www.wikihow.com/Create-a-CSV-File"
                                      target="_blank">CLICK HERE</a>')),

                               br(), 
                               fileInput('file1', 'Choose CSV File', buttonLabel='Browse',accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                                  ),
                               checkboxInput('header', ' Header?', TRUE),
                               selectInput('sep', 'Separator:',
                                           c("Comma","Semicolon","Tab","Space"), 'Comma'),
                             
                               uiOutput("varchoice"),
                               numericInput('nrow.preview','Number of rows in the preview:',20, min = 1, max = 100),
                               numericInput('ncol.preview', 'Number of columns in the preview:',
                                            10,min = 1, max = 100),
                               helpText("Note: Even if the preview only shows a restricted
                                        number of observations, the map will be based on the full dataset."),
                               tableOutput("view")
                               
                               ),

                      tabPanel("2. Cluster your cases",
                               h3("STEP 2: CLUSTER YOUR CASES USING K-MEANS ", style = "color:purple"),
                               
                               h4("Here we will use k-means (a clustering algorithm) to group your cases into self-similar 'clusters'"),

                               p(HTML('For TUTORIALS on clustering your data in COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                      target="_blank">CLICK HERE</a>')),
                               p(HTML('For a basic introduction to k-means, See <a href= 
                                                    "https://en.wikipedia.org/wiki/K-means_clustering"
                                      target="_blank">CLICK HERE</a>')),

                               tags$b("STEP 1: HYPOTHESIZE YOUR CLUSTER SOLUTION",
                                      tags$ol(tags$li("To begin, how many clusters do you think are in your database?"), 
                                              tags$li("What is your hypothesis based on -- the literature, a guess, expertise, experience, a hunch?"),
                                              tags$li("How would you describe or name these different clusters?"),
                                              tags$li("How do you think your case-based profile of variables account for these different clusters?"))),
                               
                               tags$b("STEP 2: RUN THE K-MEANS",
                                      tags$ol(tags$li("Run your k-means several times to see if you can improve the Pseudo F"),
                                              tags$li ("How strong is the Pseudo F for your solution?"), 
                                              tags$li("Looking at the Silhouette, how well are the cases distributed for each cluster?"),
                                              tags$li("Should you re-run k-means to look for more or less clsuters?"),
                                              tags$li("SOME DEFINITIONS: The Pseudo F indicates the quality of the overall solution; the larger the number, the better the fit.", style = "color:green"),
                                              tags$li("The Silhouette displays how well each case fits within its respective cluster; where a score of 1 is a perfect fit.", style = "color:green"),
                                              tags$li("NOTE: The K-means solution and related statistics are found in the GENERATE REPORT TAB.", style = "color:purple"))),
                              
                               br(),
                               helpText("Select display options."),
                               verbatimTextOutput("kmean_warning"),
                               checkboxInput('silhouette', 'Silhouette?'),
                               checkboxInput('pseudo_f', 'Pseudo F?'),
                               numericInput(inputId = "clusters", label = "Select the number of clusters", value = 2, min = 2),
                               radioButtons("setrandseedkmean", HTML("Do you want to set a seed for reproducible results?"),c("Yes", "No"), selected = "No"),
                               conditionalPanel("input.setrandseedkmean == 'Yes'",
                                                numericInput("randseedkmean",
                                                             HTML("Set a random seed."), sample(1:9999, size= 1), 
                                                             min = 1, max = 9999)), 
                               actionButton(inputId = "init_kmeans", label="Get Clusters"),
                               uiOutput("kmeans_title"),
                               tableOutput("kmeans_tab"),
                               textOutput("pseudoF"),
                               plotOutput(outputId = "kmeans_silh", inline=TRUE)
                               ),
                    
                      "Confirm & Explore Your Model",
                      tabPanel("3. Use AI to confirm your cluster solution",
                               h3("STEP 3: USING 'AI' TO CONFIRM YOUR CLUSTER SOLUTION ", style = "color:purple"),
                               h4("Here we will use the Self-Organising Map AI to explore further your k-means cluster solution"),

                               p(HTML('For TUTORIALS on using artificial intelligence AI in COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                      target="_blank">CLICK HERE</a>')),
                               tags$b("NEW USERS:",
                                      tags$ol(tags$li("For those new to the SOM, we recommend using all of the defaults"), 
                                              tags$li("However, if you have more than 25 clusters, you will need to make the map bigger, for example 6X6"),
                                              tags$li("NOTE: The SOM solution and related statistics are found in the GENERATE REPORT TAB.", style = "color:purple"))),
                               tags$b("EXPERIENCED USERS:",
                                      tags$ol(tags$li("The SOM algorithm we use comes from the SOMbrero R-Package"))), 
                                      p(HTML('To visit the SOMbrero CRAN site, including downloading its reference manual <a href="https://cran.r-project.org/web/packages/SOMbrero/index.html"
                                      target="_blank">CLICK HERE</a>')),
                               p(HTML('For a basic introduction to the Self-Organizing Map (SOM), <a href= 
                                      "https://en.wikipedia.org/wiki/Self-organizing_map"
                                      target="_blank">CLICK HERE</a>')),
                               
                               actionButton("trainbutton","Train SOM"),
                            
                               br(), br(),
                               verbatimTextOutput("som_warning"),
                               uiOutput("trainnotice"),
                               br(), 
                               h4("Options"),
                               
                               numericInput("dimx", "Map dimension X:", 5, min= 3, max= 15),
                               numericInput("dimy", "Map dimension Y:", 5, min= 3, max= 15),
                               h4("Advanced options"),
                               uiOutput("initproto"),
                               numericInput("maxit", "Max. iterations:", 500),
                               uiOutput("scaling"),
                               radioButtons("setrandseed", HTML("Do you want to set a seed for reproducible results?"),c("Yes", "No"), selected = "No"),
                               conditionalPanel("input.setrandseed == 'Yes'",
                                                numericInput("randseed",
                                                             HTML("Set a random seed."), sample(1:9999, size= 1), 
                                                             min = 1, max = 9999)), 
                               numericInput("eps0", "Scaling value for gradient descent", 1,
                                            min= 0.01, step= .01),
                               
                               
                               plotOutput(outputId = "som_3Dplot", width = "50%", height = "500px")
                               ),
                    
                      tabPanel("4. Compare and visualize your results",
                               h3("STEP 4: VISUALISING AND EXPLORING YOUR CLUSTER SOLUTIONS", style = "color:purple"),
                               h4("Here we visualize the results of both your k-means and SOM AI  cluster solutions"),
                               p(HTML('For TUTORIALS on visualising your data in COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                      target="_blank">CLICK HERE</a>')), 
                              p(HTML('NOTE: The visualisations tools used for this tab come from the SOMbrero R-Package. To understand how they work,<a href="https://cran.r-project.org/web/packages/SOMbrero/vignettes/c-doc-numericSOM.html"
                                      target="_blank">CLICK HERE</a>')),  
                              
                                h4("The grid below visually displays the results of your k-means and SOM AI cluster solutions.", style = "color:green"),                              
                               tags$b("READING THE SOM GRID:",
                                      tags$ol(tags$li("To begin, we label each case with its CASE ID and K-MEANS ID."),
                                              tags$li("To see these IDs, for 'PLOT WHAT?' select observations; and for 'TYPE OF PLOT' select names."),
                                              tags$li("The first ID on the grid is the k-means cluster number; the second ID is the case."), 
                                              tags$li("The grid also places each case in a quadrant, based on the SOM AI solution."),
                                              tags$li("The more similar the profile, the closer the cases on the grid; the more profiles differ, the further away cases are."),
                                              tags$li("The PROTOTYPES option (i.e., variables) shows how your profile of variables influenced where cases are located."),
                                              tags$li("The BARPLOT option for both OBSERVATIONS and PROTOTYPES shows the profile of variables for each quadrant."),
                                              tags$li("The line in the BARPLOT is mean=0; above the line is more of a variable;below the line is less."),
                                              tags$li("NOTE: Several of the images created here are found in the GENERATE REPORT TAB.", style = "color:purple"),
                                              tags$li("In addition, we recommend using SCREEN CAPTURE to save an image.", style = "color:purple"))),

                               tags$b("INTERPRETING YOUR RESULTS:",
                                      tags$ol(tags$li("Looking at the Names, are cases with similar k-means IDs located in similar quadrants?"),
                                              tags$li("If yes, do you think the SOM and k-means are reasonably similar solutions? Or, should you re=run your k-means?"),
                                              tags$li("How do the profiles account for the different cluster solutions and the quadrant locations of the cases?"),
                                              tags$li("What factors (i.e., variables) seem to have the biggest impact on different clusters or the model as a whole?"),
                                              tags$li("How does the data solution differ from your hypotheses back at the design phase of COMPLEX-IT?"),
                                              tags$li("Are you satisfied with your solution?  If not, go back and run your k-means and SOM again."))),
                               

                               
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
                                                            "Variable: (only used for '3d'
                                                            and 'color' plots if available)", 
                                                            choices= "(Not Available)")),
                               # conditionalPanel("input.somplottype == 'boxplot'",
                               #                  selectInput("somplotvar2", 
                               #                              "Variable: (hold Ctrl to select
                               #                              multiple variables)", 
                               #                              choices= "(Not Available)", 
                               #                              multiple= TRUE)),
                               conditionalPanel("input.trainbutton > 0", 
                                                actionButton("save_som", "Save SOM Results")),
                               uiOutput("save_som_notice"),
                               plotOutput("somplot")),
                    
                  
                      
                      "Run Scenario Simulations",                      
                   
                      tabPanel("5. Simulate your scenarios, policies, and interventions", 
                              h3("STEP 5: USING YOUR MODEL TO RUN SCENARIO SIMULATIONS", style = "color:purple"),
                              h4("Here we will use your model to explore different scenarios, policies, and interventions."),
                              h4("To do that, we will be using your k-means clusters and your SOM AI solution and grid"),
                              
                              p(HTML('For TUTORIALS on using your model to run scenario simulations in COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                     target="_blank">CLICK HERE</a>')), 
                              p(HTML('To learn more about case-based scenario simulation <a href="https://www.art-sciencefactory.com/case-based%20microsimulation.pdf" target="_blank">CLICK HERE</a>')),  
                              
                              h4("The grid below visually displays the results of your k-means and SOM AI cluster solutions.", style = "color:green"),                              
                              
                              tags$b("To Run Model",
                              tags$ol(tags$li("Start by clicking on MODEL SETUP, which creates the SOM grid created with TAB4"), 
                                    tags$li("The grid you see is based on the SOM solution you arrived at using TAB3"),
                                    tags$li("Next, click the RUN CLUSTERS tab, which places your k-means solution on the SOM grid"),
                                    tags$li("These are the k-means clusters you settled on using TAB2"),
                                    tags$li("Next, make changes to the various profile of variables for each of the cases."),
                                    tags$li ("Once done, click on RUN CLUSTERS again, to see if and where on the grid the cluster moved"),
                                    tags$li ("Next, look at the BARPLOT grid to see what profile of factors account for the new grid placement"),
                                    tags$li ("Is this where you wanted your cluster to arrive?  If not, try changing something else"),
                                    tags$li ("If satisfied with your solution, run SENSITIVITY ANALYSIS; if not, click MODEL SETUP to reset"))),
                              
                              tags$b("To Run Sensitivity Analysis",
                              tags$ol(tags$li("Pick the CLUSTER you are testing from the options"), 
                                      tags$li ("Decide how much to dither your solution by in order to account for variance and error that go with any real-world estimation of change"),
                                      tags$li ("Run the sensitivity analysis"),
                                      tags$li ("NOTE: very complex solutions can several minutes or hours to finish", style = "color:purple"))),
                              
                                    verbatimTextOutput("Agent_Warning"),
                
                               hr(),
                               fluidRow(
                                 column(2,
                                        actionButton(inputId = "Agent_Setup", label="Model Setup",
                                                     style = "foreground-color:white; 
                                                     background-color:khaki;
                                                     color:black;
                                                     float:center;
                                                     height: 50px;
                                                     width: 150px;
                                                     text-align:center;
                                                     border-color:olive;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        actionButton(inputId = "Agent_Run_Clusters", label="Run Clusters",
                                                     style = "foreground-color:white;
                                                     background-color:lavender;
                                                     color:black;
                                                     height: 50px;
                                                     width: 150px;
                                                     text-align:center;
                                                     border-color:lightslategray;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                         actionButton(inputId = "SensitivityAnalysis", label="Sensitivity",
                                                      style = "foreground-color:white; 
                                                    background-color:darksalmon; 
                                                    color:black;
                                                    height: 50px;
                                                    width: 150px;
                                                    text-align:center;
                                                    border-color:lightslategray;
                                                    border-radius: 5px;
                                                    border-width: 5px"),
                                        uiOutput('cluster_sensitivity'),
                                        
                                        hr()
                                        
                                        ),
                                 column(10,
                                       conditionalPanel("input.Agent_Setup > 0",
                                       plotOutput("somplotagent", width ="700px", height = "600px"),
                                     
                                       rHandsontableOutput("clusters_editable_table"),
                                       actionButton("back_cluster", "<<"),
                                       actionButton("forward_cluster", ">>"),
                                       plotOutput("sensitivity_barplot"),
                                       plotOutput("agent_somplot")
                                       
                                    
                                      )))
                             
                              ),

                    "Run Data-forecasting/classification",                      
                    
                            tabPanel("6. Use AI to predict the cluster membership of new cases", 
                                  h3("STEP 6: USING YOUR SOM AI TO PREDICT THE CLUSTER MEMBERSHIP OF NEW CASES", style = "color:purple"),
                                  h4("Here we will use your trained SOM GRID (TAB 4) to predict the cluster profile(s) that best represent a new set of cases"),
                                  p(HTML('For TUTORIALS on using your SOM GRID for data forcasting in COMPLEX-IT <a href= 
                                                    "https://www.art-sciencefactory.com/tutorials.html"
                                    target="_blank">CLICK HERE</a>')), 
                                  p(HTML('To learn more about AI (machine learning) for data forecasting and prediction <a href="https://en.wikipedia.org/wiki/Machine_learning" target="_blank">CLICK HERE</a>')),  
                        
                                  tags$b("RUNNING THE PREDICT TAB:",
                                         tags$ol(tags$li("To begin, you need to convert your new dataset into a CSV file"),
                                                 tags$li("This CSV file can be comprised of a single new case or a large dataset of new cases"),
                                                 tags$li("Decide to use the SOM solution from your current session or a previously saved SOM solution"), 
                                                 tags$li("Click CLASSIFY PROFILES", style = "color:green"),
                                                 tags$li("NOTE: you can find your results saved in the GENERATE REPORT TAB", style = "color:purple"),
                                                 tags$li("The programme will crash if the headers/format of your new dataset are not the same as the TAB 1 dataset", style = "color:purple"))),

                                  tags$b("INTERPRETING YOUR RESULTS:",
                                         tags$ol(tags$li("After you run the data, you get a list of each case"),
                                                 tags$li("For each case, you will see its variable profile"),
                                                 tags$li("This is followed by the SOM grid quadrant that best fits it"), 
                                                 tags$li("For validity purposes, COMPLEX-IT also provides the second best grid quadrant fit"),
                                                 tags$li("NOTE: For advanced users, goodness-of-fit for classification is based on a numeric tolerance defined as
                                  10^(-10)", style = "color:purple"))),
                                  verbatimTextOutput("Predict_Warning"),
                                  
                             br(),
                             fileInput('file_pred', 'Choose CSV File', accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
                             checkboxInput('load_prev_som', 'Use Previous SOM Solution? If unchecked it will use SOM solution from this session.'),
                             selectInput('sep_pred', 'Separator:',
                                         c("Comma","Semicolon","Tab","Space"), 'Comma'),
                             actionButton('classify_prof', 'Classify Profiles'),
                             conditionalPanel("input.classify_prof > 0",
                                              numericInput("nrow.result_pred","Number of rows in the results:" ,20, min = 1, max = 100),
                                              tableOutput("view_predict")),
                             plotOutput("predict_somplot")
                             
                             ),
                    
                    "Systems Mapping Tab", 
                    tabPanel("7. Using Sytems Mapping To Explore Cluster Variables",
                      
                      # App title ----
                      #titlePanel("STEP 8: USING SYSTEMS MAPPING TO EXPLORE CLUSTER VARIABLES"),
                      
                      tags$h3("STEP 8: USING SYSTEMS MAPPING TO EXPLORE CLUSTER VARIABLES", style = "text-align: center;"), 
                      
                      tags$h4(HTML("Here we will use Systems Mapping to visually explore the configuration of variables you used to cluster your data. <br>
    
                 The map is generated using the <a href='https://dictionary.apa.org/zero-order-correlation'>zero-order correlations</a> amongst your variables."), style = "text-align: center;"),
                      
                      verbatimTextOutput("network_warning"),
                      
                      # Sidebar layout with input and output definitions ----
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          shinyjs::useShinyjs(),
                          
                          ######################## MAIN OPTIONS BOX START ########################
                          box(id = "intro_box", width = "800px", 
                              
                              actionButton("infoButton", "Info"),
                              
                              actionButton("initialise_button", "Initialise"),
                              
                              selectInput(inputId = "cluster",
                                          label = "What Cluster would you like to analyse?",
                                          choices = NULL, 
                                          selected = 'All', 
                                          multiple = FALSE),
                              
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
                              
                              # Adding dropdown to change network ----
                              selectInput("layout", "Choose layout algorithm:",
                                          c("Circle" = "layout_in_circle",
                                            "Random" = "layout_randomly",
                                            "Davidson-Harel" = "layout_with_dh", 
                                            "Fruchterman-Reingold" = "layout_with_fr", 
                                            "Sugiyama" = "layout_with_sugiyama"), 
                                          selected = T)),
                          
                          # Adding dropdown to change network ----
                          radioButtons(inputId = "remove_unconnecteds", 
                                       label = "Remove Nodes with No Connections?", 
                                       c("No", "Yes")),
                          ######################## MAIN OPTIONS BOX START ########################
                          
                          ######################## ADVANCED OPTIONS CHECKBOX START ########################
                          box(id = "advancedOptionsBox", title = "Advanced Options", width = "800px",
                              
                              # Choose how to present line thickness
                              radioButtons(inputId = "LineThickness", 
                                           label = "How to Determine Line Thickness", 
                                           c("Threshold"="binary", "Gradation"="bins")),
                              
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
                              
                              # Adding checkboxes to change network ----
                              numericInput(inputId = "seed",
                                           label = "Set Seed (for reproducible results)",
                                           value = 1, 
                                           min = 1, 
                                           max = 10000)),
                          
                          actionButton(inputId = "advancedOptions", label = "Show / Hide"),
                          
                          
                          
                          ######################## ADVANCED OPTIONS CHECKBOX END ########################
                          
                          
                          ######################## EGO NETWORK CHECKBOX START ########################
                          box(id = "egoNetworkBox", title = "Ego Network", width = "800px",
                              
                              # Adding checkbox for egonetwork ----    
                              checkboxInput(inputId = "ego_network", 
                                            label = "Make ego-network?", 
                                            value = FALSE),
                              
                              # Adding slider for degrees ----
                              numericInput(inputId = "degree",
                                           label = "No. Degrees:",
                                           min = 1,
                                           max = 100,
                                           value = 1, 
                                           step = 1),
                              
                              # Adding target node ----
                              selectInput("target_node", "Target Node ID:",
                                          c(NULL), 
                                          selected = F)),
                          
                          
                          
                          actionButton(inputId = "egoNetwork", label = "Show / Hide"),
                          ######################## EGO NETWORK CHECKBOX END ########################
                          
                          ######################## EGO NETWORK CHECKBOX START ########################
                          box(id = "shortestPathsBox", title = "Shortest Paths", width = "800px",
                              
                              # Adding checkbox for egonetwork ----    
                              checkboxInput(inputId = "shortest_paths_toggle", 
                                            label = "Show Shortest Paths?", 
                                            value = FALSE),
                              
                              # Adding target node ----
                              selectInput("from_node", "What node to start at?",
                                          c(NULL), 
                                          selected = F), 
                              
                              # Adding target node ----
                              selectInput("to_node", "What node to go to?",
                                          c(NULL), 
                                          selected = F)), 
                          
                          actionButton(inputId = "shortestPaths", label = "Show / Hide"),
                          ######################## EGO NETWORK CHECKBOX END ########################
                          
                          ######################## EGO NETWORK CHECKBOX START ########################
                          box(id = "weightsBox", title = "Edge Weights Options", width = "800px",
                              
                              # Adding information panel ----
                              helpText("Reminder: These are all the possible edges between every node. Only put
                         your user defined weights for the edges you are interested in and upload these below. Remember, 
                         the network map is undirected, to from A to B and from B to A are synonymous."), 
                              
                              # Adding checkbox for egonetwork ----    
                              downloadButton("downloadData", label = "Download"),
                              
                              checkboxInput(inputId = "include_weights", 
                                            label = "Include Edge Weights?", 
                                            value = FALSE),
                              
                              fileInput(inputId = "weights_values", multiple = F, accept = ".csv", label = "Weights Values")),
                          
                          actionButton(inputId = "weightsOptions", label = "Show / Hide"),
                          ######################## EGO NETWORK CHECKBOX END ########################
                          
                          ######################## EXPORT OPTIONS CHECKBOX START ########################
                          box(id = "exportOptionsBox", title = "Export Options", width = "800px",
                              
                              # Adding action buttons for various outputs to change network ----
                              textInput(inputId = "title", label = "title", value = NULL), 
                              textInput(inputId = "subtitle", label = "subtitle", value = NULL), 
                              textInput(inputId = "footer", label = "footer", value = NULL),
                              
                              downloadButton('htmlSave', 'Download', label = "Download your network as an HTML file"),
                              # downloadButton('pngSave', 'Download', label = "Download your network as an PNG file"),
                              downloadButton("nodesDownload", "Download", label = "Download your network's nodes"),
                              downloadButton("edgesDownload", "Download", label = "Download your network's edges")),
                          
                          actionButton(inputId = "exportOptions", label = "Show / Hide"),
                          
                          ######################## EXPORT OPTIONS CHECKBOX END ########################
                          
                        ),
                        
                        # Main panel for displaying outputs ----
                        mainPanel(
                          
                          # Output: Histogram ----
                          visNetworkOutput("networkPlot"),
                          
                          # Input: Node selection
                          selectInput("chosen_node", "Examine node:",
                                      c(NULL), 
                                      selected = F), 
                          
                          htmlOutput("text", inline = T),
                          
                        )
                      )),
                              
                    "Export Your Results",                      
                    tabPanel("8. Generate your report", 
                              h3("STEP 8: GENERATING A REPORT FROM YOUR VARIOUS ANALYSES", style = "color:purple"),
                              h4("Here you be able to create and download a report of all your key statistical and visual information."),
                              p(HTML('For TUTORIALS on what is contained in a COMPLEX-IT report, <a href= 
                                    "https://www.art-sciencefactory.com/tutorials.html"
                                    target="_blank">CLICK HERE</a>')), 
                            
                             p("NOTE: The Files from your report will be placed in a zip file
                               and downloaded to your default download directory. Please also note, you will
                               only receive results from the sections you have used in this session. And, finally, only your
                               most recent analysis -- kmeans, SOM or policy prediction, etc -- will be downloaded.", style = "color:green"),
                             downloadButton('downloadReport', 'Download Report')
                    ),
                      tabPanel("Help",
                               h3("Help Documentation"),
                               p(HTML('To view TUTORIALS and USER HELP FILES for COMPLEX-IT report, <a href= 
                                    "https://www.art-sciencefactory.com/tutorials.html"
                                  target="_blank">CLICK HERE</a> (Note internet connection required)'))
                               
                      ),
                      widths = c(3,9)
                      
                      )
                      )
                               
                
                
                               




