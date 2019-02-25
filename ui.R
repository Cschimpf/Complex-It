library(shiny)
library(shinythemes)
library(shinyFiles)
library(rhandsontable)



# here is the first option using shiny themes:  
ui <- fluidPage(theme=shinytheme("cosmo"),


# here is the next option embedding the tag in native html,
# ----------
# tags$head(
# tags$link(rel = "stylesheet", type = "text/css", href = "sketchy.css")
 #tags$style("#nrow.preview {font-size:10px;height:10px;}"),
# ),
# ----------
                #imageOutput("complexit_logo", inline=TRUE),

                titlePanel("Complex-It 1.0.0 Beta"),
                
                #sidebarLayout(
                  

                 #sidebarPanel(
                  
                  #width = 2
                  #),
                  
                  #mainPanel(
                  navlistPanel(
                    selected = "1. Build Database and Import Cases",
                    HTML("<img src='Complexit_LOGO4.png'>"),
                    HTML("<br><br><br><br>"), HTML("<br><br><br><br>"), HTML("<br><br><br><br>"), HTML("<br>"),
                    "Build the Model",
           
                      tabPanel("1. Build Database and Import Cases",
                               h3("Here we will create your EXCEL database and import it into COMPLEX-IT"),
                             
                               p(HTML("To help with the process, complete the following two steps.")),
                               
                               tags$b("BUILDING YOUR DATABASE:",
                                      tags$ol(tags$li("Step 1: You need to get all of your cases and profile variables into a single database"), 
                                              tags$li("Step 2: You need to convert this database into an EXCEL comma separated database"))),



                                              p(HTML('For help on building an EXCEL database, See <a href= 
                                                     "https://support.office.com/en-us/article/video-get-to-know-excel-2010-create-your-first-spreadsheet-3323c699-ca68-448e-ab44-12b8e348bbf5"
                                                     target="_blank">CLICK HERE</a>')),

                                              p(HTML('For help on converting a database to EXCEL, See <a href= 
                                                     "https://support.office.com/en-us/article/export-data-to-excel-64e974e6-ae43-4301-a53e-20463655b1a9"
                                                      target="_blank">CLICK HERE</a>')),
                               
                               
                                              p(HTML('For help exporting an EXCEL database as CVS comma delimited, See <a href= 
                                                    "https://support.office.com/en-us/article/import-or-export-text-txt-or-csv-files-5250ac4c-663c-47ce-937b-339e391393ba"
                                                    target="_blank">CLICK HERE</a>')),
                               

                               tags$b("IMPORT YOUR DATA",
                                      tags$ol(tags$li("To start an analysis session, import your data set using the
                                      'Browse...' button below."), 
                                      tags$li("Your data must be in the form of a csv file. Note that when you 
                                      download Complex-It from GitHub, several datasets  will be downloaded with it. Explore the '/data' folder inside the Complex-It folder for further analysis opportunities. "),
                                      tags$li(" If the data is successfully imported, a preview of your data will be displayed. You may
                                      also subset the data by deselecting variables and pressing the 'Subset data' button."))),
                               
                               p(HTML('The Complex-It team extends a big thanks to the SOMbrero team from which our
                                      package draws on SOM training and visualization functions. For more SOM analysis
                                      visit the SOMbrero <a href= 
                                      "https://cran.r-project.org/web/packages/SOMbrero/index.html"
                                      target="_blank">package site.</a>')),
                               br(), 
                               fileInput('file1', 'Choose CSV File', buttonLabel='Browse',accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")
                                  ),
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

                      tabPanel("2. Cluster Cases",
                               h3("Here we will use k-means (a clustering algorithm) to group your cases into self-similar 'clusters'"),

                               p(HTML('For a basic introduction to k-means, See <a href= 
                                                    "https://en.wikipedia.org/wiki/K-means_clustering"
                                      target="_blank">CLICK HERE</a>')),                               
                               p(HTML("To help with the process, complete the following two steps.
                                      <BR> NOTE: Answers such as 'I do not know' or 'not sure' are acceptable.")),
                               
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
                                              tags$li("If satisfied, do you want to save your results?"))),
                      
                               p(HTML("SOME DEFINITIONS: The Pseudo F provides a measure of how distinct and well defined the clusters are; the larger the number, the better the fit.
                                      <BR> The Silhouette plots display how well each case fits within its respective cluster; where a score of 1 is a perfect fit; and -1 is a 
                                      perfect fit with a different, neighboring cluster.")),
                               p(HTML("<b>NOTE:</b> Save K-means results is currently disabled as we work on a more general report tab")),
                               
                                   
                               br(),
                               helpText("Select display options."),
                               checkboxInput('silhouette', 'Silhouette?'),
                               checkboxInput('pseudo_f', 'Pseudo F?'),
                               numericInput(inputId = "clusters", label = "Select the number of clusters", value = 2, min = 2),
                               actionButton(inputId = "init_kmeans", label="Get Clusters"),
                               # conditionalPanel("input.init_kmeans > 0", shinySaveButton("save", "Save results", "Save file as", 
                               #                                                           filetype=list(csv="csv"))),
                               tableOutput("kmeans_tab"),
                               textOutput("pseudoF"),
                               plotOutput(outputId = "kmeans_silh", inline=TRUE)
                               ),
                      "Test the Model",
                      tabPanel("3. The Computer's Turn",
                               h3("Here we will use artificial intelligence to check the accuracy of your k-means cluster solution"),

                               p(HTML('For a basic introduction to the Kohonen Self-Organizing Map (SOM), which is the artificial intelligence we will use, <a href= 
                                      "https://en.wikipedia.org/wiki/Self-organizing_map"
                                      target="_blank">CLICK HERE</a>')),                   
                               p(HTML("The utility of the SOM is that it plots your cases and their cluster memberhip on a two-dimensional map,
                                based on the complex relationships amongst the profile of variables upon which they are based.
                                In TAB 5 'COMPARE AND VISUALIZE RESULTS,' this map then can be visually explored to see how useful your k-means solution was,
                                as well as how the cases and clusters all relate to each other, based on differences in their respective variable profiles.")),
                                           
                               tags$b("NEW USERS:",
                                      tags$ol(tags$li("For those new to the SOM, we recommend using all of the defaults"), 
                                              tags$li("If, however, you have more than 25 clusters, you need to make the map bigger, for example 6X6"))),

                               tags$b("EXPERIENCED USERS:",
                                      tags$ol(tags$li("The SOM algorithm we use comes from the SOMbrero R-Package"))), 
                                      p(HTML('To visit the SOMbrero CRAN site, including downloading its reference manual <a href="https://cran.r-project.org/web/packages/SOMbrero/index.html"
                                      target="_blank">CLICK HERE</a>')),
                               p(HTML('Here, also, is a link to the tutorial SOMbrero provides on how it employs the SOM algorithm. 
                                      <a href="https://cran.r-project.org/web/packages/SOMbrero/vignettes/doc-numericSOM.html"
                                      target="_blank">CLICK HERE</a>')), 
                                     
                               
                               actionButton("trainbutton","Train SOM"),
                               #verbatimTextOutput("summary") #prints the output of a reactive variable, can show status of uploaded
                               #data, check the original SOMbrero 
                               br(), br(),
                               verbatimTextOutput("som_dimwarning"),
                               uiOutput("trainnotice"),
                               br(),
                               uiOutput("somsummary"),
                               br(), 
                               h4("Options"),
                               
                               numericInput("dimx", "Map dimension X:", 5, min= 3, max= 15),
                               numericInput("dimy", "Map dimension Y:", 5, min= 3, max= 15),
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
                    
                      tabPanel("4. Compare and Visualize Results",
                               h3("Here we visualize the results of the k-means and compare them to the Computer's results"),
                               
                                p(HTML("The utility of the SOM is that it plots your cases and their k-means cluster memberhip on a two-dimensional map,
                                      based on the complex relationships amongst the profile of variables upon which they are based.
                                      In this tab, this map can be visually explored to see how useful your k-means solution was,
                                      as well as how the cases and clusters all relate to each other, based on differences in their respective variable profiles.")),
                               p(HTML('NOTE! Most of the visualization options used here come from the SOMbrero R-Package.
                                      We strongly recommend reviewing the tutorial they provide, as it walks users through many of the visualization options available here 
                                      <a href="https://cran.r-project.org/web/packages/SOMbrero/vignettes/doc-numericSOM.html"
                                      target="_blank">CLICK HERE</a>')),  
                               
                                                            
                               tags$b("READING THE MAP:",
                                      tags$ol(tags$li("To begin, we label each case with its CASE ID and K-MEANS ID."),
                                              tags$li("To see these IDs, for 'PLOT WHAT?' select observations; and for 'TYPE OF PLOT' select names."),
                                              tags$li("The first ID on the map is the k-means and the second ID is the case."), 
                                              tags$li("The Map also places cases in the particular quadrant to which they below, based on profile differences."),
                                              tags$li("The more similar the profile, the closer the cases; the more profiles differ, the further away cases are."),
                                              tags$li("The PROTOTYPES option (i.e., variables) shows how the profile of variables influenced where cases are located."),
                                              tags$li("The BARPLOT option for both OBSERVATIONS and PROTOTYPES shows the profile of variables for each quadrant."),
                                              tags$li("The line in the BARPLOT is z-score=0; above the line is more of a variable;below the line is less."),
                                              tags$li("For advanced users, the profiles are saved in an EXCEL file in R."))),
 
                               tags$b("INTERPRETING YOUR RESULTS:",
                                      tags$ol(tags$li("Looking at the Names Map, are cases with similar k-means IDs located in similar quadrants?"),
                                              tags$li("If yes, do you think the SOM and k-means are reasonably similar solutions? Or, should you re=run your k-means?"),
                                              tags$li("How do the profiles account for the different cluster solutions and the quadrant locations of the cases?"),
                                              tags$li("What factors (i.e., variables) seem to have the biggest impact on different clusters or the model as a whole?"),
                                              tags$li("How does the data solution differ from your hypotheses back at the design phase of COMPLEX-IT?"),
                                              tags$li("Are you satisfied with your solution?  If so, CLICK on the Save SOM Results tab."))),
                               

                               
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
                    
                  
                      
                      "Extend the Model",                      
                      tabPanel("5. Predict New Cases", 
                               h3("Use your solution to predict the quadrant membership of new or different cases"),
                               p(HTML('Predict the classification of a new or different case
                                  <br><i>Goodness of fit for classification is based on a numeric tolerance defined as
                                  10^(-10)</i>'
                                 )),
                               br(),
                               fileInput('file_pred', 'Choose CSV File', accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
                               checkboxInput('header_pred', ' Header?', TRUE),
                               checkboxInput('load_prev_som', 'Use Previous SOM Solution? If unchecked it will use SOM solution from this session.'),
                               selectInput('sep_pred', 'Separator:',
                                           c("Comma","Semicolon","Tab","Space"), 'Comma'),
                               selectInput('quote_pred', 'Quote:',
                                           c("None","Double Quote","Single Quote"), 
                                           'Double Quote'),
                               actionButton('classify_prof', 'Classify Profiles'),
                               conditionalPanel("input.classify_prof > 0",
                                                numericInput("nrow.result_pred","Number of rows in the results:" ,20, min = 1, max = 100),
                                                tableOutput("view_predict"))
                               
                      ),
                      tabPanel("6. Simulate Policy Scenarios", 
                               h3("Case-Based Multi-Agent Modeling:"),
                                  
                               p(HTML("Use your SOM solution to create a simulated multi-agent environment 
                                      for evaluating how policies impact the cases in your study. NOTE. This
                                      is an experimental tab. <br>
                                      What constitutes the credibility of the results of policy? <br>
                                      Are you going to be predicting? If so, consider pulling off a quarter of your data")),
                              tags$b("To Use:",
                              tags$ol(tags$li("Upload data under Import Data"), 
                                                         tags$li("Press Model Setup"),
                                                         tags$li("Clusters from your analysis will be selected randomly, only 6 can be selected"),
                                                         tags$li("You can edit the data for the cluster or which clusters are included - MAX of 6 clusters"),
                                                         tags$li("Press Run Cases to see where the edited cluster centroids fall on the SOM grid. This is the same grid as Plot Map"))),
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
                                        # actionButton(inputId = "Agent_Use_Prev_SOM", label="Use Previous SOM",
                                        #              style = "foreground-color:white; 
                                        #              background-color:turquoise; 
                                        #              color:black;
                                        #              height: 50px;
                                        #              width: 150px;
                                        #              text-align:center;
                                        #              border-color:seagreen;
                                        #              border-radius: 5px;
                                        #              border-width: 5px"),
                                        # br(),
                                        
                                        ),
                                 column(10,
                                       conditionalPanel("input.Agent_Setup > 0",
                                       plotOutput("somplotagent", width ="700px", height = "600px"),
                                     
                                       rHandsontableOutput("clusters_editable_table"),
                                       actionButton("back_cluster", "<<"),
                                       actionButton("forward_cluster", ">>"),
                                       plotOutput("sensitivity_barplot"),
                                       plotOutput("agent_somplot")
                                       
                                       # rHandsontableOutput("cases_editable_table"),
                                       # actionButton("back_case", "<<"),
                                       # actionButton("forward_case", ">>" )
                                      )))
                               
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
                              
                    "Export Results",                      
                    tabPanel("7. Generate Report", 
                             h3("Generate a report and exportable datasets from your analysis"),
                             p("Here you can download a report. The Files will be placed in a zip file
                               and downloaded to your default download directory. Please note, you will
                               only receive results from sections you have used in this session. Only your
                               most recent analysis-kmeans, SOM or policy prediction, will be downloaded."),
                             downloadButton('downloadReport', 'Download Report')
                    ),
                      tabPanel("Help",
                               h3("Under Construction")
                               
                      ),
                      widths = c(3,9)
                      
                      )
                      )
                               
                
                
                               




