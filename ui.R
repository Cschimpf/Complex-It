library(shiny)
library(shinythemes)
library(shinyFiles)
library(rhandsontable)



#ui <- fluidPage(
# here is the first option using shiny themes:  
#----------------
ui <- fluidPage(theme=shinytheme("cosmo"),
#----------------

# here is the next option embedding the tag in native html,
# ----------
# tags$head(
# tags$link(rel = "stylesheet", type = "text/css", href = "sketchy.css")
 #tags$style("#nrow.preview {font-size:10px;height:10px;}"),
# ),
# ----------


                titlePanel("Complex-It 0.1.6 Alpha"),
                
                sidebarLayout(
                  

                 sidebarPanel(
#                  wellPanel(
                  imageOutput("complexit_logo", inline=TRUE),
                  width = 2
                  ),
                  
                  mainPanel(
                    tabsetPanel(
                    
                      tabPanel("1. Design Initial Model",
                               h3("Before Any Data Can Be Analyzed, We Need to Design Your Initial Model and Research Questions"),
                               
                               p(HTML("To help with the process, answer the following questions.  
                                      <BR> NOTE: Answers such as 'I do not know' or 'not sure' are acceptable.")),
                               
                               tags$b("IS YOUR TOPIC A COMPLEX SYSTEM?",
                                      tags$ol(tags$li("Provide a quick paragraph summary of your topic of study"), 
                                              tags$li("What makes your topic complex?"),
                                              tags$li("Is it a complex system? And, if so, why or how?"),
                                              tags$li("Can you draw a boundary around your system?"))),

                               tags$b("CASES, VARIABLES AND PROFILES:",
                                      tags$ol(tags$li("What are the cases in your system?"), 
                                              tags$li("What are the key variables you want to study?"),
                                              tags$li("How will you measure these variables?"),
                                              tags$li("How do your variables go together to form a profile?"),
                                              tags$li("How do the variables interact or infuence one another?"),
                                              tags$li("Which variables do you think are the most important?"),
                                              tags$li("Do you have data on these variables?"),
                                              tags$li("If not, how do you plan to collect the data?"),
                                              tags$li("Any concerns with the quality of data or missing data?"),
                                              tags$li("Is this study cross-sectional or across time?"),
                                              tags$li("If across time, how many time stamps will you use?"))),
 
                               tags$b("RESEARCH QUESTIONS:",
                                      tags$ol(tags$li("What is the question or questions you want to answer?"),
                                              tags$li("Can you state them formally?"))),
                                                             

                               br() 
                               ),
           
                      tabPanel("2. Build Database and Import Cases",
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

                      tabPanel("3. Cluster Cases",
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
                                              tags$li("looking at the Silhouette, how well are the cases distributed for each cluster?"),
                                              tags$li("Should you re-run k-means to look for more or less clsuters?"),
                                              tags$li("If satisfied, do you want to save your results?"))),
                      
                               p(HTML("SOME DEFINITIONS: The Pseudo F provides a measure of how distinct and well defined the clusters are; the larger the number, the better the fit.
                                      <BR> The Silhouette plots display how well each case fits within its respective cluster; where a score of 1 is a perfect fit; and -1 is a 
                                      perfect fit with a different, neighboring cluster.")),
                               
                                   
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
                      
                      tabPanel("4. The Computer's Turn",
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
                                      tags$ol(tags$li("For those familiar with the SOM, go to the SOMbrero CRAN site for details about the algorithm"))), 
                                      p(HTML('To visit the SOMbrero CRAN site, including downloading its reference manual <a href="https://cran.r-project.org/web/packages/SOMbrero/index.html"
                                      target="_blank">CLICK HERE</a>')),  
                                     
                               
                               actionButton("trainbutton","Train SOM"),
                               #verbatimTextOutput("summary") #prints the output of a reactive variable, can show status of uploaded
                               #data, check the original SOMbrero 
                               br(), br(),
                               
                               uiOutput("trainnotice"),
                               br(),
                               uiOutput("somsummary"),
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
                      tabPanel("5. Compare and Visualize Results",
                               h3("Explore your Data"),
                               p(HTML("In this panel you can visualize the computed 
                                 self-organizing map. This panel contains several plots to analyze the map. The names plot
                                 under 'Obs' will map previous kmeans labels to cases on their respective part of the SOM <br>
                                 How did the computer compare to yours? <br>
                                 Where are the cases distributed? <br>
                                 Do the groupings look good and match your clusters?")),
                               
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

                      
                      
   #                   tabPanel("8. Debrief",
  #                             h3(" "),
  #                             p(HTML("Answer the following questions: <br>
  #                                    Which factors did the computer find most important to map the cases and clusters? (See prototypes in the bar plot) <br>
  #                                    Use prototypes to decide which variables to move. <br>
  #                                    Look at the U Matrix distances - do you have to cross a mountain to move a case?")),
  #                             br() 
  #                             ),
                      
                      
                      tabPanel("6. Assemble Working Model",
                               h3("Use everything so far to draw what you know"),
                               p(HTML("Answer the following questions: <br>
                                      Do you need to start all over? <br>
                                      If so, should you add new data or more clusters? <br>
                                      If satisfied, what next? Either Predict New Cases or Simulate Scenarios.")),
                               br() 
                               ),
                      
  tabPanel("7. Decide Next Step",
           h3("Use everything so far to draw what you know"),
           p(HTML("Answer the following questions: <br>
                  Do you need to start all over? <br>
                  If so, should you add new data or more clusters? <br>
                  If satisfied, what next? Either Predict New Cases or Simulate Scenarios.")),
           br() 
           ),
  
                      
                      
                      
                      
                      
                                            
                      tabPanel("8. Predict New Cases", 
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
                               checkboxInput('load_prev_som', 'Use Previous SOM Results?'),
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
                      tabPanel("9. Simulate Policy Scenarios", 
                               h3("Case-Based Multi-Agent Modeling:", tags$b("UNDER DEVELOPMENT")),
                                  
                               p(HTML("Use your SOM solution to create a simulated multi-agent environment 
                                      for evaluating how policies impact the cases in your study. NOTE. This
                                      is an experimental tab. Not all features are currently integrated. 
                                      The grid may not reflect the dimensions selected under Train SOM. The grid
                                      is only populated with estimated data points when pressing Run Cases. Upload
                                      features are not yet supported. This tab is under development with new updates
                                      coming in February 2018 <br>
                                      What constitutes the credibility of the results of policy? <br>
                                      Are you going to be predicting? If so, consider pulling off a quarter of your data")),
                              tags$b("To Use:",
                              tags$ol(tags$li("Upload data under Import Data"), 
                                                         tags$li("Cluster data under Cluster"),
                                                         tags$li("Train the SOM under Self-Organize"),
                                                         tags$li("View the SOM plot Plot Map"),
                                                         tags$li("Navigate to Agent tab and press Model Setup"),
                                                         tags$li("Cases from your data will be selected randomly, only 6 can be selected"),
                                                         tags$li("You can edit the data for the cases or which cases are included - MAX of 6 cases"),
                                                         tags$li("Press Run Cases to see where the edited cases fall on the SOM grid. This is the same grid as Plot Map"))),
                
                               hr(),
                               fluidRow(
                                 column(3,
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
                                        # actionButton(inputId = "Agent_Run_Clusters", label="Run Clusters",
                                        #              style = "foreground-color:white; 
                                        #              background-color:lavender; 
                                        #              color:black;
                                        #              height: 50px;
                                        #              width: 150px;
                                        #              text-align:center;
                                        #              border-color:lightslategray;
                                        #              border-radius: 5px;
                                        #              border-width: 5px"),
                                        # br(),
                                        # hr(),
                                        actionButton(inputId = "Agent_Run_Cases", label="Run Cases",
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
                                        actionButton(inputId = "Agent_Use_Prev_SOM", label="Use Previous SOM",
                                                     style = "foreground-color:white; 
                                                     background-color:turquoise; 
                                                     color:black;
                                                     height: 50px;
                                                     width: 150px;
                                                     text-align:center;
                                                     border-color:seagreen;
                                                     border-radius: 5px;
                                                     border-width: 5px"),
                                        br(),
                                        hr(),
                                        #select the various types of SOM Plots
                                        selectInput("somplotwhatagent", "Plot what?", 
                                                    choices= list("Observations"= "obs",
                                                                  "Prototypes"= "prototypes")),
                                        selectInput("somplottypeagent", "Type of plot:", 
                                                    choices= c("color", "barplot", 
                                                               "names", "boxplot")),
                                        conditionalPanel("input.somplottypeagent == 'color' ||
                                                         input.somplottypeagent == '3d'",
                                                         selectInput("somplotvaragent", 
                                                                     "Variable: (only used for '3d',
                                                                     'color' and 'boxplot' plots if available)", 
                                                                     choices= "(Not Available)")),
                                        conditionalPanel("input.somplottypeagent == 'boxplot'",
                                                         selectInput("somplotvar2agent", 
                                                                     "Variable: (hold Ctrl to select
                                                                     multiple variables)", 
                                                                     choices= "(Not Available)", 
                                                                     multiple= TRUE)),
                                        br(),
                                        hr(),
                                        #select the clusters
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
                                 column(7,
                                       conditionalPanel("input.Agent_Setup > 0",
                                       plotOutput("somplotagent", width ="600px", height = "600px"),
                                       #width = "500px", height = "500px"),  
                                       #br(),
                                       #br(),
                                     
                                       #rHandsontableOutput("centroids_editable_table"),
                                       
                                       rHandsontableOutput("cases_editable_table"),
                                       actionButton("back_case", "<<"),
                                       actionButton("forward_case", ">>" )
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
                              
                      
                      tabPanel("Help",
                               h3("Under Construction")
                               
                      )
                      
                      )
                      )
                               )
                
                
                               )




