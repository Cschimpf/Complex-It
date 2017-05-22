library(shiny)
library(SOMbrero)
library(cluster)
library(FactoMineR)
library(plot3D)

ui <- fluidPage(
  
  titlePanel("Complex-It Version 0.1"),
  
  sidebarLayout(
    
    sidebarPanel(
      imageOutput("complexit_logo", inline=TRUE)
                ),
      
    mainPanel(
      tabsetPanel(
        
        tabPanel("Import Data",
                 h3("First step: import data"),
                 
                 p(HTML("To run the application, import your data set using the
                        import button below. Your data must be supplied in the form of a csv file.
                        If the importation is done properly, a preview of the data is displayed below.
                        When this is done, choose the SOM type of the left hand side panel and proceed
                        to the next step: self-organize a map.")),
                 p(HTML('This could be a place to link example datasets
                        <a href= 
                        "http://www.google.com"
                        target="_blank">complex attractor</a>, <a href= 
                        "http://www.google.com"
                        target="_blank">another example</a> and <a href= 
                        "http://www.google.com"
                        target="_blank">geospatial example </a>(download these files on your computer and
                        proceed).')),
                 br(), 
                 fileInput('file1', 'Choose CSV File', accept = c(
                   "text/csv",
                   "text/comma-separated-values,text/plain",
                   ".csv")),
                 checkboxInput('header', ' Header?', TRUE),
                 checkboxInput('rownames', ' Row names?', FALSE),
                 selectInput('sep', 'Separator:',
                             c("Comma","Semicolon","Tab","Space"), 'Comma'),
                 selectInput('quote', 'Quote:',
                             c("None","Double Quote","Single Quote"), 
                             'Double Quote'),
                 selectInput('dec', 'Decimal mark', c("Period", "Comma"),
                             'Period'),
                 numericInput('nrow.preview','Number of rows in the preview:',20),
                 numericInput('ncol.preview', 'Number of columns in the preview:',
                              10),
                 helpText("Note: Even if the preview only shows a restricted
                          number of observations, the map will be based on the full dataset."),
                 tableOutput("view")
          
        ),
        
        tabPanel("Cluster Data",
                 h3("Cluster Data - Under Construction"),
                 p(HTML("Here is a default explanation Corey hasn't filled out yet.")),
                 br(),
                 helpText("Select display options."),
                 checkboxInput('silhouette', 'Silhouette?'),
                 checkboxInput('pseudo_f', 'Pseudo F?'),
                 numericInput(inputId = "clusters", label = "Select the number of clusters", value = 2, min = 2),
                 actionButton(inputId = "init_kmeans", label="Get Clusters"),
                 tableOutput("kmeans_tab"),
                 textOutput("pseudoF"),
                 plotOutput(outputId = "kmeans_silh", inline=TRUE)
          #width = "50%", height = "800px"
        ),
        
        tabPanel("Self-Organize",
                 h3("Train the self-organizing map and project the cluster results"),
                 p(HTML("Need to come up with some introduction/instruction text here.")),
                
                 actionButton("trainbutton","Train SOM"),
                 #verbatimTextOutput("summary") #prints the output of a reactive variable, can show status of uploaded
                 #data, check the original SOMbrero 
                 br(), br(),
                 
               
                 
                 br(), br(),
                 h4("Options"),
                 uiOutput("varchoice"),
                 numericInput("dimx", "Map dimension X:", 5, min= 1),
                 numericInput("dimy", "Map dimension Y:", 5, min= 1),
                 h4("Advanced options"),
                 selectInput("affectation", "Affectation type:", 
                             c("standard","heskes")),
                 uiOutput("initproto"),
                 numericInput("maxit", "Max. iterations:", 500),
                 uiOutput("disttype"),
                 selectInput("radiustype", "Radius type:", 
                             c("letremy","gaussian")),
                 plotOutput(outputId = "som_3Dplot", width = "50%", height = "500px")
        ),
        tabPanel("Help",
                 h2("Under Construction")
                 
        )
        
      )
    )
    )
  
  
)



