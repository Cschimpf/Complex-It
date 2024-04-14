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
library(plotly)
library(DT)
library(fresh)
library(plotly)
library(shinycssloaders)


server <- function(input, output, session) {
  
  output$complexit_logo <- renderImage({list(src="Complexit_LOGO3.png")}, deleteFile = FALSE)
  
  
  ########################### PANEL 'IMPORT DATA' ############################
  ############################################################################
  
  pop_ups <- reactiveValues(pop_up_intro=FALSE,
                            pop_up_clusters=FALSE,
                            pop_up_systems=FALSE,
                            pop_up_SOM=FALSE,
                            pop_up_plot_map=FALSE,
                            pop_up_new_prediction=FALSE,
                            pop_up_scenarios=FALSE)
  
  observe({
    # Check if the tab "Using Sytems Mapping To Explore Cluster Variables" is selected
    if (!is.null(input$tabs) && input$tabs == "importing" && !pop_ups$pop_up_intro ) {
      shinyalert(
        title = "<u><b>Welcome to COMPLEX-IT</b></u>",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "Let's Go!",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        animation = TRUE,
        imageUrl = "https://static.wixstatic.com/media/d66b8f_9f553b735eec47df906350831599a8ef~mv2.jpg/v1/fill/w_460,h_236,al_c,q_80,usm_0.66_1.00_0.01,enc_auto/Logo2.jpg",
        imageWidth = 460,
        imageHeight = 236
      )
      pop_ups$pop_up_intro <- TRUE
    }
  })
  
  uploaded_data_values <- reactiveValues(display_data=NULL, current_data_file=NULL)
  
  observe({
    
    if (is.null(input$file1))
      return(NULL)
    
    
    the.sep <- switch(input$sep, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
    
    
    the.table <- na.omit(read.csv(input$file1$datapath, header=input$header,
                                  sep=the.sep))
    
    #right now this just deselects not numeric data columns
    output$varchoice <- renderUI(div(
      selectInput(inputId="varchoice", label="Input variables:", multiple = TRUE,
                  choices=as.list(colnames(the.table)[
                    sapply(the.table, class) %in%
                      c("integer", "numeric")]),
                  selected=as.list(colnames(the.table)[
                    sapply(the.table, class) %in%
                      c("integer", "numeric")])),
      actionButton(inputId = "subset_data", label = "Subset Data")))
    
    uploaded_data_values$display_data <- the.table 
    
    numeric_only_columns <- column_type_identifier(the.table)
    current_data_file_to_assign <- the.table[numeric_only_columns]
    uploaded_data_values$current_data_file <- current_data_file_to_assign 
    
  })
  
  observeEvent(input$subset_data,{
    
    if(length(ncol(uploaded_data_values$display_data) >= length(input$varchoice))){
      uploaded_data_values$current_data_file <- uploaded_data_values$display_data[input$varchoice]
    }
    
  })
  
  output$view <- renderDT(
    
    uploaded_data_values$display_data,
    options = list(scrollX = TRUE, searching = FALSE),
    rownames = FALSE
  )
  
  
  ########################### PANEL 'CLUSTER DATA' ###########################
  ############################################################################
  
  cluster_text <- "<div style='line-height: 30px'><u><b>1) Hypothesize Your Cluster Solution:</b></u>
                 To begin, how many clusters do you think are in your database?
                 What is your hypothesis based on -- the literature, a guess, expertise, experience, a hunch?
                 How would you describe or name these different clusters?
                 How do you think your case-based profile of variables account for these different clusters?

                 <u><b>2) Run the K-Means Analysis:</b></u>
                 Run your k-means several times to see if you can improve the Pseudo F
                 How strong is the Pseudo F for your solution?
                 Looking at the Silhouette, how well are the cases distributed for each cluster?
                 Should you re-run k-means to look for more or less clsuters?

                 <u><b>Do You Have a Good Statistical Fit?</b></u>
                 The Pseudo F indicates the quality of the overall solution; the larger the number, the better the fit.
                 The Silhouette displays how well each case fits within its respective cluster; where a score of 1 is a perfect fit.

                 <u><b>Need Help?</b></u>
                 For tutorials on using the SOM AI in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a>
                 For a basic introduction to k-means <a href='https://en.wikipedia.org/wiki/K-means_clustering'>click here</a></div>"
  
  
  cluster_text <- gsub("\n", "<br>", cluster_text) 
  
  observe({
    # Check if the tab "Using Sytems Mapping To Explore Cluster Variables" is selected
    if (!is.null(input$tabs) && input$tabs == "cluster_cases" && !pop_ups$pop_up_clusters ) {
      shinyalert(
        title = "<u><b>Using the K-Means Clusters</b></u>",
        text = cluster_text,
        size = "l",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_ups$pop_up_clusters <- TRUE
    }
  })
  
  observeEvent(input$infoButton_kmean, {
    shinyalert(
      title = "<u><b>Using the K-Means Clusters</b></u>",
      text = cluster_text,
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  kmeans_solution <- reactiveValues(current_kmeans_solution=NULL)
  
  observeEvent(input$init_kmeans, {
    if(is.null(uploaded_data_values$current_data_file)){
      shinyalert(
        title = "Please Upload Data First",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()}
    else {output$kmean_warning <- renderText({""})}
    if(input$setrandseedkmean == "Yes") {set.seed(input$randseedkmean)}
    else {set.seed(sample(1:9999, size= 1))}
    
    kmeans_solution$current_kmeans_solution <- create_user_gen_kmeans_solution("default_name", kmeans(uploaded_data_values$current_data_file, isolate(input$clusters)))
    
    
    output$kmeans_title <- renderUI({
      h4("Kmeans Cluster Centroids")
    })
    
    
    
    kmeans_table <- reactive({
      
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label <- generate_cluster_labels(kmeans_solution$current_kmeans_solution)
      summary_row <- generate_data_summary(uploaded_data_values$current_data_file)
      cen_tab <- cbind("Cluster" = clus_label, "Size" = kmeans_solution$current_kmeans_solution@usize, round(kmeans_solution$current_kmeans_solution@ucenters, digits = 3))
      cen_tab <- rbind(cen_tab, summary_row)
      
    })
    
    output$kmeans_tab <- renderDT(
      
      kmeans_table(),
      options = list(scrollX = TRUE, searching = FALSE),
      rownames = FALSE
    )
    
    #displays the pseudoF
    FSTAT <- pseudoF(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution,input$clusters)
    output$pseudoF <- renderText({ paste("Pseudo F: ", FSTAT) })
    
    
    output$kmeans_silh <- renderPlot({
      
      plot_silhouette(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution)
      
    }, width = 500, height = graph_dimension(uploaded_data_values$current_data_file))
    
    
    # #displays the pseudoF
    # if (input$pseudo_f == TRUE) {
    #   FSTAT <- pseudoF(current_data_file, current_kmeans_solution,input$clusters)
    #   output$pseudoF <- renderText({
    #     paste("Pseudo F: ", FSTAT)
    #   })}
    # if (input$silhouette == TRUE){
    #   output$kmeans_silh <- renderPlot({
    #     plot_silhouette(current_data_file, current_kmeans_solution)
    #   }, width = 500, height = graph_dimension(current_data_file))
    # }
    
  })
  
  
  #### Panel 'Train the SOM'
  #############################################################################
  
  train_SOM_text <- "<div style='line-height: 30px'><u><b>Advice For Using the SOM AI</b></u>
                 For those new to the SOM, we recommend using all of the defaults. However, if you have over 25 clusters, you will need to make a bigger map, such as a 6x6 grid.
                 For experienced users, the SOM algorithm we use comes from the <a href='https://cran.r-project.org/web/packages/SOMbrero/index.html'>SOMbrero R-Package</a>

                 <u><b>Need Help?</b></u>
                 For tutorials on using the SOM AI in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a>
                 For a basic introduction to the Self-Organizing Map (SOM) <a href='https://en.wikipedia.org/wiki/Self-organizing_map'>click here</a></div>"
  
  
  
  train_SOM_text <- gsub("\n", "<br>", train_SOM_text) # Convert newline characters to HTML line breaks
  
  observe({
    if (!is.null(input$tabs) && input$tabs == "AI_clusters" && !pop_ups$pop_up_SOM ) {
      shinyalert(
        title = "<u><b>Using the SOM AI</b></u>",
        text = train_SOM_text,
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_ups$pop_up_SOM <- TRUE
    }
  })
  
  observeEvent(input$infoButton_som, {
    shinyalert(
      title = "<u><b>Using the SOM AI</b></u>",
      text = train_SOM_text,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  output$initproto <- renderUI({
    selectInput("initproto", label="Prototypes initialization method:",
                choices=c("random","obs","pca"),
                selected="random")
  })
  
  output$scaling <- renderUI({
    selectInput(inputId="scaling", label="Data scaling:",
                choices =c("unitvar", "none", "center"),
                selected ="unitvar")
  })
  
  som_solution <- reactiveValues(current_som_solution=NULL)
  anova_info <- reactiveValues(anova_results=NULL, anova_results_df=NULL)
  
  observeEvent(input$trainbutton, {
    
    if(input$dimx < 3 | input$dimy < 3 | input$dimx > 15 | input$dimy > 15)
    {
      output$som_warning <- renderText({"SOM dimensions must be 3 or greater and 15 or lesser."})
      return()
    }
    else if(is.null(uploaded_data_values$current_data_file)){
      shinyalert(
        title = "Please Upload Data First",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    else
    {
      output$som_warning <- renderText({""})
    }
    tmp_data <- uploaded_data_values$current_data_file
    mapped_labels = NULL
    if(!is.null(kmeans_solution$current_kmeans_solution)){
      mapped_labels <- create_kmeans_SOM_mapping(kmeans_solution$current_kmeans_solution)
      rownames(tmp_data) <- mapped_labels
    }
    
    if(input$setrandseed == "Yes") {set.seed(input$randseed)}
    else {set.seed(sample(1:9999, size= 1))}
    som_solution$current_som_solution <- trainSOM(tmp_data, dimension=c(input$dimx,input$dimy),
                                                  maxit=input$maxit, scaling=input$scaling, init.proto=input$initproto, eps0 =input$eps0)
    
    updatePlotSomVar() # update variable choice for som plots
    
    shinyjs::onclick("toggleAdvanced", shinyjs::toggle(id = "advanced_info", anim = TRUE))
    
    output$trainnotice_header <- renderUI({
      
      tagList(h3(paste("SOM trained at:", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" "), style = "text-align: center;"),
              h4("Progress to the next tab to compare your clusters to the SOM AI", style = "text-align: center;"),
              h4("If you like your SOM AI solution, you can save it in the next tab", style = "text-align: center;"),
              br(),
              h4("Users confident with the SOM AI may wish to examine the advanced statistics below", style = "text-align: center;"))
      
    })
    
    output$trainnotice_advanced_trigger <- renderUI({
      
      useShinyjs()
      
      a(id = "toggleAdvanced", "Show/hide advanced statistics")
      
    })
    
    # Create a reactive value to store the parsed dataframe
    parsed_anova_results <- reactive({
      anova_info$anova_results <- retrieve_ANOVA_results(som_solution$current_som_solution)
      
      # Function to process the list and create a dataframe
      parse_list_to_dataframe <- function(my_list) {
        # Use lapply to apply the regular expression split to each element in the list
        split_list <- lapply(my_list, function(x) {
          parts <- unlist(strsplit(x, "\\s+"))
          if (length(parts) < 4) {
            parts <- c(parts, "")
          }
          return(parts)
        })
        
        # Create a dataframe from the split list
        df <- as.data.frame(do.call(rbind, split_list))
        
        # Rename the columns
        colnames(df) <- c("Variable", "F Value", "p Value", "Significance")
        
        return(df)
      }
      
      # Call the function with your list
      my_list <- anova_info$anova_results[(length(anova_info$anova_results)-3):2]
      
      anova_info$anova_results_df <- parse_list_to_dataframe( my_list )
    })
    
    output$trainnotice_advanced_info <- renderUI({
      
      useShinyjs()
      
      ### post the quality control factors as well
      qual_measures <- quality(som_solution$current_som_solution)
      anova_results <- retrieve_ANOVA_results(som_solution$current_som_solution)
      
      shinyjs::hidden(
        div(id = "advanced_info",
            #now print out the results
            tagList(
              #p(paste("Trained SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")),
              p(paste("Topo Error  ", format(qual_measures$topographic,digits=4),sep=" ")),
              p(paste("Quant Error ", format(qual_measures$quantization,digits=4),sep=" ")),
              br(),
              p(paste("ANOVA Results")),
              #lapply(length(anova_results):1, function(i, y) { p(paste(y[i])) }, y=anova_results)
              
              p(paste(anova_results[length(anova_results)])),
              # paste(anova_results_df)
              #lapply(length(anova_results):1, function(i, y) { p(paste(y[i])) }, y=anova_results)
            ),
            renderDT(parsed_anova_results())
        )
      )
      
    })
    
    #   output$trainnotice_advanced_table <- renderUI({
    #
    #     shinyjs::hidden(
    #       div(id = "advanced_info_table",
    #           renderTable("parsed_anova_results")
    #       )
    #     )
    #
    #   })
    #
  })
  #
  #
  #
  #   #### Panel 'Plot Map'
  #   #############################################################################
  
  plot_map_text <- "<div style='line-height: 30px'><u><b>Reading the SOM Grid:</b></u>
                 1) To begin, we label each case with its CASE ID and K-MEANS ID.
                 2) To see these IDs, for 'PLOT WHAT?' select observations; and for 'TYPE OF PLOT' select names.
                 3) The first ID on the grid is the k-means cluster number; the second ID is the case.
                 4) The grid also places each case in a quadrant, based on the SOM AI solution.
                 5) The more similar the profile, the closer the cases on the grid; the more profiles differ, the further away cases are.
                 6) The PROTOTYPES option (i.e., variables) shows how your profile of variables influenced where cases are located.
                 7) The BARPLOT option for both OBSERVATIONS and PROTOTYPES shows the profile of variables for each quadrant.
                 8) The line in the BARPLOT is mean=0; above the line is more of a variable;below the line is less.
                 9) NOTE: Several of the images created here are found in the GENERATE REPORT TAB.
                 10) In addition, we recommend using SCREEN CAPTURE to save an image.

                 <u><b>Interpreting Your Results:</b></u>
                 1) Looking at the Names, are cases with similar k-means IDs located in similar quadrants?
                 2) If yes, do you think the SOM and k-means are reasonably similar solutions? Or, should you re=run your k-means?
                 3) How do the profiles account for the different cluster solutions and the quadrant locations of the cases?
                 4) What factors (i.e., variables) seem to have the biggest impact on different clusters or the model as a whole?
                 5) How does the data solution differ from your hypotheses back at the design phase of COMPLEX-IT?
                 6) Are you satisfied with your solution?  If not, go back and run your k-means and SOM again.

                 <u><b>Need Help?</b></u>
                 For TUTORIALS on visualising your data in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a>
                 The visualisations tools used for this tab come from the SOMbrero R-Package. To understand how they work <a href='https://cran.r-project.org/web/packages/SOMbrero/vignettes/c-doc-numericSOM.html'>click here</a></div>"
  
  plot_map_text <- gsub("\n", "<br>", plot_map_text) # Convert newline characters to HTML line breaks
  
  observe({
    if (!is.null(input$tabs) && input$tabs == "compare_and_visualise" && !pop_ups$pop_up_plot_map ) {
      shinyalert(
        title = "<u><b>Using the SOM AI</b></u>",
        text = plot_map_text,
        size = "l",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_ups$pop_up_plot_map <- TRUE
    }
  })
  
  observeEvent(input$infoButton_plot_map, {
    shinyalert(
      title = "<u><b>Using the SOM AI</b></u>",
      text = plot_map_text,
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  observe({
    updateSelectInput(session, "somplottype",
                      choices=all.somplot.types[["numeric"]][[
                        input$somplotwhat]])
  })
  
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    tmp.names <- colnames(som_solution$current_som_solution$data)
    
    updateSelectInput(session, "somplotvar", choices=tmp.names)
    # updateSelectInput(session, "somplotvar2", choices=tmp.names,
    #                   selected=tmp.names[1:length(tmp.names)])
    
  })
  
  
  # Plot the SOM
  somplot_output_plot <- reactive({
    
    tmp.view <- NULL
    if (input$somplottype =="boxplot") {
      # tmp.var <- (1:ncol(current_som_solution$data))[colnames(current_som_solution$data) %in%
      #                                                  input$somplotvar2]
      tmp.var <- seq(from = 1, to = ncol(uploaded_data_values$current_data_file), by = 1)
    }
    else {tmp.var <- input$somplotvar}
    
    #This if/else set is here to add cluster labels to neurons for observation plots only
    temp.dim<-som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
    
    if(input$somplotwhat =='obs' & input$somplottype == 'boxplot'){ ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, variable = tmp.var, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))) }
    else if(input$somplotwhat =='obs' & input$somplottype == 'color'){plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, variable = tmp.var, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))) }
    else if(input$somplotwhat == 'obs' & input$somplottype == 'names'){plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))}
    else if(input$somplotwhat =='obs' & input$somplottype == 'barplot'){ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))))}
    else if (input$somplotwhat == 'prototypes' & input$somplottype == 'barplot'){ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype))}
    else if (input$somplotwhat == 'prototypes' & input$somplottype == '3d'){
      # Make the data
      data_3d <- as.data.frame(som_solution$current_som_solution[["prototypes"]])[tmp.var]
      data_3d <- rename(data_3d, z = 1)
      data_3d$x <- as.data.frame(som_solution$current_som_solution[["parameters"]][["the.grid"]][["coord"]])$x
      data_3d$y <- as.data.frame(som_solution$current_som_solution[["parameters"]][["the.grid"]][["coord"]])$y
      
      # Define the dimensions of the matrix based on the range of x and y values
      num_rows <- max(data_3d$x)
      num_cols <- max(data_3d$y)
      
      # Create an empty matrix to hold the z-values
      z_matrix <- matrix(0, nrow = num_rows, ncol = num_cols)
      
      # Fill in the matrix with the z-values from the dataframe
      for (i in 1:nrow(data_3d)) {
        row_idx <- data_3d$x[i]
        col_idx <- data_3d$y[i]
        z_matrix[row_idx, col_idx] <- data_3d$z[i]
      }
      
      # Create the 3D surface plot
      plot_ly(x = ~1:num_cols, y = ~1:num_rows, z = ~z_matrix) %>%
        add_surface()
    }
    else if (input$somplotwhat == 'prototypes' & input$somplottype == 'smooth.dist'){
      # Make the data
      smooth_dist_data <- plot(x=som_solution$current_som_solution, what='prototypes', type='smooth.dist')[["data"]]
      
      # Define the dimensions of the matrix based on the range of x and y values
      num_rows <- max(smooth_dist_data$x)
      num_cols <- max(smooth_dist_data$y)
      
      # Create an empty matrix to hold the z-values
      z_matrix <- matrix(0, nrow = num_rows, ncol = num_cols)
      
      # Fill in the matrix with the z-values from the dataframe
      for (i in 1:nrow(smooth_dist_data)) {
        row_idx <- smooth_dist_data$x[i]
        col_idx <- smooth_dist_data$y[i]
        z_matrix[row_idx, col_idx] <- smooth_dist_data$z[i]
      }
      
      # Create the 3D surface plot
      plot_ly(x = ~1:num_cols, y = ~1:num_rows, z = ~z_matrix) %>%
        add_surface() %>%
        layout(
          scene = list(
            xaxis = list(title = "Insert Title",
                         tickmode = "linear",  # Use linear tick mode
                         dtick = 1             # Set tick interval to 1 (whole numbers)
            ),
            yaxis = list(title = "Insert Title",
                         tickmode = "linear",  # Use linear tick mode
                         dtick = 1             # Set tick interval to 1 (whole numbers)
            ),
            zaxis = list(title = "Insert Title")
          )
        )
    }
    else if (input$somplotwhat == 'prototypes' & input$somplottype == 'umatrix'){plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, variable=tmp.var,view=tmp.view)}
    #else {plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype, variable=tmp.var,view=tmp.view)}
    
    
  })
  
  
  
  output$somplot <- renderPlot({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
    
    
  })
  
  output$somplot_box <- renderPlotly({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_names <- renderPlot({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_color <- renderPlot({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_obs_bar <- renderPlotly({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_prototypes_bar <- renderPlotly({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_3d <- renderPlotly({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_smooth_dist <- renderPlotly({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  output$somplot_umatrix <- renderPlot({
    if(is.null(uploaded_data_values$current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    somplot_output_plot()
  })
  
  
  
  
  
  
  observeEvent(input$save_som, {
    previous_som <- som_solution$current_som_solution #need to rename the object or it may overwrite later current_som objects
    save(previous_som, file = "./tmp/SavedSOMObject")})
  # Downloadable csv of selected dataset ----
  
  output$save_som_notice <- renderUI({
    if(input$save_som == 0){
      return()
    }
    paste("Saved SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")
    
  })
  
  #### Panel 'Case Prediction'
  #############################################################################
  
  new_prediction_text <- "<div style='line-height: 30px'><u><b>Running the Predict Tab:</b></u>
                 1) To begin, you need to convert your new dataset into a CSV file
                 2) This CSV file can be comprised of a single new case or a large dataset of new cases
                 3) Decide to use the SOM solution from your current session or a previously saved SOM solution
                 4) Click classify profiles
                 5) NOTE: you can find your results saved in the GENERATE REPORT TAB
                 6) <b>The programme will crash if the headers/format of your new dataset are not the same as the TAB 1 dataset</b>

                 <u><b>Interpreting Your Results:</b></u>
                 1) After you run the data, you get a list of each case
                 2) For each case, you will see its variable profile
                 3) For validity purposes, COMPLEX-IT also provides the second best grid quadrant fit
                 4) NOTE: For advanced users, goodness-of-fit for classification is based on a numeric tolerance defined as 10^(-10)

                 <u><b>Need Help?</b></u>
                 For TUTORIALS on using your SOM GRID for data forcasting in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a>
                 To learn more about AI (machine learning) for data forecasting and prediction <a href='https://en.wikipedia.org/wiki/Machine_learning'>click here</a></div>  "
  
  new_prediction_text <- gsub("\n", "<br>", new_prediction_text) # Convert newline characters to HTML line breaks
  
  observe({
    if (!is.null(input$tabs) && input$tabs == "forecasting" && !pop_ups$pop_up_new_prediction ) {
      shinyalert(
        title = "<u><b>Using the Case Prediction Tab</b></u>",
        text = new_prediction_text,
        size = "l",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_ups$pop_up_new_prediction <- TRUE
    }
  })
  
  observeEvent(input$infoButton_new_prediction, {
    shinyalert(
      title = "<u><b>Using the Case Prediction Tab</b></u>",
      text = new_prediction_text,
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  
  
  
  
  
  
  pInput <- reactive({
    in.file_pred <- input$file_pred
    if (is.null(in.file_pred))
      return(NULL)
    
    the.sep_p <- switch(input$sep_pred, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                        "Space"="")
    
    
    
    the.table_p <- na.omit(read.csv(in.file_pred$datapath, header=T,
                                    sep=the.sep_p))
    
    numeric_only_columns <- column_type_identifier(the.table_p)
    
    the.table_p <- the.table_p[numeric_only_columns]
    if(check_predict_header(names(uploaded_data_values$current_data_file), names(the.table_p)) ==FALSE){
      shinyalert(
        title = "The variable names for the new cases do not match the original names",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return(NULL)
    }
    output$Predict_Warning <- renderText({""})
    the.table_p
  })
  
  
  pred_cases <- reactiveValues(predicted_cases=NULL)
  
  observeEvent(input$classify_prof, {
    temp_som <- som_solution$current_som_solution
    p.input <- pInput()
    if (input$load_prev_som == TRUE) {
      tryCatch(load("./tmp/SavedSOMObject"), error = function(e) NULL)
      temp_som <- previous_som #if there is no file to load, previous_som will be NULL from global
    }
    if (is.null(p.input) | is.null(temp_som)) {
      return(NULL)}
    else {
      # the predictions are made using SOMbrero predict function against the p.input data
      predicted <- predict(temp_som, p.input)
      
      
      # calculate the distances from each case to its closest, 2nd closest, and furthest neuron
      
      Neuron_Profiles<-temp_som[["prototypes"]] #these are the neuron's prototypes
      #create an array intitialized to 1's to store all the BMUs as the loop itterates
      BMUS<-array(1,c(nrow(p.input),6))
      # Now loop through cases, each time appending the case to the existing neuron prototypes and recalculating
      for (i in 1:nrow(p.input)){
        newguess<-p.input[i,]
        D<-rbind(newguess,Neuron_Profiles) #append each case to first position with the neuron prototypes
        B<-dist(D,method="euclidean",diag=TRUE) #calculate the distances from the case to each of the neuron prototypes
        C<-rank(B[1:nrow(D)-1],ties.method= "first") #now rank the neuron prototype distance
        
        BMUS[i,2]<-which(C==2)
        
        
      }
      
      # Now append the BMUs to the file
      case_id = seq(from = 1, to = nrow(p.input), by = 1)
      
      pred_cases$predicted_cases <- cbind("Case id" = as.integer(case_id), 'Best Quadrant' = predicted, '2nd Best Quadrant' = as.integer(BMUS[,2]), p.input)
      
      # output$view_predict <- renderTable({
      #   head(predicted_cases, n=input$nrow.result_pred)
      # })
      
      output$view_predict <- renderDT(
        
        pred_cases$predicted_cases,
        options = list(scrollX = TRUE, searching = FALSE),
        rownames = FALSE
        
      )
      
    }
    output$predict_somplot <- renderPlot({
      
      #This is here to add cluster labels to neurons for observation plots only
      temp.dim<-temp_som[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
      plot(x=temp_som, what="obs", type="barplot",
           show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
    })
  })
  
  
  
  #### Panel 'Scenario Simulation'
  #############################################################################
  
  
  scenarios_text <- "<div style='line-height: 30px'><u><b>To Run Model</b></u><br>
                 1) Start by clicking on MODEL SETUP, which creates the SOM grid created with TAB4<br>
                 2) The grid you see is based on the SOM solution you arrived at using TAB3<br>
                 3) Next, click the RUN CLUSTERS tab, which places your k-means solution on the SOM grid<br>
                 4) These are the k-means clusters you settled on using TAB2<br>
                 5) Next, make changes to the various profile of variables for each of the cases.<br>
                 6) Once done, click on RUN CLUSTERS again, to see if and where on the grid the cluster moved<br>
                 7) Next, look at the BARPLOT grid to see what profile of factors account for the new grid placement<br>
                 8) Is this where you wanted your cluster to arrive?  If not, try changing something else<br>
                 9) If satisfied with your solution, run SENSITIVITY ANALYSIS; if not, click MODEL SETUP to reset<br>
                 <br>

                 <u><b>To Run Sensitivity Analysis</b></u><br>
                 1) Pick the CLUSTER you are testing from the options<br>
                 2) Decide how much to dither your solution by in order to account for variance and error that go with any real-world estimation of change<br>
                 3) Run the sensitivity analysis<br>
                 4) NOTE: very complex solutions can several minutes or hours to finish<br>
                 <br>


                 <u><b>Need Help?</b></u><br>
                 For TUTORIALS on using your model to run scenario simulations in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a><br>
                 To learn more about case-based scenario simulation <a href='https://www.art-sciencefactory.com/case-based%20microsimulation.pdf'>click here</a></div>  "
  
  
  
  observe({
    if (!is.null(input$tabs) && input$tabs == "scenarios" && !pop_ups$pop_up_scenarios ) {
      shinyalert(
        title = "<u><b>Using the Scenarios Tab</b></u>",
        text = scenarios_text,
        size = "l",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_ups$pop_up_scenarios <- TRUE
    }
  })
  
  observeEvent(input$infoButton_scenarios, {
    shinyalert(
      title = "<u><b>Using the Scenarios Tab</b></u>",
      text = scenarios_text,
      size = "l",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  agent_cluster_ordinals <- reactiveValues(first = NA, second = NA, third = NA, fourth = NA, fifth = NA, sixth = NA)
  agent_cluster_values <- reactiveValues(agent_cluster_tracker=NULL, cluster_df=NULL, agent_grid_plot=NULL, handson_store=NULL, agent_drawtools=NULL, agentdf=NULL)
  
  # Setup Button Pressed
  observeEvent(input$Agent_Setup,{
    if(is.null(kmeans_solution$current_kmeans_solution) | is.null(som_solution$current_som_solution)){
      shinyalert(
        title = "You must first run your own clusters and train the SOM",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    if(length(kmeans_solution$current_kmeans_solution@usize) > 9){
      shinyalert(
        title = "You must use 9 or fewer clusters",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()
    }
    
    agent_cluster_values$agent_cluster_tracker <- create_track_agent_tab_state("first", "none")
    erase_future_states(agent_cluster_values$agent_cluster_tracker, 0, agent_cluster_ordinals)
    agent_cluster_values$cluster_df <- generate_cluster_table(som_solution$current_som_solution, kmeans_solution$current_kmeans_solution)
    temp_logic_col <- generate_logic_column(agent_cluster_values$cluster_df)
    updateKey(cbind("Include" = temp_logic_col,agent_cluster_values$cluster_df), agent_cluster_ordinals, "first")
    agent_cluster_values$agent_cluster_tracker@terminal_state <- "first"
    
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_ordinals[[agent_cluster_values$agent_cluster_tracker@current_state]]) %>%
        hot_col("Quadrant", readOnly = TRUE)
    })
    
    #browser()
    
    output$somplotagent <- renderPlot({
      
      # dim <- current_som_solution$parameters$the.grid$dim
      # knum <<- length(current_kmeans_solution@usize)
      # current_som_solution <<- som_solution$current_som_solution
      # current_kmeans_solution <<- kmeans_solution$current_kmeans_solution
      
      agent_cluster_values$agent_grid_plot <- generate_grid_template(som_solution$current_som_solution$parameters$the.grid$dim, length(kmeans_solution$current_kmeans_solution@usize), som_solution$current_som_solution, kmeans_solution$current_kmeans_solution)[[1]]
      agent_cluster_values$agent_drawtools <- generate_grid_template(som_solution$current_som_solution$parameters$the.grid$dim, length(kmeans_solution$current_kmeans_solution@usize), som_solution$current_som_solution, kmeans_solution$current_kmeans_solution)[[2]]
      agent_cluster_values$agentdf <- generate_grid_template(som_solution$current_som_solution$parameters$the.grid$dim, length(kmeans_solution$current_kmeans_solution@usize), som_solution$current_som_solution, kmeans_solution$current_kmeans_solution)[[3]]
      agent_cluster_values$agent_grid_plot
      
    })
    output$cluster_sensitivity = renderUI({
      clus_label = generate_cluster_labels(kmeans_solution$current_kmeans_solution)
      selectInput('cluster_select', 'Select Cluster for Sensitivity', clus_label)
    })
    output$agent_somplot <- renderPlot({
      
      #This is here to add cluster labels to neurons for observation plots only
      temp.dim<-som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
      plot(x=som_solution$current_som_solution, what="obs", type="barplot",
           show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
    })
    output$Agent_Warning <- renderText({})
    output$sensitivity_barplot <- NULL
  })
  
  observeEvent(input$Agent_Run_Clusters, {
    
    #browser()
    
    if(is.null(agent_cluster_values$agent_cluster_tracker)){
      output$Agent_Warning <- renderText({"You must first Setup the Model"})
      return()
      
    }
    if(!is.null(input$clusters_editable_table)){
      new_data_state <- hot_to_r(input$clusters_editable_table)
      state <- convert_state_to_numeric(agent_cluster_values$agent_cluster_tracker, agent_cluster_values$agent_cluster_tracker@current_state)
      terminal <- convert_state_to_numeric(agent_cluster_values$agent_cluster_tracker, agent_cluster_values$agent_cluster_tracker@terminal_state)
      if(state < 6 & state != terminal){
        erase_future_states(agent_cluster_values$agent_cluster_tracker, state, agent_cluster_ordinals)
      }
      empty <- FALSE
      for(i in 1:length(agent_cluster_values$agent_cluster_tracker@possible_states))
      {
        if(is.na(reactiveValuesToList(agent_cluster_ordinals)[agent_cluster_values$agent_cluster_tracker@possible_states][i])){
          empty <- TRUE
          agent_cluster_values$agent_cluster_tracker@terminal_state <- agent_cluster_values$agent_cluster_tracker@possible_states[i]
          agent_cluster_values$agent_cluster_tracker@current_state <- agent_cluster_values$agent_cluster_tracker@terminal_state
          break
        }
      }
      updateReactiveValues(agent_cluster_values$agent_cluster_tracker, new_data_state, agent_cluster_ordinals, empty)
    }
    else{new_data_state <-agent_cluster_ordinals[[agent_cluster_values$agent_cluster_tracker@current_state]]}
    
    # current_som_solution <<- som_solution$current_som_solution
    # agent_drawtools <<- agent_cluster_values$agent_drawtools
    # agentdf <<- agent_cluster_values$agentdf
    # agent_grid_plot <<- agent_cluster_values$agent_grid_plot
    # new_data_state <<- new_data_state
    
    agent_cluster_values$agentdf <- plot_agent_SOM(new_data_state, som_solution$current_som_solution, agent_cluster_values$agent_drawtools, agent_cluster_values$agentdf)[[1]]
    
    #agentdf <<- agent_cluster_values$agentdf
    
    agent_cluster_values$agent_drawtools <- plot_agent_SOM(new_data_state, som_solution$current_som_solution, agent_cluster_values$agent_drawtools, agent_cluster_values$agentdf)[[2]]
    
    #need to add something here so it only plots the lower bound of data points
    output$somplotagent <- renderPlot({
      agent_cluster_values$agent_grid_plot + geom_point(aes(x=agent_cluster_values$agentdf$x, y=agent_cluster_values$agentdf$y, color=agent_cluster_values$agentdf$groupnames), size =4) + scale_color_manual(values = agent_cluster_values$agent_drawtools@plot_colors, name = "Clusters") + theme(legend.key = element_blank())
    })
    
    
  })
  
  observeEvent(input$back_cluster, {
    newstate <- update_tracker_current_state(agent_cluster_values$agent_cluster_tracker, agent_cluster_ordinals, -1)
    agent_cluster_values$agent_cluster_tracker@current_state <- agent_cluster_values$agent_cluster_tracker@possible_states[newstate]
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_ordinals[[agent_cluster_values$agent_cluster_tracker@current_state]])
    })
    
  })
  observeEvent(input$forward_cluster, {
    newstate <- update_tracker_current_state(agent_cluster_values$agent_cluster_tracker, agent_cluster_ordinals, 1)
    agent_cluster_values$agent_cluster_tracker@current_state <- agent_cluster_values$agent_cluster_tracker@possible_states[newstate]
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_ordinals[[agent_cluster_values$agent_cluster_tracker@current_state]])
    })
    
  })
  
  observeEvent(input$SensitivityAnalysis, {
    
    #browser()
    
    agent_cluster_values$handson_store <- reactiveValuesToList(agent_cluster_ordinals)
    eval_change <- evaluate_state_change(agent_cluster_values$handson_store, input$cluster_select, agent_cluster_values$agent_cluster_tracker, uploaded_data_values$current_data_file)
    if(is.character(eval_change)){
      output$Agent_Warning <- renderText({eval_change})
    }
    else{
      
      
      agent_cluster_values$agent_cluster_tracker@sensitivity_test <- eval_change
      full_var_names = names(uploaded_data_values$current_data_file)
      current_var_names = c()
      change_vector =c()
      for(i in 1:length(eval_change)){
        if(eval_change[i] != 0){
          current_var_names =c(current_var_names, full_var_names[i])
          change_vector=c(change_vector, eval_change[i])
        }
      }
      showModal(dataModal(current_var_names, change_vector))
    }
  })
  
  observeEvent(input$sa_ok, {
    agent_cluster_values$handson_store <- reactiveValuesToList(agent_cluster_ordinals)
    baseline <- agent_cluster_values$handson_store[['first']]
    baseline <- as.numeric(snip_state(baseline, input$cluster_select, uploaded_data_values$current_data_file))
    change_state <- agent_cluster_values$handson_store[[agent_cluster_values$agent_cluster_tracker@current_state]]
    agent_cluster_values$agent_cluster_tracker@checked_data <- change_state #for now this has to be here because it gets reassigned below
    change_state <- as.numeric(snip_state(change_state, input$cluster_select, uploaded_data_values$current_data_file))
    var_names <- names(uploaded_data_values$current_data_file)
    
    
    
    
    monte_carlo_grid = list()
    input_var = 1
    
    
    
    for(i in 1:length(change_state)){
      if(agent_cluster_values$agent_cluster_tracker@sensitivity_test[i] != 0){
        percent = input[[paste0("pont.dev", input_var)]]/100
        diff = abs(baseline[i] - change_state[i])
        up = change_state[i] + (diff * percent)
        down = change_state[i] - (diff * percent)
        step = abs(up - down)/10
        
        monte_carlo_grid[[var_names[i]]] = seq(down, up, step)
        input_var = input_var + 1
      }
      else{
        monte_carlo_grid[[var_names[i]]] = change_state[i]
      }
    }
    removeModal()
    #populate the monte carlo state space
    permutations = c()
    dev_cols =c()
    default_vector =c()
    
    for(i in 1:length(monte_carlo_grid)){
      if(length(monte_carlo_grid[[var_names[i]]]) == 1){
        default_vector=c(default_vector, monte_carlo_grid[[var_names[i]]])
      }
      else{
        default_vector=c(default_vector, NA)
        permutations =c(permutations, length(monte_carlo_grid[[var_names[i]]]))
        dev_cols <- c(dev_cols, i)
      }
    }
    showModal(waitModal(input$cluster_select, dev_cols))
    rule_list <-c()
    for(i in 1:length(permutations)){
      rule_list <- c(rule_list, permutations[i]^(length(permutations)-i))
    }
    rule_list_rev <- rev(rule_list)
    permutation_states = prod(permutations)
    som_dim <- prod(som_solution$current_som_solution$parameters$the.grid$dim)
    
    permutation_space <- genmc_state_space(permutation_states, default_vector)
    solution_space <- genmc_state_space(som_dim, 0)
    
    for(r in 1:length(rule_list)){
      grid_vector <- monte_carlo_grid[[dev_cols[r]]]
      state_index = 1
      for(i in 1:rule_list_rev[r]){
        for(j in 1:length(grid_vector)){
          for(k in 1:rule_list[r]){ #this should not be the length of rule_list[r] but the element of rule_list[r]
            permutation_space[[state_index]][dev_cols[r]] <- grid_vector[j]
            state_index = state_index + 1
          }
        }
      }
    }
    state_to_test <- floor(runif((permutation_states*2), min= 1, max = (permutation_states + 1)))
    for(s in 1:length(state_to_test)){
      temp_state <- permutation_space[[state_to_test[s]]]
      quadrant <- predict(som_solution$current_som_solution, temp_state)
      solution_space[[quadrant]] <- solution_space[[quadrant]] + 1
    }
    agent_cluster_values$agent_cluster_tracker@cluster_tested <- input$cluster_select
    agent_cluster_values$agent_cluster_tracker@sensitivity_result <- solution_space
    sub_sol_space <- c()
    sub_sol_names <- c()
    for(i in 1:length(solution_space)) {
      
      if(solution_space[[i]]> 0){
        sub_sol_names <-c(sub_sol_names, i)
        sub_sol_space <- c(sub_sol_space, solution_space[[i]])
      }}
    removeModal()
    output$sensitivity_barplot <- renderPlot({
      barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen")
    })
  })
  #
  #
  #  button_pressed <- reactiveVal(FALSE)
  
  kmeans_count <- reactiveValues(value = 0)
  
  observeEvent(input$init_kmeans, {
    kmeans_count$value <- kmeans_count$value + 1
    
    print(kmeans_count$value)
  })
  
  infoButton <- reactive({  input$infoButton  })

  text <- "Purpose of Map:
             1) The tab is intended to help you visually think about the relationships amongst your variables as a network of connections and pathways of influence. \n
             2) It shows the correlation of pairs of factor, and encourages you to evaluate them, add new nodes and connections which represent your beliefs about possible <a href='https://www.khanacademy.org/test-prep/praxis-math/praxis-math-lessons/gtp--praxis-math--lessons--statistics-and-probability/a/gtp--praxis-math--article--correlation-and-causation--lesson'>causal connections</a>, or pull out subsection of the map. \n
             3) THINK, DON’T ACCEPT: Be careful not to interpret the map as causal connections. It is only showing you the correlation between pairs of nodes. \n
             4) Keep in mind the correlations are not conditioned on other factors (i.e. they do not control for other variables), so we must keep a critical mindset – these maps are intended to prompt thinking and discussion, not offer definitive or ‘correct’ analysis. \n

             Using the Map:
             1) Along the left you will see various toggles to change your network. Changing these makes 'deep' changes to the network, as it influences the final dataframe informing the construction of the network. This means changes here can be combined and carried over between changes to these toggles. \n
             2) The network itself is rendered using the visNetwork package. Using this you can add and remove nodes and edges, and change the position of nodes. Be aware: these are 'shallow' or aesthetic changes: changing any parameter on the left will erase any changes made. \n
             3) Along the bottom you can examine node and network statistic information. \n"
  text <- gsub("\n", "<br>", text) # Convert newline characters to HTML line breaks
  text <- gsub("Purpose of Map:", "<u><b>Purpose of Map:</b></u>", text) # Bold the "Purpose of Map:" heading
  text <- gsub("Using the Map:", "<u><b>Using the Map:</b></u>", text) # Bold the "Using the Map:" heading
  
  systems_mapping_tab_button_pressed_tracker <- reactiveValues(pop_up_systems = FALSE, exportOptionsToggle=0, egoNetworkToggle=0, advancedOptionsToggle=0, shortestPathsToggle=0, weightsOptionsToggle=0)
  
  observe({
    # Check if the tab "Using Sytems Mapping To Explore Cluster Variables" is selected
    if (!is.null(input$tabs) && input$tabs == "systems_mapping" && !systems_mapping_tab_button_pressed_tracker$pop_up_systems ) {
      shinyalert(
        title = "<u><b>Using the Map</b></u>",
        text = text,
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      systems_mapping_tab_button_pressed_tracker$pop_up_systems <- TRUE
    }
  })
  
  observeEvent(infoButton(), {
    shinyalert(
      title = "<u><b>Using the Map</b></u>",
      text = text,
      size = "m",
      closeOnEsc = TRUE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      showCancelButton = FALSE,
      confirmButtonText = "OK",
      confirmButtonCol = "#bce7fa",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })

  #####  Observe statement for shortest paths first opening #####
  observe({

    if (input$egoNetwork == 1) {

      shinyalert(
        title = "Caution when using ego network",
        text = "When working with ego networks, please only tick the 'Make ego-network' box when you are ready to visualise your ego network and have selected your nodes. This includes both the first ego network you make, and any subsequent ego networks you make. Failing to do this will not break your app or the visualisation, but you may get erroneous pop-up warnings of an invalid node due subsetting moving from one subsetted network to another.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE)

    }
  })
  #####  Observe statement for shortest paths first opening #####

  #####  Observe statement for shortest paths first opening #####
  observe({

    if (input$shortestPaths == 1) {

      shinyalert(
        title = "Caution when using shortest paths",
        text = "When working with shortest paths, please only tick the 'Show shortest paths' box when you are ready to visualise your shortest paths and have selected your nodes. This includes both the first shortest paths you show, and any subsequent paths you show. Failing to do this will not break your app or the visualisation, but you may get erroneous pop-up warnings of an invalid node due subsetting  moving from one subsetted network to another.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE)

    }
  })
  #####  Observe statement for shortest paths first opening #####

  #####  Observe statement for shortest paths first opening #####
  observe({

    if (input$weightsOptions == 1) {

      shinyalert(
        title = "Caution when using weights",
        text = "To work with weights, download your network's edges using the download button below. In your
            spreadsheet editor you will have three columns: the edges (to and from) and a 'weight' column, which
            is by default a value of one. Change the weights as desired, but do not leave values blank or type
            in non-numeric values, and please do not change the name of the columns. Please upload the weights first, and then tick the box to include them. Also note, weights are ignored
            when calculating the 'examine node' statistics below the network.",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = FALSE,
        type = "warning",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE)

    }
  })
  #####  Observe statement for shortest paths first opening #####

  # ##### Observe statements for weights sanity checks #####
  # observe({
  #   if (is.null(input$weights_values$datapath) == FALSE) {
  #     testing_file <- read.csv(input$weights_values$datapath)
  #
  #     if(is.numeric(testing_file$weight) == FALSE){
  #       shinyalert(
  #         title = "You have a non-numeric character in your weight column",
  #         size = "s",
  #         type = "warning"
  #       )
  #     } else if (ncol(testing_file) != 3 ) {
  #       shinyalert(
  #         title = "You have the incorrect number of columns for your weights. You should have three: from, to, and weight.",
  #         size = "s",
  #         type = "warning"
  #       )
  #     } else if (names(testing_file)[3] != 'weight' || names(testing_file)[1] != 'from' || names(testing_file)[2] != 'to') {
  #       shinyalert(
  #         title = "You have an incorrect name in your columns. Ensure they are 'from', 'to', and 'weight' only.",
  #         size = "s",
  #         type = "warning"
  #       )
  #     } else {
  #       NULL
  #     }
  #
  #
  #   } else {
  #     NULL
  #   }
  # })
  # ##### Observe statements for weights sanity checks #####
  #
  observeEvent(input$exportOptions, {
    if(input$exportOptions == 0){
      systems_mapping_tab_button_pressed_tracker$exportOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$exportOptionsToggle <- systems_mapping_tab_button_pressed_tracker$exportOptionsToggle + 1
    }
  })
  #
  observeEvent(input$egoNetwork, {
    if(input$egoNetwork == 0){
      systems_mapping_tab_button_pressed_tracker$egoNetworkToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$egoNetworkToggle <- systems_mapping_tab_button_pressed_tracker$egoNetworkToggle + 1
    }
  })
  #
  observeEvent(input$advancedOptions, {
    if(input$advancedOptions == 0){
      systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle <- systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle + 1
    }
  })
  #
  observeEvent(input$shortestPaths, {
    if(input$shortestPaths == 0){
      systems_mapping_tab_button_pressed_tracker$shortestPathsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$shortestPathsToggle <- systems_mapping_tab_button_pressed_tracker$shortestPathsToggle + 1
    }
  })
  #
  observeEvent(input$weightsOptions, {
    if(input$weightsOptions == 0){
      systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle <- systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle + 1
    }
  })
  #
  #
  # ########## OBSERVE STATEMENTS FOR MODAL BOXES ##########
  #
  # ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########
  #
  # Observe statement for show/hide export options box
  observe(if (systems_mapping_tab_button_pressed_tracker$exportOptionsToggle == 0) {
    shinyjs::hide(id = "exportOptionsBox")
  })
  #
  # Observe statement for show/hide ego network box
  observe(if (systems_mapping_tab_button_pressed_tracker$egoNetworkToggle == 0) {
    shinyjs::hide(id = "egoNetworkBox")
  })
  #
  # Observe statement for show/hide advanced options box
  observe(if (systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle == 0) {
    shinyjs::hide(id = "advancedOptionsBox")
  })
  #
  # Observe statement for show/hide shortest paths box
  observe(if (systems_mapping_tab_button_pressed_tracker$shortestPathsToggle == 0) {
    shinyjs::hide(id = "shortestPathsBox")
  })
  #
  # Observe statement for show/hide weights box
  observe(if (systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle == 0) {
    shinyjs::hide(id = "weightsBox")
  })
  #
  # ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########
  #
  # ########## OBSERVE EVENTS FOR OPENING/CLOSING HIDE/SHOWS ##########
  # Observe statement for show/hide weights options box
  observeEvent(input$weightsOptions, {

    if(systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle %% 2 == 1){
      shinyjs::show(id = "weightsBox")
    }else{
      shinyjs::hide(id = "weightsBox")
    }
  })
  #
  # Observe statement for show/hide export options box
  observeEvent(input$exportOptions, {

    if(systems_mapping_tab_button_pressed_tracker$exportOptionsToggle %% 2 == 1){
      shinyjs::show(id = "exportOptionsBox")
    }else{
      shinyjs::hide(id = "exportOptionsBox")
    }
  })
  #
  # Observe statement for show/hide ego network box
  observeEvent(input$egoNetwork, {

    if(systems_mapping_tab_button_pressed_tracker$egoNetworkToggle %% 2 == 1){
      shinyjs::show(id = "egoNetworkBox")
    }else{
      shinyjs::hide(id = "egoNetworkBox")
    }
  })
  #
  # Observe statement for show/hide advanced options box
  observeEvent(input$advancedOptions, {

    if(systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle %% 2 == 1){
      shinyjs::show(id = "advancedOptionsBox")
    }else{
      shinyjs::hide(id = "advancedOptionsBox")
    }
  })
  #
  # Observe statement for show/hide shortest paths box
  observeEvent(input$shortestPaths, {

    if(systems_mapping_tab_button_pressed_tracker$shortestPathsToggle %% 2 == 1){
      shinyjs::show(id = "shortestPathsBox")
    }else{
      shinyjs::hide(id = "shortestPathsBox")
    }
  })
  #
  ########## OBSERVE EVENTS FOR OPENING/CLOSING HIDE/SHOWS ##########

  # Trigger the tour when the button is pressed
  observeEvent(input$tour_systems_mapping, {
    # Show elements
    shinyjs::show(id = "weightsBox")
    shinyjs::show(id = "exportOptionsBox")
    shinyjs::show(id = "egoNetworkBox")
    shinyjs::show(id = "advancedOptionsBox")
    shinyjs::show(id = "shortestPathsBox")

    # Start the tour if it's not active

    introjs(session, options = list("nextLabel" = "Next", "prevLabel" = "Previous", "skipLabel" = "Quit"),

            events = list(

              oncomplete=I('Shiny.setInputValue("weightsOptions", 0, {priority: "event"});
                            Shiny.setInputValue("exportOptions", 0, {priority: "event"});
                            Shiny.setInputValue("egoNetwork", 0, {priority: "event"});
                            Shiny.setInputValue("advancedOptions", 0, {priority: "event"});
                            Shiny.setInputValue("shortestPaths", 0, {priority: "event"});'), #, {priority: "event"}

              onexit=I('Shiny.setInputValue("weightsOptions", 0, {priority: "event"});
                            Shiny.setInputValue("exportOptions", 0, {priority: "event"});
                            Shiny.setInputValue("egoNetwork", 0, {priority: "event"});
                            Shiny.setInputValue("advancedOptions", 0, {priority: "event"});
                            Shiny.setInputValue("shortestPaths", 0, {priority: "event"});')

            ))

  })
  
  systems_mapping_values <- reactiveValues(rawcases=NULL, rawcases_filt_super=NULL, links5_download=NULL, nodes4_download=NULL, examine_nodes_dropdown_ids=NULL, user_set_seed=NULL, final_network_download=NULL, choices='All', network_initialised=NULL)
  
  dynamic_nodes_ids_values <- reactiveValues(dynamic_nodes_ids = NULL)
  
  observe(
    
    if( !is.null(uploaded_data_values$current_data_file) ){
      
      dynamic_nodes <- data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                                  label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
      dynamic_nodes_ids_values$dynamic_nodes_ids <- setNames(dynamic_nodes$id, dynamic_nodes$label)
      
      print('Line 2963 printing')
      
    }
    
  )
  
  
  observeEvent(input$initialise_button, {
    
    if(is.null(uploaded_data_values$current_data_file)){
      shinyalert(
        title = "Please Upload Data First",
        text = '',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "error",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      return()}
    
    print('Line 2991 printing')
    
    systems_mapping_values$network_initialised <- TRUE
    
    #if (!button_pressed()){
    
    #### Systems Mapping Tab ####
    
    ##### Setting Up The Initial File, inc. if it has clustering #####
    
    # rawcases <- reactive({
    #   if (!is.null(input$file1) && !is.null(current_data_file)) {
    #
    #     if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
    #       rawcases_data <- cbind(current_data_file, "Group" = current_kmeans_solution@uclusters)
    #       print("rawcases has been updated with k-means clustering results")
    #     } else {
    #       rawcases_data <- data.frame(current_data_file, Group = NA)
    #       print("raw cases is as user uploaded, with an added null group column")
    #     }
    #     return(rawcases_data)
    #   }
    # })
    
    #rawcases <- reactiveValues(value = NULL)
    
    #if(!is.null(current_data_file)){
    
    observe({
      
      print('Line 3021 printing')
      
      browser()
      
      #rawcases <-
      #  if (!is.null(input$file1) && !is.null(current_data_file)) {
      
      if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
        rawcases_data <- cbind(uploaded_data_values$current_data_file, "Group" = kmeans_solution$current_kmeans_solution@uclusters)
        systems_mapping_values$rawcases <- rawcases_data
        print("rawcases has been updated with k-means clustering results")
      } else {
        rawcases_data <- data.frame(uploaded_data_values$current_data_file, Group = NA)
        systems_mapping_values$rawcases <- rawcases_data
        print("raw cases is as user uploaded, with an added null group column")
      }
      #return(rawcases_data)
      #  }
    })
    
    #}
    
    ########## OBSERVE STATEMENTS FOR MODAL BOXES ##########
    
    ##### Observe statement for initial start up modal box #####
    #observe({
    #  shinyalert(
    #    title = "<u><b>Using the Map</b></u>",
    #    text = text,
    #    size = "m",
    #    closeOnEsc = TRUE,
    #    closeOnClickOutside = FALSE,
    #    html = TRUE,
    #    type = "info",
    #    showConfirmButton = TRUE,
    #    showCancelButton = FALSE,
    #    confirmButtonText = "OK",
    #    confirmButtonCol = "#bce7fa",
    #    timer = 0,
    #    imageUrl = "",
    #    animation = TRUE
    #  )
    #})
    
    #pop_up <- FALSE
    
    ##### Observe statement for initial start up modal box #####
    
    ########## SETTING UP INPUTS ##########
    neg_corr <- reactive({ (input$neg_corr)*-1 })
    pos_corr <- reactive({ input$pos_corr })
    minor_threshold <- reactive({ input$minor_threshold })
    seed <- reactive({ input$seed })
    layout <- reactive({ input$layout })
    degree <- reactive({ input$degree })
    target_node <- reactive({ input$target_node })
    ego_network <- reactive({ input$ego_network })
    title <- reactive({ input$title })
    subtitle <- reactive({ input$subtitle })
    footer <- reactive({ input$footer })
    shortest_paths_toggle <- reactive({ input$shortest_paths_toggle })
    from_node <- reactive({ input$from_node })
    to_node <- reactive({ input$to_node })
    include_weights <- reactive({ input$include_weights })
    remove_unconnecteds <- reactive({ input$remove_unconnecteds })
    LineThickness <- reactive({ input$LineThickness })
    htmlSave <- reactive({ input$htmlSave })
    cluster <- reactive({ input$cluster })
    chosen_node <- reactive({  input$chosen_node  })
    ########## SETTING UP INPUTS ##########
    
    print('Line 3090 printing')
    
    rawcases_filtered <- reactive({
      if (cluster() == 'All') {
        rawcases_filt <- systems_mapping_values$rawcases %>% select(-Group)
        systems_mapping_values$rawcases_filt_super <- nrow(rawcases_filt)
        rawcases_filt
      } else {
        rawcases_filt <- systems_mapping_values$rawcases %>% filter(Group == cluster()) %>% select(-Group)
        systems_mapping_values$rawcases_filt_super <- nrow(rawcases_filt)
        rawcases_filt
      }
    })
    
    
    #rawcases_filtered <-  reactive({ current_data_file })
    
    # corrs_matrix <- tryCatch({
    #
    #   result = reactive({  rcorr(as.matrix(rawcases_filtered()[,1:ncol(rawcases_filtered())]))  })
    #   return(result)
    #   },
    #   error=function(e) {
    #     message('Loading Network')
    #     print(e)
    #   }
    #   )
    
    corrs_matrix <- reactive({
      
      validate(
        need(!nrow(rawcases_filtered()) < 4, "Have you made a valid selection? (If first initalisation, please wait)")
      )
      
      rcorr(as.matrix(rawcases_filtered()[,1:ncol(rawcases_filtered())]))
      
    })
    
    
    
    
    corrs <- reactive({  flattenCorrMatrix(corrs_matrix()$r, corrs_matrix()$P)  })
    
    corrs_filtered_neg <- reactive({  corrs() %>% filter(cor < neg_corr())  })
    
    corrs_filtered_pos <- reactive({  corrs() %>% filter(cor > pos_corr())  })
    
    corrs_filtered <- reactive({
      corrs() %>%
        subset(select = c("row", "column", "color", "width")) %>%
        filter(cor < neg_corr() | cor > pos_corr())
    })
    
    
    links <- reactive({
      data.frame(
        from = corrs_filtered()$row,
        to = corrs_filtered()$column
      ) %>%
        setNames(c("from", "to"))
    })
    
    pre_nodes_1 <- reactive({ data.frame(name = colnames(rawcases_filtered()[, 1:ncol(rawcases_filtered())])) })
    
    pre_nodes_2 <- reactive({  pre_nodes_2 <- pre_nodes_1() %>% rename("label" = "name")  })
    
    nodes <- reactive({  rowid_to_column(pre_nodes_2(), "id")  }) # keep this one named as nodes
    
    corrs_filtered <- reactive({
      
      print('Line 3162 printing')
      
      corrs() %>%
        subset(select = c("row", "column", "cor")) %>%
        filter(cor < neg_corr() | cor > pos_corr()) %>%
        mutate(
          color = ifelse(cor >= 0, "green", "red"),
          width = if(input$LineThickness == "bins") {
            case_when(
              abs(cor) <= 0.1 ~ 0.2,
              abs(cor) <= 0.2 ~ 0.4,
              abs(cor) <= 0.3 ~ 0.6,
              abs(cor) <= 0.4 ~ 0.8,
              abs(cor) <= 0.5 ~ 1,
              abs(cor) <= 0.6 ~ 2,
              abs(cor) <= 0.7 ~ 3,
              abs(cor) <= 0.8 ~ 4,
              abs(cor) <= 0.9 ~ 5,
              TRUE ~ 5
            )
          } else if(input$LineThickness == "binary") {
            ifelse(abs(cor) >= minor_threshold(), 4.5, 1)
          } else {
            2
          }
        )
    })
    
    corrs_filtered2 <- reactive({  subset(corrs_filtered(), select = c("row", "column", "color", "width"))  })
    
    links2 <- reactive({  left_join(links(), corrs_filtered2(), by = c("from" = "row", "to" = "column"))  })
    
    links3 <- reactive({
      
      links2() %>%
        mutate(from = nodes()$id[match(from, nodes()$label)],
               to = nodes()$id[match(to, nodes()$label)])
    })
    
    weights_values_initial <- reactive({
      if (include_weights() == TRUE) {
        read.csv(input$weights_values$datapath)
      } else {
        NULL
      }
    })
    
    weights_values <- reactive({
      # Get the links5() dataframe
      weights <- weights_values_initial()
      
      weights$to <- unname(dynamic_nodes_ids_values$dynamic_nodes_ids[weights$to])
      weights$from <- unname(dynamic_nodes_ids_values$dynamic_nodes_ids[weights$from])
      
      # Return the modified dataframe
      weights
    })
    
    links4 <- reactive({
      if (include_weights() == TRUE) {
        links4 <- left_join(links3(), weights_values(), by = c("to", "from"))
        links4
      } else {
        links3()
      }
    })
    
    igraph <- reactive({  graph_from_data_frame(links4(), vertices = nodes(), directed = F)  })
    
    nodes2 <- reactive({
      
      print('Line 3232 printing')
      
      if (ego_network() == TRUE) {
        distances <- distances(igraph())
        distances <- as.data.frame(distances[target_node(), ] )
        distances <- rownames_to_column(distances)
        distances <- rename(distances, Node = 1, Separation = 2)
        distances <- subset(distances, Separation > degree())
        removes <- as.vector(distances$Node)
        
        nodes_ego <- nodes()[!nodes()$id %in% removes, ]
        
        nodes_ego
      } else {
        nodes()
      }
    })
    
    edges_to_keep <- reactive({
      if (shortest_paths_toggle() == TRUE) {
        
        edges_to_keep <- lapply(all_shortest_paths(igraph(), from = from_node(), to = to_node())$res, function(x) E(igraph(), path=x))
        edges_to_keep <- as_ids(unique(do.call(c, edges_to_keep)))
        edges_to_keep <- edges_to_keep %>% as.data.frame() %>% rename(joined = 1)
        edges_to_keep <- separate(data = edges_to_keep, col = joined, into = c("from", "to"))
        
        edges_to_keep$from <- as.numeric(edges_to_keep$from)
        edges_to_keep$to <- as.numeric(edges_to_keep$to)
        
        edges_to_keep
      } else {
        'NULL'
      }
    })
    
    nodes_to_keep <- reactive({
      
      print('Line 3269 printing')
      
      if (shortest_paths_toggle() == TRUE) {
        
        nodes_to_keep <- unique(do.call(c, all_shortest_paths(igraph(), from = from_node(), to = to_node())$res))
        nodes_to_keep <- as.data.frame(as.vector(nodes_to_keep)) %>% rename(id = 1)
        
        nodes_to_keep
      } else {
        'NULL'
      }
    })
    
    nodes3 <- reactive({
      if (shortest_paths_toggle() == TRUE) {
        
        nodes_to_remove <- anti_join(nodes2(), nodes_to_keep(), by = "id")
        
        new_nodes <- anti_join(nodes2(), nodes_to_remove, by = "id")
        
        new_nodes
        
      } else {
        nodes2()
      }
    })
    
    links5 <- reactive({
      if (shortest_paths_toggle() == TRUE) {
        
        edges_to_remove <- anti_join(links4(), edges_to_keep(), by = c("from", "to"))
        
        new_links <- anti_join(links4(), edges_to_remove, by = c("from", "to"))
        
        systems_mapping_values$links5_download <- new_links
        
        new_links
        
      } else {
        systems_mapping_values$links5_download <- links4()
        links4()
      }
    })
    
    nodes4 <- reactive({
      
      print('Line 3315 printing')
      
      if (remove_unconnecteds() == "Yes" & ego_network() == FALSE & shortest_paths_toggle() == FALSE){
        
        igraph <- graph_from_data_frame(links5(), vertices = nodes3(), directed = F)
        
        distances <- as.data.frame(distances(igraph))
        distances[sapply(distances, is.infinite)] <- 0
        
        distances <- distances %>% select_if(~sum(.) == 0)
        
        removes <- as.vector(colnames(distances))
        
        new_nodes <- nodes3()[!nodes3()$id %in% removes, ]
        
        systems_mapping_values$nodes4_download <- new_nodes
        #examine_nodes_dropdown_ids <<- new_nodes
        systems_mapping_values$examine_nodes_dropdown_ids <- setNames(new_nodes$id, new_nodes$label)
        
        systems_mapping_values$user_set_seed <- seed()
        
        new_nodes
      } else {
        systems_mapping_values$nodes4_download <- nodes3()
        #examine_nodes_dropdown_ids <<- nodes3()
        systems_mapping_values$examine_nodes_dropdown_ids <- setNames(nodes3()$id, nodes3()$label)
        systems_mapping_values$user_set_seed <- seed()
        nodes3()
      }
    })
    
    # if(!(input$to_node %in% nodes4())){output$network_warning <- renderText({"Please make a valid selection!"})
    # print("this is running renderText")
    # return()}
    
    final_network <- reactive({
      
      if(is.null(nodes4())){print('nodes4() is NULL')}
      else if (is.null(links5())){print('links5() is NULL')}
      else{print('neither nodes4() or links5() is NULL')}
      
      print('Line 3351 printing')
      
      # validate(
      #   need(is.null(links5()), "Have you made a valid selection?")
      # )
      
      set.seed(seed())
      
      visNetwork(nodes4(), links5(), width = "100%",
                 main = title(),
                 submain = subtitle(),
                 footer = footer()) %>%
        visPhysics(enabled = F) %>%
        visOptions(manipulation = T, highlightNearest = T)  %>%
        visIgraphLayout(layout = layout()) %>%
        visInteraction(zoomSpeed = 0.25) %>%
        visNodes(size = input$node_size)
      
    })
    
    
    
    systems_mapping_values$final_network_download <- reactive(final_network())
    
    nodes_to_dl <- reactive(nodes4())
    
    links_to_dl <- reactive({
      # Get the links5() dataframe
      links_df <- links5()
      
      # Remove the "color" and "width" columns
      subset_df <- links_df[, !(names(links_df) %in% c("color", "width"))]
      
      subset_df$to <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(subset_df$to, dynamic_nodes_ids_values$dynamic_nodes_ids)]
      subset_df$from <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(subset_df$from, dynamic_nodes_ids_values$dynamic_nodes_ids)]
      
      # Return the modified dataframe
      subset_df
    })
    
    output$nodesDownload <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(nodes_to_dl(), file, row.names = FALSE)
      }
    )
    
    output$edgesDownload <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(filtered_links_for_igraph(), file, row.names = FALSE)
      }
    )
    
    filtered_links_for_igraph <- reactive({ links5()[links5()$to %in% nodes4()$id & links5()$from %in% nodes4()$id, ] })
    
    #filtered_nodes_for_igraph <- unique(c(filtered_links_for_igraph()$from, filtered_links_for_igraph()$to))
    
    igraph_for_stats <- reactive({  graph_from_data_frame(filtered_links_for_igraph(), vertices = nodes4(), directed = F)  })
    
    igraph_for_stats2 <- reactive({
      
      if(include_weights() == TRUE){
        igraph <- delete_edge_attr(igraph_for_stats(), "weight")
      } else {
        igraph_for_stats()
      }
    })
    
    network_mean_distance <- reactive({  mean_distance(igraph_for_stats2())  })
    network_diameter <- reactive({  diameter(igraph_for_stats2())  })
    
    closeness <- reactive({
      closeness <- distances(igraph_for_stats2())
      closeness <- as.data.frame(closeness[chosen_node(), ] )
      closeness[sapply(closeness, is.infinite)] <- NA
      closeness <- rename(closeness, chosen_node = 1)
      closeness
    })
    
    direct_connections <- reactive({ sum(closeness()$chosen_node == 1, na.rm = T) })
    standardised_score <- reactive({ round(direct_connections()/(nrow(nodes4())-1), 2) })
    valid_nodes <- reactive ({ (nrow(nodes4())) - 1 - (sum(is.na(closeness()$chosen_node))) })
    node_average_distance <- reactive ({ round(sum(closeness()$chosen_node, na.rm = T)/valid_nodes(), 2) })
    
    # str1 <- reactive({
    #
    #   if(direct_connections() != 0){
    #     str1 <- paste("Number of direct links:", direct_connections())
    #     str1
    #   } else {
    #     str1 <- paste("This node is unconnected")
    #     str1
    #   }
    #
    # })
    
    
    str1 <- reactive({
      result <- tryCatch({
        if (direct_connections() != 0) {
          str1 <- paste("Number of direct links:", direct_connections())
          str1
        } else {
          str1 <- paste("This node is unconnected")
          str1
        }
      }, error = function(e) {
        message <- "Couldn't produce node or network statistics. Did you make invalid selections in choosing your network, such as choosing a 'shortest path' with an unconnected node?"
        return(message)
      })
      
      if (inherits(result, "character")) {
        return(result)
      } else {
        return("An unexpected error occurred.")
      }
    })
    
    
    
    str2 <- reactive({
      result <- tryCatch({
        if(standardised_score() != 0){
          str2 <- paste("Standardised connection score:", standardised_score())
          str2
        } else {
          str2 <- paste("This node is unconnected")
          str2
        }
      }, error = function(e) {
        message <- ""
        return(message)
      })
      
      if (inherits(result, "character")) {
        return(result)
      } else {
        return("")
      }
    })
    
    
    
    
    str3 <- reactive({
      result <- tryCatch({
        if(is.nan(node_average_distance()) == FALSE){
          str3 <- paste("Average node distance:", node_average_distance())
          str3
        } else {
          str3 <- paste("This node is unconnected")
          str3
        }
      }, error = function(e) {
        message <- ""
        return(message)
      })
      
      if (inherits(result, "character")) {
        return(result)
      } else {
        return("")
      }
    })
    
    
    str4 <- reactive({
      result <- tryCatch({
        if(is.nan(node_average_distance()) == FALSE){
          str4 <- paste("The average degree of seperation in your network is:", round(network_mean_distance(), 2))
          str4
        } else {
          str4 <- paste("This network has no connections between nodes")
          str4
        }
      }, error = function(e) {
        message <- ""
        return(message)
      })
      
      if (inherits(result, "character")) {
        return(result)
      } else {
        return("")
      }
    })
    
    
    
    str5 <- reactive({
      result <- tryCatch({
        paste("The maximum distance (diameter) of your network is:", network_diameter())
      }, error = function(e) {
        message <- ""
        return(message)
      })
      
      if (inherits(result, "character")) {
        return(result)
      } else {
        return("")
      }
    })
    
    
    output$htmlSave <- downloadHandler(
      filename = function() {
        paste('network-', Sys.Date(), '.html', sep='')
      },
      content = function(con) {
        final_network() %>% visSave(con)
      }
    )
    
    html_dl_reactive <- reactive({
      
      downloadHandler(
        filename = function() {
          paste('network-', Sys.Date(), '.html', sep='')
        },
        content = function(con) {
          final_network() %>% visSave(con)
        }
      )
      
    })
    
    # output$pngSave <- downloadHandler(
    #   filename = function() {
    #     paste('network-', Sys.Date(), '.png', sep='')
    #   },
    #   content = function(PNGfile) {
    #     final_network() %>% visExport()
    #   }
    # )
    
    output$networkPlot <- renderVisNetwork({final_network()})
    
    # output$networkPlot <- renderVisNetwork({
    #   result <- tryCatch({
    #     final_network()
    #   }, error = function(e) {
    #     # Print a general error message to the console for the developer
    #     cat("Some error occurred, this is printing\n")
    #     return(NULL)  # Return NULL to indicate an empty plot or do nothing
    #   })
    #
    #   if (inherits(result, "visNetwork")) {
    #     # If no error occurred, return the result
    #     return(result)
    #   } else {
    #     # Handle the case when an error occurred
    #     # You can return an empty plot, do nothing, or take other actions here
    #     return(NULL)
    #   }
    # })
    
    
    
    #html_object <<- final_network()
    
    #output$text <- renderText({  HTML(paste(str1(), str2(), str3(), str4(), str5(), sep = '<br/>'))  })
    
    output$text <- renderText({  HTML(paste(str1(), str2(), str3(), str4(), str5(), sep = '<br/>'))  }) #
    
    ########## MAKING THE WEIGHTS DOWNLOAD ##########
    
    weights_links <- reactive({
      # Get the links5() dataframe
      links_df <- links5()
      
      # Remove the "color" and "width" columns
      subset_df <- links_df[, !(names(links_df) %in% c("color", "width"))]
      
      subset_df$to <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(subset_df$to, dynamic_nodes_ids_values$dynamic_nodes_ids)]
      subset_df$from <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(subset_df$from, dynamic_nodes_ids_values$dynamic_nodes_ids)]
      
      subset_df$weight <- 1
      
      # Return the modified dataframe
      subset_df
    })
    
    
    output$downloadData <- downloadHandler(
      
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        write.csv(weights_links(), file, row.names = FALSE)
      }
    )
    ########## MAKING THE WEIGHTS DOWNLOAD ##########
    
    #button_pressed(TRUE)
    
    
  }
  
  #    }
  )
  
  
  ##### 'Generate Report'
  #############################################################################
  output$downloadReport <- downloadHandler(
    
    
    filename = 'report_docs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      file_names <- c("info.txt")
      info_text = "This directory contains the following files:"
      kmeans_files <- c("kmeans_profiles.csv", "clustered_data.csv","kmean_seed.txt", "silh_plot.pdf")
      som_files <- c("som_options.csv", "summary_class.txt","som_seed.txt", "som_profiles.csv", "data_quadrants.csv", "som_barplot.pdf", "som_boxplot.pdf")
      policy_files <- c("adjust_kmeans.csv", "tested_intervention.csv", "sensitivity.pdf")
      predict_files <-c("predicted_quadrants.csv")
      systems_mapping_files <- c("nodes_list.csv", 'edges_list.csv', 'set_network_seed.txt')
      
      if(is.null(kmeans_solution$current_kmeans_solution) == FALSE) {
        file_names <- c(file_names, kmeans_files)
        info_text <- c(info_text, kmeans_files)
      }
      if(is.null(som_solution$current_som_solution) == FALSE){
        file_names <- c(file_names, som_files)
        info_text <- c(info_text, som_files)
      }
      if(is.null(agent_cluster_values$agent_cluster_tracker) == FALSE && agent_cluster_values$agent_cluster_tracker@cluster_tested != "None"){
        file_names <- c(file_names, policy_files)
        info_text <- c(info_text, policy_files)
      }
      if(is.null(pred_cases$predicted_cases)== FALSE){
        file_names <- c(file_names, predict_files)
        info_text <-c(info_text,predict_files)
      }
      # if(is.null(network_initialised) == FALSE){
      #   file_names <- c(file_names, systems_mapping_files)
      #   info_text <-c(info_text,systems_mapping_files)
      # 
      # }
      
      fs <-file_names
      fileConn<-file("info.txt")
      writeLines(info_text, fileConn)
      close(fileConn)
      
      if(is.null(kmeans_solution$current_kmeans_solution) == FALSE){
        fstat <- pseudoF(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution, nrow(kmeans_solution$current_kmeans_solution@ucenters))
        fstat_col <- rep(0, nrow(kmeans_solution$current_kmeans_solution@ucenters)-1)
        fstat_col <- c(fstat, fstat_col)
        kcenters <- cbind(kmeans_solution$current_kmeans_solution@ucenters, "size" = kmeans_solution$current_kmeans_solution@usize, "Pseudo_F" = fstat_col)
        write.csv(kcenters, file = "kmeans_profiles.csv")
        clustered_data <- cbind(uploaded_data_values$current_data_file, "clus" = kmeans_solution$current_kmeans_solution@uclusters)
        write.csv(clustered_data, file = "clustered_data.csv")
        if(input$setrandseedkmean == "Yes"){kseed = as.character(input$randseedkmean)}
        else{kseed = "NULL"}
        fileConn<-file("kmean_seed.txt")
        writeLines(kseed, fileConn)
        close(fileConn)
        pdf("silh_plot.pdf")
        plot_silhouette(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution)
        dev.off()
      }
      if(is.null(som_solution$current_som_solution) == FALSE){
        som_options =list("x dim" = som_solution$current_som_solution$parameters$the.grid$dim[1], "y dim" = som_solution$current_som_solution$parameters$the.grid$dim[2],
                          "proto_init" = som_solution$current_som_solution$parameters$init.proto, "max_iter" = som_solution$current_som_solution$parameters$maxit,
                          "data_scaling" = som_solution$current_som_solution$parameters$scaling, "gradient_descent" = som_solution$current_som_solution$parameters$eps0)
        som_options_df <- as.data.frame(som_options)
        write.csv(som_options_df, file = "som_options.csv")
        class_summary <- capture.output(summary(som_solution$current_som_solution))
        temp_qual <- quality(som_solution$current_som_solution)
        class_summary <- class_summary[12:length(class_summary)]
        class_summary <- append(class_summary, paste("      Quant Error:", format(temp_qual$quantization, digits = 6)), after=2)
        fileConn<-file("summary_class.txt")
        writeLines(class_summary, fileConn)
        close(fileConn)
        if(input$setrandseed == "Yes"){somseed = as.character(input$randseed)}
        else{somseed = "NULL"}
        fileConn<-file("som_seed.txt")
        writeLines(somseed, fileConn)
        close(fileConn)
        som_profiles <- as.data.frame(som_solution$current_som_solution$prototypes)
        write.csv(som_profiles, file = "som_profiles.csv")
        data_quadrants <- cbind(uploaded_data_values$current_data_file, "quadrant" = som_solution$current_som_solution$clustering)
        write.csv(data_quadrants, file = "data_quadrants.csv")
        
        temp.dim<-som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]]
        pdf("som_barplot.pdf")
        plot(x=som_solution$current_som_solution, what="obs", type="barplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
        dev.off()
        pdf("som_boxplot.pdf")
        plot(x=som_solution$current_som_solution, what="obs", type="boxplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
        dev.off()
        
      }
      if(is.null(agent_cluster_values$agent_cluster_tracker) == FALSE && agent_cluster_values$agent_cluster_tracker@cluster_tested != "None"){
        write.csv(agent_cluster_values$agent_cluster_tracker@checked_data, file ="adjust_kmeans.csv")
        temp_names <- names(uploaded_data_values$current_data_file)
        intervention =c()
        deviation =c()
        input_var = 1
        for(i in 1:length(temp_names)){
          if(agent_cluster_values$agent_cluster_tracker@sensitivity_test[i] != 0){
            intervention =c(intervention, agent_cluster_values$agent_cluster_tracker@sensitivity_test[i])
            deviation =c(deviation, input[[paste0("pont.dev", input_var)]])
            input_var = input_var + 1
          }
          else{
            intervention = c(intervention, 0)
            deviation =c(deviation, 0)
          }
        }
        tested_changes <- matrix(c(intervention, deviation), nrow = 2, ncol = length(deviation), byrow = TRUE,
                                 dimnames = list(c("Intervention", "Deviation"), c(temp_names)))
        write.csv(tested_changes, file ="tested_intervention.csv")
        
        sub_sol_space <- c()
        sub_sol_names <- c()
        for(i in 1:length(agent_cluster_values$agent_cluster_tracker@sensitivity_result)) {
          
          if(agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]]> 0){
            sub_sol_names <-c(sub_sol_names, i)
            sub_sol_space <- c(sub_sol_space, agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]])
          }}
        
        pdf("sensitivity.pdf")
        barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen")
        dev.off()
      }
      if(is.null(pred_cases$predicted_cases) == FALSE){
        write.csv(pred_cases$predicted_cases, file ="predicted_quadrants.csv")
        
      }
      
      # if(is.null(network_initialised) == FALSE){
      # 
      #   write.csv(nodes4_download, file ="nodes_list.csv")
      # 
      #   links5_download <- links5_download[, !(names(links5_download) %in% c("color", "width"))]
      # 
      #   links5_download$to <- names(dynamic_nodes_ids)[match(links5_download$to, dynamic_nodes_ids)]
      #   links5_download$from <- names(dynamic_nodes_ids)[match(links5_download$from, dynamic_nodes_ids)]
      # 
      #   write.csv(links5_download, file ="edges_list.csv")
      # 
      #   fileConn<-file("set_network_seed.txt")
      #   set_seed_info <- paste('Seed set for network visualisation is: ', user_set_seed, sep = '')
      #   writeLines(set_seed_info, fileConn)
      #   close(fileConn)
      # 
      # }
      
      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
  
  
  # observe({
  #
  #   if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
  #     updateSelectInput(session = session, inputId = "cluster", choices = c("All", sort(unique(na.omit(rawcases$Group)))))
  #     print("This has run!!!")
  #   }})
  
  
  observe({
    
    if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
      systems_mapping_values$choices <- c("All", sort(unique(na.omit(kmeans_solution$current_kmeans_solution@uclusters))))
      print("This has run!!!")
      
    }})
  
  
  
  
  
  
  
  
  
  
  
  # dynamic_nodes <- reactive({
  #   if(is.null(current_data_file)) {
  #     return(NULL)
  #   } else {
  #     data.frame(id = seq_along(current_data_file[,1:ncol(current_data_file)]),
  #                label = colnames(current_data_file[,1:ncol(current_data_file)]))
  #   }
  # })
  #
  # dynamic_nodes_ids <<- reactive({
  #   if(is.null(dynamic_nodes())) {
  #     return(NULL)
  #   } else {
  #     setNames(dynamic_nodes()$id, dynamic_nodes()$label)
  #   }
  # })
  
  
  # # Create a reactive value that wraps the global variable
  # reactive_current_data_file <- reactiveVal(current_data_file)
  #
  # # Update the reactive value when the global variable changes
  # observe({
  #   reactive_current_data_file(current_data_file)
  # })
  
  dynamic_nodes <- reactive({
    if(is.null(uploaded_data_values$current_data_file)) {
      return(NULL)
    } else {
      dynamic_nodes <- data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                                  label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
      dynamic_nodes_ids_values$dynamic_nodes_ids <-  setNames(dynamic_nodes$id, dynamic_nodes$label)
      
      print('This dynamic nodes reactive has printed')
      
    }
  })
  
  # dynamic_nodes_ids <- reactive({
  #   if(is.null(dynamic_nodes())) {
  #     return(NULL)
  #   } else {
  #     setNames(dynamic_nodes()$id, dynamic_nodes()$label)
  #   }
  # })
  
  ##############################################
  
  examine_nodes_dropdown <- reactive({
    if(is.null(uploaded_data_values$current_data_file)) {
      return(NULL)
    } else {
      
      data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                 label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
      #unique(c(filtered_links_for_igraph()$from, filtered_links_for_igraph()$to))
      
    }
  })
  
  # examine_nodes_dropdown_ids <<- reactive({
  #   if(is.null(dynamic_nodes())) {
  #     print('examine_nodes_dropdown_ids RUNNING!!')
  #     return(NULL)
  #   } else if(nrow(nodes4()) < length(dynamic_nodes())) {
  #     setNames(nodes4()$id, nodes4()$label)
  #     print('examine_nodes_dropdown_ids RUNNING!!')
  #   } else {
  #     setNames(dynamic_nodes()$id, dynamic_nodes()$label)
  #     print('examine_nodes_dropdown_ids RUNNING!!')
  #   }
  # })
  
  # output$troubleshooting_text <- renderPrint({
  #  filtered_nodes_for_igraph()
  # })
  
  
  
  observeEvent(input$file1, {
    
    Sys.sleep(5)
    
    if (!is.null(uploaded_data_values$current_data_file)) {
      
      print("This is printing :)")
      
      #dynamic_nodes <- data.frame(
      #id = seq_along(current_data_file[, 2:ncol(current_data_file)]),
      #label = colnames(current_data_file[, 2:ncol(current_data_file)])
      #)
      
      #dynamic_nodes_ids <- setNames(dynamic_nodes$id, dynamic_nodes$label)
      
      #print("This is printing :)")
      
      # Perform any other actions with dynamic_nodes or dynamic_nodes_ids here
      
    }
  })
  
  
  
  #   observe({
  #     updateSelectInput(session = session, inputId = "target_node", choices = dynamic_nodes_ids())
  #   })
  #
  #   observe({
  #     updateSelectInput(session = session, inputId = "from_node", choices = dynamic_nodes_ids())
  #   })
  #
  #   observe({
  #     updateSelectInput(session = session, inputId = "to_node", choices = dynamic_nodes_ids())
  #   })
  #
  #   observe({
  #     updateSelectInput(session = session, inputId = "chosen_node", choices = dynamic_nodes_ids())
  #   })
  
  
  
  
  # observeEvent(input$initialise_button, {
  #
  #   if (!button_pressed()){
  #
  # observe({
  #
  #   if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
  #     #updateSelectInput(session = session, inputId = "cluster", choices = c("All", sort(unique(na.omit(rawcases$Group)))))
  #     print("This is running")
  #   }
  #
  #   #
  #   # observe({
  #   #
  #   #   print("this has run")
  #   #
  #   #   #rawcases <-
  #   #   #  if (!is.null(input$file1) && !is.null(current_data_file)) {
  #   #
  #   #   if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
  #   #     rawcases_data <- cbind(current_data_file, "Group" = current_kmeans_solution@uclusters)
  #   #     rawcases <<- rawcases_data
  #   #     print("rawcases has been updated with k-means clustering results")
  #   #   } else {
  #   #     rawcases_data <- data.frame(current_data_file, Group = NA)
  #   #     rawcases <<- rawcases_data
  #   #     print("raw cases is as user uploaded, with an added null group column")
  #   #   }
  #   #   #return(rawcases_data)
  #   #   #  }
  #   # })
  #
  #
  # })
  #
  # dynamic_nodes <- reactive({
  #   if(is.null(current_data_file)) {
  #     return(NULL)
  #   } else {
  #     data.frame(id = seq_along(current_data_file[,2:ncol(current_data_file)]),
  #                label = colnames(current_data_file[,2:ncol(current_data_file)]))
  #   }
  # })
  #
  # dynamic_nodes_ids <<- reactive({
  #   if(is.null(dynamic_nodes())) {
  #     return(NULL)
  #   } else {
  #     setNames(dynamic_nodes()$id, dynamic_nodes()$label)
  #   }
  # })
  #
  
  #reactive_examine_nodes_dropdown_ids <- reactiveVal(examine_nodes_dropdown_ids)
  
  # Update the reactive value when the global variable changes
  # observe({
  #   reactive_current_data_file(examine_nodes_dropdown_ids)
  # })
  
  observeEvent(input$initialise_button, {
    
    if(is.null(uploaded_data_values$current_data_file)){output$network_warning <- renderText({"Please upload data first."})
    print("this is running renderText")
    return()}
    
    observe({
      updateSelectInput(session = session, inputId = "target_node", choices = dynamic_nodes_ids_values$dynamic_nodes_ids)
    })
    
    observe({
      updateSelectInput(session = session, inputId = "from_node", choices = dynamic_nodes_ids_values$dynamic_nodes_ids)
    })
    
    observe({
      updateSelectInput(session = session, inputId = "to_node", choices = dynamic_nodes_ids_values$dynamic_nodes_ids)
    })
    
    observe({
      
      #if ( nrow(nodes4_download) < length(dynamic_nodes_ids) ){
      
      updateSelectInput(session = session, inputId = "chosen_node", choices = systems_mapping_values$examine_nodes_dropdown_ids)
      
      #} else {
      
      #updateSelectInput(session = session, inputId = "chosen_node", choices = dynamic_nodes_ids)
      
      #}
      
    })
    
    observe({
      updateSelectInput(session = session, inputId = "cluster", choices = systems_mapping_values$choices)
    })
    
  })
  
  observeEvent(c(input$neg_corr, input$pos_corr, input$minor_threshold,
                 input$seed, input$layout, input$degree, input$target_node,
                 input$ego_network, input$ego_network, input$title,
                 input$subtitle, input$footer, input$shortest_paths_toggle,
                 input$from_node, input$to_node, input$include_weights,
                 input$remove_unconnecteds, input$LineThickness,
                 input$htmlSave, input$cluster),
               
               {
                 
                 observe({
                   
                   updateSelectInput(session = session, inputId = "chosen_node", choices = systems_mapping_values$examine_nodes_dropdown_ids)
                   
                 })
                 
                 
               })
  
  
  
  
  # triggered <<- FALSE
  #
  # observeEvent(input$target_node,
  #              {
  #                observe({
  #                  if( !(input$target_node %in% examine_nodes_dropdown_ids)){# | !(input$from_node %in% examine_nodes_dropdown_ids) | !(input$to_node %in% examine_nodes_dropdown_ids) ){
  #                    triggered <<- TRUE
  #                    cat('Triggered is ', triggered, sep='')
  #
  #                    if (triggered == TRUE) {
  #                      shinyalert(
  #                        title = "<u><b>Warning!</b></u>",
  #                        text = 'Sample Text',
  #                        size = "m",
  #                        closeOnEsc = TRUE,
  #                        closeOnClickOutside = FALSE,
  #                        html = TRUE,
  #                        type = "info",
  #                        showConfirmButton = TRUE,
  #                        showCancelButton = FALSE,
  #                        confirmButtonText = "OK",
  #                        confirmButtonCol = "#bce7fa",
  #                        timer = 0,
  #                        imageUrl = "",
  #                        animation = TRUE
  #                      )
  #                    }
  #                    triggered <<- FALSE
  #                    }
  #                })
  #              })
  
  
  observeEvent(input$target_node, {
    if (!(input$target_node %in% systems_mapping_values$examine_nodes_dropdown_ids) & !is.null(systems_mapping_values$network_initialised)) {
      shinyalert(
        title = "<u><b>Warning!</b></u>",
        text = "The node you have selected is not in your current network, and thus can't make a valid ego-network. If this is unexpected, perhaps try changing your variables such that this node appears in your network?",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  observeEvent(input$from_node, {
    if (!(input$from_node %in% systems_mapping_values$examine_nodes_dropdown_ids) & !is.null(systems_mapping_values$network_initialised)) {
      shinyalert(
        title = "<u><b>Warning!</b></u>",
        text = "The node you have selected is not in your current network, and thus can't make a valid shortest path with it. If this is unexpected, perhaps try changing your variables such that this node appears in your network?",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  observeEvent(input$to_node, {
    if (!(input$to_node %in% systems_mapping_values$examine_nodes_dropdown_ids) & !is.null(systems_mapping_values$network_initialised)) {
      shinyalert(
        title = "<u><b>Invalid Node Selection!</b></u>",
        text = "The node you have selected is not in your current network, and thus can't make a valid shortest path with it. If this is unexpected, perhaps try changing your variables such that this node appears in your network?",
        size = "m",
        closeOnEsc = TRUE,
        closeOnClickOutside = FALSE,
        html = TRUE,
        type = "info",
        showConfirmButton = TRUE,
        showCancelButton = FALSE,
        confirmButtonText = "OK",
        confirmButtonCol = "#bce7fa",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  
  #
  
  # observeEvent(c(input$neg_corr, input$pos_corr, input$minor_threshold,
  #                input$seed, input$layout, input$degree, input$target_node,
  #                input$ego_network, input$ego_network, input$title,
  #                input$subtitle, input$footer, input$shortest_paths_toggle,
  #                input$from_node, input$to_node, input$include_weights,
  #                input$remove_unconnecteds, input$LineThickness,
  #                input$htmlSave),
  #
  #              {
  #
  #                observe({
  #
  #                  if(is.null(run_through)){
  #                    NULL
  #                  } else if(run_through == TRUE){
  #                    NULL
  #                  } else if(run_through == FALSE){
  #                    output$network_warning <- renderText({"You've made an invalid network selection."})
  #                  } else {
  #                    NULL
  #                  }
  #
  #                })
  #
  #
  #              })
  
  
  observeEvent(input$init_kmeans,
               
               {
                 
                 observe({
                   
                   if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
                     
                     choices_df <- data.frame(clusters = kmeans_solution$current_kmeans_solution@uclusters)
                     choices_value_counts <- table(choices_df$clusters)
                     choices_filtered_values <- as.numeric(names(choices_value_counts[choices_value_counts > 4]))
                     systems_mapping_values$choices <- c("All", sort(choices_filtered_values))
                     
                     print("This has run!!!")
                     
                   }})
                 
                 observe({
                   updateSelectInput(session = session, inputId = "cluster", choices = systems_mapping_values$choices)
                 })
                 
                 
               })
  
  
  
  #
  #   }
  #
  # })
  
  
  observeEvent(input$egoNetwork, {
    print(paste0("EgoNetwork Value: ", input$egoNetwork))
  })
}  

