
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

server <- function(input, output, session) {
  
  output$complexit_logo <- renderImage({list(src="Complexit_LOGO3.png")}, deleteFile = FALSE)
  
  
  
  #### Panel 'Import data'
  ##############################################################################
  dInput <- reactive({
    in.file <- input$file1
    if (is.null(in.file))
      return(NULL)
    
    
    the.sep <- switch(input$sep, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
    
    
    
    
    the.table <- na.omit(read.csv(in.file$datapath, header=input$header,
                                  sep=the.sep))
    
    
    #right now this just deselects not numeric data columns
    output$varchoice <- renderUI(div(
      checkboxGroupInput(inputId="varchoice", label="Input variables:",
                         choices=as.list(colnames(the.table)[
                           sapply(the.table, class) %in%
                             c("integer", "numeric")]),
                         selected=as.list(colnames(the.table)[
                           sapply(the.table, class) %in%
                             c("integer", "numeric")])),
      actionButton(inputId = "subset_data", label = "Subset Data")))
    numeric_only_columns <- column_type_identifier(the.table)
    full_data <<- the.table
    current_data_file <<- the.table[numeric_only_columns]
    the.table#this is set such that the last thing in this method/function is the table and is therefore set to dInput
    
  })
  observeEvent(input$subset_data,{
    d.input <- dInput()
    
    subset_list <- input$varchoice
    if(length(ncol(d.input) >= length(subset_list))){
      current_data_file <<- d.input[subset_list]
    }
    
    
  })
  # data preview table
  # once data is populated, with the dInput call here, it will automatically be populated
  output$view <- renderTable({
    d.input <- dInput()
    if (is.null(d.input))
      return(NULL)
    if (ncol(full_data)>input$ncol.preview)
      full_data <- full_data[,1:input$ncol.preview]
    head(full_data, n=input$nrow.preview)
  })
  
  
  observeEvent(input$initialise_button, { 
    
    if(is.null(current_data_file)){output$network_warning <- renderText({"Please upload data first."})
    print("this is running renderText")
    return()}
    
#    if (!button_pressed()){
    
      observe({
        if (!is.null(reactive_current_data_file)) {
          
          dynamic_nodes <- data.frame(id = seq_along(current_data_file[,1:ncol(current_data_file)]),
                                      label = colnames(current_data_file[,1:ncol(current_data_file)]))
          
          dynamic_nodes_ids <<- setNames(dynamic_nodes$id, dynamic_nodes$label)
          
          examine_nodes_dropdown_ids <<- dynamic_nodes_ids
          
          #examine_nodes_dropdown_ids <<- setNames(nodes4_download$id, nodes4_download$label)
          
          print("dynamic nodes ids updated")
        } else {
          dynamic_nodes_ids <<- NULL
          print("dynamic nodes NULL")
        }
      })  
      
      # observe({
      # 
      #   if ( !is.null(reactive_current_data_file) & !is.null(examine_nodes_dropdown_ids) ){
      # 
      #     if(nrow(nodes4_download) < length(dynamic_nodes_ids)){
      # 
      #       examine_nodes_dropdown_ids <<- setNames(nodes4_download$id, nodes4_download$label)
      # 
      #       print("detected divergence")
      # 
      #     }
      # 
      #     }
      # 
      #   })
      
#    }
    
  })
  
  
  #### Panel 'Cluster Data'
  #############################################################################
  #this observe event looks for users to press 'get clusters'
  observeEvent(input$init_kmeans, {
    if(is.null(current_data_file)){output$kmean_warning <- renderText({"Please upload data first."})
    return()}
    else {output$kmean_warning <- renderText({""})}
    if(input$setrandseedkmean == "Yes") {set.seed(input$randseedkmean)}
    else {set.seed(sample(1:9999, size= 1))}
    current_kmeans_solution <<- create_user_gen_kmeans_solution("default_name", kmeans(current_data_file, isolate(input$clusters)))
    
    output$kmeans_title <- renderUI({
      h4("Kmeans Cluster Centroids")
    })
    output$kmeans_tab <- renderTable({
      
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label <- generate_cluster_labels()
      summary_row <- generate_data_summary()
      cen_tab <- cbind("Cluster" = clus_label, "Size" = current_kmeans_solution@usize, round(current_kmeans_solution@ucenters, digits = 3))
      cen_tab <- rbind(cen_tab, summary_row)
      
    })
    
    #displays the pseudoF
    if (input$pseudo_f == TRUE) {
      FSTAT <- pseudoF(current_data_file, current_kmeans_solution,input$clusters)
      output$pseudoF <- renderText({
        paste("Pseudo F: ", FSTAT)
      })}
    if (input$silhouette == TRUE){
      output$kmeans_silh <- renderPlot({
        plot_silhouette(current_data_file, current_kmeans_solution)
      }, width = 500, height = graph_dimension(current_data_file))
    }
  })
  #### Panel 'Train the SOM'
  #############################################################################
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
  
  observeEvent(input$trainbutton, {
    if(input$dimx < 3 | input$dimy < 3 | input$dimx > 15 | input$dimy > 15)
    {
      output$som_warning <- renderText({"SOM dimensions must be 3 or greater and 15 or lesser."})
      return()
    }
    else if(is.null(current_data_file)){
      output$som_warning <- renderText({"Please upload data first."})
      return()
    }  
    else
    {
      output$som_warning <- renderText({""})
    }
    tmp_data <- current_data_file
    mapped_labels = NULL
    if(!is.null(current_kmeans_solution)){
      mapped_labels <- create_kmeans_SOM_mapping()
      rownames(tmp_data) <- mapped_labels
    }
    
    if(input$setrandseed == "Yes") {set.seed(input$randseed)}
    else {set.seed(sample(1:9999, size= 1))}
    current_som_solution<<- trainSOM(tmp_data, dimension=c(input$dimx,input$dimy),
                                     maxit=input$maxit, scaling=input$scaling, init.proto=input$initproto, eps0 =input$eps0)
    
    updatePlotSomVar() # update variable choice for som plots
    output$trainnotice <- renderUI({
      ### post the quality control factors as well
      qual_measures <- quality(current_som_solution)
      anova_results <- retrieve_ANOVA_results()
      
      #now print out the results
      tagList(
        p(paste("Trained SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")),
        p(paste("Topo Error  ", format(qual_measures$topographic,digits=4),sep=" ")),
        p(paste("Quant Error ", format(qual_measures$quantization,digits=4),sep=" ")),
        p(paste("ANOVA Results")),
        lapply(length(anova_results):1, function(i, y) { p(paste(y[i])) }, y=anova_results)
      )
    })
    
    
  })  
  
  
  #### Panel 'Plot Map'
  #############################################################################
  observe({
    updateSelectInput(session, "somplottype",
                      choices=all.somplot.types[["numeric"]][[
                        input$somplotwhat]])
  })
  
  # update variables available for plotting
  updatePlotSomVar <- function() observe({
    tmp.names <- colnames(current_som_solution$data)
    
    updateSelectInput(session, "somplotvar", choices=tmp.names)
    # updateSelectInput(session, "somplotvar2", choices=tmp.names,
    #                   selected=tmp.names[1:length(tmp.names)])
    
  })
  # Plot the SOM
  output$somplot <- renderPlot({
    if(is.null(current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    tmp.view <- NULL
    if (input$somplottype =="boxplot") {
      # tmp.var <- (1:ncol(current_som_solution$data))[colnames(current_som_solution$data) %in%
      #                                                  input$somplotvar2]
      tmp.var <- seq(from = 1, to = ncol(current_data_file), by = 1)
    }
    else {tmp.var <- input$somplotvar}
    
    #This if/else set is here to add cluster labels to neurons for observation plots only
    temp.dim<-current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
    if(input$somplotwhat =='obs'& input$somplottype == 'boxplot' | input$somplottype == 'color'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
                                                                                                      variable = tmp.var, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))}
    else if(input$somplotwhat == 'obs'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
                                             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))}
    else if (input$somplotwhat == 'prototypes' & input$somplottype == 'barplot'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype)
    }
    else {
      plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
           variable=tmp.var,view=tmp.view)
    }
    
  })
  observeEvent(input$save_som, {
    previous_som <- current_som_solution #need to rename the object or it may overwrite later current_som objects
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
    if(check_predict_header(names(current_data_file), names(the.table_p)) ==FALSE){
      output$Predict_Warning <- renderText({"The variable names for the new cases do not match the original names"})
      return(NULL)
    }
    output$Predict_Warning <- renderText({""})
    the.table_p
  })
  
  
  observeEvent(input$classify_prof, {
    temp_som <- current_som_solution
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
      
      predicted_cases <<- cbind("Case id" = as.integer(case_id), 'Best Quadrant' = predicted, '2nd Best Quadrant' = as.integer(BMUS[,2]), p.input)
      
      output$view_predict <- renderTable({
        head(predicted_cases, n=input$nrow.result_pred)
      })
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
  agent_cluster_values <- reactiveValues(first = NA, second = NA, third = NA, fourth = NA, fifth = NA, sixth = NA)
  
  # Setup Button Pressed
  observeEvent(input$Agent_Setup,{
    if(is.null(current_kmeans_solution) | is.null(current_som_solution)){
      output$Agent_Warning <- renderText({"You must first run your own clusters and train the SOM"})
      return()
    }
    if(length(current_kmeans_solution@usize) > 9){
      output$Agent_Warning <- renderText({"You must use 9 or fewer clusters"})
      return()
    }
    
    agent_cluster_tracker <<- create_track_agent_tab_state("first", "none")
    erase_future_states(agent_cluster_tracker, 0, agent_cluster_values)
    cluster_df <<- generate_cluster_table()
    temp_logic_col <- generate_logic_column(cluster_df)
    updateKey(cbind("Include" = temp_logic_col,cluster_df), agent_cluster_values, "first")
    agent_cluster_tracker@terminal_state <<- "first"
    
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_values[[agent_cluster_tracker@current_state]]) %>%
        hot_col("Quadrant", readOnly = TRUE)
    })
    
    output$somplotagent <- renderPlot({
      agent_grid_plot <<- generate_grid_template(current_som_solution$parameters$the.grid$dim, length(current_kmeans_solution@usize))#this will need to be updated with new vals for gen grid
      agent_grid_plot
    })
    output$cluster_sensitivity = renderUI({
      clus_label = generate_cluster_labels()
      selectInput('cluster_select', 'Select Cluster for Sensitivity', clus_label)
    })
    output$agent_somplot <- renderPlot({
      
      #This is here to add cluster labels to neurons for observation plots only
      temp.dim<-current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
      plot(x=current_som_solution, what="obs", type="barplot",
           show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
    })
    output$Agent_Warning <- renderText({})
    output$sensitivity_barplot <- NULL
  })
  
  observeEvent(input$Agent_Run_Clusters, {
    if(is.null(agent_cluster_tracker)){
      output$Agent_Warning <- renderText({"You must first Setup the Model"})
      return()
      
    }
    if(!is.null(input$clusters_editable_table)){
      new_data_state <- hot_to_r(input$clusters_editable_table)
      state <- convert_state_to_numeric(agent_cluster_tracker, agent_cluster_tracker@current_state)
      terminal <- convert_state_to_numeric(agent_cluster_tracker, agent_cluster_tracker@terminal_state)
      if(state < 6 & state != terminal){
        erase_future_states(agent_cluster_tracker, state, agent_cluster_values)
      }
      empty <- FALSE
      for(i in 1:length(agent_cluster_tracker@possible_states))
      {
        if(is.na(reactiveValuesToList(agent_cluster_values)[agent_cluster_tracker@possible_states][i])){
          empty <- TRUE
          agent_cluster_tracker@terminal_state <<- agent_cluster_tracker@possible_states[i]
          agent_cluster_tracker@current_state <<- agent_cluster_tracker@terminal_state
          break
        }
      }
      updateReactiveValues(agent_cluster_tracker, new_data_state, agent_cluster_values, empty)
    }
    else{new_data_state <-agent_cluster_values[[agent_cluster_tracker@current_state]]}
    plot_agent_SOM(new_data_state)
    
    #need to add something here so it only plots the lower bound of data points
    output$somplotagent <- renderPlot({
      agent_grid_plot + geom_point(aes(color=agentdf$groupnames), size =4) + scale_color_manual(values = agent_drawtools@plot_colors, name = "Clusters") + theme(legend.key = element_blank())
      
    })
    
    
  })
  observeEvent(input$back_cluster, {
    newstate <- update_tracker_current_state(agent_cluster_tracker, agent_cluster_values, -1)
    agent_cluster_tracker@current_state <<- agent_cluster_tracker@possible_states[newstate]
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_values[[agent_cluster_tracker@current_state]])
    })
    
  })
  observeEvent(input$forward_cluster, {
    newstate <- update_tracker_current_state(agent_cluster_tracker, agent_cluster_values, 1)
    agent_cluster_tracker@current_state <<- agent_cluster_tracker@possible_states[newstate]
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_values[[agent_cluster_tracker@current_state]])
    })
    
  })
  
  observeEvent(input$SensitivityAnalysis, {
    handson_store <<- reactiveValuesToList(agent_cluster_values)
    eval_change <- evaluate_state_change(handson_store, input$cluster_select)
    if(is.character(eval_change)){
      output$Agent_Warning <- renderText({eval_change})
    }
    else{
      
      
      agent_cluster_tracker@sensitivity_test <<- eval_change
      full_var_names = names(current_data_file)
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
    handson_store <<- reactiveValuesToList(agent_cluster_values)
    baseline <- handson_store[['first']]
    baseline <- as.numeric(snip_state(baseline, input$cluster_select))
    change_state <- handson_store[[agent_cluster_tracker@current_state]]
    agent_cluster_tracker@checked_data <<- change_state #for now this has to be here because it gets reassigned below
    change_state <- as.numeric(snip_state(change_state, input$cluster_select))
    var_names <- names(current_data_file)
    
    
    
    
    monte_carlo_grid = list()
    input_var = 1
    
    
    
    for(i in 1:length(change_state)){
      if(agent_cluster_tracker@sensitivity_test[i] != 0){
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
    som_dim <- prod(current_som_solution$parameters$the.grid$dim)
    
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
      quadrant <- predict(current_som_solution, temp_state)
      solution_space[[quadrant]] <- solution_space[[quadrant]] + 1
    }
    agent_cluster_tracker@cluster_tested <<- input$cluster_select
    agent_cluster_tracker@sensitivity_result <<- solution_space
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
  
  observe({
    # Check if the tab "Using Sytems Mapping To Explore Cluster Variables" is selected
    if (!is.null(input$my_navlist) && input$my_navlist == "7. Using Sytems Mapping To Explore Cluster Variables" && !pop_up ) {
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
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
      pop_up <<- TRUE
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
      confirmButtonCol = "#AEDEF4",
      timer = 0,
      imageUrl = "",
      animation = TRUE
    )
  })
  
  observeEvent(input$initialise_button, { 
    
    if(is.null(current_data_file)){output$network_warning <- renderText({"Please upload data first."})
    print("this is running renderText")
    return()}
    
    print("this is running")  
    
    network_initialised <<- TRUE
    
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

        print("this has run")

          #rawcases <-
          #  if (!is.null(input$file1) && !is.null(current_data_file)) {

              if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
                rawcases_data <- cbind(current_data_file, "Group" = current_kmeans_solution@uclusters)
                rawcases <<- rawcases_data
                print("rawcases has been updated with k-means clustering results")
              } else {
                rawcases_data <- data.frame(current_data_file, Group = NA)
                rawcases <<- rawcases_data
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
      #    confirmButtonCol = "#AEDEF4",
      #    timer = 0,
      #    imageUrl = "",
      #    animation = TRUE
      #  )
      #})
      
      #pop_up <- FALSE
      
      ##### Observe statement for initial start up modal box #####
    
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
          confirmButtonCol = "#AEDEF4",
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
          confirmButtonCol = "#AEDEF4",
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
            confirmButtonCol = "#AEDEF4",
            timer = 0,
            imageUrl = "",
            animation = TRUE)
          
        }
      })
      #####  Observe statement for shortest paths first opening #####
      
      ##### Observe statements for weights sanity checks #####
      observe({
        if (is.null(input$weights_values$datapath) == FALSE) {
          testing_file <- read.csv(input$weights_values$datapath)
          
          if(is.numeric(testing_file$weight) == FALSE){
            shinyalert(
              title = "You have a non-numeric character in your weight column",
              size = "s",
              type = "warning"
            )
          } else if (ncol(testing_file) != 3 ) {
            shinyalert(
              title = "You have the incorrect number of columns for your weights. You should have three: from, to, and weight.",
              size = "s",
              type = "warning"
            )
          } else if (names(testing_file)[3] != 'weight' || names(testing_file)[1] != 'from' || names(testing_file)[2] != 'to') {
            shinyalert(
              title = "You have an incorrect name in your columns. Ensure they are 'from', 'to', and 'weight' only.",
              size = "s",
              type = "warning"
            )
          } else {
            NULL
          }
          
          
        } else {
          NULL
        }
      })
      ##### Observe statements for weights sanity checks #####
      
      ########## OBSERVE STATEMENTS FOR MODAL BOXES ##########
      
      ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########
      
      # Observe statement for show/hide export options box
      observe(if (input$exportOptions == 0) {
        shinyjs::hide(id = "exportOptionsBox")
      })
      
      # Observe statement for show/hide ego network box
      observe(if (input$egoNetwork == 0) {
        shinyjs::hide(id = "egoNetworkBox")
      })
      
      # Observe statement for show/hide advanced options box
      observe(if (input$advancedOptions == 0) {
        shinyjs::hide(id = "advancedOptionsBox")
      })
      
      # Observe statement for show/hide shortest paths box
      observe(if (input$shortestPaths == 0) {
        shinyjs::hide(id = "shortestPathsBox")
      })
      
      # Observe statement for show/hide weights box
      observe(if (input$weightsOptions == 0) {
        shinyjs::hide(id = "weightsBox")
      })
      
      ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########
      
      ########## OBSERVE EVENTS FOR OPENING/CLOSING HIDE/SHOWS ##########
      # Observe statement for show/hide weights options box
      observeEvent(input$weightsOptions, {
        
        if(input$weightsOptions %% 2 == 1){
          shinyjs::show(id = "weightsBox")
        }else{
          shinyjs::hide(id = "weightsBox")
        }
      })
      
      # Observe statement for show/hide export options box
      observeEvent(input$exportOptions, {
        
        if(input$exportOptions %% 2 == 1){
          shinyjs::show(id = "exportOptionsBox")
        }else{
          shinyjs::hide(id = "exportOptionsBox")
        }
      })
      
      # Observe statement for show/hide ego network box
      observeEvent(input$egoNetwork, {
        
        if(input$egoNetwork %% 2 == 1){
          shinyjs::show(id = "egoNetworkBox")
        }else{
          shinyjs::hide(id = "egoNetworkBox")
        }
      })
      
      # Observe statement for show/hide advanced options box
      observeEvent(input$advancedOptions, {
        
        if(input$advancedOptions %% 2 == 1){
          shinyjs::show(id = "advancedOptionsBox")
        }else{
          shinyjs::hide(id = "advancedOptionsBox")
        }
      })
      
      # Observe statement for show/hide shortest paths box
      observeEvent(input$shortestPaths, {
        
        if(input$shortestPaths %% 2 == 1){
          shinyjs::show(id = "shortestPathsBox")
        }else{
          shinyjs::hide(id = "shortestPathsBox")
        }
      })
      
      ########## OBSERVE EVENTS FOR OPENING/CLOSING HIDE/SHOWS ##########
      
      
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
      
        rawcases_filtered <- reactive({
        if (cluster() == 'All') {
          rawcases_filt <- rawcases %>% select(-Group)
          rawcases_filt_super <<- nrow(rawcases_filt)
          rawcases_filt
        } else {
          rawcases_filt <- rawcases %>% filter(Group == cluster()) %>% select(-Group)
          rawcases_filt_super <<- nrow(rawcases_filt)
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

        weights$to <- unname(dynamic_nodes_ids[weights$to])
        weights$from <- unname(dynamic_nodes_ids[weights$from])
        
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
          
          links5_download <<- new_links
          
          new_links
      
        } else {
          links5_download <<- links4()
          links4()
        }
      })
      
      nodes4 <- reactive({
        if (remove_unconnecteds() == "Yes" & ego_network() == FALSE & shortest_paths_toggle() == FALSE){
          
          igraph <- graph_from_data_frame(links5(), vertices = nodes3(), directed = F)
          
          distances <- as.data.frame(distances(igraph))
          distances[sapply(distances, is.infinite)] <- 0
          
          distances <- distances %>% select_if(~sum(.) == 0)
          
          removes <- as.vector(colnames(distances))
          
          new_nodes <- nodes3()[!nodes3()$id %in% removes, ]
          
          nodes4_download <<- new_nodes
          #examine_nodes_dropdown_ids <<- new_nodes
          examine_nodes_dropdown_ids <<- setNames(new_nodes$id, new_nodes$label)
          
          user_set_seed <<- seed()
          
          new_nodes
        } else {
          nodes4_download <<-  nodes3()
          #examine_nodes_dropdown_ids <<- nodes3()
          examine_nodes_dropdown_ids <<- setNames(nodes3()$id, nodes3()$label)
          user_set_seed <<- seed()
          nodes3()
        }
      })
      
      # if(!(input$to_node %in% nodes4())){output$network_warning <- renderText({"Please make a valid selection!"})
      # print("this is running renderText")
      # return()}
      
      final_network <- reactive({

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
          visIgraphLayout(layout = layout())
      })
      
      
      
      final_network_download <<- reactive(final_network())
      
      nodes_to_dl <- reactive(nodes4())
      
      links_to_dl <- reactive({
        # Get the links5() dataframe
        links_df <- links5()
        
        # Remove the "color" and "width" columns
        subset_df <- links_df[, !(names(links_df) %in% c("color", "width"))]
        
        subset_df$to <- names(dynamic_nodes_ids)[match(subset_df$to, dynamic_nodes_ids)]
        subset_df$from <- names(dynamic_nodes_ids)[match(subset_df$from, dynamic_nodes_ids)]
        
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
      
      str1 <- reactive({
        
        if(direct_connections() != 0){
          str1 <- paste("Number of direct links:", direct_connections())
          str1
        } else {
          str1 <- paste("This node is unconnected")
          str1
        }
        
      })
      
      str2 <- reactive({
        
        if(standardised_score() != 0){
          str2 <- paste("Standardised connection score:", standardised_score())
          str2
        } else {
          str2 <- paste("This node is unconnected")
          str2
        }
        
      })
      
      str3 <- reactive({
        
        if(is.nan(node_average_distance()) == FALSE){
          str3 <- paste("Average node distance:", node_average_distance())
          str3
        } else {
          str3 <- paste("This node is unconnected")
          str3
        }
        
      })
      
      str4 <- reactive({  
        
        if(is.nan(node_average_distance()) == FALSE){
          str4 <- paste("The average degree of seperation in your network is:", round(network_mean_distance(), 2))  
          str4
        } else {
          str4 <- paste("This network has no connections between nodes")
          str4
        }  
        
      })
      
      str5 <- reactive({  paste("The maximum distance (diameter) of your network is:", network_diameter())  })
      
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
      
      #html_object <<- final_network() 
      
      #output$text <- renderText({  HTML(paste(str1(), str2(), str3(), str4(), str5(), sep = '<br/>'))  })
      
      output$text <- renderText({  HTML(paste(str1(), str2(), str3(), str4(), str5(), sep = '<br/>'))  })
      
      ########## MAKING THE WEIGHTS DOWNLOAD ##########
      
      weights_links <- reactive({
        # Get the links5() dataframe
        links_df <- links5()
        
        # Remove the "color" and "width" columns
        subset_df <- links_df[, !(names(links_df) %in% c("color", "width"))]
        
        subset_df$to <- names(dynamic_nodes_ids)[match(subset_df$to, dynamic_nodes_ids)]
        subset_df$from <- names(dynamic_nodes_ids)[match(subset_df$from, dynamic_nodes_ids)]

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
      
      if(is.null(current_kmeans_solution) == FALSE) {
        file_names <- c(file_names, kmeans_files)
        info_text <- c(info_text, kmeans_files)
      }
      if(is.null(current_som_solution) == FALSE){
        file_names <- c(file_names, som_files)
        info_text <- c(info_text, som_files)
      }
      if(is.null(agent_cluster_tracker) == FALSE && agent_cluster_tracker@cluster_tested != "None"){
        file_names <- c(file_names, policy_files)
        info_text <- c(info_text, policy_files)
      }
      if(is.null(predicted_cases)== FALSE){
        file_names <- c(file_names, predict_files)
        info_text <-c(info_text,predict_files)
      }
      if(is.null(network_initialised) == FALSE){
        file_names <- c(file_names, systems_mapping_files)
        info_text <-c(info_text,systems_mapping_files)
        
      }
      
      fs <-file_names
      fileConn<-file("info.txt")
      writeLines(info_text, fileConn)
      close(fileConn)
      
      if(is.null(current_kmeans_solution) == FALSE){
        fstat <- pseudoF(current_data_file, current_kmeans_solution, nrow(current_kmeans_solution@ucenters))
        fstat_col <- rep(0, nrow(current_kmeans_solution@ucenters)-1)
        fstat_col <- c(fstat, fstat_col)
        kcenters <- cbind(current_kmeans_solution@ucenters, "size" = current_kmeans_solution@usize, "Pseudo_F" = fstat_col)
        write.csv(kcenters, file = "kmeans_profiles.csv")
        clustered_data <- cbind(current_data_file, "clus" = current_kmeans_solution@uclusters)
        write.csv(clustered_data, file = "clustered_data.csv")
        if(input$setrandseedkmean == "Yes"){kseed = as.character(input$randseedkmean)}
        else{kseed = "NULL"}
        fileConn<-file("kmean_seed.txt")
        writeLines(kseed, fileConn)
        close(fileConn)
        pdf("silh_plot.pdf")
        plot_silhouette(current_data_file, current_kmeans_solution)
        dev.off()
      }
      if(is.null(current_som_solution) == FALSE){
        som_options =list("x dim" = current_som_solution$parameters$the.grid$dim[1], "y dim" = current_som_solution$parameters$the.grid$dim[2],
                          "proto_init" = current_som_solution$parameters$init.proto, "max_iter" = current_som_solution$parameters$maxit,
                          "data_scaling" = current_som_solution$parameters$scaling, "gradient_descent" = current_som_solution$parameters$eps0)
        som_options_df <- as.data.frame(som_options)
        write.csv(som_options_df, file = "som_options.csv")
        class_summary <- capture.output(summary(current_som_solution))
        temp_qual <- quality(current_som_solution)
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
        som_profiles <- as.data.frame(current_som_solution$prototypes)
        write.csv(som_profiles, file = "som_profiles.csv")
        data_quadrants <- cbind(current_data_file, "quadrant" = current_som_solution$clustering)
        write.csv(data_quadrants, file = "data_quadrants.csv")
        
        temp.dim<-current_som_solution[["parameters"]][["the.grid"]][["dim"]]
        pdf("som_barplot.pdf")
        plot(x=current_som_solution, what="obs", type="barplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
        dev.off()
        pdf("som_boxplot.pdf")
        plot(x=current_som_solution, what="obs", type="boxplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim)))
        dev.off()
        
      }
      if(is.null(agent_cluster_tracker) == FALSE && agent_cluster_tracker@cluster_tested != "None"){
        write.csv(agent_cluster_tracker@checked_data, file ="adjust_kmeans.csv")
        temp_names <- names(current_data_file)
        intervention =c()
        deviation =c()
        input_var = 1
        for(i in 1:length(temp_names)){
          if(agent_cluster_tracker@sensitivity_test[i] != 0){
            intervention =c(intervention, agent_cluster_tracker@sensitivity_test[i])
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
        for(i in 1:length(agent_cluster_tracker@sensitivity_result)) {
          
          if(agent_cluster_tracker@sensitivity_result[[i]]> 0){
            sub_sol_names <-c(sub_sol_names, i)
            sub_sol_space <- c(sub_sol_space, agent_cluster_tracker@sensitivity_result[[i]])
          }}
        
        pdf("sensitivity.pdf")
        barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen")
        dev.off()
      }
      if(is.null(predicted_cases) == FALSE){
        write.csv(predicted_cases, file ="predicted_quadrants.csv")
        
      }
      
       if(is.null(network_initialised) == FALSE){
         
         write.csv(nodes4_download, file ="nodes_list.csv")
         
         links5_download <- links5_download[, !(names(links5_download) %in% c("color", "width"))]
         
         links5_download$to <- names(dynamic_nodes_ids)[match(links5_download$to, dynamic_nodes_ids)]
         links5_download$from <- names(dynamic_nodes_ids)[match(links5_download$from, dynamic_nodes_ids)]

         write.csv(links5_download, file ="edges_list.csv")
         
         fileConn<-file("set_network_seed.txt")
         set_seed_info <- paste('Seed set for network visualisation is: ', user_set_seed, sep = '')
         writeLines(set_seed_info, fileConn)
         close(fileConn)
         
       }
      
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
    
    if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
      choices <<- c("All", sort(unique(na.omit(current_kmeans_solution@uclusters))))
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
  
  
  # Create a reactive value that wraps the global variable
  reactive_current_data_file <- reactiveVal(current_data_file)
  
  # Update the reactive value when the global variable changes
  observe({
    reactive_current_data_file(current_data_file)
  })
  
  dynamic_nodes <- reactive({
    if(is.null(current_data_file)) {
      return(NULL)
    } else {
      data.frame(id = seq_along(current_data_file[,1:ncol(current_data_file)]),
                 label = colnames(current_data_file[,1:ncol(current_data_file)]))
    }
  })

  dynamic_nodes_ids <<- reactive({
    if(is.null(dynamic_nodes())) {
      return(NULL)
    } else {
      setNames(dynamic_nodes()$id, dynamic_nodes()$label)
    }
  })
  
  ##############################################
  
  examine_nodes_dropdown <- reactive({
    if(is.null(current_data_file)) {
      return(NULL)
    } else {
      
      data.frame(id = seq_along(current_data_file[,1:ncol(current_data_file)]),
                 label = colnames(current_data_file[,1:ncol(current_data_file)]))
    
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
    
    if (!is.null(current_data_file)) {
      
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
  
    if(is.null(current_data_file)){output$network_warning <- renderText({"Please upload data first."})
    print("this is running renderText")
    return()}
    
  observe({
    updateSelectInput(session = session, inputId = "target_node", choices = dynamic_nodes_ids)
  })

  observe({
    updateSelectInput(session = session, inputId = "from_node", choices = dynamic_nodes_ids)
  })

  observe({
    updateSelectInput(session = session, inputId = "to_node", choices = dynamic_nodes_ids)
  })

  observe({
    
    #if ( nrow(nodes4_download) < length(dynamic_nodes_ids) ){
    
    updateSelectInput(session = session, inputId = "chosen_node", choices = examine_nodes_dropdown_ids)
    
    #} else {
      
    #updateSelectInput(session = session, inputId = "chosen_node", choices = dynamic_nodes_ids)
      
    #}
    
  })
  
  observe({
  updateSelectInput(session = session, inputId = "cluster", choices = choices)
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

                   updateSelectInput(session = session, inputId = "chosen_node", choices = examine_nodes_dropdown_ids)

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
  #                        confirmButtonCol = "#AEDEF4",
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
    if (!(input$target_node %in% examine_nodes_dropdown_ids) & !is.null(network_initialised)) {
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
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  observeEvent(input$from_node, {
    if (!(input$from_node %in% examine_nodes_dropdown_ids) & !is.null(network_initialised)) {
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
        confirmButtonCol = "#AEDEF4",
        timer = 0,
        imageUrl = "",
        animation = TRUE
      )
    }
  })
  
  observeEvent(input$to_node, {
    if (!(input$to_node %in% examine_nodes_dropdown_ids) & !is.null(network_initialised)) {
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
        confirmButtonCol = "#AEDEF4",
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
                   
                   if (kmeans_count$value > 0 && !is.null(current_kmeans_solution)) {
                     
                     choices_df <- data.frame(clusters = current_kmeans_solution@uclusters)
                     choices_value_counts <- table(choices_df$clusters)
                     choices_filtered_values <- as.numeric(names(choices_value_counts[choices_value_counts > 4]))
                     choices <<- c("All", sort(choices_filtered_values))
                     
                     print("This has run!!!")
                     
                   }})
                 
                 observe({
                   updateSelectInput(session = session, inputId = "cluster", choices = choices)
                 })
                 
                 
               })
  
  
  
  # 
  #   }
  # 
  # })
  
}  

