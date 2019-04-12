library(shiny)
suppressMessages(library(SOMbrero))
library(cluster)
library(shinyFiles)
library(rhandsontable)
library(ggplot2)
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
    
    
    #right now this just deselects not numeric data columns, later should it auto subset these?
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
    full_data <<- the.table #check if needed
    current_data_file <<- the.table[numeric_only_columns]
    the.table#this is set such that the last thing in this method/function is the table and is therefore set to dInput
    
  })
  observeEvent(input$subset_data,{
    d.input <- dInput()
    
    subset_list <- input$varchoice
    if(length(ncol(d.input) >= length(subset_list))){
      #subset_list <- c()
      #for(i in 1:length(input$varchoice)){
      #subset_list <-c(subset_list, input$varchoice[i])
      #}
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
  
  #### Panel 'Cluster Data'
  #############################################################################
  #this observe event looks for users to press 'get clusters'
  observeEvent(input$init_kmeans, { 
    if(is.null(current_data_file)){return()}  #this function will return nothing if there is no data
    #print(current_data_file)
    current_kmeans_solution <<- create_user_gen_kmeans_solution("default_name", kmeans(current_data_file, isolate(input$clusters)))
    
    
    
    
    output$kmeans_tab <- renderTable({
      
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label = generate_cluster_labels()
      
      cen_tab <- cbind("Cluster" = clus_label, current_kmeans_solution@ucenters, "Size" = current_kmeans_solution@usize)
      
    })
    #initializes the directory and save function
   
    observeEvent(input$save, {
      roots =c(wd='.')
      shinyFileSave(input, 'save', roots=roots)
      
      fileinfo <- parseSavePath(roots, input$save)
      k_data <- cbind(current_data_file, "clus_labels" = current_kmeans_solution@uclusters)
      print(fileinfo$datapath)
      write.csv(k_data, fileinfo$datapath)
      
      #save the cluster summary details
      sumfileinfo <- extend_filename(fileinfo$datapath, "_kstats.")
      summarykstats <- cbind(as.data.frame(current_kmeans_solution@ucenters), "clus_size" = current_kmeans_solution@usize)
      write.csv(summarykstats, sumfileinfo)
      
      #save silhouette if checked
      if (input$silhouette == TRUE){
        
        setwd(".")
        pdf("temp_silh.pdf")
        plot_silhouette(current_data_file, current_kmeans_solution) 
        dev.off()
      }
      
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
      output$som_dimwarning <- renderText({"SOM dimensions must be 3 or greater and 15 or lesser."})
      return()
    }
    else
    {
      output$som_dimwarning <- renderText({""})
    }
    tmp_data <- current_data_file
    mapped_labels = NULL
    if(!is.null(current_kmeans_solution)){
      mapped_labels <- create_kmeans_SOM_mapping()
      rownames(tmp_data) <- mapped_labels
    }
    
    set.seed(input$rand.seed)
    current_som_solution<<- trainSOM(tmp_data, dimension=c(input$dimx,input$dimy), 
                                     maxit=input$maxit, scaling=input$scaling, init.proto=input$initproto)
   
    updatePlotSomVar() # update variable choice for som plots
    output$trainnotice <- renderUI({
        ### post the quality control factors as well
        ###THIS IS WHERE YOU CUT CODE OUT###
        qual_measures <- quality(current_som_solution)
        somsummarydata<-capture.output(summary(current_som_solution))
        #now print out the results
        tagList(
          p(paste("Trained SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")),
          p(paste("Topo Error  ", format(qual_measures$topographic,digits=4),sep=" ")),
          p(paste("Quant Error ", format(qual_measures$quantization,digits=4),sep=" ")),
          paste(somsummarydata,collapse="\n\n")
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
    updateSelectInput(session, "somplotvar2", choices=tmp.names, 
                      selected=tmp.names[1:min(5,length(tmp.names))])
  })
  # Plot the SOM 
  output$somplot <- renderPlot({
    if(is.null(current_data_file))
      return(NULL)
    if(input$trainbutton ==0)
      return(NULL)
    
    tmp.view <- NULL
    if (input$somplottype =="boxplot") {
      tmp.var <- (1:ncol(current_som_solution$data))[colnames(current_som_solution$data) %in% 
                                                       input$somplotvar2]
    }
    else {tmp.var <- input$somplotvar}

    #This if/else set is here to add cluster labels to neurons for observation plots only
    temp.dim<-current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
    if(input$somplotwhat =='obs'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
                                       variable=tmp.var,view=tmp.view, print.title = TRUE,the.titles = paste("Quadrant ", 1:prod(temp.dim)))}
    #if(input$somplotwhat =='obs'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
    #                                   variable=tmp.var,view=tmp.view, print.title = TRUE)}
    else {
    plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
         variable=tmp.var,view=tmp.view)
        }
    
  })
  observeEvent(input$save_som, {
               previous_som <- current_som_solution #need to rename the object or it may overwrite later current_som objects
               save(previous_som, file = "./tmp/SavedSOMObject")})
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
    
 
    
    the.table_p <- na.omit(read.csv(in.file_pred$datapath, header=input$header_pred, 
                                    sep=the.sep_p))
  
    numeric_only_columns <- column_type_identifier(the.table_p)

    the.table_p[numeric_only_columns]
  })
  
  
  observeEvent(input$classify_prof, {
    temp_som <- current_som_solution
    p.input <- pInput()
    if (input$load_prev_som == TRUE) {
      tryCatch(load("./tmp/SavedSOMObject"), error = function(e) NULL)
      temp_som <- previous_som #if there is no file to load, previous_som will be NULL from global
    }
    if (is.null(p.input) | is.null(temp_som)) {
      print("Nothing here!")
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
      p.input <- cbind(p.input, 'Best Quadrant' = predicted, '2nd Best Quadrant' = as.integer(BMUS[,2]))
      
      output$view_predict <- renderTable({
        head(p.input, n=input$nrow.result_pred)
      })
    }
  })
  
  
  
  #### Panel 'Agent-Model'
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
                                           print.title = TRUE,the.titles = paste("Quadrant ", 1:prod(temp.dim)))
      })
      output$Agent_Warning <- renderText({})
      output$sensitivity_barplot <- NULL 
    })
    #still need to update case -> cluster in this function
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
      #agent_grid_plot
    })
    
    
  })
  observeEvent(input$back_cluster, {
    newstate <- update_tracker_current_state(agent_cluster_tracker, agent_cluster_values, -1)
    agent_cluster_tracker@current_state <<- agent_cluster_tracker@possible_states[newstate]
    output$clusters_editable_table <- renderRHandsontable({
      rhandsontable(agent_cluster_values[[agent_cluster_tracker@current_state]])
      })
    #values$update_data <- values$update_data + 1
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
      #need to have this here and for the observe event sa_ok, how to have it in both spaces?
     
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
  
  
  #########
  output$downloadReport <- downloadHandler(
 
    
    filename = 'report_docs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      file_names <- c("info.txt")
      info_text = "This directory contains the following files:"
      kmeans_files <- c("kmeans_profiles.csv", "clustered_data.csv", "silh_plot.pdf")
      som_files <- c("som_options.csv", "summary_class.txt", "som_profiles.csv", "data_quadrants.csv", "som_barplot.pdf", "som_boxplot.pdf")
      policy_files <- c("adjust_kmeans.csv", "tested_intervention.csv", "sensitivity.pdf")
      
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
        som_profiles <- as.data.frame(current_som_solution$prototypes)
        write.csv(som_profiles, file = "som_profiles.csv")
        data_quadrants <- cbind(current_data_file, "quadrant" = current_som_solution$clustering)
        write.csv(data_quadrants, file = "data_quadrants.csv")
        
        temp.dim<-current_som_solution[["parameters"]][["the.grid"]][["dim"]] 
        pdf("som_barplot.pdf")
        plot(x=current_som_solution, what="obs", type="barplot",
             print.title = TRUE,the.titles = paste("Quadrant ", 1:prod(temp.dim)))
        dev.off()
        pdf("som_boxplot.pdf")
        plot(x=current_som_solution, what="obs", type="boxplot",
              print.title = TRUE,the.titles = paste("Quadrant ", 1:prod(temp.dim)))
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
      
      

      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  
 
  
    # Run Clusters Button Pressed
    # observeEvent(input$Agent_Run_Clusters,{
    # Agent_SOM <- Agent_SOM_loaded
    # SOMgriddatanew<-SOMgriddata
    # #Predict the Cluster Neurons from the centroids
    # Agents_Clusters_predicted <- predict(Agent_SOM, the.table_agent_clusters[,2:(as.numeric(length(the.table_agent_clusters))-1)])
    # #First, initialize the Agent_SOM to blanks creating an empty array
    # for (i in 1:length(SOMgriddatanew)){ 
    # SOMgriddatanew[i]<-NA
    # }
    # #Next replace the labels with the cluster neurons
    # for (i in 1:length(the.table_agent_clusters$clus_size)){
    #   SOMgriddatanew[Agents_Clusters_predicted[i]]<-i
    # } 
    # output$somplotagent <- renderPlot({
    #   color2D.matplot(SOMgriddatanew, show.values = TRUE, axes = FALSE, xlab = "", ylab = "", vcex = 2, vcol = "black",extremes = c("red", "yellow"),na.color="black")
    # })
    # output$view_predict_clusters <- renderTable(the.table_agent_clusters)
    # })
    
 
  # Use Previous Button Pressed
  # observeEvent(input$Agent_Use_Prev_SOM,{
  #   output$view_predict_cases <- renderTable(the.table_agent_cases)
  # })
  
  
 
  

  
}  
  
