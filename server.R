library(shiny)
suppressMessages(library(SOMbrero))
library(cluster)
library(FactoMineR)
library(plot3D)
library(shinyFiles)
library(plotrix) 



server <- function(input, output, session) {
  
  output$complexit_logo <- renderImage(list(src="complexit_logo_small2.jpg"), 
                                       deleteFile=FALSE)
  

  #### Panel 'Import data'
  ##############################################################################
  dInput <- reactive({
    in.file <- input$file1
    if (is.null(in.file))
      return(NULL)
    
    
    the.sep <- switch(input$sep, "Comma"=",", "Semicolon"=";", "Tab"="\t",
                      "Space"="")
    
    the.quote <- switch(input$quote, "None"="","Double Quote"='"',
                        "Single Quote"="'")
    
    
    the.table <- na.omit(read.csv(in.file$datapath, header=input$header, 
                                  sep=the.sep, quote=the.quote))
    
    
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
    full_data <<- the.table ###this may be redundant and unnecessary??
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
    #print(current_data_file)
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
      
     
      #current_kmeans_solution <<- create_user_saved_kmeans_res("default_name", k$centers, k$cluster, k$size)
      ### Save the original kmeans solution as well to more easily call off-the-shelf prediction routines
      #current_kmeans_original_solution <<- k
      #this block creates the 'Cluster 1, 2...n' labels for the table display in Shiny
      clus_label =c()
      for(i in 1: nrow(current_kmeans_solution@ucenters)){
        clus_label = c(clus_label, paste(c("Cluster"), toString(i), sep = " "))
      }
      cen_tab <<- cbind(clus_label, current_kmeans_solution@ucenters, size = current_kmeans_solution@usize)
      
    })
    #initializes the directory and save function
    roots =c(wd='.')
    shinyFileSave(input, 'save', roots=roots)
    observeEvent(input$save, {
      
      fileinfo <- parseSavePath(roots, input$save)
      
      k_data <- cbind(current_data_file, "clus_labels" = current_kmeans_solution@uclusters)
      
      write.csv(k_data, as.character(fileinfo$datapath))
      
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
    
    #this function displays the pseudoF
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
    ########  
  })  
  output$trainnotice <- renderUI({
    if(input$trainbutton == 0){
      return()
    }
    else{
      ### post the quality control factors as well
      qc<-quality(current_som_solution)
      tagList(
        p(paste("Trained SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")),
        p(paste("Topo Error  ", format(qc$topographic,digits=4),sep=" ")),
        p(paste("Quant Error ", format(qc$quantization,digits=4),sep=" "))
        )
    }
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
    if(input$somplotwhat =='obs'){plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
                                       variable=tmp.var,view=tmp.view, print.title = TRUE)}
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
  
  #### Panel 'Agent-Model'
  #############################################################################
  # Plot the SOM
    #
  
    # Setup Button Pressed
    observeEvent(input$Agent_Setup,{
      output$somplotagent <- renderPlot({
        color2D.matplot(SOMgriddata, show.values = TRUE, axes = FALSE, xlab = "", ylab = "", vcex = 2, vcol = "black",extremes = c("red", "yellow"),na.color="black")
      })
    })
  
    # Run Clusters Button Pressed
    observeEvent(input$Agent_Run_Clusters,{
    Agent_SOM <- Agent_SOM_loaded
    SOMgriddatanew<-SOMgriddata
    #Predict the Cluster Neurons from the centroids
    Agents_Clusters_predicted <- predict(Agent_SOM, the.table_agent_clusters[,2:(as.numeric(length(the.table_agent_clusters))-1)])
    #First, initialize the Agent_SOM to blanks creating an empty array
    for (i in 1:length(SOMgriddatanew)){ 
    SOMgriddatanew[i]<-NA
    }
    #Next replace the labels with the cluster neurons
    for (i in 1:length(the.table_agent_clusters$clus_size)){
      SOMgriddatanew[Agents_Clusters_predicted[i]]<-i
    } 
    output$somplotagent <- renderPlot({
      color2D.matplot(SOMgriddatanew, show.values = TRUE, axes = FALSE, xlab = "", ylab = "", vcex = 2, vcol = "black",extremes = c("red", "yellow"),na.color="black")
    })
    output$view_predict_clusters <- renderTable(the.table_agent_clusters)
    })
    
  # Run Cases Button Pressed
  observeEvent(input$Agent_Run_Cases,{
    output$view_predict_cases <- renderTable(the.table_agent_cases)
    tmp.var <- input$somplotvaragent
    output$somplotagent <- renderPlot({
    plot(x=Agent_SOM_loaded, what="obs", type="names",variable=NULL,view=NULL, print.title = TRUE)
    }) 
  })
  # Use Previous Button Pressed
  observeEvent(input$Agent_Use_Prev_SOM,{
    output$view_predict_cases <- renderTable(the.table_agent_cases)
  })
  
  
  #Not sure what these two do
  observe({
    updateSelectInput(session, "somplottypeagent", 
                      choices=all.somplot.types[["numeric"]][[
                        input$somplotwhatagent]])
  })
    # update variables available for plotting
  updatePlotSomVaragent <- function() observe({
    tmp.names <- colnames(Agent_SOM$data)
    
    updateSelectInput(session, "somplotvaragent", choices=tmp.names)
    updateSelectInput(session, "somplotvar2agent", choices=tmp.names, 
                      selected=tmp.names[1:min(5,length(tmp.names))])
  })
  

  
  
  
#   output$somplotagent <- renderPlot({
#    plot(x=Agent_SOM_loaded, what="obs", type="names",variable=NULL,view=NULL, print.title = TRUE)
#    tmp.view <- NULL
#    if (input$somplottypeagent =="boxplot") {
#      tmp.var <- (1:ncol(Agent_SOM$data))[colnames(Agent_SOM$data) %in% 
#                                                       input$somplotvar2agent]
#    }
#    else {tmp.var <- input$somplotvaragent}
#    #This if/else set is here to add cluster labels to neurons for observation plots only
#    if(input$somplotwhatagent =='obs'){plot(x=Agent_SOM, what=input$somplotwhatagent, type=input$somplottypeagent,
#                                       variable=tmp.var,view=tmp.view, print.title = TRUE)}
#    else {
#      plot(x=Agent_SOM, what=input$somplotwhatagent, type=input$somplottypeagent,
#           variable=tmp.var,view=tmp.view)
#    }
#  })
  
  
    #  
  #  output$somplot <- renderPlot({
#    if(is.null(current_data_file))
#      return(NULL)
#    tmp.view <- NULL
#    tmp.var <- input$somplotvar
    #This if/else set is here to add cluster labels to neurons for observation plots only
#    plot(x=current_som_solution, what=input$somplotwhat, type=input$somplottype,
#                                      variable=tmp.var,view=tmp.view, print.title = TRUE)
# })
  
#  pInput <- reactive({
#    in.file_pred <- input$file_pred
#    if (is.null(in.file_pred))
#      return(NULL)
#    
#    the.sep_p <- switch(input$sep_pred, "Comma"=",", "Semicolon"=";", "Tab"="\t",
#                      "Space"="")
#    
#    the.quote_p <- switch(input$quote_pred, "None"="","Double Quote"='"',
#                        "Single Quote"="'")
#    
#    the.table_p <- na.omit(read.csv(in.file_pred$datapath, header=input$header_pred, 
#                                  sep=the.sep_p, quote=the.quote_p))
#    
#    numeric_only_columns <- column_type_identifier(the.table_p) 
#    the.table_p[numeric_only_columns]
#  })
#
#  
#  observeEvent(input$classify_prof, {
#    temp_som <- current_som_solution
#    warning <- "No new cases uploaded for profile recognition"
#    p.input <- pInput()
#    if (input$load_prev_som == TRUE) {
#      tryCatch(load("./tmp/SavedSOMObject"), error = function(e) NULL)
#      temp_som <- previous_som #if there is no file to load, previous_som will be NULL from global
#      warning <- "No previous saved SOM"
#    }
#    if (is.null(p.input) | is.null(temp_som)) {
#      #this is very clunky but should handle the two major kinds of errors for now
#      if(is.null(p.input)){
#        warning <- "No new cases uploaded for profile recognition"
#      }
#      output$prof_rec_error <- renderUI({
#        tagList(
#        strong(paste("Warning!", warning, sep = " ")),
#        br()
#        )
#      })
#      return(NULL)}
#    else {
#      predicted <- predict(temp_som, p.input)
#      warning <- ""
#      #so very clunky just repeating the code to get rid of the warning when it goes to print the predict results
#      output$prof_rec_error <- renderUI({
#        warning
#      })
#      p.input <- cbind(p.input, 'Matched Neuron' = predicted)
#    #some prediction function goes here
#      output$view_predict <- renderTable({
#     
#        head(p.input, n=input$nrow.result_pred)
#    })
#    }
#  })
}


########################Discarded Code
# output$download_clusters <- downloadHandler(
#   filename = function() {
#     paste(substr(input$file1$name,1, nchar(input$file1$name)-4), format(Sys.time(),format="_%Y-%m-%d_%H.%M"), ".csv", sep="")
#   },
#   content = function(file){
#     write.csv(cen_tab, file)
#   }, 
#   contentType = "text/csv"
#   
# )
#this block of code appends the cluster labels to the data and then writes out to the C:\ directory
#clus <- k$cluster
#k_data <- cbind(current_data_file,clus)

### Initialize the predict.kmeans function:
# predict.kmeans=
#   function(km, data)
#   {k <- nrow(km@ucenters)
#   n <- nrow(data)
#   d <- as.matrix(dist(rbind(km@ucenters, data)))[-(1:k),1:k]
#   out <- apply(d, 1, which.min)
#   return(out)}
# 
# 
# ### see if predicted kmeans is selected
# if (input$Predicted_Kmeans_solution == TRUE) {
#   ### Then predict new results assuming it is in the current data file
#   predicted_cluster_data <<- predict.kmeans(current_kmeans_solution, current_data_file)
# }
# 
# ### see if predicted SOM is selected
# if (input$Predicted_SOM_solution == TRUE) {
#   ### Then predict new results assuming it is in the current data file
#   predicted_cluster_data <<- predict(current_som_solution, current_data_file)
# }

### if predicted is selected, use the new solutions
# if (input$Predicted_Kmeans_solution == TRUE) {
#   ### assign k_data to the new cluster values
#   current_kmeans_solution_predicted<-current_kmeans_solution
#   current_kmeans_solution_predicted@uclusters<-as.list(as.numeric(predicted_cluster_data))
#   k_data <- append_cluster_labels(current_kmeans_solution_predicted, current_data_file)
# }
# if (input$Predicted_SOM_solution == TRUE) {
#   ### assign k_data to the new cluster values
#   current_kmeans_solution_predicted<-current_kmeans_solution
#   current_kmeans_solution_predicted@uclusters<-as.list(as.numeric(predicted_cluster_data))
#   k_data <- append_cluster_labels(current_kmeans_solution_predicted, current_data_file)
