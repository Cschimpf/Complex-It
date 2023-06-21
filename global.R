####Global Variables#####


# Any code in this global.r file will run one time prior to launching
# the Shiny App.  The code will not continually scan in the background
# while the App runs, so a good place for intialization of variables

#Data Variables
####
current_data_file = NULL
full_data = NULL
current_kmeans_solution = NULL
current_som_solution = NULL
previous_som = NULL
predicted_cases = NULL
monte_carlo = NULL
rawcases = NULL
dynamic_nodes_ids = NULL
choices = "All"
network_initialised = NULL
pop_up = FALSE

agent_case_tracker = NULL
agent_cluster_tracker = NULL
agent_grid_plot = NULL
agent_grid_colors = NULL
agentdf = NULL
agent_drawtools = NULL
displacement = list("1" =c(0, 2), "2" =c(0,-2), "3" =c(2,0), "4" =c(-2,0), "5" =c(-5,0), "6" =c(0,5))

all.somplot.types <- list("numeric"=
                            list("prototypes"=
                                   list("3d", "barplot",
                                        "smooth.dist",
                                        "umatrix"),
                                 "obs"=c("color", "barplot", 
                                         "names", "boxplot")))

####Global Classes and related Functions####
setClass("user_gen_kmeans_solution", representation(save_name = "character", uclusters = "integer", ucenters = "matrix",
                                                    utotss = "numeric", uwithinss = "numeric", utotwithinss = "numeric",
                                                    ubetweenss = "numeric", usize = "integer"))

create_user_gen_kmeans_solution <- function(objectname, km){
  new_class = new("user_gen_kmeans_solution", save_name = objectname, uclusters = km$cluster, ucenters = km$centers, utotss = km$totss,
                  uwithinss = km$withinss, utotwithinss = km$tot.withinss, ubetweenss = km$betweenss, usize = km$size)
  return(new_class)
}
                                            
                    




#####General Purpose Functions####


#### 'General'
##################################

extend_filename <- function(filename, ext){
 
  splitname = strsplit(as.character(filename), "[.]")
  
  if(length(splitname[[1]]) == 3){
    splitname[[1]][2] = paste(splitname[[1]][2], ext, sep="")
    finalname = paste('.', splitname[[1]][2], splitname[[1]][3], sep="")
  }
  return(finalname)
}

#uses the number of data rows to calculate a dimension for the graph, width/height 
#will return 400 as a default height if the scaled nrows is less than 400
graph_dimension = function(data, scale = 5){
  dimension = (nrow(data) * scale)
  if (dimension < 400) 
  {dimension = 400}
  return(dimension)
}

#used by multiple 'tabs' for generating clusters
generate_cluster_labels <-function(){
  clus_label =c()
  for(i in 1: nrow(current_kmeans_solution@ucenters)){
    clus_label = c(clus_label, paste(c("Cluster"), toString(i), sep = " "))
  }
  return(clus_label)
}


####Panel 'Import Data'
##################################

column_type_identifier <-function(data){
  numeric_only_columns <-sapply(data, class) %in% c("integer", "numeric")
  return(numeric_only_columns)
 
}

logic_vector_inverter <- function(logic_vec){
  new_logic_vec = logic_vec
  for(i in 1:length(logic_vec)){
    if(logic_vec[i] == TRUE){
      new_logic_vec[i] = FALSE
    }
    else{
      new_logic_vec[i] = TRUE
    }
  }
  return(new_logic_vec)
}


####Panel 'Cluster Data'
##################################

#calculate PSEUDO F

pseudoF = function(data,sol, k){
  T = sum(scale(data, scale=F)^2)
  W <- sum(sol@uwithinss) #this is using the withinss from 'k' above
  pF = ((T-W)/(k-1))/(W/(nrow(data)-k))
  return(pF)
}

plot_silhouette <- function(data, km){
  dissM <- daisy(data)
  sil_plot <- plot(silhouette(km@uclusters, dissM)) 
  return(sil_plot)
}

generate_data_summary <- function() {
  summary_row <-c("Total Size & Var Avg")
  summary_row <-c(summary_row, nrow(current_data_file))
  col_names <- names(current_data_file)
  for(i in 1:ncol(current_data_file)){
    var_avg <- round(mean(current_data_file[[col_names[i]]]), digits = 3)
    summary_row <-c(summary_row, var_avg)
  }
  
  
  return(summary_row)
}

####Panel 'Self Organize'
##################################

#this creates a list of rownames that have the pattern: "clus#_Xcase#"
create_kmeans_SOM_mapping <- function(){
  if(is.null(current_kmeans_solution)){return()}
  label_merge = c()
  for(i in 1:length(current_kmeans_solution@uclusters)){
    new_label <- paste(current_kmeans_solution@uclusters[i], ";", i, sep="")
    label_merge =c(label_merge, new_label)
  }
  return(label_merge)
  
}

retrieve_ANOVA_results <-function() {
  full_text <- capture.output(summary(current_som_solution))
  start_index <- length(full_text)
  anova_summary <- list()
  for(i in start_index:1){
    if(length(full_text[i]) == ""){
      next
    }
    else if(grepl("Degrees of freedom", full_text[i], fixed = TRUE) == TRUE){
      anova_summary <- c(anova_summary, full_text[i])
      break
    }
    else{
      anova_summary <- c(anova_summary, full_text[i])
    }
  }
  return(anova_summary)
}

#### Panel 'Scenario Simulation'
####################################
setClass("track_agent_tab_state", representation(current_state = "character", terminal_state = "character", possible_states = "character", cluster_tested = "character", 
                                                 checked_data = "data.frame", sensitivity_test = "list", sensitivity_result = "list"))

create_track_agent_tab_state <- function(start, end){
  new_class = new("track_agent_tab_state", current_state = start, terminal_state = end, possible_states = c("first", "second", "third", "fourth", "fifth", "sixth"), 
                  cluster_tested = "None", sensitivity_test = list(), sensitivity_result = list())
  return(new_class)
}

convert_state_to_numeric<- function(track_obj, to_convert_state){
  state = 1
  for(i in 1:length(track_obj@possible_states)){
    if(track_obj@possible_states[i] == to_convert_state) {
      break
    }
    state = state + 1
  }
  return(state)
}

erase_future_states <- function(track_obj, state, val_list){
  state = state + 1
  for(i in state:length(track_obj@possible_states)){
    updateKey(NA, val_list, track_obj@possible_states[i])
  }
  
}


setGeneric(name="update_tracker_current_state", 
           def = function(track_obj, val_list, move){
             
             standardGeneric("update_tracker_current_state")
           })

setMethod(f = "update_tracker_current_state", 
          signature = "track_agent_tab_state",
          definition = function(track_obj, val_list, move){
            state <- convert_state_to_numeric(track_obj, track_obj@current_state)
            terminal <- convert_state_to_numeric(track_obj, track_obj@terminal_state)
            
            if(state > 1 & state < terminal | state == 1 & move == 1 & terminal > 1 | state == terminal & state > 1 & move == -1){
              newstate = state + move
              return(newstate)
            }
            return(state)
          })




updateReactiveValues <- function(track_obj, new_val, val_list, empty_state){
  #note original for loop and if statement for finding empty_states was moved into the  observeEvent run case/cluster function in server.R
  if(empty_state == TRUE){
    for(i in 1:length(track_obj@possible_states))
    {
      if(is.na(reactiveValuesToList(val_list)[track_obj@possible_states][i])){
        updateKey(new_val, val_list, track_obj@possible_states[i])
        break 
      }
    }
    
  }
  else{
    final_state <- length(track_obj@possible_states) -1
    for(i in 2:final_state)
    {
      updateKey(val_list[[track_obj@possible_states[i+1]]], val_list, track_obj@possible_states[i])
    }
    updateKey(new_val, val_list, track_obj@possible_states[6]) #hard coded for now until the size of state array is abstracted out
    
  }
}

updateKey <- function(new_val, val_list, index){
  switch(index, 
         "first" ={
           val_list$first <- new_val
         },
         "second" = {
           val_list$second <- new_val
         },
         "third" = {
           val_list$third <- new_val
         },
         "fourth" = {
           val_list$fourth <- new_val
         },
         "fifth" = {
           val_list$fifth <- new_val
         },
         "sixth" = {
           val_list$sixth <- new_val
         })}
#generates plot template and associated data.frame for plotting to grid and indexes for neurons
#dim_params is a dummy argument for future versions where it will dynamically generate grid sizes
generate_grid_template <-function(dims, knum){
  groupnames <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9")
  if (knum <9) {groupnames <- groupnames[1:knum]}
  agent_drawtools <<- create_SOMdrawtools(current_som_solution$parameters$the.grid$dim, length(current_kmeans_solution@usize))
  

  
  agentdf <<- cbind(as.data.frame(cbind(x = rep(0, knum), y = rep(0,knum))), groupnames = groupnames)
  grid_template <- ggplot(agentdf, aes(x=agentdf$x, y=agentdf$y)) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                                                          panel.background = element_blank(),axis.line = element_line(colour = "white"), 
                                                                          axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x =element_blank(),
                                                                          axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank()) +
    scale_y_continuous(limits=c(0,100)) +scale_x_continuous(limits=c(0,100)) +
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0), size=1) + 
    geom_segment(aes(x = 0, y = 100, xend = 100, yend = 100), size=1) + 
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100), size=1) + 
    geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100), size=1) 
  
  xincr <- 100/dims[1]
  yincr <-100/dims[2]
  
  tick <- 0
  
  for (i in 1:(dims[1]-1))
  {
    tick <- tick + xincr
    grid_template <- grid_template + geom_segment(aes_string(x = tick, y = 0, xend = tick, yend = 100), size=1)
  }
  tick <- 0 
  for (j in 1:(dims[2]-1))
  {
    tick <- tick + yincr
    grid_template <- grid_template + geom_segment(aes_string(x=0, y=tick, xend=100, yend = tick), size=1)
  }
  
  return(grid_template)
}


generate_logic_column <- function(df){
  subset <- 9
  if(nrow(df) < 9){
    subset <- nrow(df)
    }
  logic_col <- rep(FALSE, nrow(df))
  active_cases <- sample(1:nrow(df), subset, replace=F)
  for(i in 1:length(active_cases)){
    logic_col[active_cases][i] <- TRUE
  }
  return(logic_col)
}



generate_cluster_table <- function(){
  clus_label = generate_cluster_labels()
  predicted <- predict(current_som_solution, current_kmeans_solution@ucenters)
  newdf <- cbind(as.data.frame(current_kmeans_solution@ucenters), "Cluster" = clus_label, "Quadrant" = predicted)
  #newdf <- as.data.frame(cbind(round(current_kmeans_solution@ucenters, 3), "Cluster" = clus_label, "Quadrant" = predicted))
  return(newdf)
}


plot_agent_SOM <-function(current_table) {
  
  active_rows =c()
  for(i in 1:nrow(current_table)){
    if(current_table[i, 1] == TRUE) {
      active_rows <- c(active_rows, i)
    }
  }
  temp_active <<- active_rows
  #2:ncol(df)-2 to remove the input col and the cluster and quadrant column
  final_col <- ncol(current_table)-2
  subset_table <- current_table[active_rows,2:final_col]
  predicted_neuron <- predict(current_som_solution, subset_table)
 
  plot_locations <-list() #need to build up plot locations so it reflects the active_rows
  index = 1
  for(i in active_rows){
    plot_locations[[as.character(i)]] <- agent_drawtools@neuron_centers[[as.character(predicted_neuron[index])]]
   
    index = index + 1
  }
  
  index = 1
  for(i in active_rows){
    if(agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]] > 0){
      plot_locations[[as.character(i)]][1] <- plot_locations[[as.character(i)]][1] + agent_drawtools@displace_vector[[as.character(agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]])]][1] 
      plot_locations[[as.character(i)]][2] <- plot_locations[[as.character(i)]][2] + agent_drawtools@displace_vector[[as.character(agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]])]][2] 
      
    }
    #updates the values to be plotted
    agentdf[i,1] <<- plot_locations[[as.character(i)]][1]
    agentdf[i,2] <<- plot_locations[[as.character(i)]][2]
    agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]] <- agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]] +1
    index = index + 1
    
  }
}

setClass("SOMdrawtools", representation(neuron_centers = "list", displace_vector = "list", 
                                        displace_tracker = "list", plot_colors = "character"))

create_SOMdrawtools <- function(dims, knum){
  vectors <- list("1" =c(2,0), "2" =c(0,2), "3" =c(-2,0), "4" =c(0,-2), "5" = c(2,2), "6" =c(-2,2), 
                  "7"=c(2,-2),"8" =c(-2,-2))
  centers_displace <- generate_neuron_centers(dims)
  centers <- centers_displace$neurons
  tracker <- centers_displace$displace
  colors <- grid_color_subset(knum)
  new_class <- new("SOMdrawtools", neuron_centers = centers, displace_vector = vectors, 
                   displace_tracker = tracker, plot_colors = colors)
}

generate_neuron_centers <- function(dims){
  xincr <- 100/dims[1]
  yincr <- 100/dims[2]
  neuron_centers <- list()
  
  entry = 1
  tickx = 0
  for(i in 1:dims[1]){
    ticky <- 0
    tempx <- tickx+ xincr/2 
    for(j in 1:dims[2]){
      tempy <- ticky + yincr/2
      neuron_centers[[as.character(entry)]] <- c(tempx, tempy)
      ticky <- ticky + yincr
      entry <- entry + 1
      
    }
    tickx <- tickx + xincr 
  }
  displacement_tracker <- list()
  for(i in 1:(dims[1]*dims[2])){
    displacement_tracker[[as.character(i)]] <- 0
  }
  return_list <- list("neurons" = neuron_centers, "displace" = displacement_tracker)
  return(return_list)
  
}

grid_color_subset <- function(knum) {
  colors <- c("darkblue", "purple1", "red", "green4", "deeppink1", "indianred4", "seagreen3", "gray55", "goldenrod1", "burlywood4", "darkorange2", "darkorchid4")
  subset <- sample(1:9, knum)
  case_colors =c()
  for(i in subset){
    case_colors <- c(case_colors, colors[i])
  }
  return(case_colors)
}

###MONTE CARLO FUNCTIONS and CLASS###



snip_state <- function(state, select_clus){
  state <- state[names(current_data_file)]
  row <- substr(select_clus, nchar(select_clus)-1, nchar(select_clus))
  state <- state[as.integer(row), ]
  for(i in 1:length(state)){
    state[i] = round(state[i], 2)
  }
  
  return(state)
}
detect_change_direction <- function(baseline, change_state){
  if(baseline > change_state){sign = "negative"}
  else{sign = "positive"}
  return(sign)
}

zero_order_change <- function(baseline, change_state){
  if(baseline == 0){
    pdiff = change_state * 100
  }
  else{pdiff = baseline * 100}
  return(pdiff)
}

evaluate_state_change <- function(state_vals, select_clus) {
 
  baseline <- state_vals[['first']]
  change_state <- state_vals[[agent_cluster_tracker@current_state]]
  
  baseline <- snip_state(baseline, select_clus)
  change_state <- snip_state(change_state, select_clus)
  
  pdiff_vector <- rep(0, length(baseline))
  for(i in 1:length(baseline)){
    if(baseline[i] != change_state[i]){
      sign = detect_change_direction(baseline[i], change_state[i])
      if(baseline[i] == 0 | change_state[i] == 0){
        pdiff = zero_order_change(baseline[i], change_state[i])
      }
      else{
        diff = abs(baseline[i] - change_state[i])
        pdiff = diff/baseline[i]
        if(sign == "negative"){pdiff =-pdiff}
      }
      pdiff_vector[i] = round(pdiff *100, digits=2)
    }
    
  }
  change = FALSE 
  for(i in 1:length(pdiff_vector)){
    if(pdiff_vector[i] != 0){
      change = TRUE
    }
  }
  if(change == TRUE){return(pdiff_vector)}
  else{return("There were no changes to the target cluster for sensitivity analysis")}
  
}
dataModal <- function(names, change, failed = FALSE) {
  modalDialog(
    p("Select a range in which each projected change may deviate"),
    
    lapply(1:length(names), function(y, n, i) 
      { sliderInput(paste0("pont.dev", i), paste0(n[i], " : Potential Deviation", " (User Change: ", y[i], "%)"), min =0, max=100, value=0, width="400px") }, y=change, n=names),
    
    
    
    if (failed)
      div(tags$b("Invalid name of data object", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("sa_ok", "OK")
    )
  )
}

waitModal <- function(cluster, vars_tested) {
  modalDialog(
    p("Sensitivity Analaysis currently running."),
    p("This may take several minutes depending on how many attributes were tested."),
    paste("You tested changing ", length(vars_tested), " attributes on ", cluster)
   
  )
}


genmc_state_space <- function(n, default_val){ 
  state_space = list()
  for(i in 1:n){
    state_space[[i]] <- default_val
  }
  return(state_space)
} 

#### Panel 'Predict/Forecast'
####################################

check_predict_header <- function(train_names, trial_names){
  match = TRUE
  for(i in 1:length(train_names)){
    if(any(trial_names == train_names[i])== FALSE){
      match = FALSE
      break
    }
  }
  return(match)
}

###############Still Under Development for Future Versions###############

# Systems Mapping Tab #
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
