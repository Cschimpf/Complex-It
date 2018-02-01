####Global Variables#####


# Any code in this global.r file will run one time prior to launching
# the Shiny App.  The code will not continually scan in the background
# while the App runs, so a good place for intialization of variables using
# code

#Data Variables
####
current_data_file = NULL
full_data = NULL
current_kmeans_solution = NULL
current_som_solution = NULL
previous_som = NULL

agent_case_tracker = NULL
agent_cluster_tracker = NULL
agent_grid_plot = NULL
agentdf = NULL
agent_grid_slots = NULL
displacement = list("1" =c(0, 2), "2" =c(0,-2), "3" =c(2,0), "4" =c(-2,0), "5" =c(-5,0), "6" =c(0,5))


####
#numeric_only_columns = NULL
#other_columns = NULL
all.somplot.types <- list("numeric"=
                            list("prototypes"=
                                   list("3d", "barplot",
                                        "smooth distances"="smooth.dist",
                                        "U matrix distances"="umatrix"),
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
                                            
                    

setClass("user_saved_kmeans_res", representation(save_name = "character", ucentroids = "list", uclusters = "list", usize = "integer"))

create_user_saved_kmeans_res <- function(objectname, ucenters, cluster_labels, k_size){
  new_labels <- list_cluster_labels(cluster_labels)
  ucenters <- list(ucenters)
  new_class = new("user_saved_kmeans_res", save_name = objectname, ucentroids = ucenters, uclusters = new_labels, usize = k_size)
  return(new_class)
}

list_cluster_labels <-function(unlist){
  new_list = list()
  for(i in 1:length(unlist)){
    new_list[[i]] = as.numeric(unlist[i])
  }
  return(new_list)
}

setGeneric(name="append_cluster_labels", 
           def = function(cluster_obj, data_obj){
             
             standardGeneric("append_cluster_labels")
           })

setMethod(f = "append_cluster_labels", 
          signature = "user_saved_kmeans_res",
          definition = function(cluster_obj, data_obj){
            
            clus_labels =c()
            for(i in 1: length(cluster_obj@uclusters)){
              newnum <- as.numeric(cluster_obj@uclusters[i])
              clus_labels = c(clus_labels, newnum)
            }
            appended_labels <- cbind(data_obj, clus_labels)
            return(appended_labels)
          })


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


####Panel 'Import Data'
##################################

column_type_identifier <-function(data){
  numeric_only_columns <-sapply(data, class) %in% c("integer", "numeric")
  return(numeric_only_columns)
  #other_columns <<- logic_vector_inverter(numeric_only_columns)
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

#for now I left your code here so there is a trace of what changes were made

pseudoF = function(data,sol, k){
  #nk = length(k)
  #n = nrow(X)
  T = sum(scale(data, scale=F)^2)
  #W = rep(T, nk)
  #for (i in 1:nk){
  #cli = kmeans(X, k[i], nstart=ns,algorithm="Lloyd")
  #W[i] = sum(cli$withinss)
  #}
  W <- sum(sol@uwithinss) #this is using the withinss from 'k' above
  pF = ((T-W)/(k-1))/(W/(nrow(data)-k))
  return(pF)
}

plot_silhouette <- function(data, km){
  dissM <- daisy(data)
  sil_plot <- plot(silhouette(km@uclusters, dissM)) 
  return(sil_plot)
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

#### Panel 'Agent-Model'
####################################
setClass("track_agent_tab_state", representation(current_state = "character", terminal_state = "character", possible_states = "character"))

create_track_agent_tab_state <- function(start, end){
  new_class = new("track_agent_tab_state", current_state = start, terminal_state = end, possible_states = c("first", "second", "third", "fourth", "fifth", "sixth"))
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
generate_grid_template <-function(dim_params){

  agentdf <<- as.data.frame(cbind(x1=0, y1=0, x2=0, y2=0, x3=0, y3=0, x4=0, y4=0, x5=0, y5=0, x6=0, y6=0))
  grid_template <- ggplot(agentdf) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
                                panel.background = element_blank(),axis.line = element_line(colour = "white"), 
                                axis.title.x=element_blank(), axis.ticks.x=element_blank(), axis.text.x =element_blank(),
                                axis.ticks.y=element_blank(), axis.text.y=element_blank(), axis.title.y=element_blank()) +
    scale_y_continuous(limits=c(0,100)) +scale_x_continuous(limits=c(0,100)) +
    geom_segment(aes(x = 0, y = 0, xend = 100, yend = 0), size=1) + 
    geom_segment(aes(x = 0, y = 100, xend = 100, yend = 100), size=1) + 
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 100), size=1) + 
    geom_segment(aes(x = 100, y = 0, xend = 100, yend = 100), size=1) +
    geom_segment(aes(x = 20, y = 0, xend = 20, yend = 100), size=1) + 
    geom_segment(aes(x = 40, y = 0, xend = 40, yend = 100), size=1)  + 
    geom_segment(aes(x = 60, y = 0, xend = 60, yend = 100), size=1) + 
    geom_segment(aes(x = 80, y = 0, xend = 80, yend = 100), size=1) + 
    geom_segment(aes(x = 0, y = 20, xend = 100, yend = 20), size=1) + 
    geom_segment(aes(x = 0, y = 40, xend = 100, yend = 40), size=1) + 
    geom_segment(aes(x = 0, y = 60, xend = 100, yend = 60), size=1) + 
    geom_segment(aes(x = 0, y = 80, xend = 100, yend = 80), size=1)
  
  agent_grid_slots <<- list("1" = geom_point(aes(x=agentdf$x1, y=agentdf$y1), size=3, colour="red"),
                           "2" = geom_point(aes(x=agentdf$x2, y=agentdf$y2), size=3, colour="orange"),
                           "3" = geom_point(aes(x=agentdf$x3, y=agentdf$y3), size=3, colour="pink"),
                           "4" = geom_point(aes(x=agentdf$x4, y=agentdf$y4), size=3, colour="green"),
                           "5" = geom_point(aes(x=agentdf$x5, y=agentdf$y5), size=3, colour="blue"),
                           "6" =geom_point(aes(x=agentdf$x6, y=agentdf$y6), size=3, colour="purple"))
  agent_grid_slots_index <<- list("1" =c(10,10), "2" =c(10,30), "3" =c(10,50), "4" =c(10,70), "5" =c(10,90),
                                  "6" =c(30,10), "7" =c(30,30), "8" =c(30,50), "9" =c(30,70), "10" =c(30,90),
                                  "11" =c(50,10), "12" =c(50,30), "13" =c(50,50), "14" =c(50,70), "15" =c(50,90),
                                  "16" =c(70,10), "17" =c(70,30), "18" =c(70,50), "19" =c(70,70), "20" =c(70,90),
                                  "21" =c(90,10), "22" =c(90,30), "23" =c(90,50), "24" =c(90,70), "25" =c(90,90))

  
  return(grid_template)
}

generate_logic_column <- function(df){
  #will need to add a check for how many rows in df
  #and adjust below accordingly 
  logic_col <- rep(FALSE, nrow(df))
  active_cases <- sample(1:nrow(df), 6, replace=F)
  for(i in 1:length(active_cases)){
    logic_col[active_cases][i] <- TRUE
  }
  return(logic_col)
}

plot_agent_SOM <-function(current_table) {
  active_rows =c()
  for(i in 1:nrow(current_table)){
    if(current_table[i, 1] == TRUE) {
      active_rows =c(active_rows, i)
    }
  }
  subset_table <- current_table[active_rows,2:ncol(current_table)]
  predicted_neuron <- predict(current_som_solution, subset_table)
  print(predicted_neuron)
  
  duplicate_neuron <-c(0,0,0,0,0,0)
  current_neurons <-c()
  plot_locations <-list()
  for(i in 1:length(predicted_neuron)){
    plot_locations[[as.character(i)]] <- agent_grid_slots_index[[as.character(predicted_neuron[i])]]
    if(as.integer(predicted_neuron[i] %in% current_neurons)){
      duplicate_neuron[i] <- 1
    }
    current_neurons <-c(current_neurons, as.integer(predicted_neuron[i]))
    
  }
  x = 1
  y = 2
  displace = 1
  for(i in 1:length(plot_locations)){
    if(duplicate_neuron[i] == 1){
      plot_locations[[as.character(i)]][1] <- plot_locations[[as.character(i)]][1] + displacement[[as.character(displace)]][1] 
      plot_locations[[as.character(i)]][2] <- plot_locations[[as.character(i)]][2] + displacement[[as.character(displace)]][2] 
      displace = displace + 1
    }
    agentdf[x] <<- plot_locations[[as.character(i)]][1]
    agentdf[y] <<- plot_locations[[as.character(i)]][2]
    x = x + 2
    y = y + 2
    
  }
}

###############Still Under Development for Future Versions###############

list_cluster_centers <-function(unlist, k_number) {
  
}
#in order to this right it will need to maybe have its own class
#that starts with the name of the var clustered and then includes
#the centers below it. Right now can't retrieve var names and
#wonder if its useful to put the centroids into a list with no var name
#references 
create_centers_indexes <-function(unlist, k_number){
  for (i in 1:(length(unlist)/k_number)){
    print(5)
  }
}

convert_list_to_vector <-function(a_list){
  new_vec =c()
  for(i in 1:length(current_kmeans_solution@uclusters)){
    new_vec = c(new_vec, as.integer(current_kmeans_solution@uclusters[i]))
  }
  return(new_vec)
}