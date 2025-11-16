column_type_identifier <-function(data){
  numeric_only_columns <-sapply(data, class) %in% c("integer", "numeric")
  return(numeric_only_columns)
  
}


#used by multiple 'tabs' for generating clusters
generate_cluster_labels <-function(current_kmeans_solution){
  clus_label =c()
  for(i in 1: nrow(current_kmeans_solution@ucenters)){
    clus_label = c(clus_label, paste(c("Cluster"), toString(i), sep = " "))
  }
  return(clus_label)
}


setClass("user_gen_kmeans_solution", representation(save_name = "character", uclusters = "integer", ucenters = "matrix",
                                                    utotss = "numeric", uwithinss = "numeric", utotwithinss = "numeric",
                                                    ubetweenss = "numeric", usize = "integer"))

create_user_gen_kmeans_solution <- function(objectname, km){
  new_class = new("user_gen_kmeans_solution", save_name = objectname, uclusters = km$cluster, ucenters = km$centers, utotss = km$totss,
                  uwithinss = km$withinss, utotwithinss = km$tot.withinss, ubetweenss = km$betweenss, usize = km$size)
  return(new_class)
}



generate_data_summary <- function(current_data_file) {
  summary_row <-c("Total Size & Var Avg")
  summary_row <-c(summary_row, nrow(current_data_file))
  col_names <- names(current_data_file)
  for(i in 1:ncol(current_data_file)){
    var_avg <- round(mean(current_data_file[[col_names[i]]]), digits = 3)
    summary_row <-c(summary_row, var_avg)
  }
  
  
  return(summary_row)
}


pseudoF = function(data,sol, k){
  T = sum(scale(data, scale=F)^2)
  W <- sum(sol@uwithinss) #this is using the withinss from 'k' above
  pF = ((T-W)/(k-1))/(W/(nrow(data)-k))
  return(pF)
}



#uses the number of data rows to calculate a dimension for the graph, width/height 
#will return 400 as a default height if the scaled nrows is less than 400
graph_dimension = function(data, scale = 5){
  dimension = (nrow(data) * scale)
  if (dimension < 400) 
  {dimension = 400}
  return(dimension)
}


plot_silhouette <- function(data, km){
  dissM <- daisy(data)
  sil_plot <- plot(silhouette(km@uclusters, dissM)) 
  return(sil_plot)
}


create_kmeans_SOM_mapping <- function(current_kmeans_solution){
  if(is.null(current_kmeans_solution)){return()}
  label_merge = c()
  for(i in 1:length(current_kmeans_solution@uclusters)){
    new_label <- paste(current_kmeans_solution@uclusters[i], ";", i, sep="")
    label_merge =c(label_merge, new_label)
  }
  return(label_merge)
  
}



retrieve_ANOVA_results <-function(current_som_solution) {
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


all.somplot.types <- list("numeric"=
                            list("prototypes"=
                                   list("3d", "barplot",
                                        "smooth.dist",
                                        "umatrix"),
                                 "obs"=c("color", "barplot", 
                                         "names", "boxplot")))


flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


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



create_track_agent_tab_state <- function(start, end){
  new_class = new("track_agent_tab_state", current_state = start, terminal_state = end, possible_states = c("first", "second", "third", "fourth", "fifth", "sixth"), 
                  cluster_tested = "None", sensitivity_test = list(), sensitivity_result = list())
  return(new_class)
}


setClass("track_agent_tab_state", representation(current_state = "character", terminal_state = "character", possible_states = "character", cluster_tested = "character", 
                                                 checked_data = "data.frame", sensitivity_test = "list", sensitivity_result = "list"))



erase_future_states <- function(track_obj, state, val_list){
  state = state + 1
  for(i in state:length(track_obj@possible_states)){
    updateKey(NA, val_list, track_obj@possible_states[i])
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



generate_cluster_table <- function(current_som_solution, current_kmeans_solution){
  clus_label = generate_cluster_labels(current_kmeans_solution)
  predicted <- predict(current_som_solution, current_kmeans_solution@ucenters)
  newdf <- cbind(as.data.frame(current_kmeans_solution@ucenters), "Cluster" = clus_label, "Quadrant" = predicted)
  #newdf <- as.data.frame(cbind(round(current_kmeans_solution@ucenters, 3), "Cluster" = clus_label, "Quadrant" = predicted))
  return(newdf)
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




generate_grid_template <-function(dims, knum, current_som_solution, current_kmeans_solution){
  groupnames <- c("Cluster 1", "Cluster 2", "Cluster 3", "Cluster 4", "Cluster 5", "Cluster 6", "Cluster 7", "Cluster 8", "Cluster 9")
  if (knum <9) {groupnames <- groupnames[1:knum]}
  agent_drawtools <- create_SOMdrawtools(current_som_solution$parameters$the.grid$dim, length(current_kmeans_solution@usize))
  
  
  
  agentdf <- cbind(as.data.frame(cbind(x = rep(0, knum), y = rep(0,knum))), groupnames = groupnames)
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
  
  return(list(grid_template, agent_drawtools, agentdf))
}




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


setClass("SOMdrawtools", representation(neuron_centers = "list", displace_vector = "list", 
                                        displace_tracker = "list", plot_colors = "character"))




convert_state_to_numeric <- function(track_obj, to_convert_state){
  state = 1
  for(i in 1:length(track_obj@possible_states)){
    if(track_obj@possible_states[i] == to_convert_state) {
      break
    }
    state = state + 1
  }
  return(state)
}



plot_agent_SOM <-function(current_table, current_som_solution, agent_drawtools, agentdf) {
  
  active_rows =c()
  for(i in 1:nrow(current_table)){
    if(current_table[i, 1] == TRUE) {
      active_rows <- c(active_rows, i)
    }
  }
  temp_active <- active_rows
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
    agentdf[i,1] <- plot_locations[[as.character(i)]][1]
    agentdf[i,2] <- plot_locations[[as.character(i)]][2]
    agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]] <- agent_drawtools@displace_tracker[[as.character(predicted_neuron[index])]] +1
    index = index + 1
    
  }
  
  return(list(agentdf, agent_drawtools))# 
}

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



evaluate_state_change <- function(state_vals, select_clus, agent_cluster_tracker, current_data_file) {
  
  baseline <- state_vals[['first']]
  change_state <- state_vals[[agent_cluster_tracker@current_state]]
  
  baseline <- snip_state(baseline, select_clus, current_data_file)
  change_state <- snip_state(change_state, select_clus, current_data_file)
  
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




snip_state <- function(state, select_clus, current_data_file){
  state <- state[names(current_data_file)]
  row <- substr(select_clus, nchar(select_clus)-1, nchar(select_clus))
  state <- state[as.integer(row), ]
  for(i in 1:length(state)){
    state[i] = round(state[i], 2)
  }
  
  return(state)
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


detect_change_direction <- function(baseline, change_state){
  if(baseline > change_state){sign = "negative"}
  else{sign = "positive"}
  return(sign)
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





create_nice_ggplot <- function(superclusters_object, current_som_solution_object){
  
  x_dim <- current_som_solution_object[["parameters"]][["the.grid"]][["dim"]][[1]]
  y_dim <- current_som_solution_object[["parameters"]][["the.grid"]][["dim"]][[2]]
  
  # write the text to build the base plot
  p <- 'ggplot() +
  geom_blank() +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    axis.text = element_blank()
  ) +
  coord_fixed()'
  
  # Draw the outer grid
  p <- paste(p,
             paste0('+ scale_x_continuous(breaks = 1:',
                    x_dim,
                    ', expand = c(0, 0))',
                    '+ scale_y_continuous(breaks = 1:',
                    y_dim,
                    ', expand = c(0, 0))'))
  
  
  # Add colours for super cluster plots
  
  dims_coords <- expand.grid(1:y_dim, 1:x_dim) %>%
    rename(y_coords = 1, x_coords = 2)
  
  dims_coords$super_cluster <- superclusters_object[["cluster"]]
  
  colour_palette <- data.frame(super_cluster = c(1:10),
                               colours = c('red', 'blue', 'green',
                                           'purple', 'orange', 'pink',
                                           'cyan', 'brown', 'yellow',
                                           'grey'))
  
  dims_coords <- left_join(dims_coords, colour_palette)
  
  dims_coords$quadrant <- 1:nrow(dims_coords)
  
  for(i in 1:nrow(dims_coords)){
    
    #in each iteration, add one annotate line 
    #with info from the annotations dataframe
    
    xmax <- dims_coords[i, 2]
    xmin <- xmax-1
    
    ymax <- dims_coords[i, 1]
    ymin <- ymax-1
    
    colour <- dims_coords[i, 4]
    
    p <- paste(p,
               paste0('+ annotate("rect", xmin = ',
                      xmin,
                      ', xmax = ',
                      xmax,
                      ', ymin = ',
                      ymin,
                      ', ymax = ',
                      ymax,
                      ', alpha = 0.5, fill = "', colour,
                      '")'))
    
  }
  
  # Add vline and hline for separations
  p <- paste(p,
             paste0('+ geom_vline(xintercept = 0:',
                    x_dim,
                    ')',
                    '+ geom_hline(yintercept = 0:',
                    y_dim,
                    ')'))
  
  
  
  # Add quadrant titles
  for(i in 1:nrow(dims_coords)){
    
    xmax <- dims_coords[i, 2]
    xmin <- xmax-1
    
    ymax <- dims_coords[i, 1]
    ymin <- ymax-0.25
    
    quadrant_text <- paste0("'", paste0('Quadrant ', dims_coords[i, 5]), "'")
    
    p <- paste(p,
               paste0("+geom_fit_text(aes(label=", quadrant_text, ",
                       xmin=", xmin, ",
                       xmax=", xmax, ",
                       ymin=", ymin, ",
                       ymax=", ymax, "),
                   reflow=F, grow = T)"
               )
    )
    
  }
  
  
  
  
  
  
  
  
  # Add vline and hline for separations
  
  
  k_means_data <- data.frame(
    k_means_cluster = names(current_som_solution_object[["clustering"]]),
    quadrant = as.vector(current_som_solution_object[["clustering"]]),
    stringsAsFactors = FALSE # Prevent conversion of strings to factors
  )
  
  k_means_data$k_means_cluster <- sub(";.*", "", k_means_data$k_means_cluster)
  
  k_means_data <- k_means_data %>%
    distinct() %>%
    group_by(quadrant) %>%
    summarise(k_means_cluster = paste(k_means_cluster, collapse = " "))
  
  # Add k-means clusters to grid
  for(indiv_quadrant in unique(k_means_data$quadrant)){
    
    
    indiv_kmeans_data <- k_means_data %>%
      filter(quadrant == indiv_quadrant) %>%
      select(k_means_cluster) %>%
      as.character() %>%
      strsplit(',') %>%
      unlist()
    
    indiv_kmeans_data <- paste0("'", indiv_kmeans_data, "'")
    
    xmax <- dims_coords %>%
      filter(quadrant == indiv_quadrant) %>%
      select(x_coords) %>%
      as.numeric() 
    
    xmin = xmax-1
    
    ymax <- dims_coords %>%
      filter(quadrant == indiv_quadrant) %>%
      select(y_coords) %>%
      as.numeric() %>%
      -0.25
    
    ymin <- ymax-0.75
    
    
    p <- paste(p,
               paste0("+geom_fit_text(aes(label=", indiv_kmeans_data, ",
                      xmin=", xmin, ",
                      xmax=", xmax, ",
                      ymin=", ymin, ",
                      ymax=", ymax, "),
                  reflow=T, grow = F)"
               )
    )
    
  }
  
  
  return(eval(parse( text=p )))
  
  
}  

remove_xaxis_labels <- function(p) {
  
  x_axis_names <- names(p$x$layout)[grepl("^xaxis", names(p$x$layout))]
  
  code_to_append <- NULL
  
  for(xaxis in x_axis_names){
    
    code_to_append <- paste0(code_to_append,
                             '%>% layout(', xaxis, '= list(showticklabels = FALSE, ticks=""),
    margin = list(b = 20))'
    )
    
  }
  
  eval(parse(text = paste0('p ', code_to_append)))
  
}

##### DEFINING TEXT IN THE APP #####

## CLUSTERING ##
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

## SOM AI ##
train_SOM_text <- "<div style='line-height: 30px'><u><b>Advice For Using the SOM AI</b></u>
                 For those new to the SOM, we recommend using all of the defaults. However, if you have over 25 clusters, you will need to make a bigger map, such as a 6x6 grid.
                 For experienced users, the SOM algorithm we use comes from the <a href='https://cran.r-project.org/web/packages/SOMbrero/index.html'>SOMbrero R-Package</a>

                 <u><b>Need Help?</b></u>
                 For tutorials on using the SOM AI in COMPLEX-IT <a href='https://www.art-sciencefactory.com/tutorials.html'>click here</a>
                 For a basic introduction to the Self-Organizing Map (SOM) <a href='https://en.wikipedia.org/wiki/Self-organizing_map'>click here</a></div>"



train_SOM_text <- gsub("\n", "<br>", train_SOM_text) # Convert newline characters to HTML line breaks

## COMPARE AND VISUALISE ##
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

## CASE PREDICTION ##
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

## SCENARIO SIMULATION ##
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

## SYSTEMS MAPPING ##
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

## MAKING THE DIFFERENT K MEANS CHARTS ##
create_jitter_plot <- function(data_to_graph, min_silh_width, cluster){
  
  jitter_plot <- ggplot(data_to_graph, aes(x = 0, y = sil_width)) +
    geom_jitter(height = 0, width = 0.01) + # Adjust 'height' for vertical spread
    scale_x_continuous(breaks = NULL, labels = NULL) +
    labs(x = NULL) +
    expand_limits(y=c(min_silh_width, 1)) +
    geom_hline(yintercept = 0, linetype="dotted") +
    ylab("Silhouette width")+
    theme_minimal()+
    ggtitle(paste0("Cluster: ", cluster),
            subtitle = paste0("Average silhouette width: ", round(mean(data_to_graph$sil_width), 2), "\n",
                              "n = ", paste(nrow(data_to_graph))))+
    theme_minimal()+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(jitter_plot)
  
}

create_violin_plot <- function(data_to_graph, min_silh_width, cluster){
  
  violin_plot <- ggplot(data_to_graph, aes(x = "", y=sil_width)) +
    geom_violin(color = "black", fill="steelblue") +
    expand_limits(y=c(min_silh_width, 1)) +
    geom_hline(yintercept = 0, linetype="dotted") +
    ylab("Silhouette width")+
    theme_minimal()+
    ggtitle(paste0("Cluster: ", cluster),
            subtitle = paste0("Average silhouette width: ", round(mean(data_to_graph$sil_width), 2), "\n",
                              "n = ", paste(nrow(data_to_graph))))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    labs(x = NULL)
  
  return(violin_plot)
  
}

create_histogram_plot <- function(data_to_graph, min_silh_width, max_count_perc, cluster){
  
  histogram_plot <- ggplot(data_to_graph, aes(x=sil_width, y = after_stat(count)/nrow(data_to_graph))) + 
    geom_histogram(fill = "steelblue", binwidth = 0.01) + 
    expand_limits(x=c(min_silh_width, 1), y = c(0, max_count_perc)) +
    geom_vline(xintercept = 0, linetype="dotted") +
    ylab("Percentage of cases")+
    xlab("Silhouette width")+
    theme_minimal()+
    ggtitle(paste0("Cluster: ", cluster),
            subtitle = paste0("Average silhouette width: ", round(mean(data_to_graph$sil_width), 2), "\n",
                              "n = ", paste(nrow(data_to_graph))))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))+
    scale_y_continuous(labels = scales::percent)
  
  return(histogram_plot)
  
}

create_silhouette_plot <- function(data_to_graph, min_silh_width, cluster){
  
  silhouette_plot <- ggplot(data_to_graph, aes(x = nrow, y = sil_width)) +
    geom_bar(stat = "identity", fill = "steelblue")  + # This makes it a horizontal bar chart
    labs(x = "Individual Cases", y = "Silhouette width") +
    scale_x_continuous(breaks = NULL, labels = NULL)+
    expand_limits(y=c(min_silh_width, 1)) +
    theme_minimal()+
    coord_flip()+
    ggtitle(paste0("Cluster: ", cluster),
            subtitle = paste0("Average silhouette width: ", round(mean(data_to_graph$sil_width), 2), "\n",
                              "n = ", paste(nrow(data_to_graph))))+
    theme(plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  return(silhouette_plot)
  
}








