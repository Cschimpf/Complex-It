library(shiny)
library(shinyBS)
library(shinythemes)
library(rhandsontable)
suppressMessages(library(SOMbrero))
library(cluster)
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
library(shinyjs)
library(visNetwork)
library(shinyalert)
library(htmltools)
library(crayon)
library(shinydashboard)
library(zip)
library(rintrojs)
library(fresh)
library(DT)
library(plotly)
library(shinycssloaders)
library(ggfittext)
library(shinyWidgets)


server <- function(input, output, session) {
  
  # lengthChange = FALSE
  # options(DT.options = list(dom = 'ftpi'))
  
  output$complexit_logo <- renderImage({list(src="Complexit_LOGO3.png")}, deleteFile = FALSE)
  
  ########################### PANEL 'IMPORT DATA' ############################
  ############################################################################
  
  pop_ups <- reactiveValues(pop_up_intro=FALSE)
  
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
  
  uploaded_data_values <- reactiveValues(display_data=NULL,
                                         current_data_file=NULL,
                                         the_table=NULL)
  
  # Reactive value to track the current page
  current_page <- reactiveVal(1)
  
  # Reactive value to store the selected format
  selected_format <- reactiveVal(NULL)
  
  # When "Show Modal" button is clicked, open modal and reset page to 1
  observeEvent(input$show_modal, {
    current_page(1) # Reset page to 1 every time the modal is opened
    selected_format(NULL) # Clear the selected format
    showModal(
      modalDialog(
        easyClose = TRUE,
        footer = tagList(
          actionButton("start_over", "Start Over"), # Start Over button
          modalButton("Cancel") # Close modal button
        ),
        uiOutput("modal_content") # Dynamic content
      )
    )
  })
  
  # Render dynamic content for the modal based on the current page
  output$modal_content <- renderUI({
    if (current_page() == 1) {
      # First page with three buttons
      tagList(
        h3("Choose a format to upload"),
        actionButton("btn_csv", "CSV"),
        actionButton("btn_excel", "Excel"),
        actionButton("btn_spss", "SPSS"),
        actionButton("btn_stata", "Stata"),
        actionButton("btn_sas", "SAS")
      )
    } else if (current_page() == 2 && selected_format() == 'CSV') {
      # Second page with dynamic message based on the selected format
      tagList(
        
        fileInput('file1', 'Choose CSV File', buttonLabel='Browse',accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv")
        ),
        
        checkboxInput('header', ' Header?', TRUE),
        
        selectInput('sep', 'Separator:',
                    c("Comma","Semicolon","Tab","Space"), 'Comma'),
        
        actionButton("upload_CSV_file", "Upload CSV")
      )
    } else if (current_page() == 2 && selected_format() == 'Excel') {
      # Second page with dynamic message based on the selected format
      tagList(
        
        fileInput('file1', 'Choose Excel File', buttonLabel='Browse',accept = c(
          ".xlsx",
          ".xls")
        ),
        
        checkboxInput('header', ' Header?', TRUE),
        
        numericInput("number_of_excel_sheet",
                     'What sheet is your data on?',
                     1, 
                     min = 1,
                     max = 255,
                     step = 1),
        
        actionButton("upload_Excel_file", "Upload Excel")
      )
    } else if (current_page() == 2 && selected_format() == 'SPSS') {
      # Second page with dynamic message based on the selected format
      tagList(
        
        fileInput('file1', 'Choose SPSS (.sav) File', buttonLabel='Browse',accept = c(
          ".sav")
        ),
        
        actionButton("upload_SPSS_file", "Upload SPSS")
      )
    } else if (current_page() == 2 && selected_format() == 'Stata') {
      # Second page with dynamic message based on the selected format
      tagList(
        
        fileInput('file1', 'Choose Stata (.dta) File', buttonLabel='Browse',accept = c(
          ".dta")
        ),
        
        actionButton("upload_Stata_file", "Upload Stata")
      )
    } else if (current_page() == 2 && selected_format() == 'SAS') {
      # Second page with dynamic message based on the selected format
      tagList(
        
        fileInput('file1', 'Choose SAS (.sas7bdat) File', buttonLabel='Browse',accept = c(
          ".sas7bdat")
        ),
        
        actionButton("upload_SAS_file", "Upload SAS")
      )
    }
  })
  
  # Advance to the second page and store the selected format
  observeEvent(input$btn_csv, {
    selected_format("CSV")
    current_page(2)
  })
  observeEvent(input$btn_excel, {
    selected_format("Excel")
    current_page(2)
  })
  observeEvent(input$btn_spss, {
    selected_format("SPSS")
    current_page(2)
  })
  observeEvent(input$btn_stata, {
    selected_format("Stata")
    current_page(2)
  })
  observeEvent(input$btn_sas, {
    selected_format("SAS")
    current_page(2)
  })
  
  # Observe "Start Over" button and reset page to 1
  observeEvent(input$start_over, {
    current_page(1) # Reset to page 1
    selected_format(NULL) # Clear the selected format
  })
  
  observeEvent(input$upload_CSV_file, {
    
    the.sep <- switch(input$sep, "Comma" = ",", "Semicolon" = ";", "Tab" = "\t", "Space" = "")
    
    # Attempt to read the file with the given sheet number
    tryCatch({
      the.table <- na.omit(read.csv(input$file1$datapath, header = input$header, sep = the.sep))
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
      uploaded_data_values$the_table <- the.table
      
    }, error = function(e) {
      
      shinyalert(
        title = "Error!",
        text = 'Something has gone wrong with your CSV upload. If unsure, try "start over" or consult the tutorial on getting your data into CSV format.',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
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
      
    })
    
  })
  
  observeEvent(input$upload_Excel_file, {
    
    # Attempt to read the file with the given sheet number
    tryCatch({
      the.table <- na.omit(readxl::read_excel(input$file1$datapath, 
                                              col_names = input$header,
                                              sheet = input$number_of_excel_sheet))
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
      uploaded_data_values$the_table <- the.table
      
    }, error = function(e) {
      
      shinyalert(
        title = "Error!",
        text = 'Something has gone wrong with your Excel upload. Did you put in an invalid sheet number? If unsure, try "start over" or upload your data in .CSV format.',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
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
      
    })
    
  })
  
  observeEvent(input$upload_SPSS_file, {
    
    # Attempt to read the file with the given sheet number
    tryCatch({
      the.table <- na.omit(haven::read_sav(input$file1$datapath))
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
      uploaded_data_values$the_table <- the.table
      
    }, error = function(e) {
      
      shinyalert(
        title = "Error!",
        text = 'Something has gone wrong with your SPSS upload. If unsure, try "start over" or upload your data in .CSV format.',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
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
      
    })
    
  })
  
  observeEvent(input$upload_Stata_file, {
    
    # Attempt to read the file with the given sheet number
    tryCatch({
      the.table <- na.omit(haven::read_dta(input$file1$datapath))
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
      uploaded_data_values$the_table <- the.table
      
    }, error = function(e) {
      
      shinyalert(
        title = "Error!",
        text = 'Something has gone wrong with your Stata upload. If unsure, try "start over" or upload your data in .CSV format.',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
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
      
    })
    
  })
  
  observeEvent(input$upload_SAS_file, {
    
    # Attempt to read the file with the given sheet number
    tryCatch({
      the.table <- na.omit(haven::read_sas(input$file1$datapath))
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
      uploaded_data_values$the_table <- the.table
      
    }, error = function(e) {
      
      shinyalert(
        title = "Error!",
        text = 'Something has gone wrong with your SAS upload. If unsure, try "start over" or upload your data in .CSV format.',
        size = "s",
        closeOnEsc = TRUE,
        closeOnClickOutside = TRUE,
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
      
    })
    
  })
  
  # Observe the reactive value and close the modal if it's not NULL
  observe({
    if (!is.null(uploaded_data_values$the_table)) {
      removeModal()
    }
  })
  
  observe({
    
    # Retrieve the.table from reactiveValues
    the.table <- uploaded_data_values$the_table
    if (is.null(the.table)) return(NULL)
    
    output$varchoice <- renderUI({div(
      pickerInput(
        inputId = "varchoice",
        label = "Input variables:",
        multiple = TRUE,
        choices = as.list(colnames(the.table)[sapply(the.table, class) %in% c("integer", "numeric")]),
        selected = as.list(colnames(the.table)[sapply(the.table, class) %in% c("integer", "numeric")]),
        options = pickerOptions(
          selectedTextFormat = "count > 2",
          countSelectedText = "Multiple columns selected",
          `actions-box` = TRUE
        )
      ),
      pickerInput(
        inputId = "input_table_id_col",
        label = "Does your data have a unique ID column?:",
        multiple = FALSE,
        choices = c("No ID Column", colnames(the.table)),
        selected = "No ID Column"
      ),
      actionButton(
        #height: 50px;
        inputId = "subset_data",
        class = "full-width-button",
        style = "foreground-color:white;
        background-color:darksalmon;
        color:black;
        float:center;
        text-align:center;
        border-color:black;
        border-radius: 5px;
        border-width: 5px;
        margin-bottom: 5px;
        margin-top: 5px;",
        label = HTML("Subset Data and <br/> Define ID Column")
      )
    )
      })
    
      uploaded_data_values$display_data <- the.table
      uploaded_data_values$rendered_data <- the.table

      numeric_only_columns <- column_type_identifier(the.table)
      current_data_file_to_assign <- the.table[numeric_only_columns]
      uploaded_data_values$current_data_file <- current_data_file_to_assign
    
  })
  
  
  observeEvent(input$subset_data,{

    if(length(ncol(uploaded_data_values$display_data) >= length(input$varchoice))){
      uploaded_data_values$current_data_file <- uploaded_data_values$display_data[input$varchoice]
      uploaded_data_values$rendered_data <- uploaded_data_values$display_data %>%
        select(-c(setdiff(as.vector(colnames(uploaded_data_values$the_table)[sapply(uploaded_data_values$the_table, class) %in% c("integer", "numeric")]),
                          input$varchoice)))
    }
    
    the.table <- uploaded_data_values$the_table
    
    if(input$input_table_id_col == "No ID Column"){
      
      rownames(the.table) <- NULL
      
      rownames(the.table) <- paste0("Case ", rownames(the.table))
      
    }else{
      
      rownames(the.table) <- NULL
      
      selected_col <- input$input_table_id_col
      
      rownames(the.table) <- paste0("Case ", rownames(the.table), ": ", the.table[[selected_col]])
      
    }
    
    uploaded_data_values$display_data <- the.table
    uploaded_data_values$rendered_data <- the.table
    
    numeric_only_columns <- column_type_identifier(the.table)
    current_data_file_to_assign <- the.table[numeric_only_columns]
    uploaded_data_values$current_data_file <- current_data_file_to_assign

  })
  
  output$view <- renderDT(
    uploaded_data_values$rendered_data,
    options = list(scrollX = TRUE,
                   searching = FALSE,
                   lengthChange = FALSE),
    rownames = FALSE
  )
  
  
  ########################### PANEL 'CLUSTER DATA' ###########################
  ############################################################################
  
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
  
  # Trigger the tour when the button is pressed
  observeEvent(input$tour_kmeans, {
    
    # Start the tour
    introjs(session,
            options = list("nextLabel" = "Next",
                           "prevLabel" = "Previous",
                           "skipLabel" = "Quit",
                           steps = list(
                             list(
                               element = "#init_kmeans",
                               intro = "Click this button to compute your 
                               K-means clusters."),
                             
                             list(
                               element = "#infoButton_kmean",
                               intro = "Clicking this button lets you read some 
                               information about using the K-means clustering tab."),
                             
                             list(
                               element = "#clusters",
                               intro = "Adjusting this input lets you determine 
                               how many K-means clusters you wish to calculate."),
                             
                             list(
                               element = "#setrandseedkmean",
                               intro = "This toggle enables you to set a seed 
                               when producing your K-means clusters, allowing for 
                               reproducible results."),
                             
                             list(
                               element = "#kmeans_tabs_box",
                               intro = "Under these tabs you will find statistics 
                               and charts detailing the fit of your K-means clusters.")
                             )
                           ),
            )
    
  })
  
  kmeans_solution <- reactiveValues(current_kmeans_solution=NULL)
  kmeans_count <- reactiveValues(value = 0)
  
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
    
    observeEvent(input$init_kmeans, {
      kmeans_count$value <- kmeans_count$value + 1
    })
    
    kmeans_solution$current_kmeans_solution <- create_user_gen_kmeans_solution("default_name", kmeans(uploaded_data_values$current_data_file, isolate(input$clusters)))
    
    updateSelectInput(session = session, inputId = "kmeans_table_to_show", choices = c('All Clusters Overview', sort(unique(kmeans_solution$current_kmeans_solution@uclusters))))
    
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
    
    #displays the pseudoF
    FSTAT <- pseudoF(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution,input$clusters)
    output$pseudoF <- renderText({ paste("Pseudo F: ", FSTAT) })
    
    raw_silh_data <- reactive({
      return(
        as.data.frame(
          silhouette(
            kmeans_solution$current_kmeans_solution@uclusters,
            daisy(uploaded_data_values$current_data_file)
            )
          )
        )
    })
    
    kmeans_plot_output <- reactive({
      
      # raw_silh_data <- as.data.frame(silhouette(kmeans_solution$current_kmeans_solution@uclusters,
      #                                           daisy(uploaded_data_values$current_data_file)))
      
      min_silh_width <- ifelse(min(raw_silh_data()$sil_width) > 0,
                               0,
                               min(raw_silh_data()$sil_width))
      
      max_count_df <- raw_silh_data() %>%
        mutate(sil_width_bin = trunc(sil_width / 0.01) * 0.01) %>%
        group_by(cluster, sil_width_bin) %>%
        tally(name = "count") %>%
        ungroup() 
      
      max_count <- max(max_count_df$count)
      
      max_count_perc <- max_count_df %>%
        group_by(cluster) %>%
        reframe(count_as_cluster_perc = count/sum(count)) %>%
        select(count_as_cluster_perc) %>%
        max() 
      
      plot_output_list <- list()
      
      if(input$k_means_cluster == "All"){
        
        defined_clusters <- sort(unique(raw_silh_data()$cluster))
        
      }else{
        
        defined_clusters <- input$k_means_cluster
        
      }
      
      
      for(cluster in defined_clusters){
        
        ## Filter for data we need
        data_to_graph <- raw_silh_data() %>%
          filter(cluster == !!cluster)
        
        if(input$k_means_plot == "Jitter"){
          
          ## Create Jitter
          plot_output_list[[cluster]] <- create_jitter_plot(data_to_graph,
                                                            min_silh_width,
                                                            cluster)
          }
        
        if(input$k_means_plot == "Violin"){
          
          ## Create Violin
          plot_output_list[[cluster]] <- create_violin_plot(data_to_graph,
                                                            min_silh_width,
                                                            cluster)
          }
        
        if(input$k_means_plot == "Histogram"){
          
          ## Create histogram
          plot_output_list[[cluster]] <- create_histogram_plot(data_to_graph,
                                                               min_silh_width,
                                                               max_count_perc,
                                                               cluster)
          }
        
        if(input$k_means_plot == "Silhouette"){
          
          data_to_graph <- data_to_graph %>%
            arrange(sil_width)
          
          data_to_graph$nrow <- 1:nrow(data_to_graph)
          
          ## Create silhouette
          plot_output_list[[cluster]] <- create_silhouette_plot(data_to_graph,
                                                                min_silh_width,
                                                                cluster)
          }
        
      }
      
      
      if(input$k_means_cluster == "All"){
        
        return(patchwork::wrap_plots(plot_output_list,
                                     ncol = floor(sqrt(length(plot_output_list)))+1))
        
      }else{
        
        return(plot_output_list[[1]])
        
      }
      
    })
    
    
    output$kmeans_silh <- renderPlot({
      
      kmeans_plot_output()
      
    })
    
    
    distances_from_ucenters <- reactive({
      
      ## Grab raw silhouette data and make row names the ID col
      raw_silh_data <- raw_silh_data()
      rownames(raw_silh_data) <- names(kmeans_solution$current_kmeans_solution@uclusters)
      raw_silh_data <- raw_silh_data %>%
        select(-c(neighbor)) %>%
        mutate(ID = rownames(raw_silh_data))
      
      ## Create ucenters data
      ucenters <- as.data.frame(kmeans_solution$current_kmeans_solution@ucenters)
      ucenters$cluster <- rownames(ucenters)
      
      ## Create blank df to append to
      distances_from_ucenters <- data.frame()
      
      ## For each unique cluster
      for(i in unique(ucenters$cluster)){
        
        ## Filter to get each cluster#s ucenters
        cluster_ucenters <- ucenters %>%
          filter(cluster == i) %>%
          select(-cluster)
        
        ## Get the cluster membership (row names)
        cluster_membership <- raw_silh_data %>%
          filter(cluster == i) %>%
          rownames()
        
        ## Get each row of data for each member
        cluster_membership_data <- uploaded_data_values$current_data_file %>%
          filter(rownames(uploaded_data_values$current_data_file) %in% cluster_membership)
        
        ## Get distance from ucenter by doing actual data less ucenter 
        cluster_distance_from_ucenters <- sweep(cluster_membership_data, 2, as.numeric(cluster_ucenters[1, ]), FUN = "-")
        
        ## Set cluster to be what i is, add unique ID col
        # cluster_distance_from_ucenters$cluster <- i
        cluster_distance_from_ucenters$ID <- rownames(cluster_distance_from_ucenters)
        
        cluster_distance_from_ucenters <- cluster_distance_from_ucenters %>% 
          left_join(raw_silh_data)
        
        ## Bind to the master distances_from_ucenters df
        distances_from_ucenters <- rbind(distances_from_ucenters, cluster_distance_from_ucenters)
        
      }
      
      ## Clean up df
      distances_from_ucenters <- distances_from_ucenters %>%
        relocate(ID, sil_width, everything()) %>%
        arrange(desc(sil_width)) %>%
        mutate(across(where(is.numeric), round, 2))
      
      ## Return that master df
      return(distances_from_ucenters)
      
    })
    
    kmeans_table_to_show <- reactive({
      
      if(input$kmeans_table_to_show == 'All Clusters Overview'){
        return(kmeans_table())
      }else{
        return(
          
          distances_from_ucenters() %>%
            filter(cluster == input$kmeans_table_to_show) %>%
            select(-c(cluster))
          
        )
      }
      
    })
    
    ## Let output be either the all clusters overview or individual clusters
    output$kmeans_tab <- renderDT(
      
      kmeans_table_to_show(),
      options = list(scrollX = TRUE,
                     searching = FALSE,
                     lengthChange = FALSE),
      rownames = FALSE
    )

    
  })
  
  #### Panel 'Train the SOM'
  #############################################################################
  
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
  
  # Trigger the tour when the button is pressed
  observeEvent(input$tour_som, {
    
    # Start the tour
    introjs(session,
            options = list("nextLabel" = "Next",
                           "prevLabel" = "Previous",
                           "skipLabel" = "Quit",
                           
                           steps = list(
                             list(
                               element = "#trainbutton",
                               intro = "Click this button to compute your
                               SOM AI solution."),

                             list(
                               element = "#som_dimensions",
                               intro = "Here you can adjust the size of your 
                               SOM AI grid. If unsure, the default size of 5x5 
                               is sufficient."),

                             list(
                               element = "#advanced_options_intro",
                               intro = "Here you can adjust various advanced options. 
                               If unsure, the default options are sufficient."),
                             
                             list(
                               element = "#setrandseed",
                               intro = "This toggle enables you to set a seed 
                               when producing your SOM AI grid, allowing for 
                               reproducible results."),
                             
                             list(
                               element = "#SOM_AI_tabset",
                               intro = "Under these tabs you will find statistics 
                               and charts detailing the fit of your SOM AI clusters solution.")
                           )
            ),
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
  
  som_button_pressed_tracker <- reactiveValues(advancedInfoToggle = 0)
  
  
  observeEvent(input$advancedSOMinfo, {
    if(input$advancedSOMinfo == 0){
      som_button_pressed_tracker$advancedInfoToggle <- 0
    } else {
      som_button_pressed_tracker$advancedInfoToggle <- som_button_pressed_tracker$advancedInfoToggle + 1
    }
  })

  
  # Observe statement for show/hide advanced info selector
  observe(if (is.null(som_solution$current_som_solution) == TRUE) {
    shinyjs::hide(id = "advancedSOMinfo")
  } else {
    shinyjs::show(id = "advancedSOMinfo")
  })
  
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
    
    output$trainnotice_header <- renderUI({
      
      tagList(h3(paste("SOM trained", format(Sys.time(),format="%d %b %Y"), "at", format(Sys.time(),format="%T"), sep=" "), style = "text-align: center;"),
              h4("You can examine your SOM AI statistics here, or progress to the next tab to compare your SOM AI result to your K-Mean clusters.", style = "text-align: center;"),
      )
      
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
        
        df <- df %>%
          mutate(`p Value` = ifelse(`p Value` == 0, 'Too small to represent', `p Value`))
        
        return(df)
      }
      
      # Call the function with your list
      my_list <- anova_info$anova_results[(length(anova_info$anova_results)-3):2]
      
      anova_info$anova_results_df <- parse_list_to_dataframe(my_list)
      
    })
    
    output$trainnotice_advanced_info <- renderUI({
      
      ### post the quality control factors as well
      qual_measures <- quality(som_solution$current_som_solution)
      anova_results <- retrieve_ANOVA_results(som_solution$current_som_solution)
      
      #shinyjs::hidden(
      div(id = "advanced_info",
          #now print out the results
          tagList(
            br(),
            h3(paste("SOM Solution Statistics"), style = "text-align: center;"),
            #p(paste("Trained SOM ", format(Sys.time(),format="%Y-%m-%d-%H:%M:%S"),sep=" ")),
            
            h4(
              paste("Topographic Error:  ", format(qual_measures$topographic,digits=4), "|",
                    "Quantization Error: ", format(qual_measures$quantization,digits=4), "|",
                    anova_results[length(anova_results)], sep=" ")
              ),
            
          ),
          renderDT(parsed_anova_results(),
                   options = list(
                     pageLength = 10,  # Show 10 rows per page
                     dom = 'tip'  # Only show table (t), info (i), and pagination (p)
                   )
          )
      )
      #)
      
    })
    
    
    
    output$som_3Dplot <- renderPlot({
      
      SOM_SC <- superClass(sommap=som_solution$current_som_solution, method='ward.D', k=input$som_3Dplot_superclusters)
      
      plot(SOM_SC, what='prototypes', type='dendrogram')
      
    })
    
    
    output$som_3DMap <- renderPlot({
      
      SOM_SC <- superClass(sommap=som_solution$current_som_solution, method='ward.D', k=input$som_3DMap_superclusters)
      
      plot(SOM_SC, what='prototypes', type='grid')
      
    })
    
  })



    #### Panel 'Plot Map'
    #############################################################################
  
  # Trigger the tour when the button is pressed
  observeEvent(input$tour_viz, {
    
    # Start the tour
    introjs(session,
            options = list("nextLabel" = "Next",
                           "prevLabel" = "Previous",
                           "skipLabel" = "Quit",
                           
                           steps = list(
                             list(
                               element = "#save_som_intro_box",
                               intro = "Click this button to save your SOM 
                               solution from the previous tab."),
                             
                             list(
                               element = "#infoButton_plot_map",
                               intro = "Clicking this button lets you read some 
                               information about using the Compare and Visualise tab."),
                             
                             list(
                               element = "#somplotwhat_introbox",
                               intro = "Here you can choose whether to plot 
                               your dataset's observations or prototypes."),
                             
                             list(
                               element = "#somplottype_introbox",
                               intro = "Here you can choose what type of chart 
                               to display."),
                             
                             list(
                               element = "#conditional_toggles_introbox",
                               intro = "Some charts take additional toggles, 
                               which you can adjust here if applicable.")
                           )
            ),
    )
    
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
      tmp.var <- seq(from = 1, to = ncol(uploaded_data_values$current_data_file), by = 1)
    }
    else {tmp.var <- input$somplotvar}
    
    #This if/else set is here to add cluster labels to neurons for observation plots only
    temp.dim<-som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
    
    if(input$somplotwhat =='obs' & input$somplottype == 'boxplot'){
      
      plot_obj <- ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, variable = tmp.var, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))))
      
      plot_obj <- remove_xaxis_labels(plot_obj)
      
      plot_obj %>%
        layout(title = "Overview of Variables")
      
    }
    else if(input$somplotwhat =='obs' & input$somplottype == 'color'){plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, variable = tmp.var, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))) }
    else if(input$somplotwhat == 'obs' & input$somplottype == 'names'){create_nice_ggplot(superClass(sommap=som_solution$current_som_solution, method='ward.D', k=input$names_SC_num), som_solution$current_som_solution)}
    else if(input$somplotwhat =='obs' & input$somplottype == 'barplot'){
      
      plot_obj <- ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype, show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))))
      
      plot_obj <- remove_xaxis_labels(plot_obj)
      
      plot_obj %>%
        layout(title = "Overview of Variables"
        ) 
      
      
    }
    
    else if (input$somplotwhat == 'prototypes' & input$somplottype == 'barplot'){
      
      plot_obj <- ggplotly(plot(x=som_solution$current_som_solution, what=input$somplotwhat, type=input$somplottype))
      
      plot_obj <- remove_xaxis_labels(plot_obj)
      
      plot_obj %>%
        layout(title = "Overview of Variables")
      
    }
    
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
    
    if(is.null(som_solution$current_som_solution)){
      
      shinyalert(
        title = "You must first train the SOM AI",
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
    
    previous_som <- som_solution$current_som_solution #need to rename the object or it may overwrite later current_som objects
    save(previous_som, file = "./tmp/SavedSOMObject")})
  # Downloadable csv of selected dataset ----
  

  save_som_notice_text <- reactiveVal("No SOM Solution saved yet.")
  
  observeEvent(input$save_som, {
    
    if(input$save_som == 0){
      save_som_notice_text("No SOM Solution saved yet.")
      return()
      }
    
    if(is.null(som_solution$current_som_solution)){
      save_som_notice_text("No SOM Solution saved yet.")
      return()
      }
    
    save_som_notice_text(paste("SOM saved", format(Sys.time(),format="%d %b %Y"), "at", format(Sys.time(),format="%T"), sep=" "))
        
      })
  
  output$save_som_notice <- renderUI(save_som_notice_text())
    

  
  #### Panel 'Case Prediction'
  #############################################################################
  
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
  
  # Trigger the tour when the button is pressed
  observeEvent(input$tour_scenarios, {
    
    # Start the tour
    introjs(session,
            options = list("nextLabel" = "Next",
                           "prevLabel" = "Previous",
                           "skipLabel" = "Quit",
                           
                           steps = list(
                             list(
                               element = "#infoButton_scenarios",
                               intro = "Clicking this button lets you read some 
                               information about using the Compare and Visualise tab."),
                             
                             list(
                               element = "#Agent_Setup_introbox",
                               intro = "On first using the Simulations tab set up 
                               the model by pressing this button. You only need 
                               to do this once."),
                             
                             list(
                               element = "#Agent_Run_Clusters_introbox",
                               intro = "Here you can run your clusters, including 
                               different scenarios you model."),
                             
                             list(
                               element = "#SensitivityAnalysis_introbox",
                               intro = "here you can put error bars around how 
                               effective your interventions may be, and run a 
                               Monte-Carlo analysis for how effective your 
                               interventions are.")
                             
                             )
                           
                           )
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
    
    output$somplotagent <- renderPlot({
      
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
    
    agent_cluster_values$agentdf <- plot_agent_SOM(new_data_state, som_solution$current_som_solution, agent_cluster_values$agent_drawtools, agent_cluster_values$agentdf)[[1]]
    
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

    test_expand_gird_df <- expand.grid(monte_carlo_grid)
    
    test_expand_gird_df <- test_expand_gird_df %>%
      arrange(across(all_of(names(test_expand_gird_df)[dev_cols])))
    
    permutation_space <- as.list(as.data.frame(t(test_expand_gird_df)))

    # start with progress here
    withProgress(message = 'Running Simulation:', value = 0, { 
      
      state_to_test <- floor(runif((permutation_states*2), min= 1, max = (permutation_states + 1)))
      
      progress_steps <- length(state_to_test)+1
      
      # Combine all states at once into a matrix or dataframe
      temp_states <- do.call(rbind, permutation_space[state_to_test])
      
      # Predict on the whole batch
      quadrants <- predict(som_solution$current_som_solution, temp_states)
      
      # Count quadrant occurrences
      tab <- as.data.frame(table(quadrants))
      
      for(row in 1:nrow(tab)){
        
        # print(row)
        
        quadrant_number <- as.numeric(as.character(tab[[row, 1]]))
        
        solution_space[[quadrant_number]] <- as.numeric(tab[row, 2])
        
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
      
      incProgress(1/progress_steps)
      
      Sys.sleep(1)
      
    })
    
    removeModal()
    output$sensitivity_barplot <- renderPlot({
      barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen")
    })
  })

  infoButton <- reactive({  input$infoButton  })
  
  systems_mapping_tab_button_pressed_tracker <- reactiveValues(pop_up_systems = FALSE, exportOptionsToggle=0, egoNetworkToggle=0, advancedOptionsToggle=0, shortestPathsToggle=0, weightsOptionsToggle=0)

  
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

  
  observeEvent(input$exportOptions, {
    if(input$exportOptions == 0){
      systems_mapping_tab_button_pressed_tracker$exportOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$exportOptionsToggle <- systems_mapping_tab_button_pressed_tracker$exportOptionsToggle + 1
    }
  })

  observeEvent(input$egoNetwork, {
    if(input$egoNetwork == 0){
      systems_mapping_tab_button_pressed_tracker$egoNetworkToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$egoNetworkToggle <- systems_mapping_tab_button_pressed_tracker$egoNetworkToggle + 1
    }
  })

  observeEvent(input$advancedOptions, {
    if(input$advancedOptions == 0){
      systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle <- systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle + 1
    }
  })

  observeEvent(input$shortestPaths, {
    if(input$shortestPaths == 0){
      systems_mapping_tab_button_pressed_tracker$shortestPathsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$shortestPathsToggle <- systems_mapping_tab_button_pressed_tracker$shortestPathsToggle + 1
    }
  })

  observeEvent(input$weightsOptions, {
    if(input$weightsOptions == 0){
      systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle <- 0
    } else {
      systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle <- systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle + 1
    }
  })


  ########## OBSERVE STATEMENTS FOR MODAL BOXES ##########

  ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########

  # Observe statement for show/hide export options box
  observe(if (systems_mapping_tab_button_pressed_tracker$exportOptionsToggle == 0) {
    shinyjs::hide(id = "exportOptionsBox")
  })

  # Observe statement for show/hide ego network box
  observe(if (systems_mapping_tab_button_pressed_tracker$egoNetworkToggle == 0) {
    shinyjs::hide(id = "egoNetworkBox")
  })

  # Observe statement for show/hide advanced options box
  observe(if (systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle == 0) {
    shinyjs::hide(id = "advancedOptionsBox")
  })

  # Observe statement for show/hide shortest paths box
  observe(if (systems_mapping_tab_button_pressed_tracker$shortestPathsToggle == 0) {
    shinyjs::hide(id = "shortestPathsBox")
  })

  # Observe statement for show/hide weights box
  observe(if (systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle == 0) {
    shinyjs::hide(id = "weightsBox")
  })

  ########## OBSERVE STATEMENTS FOR INITIALLY CLOSING HIDE/SHOWS ##########

  ########## OBSERVE EVENTS FOR OPENING/CLOSING HIDE/SHOWS ##########
  # Observe statement for show/hide weights options box
  observeEvent(input$weightsOptions, {
    
    if(systems_mapping_tab_button_pressed_tracker$weightsOptionsToggle %% 2 == 1){
      shinyjs::show(id = "weightsBox")
    }else{
      shinyjs::hide(id = "weightsBox")
    }
  })

  # Observe statement for show/hide export options box
  observeEvent(input$exportOptions, {
    
    if(systems_mapping_tab_button_pressed_tracker$exportOptionsToggle %% 2 == 1){
      shinyjs::show(id = "exportOptionsBox")
    }else{
      shinyjs::hide(id = "exportOptionsBox")
    }
  })

  # Observe statement for show/hide ego network box
  observeEvent(input$egoNetwork, {
    
    if(systems_mapping_tab_button_pressed_tracker$egoNetworkToggle %% 2 == 1){
      shinyjs::show(id = "egoNetworkBox")
    }else{
      shinyjs::hide(id = "egoNetworkBox")
    }
  })

  # Observe statement for show/hide advanced options box
  observeEvent(input$advancedOptions, {
    
    if(systems_mapping_tab_button_pressed_tracker$advancedOptionsToggle %% 2 == 1){
      shinyjs::show(id = "advancedOptionsBox")
    }else{
      shinyjs::hide(id = "advancedOptionsBox")
    }
  })

  # Observe statement for show/hide shortest paths box
  observeEvent(input$shortestPaths, {
    
    if(systems_mapping_tab_button_pressed_tracker$shortestPathsToggle %% 2 == 1){
      shinyjs::show(id = "shortestPathsBox")
    }else{
      shinyjs::hide(id = "shortestPathsBox")
    }
  })

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
    
    if( !is.null(uploaded_data_values$current_data_file) && ncol(uploaded_data_values$current_data_file) > 0 ){
      
      
      
      dynamic_nodes <- data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                                  label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
      dynamic_nodes_ids_values$dynamic_nodes_ids <- setNames(dynamic_nodes$id, dynamic_nodes$label)
      
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
    
    systems_mapping_values$network_initialised <- TRUE
    
    #### Systems Mapping Tab ####
    
    ##### Setting Up The Initial File, inc. if it has clustering #####
    
    observe({
      
      if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
        rawcases_data <- cbind(uploaded_data_values$current_data_file, "Group" = kmeans_solution$current_kmeans_solution@uclusters)
        systems_mapping_values$rawcases <- rawcases_data
      } else {
        rawcases_data <- data.frame(uploaded_data_values$current_data_file, Group = NA)
        systems_mapping_values$rawcases <- rawcases_data
      }
      
    })
    
    ########## OBSERVE STATEMENTS FOR MODAL BOXES ##########
    
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
        
        systems_mapping_values$links5_download <- new_links
        
        new_links
        
      } else {
        systems_mapping_values$links5_download <- links4()
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
        
        systems_mapping_values$nodes4_download <- new_nodes
        systems_mapping_values$examine_nodes_dropdown_ids <- setNames(new_nodes$id, new_nodes$label)
        
        systems_mapping_values$user_set_seed <- seed()
        
        new_nodes
      } else {
        systems_mapping_values$nodes4_download <- nodes3()
        systems_mapping_values$examine_nodes_dropdown_ids <- setNames(nodes3()$id, nodes3()$label)
        systems_mapping_values$user_set_seed <- seed()
        nodes3()
      }
    })

    
    final_network <- reactive({

      set.seed(seed())
      
      visNetwork(nodes4(), links5(), width = "100%",
                 main = title(),
                 submain = subtitle(),
                 footer = footer()) %>%
        visPhysics(enabled = F) %>%
        visOptions(highlightNearest = T, 
                   manipulation = list(enabled = T, addNodeCols = c("label")))  %>%
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
    
    output$networkPlot <- renderVisNetwork({final_network()})

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
    
    
  }
  
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
      
      fs <-file_names
      fileConn<-file("info.txt")
      writeLines(info_text, fileConn)
      close(fileConn)
      
      if(is.null(kmeans_solution$current_kmeans_solution) == FALSE){
        
        kmeans_dir <- file.path(tmpdir, 'kmeans_clustering')
        dir.create(kmeans_dir) 
        
        fstat <- pseudoF(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution, nrow(kmeans_solution$current_kmeans_solution@ucenters))
        fstat_col <- rep(0, nrow(kmeans_solution$current_kmeans_solution@ucenters)-1)
        fstat_col <- c(fstat, fstat_col)
        kcenters <- cbind(kmeans_solution$current_kmeans_solution@ucenters, "size" = kmeans_solution$current_kmeans_solution@usize, "Pseudo_F" = fstat_col)
        write.csv(kcenters, file = file.path(kmeans_dir, "kmeans_profiles.csv"))
        clustered_data <- cbind(uploaded_data_values$current_data_file,
                                silhouette(kmeans_solution$current_kmeans_solution@uclusters, daisy(uploaded_data_values$current_data_file)))
        clustered_data$neighbor <- NULL
        write.csv(clustered_data, file = file.path(kmeans_dir, "clustered_data.csv"))
        if(input$setrandseedkmean == "Yes"){kseed = as.character(input$randseedkmean)}
        else{kseed = "NULL"}
        fileConn<-file(file.path(kmeans_dir, "kmean_seed.txt"))
        writeLines(kseed, fileConn)
        close(fileConn)
        
        raw_silh_data <- as.data.frame(silhouette(kmeans_solution$current_kmeans_solution@uclusters,
                                                  daisy(uploaded_data_values$current_data_file)))
        
        min_silh_width <- ifelse(min(raw_silh_data$sil_width) > 0,
                                 0,
                                 min(raw_silh_data$sil_width))
        
        max_count_df <- raw_silh_data %>%
          mutate(sil_width_bin = trunc(sil_width / 0.01) * 0.01) %>%
          group_by(cluster, sil_width_bin) %>%
          tally(name = "count") %>%
          ungroup() 
        
        max_count <- max(max_count_df$count)
        
        max_count_perc <- max_count_df %>%
          group_by(cluster) %>%
          reframe(count_as_cluster_perc = count/sum(count)) %>%
          select(count_as_cluster_perc) %>%
          max() 
        
        jitter_plot_output_list <- list()
        violin_plot_output_list <- list()
        histogram_plot_output_list <- list()
        silhouette_plot_output_list <- list()
        
        defined_clusters <- sort(unique(raw_silh_data$cluster))
        
        for(cluster in defined_clusters){
          
          ## Filter for data we need
          data_to_graph <- raw_silh_data %>%
            filter(cluster == !!cluster)
          
          ## Create Jitter
          jitter_plot_output_list[[cluster]] <- create_jitter_plot(data_to_graph,
                                                                   min_silh_width,
                                                                   cluster)
          
          ## Create Violin
          violin_plot_output_list[[cluster]] <- create_violin_plot(data_to_graph,
                                                                   min_silh_width,
                                                                   cluster)
          ## Create histogram
          histogram_plot_output_list[[cluster]] <- create_histogram_plot(data_to_graph,
                                                                         min_silh_width,
                                                                         max_count_perc,
                                                                         cluster)
          ## Create silhouette
          data_to_graph <- data_to_graph %>%
            arrange(sil_width)
          
          data_to_graph$nrow <- 1:nrow(data_to_graph)
          
          silhouette_plot_output_list[[cluster]] <- create_silhouette_plot(data_to_graph,
                                                                           min_silh_width,
                                                                           cluster)
          
        }

        ## WRITE JITTER PLOT TO PDF ##
        pdf(file.path(kmeans_dir, "all_jitter_plots.pdf"))
        print(patchwork::wrap_plots(jitter_plot_output_list,
                                    ncol = floor(sqrt(length(jitter_plot_output_list)))+1))
        dev.off()
        
        ## WRITE VIOLIN PLOT TO PDF ##
        pdf(file.path(kmeans_dir, "all_violin_plots.pdf"))
        print(patchwork::wrap_plots(violin_plot_output_list,
                                    ncol = floor(sqrt(length(violin_plot_output_list)))+1))
        dev.off()
        
        ## WRITE HISTOGRAM PLOT TO PDF ##
        pdf(file.path(kmeans_dir, "all_histogram_plots.pdf"))
        print(patchwork::wrap_plots(histogram_plot_output_list,
                                    ncol = floor(sqrt(length(histogram_plot_output_list)))+1))
        dev.off()
        
        ## WRITE SILHOUETTE PLOT TO PDF ##
        pdf(file.path(kmeans_dir, "all_silhouette_plots.pdf"))
        print(patchwork::wrap_plots(silhouette_plot_output_list,
                                    ncol = floor(sqrt(length(silhouette_plot_output_list)))+1))
        dev.off()
        
        file_names <- c(file_names,
                        paste0('kmeans_clustering/', list.files(kmeans_dir)))
        
        }
      
      
      
      
      if(is.null(som_solution$current_som_solution) == FALSE){
        
        SOM_AI_dir <- file.path(tmpdir, 'SOM_AI')
        dir.create(SOM_AI_dir) 
        
        ## Create som_options file -- 
        som_options =list("x dim" = som_solution$current_som_solution$parameters$the.grid$dim[1], "y dim" = som_solution$current_som_solution$parameters$the.grid$dim[2],
                          "proto_init" = som_solution$current_som_solution$parameters$init.proto, "max_iter" = som_solution$current_som_solution$parameters$maxit,
                          "data_scaling" = som_solution$current_som_solution$parameters$scaling, "gradient_descent" = som_solution$current_som_solution$parameters$eps0)
        som_options_df <- as.data.frame(som_options)
        write.csv(som_options_df, file = file.path(SOM_AI_dir, "som_options.csv")) 
        
        ## Create summary_class file -- 
        class_summary <- capture.output(summary(som_solution$current_som_solution))
        temp_qual <- quality(som_solution$current_som_solution)
        class_summary <- class_summary[12:length(class_summary)]
        class_summary <- append(class_summary, paste("      Quant Error:", format(temp_qual$quantization, digits = 6)), after=2)
        fileConn<-file(file.path(SOM_AI_dir, "summary_class.txt")) 
        writeLines(class_summary, fileConn)
        close(fileConn)
        
        ## Create som_seed file -- 
        if(input$setrandseed == "Yes"){somseed = as.character(input$randseed)}
        else{somseed = "NULL"}
        fileConn<-file(file.path(SOM_AI_dir, "som_seed.txt")) 
        writeLines(somseed, fileConn)
        close(fileConn)
        
        ## Create som_profiles file -- 
        som_profiles <- as.data.frame(som_solution$current_som_solution$prototypes)
        write.csv(som_profiles, file = file.path(SOM_AI_dir, "som_profiles.csv")) 
        
        ## Create data_quadrants file -- 
        data_quadrants <- cbind(uploaded_data_values$current_data_file, "quadrant" = som_solution$current_som_solution$clustering)
        write.csv(data_quadrants, file = file.path(SOM_AI_dir, "data_quadrants.csv")) 
        
        ## Create som_barplot file --
        temp.dim<-som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]]
        pdf(file.path(SOM_AI_dir, "som_barplot.pdf")) 
        print(plot(x=som_solution$current_som_solution, what="obs", type="barplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))))
        dev.off()
        
        ## Create som_boxplot file --
        pdf(file.path(SOM_AI_dir, "som_boxplot.pdf")) 
        print(plot(x=som_solution$current_som_solution, what="obs", type="boxplot",
             show.names = TRUE,names = paste("Quadrant ", 1:prod(temp.dim))))
        dev.off()
        
        file_names <- c(file_names,
                        paste0('SOM_AI/', list.files(SOM_AI_dir)))
        
      }
      
      
      
      if(is.null(agent_cluster_values$agent_cluster_tracker) == FALSE && agent_cluster_values$agent_cluster_tracker@cluster_tested != "None"){
        
        
        policy_preds_dir <- file.path(tmpdir, 'scenario_simulations')
        dir.create(policy_preds_dir) 
        
        write.csv(agent_cluster_values$agent_cluster_tracker@checked_data, file = file.path(policy_preds_dir, "adjust_kmeans.csv"))
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
        write.csv(tested_changes, file = file.path(policy_preds_dir, "tested_intervention.csv"))
        
        sub_sol_space <- c()
        sub_sol_names <- c()
        for(i in 1:length(agent_cluster_values$agent_cluster_tracker@sensitivity_result)) {
          
          if(agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]]> 0){
            sub_sol_names <-c(sub_sol_names, i)
            sub_sol_space <- c(sub_sol_space, agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]])
          }}
        
        pdf(file.path(policy_preds_dir, "sensitivity.pdf"))
        print(barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen"))
        dev.off()
        
        file_names <- c(file_names,
                        paste0('scenario_simulations/', list.files(policy_preds_dir)))
        
      }
      
      if(is.null(pred_cases$predicted_cases) == FALSE){
        
        data_forecasting_dir <- file.path(tmpdir, 'data_forecasting')
        dir.create(data_forecasting_dir) 
        
        write.csv(pred_cases$predicted_cases, file = file.path(data_forecasting_dir, "predicted_quadrants.csv"))
        
        file_names <- c(file_names,
                        paste0('data_forecasting/', list.files(data_forecasting_dir)))
        
      }
      
      if(is.null(systems_mapping_values$network_initialised) == FALSE){

        systems_mapping_dir <- file.path(tmpdir, 'systems_mapping')
        dir.create(systems_mapping_dir)

        write.csv(systems_mapping_values$nodes4_download, file = file.path(systems_mapping_dir, "nodes_list.csv"))

        systems_mapping_values$links5_download <- systems_mapping_values$links5_download[, !(names(systems_mapping_values$links5_download) %in% c("color", "width"))]

        systems_mapping_values$links5_download$to <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(systems_mapping_values$links5_download$to, dynamic_nodes_ids_values$dynamic_nodes_ids)]
        systems_mapping_values$links5_download$from <- names(dynamic_nodes_ids_values$dynamic_nodes_ids)[match(systems_mapping_values$links5_download$from, dynamic_nodes_ids_values$dynamic_nodes_ids)]

        write.csv(systems_mapping_values$links5_download, file = file.path(systems_mapping_dir, "edges_list.csv"))

        fileConn<-file(file.path(systems_mapping_dir, "set_network_seed.txt"))
        set_seed_info <- paste('Seed set for network visualisation is: ', systems_mapping_values$user_set_seed, sep = '')
        writeLines(set_seed_info, fileConn)
        close(fileConn)

        file_names <- c(file_names,
                        paste0('systems_mapping/', list.files(systems_mapping_dir)))

      }
      
      zip(zipfile=fname, files=file_names)
      
    },
    contentType = "application/zip"
  )
  
  # User generated report code
  
  # ---- Helper: scalar-safe coalescer ----
  `%||s%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.character(a) && length(a) == 1 && !nzchar(a)) return(b)
    a
  }
  
  # ---- Dynamic UI for Section 1 ----
  output$s1_inputs <- renderUI({
    if (!isTRUE(input$include_s1)) return(NULL)
    tagList(
      tags$div(
        style = "margin-left: 8px; border-left: 3px solid #e1e1e1; padding-left: 12px;",
        textInput("s1_title", "Section 1 title", placeholder = "e.g., Methods"),
        textAreaInput(
          "s1_body", "Section 1 prose",
          placeholder = "Write content for Section 1 (Markdown supported)...",
          rows = 8
        )
      )
    )
  })
  
  
  # ---- Dynamic UI for Section 2 ----
  output$s2_inputs <- renderUI({
    if (!isTRUE(input$include_s2)) return(NULL)
    tagList(
      tags$div(
        style = "margin-left: 8px; border-left: 3px solid #e1e1e1; padding-left: 12px;",
        textInput("s2_title", "Section 2 title", placeholder = "e.g., Results"),
        textAreaInput(
          "s2_body", "Section 2 intro",
          placeholder = "Write intro for Section 2 (Markdown supported)...",
          rows = 6
        )
      )
    )
  })
  
  # ---- Dynamic UI for Section 3 ----
  output$s3_inputs <- renderUI({
    if (!isTRUE(input$include_s3)) return(NULL)
    tagList(
      tags$div(
        style = "margin-left: 8px; border-left: 3px solid #e1e1e1; padding-left: 12px;",
        textInput("s3_title", "Section 3 title", placeholder = "e.g., Results"),
        textAreaInput(
          "s3_body", "Section 3 intro",
          placeholder = "Write intro for Section 3 (Markdown supported)...",
          rows = 6
        )
      )
    )
  })
  
  # ---- Dynamic UI for Section 3 ----
  output$s3_inputs <- renderUI({
    if (!isTRUE(input$include_s4)) return(NULL)
    tagList(
      tags$div(
        style = "margin-left: 8px; border-left: 3px solid #e1e1e1; padding-left: 12px;",
        textInput("s4_title", "Section  title", placeholder = "e.g., Results"),
        textAreaInput(
          "s4_body", "Section 4 intro",
          placeholder = "Write intro for Section 4 (Markdown supported)...",
          rows = 6
        )
      )
    )
  })
  
  
  # ---- Rmd template built on the fly (parameterized) ----
  rmd_template_path <- reactive({
    
    rmd_lines <- c(
      '---',
      'title: "`r params$title`"',
      'subtitle: "`r params$subtitle`"',
      'author: ""',
      'date: "`r format(Sys.Date(), \'%d %B %Y\')`"',
      'output:',
      '  html_document:',
      '    toc: true',
      '    toc_float: false',
      '    number_sections: false',
      '    theme: cosmo',
      '    df_print: paged',
      'params:',
      '  title:',
      '    value: "Untitled Report"',
      '  subtitle:',
      '    value: ""',
      '  intro:',
      '    value: ""',
      
      
      # ---- Section 1 params ----
      '  include_s1:',
      '    value: false',
      '  s1_title:',
      '    value: "Section 1"',
      '  s1_body:',
      '    value: ""',
      # your wider app passes this:
      '  data_obj:',
      '    value: !r NULL',
      
      # ---- Section 2 params ----
      '  include_s2:',
      '    value: false',
      '  s2_title:',
      '    value: "Section 2"',
      '  s2_body:',
      '    value: ""',
      # Section 2 Tables 
      '  all_clusters_tbl:',
      '    value: !r NULL',
      '  cluster_rows_tbl:',
      '    value: !r NULL',
      # Section 2 Charts
      '  s2_jitters:',
      '    value: !r NULL',
      '  s2_violins:',
      '    value: !r NULL',
      '  s2_histograms:',
      '    value: !r NULL',
      '  s2_silhouettes:',
      '    value: !r NULL',
      
      # ---- Section 3 params ----
      '  include_s3:',
      '    value: false',
      '  s3_title:',
      '    value: "Section 3"',
      '  s3_body:',
      '    value: ""',
      # Section 3 Prose 
      '  s3_topo_prose:',
      '    value: !r NULL',
      '  s3_quant_prose:',
      '    value: !r NULL',
      '  s3_deg_free_prose:',
      '    value: !r NULL',
      '  s3_dendro:',
      '    value: !r NULL',
      '  s3_map:',
      '    value: !r NULL',
      # Section 3 table 
      '  s3_ANOVA_table:',
      '    value: !r NULL',
      
      # ---- Section 4 params ----
      '  include_s4:',
      '    value: false',
      '  s4_title:',
      '    value: "Section 2"',
      '  s4_body:',
      '    value: ""',
      # Section 4 charts
      '  s4_obs_box:',
      '    value: !r NULL',
      '  s4_obs_bar:',
      '    value: !r NULL',
      '  s4_proto_bar:',
      '    value: !r NULL',
      
      # Section 5 grid
      '  s5_grid:',
      '    value: !r NULL',
      
      '---',
      '',
      '```{r setup, include=FALSE}',
      'knitr::opts_chunk$set(',
      '  echo = FALSE,',
      '  warning = FALSE,',
      '  message = FALSE',
      ')',
      'library(DT)',
      'library(ggplot2)',
      '```',
      '',
      
      # ---- Intro block ----
      '```{r intro, results="asis"}',
      'if (nzchar(params$intro)) {',
      '  cat(params$intro, "\\n\\n")',
      '} else {',
      '  cat("This is the report intro. I want this to be populated by the `intro` user input/text.\\n\\n")',
      '}',
      '```',
      '',
      '___',
      '',
      
      # =========================
      # ---- Section 1 text ----
      # =========================
      '```{r section1_text, results="asis"}',
      'if (isTRUE(params$include_s1)) {',
      '  h <- if (nzchar(params$s1_title)) params$s1_title else "Section 1"',
      '  cat(paste0("## ", h, "\\n\\n"))',
      '  if (nzchar(params$s1_body)) {',
      '    cat(params$s1_body, "\\n\\n")',
      '  } else {',
      '    cat("This is section 1 intro, I want it to be populated by `s1_body`.\\n\\n")',
      '  }',
      '}',
      '```',
      '',
      
      # ---- Section 1 data + table (ONLY if include_s1) ----
      '```{r section1_data, eval=isTRUE(params$include_s1)}',
      'if (!is.null(params$data_obj)) {',
      '  data <- params$data_obj',
      '  datatable(data)',
      '} else {',
      '  # fallback dummy if data_obj not provided',
      '  data <- mtcars',
      '  data$car <- rownames(mtcars)',
      '  rownames(data) <- NULL',
      '  datatable(data)',
      '}',
      '```',
      '',
      
      # =========================
      # ---- Section 2 (TABSET) ----
      # =========================
      
      # ---- Section 2 header + intro + "Tables" tab heading (UNCHANGED) ----
      '```{r section2_text_and_tables_header, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      '  h2 <- if (nzchar(params$s2_title)) params$s2_title else "Section 2"',
      '  cat(paste0("## ", h2, "\\n\\n"))',
      '  if (nzchar(params$s2_body)) {',
      '    cat(params$s2_body, "\\n\\n")',
      '  } else {',
      '    cat("This is section 2 intro, populated by `s2_body`.\\n\\n")',
      '  }',
      '  cat(paste0("### ", "Tables", " {.tabset}\\n\\n"))',
      '  cat("#### K-Means Profiles\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_k_means_profiles, eval=isTRUE(params$include_s2)}',
      
      'DT::datatable(params$all_clusters_tbl)',
      
      '```',
      
      # ---- Case-Level Data Tabset Panel Title (UNCHANGED) ---- 
      '```{r section2_cluster_rows_header, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      '  cat("#### Individual Case Data\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- Case-Level Data Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_cluster_rows_tbl, eval=isTRUE(params$include_s2)}',
      'library(crosstalk)',
      
      'shared_all_clusters <- SharedData$new(params$cluster_rows_tbl, group = "cluster")',
      
      'crosstalk::bscols(',
        'widths = c(3, 9),',
        'list(',
          'filter_select(id = "species", label = "Cluster", sharedData = shared_all_clusters, group = ~cluster)',
        '),',
        'datatable(shared_all_clusters,',
                  'rownames = FALSE,',
                  'filter = "top",',
                  'options = list(pageLength = 10, autoWidth = TRUE, dom = "tip", lengthChange = FALSE))',
      ')',
      '```',
      
      # ---- Section 2 header + intro + "Tables" tab heading (UNCHANGED) ----
      # ---- Section 2 header + intro + "Tables" tab heading (UNCHANGED) ----
      '```{r section2_setting_up_charts, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      
      '  cat(paste0("### ", "Charts", " {.tabset}\\n\\n"))',
      
      '  cat("#### Jitter Plots\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_jitter_plots, eval=isTRUE(params$include_s2)}',
      
      'params$s2_jitters',
      
      '```',
      
      "",
      
      # ---- Case-Level Data Tabset Panel Title (UNCHANGED) ---- 
      '```{r section2_violin_plots_header, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      '  cat("#### Violin Plots\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_violin_plots, eval=isTRUE(params$include_s2)}',
      
      'params$s2_violins',
      
      '```',
      
      "",
      
      # ---- Case-Level Data Tabset Panel Title (UNCHANGED) ---- 
      '```{r section2_histogram_plots_header, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      '  cat("#### Histogram Plots\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_histogram_plots, eval=isTRUE(params$include_s2)}',
      
      'params$s2_histograms',
      
      '```',
      
      "",
      
      # ---- Case-Level Data Tabset Panel Title (UNCHANGED) ---- 
      '```{r section2_silhouette_plots_header, results="asis"}',
      'if (isTRUE(params$include_s2)) {',
      '  cat("#### Silhouette Plots\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section2_silhouette_plots, eval=isTRUE(params$include_s2)}',
      
      'params$s2_silhouettes',
      
      '```',
      
      "",
      
      # =========================
      # ---- Section 3 text ----
      # =========================
      '```{r section3_text, results="asis"}',
      'if (isTRUE(params$include_s3)) {',
      '  h <- if (nzchar(params$s3_title)) params$s3_title else "Section 3"',
      '  cat(paste0("## ", h, "\\n\\n"))',
      '  if (nzchar(params$s3_body)) {',
      '    cat(params$s3_body, "\\n\\n")',
      '  } else {',
      '    cat("This is section 1 intro, I want it to be populated by `s1_body`.\\n\\n")',
      '  }',
      '}',
      '```',
      '',
      
      '```{r section3_errors_prose, results="asis"}',
      'if (isTRUE(params$include_s3)) {',
      
      'cat(',
        '"In the SOM analysis:\n\n",',
        '"- ", params$s3_topo_prose, "\n",',
        '"- ", params$s3_quant_prose, "\n",',
        '"- ", params$s3_deg_free_prose, "\n\n",',
        'sep = ""',
      ')',
      
      'DT::datatable(params$s3_ANOVA_table)',
      
      'cat(paste0("### ", "SOM Super Cluster Charts", " {.tabset}\\n\\n"))',
      
      'cat("#### Dendrogram\\n\\n")',
      '}',
      '```',
      
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section3_dendrogram, eval=isTRUE(params$include_s3)}',
      'if (isTRUE(params$include_s3)) {',
      
      'plot(',
        'params$s3_dendro,',
        'what="prototypes",',
        'type="dendrogram"',
      ')',
      '}',
      
      '```',
      
      '',
      
      # ---- Case-Level Data Tabset Panel Title (UNCHANGED) ---- 
      '```{r section3_map, results="asis"}',
      'if (isTRUE(params$include_s3)) {',
      '  cat("#### Map\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- K Means Profiles Tabset Panel Content (UNCHANGED; still a placeholder) ----
      '```{r section3_map_plot, eval=isTRUE(params$include_s3)}',
      'if (isTRUE(params$include_s3)) {',
      'plot(',
      'params$s3_map,',
      'what="prototypes",',
      'type="grid"',
      ')',
      '}',
      
      '```',
      
      # =========================
      # ---- Section 4 text ----
      # =========================
      '```{r section4_text, results="asis"}',
      'if (isTRUE(params$include_s4)) {',
      '  h <- if (nzchar(params$s4_title)) params$s4_title else "Section 4"',
      '  cat(paste0("## ", h, "\\n\\n"))',
      '  if (nzchar(params$s4_body)) {',
      '    cat(params$s4_body, "\\n\\n")',
      '  } else {',
      '    cat("This is section 4 intro, I want it to be populated by `s4_body`.\\n\\n")',
      '  }',
      '}',
      '```',
      '',
      
      # ---- Section 4 "Barplots and Boxplots" tab heading (UNCHANGED) ----
      '```{r section4_setting_tabset, results="asis"}',
      'if (isTRUE(params$include_s4)) {',
      
      '  cat(paste0("### ", "Charts", " {.tabset}\\n\\n"))',
      
      '  cat("#### Observations Boxplot\\n\\n")',
      '}',
      '```',
      '',
      
      # ---- Display obs boxplot witin tabset ----
      '```{r section4_obs_box_plot, eval=isTRUE(params$include_s4)}',
      'if (isTRUE(params$include_s4)) {',
      'params$s4_obs_box',
      '}',
      
      '```',
      
      # ---- Section 4 "Barplots and Boxplots" tab heading (UNCHANGED) ----
      '```{r section4_obs_bar_plot, results="asis"}',
      'if (isTRUE(params$include_s4)) {',
      '  cat("#### Observations Barplot\\n\\n")',
      'params$s4_obs_bar',
      '}',
      '```',
      '',
      
      # ---- Section 4 "Barplots and Boxplots" tab heading (UNCHANGED) ----
      '```{r section4_proto_bar_plot, results="asis"}',
      'if (isTRUE(params$include_s4)) {',
      '  cat("#### Prototypes Barplot\\n\\n")',
      # 'params$s4_proto_bar',
      'params$s5_grid', ## REMOVE AFTER TRYING
      '}',
      '```',
      '',
      
      ""
      
      
      
      
    )
    
    f <- file.path(tempdir(), "report_template.Rmd")
    writeLines(rmd_lines, f, useBytes = TRUE)
    f
  })
  
  # ---- Download handler ----
  output$download_report <- downloadHandler(
    
    filename = function() {
      ttl <- input$title %||s% "report"
      ttl <- gsub("[^A-Za-z0-9_-]+", "_", ttl)
      paste0(ttl, ".html")
    },
    content = function(file) {
      
      if(is.null(kmeans_solution$current_kmeans_solution) == FALSE){
        
        fstat <- pseudoF(uploaded_data_values$current_data_file, kmeans_solution$current_kmeans_solution, nrow(kmeans_solution$current_kmeans_solution@ucenters))
        fstat_col <- rep(0, nrow(kmeans_solution$current_kmeans_solution@ucenters)-1)
        fstat_col <- c(fstat, fstat_col)
        kcenters <- cbind(kmeans_solution$current_kmeans_solution@ucenters, "size" = kmeans_solution$current_kmeans_solution@usize, "Pseudo_F" = fstat_col)
        clustered_data <- cbind(uploaded_data_values$current_data_file,
                                silhouette(kmeans_solution$current_kmeans_solution@uclusters, daisy(uploaded_data_values$current_data_file)))
        clustered_data$neighbor <- NULL

        raw_silh_data <- as.data.frame(silhouette(kmeans_solution$current_kmeans_solution@uclusters,
                                                  daisy(uploaded_data_values$current_data_file)))
        
        min_silh_width <- ifelse(min(raw_silh_data$sil_width) > 0,
                                 0,
                                 min(raw_silh_data$sil_width))
        
        max_count_df <- raw_silh_data %>%
          mutate(sil_width_bin = trunc(sil_width / 0.01) * 0.01) %>%
          group_by(cluster, sil_width_bin) %>%
          tally(name = "count") %>%
          ungroup() 
        
        max_count <- max(max_count_df$count)
        
        max_count_perc <- max_count_df %>%
          group_by(cluster) %>%
          reframe(count_as_cluster_perc = count/sum(count)) %>%
          select(count_as_cluster_perc) %>%
          max() 
        
        jitter_plot_output_list <- list()
        violin_plot_output_list <- list()
        histogram_plot_output_list <- list()
        silhouette_plot_output_list <- list()
        
        defined_clusters <- sort(unique(raw_silh_data$cluster))
        
        for(cluster in defined_clusters){
          
          ## Filter for data we need
          data_to_graph <- raw_silh_data %>%
            filter(cluster == !!cluster)
          
          ## Create Jitter
          jitter_plot_output_list[[cluster]] <- create_jitter_plot(data_to_graph,
                                                                   min_silh_width,
                                                                   cluster)
          
          ## Create Violin
          violin_plot_output_list[[cluster]] <- create_violin_plot(data_to_graph,
                                                                   min_silh_width,
                                                                   cluster)
          ## Create histogram
          histogram_plot_output_list[[cluster]] <- create_histogram_plot(data_to_graph,
                                                                         min_silh_width,
                                                                         max_count_perc,
                                                                         cluster)
          ## Create silhouette
          data_to_graph <- data_to_graph %>%
            arrange(sil_width)
          
          data_to_graph$nrow <- 1:nrow(data_to_graph)
          
          silhouette_plot_output_list[[cluster]] <- create_silhouette_plot(data_to_graph,
                                                                           min_silh_width,
                                                                           cluster)
          
        }
        
        ## CREATE ALL JITTER PLOTS OBJ ##
        all_jitter_plot <- patchwork::wrap_plots(
          jitter_plot_output_list,
          ncol = floor(sqrt(length(jitter_plot_output_list)))+1
          )
        
        ## CREATE ALL VIOLIN PLOTS OBJ ##
        all_violin_plot <- patchwork::wrap_plots(
          violin_plot_output_list,
          ncol = floor(sqrt(length(violin_plot_output_list)))+1
          )
        
        ## CREATE ALL HISTOGRAM PLOTS OBJ ##
        all_histogram_plot <- patchwork::wrap_plots(
          histogram_plot_output_list,
          ncol = floor(sqrt(length(histogram_plot_output_list)))+1
          )
        
        ## CREATE ALL SILHOUETTE PLOTS OBJ ##
        all_silhouette_plot <- patchwork::wrap_plots(
          silhouette_plot_output_list,
          ncol = floor(sqrt(length(silhouette_plot_output_list)))+1
          )
        
      }
      
      if(is.null(som_solution$current_som_solution) == FALSE){
        
        print('current som solution is firing')
        
        ## Create the ANOVA results table for the Rmd report --
        anova_results <- retrieve_ANOVA_results(som_solution$current_som_solution)
        
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
          
          df <- df %>%
            mutate(`p Value` = ifelse(`p Value` == 0, 'Too small to represent', `p Value`))
          
          return(df)
        }
        
        # Call the function with your list
        my_list <- anova_results[(length(anova_results)-3):2]
        
        anova_results_df <- parse_list_to_dataframe(my_list)
        
        
        ## Get the other stats
        SOM_topo_prose <- paste(
          'Topographical error:',
          round(
            as.numeric(
              quality(som_solution$current_som_solution)[1]
              ),
            3
            )
          )
        
        SOM_quant_prose <- paste(
          'Quantization error:',
          round(
            as.numeric(
              quality(som_solution$current_som_solution)[2]
              ),
            3
            )
        )
        
        SOM_deg_free_prose <- paste(
          as.character(unlist(
            retrieve_ANOVA_results(som_solution$current_som_solution)[
              length(retrieve_ANOVA_results(som_solution$current_som_solution))
              ]
            ))
          )
        
        ## Create SOM dendroplot and map
        
        ## Dendrogram
        SOM_SC_plot_data <- superClass(
            sommap=som_solution$current_som_solution,
            method='ward.D',
            k=input$som_3Dplot_superclusters)
        
        ## Map
        SOM_SC_map_data <- superClass(
          sommap=som_solution$current_som_solution,
          method='ward.D',
          k=input$som_3DMap_superclusters)
        
        ## CREATE SECTION FOUR BARPLOT AND BOXPLOT
        
        # OBS BOXPLOT --
        tmp.var <- seq(from = 1, to = ncol(uploaded_data_values$current_data_file), by = 1)
        temp.dim <- som_solution$current_som_solution[["parameters"]][["the.grid"]][["dim"]] #gets the dimension of the grid
          
          obs_box <- ggplotly(
            plot(
              x=som_solution$current_som_solution,
              what='obs',
              type='boxplot',
              variable = tmp.var,
              show.names = TRUE,
              names = paste("Quadrant ", 1:prod(temp.dim))
              )
            )
          
          obs_box <- remove_xaxis_labels(obs_box)
          
          obs_box <- obs_box %>%
            layout(title = "Overview of Variables")
          
          # OBS BARPLOT
          obs_bar <- ggplotly(
              plot(
                x=som_solution$current_som_solution,
                what='obs',
                type='barplot',
                show.names = TRUE,
                names = paste("Quadrant ", 1:prod(temp.dim))
                )
              )
            
          obs_bar <- remove_xaxis_labels(obs_bar)
            
          obs_bar <- obs_bar %>%
            layout(title = "Overview of Variables") 
          
          # PROTOTYPES BAR
          proto_bar <- ggplotly(
              plot(
                x=som_solution$current_som_solution,
                what='prototypes',
                type='barplot'
                )
              )
          
          proto_bar <- remove_xaxis_labels(proto_bar)
          
          proto_bar <- proto_bar %>%
              layout(title = "Overview of Variables")
          
        
      }
      
      if(is.null(agent_cluster_values$agent_cluster_tracker) == FALSE && agent_cluster_values$agent_cluster_tracker@cluster_tested != "None"){
        
        #need to add something here so it only plots the lower bound of data points
        
        agent_cluster_values_data <- agent_cluster_values
        
        # policy_pred_grid <- agent_cluster_values$agent_grid_plot +
        #   geom_point(
        #     aes(
        #       x=agent_cluster_values$agentdf$x,
        #       y=agent_cluster_values$agentdf$y,
        #       color=agent_cluster_values$agentdf$groupnames),
        #     size =4
        #     )+
        #   scale_color_manual(
        #     values = agent_cluster_values$agent_drawtools@plot_colors,
        #     name = "Clusters")+
        #   theme(legend.key = element_blank())
        
        # policy_preds_dir <- file.path(tmpdir, 'scenario_simulations')
        # dir.create(policy_preds_dir) 
        # 
        # write.csv(agent_cluster_values$agent_cluster_tracker@checked_data, file = file.path(policy_preds_dir, "adjust_kmeans.csv"))
        # temp_names <- names(uploaded_data_values$current_data_file)
        # intervention =c()
        # deviation =c()
        # input_var = 1
        # for(i in 1:length(temp_names)){
        #   if(agent_cluster_values$agent_cluster_tracker@sensitivity_test[i] != 0){
        #     intervention =c(intervention, agent_cluster_values$agent_cluster_tracker@sensitivity_test[i])
        #     deviation =c(deviation, input[[paste0("pont.dev", input_var)]])
        #     input_var = input_var + 1
        #   }
        #   else{
        #     intervention = c(intervention, 0)
        #     deviation =c(deviation, 0)
        #   }
        # }
        # tested_changes <- matrix(c(intervention, deviation), nrow = 2, ncol = length(deviation), byrow = TRUE,
        #                          dimnames = list(c("Intervention", "Deviation"), c(temp_names)))
        # write.csv(tested_changes, file = file.path(policy_preds_dir, "tested_intervention.csv"))
        # 
        # sub_sol_space <- c()
        # sub_sol_names <- c()
        # for(i in 1:length(agent_cluster_values$agent_cluster_tracker@sensitivity_result)) {
        #   
        #   if(agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]]> 0){
        #     sub_sol_names <-c(sub_sol_names, i)
        #     sub_sol_space <- c(sub_sol_space, agent_cluster_values$agent_cluster_tracker@sensitivity_result[[i]])
        #   }}
        # 
        # pdf(file.path(policy_preds_dir, "sensitivity.pdf"))
        # print(barplot(sub_sol_space, names.arg = sub_sol_names, main = "Senstivity Analysis Results", xlab  = "Quadrant", col = "yellowgreen"))
        # dev.off()
        # 
        # file_names <- c(file_names,
        #                 paste0('scenario_simulations/', list.files(policy_preds_dir)))
        
      }
      
      # Safely capture the current dataset at download time
      data_obj <- uploaded_data_values$rendered_data
      
      params <- list(
        title      = input$title    %||s% "Untitled Report",
        subtitle   = input$subtitle %||s% "",
        intro      = input$intro    %||s% "",
        
        # Section 1
        include_s1 = isTRUE(input$include_s1),
        s1_title   = input$s1_title %||s% "Section 1",
        s1_body    = input$s1_body  %||s% "",
        data_obj   = if(exists("data_obj")){data_obj}else{NULL},
        
        # Section 2
        include_s2 = isTRUE(input$include_s2),
        s2_title   = input$s2_title %||s% "Section 2",
        s2_body    = input$s2_body  %||s% "",

        # Section 2 Tables
        all_clusters_tbl = if(exists("kcenters")){kcenters}else{NULL},
        cluster_rows_tbl = if(exists("clustered_data")){clustered_data}else{NULL},

        # Section 2 Charts
        s2_jitters = if(exists("all_jitter_plot")){all_jitter_plot}else{NULL},
        s2_violins = if(exists("all_violin_plot")){all_violin_plot}else{NULL},
        s2_histograms = if(exists("all_histogram_plot")){all_histogram_plot}else{NULL},
        s2_silhouettes = if(exists("all_silhouette_plot")){all_silhouette_plot}else{NULL},
        
        # Section 3
        include_s3 = isTRUE(input$include_s3),
        s3_title   = input$s3_title %||s% "Section 3",
        s3_body    = input$s3_body  %||s% "",
        
        # Section 3 prose
        s3_topo_prose = if(exists("SOM_topo_prose")){SOM_topo_prose}else{NULL},
        s3_quant_prose = if(exists("SOM_quant_prose")){SOM_quant_prose}else{NULL},
        s3_deg_free_prose = if(exists("SOM_deg_free_prose")){SOM_deg_free_prose}else{NULL},
        
        # Section 3 ANOVA table
        s3_ANOVA_table = if(exists("anova_results_df")){anova_results_df}else{NULL},
        
        # Section 3 plots
        s3_dendro = if(exists("SOM_SC_plot_data")){SOM_SC_plot_data}else{NULL},
        s3_map = if(exists("SOM_SC_map_data")){SOM_SC_map_data}else{NULL},
        
        # Section 4
        include_s4 = isTRUE(input$include_s4),
        s4_title   = input$s4_title %||s% "Section 4",
        s4_body    = input$s4_body  %||s% "",
        
        # Section 4 plots
        s4_obs_box = if(exists("obs_box")){obs_box}else{NULL},
        s4_obs_bar = if(exists("obs_bar")){obs_bar}else{NULL},
        s4_proto_bar = if(exists("proto_bar")){proto_bar}else{NULL},
        
        # Section 5 grid
        s5_grid = if(exists("policy_pred_grid")){policy_pred_grid}else{NULL}
        
      )
      
      out_dir <- tempdir()
      rmarkdown::render(
        input       = rmd_template_path(),
        output_file = "report.html",
        output_dir  = out_dir,
        params      = params,
        envir       = new.env(parent = globalenv()),
        encoding    = "UTF-8"
      )
      file.copy(file.path(out_dir, "report.html"), file, overwrite = TRUE)
    }
  )
  
  
  
  
  
  
  ############################################################
  
  
  
  
  
  
  
  
  
  observe({
    
    if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
      systems_mapping_values$choices <- c("All", sort(unique(na.omit(kmeans_solution$current_kmeans_solution@uclusters))))
      
    }})

  
  dynamic_nodes <- reactive({
    if(is.null(uploaded_data_values$current_data_file)) {
      return(NULL)
    } else {
      dynamic_nodes <- data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                                  label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
      dynamic_nodes_ids_values$dynamic_nodes_ids <-  setNames(dynamic_nodes$id, dynamic_nodes$label)
      
    }
  })
  
  ##############################################
  
  examine_nodes_dropdown <- reactive({
    if(is.null(uploaded_data_values$current_data_file)) {
      return(NULL)
    } else {
      
      data.frame(id = seq_along(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]),
                 label = colnames(uploaded_data_values$current_data_file[,1:ncol(uploaded_data_values$current_data_file)]))
      
    }
  })
  
  
  observeEvent(input$initialise_button, {
    
    if(is.null(uploaded_data_values$current_data_file)){output$network_warning <- renderText({"Please upload data first."})
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
      
      updateSelectInput(session = session, inputId = "chosen_node", choices = systems_mapping_values$examine_nodes_dropdown_ids)
      
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

  
  
  observeEvent(input$init_kmeans,
               
               {
                 
                 observe({
                   
                   if (kmeans_count$value > 0 && !is.null(kmeans_solution$current_kmeans_solution)) {
                     
                     choices_df <- data.frame(clusters = kmeans_solution$current_kmeans_solution@uclusters)
                     choices_value_counts <- table(choices_df$clusters)
                     choices_filtered_values <- as.numeric(names(choices_value_counts[choices_value_counts > 4]))
                     systems_mapping_values$choices <- c("All", sort(choices_filtered_values))
                     
                   }})
                 
                 observe({
                   updateSelectInput(session = session, inputId = "cluster", choices = systems_mapping_values$choices)
                   updateSelectInput(session = session, inputId = "k_means_cluster", choices = systems_mapping_values$choices)
                 })
                 
                 
               })

  
}  

