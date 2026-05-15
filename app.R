##
## TLS - DATA PROCESSING APP
##
## PURPOSE: 
## Allow users to upload individual files for Cartesian coordinates taken from
## the different TLS positions (each file will be in its own coordinate system)
## and convert those individual files into the single 'TLS data' file format
## that is required by the actual TLS IPA Data Analysis Webapp
##
library(shiny)
library(shinythemes)
library(shinyjs)     ## allows a reset button

## define where to get functions from
source('R/utils.R')  

library(shiny)

# Define UI
ui <- fluidPage(theme = shinytheme("spacelab"),
                useShinyjs(), # Initialize shinyjs

    # TITLE
    titlePanel("TLS - Data Processing Tool"),

    # 
    ## PREPARE SPACE FOR THE APP INPUTS
    sidebarLayout(
        sidebarPanel(
          fileInput('f1', "1. Upload Cartesian coordinates from Position 1", accept = c(".csv", ".txt")),
          fileInput('f2', "2. Upload Cartesian coordinates from Position 2", accept = c(".csv", ".txt")), 
          fileInput('f3', "3. Upload Cartesian coordinates from Position 3", accept = c(".csv", ".txt")), 
          fileInput('f4', "4. Upload Cartesian coordinates from Position 4", accept = c(".csv", ".txt")),
          ## do the files include a header?
          selectInput('file_header', "Do the files contain headers?", choices = c('No', 'Yes')),
          ## What naming scheme does the user want to use?
          textInput(inputId = "target_name",
                    label = "Default Target Prefix:",
                    value = "Target_"),
          uiOutput("pre_process_error"),
          fluidRow(
            column(6, 
                   disabled(actionButton('runAnalysis','Process Data', class = "btn-primary"))
                   ),
            column(6, 
                   align = "right", 
                   actionButton('reset_btn', 'Reset App', class = "btn-danger")
                   )
          )
        ),

        # 
        ## PREPARE SPACE FOR THE APP OUTPUT
        mainPanel(
          htmlOutput("main_error_ui"),      ## a place where errors will show up
          textOutput("target_statement"),
          h3("Processed Data"),
          DT::dataTableOutput("data_table"), 
          br(),
          uiOutput("download_ui")            ## placeholder for download button
          #downloadButton("download_data", "Download Processed Data", class = "btn-success")
        )
    )
)


##
## SERVER LOGIC
## 
server <- function(input, output, session) {
  
  # When the reset button is clicked, refresh the entire page
  observeEvent(input$reset_btn, {
    shinyjs::runjs("history.go(0);") 
  })
  
  # Reactive to check for duplicate names instantly
  filename_error <- reactive({
    files <- list(input$f1, input$f2, input$f3, input$f4)
    check_unique_filenames(files)
  })
  
  # Logic to enable/disable the Process button
  observe({
    # Condition A: All 4 files are uploaded
    all_present <- !is.null(input$f1) & !is.null(input$f2) & 
      !is.null(input$f3) & !is.null(input$f4)
    
    # Condition B: No duplicate filenames
    is_unique <- is.null(filename_error())
    
    if (all_present && is_unique) {
      shinyjs::enable("runAnalysis")
    } else {
      shinyjs::disable("runAnalysis")
    }
  })


  ## Check that all four files have unique names
  ## ...
  ## Determine if the files have headers
  header_bool <- reactive({
    # This will update all dependent reactives whenever input$file_header changes
    input$file_header != "No"
  })
  
  ## Functions that will import the four data files
  get_dat1 = reactive({
    if(is.null(input$f1)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$f1$name)
      dat1 = switch(ext, csv = read.csv(input$f1$datapath, header = header_bool()), 
                         txt = read.table(input$f1$datapath, header = header_bool()), 
                         validate("Invalid file; Please upload a .csv or .txt file"))
      return(dat1)
    }
  })
  get_dat2 = reactive({
    if(is.null(input$f2)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$f2$name)
      dat2 = switch(ext, csv = read.csv(input$f2$datapath, header = header_bool()), 
                    txt = read.table(input$f2$datapath, header = header_bool()), 
                    validate("Invalid file; Please upload a .csv or .txt file"))
      return(dat2)
    }
  })
  get_dat3 = reactive({
    if(is.null(input$f3)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$f3$name)
      dat3 = switch(ext, csv = read.csv(input$f3$datapath, header = header_bool()), 
                    txt = read.table(input$f3$datapath, header = header_bool()), 
                    validate("Invalid file; Please upload a .csv or .txt file"))
      return(dat3)
    }
  })
  get_dat4 = reactive({
    if(is.null(input$f4)) {
      return(NULL)
    } else {
      ext = tools::file_ext(input$f4$name)
      dat4 = switch(ext, csv = read.csv(input$f4$datapath, header = header_bool()), 
                    txt = read.table(input$f4$datapath, header = header_bool()), 
                    validate("Invalid file; Please upload a .csv or .txt file"))
      return(dat4)
    }
  })
  
  # Error message:
  # Watch the trigger instead of the list
  error_message <- eventReactive(input$runAnalysis, {

    req(input$f1, input$f2, input$f3, input$f4)
    
    data_list <- list(get_dat1(), get_dat2(), get_dat3(), get_dat4())
    check_data_integrity(data_list, input$file_header)
  })

  
  
  ## 
  ## NOW DO STUFF, WHEN THE BUTTON IS CLICKED
  ##
  all_results <- eventReactive(input$runAnalysis, {

    # Otherwise, proceed with checking if there's an error. 
    # We call the validation inside here too, or check the error_message()
    msg <- check_data_integrity(list(get_dat1(), get_dat2(), get_dat3(), get_dat4()), input$file_header)
    
    # If there's an error, stop and don't return results
    req(is.null(msg))
    
    ## initialize some storage
    results_out = list()
    
    #browser()
    
    ## Read in the data
    results_out$dat1 = get_dat1()
    results_out$dat2 = get_dat2()
    results_out$dat3 = get_dat3()
    results_out$dat4 = get_dat4()
    
    ## 
    ## Check that the data are acceptable
    ##

    ## Read in the (potentially user-specified) target naming scheme
    results_out$target_name <- input$target_name
    
    ##
    ## DO STUFF WITH THE DATA
    ## 
    ## Convert the four files to the unified named 'Zc' format 
    ## (use the Target-Distance process to reorder files 2-4 to 
    ##  correspond with the file 1 target ordering)
    ##
    results_out$processed_data <- process_tls_data(results_out$dat1, results_out$dat2, 
                                                   results_out$dat3, results_out$dat4, name_scheme = input$target_name)
    
  
    ## Statement about how many targets are in the data set
    results_out$target_statement <- paste0("The processed data contains coordinates for ", nrow(results_out$processed_data), " targets.")
    
    return(results_out)
  })
  
  ##
  ## NOW THAT ALL THE CALCULATIONS ARE DONE, PREP OUTPUT FOR APP RENDERING
  ##
  ## 
  # Render a warning message that shows up BEFORE clicking process
  output$pre_process_error <- renderUI({
    msg <- filename_error()
    if (is.null(msg)) return(NULL)
    
    div(style = "color: #a94442; background-color: #f2dede; padding: 10px; border: 1px solid #ebccd1; border-radius: 4px; margin-bottom: 10px;",
        icon("exclamation-circle"), msg)
  })
  
  # Box that gives error if something is wrong with the data (e.g., NA values; not all files have the same targets)
  output$main_error_ui <- renderUI({
    # This will be silent until the first time the button is clicked
    msg <- error_message()
    
    if (is.null(msg)) return(NULL)
    
    div(class = "alert alert-danger", role = "alert",
        icon("exclamation-triangle"), 
        span(style="margin-left: 10px; font-weight: bold;", msg))
  })
  
  ## The processed data
  output$data_table <- DT::renderDataTable({
    all_results()$processed_data
  }, options = list(
    searching = FALSE, 
    ordering = FALSE, 
    paging = FALSE,
    scrollY = "400px",      # Enables vertical scrolling at a fixed height
    scrollCollapse = TRUE
  ))
  
  ## The statement on the number of targets
  output$target_statement <- renderText({
    # Ensure the statement exists before trying to render it
    req(all_results()$target_statement) 
    all_results()$target_statement
  })
  
  ## Stuff to download the data
  output$download_data <- downloadHandler(
    filename = function() {
      # Dynamically name the file with the current date
      paste0("processed_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # This ensures results_out$processed_data is not NULL before writing
      req(all_results()$processed_data)
      # 'file' is a temporary path automatically created by Shiny.
      # We write our reactive data to that path.
      write.csv(all_results()$processed_data, file, row.names = FALSE)
    }
  )
  
  ##
  ## Download button
  ##
  output$download_ui <- renderUI({
    # Only show the button if the data exists
    if (!is.null(all_results()$processed_data)) {
      downloadButton("download_data", "Download Processed Data", class = "btn-success")
    }
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
