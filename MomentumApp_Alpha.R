#1 Name-------------------------------------------------------------------------
#Leon Friebel
#Initiation: 07/01/2024
#Last edit: 07/19/2024

#UI-----------------------------------------------------------------------------
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(title = "Momentum Analysis"),
  dashboardSidebar(
    sidebarMenu(
      id = "pages",
      menuItem("Historic Game Analysis", tabName = "charts", icon = icon("chart-line")),
      menuItem("Dickinson Game Analysis", tabName = "dickinson", icon = icon("database")),
      menuItem("Live Data Entry", tabName = "live", icon = icon("clock")),
      menuItem("Game Plan", tabName = "plan", icon = icon("table")),
      menuItem("Formation Averages", tabName = "formation", icon = icon("chart-bar"))
    )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .fixed-table {
          height: 500px; 
          overflow-y: scroll;
          overflow-x: auto; 
        }
      "))
    ),
    tabItems(
      tabItem(
        tabName = "charts",
        fluidRow(
          column(3, selectInput("year", "Select Season", choices = 1999:2024)),
          column(3, selectInput("ID", "Game ID", choices = NULL)),  # Initialize with NULL choices
          column(3, actionButton("update", "Run Momentum Analysis")) 
        ),
        fluidRow(
          textOutput("message")
        ),
        fluidRow(
          plotOutput("momentumPlot")
        ),
        fluidRow(
          tableOutput("average")
        ),
        fluidRow(
          div(class = "fixed-table", tableOutput("contents"))
        ),
        fluidRow(
          plotOutput("adj_momentumPlot")
        )
      ),
      tabItem(
        tabName = "dickinson",
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              selectInput("id", "Game ID", choices = NULL),  # Initialize with NULL choices
              actionButton("update", "Run Test")
            ),
            mainPanel(
              plotOutput("DickinsonMomentumPlot"),
              tableOutput("DickinsonContents"),
              textOutput("DickinsonMessage")
            )
          )
        )
      ),
      tabItem(
        tabName = "live",
        fluidPage(
          titlePanel("Live Data Entry"),
          fluidRow(
            column(1, selectInput("Ball", "Possession", choices = c("Home", "Away", NA))),
            column(1, selectInput("DN", "Down", choices = c(1, 2, 3, 4, 0))),
            column(1, numericInput("DIST", "To Go", value = 0)),
            column(2, selectInput("PLAY.TYPE", "Play Type", choices = c("Pass", "Run", "Punt", "Punt Rec",
                                                                        "Fake Punt", "KO Rec", "KO", "Onside Kick Rec",
                                                                        "Extra Pt.","Extra Pt. Block",
                                                                        "FG","FG Block", "Fake FG", "2 Pt.", "2 Pt. Defend", NA))),
            column(2, uiOutput("result_ui")),
            column(1, numericInput("GN.LS", "Gain/Loss", value = 0)),
            column(1, selectInput("H", "Hash", choices = c("R", "M", "RP", "L", "LP", NA))),
            column(1, selectInput("ODK", "ODK", choices = c("O", "D", "K"))),
            column(1, numericInput("YARD.LN", "Yard Line", value = NA)),
            column(1, selectInput("QTR", "Quarter", choices = c(1, 2, 3, 4, 5)))
          ),
          fluidRow(
            column(2, textInput("PERSONNEL", "Personnel", value = NA)),
            column(2, textInput("BACKFIELD", "Backfield", value = NA)),
            column(2, textInput("OFF.FORM", "Formation", value = NA)),
            column(2, textInput("MOTION", "Motion", value = NA)),
            column(2, textInput("PROTECTION", "Protection", value = NA)),
            column(2, textInput("OFF.PLAY", "Play", value = NA))
          ),
          fluidRow(
            column(2, actionButton("add_btn", "Add Data")),
            column(2, actionButton("delete_btn", "Delete Latest Row")),
            column(2, actionButton("run_btn", "Run Analysis")),
            column(2, downloadButton("downloadData", "Download Data"))
          ),
          mainPanel(
            plotOutput("live_momentumPlot"),
            tableOutput("live_data_table"),
            verbatimTextOutput("live_analysis_summary")
          )
        )
      ),
      tabItem(
        tabName = "plan",
        fluidPage(
          titlePanel("Upload and Combine Datasets"),
          sidebarLayout(
            sidebarPanel(
              textInput("home_team", "Home Team", value = ""),
              textInput("away_team", "Away Team", value = ""),
              fileInput("files", "Upload CSV Files", multiple = TRUE, accept = c(".xlsx")),
              uiOutput("select_datasets"),
              actionButton("combine", "Combine Selected Datasets"),
              downloadButton("download_combined", "Download Combined Dataset")
            ),
            mainPanel(
              tableOutput("combined_data")
            )
          )
        )
      ),
      tabItem(
        tabName = "formation",
        fluidPage(
          titlePanel("Formation/Play Averages and Momentum"),
          sidebarLayout(
            sidebarPanel(
              fileInput("file1", "Choose CSV File", accept = c(".csv")),
              tags$hr(),
              checkboxInput("header", "Header", TRUE),
              width = 3,
              textInput("team", "Opponent", value = ""),
              selectInput("type", "Type", choices = c("OFF.FORM", "OFF.PLAY"))
            ),
            mainPanel(
              tabsetPanel(
                tabPanel("1st Down", dataTableOutput("table_down1")),
                tabPanel("2nd Down", dataTableOutput("table_down2")),
                tabPanel("3rd Down", dataTableOutput("table_down3")),
                tabPanel("4th Down", dataTableOutput("table_down4"))
              )
            )
          )
        )
      )
    )
  )
)



#Server logic-----------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive expression to read the selected dataset
  dataset <- reactive({
    year <- input$year
    file_path <- paste0("play_by_play_", year, ".csv")
    if (file.exists(file_path)) {
      read.csv(file_path)
    } else {
      NULL
    }
  })
  
  # Update game ID choices based on the selected dataset
  observe({
    df <- dataset()
    if (!is.null(df)) {
      unique_ids <- unique(df$game_id)
      updateSelectInput(session, "ID", choices = unique_ids)
    } else {
      updateSelectInput(session, "ID", choices = NULL)
    }
  })
  
  # Handle the action button click for historic game analysis
  observeEvent(input$update, {
    df <- dataset()
    ID <- input$ID
    
    tryCatch({
      if (!is.null(df) && !is.null(ID)) {
        trial <- Test(df, ID)  # Assuming Test is a function that processes the data
        
        output$message <- renderText({
          paste("Momentum Charts for Game ID:", ID)
        })
        
        output$momentumPlot <- renderPlot({
          ggplot_obj <- MomentumVisuals(trial)  # Assuming MomentumVisuals is a function that returns a ggplot object
          ggplot_obj + theme_classic()
        })
        
        t <- trial %>%
          select(PlayNumber, home_team, away_team, qtr, time, yrdln, down, ydstogo, desc)
        output$contents <- renderTable({
          t
        })
        
        # Calculate means
        aH <- mean(trial$MomentumHome)
        aA <- mean(trial$MomentumAway)
        
        # Create a dataframe with the calculated means
        df <- data.frame(Home_Average = aH, Away_Average = aA)
        
        # Render the dataframe as a table in the UI
        output$average <- renderTable({
          df
        })
      } else {
        output$message <- renderText({
          "Data or Game ID not available."
        })
        
        output$momentumPlot <- renderPlot({
          ggplot()  # Empty plot to avoid NULL rendering
        })
        
        output$adj_momentumPlot <- renderPlot({
          ggplot()  # Empty plot to avoid NULL rendering
        })
        
        output$contents <- renderTable({
          NULL
        })
      }
    }, error = function(e) {
      output$DickinsonMessage <- renderText({
        paste("Error:", e$message)
      })
      
      output$DickinsonContents <- renderTable({
        NULL
      })
      
      output$DickinsonMomentumPlot <- renderPlot({
        ggplot()  # Empty plot to avoid NULL rendering
      })
    })
  })
  
  
  # Dickenson analysis
  dataset_dickinson <- reactive({
    file_path <- "dickinson_2023.csv"  # Adjust path as needed
    read.csv(file_path)
  })
  
  observe({
    data <- dataset_dickinson()
    unique_ids <- unique(data$game_ID)
    updateSelectInput(session, "id", choices = unique_ids)
  })
  
  observeEvent(input$update, {
    data <- dataset_dickinson()
    ID <- input$id
    
    tryCatch({
      trial <- Dickinson.Test(data, ID)
      t <- Live.DataClean(trial)
      Short_Analysis <- Live.s_analysis(trial)
      
      output$DickinsonMessage <- renderText({
        paste("Momentum Chart for", ID)
      })
      
      output$DickinsonContents <- renderTable({
        Short_Analysis
      })
      
      output$DickinsonMomentumPlot <- renderPlot({
        ggplot_obj <- Live.MomentumVisuals(trial)
        ggplot_obj + theme_classic()
      })
    }, error = function(e) {
      output$DickinsonMessage <- renderText({
        "Game not available"
      })
      
      output$DickinsonContents <- renderTable({
        NULL
      })
      
      output$DickinsonMomentumPlot <- renderPlot({
        NULL
      })
    })
  })
  
  # Initialize reactive Values for live_data
  live_data <- reactiveValues(data = data.frame(
    Ball = character(),
    DN = integer(),
    DIST = numeric(),
    PLAY.TYPE = character(),
    RESULT = character(),
    GN.LS = numeric(),
    H = character(),
    ODK = character(),
    YARD.LN = numeric(),
    PERSONNEL = character(),
    BACKFIELD = character(),
    OFF.FORM = character(),
    MOTION = character(),
    PROTECTION = character(),
    OFF.PLAY = character(),
    QTR = integer(),
    stringsAsFactors = FALSE
  ))
  
  # Observe event for add button
  observeEvent(input$add_btn, {
    new_row <- data.frame(
      Play.. = nrow(live_data$data) + 1,
      Ball = input$Ball,
      DN = input$DN,
      DIST = input$DIST,
      PLAY.TYPE = input$PLAY.TYPE,
      RESULT = input$RESULT,
      GN.LS = input$GN.LS,
      H = input$H,
      ODK = input$ODK,
      YARD.LN = input$YARD.LN,
      PERSONNEL = input$PERSONNEL,
      BACKFIELD = input$BACKFIELD,
      OFF.FORM = input$OFF.FORM,
      MOTION = input$MOTION,
      PROTECTION = input$PROTECTION,
      OFF.PLAY = input$OFF.PLAY,
      QTR = as.numeric(input$QTR),

      stringsAsFactors = FALSE
    )
    
    live_data$data <- rbind(live_data$data, new_row)
    
    # Recalculate quarter_end based on changes in QTR
  })
  
  # Observe event for delete button
  observeEvent(input$delete_btn, {
    if (!is.null(live_data$data) && nrow(live_data$data) > 0) {
      live_data$data <- live_data$data[-nrow(live_data$data), , drop = FALSE]
    }
  })
  
  # Render the live data table
  output$live_data_table <- renderTable({
    live_data$data
  })
  
  # Render the analysis summary
  observeEvent(input$run_btn, {
    output$live_analysis_summary <- renderPrint({
      if (nrow(live_data$data) >= 2) {
        t <- Live.Test(live_data$data)
        t1 <- Live.DataClean(t)
        
        output$message <- renderText({
          paste("Momentum Charts for Game ID:", ID)
        })
        
        output$live_momentumPlot <- renderPlot({
          ggplot_obj <- Live.MomentumVisuals(t1)
          ggplot_obj
        })
        
      } else {
        "Add more data to see analysis."
      }
    })
  })
  
  # Logic for dynamically updating RESULT input choices based on PLAY.TYPE
  play_type_results <- list(
    "Pass" = c("Complete", "Incomplete", "Interception", "Scramble", "Sack", "TD", "Safety", "Complete, Fumble", "Dropped"),
    "Run" = c("Rush", "Rush, TD", "Fumble", "Scramble, TD", "Safety"),
    "Punt" = c("Punt", "Return", "Block", "Touchback", "Downed", "Out of Bounds"),
    "Punt Rec" = c("Return", "Fair Catch", "Fumble"),
    "Fake Punt" = c("Rush", "Pass"),
    "KO Rec" = c("Return", "Fumble"),
    "KO" = c("Kickoff", "Touchback", "Out of Bounds"),
    "Onside Kick Rec" = c("Recovery", "Touchback"),
    "Extra Pt." = c("Good", "No Good", "Blocked"),
    "Extra Pt. Block" = c("Blocked", "Return"),
    "FG" = c("Good", "No Good", "Blocked"),
    "FG Block" = c("Blocked", "Return"),
    "Fake FG" = c("Rush", "Pass"),
    "2 Pt." = c("Good", "No Good"),
    "2 Pt. Defend" = c("Stopped", "Return")
  )
  
  output$result_ui <- renderUI({
    selectInput("RESULT", "Result", choices = play_type_results[[input$PLAY.TYPE]])
  })
  
  # Download handler for the data
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("live_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(live_data$data, file, row.names = FALSE)
    }
  )
  
  # Reactive values to store datasets
  datasets <- reactiveValues(data = list())
  combined_data <- reactiveVal(NULL)
  
  # Observe file upload and read data
  observeEvent(input$files, {
    req(input$files)
    for (i in 1:nrow(input$files)) {
      file <- input$files[i, ]
      
      # Check if the file is an Excel file based on its extension
      if (grepl("\\.xlsx$", file$name)) {
        # Read the Excel file
        dataset <- read_excel(file$datapath)
        
        # Save it as a CSV file temporarily
        temp_csv_file <- tempfile(fileext = ".csv")
        write.csv(dataset, temp_csv_file, row.names = FALSE)
        
        # Read the CSV file
        dataset <- read.csv(temp_csv_file)
      } else {
        # Read the CSV file directly
        dataset <- read.csv(file$datapath)
      }
      
      # Get home and away teams input
      home_team <- input$home_team
      away_team <- input$away_team
      
      # Add data using the addData function
      dataset <- addData(dataset, home_team, away_team)
      dataset <- Scout.situational(dataset)
      
      # Store the dataset
      datasets$data[[file$name]] <- dataset
    }
  })
  
  # Dynamically create UI for selecting datasets to combine
  output$select_datasets <- renderUI({
    req(datasets$data)
    checkboxGroupInput("datasets_to_combine", "Select Datasets to Combine",
                       choices = names(datasets$data))
  })
  
  # Observe combine button click
  observeEvent(input$combine, {
    req(input$datasets_to_combine)
    selected_data <- lapply(input$datasets_to_combine, function(name) {
      datasets$data[[name]]
    })
    combined_data_list <- do.call(rbind, selected_data)
    combined_data(combined_data_list)
  })
  
  # Render combined dataset table
  output$combined_data <- renderTable({
    combined_data()
  })
  
  # Download handler for combined dataset
  output$download_combined <- downloadHandler(
    filename = function() {
      "combined_dataset.csv"
    },
    content = function(file) {
      write.csv(combined_data(), file, row.names = FALSE)
    }
  )
  
  # Averages calculation and rendering
  data <- reactive({
    req(input$file1)
    
    df <- read.csv(input$file1$datapath, header = input$header)
    
    return(df)
  })
  
  # Reactive function to calculate formation averages
  down_dfs <- reactive({
    df <- data()
    
    if (is.null(df)) {
      return(NULL)
    }
    
    # Get team input
    team <- input$team
    type <- input$type
    if (type == "OFF.FORM") {
      formationAverages(df, team)
    } else if (type == "OFF.PLAY") {
      playAverages(df, team)
    }
    
  })
  
  # Render tables for each down
  output$table_down1 <- renderDataTable({
    df <- down_dfs()[[1]]
    datatable(df, options = list(paging = FALSE))
  })
  
  output$table_down2 <- renderDataTable({
    df <- down_dfs()[[2]]
    datatable(df, options = list(paging = FALSE))
  })
  
  output$table_down3 <- renderDataTable({
    df <- down_dfs()[[3]]
    datatable(df, options = list(paging = FALSE))
  })
  
  output$table_down4 <- renderDataTable({
    df <- down_dfs()[[4]]
    datatable(df, options = list(paging = FALSE))
  })
  
}


## Run the App------------------------------------------------------------------

shinyApp(ui, server)

