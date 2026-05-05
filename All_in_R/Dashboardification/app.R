library(shiny)
library(dplyr)
library(DT)

source("optimization_backend.R")

dining_hall_lookup <- c(
  "Cafe 3" = "C3",
  "Crossroads" = "XRDS",
  "Clark Kerr" = "CKC"
)

ui <- fluidPage(
  titlePanel("Protein Frequency Optimization Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "dining_hall_label",
        "Dining hall",
        choices = names(dining_hall_lookup),
        selected = "Crossroads"
      ),
      
      selectInput(
        "scenario",
        "Scenario",
        choices = c(
          "Scenario 1: Max cost reduction while maintaining sustainability" = "s1",
          "Scenario 2: Max sustainability without increasing cost" = "s2",
          "Scenario 3: Custom cost reduction, then max sustainability" = "s3"
        ),
        selected = "s3"
      ),
      
      conditionalPanel(
        condition = "input.scenario == 's3'",
        numericInput(
          "cost_reduction_target",
          "Scenario 3 cost reduction target",
          value = 0.07,
          min = 0,
          max = 0.5,
          step = 0.01
        )
      ),
      
      numericInput(
        "lower_multiplier",
        "Lower multiplier",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05
      ),
      
      numericInput(
        "upper_multiplier",
        "Upper multiplier",
        value = 1.5,
        min = 1,
        max = 3,
        step = 0.05
      ),
      
      actionButton("run", "Run optimization")
    ),
    
    mainPanel(
      h3("Scenario Summary"),
      DTOutput("summary_table"),
      
      h3("Category Frequency"),
      DTOutput("category_table"),
      
      h3("Category Frequency Plot"),
      plotOutput("category_plot", height = "500px"),
      
      h3("Sustainable vs. Conventional Spend Plot"),
      plotOutput("spend_plot", height = "550px")
    )
  )
)

server <- function(input, output, session) {
  
  results <- eventReactive(input$run, {
    
    dining_hall_name <- dining_hall_lookup[[input$dining_hall_label]]
    
    tryCatch(
      {
        run_dashboard_scenario(
          dining_hall_name = dining_hall_name,
          dining_hall_label = input$dining_hall_label,
          scenario = input$scenario,
          cost_reduction_target = input$cost_reduction_target,
          lower_multiplier = input$lower_multiplier,
          upper_multiplier = input$upper_multiplier,
          data_dir = "../Basic_Data"
        )
      },
      error = function(e) {
        showNotification(
          paste("Optimization failed:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )
  })
  
  output$summary_table <- renderDT({
    req(results())
    datatable(
      results()$scenario_summary,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$category_table <- renderDT({
    req(results())
    
    scenario_name <- results()$scenario_summary$scenario[2]
    
    results()$category_frequency %>%
      filter(scenario == scenario_name) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$category_plot <- renderPlot({
    req(results())
    
    scenario_name <- results()$scenario_summary$scenario[2]
    
    results()$plots[[scenario_name]]$category_frequency
  })
  
  output$spend_plot <- renderPlot({
    req(results())
    
    scenario_name <- results()$scenario_summary$scenario[2]
    
    results()$plots[[scenario_name]]$sus_vs_conv_spend
  })
}

shinyApp(ui, server)