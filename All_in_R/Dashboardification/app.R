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
  
  tabsetPanel(
    
    tabPanel(
      "Feasibility Boundaries",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "bounds_dining_hall_label",
            "Dining hall",
            choices = names(dining_hall_lookup),
            selected = "Crossroads"
          ),
          
          numericInput(
            "bounds_lower_multiplier",
            "Lower multiplier",
            value = 0.5,
            min = 0,
            max = 1,
            step = 0.05
          ),
          
          numericInput(
            "bounds_upper_multiplier",
            "Upper multiplier",
            value = 1.5,
            min = 1,
            max = 3,
            step = 0.05
          ),
          
          actionButton("run_bounds", "Run Scenarios 1 & 2")
        ),
        
        mainPanel(
          h3("Boundary Summary"),
          DTOutput("bounds_summary_table"),
          
          h3("Scenario 1: Max Cost Reduction"),
          plotOutput("s1_category_plot", height = "450px"),
          plotOutput("s1_spend_plot", height = "500px"),
          
          h3("Scenario 2: Max Sustainable Spend"),
          plotOutput("s2_category_plot", height = "450px"),
          plotOutput("s2_spend_plot", height = "500px")
        )
      )
    ),
    
    tabPanel(
      "Custom Cost Reduction Scenario",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "custom_dining_hall_label",
            "Dining hall",
            choices = names(dining_hall_lookup),
            selected = "Crossroads"
          ),
          
          numericInput(
            "custom_cost_reduction_target",
            "Cost reduction target",
            value = 0.07,
            min = 0,
            max = 0.5,
            step = 0.01
          ),
          
          numericInput(
            "custom_lower_multiplier",
            "Lower multiplier",
            value = 0.5,
            min = 0,
            max = 1,
            step = 0.05
          ),
          
          numericInput(
            "custom_upper_multiplier",
            "Upper multiplier",
            value = 1.5,
            min = 1,
            max = 3,
            step = 0.05
          ),
          
          actionButton("run_custom", "Run Scenario 3")
        ),
        
        mainPanel(
          h3("Scenario 3 Summary"),
          DTOutput("custom_summary_table"),
          
          h3("Category Frequency"),
          DTOutput("custom_category_table"),
          
          h3("Category Frequency Plot"),
          plotOutput("custom_category_plot", height = "500px"),
          
          h3("Sustainable vs. Conventional Spend Plot"),
          plotOutput("custom_spend_plot", height = "550px")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  
  bounds_results <- eventReactive(input$run_bounds, {
    dining_hall_name <- dining_hall_lookup[[input$bounds_dining_hall_label]]
    
    tryCatch(
      {
        s1 <- run_dashboard_scenario(
          dining_hall_name = dining_hall_name,
          dining_hall_label = input$bounds_dining_hall_label,
          scenario = "s1",
          lower_multiplier = input$bounds_lower_multiplier,
          upper_multiplier = input$bounds_upper_multiplier,
          data_dir = "../Basic_Data"
        )
        
        s2 <- run_dashboard_scenario(
          dining_hall_name = dining_hall_name,
          dining_hall_label = input$bounds_dining_hall_label,
          scenario = "s2",
          lower_multiplier = input$bounds_lower_multiplier,
          upper_multiplier = input$bounds_upper_multiplier,
          data_dir = "../Basic_Data"
        )
        
        list(s1 = s1, s2 = s2)
      },
      error = function(e) {
        showNotification(
          paste("Boundary optimization failed:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )
  })
  
  custom_results <- eventReactive(input$run_custom, {
    dining_hall_name <- dining_hall_lookup[[input$custom_dining_hall_label]]
    
    tryCatch(
      {
        run_dashboard_scenario(
          dining_hall_name = dining_hall_name,
          dining_hall_label = input$custom_dining_hall_label,
          scenario = "s3",
          cost_reduction_target = input$custom_cost_reduction_target,
          lower_multiplier = input$custom_lower_multiplier,
          upper_multiplier = input$custom_upper_multiplier,
          data_dir = "../Basic_Data"
        )
      },
      error = function(e) {
        showNotification(
          paste("Scenario 3 failed:", e$message),
          type = "error",
          duration = 10
        )
        NULL
      }
    )
  })
  
  output$bounds_summary_table <- renderDT({
    req(bounds_results())
    
    bind_rows(
      bounds_results()$s1$scenario_summary,
      bounds_results()$s2$scenario_summary
    ) %>%
      distinct(scenario, .keep_all = TRUE) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$s1_category_plot <- renderPlot({
    req(bounds_results())
    scenario_name <- bounds_results()$s1$scenario_summary$scenario[2]
    bounds_results()$s1$plots[[scenario_name]]$category_frequency
  })
  
  output$s1_spend_plot <- renderPlot({
    req(bounds_results())
    scenario_name <- bounds_results()$s1$scenario_summary$scenario[2]
    bounds_results()$s1$plots[[scenario_name]]$sus_vs_conv_spend
  })
  
  output$s2_category_plot <- renderPlot({
    req(bounds_results())
    scenario_name <- bounds_results()$s2$scenario_summary$scenario[2]
    bounds_results()$s2$plots[[scenario_name]]$category_frequency
  })
  
  output$s2_spend_plot <- renderPlot({
    req(bounds_results())
    scenario_name <- bounds_results()$s2$scenario_summary$scenario[2]
    bounds_results()$s2$plots[[scenario_name]]$sus_vs_conv_spend
  })
  
  output$custom_summary_table <- renderDT({
    req(custom_results())
    
    datatable(
      custom_results()$scenario_summary,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$custom_category_table <- renderDT({
    req(custom_results())
    
    scenario_name <- custom_results()$scenario_summary$scenario[2]
    
    custom_results()$category_frequency %>%
      filter(scenario == scenario_name) %>%
      datatable(options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$custom_category_plot <- renderPlot({
    req(custom_results())
    
    scenario_name <- custom_results()$scenario_summary$scenario[2]
    
    custom_results()$plots[[scenario_name]]$category_frequency
  })
  
  output$custom_spend_plot <- renderPlot({
    req(custom_results())
    
    scenario_name <- custom_results()$scenario_summary$scenario[2]
    
    custom_results()$plots[[scenario_name]]$sus_vs_conv_spend
  })
}

shinyApp(ui, server)