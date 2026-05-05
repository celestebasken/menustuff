library(shiny)
library(dplyr)
library(DT)

source("optimization_backend.R")

dining_hall_lookup <- c(
  "Cafe 3" = "C3",
  "Crossroads" = "XRDS",
  "Clark Kerr" = "CKC"
)

make_dashboard_narrative <- function(results_obj, dining_hall_label) {
  s <- results_obj$scenario_summary
  baseline <- s %>% filter(scenario == "baseline")
  scen <- s %>% filter(scenario != "baseline") %>% slice(1)
  
  scenario_name <- scen$scenario
  
  cat_tbl <- results_obj$category_frequency %>%
    filter(scenario == scenario_name)
  
  biggest_increase <- cat_tbl %>%
    filter(pct_change_meals == max(pct_change_meals, na.rm = TRUE)) %>%
    slice(1)
  
  biggest_decrease <- cat_tbl %>%
    filter(pct_change_meals == min(pct_change_meals, na.rm = TRUE)) %>%
    slice(1)
  
  HTML(paste0(
    "<p><strong>This optimization for ", dining_hall_label, "</strong></p>",
    "<p>",
    "Reduces costs <strong>", round(abs(scen$cost_pct_change), 1), "%</strong> relative to baseline.<br>",
    "Increases sustainable spend from <strong>", round(100 * baseline$sus_pct, 1), "%</strong> to <strong>",
    round(100 * scen$sus_pct, 1), "%</strong> of total spend.<br>",
    "Total greenhouse gas equivalents change by <strong>", round(scen$ghg_pct_change, 1), "%</strong>.<br>",
    "The largest category increase is <strong>", biggest_increase$category, " (",
    round(biggest_increase$pct_change_meals, 1), "%)</strong>, while the largest decrease is <strong>",
    biggest_decrease$category, " (", round(biggest_decrease$pct_change_meals, 1), "%)</strong>.",
    "</p>"
  ))
}

make_hypothetical_assumption_text <- function(results_obj) {
  assumptions <- results_obj$hypothetical_assumptions
  scenario_name <- results_obj$scenario_summary$scenario[2]
  
  ingredient_tbl <- results_obj$ingredient_details[[scenario_name]]
  
  recommended_frequency <- ingredient_tbl %>%
    filter(ingredient == assumptions$hypothetical_name) %>%
    pull(freq_optimized)
  
  if (length(recommended_frequency) == 0) {
    recommended_frequency <- 0
  }
  
  recommendation_text <- if (recommended_frequency > 0) {
    "This hypothetical was added to the menu, so it is price and sustainability-competitive based on current purchasing."
  } else {
    "This hypothetical was not added to the menu, indicating that it is not price or sustainability-competitive based on current purchasing."
  }
  
  HTML(paste0(
    "<p>",
    "New hypothetical protein: <strong>", assumptions$hypothetical_name, "</strong><br>",
    "Protein category: <strong>", assumptions$category, "</strong><br>",
    "Assumed protein weight in menu items: <strong>",
    round(assumptions$assumed_expected_lb_per_appearance, 2), " lb</strong><br>",
    "Cap: <strong>", assumptions$cap, "</strong><br>",
    "Recommended frequency: <strong>", recommended_frequency, "</strong>",
    "</p>",
    "<h3>Recommendation</h3>",
    "<p>", recommendation_text, "</p>"
  ))
}

make_dashboard_category_table <- function(results_obj) {
  scenario_name <- results_obj$scenario_summary$scenario[2]
  
  results_obj$category_frequency %>%
    filter(scenario == scenario_name) %>%
    transmute(
      Category = category,
      `Baseline % of Meals` = paste0(round(100 * baseline_share, 2), "%"),
      `Optimized % of Meals` = paste0(round(100 * optimized_share, 2), "%"),
      `% Change` = paste0(
        ifelse(pct_change_meals > 0, "+", ""),
        round(pct_change_meals, 2),
        "%"
      )
    )
}

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
          
          h2("Feasibility Boundary Explorer"),
          
          p(
            "This page helps users understand the feasible optimization boundaries for a selected dining hall. 
            Scenario 1 estimates the maximum cost reduction possible while maintaining baseline sustainable spend. 
            Scenario 2 estimates the maximum sustainable spend possible without increasing total cost. 
            Based on these boundaries, on page 2, you can set a Custom Cost Reduction Scenario. 
            You can also adjust the lower and upper multipliers to control how much each protein category is allowed to change from baseline."
          ),
          
          hr(),
          
          h3("Scenario 1: Max Cost Reduction"),
          uiOutput("s1_narrative"),
          DTOutput("s1_deliverable_table"),
          plotOutput("s1_category_plot", height = "450px"),
          plotOutput("s1_spend_plot", height = "500px"),
          
          h3("Scenario 2: Max Sustainable Spend"),
          uiOutput("s2_narrative"),
          DTOutput("s2_deliverable_table"),
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
          h2("Custom Cost Reduction Scenario"),
          
          p(
            "This page allows users to choose a specific cost reduction target and identify the protein frequency mix that maximizes sustainable spend under that constraint. 
  After meeting the selected cost target, the model prioritizes sustainability and then minimizes greenhouse gas equivalents where possible. 
  Users can adjust the lower and upper multipliers to control how much each protein category is allowed to change from baseline."
          ),
          
          hr(),
          
          h3("Optimization Summary"),
          uiOutput("custom_narrative"),
          
          h3("Category Meal Share"),
          DTOutput("custom_deliverable_table"),
          
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
    ),
    tabPanel(
      "Hypothetical Proteins",
      
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "hyp_dining_hall_label",
            "Dining hall",
            choices = names(dining_hall_lookup),
            selected = "Crossroads"
          ),
          
          numericInput(
            "hyp_cost_reduction_target",
            "Cost reduction target",
            value = 0.07,
            min = 0,
            max = 0.5,
            step = 0.01
          ),
          
          numericInput(
            "hyp_lower_multiplier",
            "Lower multiplier",
            value = 0.5,
            min = 0,
            max = 1,
            step = 0.05
          ),
          
          numericInput(
            "hyp_upper_multiplier",
            "Upper multiplier",
            value = 1.5,
            min = 1,
            max = 3,
            step = 0.05
          ),
          
          hr(),
          
          textInput(
            "hyp_name",
            "Hypothetical protein name",
            value = "Hypothetical Tofu"
          ),
          
          selectInput(
            "hyp_category",
            "Protein category",
            choices = c(
              "Beef",
              "Chicken",
              "Fish",
              "Lamb",
              "Pork",
              "Soy",
              "Turkey"
            ),
            selected = "Soy"
          ),
          
          selectInput(
            "hyp_default_sus",
            "Default sustainable?",
            choices = c("Yes", "No"),
            selected = "Yes"
          ),
          
          numericInput(
            "hyp_conventional_price",
            "Conventional price per lb",
            value = 2.50,
            min = 0,
            step = 0.05
          ),
          
          numericInput(
            "hyp_sustainable_price",
            "Sustainable price per lb",
            value = 3.25,
            min = 0,
            step = 0.05
          ),
          
          numericInput(
            "hyp_cap",
            "Maximum appearances allowed",
            value = 3,
            min = 0,
            step = 1
          ),
          
          actionButton("run_hyp", "Run Hypothetical Scenario")
        ),
        
        mainPanel(
          h2("Hypothetical Protein Scenario"),
          
          p(
            "This page allows users to add a hypothetical protein that is not currently purchased by the dining hall. 
        The model keeps total meals fixed, allows the new protein to enter up to the selected cap, and then identifies the menu frequency mix that meets the selected cost reduction target while maximizing sustainable spend and minimizing greenhouse gas equivalents where possible."
          ),
          
          hr(),
          
          h3("Optimization Summary"),
          uiOutput("hyp_narrative"),
          
          h3("Assumptions Used"),
          DTOutput("hyp_assumptions_table"),
          
          h3("Category Meal Share"),
          DTOutput("hyp_deliverable_table"),
          
          h3("Scenario Summary"),
          DTOutput("hyp_summary_table"),
          
          h3("Category Frequency Plot"),
          plotOutput("hyp_category_plot", height = "500px"),
          
          h3("Sustainable vs. Conventional Spend Plot"),
          plotOutput("hyp_spend_plot", height = "550px")
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
  
  hyp_results <- eventReactive(input$run_hyp, {
    dining_hall_name <- dining_hall_lookup[[input$hyp_dining_hall_label]]
    
    tryCatch(
      {
        run_dashboard_hypothetical_scenario(
          dining_hall_name = dining_hall_name,
          dining_hall_label = input$hyp_dining_hall_label,
          hypothetical_name = input$hyp_name,
          hypothetical_category = input$hyp_category,
          default_sus = input$hyp_default_sus,
          conventional_price_lb = input$hyp_conventional_price,
          sustainable_price_lb = input$hyp_sustainable_price,
          hypothetical_cap = input$hyp_cap,
          cost_reduction_target = input$hyp_cost_reduction_target,
          lower_multiplier = input$hyp_lower_multiplier,
          upper_multiplier = input$hyp_upper_multiplier,
          data_dir = "../Basic_Data"
        )
      },
      error = function(e) {
        showNotification(
          paste("Hypothetical scenario failed:", e$message),
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
  
  output$s1_narrative <- renderUI({
    req(bounds_results())
    make_dashboard_narrative(
      bounds_results()$s1,
      input$bounds_dining_hall_label
    )
  })
  
  output$s1_deliverable_table <- renderDT({
    req(bounds_results())
    
    datatable(
      make_dashboard_category_table(bounds_results()$s1),
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
  })
  
  output$s2_narrative <- renderUI({
    req(bounds_results())
    make_dashboard_narrative(
      bounds_results()$s2,
      input$bounds_dining_hall_label
    )
  })
  
  output$s2_deliverable_table <- renderDT({
    req(bounds_results())
    
    datatable(
      make_dashboard_category_table(bounds_results()$s2),
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
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
  
  output$custom_narrative <- renderUI({
    req(custom_results())
    make_dashboard_narrative(custom_results(), input$custom_dining_hall_label)
  })
  
  output$custom_deliverable_table <- renderDT({
    req(custom_results())
    
    datatable(
      make_dashboard_category_table(custom_results()),
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
  })
  
  output$hyp_narrative <- renderUI({
    req(hyp_results())
    make_dashboard_narrative(
      hyp_results(),
      input$hyp_dining_hall_label
    )
  })
  
  output$hyp_deliverable_table <- renderDT({
    req(hyp_results())
    
    datatable(
      make_dashboard_category_table(hyp_results()),
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
  })
  
  output$hyp_summary_table <- renderDT({
    req(hyp_results())
    
    datatable(
      hyp_results()$scenario_summary,
      options = list(pageLength = 5, scrollX = TRUE)
    )
  })
  
  output$hyp_category_plot <- renderPlot({
    req(hyp_results())
    
    scenario_name <- hyp_results()$scenario_summary$scenario[2]
    
    hyp_results()$plots[[scenario_name]]$category_frequency
  })
  
  output$hyp_spend_plot <- renderPlot({
    req(hyp_results())
    
    scenario_name <- hyp_results()$scenario_summary$scenario[2]
    
    hyp_results()$plots[[scenario_name]]$sus_vs_conv_spend
  })
  
  output$hyp_assumptions_table <- renderDT({
    req(hyp_results())
    
    datatable(
      hyp_results()$hypothetical_assumptions,
      rownames = FALSE,
      options = list(
        dom = "t",
        ordering = FALSE
      )
    )
  })
}


shinyApp(ui, server)