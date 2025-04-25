# LLIN Coverage Estimation Calculator in R (Shiny App)

library(shiny)
library(DT)

ui <- fluidPage(
  titlePanel("LLIN Coverage Estimation Calculator"),
  fluidRow(
    column(12,
           wellPanel(
             fluidRow(
               column(4, numericInput("total_nets", "Total Nets Available:", value = 1000, min = 1)),
               column(4,
                      h4("Total Population per Urban Extent:"),
                      tags$ul(
                        tags$li(numericInput("population_50", "50% Urban Extent:", value = 5000, min = 1)),
                        tags$li(numericInput("population_70", "70% Urban Extent:", value = 3000, min = 1))
                      )
               ),
               column(4,
                      h4("Proportion of Households by Size:"),
                      tags$ul(
                        tags$li(numericInput("hh1_2", "1-2 People:", value = 0.3, min = 0, max = 1, step = 0.01)),
                        tags$li(numericInput("hh3_4", "3-4 People:", value = 0.4, min = 0, max = 1, step = 0.01)),
                        tags$li(numericInput("hh5plus", "5+ People:", value = 0.3, min = 0, max = 1, step = 0.01))
                      ),
                      helpText("Note: Proportions will be automatically normalized to sum to 1.")
               )
             ),
             downloadButton("download_combined_csv", "Download Combined CSV")
           )
    )
  ),
  fluidRow(
    column(6, h4("Population Coverage - 50% Urban Extent"), uiOutput("progress_50")),
    column(6, h4("Population Coverage - 70% Urban Extent"), uiOutput("progress_70"))
  ),
  fluidRow(
    column(12,
           h4("Combined Results Table (50% and 70% Urban Extents)"),
           DTOutput("summary_table_combined")
    )
  )
)

server <- function(input, output, session) {
  
  calculate_summary <- function(population) {
    total_prop <- input$hh1_2 + input$hh3_4 + input$hh5plus
    hh_distribution <- c(`1-2` = input$hh1_2, `3-4` = input$hh3_4, `5+` = input$hh5plus) / total_prop
    
    nets_per_hh <- c(`1-2` = 1, `3-4` = 2, `5+` = 3)
    avg_people_per_hh <- c(`1-2` = 1.5, `3-4` = 3.5, `5+` = 5.5)
    
    avg_hh_size <- sum(hh_distribution * avg_people_per_hh)
    total_households <- population / avg_hh_size
    avg_nets_per_hh <- sum(hh_distribution * nets_per_hh)
    estimated_hh_covered <- input$total_nets / avg_nets_per_hh
    min_hh_covered <- floor(input$total_nets / 3)
    max_hh_covered <- input$total_nets
    people_covered_by_category <- estimated_hh_covered * hh_distribution * avg_people_per_hh
    total_people_covered <- sum(people_covered_by_category)
    prop_population_covered <- round((total_people_covered / population) * 100, 1)
    
    summary_df <- data.frame(
      `Household Size Category` = names(hh_distribution),
      `Proportion of Households` = paste0(round(hh_distribution * 100, 1), "%"),
      `Nets per Household` = nets_per_hh,
      `Average People per Household` = avg_people_per_hh,
      `Estimated People Covered` = round(people_covered_by_category)
    )
    
    summary_df <- rbind(
      summary_df,
      data.frame(
        `Household Size Category` = "TOTAL / AVERAGE",
        `Proportion of Households` = "",
        `Nets per Household` = sprintf("%.2f avg", avg_nets_per_hh),
        `Average People per Household` = sprintf("%.2f avg", avg_hh_size),
        `Estimated People Covered` = round(total_people_covered)
      ),
      data.frame(
        `Household Size Category` = "Population Coverage Summary",
        `Proportion of Households` = paste0(prop_population_covered, "% population covered"),
        `Nets per Household` = paste0("Min HHs: ", min_hh_covered),
        `Average People per Household` = paste0("Max HHs: ", max_hh_covered),
        `Estimated People Covered` = ""
      )
    )
    return(list(df = summary_df, coverage = prop_population_covered))
  }
  
  summary_data_50 <- reactive({
    res <- calculate_summary(input$population_50)
    df <- res$df
    df$Urban_Extent <- "50%"
    attr(df, "coverage") <- res$coverage
    df
  })
  
  summary_data_70 <- reactive({
    res <- calculate_summary(input$population_70)
    df <- res$df
    df$Urban_Extent <- "70%"
    attr(df, "coverage") <- res$coverage
    df
  })
  
  summary_combined <- reactive({
    rbind(summary_data_50(), summary_data_70())
  })
  
  output$summary_table_combined <- renderDT({
    datatable(summary_combined(), rownames = FALSE, options = list(dom = 't'))
  })
  
  output$progress_50 <- renderUI({
    coverage <- attr(summary_data_50(), "coverage")
    tagList(
      tags$div(style = "background-color:#f5f5f5; border-radius:5px; padding:5px;",
               tags$div(style = paste0("width:", coverage, "%; background-color:#4CAF50; color:white; padding:5px; border-radius:5px;"),
                        paste0("", coverage, "% Population Covered"))
      )
    )
  })
  
  output$progress_70 <- renderUI({
    coverage <- attr(summary_data_70(), "coverage")
    tagList(
      tags$div(style = "background-color:#f5f5f5; border-radius:5px; padding:5px;",
               tags$div(style = paste0("width:", coverage, "%; background-color:#2196F3; color:white; padding:5px; border-radius:5px;"),
                        paste0("", coverage, "% Population Covered"))
      )
    )
  })
  
  output$download_combined_csv <- downloadHandler(
    filename = function() { "LLIN_Coverage_Combined.csv" },
    content = function(file) {
      write.csv(summary_combined(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
