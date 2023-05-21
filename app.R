library(shiny)
library(DT)
library(tidyverse)
library(ggplot2)
library(bslib)
library(shinycssloaders) # withSpinner
library(pufr)

# References
# https://github.com/gpilgrim2670/SwimMap/blob/master/app.R
# Exporting the results of a table
# https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table

theme_set(
  theme_minimal(base_family = "Inter", base_size = 18) +
    theme(text = element_text(size = 24))
)

# crp_data <- read_rds("/path/to/test_data.rds")

about_text <- "
# What is PUFVis?

PUFVis is a shiny app to calculate, explore and visualize in detail Physical Unclonable Function metrics.

# References
[PUFR](https://servinagrero.github.io/pufr)
"

ui <- fluidPage(
  ## includeCSS("main.css"),
  ## theme = bs_theme(version = 5, bootswatch = "minty"),
  navbarPage("PUF METRICS",
    theme = bs_theme(
      bootswatch = "zephyr",
      base_font = font_google("IBM Plex Sans"),
      code_font = font_google("JetBrains Mono")
    ),
    tabPanel("Exploration",
      fluid = TRUE,
      titlePanel("Responses exploration"),
      mainPanel(
        DTOutput("tbl"),
        downloadButton("exportMetrics", "Export metrics to CSV"),
      )
    ),
    tabPanel("Metric Summary",
      fluid = TRUE,
      titlePanel("PUF Metrics"),
      sidebarLayout(
        sidebarPanel(
          hr(),
          sliderInput(
            inputId = "DivCompRankA",
            label = "Top Times Range:",
            min = 1, max = 3500,
            value = c(1, 250)
          ),
        ),
        mainPanel(
          ## helpText("Which devices and regions to show"),
          plotOutput("dataHist"),
        )
      )
    ),
    tabPanel("Load data",
      fluid = TRUE,
      titlePanel("Load Data")
    ),
    tabPanel("About",
      fluid = TRUE,
      markdown(about_text)
    )
  )
)

server <- function(input, output) {
  output$tbl <- renderDT(
    iris,
    options = list(lengthChange = FALSE)
  )

  output$exportMetrics <- downloadHandler(
    filename = function() {
      "metrics.csv"
    },
    content = function(fname) {
      write.csv(data.frame(), fname)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)
