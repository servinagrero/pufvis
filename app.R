library(shiny)
library(DBI)
library(DT)
library(tidyverse)
library(ggplot2)
library(stringr)
library(bslib)
library(pufr)
library(shinycssloaders) # withSpinner

# https://github.com/gpilgrim2670/SwimMap/blob/master/app.R

# Exporting the results of a table
# https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table

theme_set(
  theme_minimal(base_family = "Inter", base_size = 18) +
    theme(text = element_text(size = 24))
)

crp_data <- read_rds("/home/vinagres/Documents/PhD/data/sram_crps_1.rds.bz2")

#' Convert a list of bytes to a list of bits
bytes_to_bits <- function(bytes, length = 8) {
  fn <- function(x) rev(intToBits(x)[seq_len(length)]) |> as.numeric()
  sapply(bytes, fn)
}

about_txt <- "
# What is this?

This is an app for visualizing a PUF dataset.

`This is a monospace test`

[PUFR](https://servinagrero.github.io/pufr)
"

ui <- fluidPage(
  ## includeCSS("main.css"),
  ## titlePanel("SRAM PUF Data"),
  ## theme = bs_theme(version = 5, bootswatch = "minty"),
  navbarPage("PUF METRICS",
    theme = bs_theme(
      bootswatch = "zephyr",
      base_font = font_google("IBM Plex Sans"),
      code_font = font_google("JetBrains Mono")
    ),
    tabPanel("Exploration",
      fluid = TRUE,
      titlePanel("Division II School Types"),
      mainPanel(
        textOutput(outputId = "description_DII"),
        DTOutput("tbl"),
        downloadButton("exportMetrics", "Export metrics to CSV"),
      )
    ),
    tabPanel("Metric Summary",
      fluid = TRUE,
      titlePanel("PUF Metrics"),
      sidebarLayout(
        sidebarPanel(
          ## selectInput(inputId = "board", label = "Board:", choices = c("all", dev_ids)),
          ## selectInput(inputId = "address", label = "Address:", choices = c("all", dev_address)),
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
          # textOutput("uniformity"),
          # tableOutput("bitaliasing"),
          # plotOutput("dataMap")
          # tableOutput('tbl')
        )
      )
    ),
    tabPanel("Load data",
      fluid = TRUE,
      titlePanel("Load Data")
    ),
    tabPanel("About",
      fluid = TRUE,
      markdown(about_txt)
    )
  )
)

server <- function(input, output) {
  #   output$selectors <- renderUI({
  #     reactive(selectInput("board", "Board: ", choices = board_ids))
  #     reactive(selectInput("region", "Region: ", choices = board_regions))
  # })

  ## tbl_input <- reactive({
  ##   if (input$board != "all") {
  ##     samples %>% filter(board == input$board)
  ##   } else {
  ##     samples
  ##   }
  ## })

  output$tbl <- renderDT(
    iris,
    options = list(lengthChange = FALSE)
  )

  ## output$dataHist <- renderPlot({
  ##   if (input$address != "all") {
  ##     data_region <- tbl_input() |> filter(address == input$address)
  ##     bytes <- stringr::str_split(data_region$data, ",") |>
  ##       unlist() |>
  ##       as.vector() |>
  ##       as.numeric()

  ##     ggplot(data.frame(bytes = bytes)) +
  ##       geom_histogram(aes(x = bytes))
  ##   } else {
  ##     bytes <- stringr::str_split(tbl_input()$data, ",") |>
  ##       unlist() |>
  ##       as.vector() |>
  ##       as.numeric()

  ##     df <- data.frame(
  ##       bytes = bytes,
  ##       board = rep(dev_ids, each = length(dev_address) * 512)
  ##     )
  ##     print(head(df))
  ##     ggplot(df) +
  ##       geom_histogram(aes(x = bytes, fill = board), alpha = 0.6)
  ##   }
  ## })

  # output$uniformity <- renderText({
  #     if (input$board != "all") {
  #   samples <- tbl_input() |> filter(board == input$board)
  #   nregions <- unique(samples$address) |> length()
  #   samples <- samples |> head(nregions)
  #
  #   bits <- stringr::str_split(samples$data, ",") |>
  #     unlist() |>
  #     as.vector() |>
  #       as.numeric() |>
  #       toBits()
  #
  #   uniformity(bits)
  #     }
  # })

  # output$bitaliasing <- renderTable({
  #     if (input$address != "all") {
  #         samples <- tbl_input() |> filter(address == input$address)
  #
  #         bitalias <- stringr::str_split(samples$data, ",") |>
  #             unlist() |>
  #             as.vector() |>
  #             as.numeric() |>
  #             toBits()
  #         bitaliasing(bitalias)
  #     } else {
  #         regions <- unique(samples$address)
  #         results <- data.frame()
  #         for (address in regions) {
  #             data_region <- samples %>% filter(address == address)
  #             bitalias <- stringr::str_split(data_region$data, ",") |>
  #                 unlist() |> as.vector() |> as.numeric() |> toBits() |> bitaliasing()
  #             results <- results %>% bind_rows(data.frame(address = address, bitaliasing = bitalias[[1]]))
  #         }
  #         results
  #     }
  # })

  # output$dataMap <- renderPlot({
  #   if (input$board != "all") {
  #     # validate(need(input$board != "all"), "Please select a board")
  #     samples <- tbl_input() |> filter(board == input$board)
  #     nregions <- unique(samples$address) |> length()
  #     samples <- samples |> head(nregions)

  #     bytes <- stringr::str_split(samples$data, ",") |>
  #       unlist() |>
  #       as.vector() |>
  #       as.numeric()
  #     bytes_df <- expand.grid(address = board_address[2:201], offset = seq_len(length(bytes) / nregions))
  #     bytes_df$value <- bytes
  #     ggplot(bytes_df) +
  #       geom_tile(aes(x = offset, y = address, fill = value))
  #   }
  # })

  output$exportMetrics <- downloadHandler(
    filename = function() {
      "metrics.csv"
    },
    content = function(fname) {
      write.csv(data.frame(), fname)
    }
  )
}

app <- shinyApp(ui, server)
runApp(app, port = 8080)


# Run the application
shinyApp(ui = ui, server = server)
