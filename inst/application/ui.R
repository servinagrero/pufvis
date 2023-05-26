library(bslib)
library(bsicons)
library(shinycssloaders) # withSpinner
library(plotly)
library(sass)
library(pufvis)
library(shinyalert)

fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr",
    base_font = font_google("Open Sans"),
    heading_font = font_google("Source Code Pro"),
    code_font = font_google("Source Code Pro")
  ),
  tags$style(sass(sass_file("www/custom.scss"))),
  withMathJax(),
  useShinyalert(force = TRUE),
  navbarPage(
    "PUFVIS",
    tabPanel("Import",
      fluid = TRUE,
      icon = icon("arrow-up"),
      h2("Import CRPs or raw responses"),
      mainPanel(
        width = 12,
        markdown("Maximum size of **4GB** is allowed."),
        shiny::fluidRow(
          column(
            6,
            helpText("Accepted files are .csv, .tsv, .rds and .h5"),
            fileInput("crpsFile",
              label = "CRP Data",
              buttonLabel = "Browse file",
              accept = c(".csv", ".tsv", ".rds", ".h5")
            ),
            markdown("
                   If CSV or TSV is supplied, the following columns are required:
                   - **device**: Number of string identifying the device.
                   - **challenge**: Numeric index of the challenge.
                   - **response**: Binary response of the challenge.
                   - **sample**: Number of string indentifying the sample.

                   The file can contain other additional columns as metadata.

                  If an RDS is supplied, it's read as a 3D array. In this case, the fields **device**, **challenge** and **sample** are read from `dimnames` respectively.")
          ),
          column(
            6,
            h2("WORK IN PROGRESS"),
            fileInput("rawData",
              label = "Raw data",
              buttonLabel = "Browse file",
              accept = c(".csv", ".tsv", ".rds", ".h5")
            ),
            helpText("The generation algorithm should be chosen before loading the raw data"),
            selectInput("crpAlgorithm",
              label = "CRP generation algorithm",
              choices = sapply(algorithms, function(x) x$name)
            ),
            textOutput("algoExplanation")
          )
        )
      )
    ),
    tabPanel("Explore",
      fluid = TRUE,
      icon = icon("search"),
      titlePanel("Responses exploration"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("referenceSample"),
          uiOutput("crpsExplore"),
          width = 3
        ),
        mainPanel(
          withSpinner(plotlyOutput("crpsHeatmap")),
          withSpinner(plotlyOutput("crpsDistribution")),
          width = 9
        )
      )
    ),
    tabPanel("Metrics",
      fluid = TRUE,
      icon = icon("bar-chart"),
      titlePanel("PUF Metrics"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("referenceSample2"),
          checkboxInput("normPlots", "Force metrics output to the full range [0, 1]", value = FALSE),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Uniformity",
              value = "tab_uniformity",
              value_box(
                title = "Uniformity",
                value = "Uniformity",
                showcase = bs_icon("bar-chart")
              ),
              withSpinner(splitLayout(
                cellWidths = c("50%", "50%"),
                plotOutput("unifHist"), plotOutput("unifScatter")
              )),
            ),
            tabPanel(
              "Bitaliasing",
              value = "tab_bitaliasing",
              withSpinner(splitLayout(
                cellWidths = c("50%", "50%"),
                plotOutput("bitaliasHist"), plotOutput("bitaliasScatter")
              )),
            ),
            tabPanel(
              "Uniqueness",
              value = "tab_uniqueness",
              tags$p("Number of pairs calculated as \\(N = \\frac{D * (D-1)}{2}\\)"),
              textOutput("uniqPairs"),
              withSpinner(plotlyOutput("uniqHist")),
              withSpinner(plotlyOutput("uniqPairsPlot"))
            ),
            tabPanel(
              "Reliability",
              value = "tab_reliability",
              plotOutput("relHist"),
              plotOutput("relScatter"),
              plotOutput("relBitalias"),
            ),
            tabPanel(
              "Rel. Entropy",
              value = "tab_relentropy",
              plotOutput("relEntropyHist"),
              plotOutput("relEntropyScatter"),
            ),
          ),
          width = 9
        )
      )
    ),
    tabPanel("Export",
      fluid = TRUE,
      icon = icon("list"),
      titlePanel("Metrics Summary"),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "tableColumns",
            label = "Included in summary",
            choices = pufvis::columns_summary,
            selected = pufvis::columns_summary,
            multiple = TRUE
          ),
          hr(),
          downloadButton("exportMetrics", "Export metrics to CSV")
        ),
        mainPanel(
          # layout_column_wrap(
          #   width = "250px",
          #   value_box(
          #     title = "Uniformity",
          #     value = "Uniformity",
          #     showcase = bs_icon("bar-chart"),
          #   )
          # ),
          tableOutput("metricsTable"),
          verbatimTextOutput("metricsExportTable")
        )
      )
    ),
    tabPanel("About",
      fluid = TRUE,
      icon = icon("info"),
      includeMarkdown("www/about.md")
    )
  )
)
