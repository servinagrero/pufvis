library(bslib)
library(bsicons)
library(shinycssloaders)
library(shinyalert)
library(sass)
library(plotly)
library(pufvis)

options(
  spinner.color = "#0275D8",
  spinner.color.background = "#ffffff",
  spinner.type = 8
)

fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "zephyr"
  ),
  tags$style(sass(sass_file("www/custom.scss"))),
  withMathJax(),
  useShinyalert(force = TRUE),
  navbarPage(
    "PUFVIS",
    tabPanel("Import",
      fluid = TRUE,
      icon = icon("arrow-up"),
      mainPanel(
        width = 12,
        markdown("Maximum size of **4GB** is allowed."),
        p("Accepted files are .csv, .tsv, .rds and .h5"),
        shiny::fluidRow(
          column(
            6,
            h2("Import CRPs"),
            fileInput("crpsFile",
              label = "CRP Data",
              buttonLabel = "Browse...",
              accept = c(".csv", ".tsv", ".rds", ".h5")
            ),
            markdown("
                   If CSV or TSV is supplied, the following columns are required:
                   - **device**: Number or string identifying the device.
                   - **challenge**: Numeric index of the challenge.
                   - **response**: Binary response of the challenge.
                   - **sample**: Number of string indentifying the sample.

                   The file can contain other additional columns as metadata.

                  If an RDS is supplied, it's read as a 3D array. In this case, the fields **device**, **challenge** and **sample** are read from `dimnames` respectively.")
          ),
          column(
            6,
            h2("Create CRPs from responses"),
            fileInput("rawData",
              label = "Raw data",
              buttonLabel = "Browse...",
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
      mainPanel(
        width = 12,
        column(6, uiOutput("crpsExplore")),
        fluidRow(
          column(
            6,
            h2("Response heatmap"),
            p("2D heatmap showing the distribution of responses for each device and challenge."),
            withSpinner(plotlyOutput("crpsHeatmap")),
            uiOutput("referenceSample")
          ),
          column(
            6,
            h2("Response bit ratio"),
            p("A positive ratio implies more 1s than 0s and a negative ratio, the opposite."),
            withSpinner(plotlyOutput("crpsDistribution"))
          )
        )
      )
    ),
    tabPanel("Metrics",
      fluid = TRUE,
      icon = icon("bar-chart"),
      sidebarLayout(
        sidebarPanel(
          uiOutput("referenceSample2"),
          checkboxInput("normPlots", "Force output to range [0, 1]", value = FALSE),
          checkboxInput("entropyResults", "Force results to be entropy", value = FALSE),
          helpText("When entropy calculation is enabled, the binary entropy of each metric is computed instead."),
          width = 3
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Uniformity",
              value = "tab_uniformity",
              h2("Uniformity distribution"),
              withSpinner(plotOutput("unifHist")),
              h2("Uniformity per device"),
              withSpinner(plotlyOutput("unifScatter"))
            ),
            tabPanel(
              "Bitaliasing",
              value = "tab_bitaliasing",
              h2("Bitaliasing distribution"),
              withSpinner(plotOutput("bitaliasHist")),
              h2("Bitaliasing per challenge"),
              withSpinner(plotlyOutput("bitaliasScatter"))
            ),
            tabPanel(
              "Uniqueness",
              value = "tab_uniqueness",
              h2("Uniqueness distribution"),
              tags$p("Number of pairs calculated as \\(N = (D * (D-1)) / 2\\)"),
              textOutput("uniqPairs"),
              withSpinner(plotlyOutput("uniqHist")),
              h2("Inter-HD across all devices"),
              withSpinner(plotlyOutput("uniqPairsPlot"))
            ),
            tabPanel(
              "Reliability",
              value = "tab_reliability",
              h2("Reliability distribution"),
              withSpinner(plotOutput("relHist")),
              h2("Reliability per challenge"),
              withSpinner(plotlyOutput("relScatter")),
              h2("Bitaliasing VS Reliability"),
              withSpinner(plotlyOutput("relBitalias"))
            ),
            tabPanel(
              "Rel. Entropy",
              value = "tab_relentropy",
              h2("Reliable entropy distribution"),
              withSpinner(plotOutput("relEntropyHist")),
              h2("Reliable entropy per challenge"),
              withSpinner(plotlyOutput("relEntropyScatter"))
            ),
          ),
          width = 9
        )
      )
    ),
    tabPanel("Export",
      fluid = TRUE,
      icon = icon("list"),
      shiny::sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "tableColumns",
            label = "Included in summary",
            choices = pufvis::columns_summary,
            selected = pufvis::columns_summary,
            multiple = TRUE
          ),
          selectInput(
            inputId = "metricsTableFormat",
            label = "Format to export",
            choices = c("latex", "html", "pipe", "simple", "rst"),
            selected = "latex",
          ),
          downloadButton("exportExcel", "Export metrics to Excel"),
          downloadButton("exportTable", "Export table to CSV")
        ),
        mainPanel(
          tableOutput("metricsSummaryTable"),
          h3("Text Output"),
          verbatimTextOutput("metricsTableText")
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
