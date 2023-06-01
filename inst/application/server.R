library(pufvis)
library(dplyr)
library(shinyalert)
library(RcppAlgos)
library(openxlsx)
library(knitr)
library(kableExtra)

theme_set(theme_minimal(base_family = "Inter") + theme(text = element_text(size = 18)))


function(input, output, session) {
  session$onSessionEnded(stopApp)

  source("import.R", local = TRUE)$value
  source("metrics.R", local = TRUE)$value

  output$referenceSample <- renderUI({
    if (is.null(crp_table())) {
      return(NULL)
    }
    selectInput(
      inputId = "refSampleHeatmap",
      label = "Sample to display",
      choices = dimnames(crp_table())[[3]],
      multiple = FALSE,
    )
  })

  output$crpsExplore <- renderUI({
    size <- dim(crp_table())
    nresp <- length(crp_table())
    format_fn <- scales::comma_format(big.mark = ".", decimal.mark = ",")
    mem_size <- object.size(crp_table())[[1]]
    value_box(
      title = "CRP Summary",
      value = paste("Total Responses:", format_fn(nresp)),
      showcase_layout = showcase_top_right(),
      theme_color = "light",
      showcase = bs_icon("graph-up"),
      p(paste("Number of Devices:", size[[1]])),
      p(paste("Challenges per device:", size[[2]])),
      p(paste("Samples per challenge:", size[[3]])),
      p(paste("Total responses per sample:", format_fn(size[[1]] * size[[2]]))),
      p(paste("Total responses per device:", format_fn(size[[1]] * size[[3]]))),
      p(paste("Size in memory:", scales::label_bytes()(mem_size)))
    )
  })


  output$exportMetrics <- downloadHandler(
    filename = function() {
      "metrics.csv"
    },
    content = function(fname) {
      write.csv(as.data.frame(metricsTable()), fname)
    }
  )

  output$exportExcel <- downloadHandler(
    filename = function() {
      "metrics.xlsx"
    },
    content = function(fname) {
      pufvis::metrics_to_excel(fname, crp_metrics())
    }
  )
}
