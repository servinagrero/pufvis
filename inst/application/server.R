library(pufvis)
library(dplyr)
library(shinyalert)
library(RcppAlgos)


function(input, output) {
  observeEvent(input$crpsFile, {
    ext <- tools::file_ext(input$crpsFile$name)
    crps_df <- switch(ext,
      csv = vroom::vroom(input$crpsFile$datapath),
      tsv = vroom::vroom(input$crpsFile$datapath, delim = "\t"),
      rds = readr::read_rds(input$crpsFile$datapath),
      validate("Invalid file; Please upload a csv, tsv or rds file")
    )
    if (!pufvis::validate_df(crps_df)) {
      shinyalert("Problem uploading CRPs",
        "Malformed data frame.",
        type = "error"
      )
    } else {
      crp_table(crps_to_arr(crps_df))
      shinyalert("Sucess",
        "CRP data loaded successfully.",
        type = "success"
      )
      # showNotification("CRP data loaded successfully.")
    }
  })

  observeEvent(input$rawData, {
    ext <- tools::file_ext(input$rawData$name)
    responses <- switch(ext,
      csv = vroom::vroom(input$crpsFile$datapath),
      tsv = vroom::vroom(input$crpsFile$datapath, delim = "\t"),
      rds = readr::read_rds(input$crpsFile$datapath),
      validate("Invalid file; Please upload a csv, tsv or rds file")
    )
    crps_df <- pufvis::create_crps(responses, input$crpAlgorithm)
    crp_table(crps_to_arr(crps_df))
  })

  output$algoExplanation <- eventReactive(input$crpAlgorithm, {
    pufvis::algorithms[[grep(input$crpAlgorithm, pufvis::algorithms)]]$desc
  })

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
    # value_box(
    #   title = "CRP Summary",
    #   value = paste("# Responses:", format_fn(nresp)),
    #   showcase = bs_icon("graph-up"),
    #   p(paste("# Devices:", size[[1]])),
    #   p(paste("# Challenges:", size[[2]])),
    #   p(paste("# Samples:", size[[3]]))
    # )
    card(
      card_header("CRP Summary", class = "bg-dark"),
      paste("# Devices:", size[[1]]),
      tags$br(),
      paste("# Challenges:", size[[2]]),
      tags$br(),
      paste("# Samples:", size[[3]]),
      tags$br(),
      paste("# Responses:", format_fn(nresp))
    )
  })

  crps_heatmap <- reactive({
    if (is.null(crp_table())) {
      return(NULL)
    }
    crp_table()[, , input$refSampleHeatmap] %>%
      reshape2::melt() %>%
      mutate(value = factor(value)) %>%
      rename(device = "Var1", challenge = "Var2", response = "value") %>%
      plot_ly() %>%
      add_heatmap(
        x = ~challenge, y = ~device, z = ~response, color = ~response, colors = viridis::viridis(2),
        hovertemplate = "Device: %{y}<br>Challenge: %{x}<br>Response: %{z}"
      ) %>%
      layout(title = "Response distribution")
  })
  output$crpsHeatmap <- renderPlotly(crps_heatmap())

  crpsDistrib <- reactive({
    if (is.null(crp_table())) {
      return(NULL)
    }
    ratio_mat <- apply(crp_table(), 3, function(m) apply(m, 1, pufr::ratio_bits)) %>%
      as.data.frame() %>%
      mutate(device = 1:n()) %>%
      reshape2::melt(id.vars = "device") %>%
      rename(sample = "variable", ratio = "value") %>%
      mutate(sample = factor(sample))


    plot_ly(ratio_mat, type = "bar", colors = viridis::viridis(max(ratio_mat$device))) %>%
      add_trace(
        x = ~device, y = ~ratio, color = ~sample, text = ~sample,
        hovertemplate = "Device: %{x}<br>Sample: %{text}<br>Ratio: %{y}"
      ) %>%
      layout(barmode = "group", title = "Ratio of responses", yaxis = list(range = c(-1, 1)))
  })
  output$crpsDistribution <- renderPlotly(crpsDistrib())

  histUnif <- reactive({
    p <- ggplot(data.frame(unif = crp_metrics()$unif)) +
      geom_histogram(aes(x = unif), bins = 100)
    if (input$normPlots) {
      p + scale_x_continuous(limits = c(0, 1))
    } else {
      p
    }
  })
  output$unifHist <- renderPlot(histUnif())

  output$relHist <- renderPlot({
    rel <- colMeans(crp_metrics()$rel)
    ggplot() +
      geom_histogram(aes(x = rel)) +
      scale_x_continuous(limits = c(0, 1))
  })

  output$relScatter <- renderPlot({
    rel <- crp_metrics()$rel
    rel %>%
      reshape2::melt() %>%
      ggplot() +
      geom_boxplot(aes(x = as.factor(Var1), y = value))
  })

  output$relBitalias <- renderPlot({
    data.frame(
      bitalias = crp_metrics()$bitalias,
      rel = colMeans(crp_metrics()$rel)
    ) %>%
      ggplot() +
      geom_point(aes(x = rel, y = bitalias)) +
      coord_cartesian(xlim = c(0, 1), ylim = c(0, 1))
  })

  output$relEntropyHist <- renderPlot({
    crp_metrics()$relentropy %>%
      reshape2::melt() %>%
      ggplot() +
      geom_histogram(aes(x = value, fill = Var1), alpha = 0.5) +
      scale_x_continuous(limits = c(-1, 1))
  })

  output$relEntropyScatter <- renderPlot({
    crp_metrics()$relentropy %>%
      reshape2::melt() %>%
      ggplot() +
      geom_raster(aes(x = Var1, y = Var2, fill = value))
  })

  crp_metrics <- reactive({
    calculate_metrics(crp_table(), 1)
  })

  metricsTable <- eventReactive(input$tableColumns, {
    if (is.null(crp_table())) {
      return(NULL)
    }
    pufvis::create_summary(crp_metrics()) %>%
      as.data.frame() %>%
      select(!!!input$tableColumns)
  })
  output$metricsTable <- renderTable(
    {
      if (is.null(metricsTable())) {
        return(NULL)
      }
      metricsTable()
    },
    rownames = TRUE
  )
  output$metricsExportTable <- renderText({
    if (is.null(metricsTable())) {
      return(NULL)
    }
    metricsTable() %>%
      knitr::kable(format = "latex", booktabs = TRUE, caption = "Summary of the PUF metrics")
  })


  output$unifScatter <- renderPlot({
    unif <- crp_metrics()$unif
    ggplot(data.frame(unif = unif, device = seq_along(unif))) +
      geom_point(aes(x = device, y = unif))
  })

  histBitalias <- reactive({
    bitalias <- crp_metrics()$bitalias
    p <- ggplot(data.frame(bitalias = bitalias)) +
      geom_histogram(aes(x = bitalias), bins = 100)
    if (input$normPlots) {
      p + scale_x_continuous(limits = c(0, 1))
    } else {
      p
    }
  })

  output$bitaliasHist <- renderPlot(histBitalias())

  output$bitaliasScatter <- renderPlot({
    bitalias <- crp_metrics()$bitalias
    ggplot(data.frame(unif = bitalias, crp = seq_along(bitalias))) +
      geom_point(aes(x = crp, y = bitalias))
  })

  output$uniqPairs <- renderText({
    paste("Number of pairs:", length(crp_metrics()$uniq))
  })
  output$uniqHist <- renderPlotly({
    plot_ly(alpha = 0.6) %>%
      # plot_ly(alpha = 0.6, nbinsx = 50) %>%
      add_histogram(crp_metrics()$uniq,
        name = "", color = viridis::viridis(1, direction = -1),
        hovertemplate = "Uniqueness: %{x:2.3f}<br>Number of pairs: %{y}"
      ) %>%
      config(toImageButtonOptions = list(
        format = "jpeg",
        filename = "custom_image",
        # height= 500,
        # width= 700,
        scale = 3
      )) %>%
      layout(
        bargap = 0.05,
        xaxis = list(title = "Uniqueness", range = c(0, 1)),
        yaxis = list(title = "Number of pairs")
      )
  })
  output$uniqPairsPlot <- renderPlotly({
    uniq <- crp_metrics()$uniq
    # dev_names <- as.vector(dimnames(crp_table())[[1]])
    RcppAlgos::comboGeneral(seq_len(nrow(crp_table())), 2) %>%
      as.data.frame() %>%
      cbind(uniq) %>%
      plot_ly() %>%
      add_heatmap(x = ~V1, y = ~V2, z = ~uniq)
  })

  # output$exportMetrics <- downloadHandler(
  #   filename = function() {
  #     "metrics.csv"
  #   },
  #   content = function(fname) {
  #     write.csv(as.data.frame(m), fname)
  #   }
  # )
}
