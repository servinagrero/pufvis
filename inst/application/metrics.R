# Exploration -------------------------------------------------------------

crps_heatmap <- reactive({
  if (is.null(crp_table())) {
    return(NULL)
  }
  crp_table()[, , input$refSampleHeatmap] %>%
    reshape2::melt() %>%
    rename(device = "Var1", challenge = "Var2", response = "value") %>%
    plot_ly(x = ~challenge, y = ~device, z = ~response,
      color = ~response,
      name = "Response", type = "heatmap",
      hovertemplate = "Device: %{y}<br>Challenge: %{x}<br>Response: %{z}",
      colors = viridis::viridis_pal(option = "D")(2),
      showscale = FALSE
    ) %>%
    layout(
      xaxis = list(title = "Challenge"),
      yaxis = list(title = "Device")
    )
})
output$crpsHeatmap <- renderPlotly(crps_heatmap())

crpsDistrib <- reactive({
  if (is.null(crp_table())) {
    return(NULL)
  }
  devs <- stringr::str_sort(dimnames(crp_table())[[1]], numeric = TRUE)
  apply(crp_table(), 3, function(m) apply(m, 1, pufr::ratio_bits)) %>%
    as.data.frame() %>%
    mutate(device = devs) %>%
    reshape2::melt(id.vars = "device") %>%
    rename(sample = "variable", ratio = "value") %>%
    plot_ly(
      x = ~device, y = ~sample, color = ~ratio, z = ~ratio, name = "Bit Ratio",
      type = "heatmap", colors = viridis::viridis(length(devs)),
      hovertemplate = "Device: %{x}<br>Sample: %{y}<br>Ratio: %{z}"
    ) %>%
    layout(
      xaxis = list(title = "Device", categoryorder = "array", categoryarray = devs),
      yaxis = list(title = "Sample")
      )
})
output$crpsDistribution <- renderPlotly(crpsDistrib())

# Uniformity --------------------------------------------------------------

histUnif <- reactive({
  if (input$entropyResults) {
    unif <- pufr::entropy_p(crp_metrics()$unif)
  } else {
    unif <- crp_metrics()$unif
  }
  p <- ggplot(data.frame(unif = unif)) +
    geom_histogram(aes(x = unif), bins = 100) +
    labs(x = "Uniformity")
  if (input$normPlots) {
    p + scale_x_continuous(limits = c(0, 1))
  } else {
    p
  }
})
output$unifHist <- renderPlot(histUnif())


output$unifScatter <- renderPlotly({
  if (input$entropyResults) {
    unif <- pufr::entropy_p(crp_metrics()$unif)
  } else {
    unif <- crp_metrics()$unif
  }
  devs <- crp_metrics()$devices
  data.frame(
    unif = unif,
    device = devs
  ) %>%
    plot_ly(x = ~device, y = ~unif, type = "scatter") %>%
    layout(
      xaxis = list(
        title = "Device", categoryorder = "array",
        categoryarray = devs
      ),
      yaxis = list(title = "Uniformity", range = c(0, 1))
    )
})

# Bitaliasing -------------------------------------------------------------



histBitalias <- reactive({
  if (input$entropyResults) {
    bitalias <- pufr::entropy_p(crp_metrics()$bitalias)
  } else {
    bitalias <- crp_metrics()$bitalias
  }
  p <- ggplot(data.frame(bitalias = bitalias)) +
    geom_histogram(aes(x = bitalias), bins = 100) +
    labs(x = "Bitaliasing")
  if (input$normPlots) {
    p + scale_x_continuous(limits = c(0, 1))
  } else {
    p
  }
})

output$bitaliasHist <- renderPlot(histBitalias())

output$bitaliasScatter <- renderPlotly({
  if (input$entropyResults) {
    bitalias <- pufr::entropy_p(crp_metrics()$bitalias)
  } else {
    bitalias <- crp_metrics()$bitalias
  }
  challenges <- crp_metrics()$challenges

  data.frame(
    challenge = challenges,
    bitalias = bitalias
  ) %>%
    plot_ly(x = ~challenge, y = ~bitalias, type = "scatter") %>%
    layout(
      xaxis = list(
        title = "Challenge",
        categoryorder = "array",
        categoryarray = challenges
      ),
      yaxis = list(title = "Bitaliasing", range = c(0, 1))
    )
})

# Uniqueness --------------------------------------------------------------

output$uniqPairs <- renderText({
  paste("Number of pairs:", length(crp_metrics()$uniq))
})
output$uniqHist <- renderPlotly({
  plot_ly(alpha = 0.8) %>%
    add_histogram(crp_metrics()$uniq,
      name = "Uniqueness", color = viridis::viridis(1, direction = -1),
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
      title = "Histogram of Uniqueness",
      bargap = 0.05,
      xaxis = list(title = "Uniqueness", range = c(0, 1)),
      yaxis = list(title = "Number of pairs")
    )
})
output$uniqPairsPlot <- renderPlotly({
  uniq <- crp_metrics()$uniq
  devs <- crp_metrics()$devices
  RcppAlgos::comboGeneral(devs, 2) %>%
    as.data.frame() %>%
    arrange(V1, V2) %>%
    cbind(uniq) %>%
    plot_ly(
      x = ~V1, y = ~V2, z = ~uniq, name = "Uniqueness", type = "heatmap",
      hovertemplate = "First: %{x}<br>Second: %{y}<br>HD: %{z}"
    ) %>%
    layout(
      xaxis = list(
        title = "First device",
        categoryorder = "array",
        categoryarray = devs
      ), yaxis = list(
        title = "Second device", categoryorder = "array",
        categoryarray = devs
      )
    )
})

# Reliability -------------------------------------------------------------

output$relHist <- renderPlot({
  rel <- colMeans(crp_metrics()$rel)
  ggplot() +
    geom_histogram(aes(x = rel)) +
    labs(x = "Reliability") +
    scale_x_continuous(limits = c(0, 1))
})

output$relScatter <- renderPlotly({
  crp_metrics()$rel %>%
    reshape2::melt() %>%
    plot_ly() %>%
    add_heatmap(
      x = ~Var2, y = ~Var1, z = ~value, name = "Reliability",
      hovertemplate = "Challenge: %{x}<br>Device: %{y}<br>Reliability: %{z}"
    ) %>%
    layout(
      xaxis = list(title = "Challenge"),
      yaxis = list(title = "Device")
    )
})

output$relBitalias <- renderPlotly({
  if (input$entropyResults) {
    bitalias <- pufr::entropy_p(crp_metrics()$bitalias)
  } else {
    bitalias <- crp_metrics()$bitalias
  }
  data.frame(
    bitalias = bitalias,
    rel = colMeans(crp_metrics()$rel),
    challenge = as.integer(crp_metrics()$challenges)
  ) %>%
    plot_ly(
      x = ~rel, y = ~bitalias, text = ~challenge, color = ~challenge, type = "scatter",
      hovertemplate = "Challenge: %{text}<br>Reliability: %{x}<br>Bitaliasing: %{y}"
    ) %>%
    layout(
      xaxis = list(title = "Bitaliasing", range = c(0, 1)),
      yaxis = list(title = "Reliability", range = c(0, 1))
    )
})

# Rel Entropy -------------------------------------------------------------

output$relEntropyHist <- renderPlot({
  crp_metrics()$relentropy %>%
    reshape2::melt() %>%
    ggplot() +
    geom_histogram(aes(x = value, fill = Var1), alpha = 0.5) +
    scale_x_continuous(limits = c(-1, 1)) +
    labs(x = "Reliable Entropy")
})

output$relEntropyScatter <- renderPlotly({
  crp_metrics()$relentropy %>%
    reshape2::melt() %>%
    plot_ly(
      x = ~Var2, y = ~Var1, z = ~value, name = "Rel. Entropy", type = "heatmap",
      hovertemplate = "Device: %{y}<br>Challenge: %{x}<br>Rel. Entropy: %{z}"
    ) %>%
    layout(
      xaxis = list(title = "Challenge"), yaxis = list(title = "Device")
    )
})


# Export metrics ----------------------------------------------------------

crp_metrics <- reactive({
  compute_metrics(crp_table(), 1)
})


metricsTable <- eventReactive(input$tableColumns, {
  if (is.null(crp_table())) {
    return(NULL)
  }
  pufvis::create_summary(crp_metrics()) %>%
    as.data.frame() %>%
    select(!!!input$tableColumns)
})
output$metricsSummaryTable <- function() {
  metricsTable() %>%
    knitr::kable("html", caption = "Metrics summary") %>%
    kable_styling(full_width = TRUE)
}

output$metricsTableText <- renderPrint({
  metricsTable() %>%
    knitr::kable(
      format = input$metricsTableFormat,
      caption = "Summary of the PUF metrics",
      label = "puf_metrics",
      booktabs = TRUE
    )
})
