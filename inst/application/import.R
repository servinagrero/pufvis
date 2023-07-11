observeEvent(input$crpsFile, {
  ext <- tools::file_ext(input$crpsFile$name)
  crps <- switch(ext,
    csv = vroom::vroom(input$crpsFile$datapath),
    tsv = vroom::vroom(input$crpsFile$datapath, delim = "\t"),
    rds = readr::read_rds(input$crpsFile$datapath),
    validate("Invalid file; Please upload a csv, tsv or rds file")
  )
  tryCatch(
    {
      pufvis::check_crps(crps)
      if (is.data.frame(crps)) {
        crp_table(df_to_crps(crps))
      } else {
        crp_table(crps)
      }
      shinyalert("Sucess",
        "CRP data loaded successfully.",
        type = "success"
      )

      samples <- dimnames(crp_table())[[3]]
      updateSelectInput(
        session,
        "refSampleHeatmap",
        choices = samples,
        selected = samples[[1]]
      )
    },
    error = function(err) {
      shinyalert("Problem uploading CRPs",
        err$message,
        type = "error"
      )
    }
  )
})

observeEvent(input$rawData, {
  ext <- tools::file_ext(input$rawData$name)
  responses <- switch(ext,
    csv = vroom::vroom(input$crpsFile$datapath),
    tsv = vroom::vroom(input$crpsFile$datapath, delim = "\t"),
    rds = readr::read_rds(input$crpsFile$datapath),
    validate("Invalid file; Please upload a csv, tsv or rds file")
  )
  tryCatch(
    {
      crps_df <- pufvis::create_crps(responses, input$crpAlgorithm)
      crp_table(crps_df)
      shinyalert("Sucess",
        "Responses loaded",
        type = "success"
      )
    },
    error = function(err) {
      shinyalert("Problem uploading responses",
        err$message,
        type = "error"
      )
    }
  )
})

output$algoExplanation <- eventReactive(input$crpAlgorithm, {
  pufvis::algorithms[[grep(input$crpAlgorithm, pufvis::algorithms)]]$desc
})
