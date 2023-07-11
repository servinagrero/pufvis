# References
# https://github.com/gpilgrim2670/SwimMap/blob/master/app.R
# https://stackoverflow.com/questions/44504759/shiny-r-download-the-result-of-a-table
# https://stackoverflow.com/questions/14810409/how-to-save-plots-that-are-made-in-a-shiny-app

library(shiny)
library(dplyr)
library(abind)
library(pufr)

#' Convert raw response data into CRPs
#'
#' @param responses Data frame or array with the raw responses
#' @param algo Algorithm used to generate the CRPs from the responses.
#' @param ... Rest of the parameters to pass to the algorithm
#'
#' @returns A 3D array with the PUF CRPs
#'
#' @seealso [pufvis::algorithms]
#' @export
responses_to_crps <- function(responses, algo, ...) {
  data.frame()
}

#' Convert a CRP data frame into a CRP array
#'
#' @param crps Data frame containing the responses.
#'
#' @returns A 3D array with the PUF CRPs
#'
#' @export
df_to_crps <- function(crps) {
  ndevs <- max(crps$device)
  nchallenges <- max(crps$challenge)
  crp_mat <- crps %>%
    arrange(sample, device, challenge) %>%
    mutate(response = as.as.integer(response)) %>% # Reduce memory usage
    group_by(sample) %>%
    group_map(function(x, ...) t(matrix(x$response, nrow = nchallenges, ncol = ndevs, byrow = TRUE)))
  arr <- do.call(abind, c(crp_mat, along = 3))
  dimnames(arr) <- list(
    unique(crps$device),
    unique(crps$challenge),
    unique(crps$sample)
  )
  arr
}

#' Calculate all metrics of the given CRP array
#'
#' @param crp_table A 3D array containing the PUF CRPs
#' @param ref_sample Numeric or character index representing the reference sample
#'
#' @returns A list containing all PUF quality metrics
#'
#' @export
compute_metrics <- function(crp_table, ref_sample = 1) {
  crps <- crp_table[, , ref_sample]
  m <- list(
    unif = as.vector(pufr::crps_weight(crps, 1)),
    bitalias = as.vector(pufr::crps_weight(crps, 2)),
    uniq = pufr::uniqueness(crps),
    bitratio = apply(crps, 2, pufr::ratio_bits),
    rel = pufr::reliability(crp_table, ref_sample)
  )
  hba <- pufr::entropy_p(m$bitalias)
  helper <- function(dev_id) {
    hunif_bit <- ifelse(crps[dev_id, ] == 1, m$unif, 1 - m$unif)
    min(hunif_bit, hba) - entropy_p(m$rel[dev_id, ])
  }
  m$relentropy <- t(sapply(seq_len(nrow(crps)), helper))
  if (is.null(dimnames(crp_table))) {
    m$devices <- seq_len(dim(crp_table)[[1]])
    m$challenges <- seq_len(dim(crp_table)[[2]])
    m$samples <- seq_len(dim(crp_table)[[3]])
  } else {
    m$devices <- dimnames(crp_table)[[1]]
    m$challenges <- dimnames(crp_table)[[2]]
    m$samples <- dimnames(crp_table)[[3]]
  }
  class(m) <- "metrics"
  m
}

#' Check if CRPs are valid
#'
#' CRPs can be provided in a data frame or in an array.
#' If a data frame is provided, the following columns are expected:
#' - device, challenge, response, sample
#'
#' If an array is provided, the array needs to have 3 dimensions.
#'
#' @param crps Data frame or array containing the CRPs
#'
#' @returns Nothing if the CRPs are valid. If the CRPs are not valid, an error message containing the missing columns.
#'
#' @export
check_crps <- function(crps) {
  if (is.data.frame(crps)) {
    colnames <- c("device", "challenge", "response", "sample")
    req <- sapply(colnames, function(n) n %in% names(crps))
    if (any(!req)) {
      stop(paste("Missing fields:", paste(colnames[!req], collapse = ", ")))
    }
  } else if (is.array(crps)) {
    if (length(dim(crps)) != 3) {
      stop("CRP array needs at least 2 samples")
    }
  }
  return(TRUE)
}

#' @export
columns_summary <- c(names(summary(1)), "SD")

#' @export
create_summary <- function(metrics) {
  rel <- colMeans(metrics$rel)
  relentropy <- colMeans(metrics$relentropy)
  m <- rbind(
    c(as.vector(summary(metrics$unif)), sd(metrics$unif)),
    c(as.vector(summary(metrics$bitalias)), sd(metrics$bitalias)),
    c(as.vector(summary(metrics$uniq)), sd(metrics$uniq)),
    c(as.vector(summary(rel)), sd(rel)),
    c(as.vector(summary(metrics$ratio)), sd(metrics$ratio)),
    c(as.vector(summary(relentropy)), sd(relentropy))
  )
  rownames(m) <- c("Uniformity", "Bitaliasing", "Uniqueness", "Reliability", "Bit Ratio", "Reliable Entropy")
  colnames(m) <- pufvis::columns_summary
  m
}

#' Open the shiny application
#'
#' @param crps Array or dataframe containing the crps
#' @param algorithms List of algorithms used to create the CPRs from the raw responses
#' @param ... Rest of the arguments passed to `shiny::runApp`
#'
#' @export
pufvis <- function(crps = NULL, algos = algorithms, ...) {
  if (is.data.frame(crps)) {
    crp_table <<- reactiveVal(crps_to_arr(crps))
  } else if (is.array(crps)) {
    crp_table <<- reactiveVal(crps)
  } else {
    crp_table <<- reactiveVal()
  }

  pkg_app_dir <- system.file("application", package = "pufvis")
  file.copy(pkg_app_dir, tempdir(), recursive = TRUE)
  app_dir <- paste0(tempdir(), "/application")
  on.exit(unlink(app_dir, recursive = TRUE))

  try(runApp(appDir = app_dir, ...))
}

#' Export PUF metrics into an XLSX file
#'
#' @param file Path to the xlsx file
#' @param metrics The PUF quality metrics
#' @param ... Rest of the parameters to pass to the algorithm
#'
#' @returns The XLSX workbook
#' @export
metrics_to_excel <- function(file, metrics, ...) {
  devs <- metrics$devices
  pairs <- as.data.frame(RcppAlgos::comboGeneral(devs, 2)) %>%
    rename(First = "V1", Second = "V2")
  list(
    "Uniformity" = data.frame(
      Device = metrics$devices,
      Uniformity = metrics$unif
    ),
    "Bitaliasing" = data.frame(
      Challenge = metrics$challenges,
      Bitaliasing = metrics$bitalias
    ),
    "Uniqueness" = pairs %>% bind_cols(data.frame(
      Uniqueness = metrics$uniq
    )),
    "Reliability" = metrics$rel %>%
      reshape2::melt() %>%
      rename(Device = "Var1", Challenge = "Var2", Reliability = "value"),
    "BitRatio" = data.frame(
      Challenge = metrics$challenges,
      BitRatio = metrics$ratio
    ),
    "RelEntropy" = metrics$relentropy %>%
      reshape2::melt() %>%
      rename(Device = "Var1", Challenge = "Var2", RelEntropy = "value")
  ) %>%
    write.xlsx(file, asTable = TRUE)
}
