library(pufr)
library(DT)
library(tidyverse)
library(ggplot2)
library(glue)
library(abind)

theme_set(
  theme_minimal(base_family = "Inter", base_size = 18) +
    theme(panel.grid.minor = element_blank())
)

#' @export
create_crps <- function(responses, algo, ...) {
  data.frame()
}

#' @param crps Data frame containing the responses.
#' @export
crps_to_arr <- function(crps) {
  ndevs <- max(crps$device)
  nchallenges <- max(crps$challenge)
  crp_mat <- crps %>%
    group_by(sample) %>%
    group_map(function(x, ...) t(matrix(x$response, nrow = nchallenges, ncol = ndevs, byrow = TRUE)))
  arr <- do.call(abind, c(crp_mat, along = 3))
  dimnames(arr) <- list(unique(crps$device), unique(crps$challenge), unique(crps$sample))
  arr
}

#' @export
calculate_metrics <- function(crp_table, ref_sample = 1) {
  crps <- crp_table[, , ref_sample]
  rel_fn <- function(v, ref = ref_sample) {
    sample_ids <- setdiff(seq_along(v), ref)
    intra_hd_fn <- function(i) {
      hamming_dist(v[ref], v[i], norm = TRUE)
    }
    return(1 - vapply(sample_ids, intra_hd_fn, numeric(1)))
  }

  m <- list(
    unif = as.vector(pufr::crps_weight(crps, 1)),
    bitalias = as.vector(pufr::crps_weight(crps, 2)),
    uniq = pufr::crps_uniqueness(crps),
    ratio = as.vector(apply(crps, 2, pufr::ratio_bits)),
    rel = t(sapply(
      seq_len(nrow(crps)),
      function(r) {
        sapply(
          seq_len(ncol(crps)),
          function(c) {
            mean(rel_fn(crp_table[r, c, ]))
          }
        )
      }
    ))
  )
  hba <- pufr::entropy_p(m$bitalias)
  m$relentropy <- t(sapply(seq_len(nrow(crps)), function(dev_id) {
    min(ifelse(crps[dev_id, ] == 1, m$unif, 1 - m$unif), hba) - pufr::entropy_p(m$rel[dev_id, ])
  }))
  m
}

# tibble(
#   device = rep(1:80, each = 500, times = 5),
#   challenge = rep(1:500, times = 80 * 5),
#   response = rbits(80 * 500 * 5),
#   sample = rep(1:5, each = 80 * 500)
# ) %>%
#   readr::write_csv2("./test_crps.csv")
#
# crps_df <- vroom::vroom("./test_crps.csv") %>%
#   group_by(sample, device) %>%
#   mutate(crp_id = seq_len(n()))
#


# reliability <- crps_df %>%
#   group_by(sample, crp_id) %>%
#   summarise(rel = mean(rel_fn(crp)))
#

#' @export
validate_df <- function(df) {
  names <- names(df)
  all(
    "device" %in% names,
    "challenge" %in% names,
    "response" %in% names,
    "sample" %in% names
  )
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
