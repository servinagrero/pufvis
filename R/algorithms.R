#' @export
algorithms <- list(
  list(
    name = "Quadratic",
    desc = "Create all possible combinations without repetition",
    callback = function(df, ...) {
      devs <- unique(df$device)
      samples <- unique(df$sample)
      nchallenges <- length(unique(df$challeng))
      arr_raw <- df %>%
        arrange(sample, device, challenge) %>%
        group_by(sample) %>%
        group_map(function(sample_df, ...) {
          matrix(sample_df, nrow = length(devs), ncol = nchallenges, byrow = TRUE)
        })
      arr <- do.call(abind, c(arr_raw, along = 3))
      # sapply(samples, function(s_id) arr[,,s_id])
      # RcppAlgos::comboGeneral(devs, 2)
      arr
    }
  ),
  list(
    name = "Split",
    desc = "Split the CRPs in two halves and create all combinations",
    callback = function(df, ...) {}
  ),
  list(
    name = "Group random",
    desc = "Select devices from random groups",
    callback = function(df, ...) {}
  )
)
