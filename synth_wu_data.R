# this lavaan data model reproduces the configuration in Wu's paper
wu_data_model <- "Factor1 =~ 0.75*A1 + 0.75*A2 + 0.75*A3 + 0.75*A4 + 0.75*A5 + 0.75*A6
Factor2 =~ 0.65*B1 + 0.65*B2 + 0.65*B3 + 0.65*B4 + 0.65*B5 + 0.65*B6
Factor1 ~~ 0.3*Factor2"

# collect stats function
wu_collect_stats <- function(dt, untreated, mim) {
  alpha_A <- psych::alpha(dt[, 1:6])
  alpha_B <- psych::alpha(dt[, 7:12])
  # the halves that were subject to missingness
  alpha_mix <- psych::alpha(dt[c(1:3, 7:9)])
  
  # split half reliability
  splith_A <- split_half(dt[, 1:3], dt[, 4:6])
  splith_B <- split_half(dt[, 7:9], dt[, 10:12])
  
  # slope of B~A
  scale_A <- rowSums(dt[, 1:6])
  scale_B <- rowSums(dt[, 7:12])
  linmod <- lm(scale_B~scale_A)
  
  results <- list(
    alpha_A = alpha_A$total$std.alpha
    , ave_r_A = alpha_A$total$average_r
    , mean_A = alpha_A$total$mean
    , sd_A = alpha_A$total$sd
    , splith_A = splith_A
    , alpha_B = alpha_B$total$std.alpha
    , ave_r_B = alpha_B$total$average_r
    , mean_B = alpha_B$total$mean
    , sd_B = alpha_B$total$sd
    , splith_B = splith_B
    , alpha_mix = alpha_mix$total$std.alpha
    , ave_r_mix = alpha_mix$total$average_r
    , mean_mix = alpha_mix$total$mean
    , sd_mix = alpha_mix$total$sd
    , slope = coefficients(linmod)[2]
  )
  if (!missing(untreated) && !missing(mim)) {
    # scale score errors
    unt_scale_A <- rowSums(untreated[, 1:6])
    
    sse_A <- sum(scale_A - unt_scale_A) /
      length(unique(unlist(mim$B_comp_j[1:3])))
    
    unt_scale_B <- rowSums(untreated[, 7:12])
    
    sse_B <- sum(scale_B - unt_scale_B) /
      length(unique(unlist(mim$B_comp_j[7:9])))
  
    results$sse_A <- sse_A
    results$sse_B <- sse_B
  }
  return(results)
}

# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_benchmark <- function(to_impute, untreated) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim

  # conduct imputations
  benchmark <- sapply(lik_imps, function(li) {
    wu_collect_stats(LikertImpute(
      ti_data, ti_mim
      , li, rounding = nd_round)
      , untreated = untreated
      , mim = ti_mim)    
  })
  return(benchmark)
}

# # testing
# ss <- sample_sizes[1]
# wtype <- wu_types[1]
# wu_data <- synth_wu_data(ss)
# dt <- wu_data[[wtype]]
# mprop <- missing_prop[1]
# this_label <- paste0("wu_", wtype, "_mcar_", mprop, "_", ss)
# this_set <- synth_missing(dt
#                           , syn_control = missing_control(
#                             pattern = "MCAR"
#                             , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
#                             , prob = mprop))
# li <- lik_imps[4]
# to_impute <- this_set
# ti_data <- to_impute$data
# ti_mim <- to_impute$mim
# imputed <- LikertImpute(
#   ti_data, ti_mim
#   , li, rounding = round)
# any(imputed[, c(1:3, 7:9)] < 1 || imputed[, c(1:3, 7:9)] > 7)
# bm <-
#   wu_collect_stats(imputed
#     , untreated = dt
#     , mim = this_set$mim)
# 
# wu_collect_stats(dt)

# # Population stats
# wu_data_pop <- synth_wu_data(500000, seed = 100001)
# 
# wu_stats_pop_sym <- wu_collect_stats(wu_data_pop$sym)
# wu_stats_pop_masym <- wu_collect_stats(wu_data_pop$masym)
# wu_stats_pop_sasym <- wu_collect_stats(wu_data_pop$sasym)
# 
# 
# save(wu_stats_pop_sym
#      , wu_stats_pop_masym
#      , wu_stats_pop_sasym
#      , file = "wu_stats_pop.RData")