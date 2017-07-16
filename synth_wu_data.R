synth_wu_data <- function(sample_size, seed) {
  if (!(missing(seed))) set.seed(seed) # for reproducibility
  
  # this lavaan data model reproduces the configuration in Wu's paper
  wu_data_model <- "Factor1 =~ 0.75*A1 + 0.75*A2 + 0.75*A3 + 0.75*A4 + 0.75*A5 + 0.75*A6
  Factor2 =~ 0.65*B1 + 0.65*B2 + 0.65*B3 + 0.65*B4 + 0.65*B5 + 0.65*B6
  Factor1 ~~ 0.25*Factor2"
  
  # a function that will generate the requested sample size
  # already discretized and ready to go
  synth_wu <- function(model, cut = TRUE, rhum_dist = "symmetric", sample_size) {
    wu_data <- simulateData(model, sample.nobs=sample_size)
    if (cut) wu_data <- cut_rhemtulla(wu_data, dist = rhum_dist, levs = 7)
    
    return(wu_data)
  }
  
  wu_data_sym <- synth_wu(wu_data_model
                          , sample_size = sample_size)
  wu_data_masym <- synth_wu(wu_data_model
                            , rhum_dist = "moderate_asym"
                            , sample_size = sample_size)
  
  # do some manual cuts
  wu_data_sasym <- synth_wu(wu_data_model
                            , cut = FALSE
                            , sample_size = sample_size)
  wu_data_sasym[, c(1, 2, 6)] <- cut_rhemtulla(wu_data_sasym[, c(1, 2, 6)]
                                              , dist = "severe_asym"
                                              , levs = 7)
  wu_data_sasym[, 3:5] <- cut_rhemtulla(wu_data_sasym[, 3:5]
                                              , dist = "moderate_asym"
                                              , levs = 7)
  wu_data_sasym[, 7] <- cut_rhemtulla(wu_data_sasym[, 7]
                                              , dist = "severe_asym"
                                              , levs = 7)
  wu_data_sasym[, 11] <- cut_rhemtulla(wu_data_sasym[, 11]
                                              , dist = "moderate_asym"
                                              , levs = 7)
  wu_data_sasym[, c(8:10, 12)] <- cut_rhemtulla(wu_data_sasym[, c(8:10, 12)]
                                              , levs = 7)
  
  return(list(sym = wu_data_sym
              , masym = wu_data_masym
              , sasym = wu_data_sasym))
}

# collect stats function
wu_collect_stats <- function(dt) {
  alpha_A <- alpha(dt[,1:6])
  alpha_B <- alpha(dt[7:12])
  alpha_mix1 <- alpha(dt[c(1:3, 7:9)])
  alpha_mix2 <- alpha(dt[c(1:3, 10:12)])
  alpha_mix3 <- alpha(dt[c(4:6, 7:9)])
  
  scale_A <- rowSums(dt[, 1:6])
  scale_B <- rowSums(dt[, 7:12])
  linmod <- lm(scale_B~scale_A)
  
  return(list(
    alpha_A = alpha_A$total
    , alpha_B = alpha_B$total
    , alpha_mix1 = alpha_mix1$total
    , alpha_mix2 = alpha_mix2$total
    , alpha_mix3 = alpha_mix3$total
    , slope = coefficients(linmod)[2]
  ))
}

# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_benchmark <- function(to_impute) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  n <- length(lik_imps)
  benchmark <- list(
  alpha_A = numeric(n)
  , ave_r_A = numeric(n)
  , mean_A = numeric(n)
  , sd_A = numeric(n)
  , alpha_B = numeric(n)
  , ave_r_B = numeric(n)
  , mean_B = numeric(n)
  , sd_B = numeric(n)
  , alpha_mix1 = numeric(n)
  , ave_r_mix1 = numeric(n)
  , mean_mix1 = numeric(n)
  , sd_mix1 = numeric(n)
  , alpha_mix2 = numeric(n)
  , ave_r_mix2 = numeric(n)
  , mean_mix2 = numeric(n)
  , sd_mix2 = numeric(n)
  , alpha_mix3 = numeric(n)
  , ave_r_mix3 = numeric(n)
  , mean_mix3 = numeric(n)
  , sd_mix3 = numeric(n)
  , slope = numeric(n)
  )
  benchmark <- lapply(benchmark, function(x) {
    names(x) <- lik_imps
    x
  })
  # conduct imputations
  for (li in lik_imps) {
    bm <-
      wu_collect_stats(LikertImpute(
        ti_data, ti_mim
        , li, rounding = nd_round))
    benchmark <- within(benchmark , {
         alpha_A[li] <- bm$alpha_A$std.alpha
         ave_r_A[li] <- bm$alpha_A$average_r
         mean_A[li] <- bm$alpha_A$mean
         sd_A[li] <- bm$alpha_A$sd
         alpha_B[li] <- bm$alpha_B$std.alpha
         ave_r_B[li] <- bm$alpha_B$average_r
         mean_B[li] <- bm$alpha_B$mean
         sd_B[li] <- bm$alpha_B$sd
         alpha_mix1[li] <- alpha_mix1 <- bm$alpha_mix1$std.alpha
         ave_r_mix1[li] <- bm$alpha_mix1$average_r
         mean_mix1[li] <- bm$alpha_mix1$mean
         sd_mix1[li] <- bm$alpha_mix1$sd
         alpha_mix2[li] <- bm$alpha_mix2$std.alpha
         ave_r_mix2[li] <- bm$alpha_mix2$average_r
         mean_mix2[li] <- bm$alpha_mix2$mean
         sd_mix2[li] <- bm$alpha_mix2$sd
         alpha_mix3[li] <- bm$alpha_mix3$std.alpha
         ave_r_mix3[li] <- bm$alpha_mix3$average_r
         mean_mix3[li] <- bm$alpha_mix3$mean
         sd_mix3[li] <- bm$alpha_mix3$sd
         slope[li] <- bm$slope
         })
      }
  return(benchmark)
}

# testing
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
# li <- lik_imps[1]
# to_impute <- this_set
# ti_data <- to_impute$data
# ti_mim <- to_impute$mim
# bm <-
#   wu_collect_stats(LikertImpute(
#     ti_data, ti_mim
#     , li, rounding = nd_round))

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