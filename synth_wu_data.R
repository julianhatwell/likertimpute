# this lavaan data model reproduces the configuration in Wu's paper
wu_data_model <- "Factor1 =~ 0.75*A1 + 0.75*A2 + 0.75*A3 + 0.75*A4 + 0.75*A5 + 0.75*A6
Factor2 =~ 0.65*B1 + 0.65*B2 + 0.65*B3 + 0.65*B4 + 0.65*B5 + 0.65*B6
Factor1 ~~ 0.3*Factor2"

non_response_cols <- paste0(rep(c("A", "B"), each = 3), 1:3)

# collect stats function
wu_collect_stats <- function(dt, untreated, mim) {
  
  if (!(missing(mim))) {
    tot_case <- nrow(dt)
    bef_imp_cc <- length(mim$i_none_missing)
    aft_imp_cc <- sum(complete.cases(dt))
    imp_case <- aft_imp_cc - bef_imp_cc
    
    tot_attr <- nrow(dt) * ncol(dt)
    bef_imp_attr <- tot_attr - length(unlist(mim$A_comp_i))
    aft_imp_attr <- tot_attr - sum(missing_values(dt))
    imp_attr <- aft_imp_attr - bef_imp_attr  
  }
  
  cc <- complete.cases(dt)
  dt <- dt[cc, ]
  
  alpha_A <- psych::alpha(dt[, 1:6])
  alpha_B <- psych::alpha(dt[, 7:12])
  # the halves that were subject to missingness
  alpha_mix <- psych::alpha(dt[c(1:3, 7:9)])
  
  ci_alpha_A <- arulesimp::alpha_ci(alpha_A$total$std.alpha
                              , n = sample_sizes
                              , p = 7
                              , include_a = FALSE)
  ci_alpha_B <- arulesimp::alpha_ci(alpha_B$total$std.alpha
                              , n = sample_sizes
                              , p = 7
                              , include_a = FALSE)
  
  ci_mean_A <- arulesimp::t_ci(alpha_A$total$mean
                                    , st_dev = alpha_A$total$sd
                                    , n = sample_sizes
                                    , include_s = FALSE)
  ci_mean_B <- arulesimp::t_ci(alpha_B$total$mean
                               , st_dev = alpha_B$total$sd
                               , n = sample_sizes
                               , include_s = FALSE)
  
  # split half reliability
  splith_A <- split_half(dt[, 1:3], dt[, 4:6])
  splith_B <- split_half(dt[, 7:9], dt[, 10:12])
  splith_mix <- split_half(dt[, 1:3], dt[, 7:9])
  
  results <- list(
    alpha_A = alpha_A$total$std.alpha
    , alpha_A_ci_lower = ci_alpha_A[1]
    , alpha_A_ci_upper = ci_alpha_A[2]
    , ave_r_A = alpha_A$total$average_r
    , mean_A = alpha_A$total$mean
    , sd_A = alpha_A$total$sd
    , mean_A_ci_lower = ci_mean_A[1]
    , mean_A_ci_upper = ci_mean_A[2]
    , splith_A = splith_A
    , alpha_B = alpha_B$total$std.alpha
    , alpha_B_ci_lower = ci_alpha_B[1]
    , alpha_B_ci_upper = ci_alpha_B[2]
    , ave_r_B = alpha_B$total$average_r
    , mean_B = alpha_B$total$mean
    , sd_B = alpha_B$total$sd
    , mean_B_ci_lower = ci_mean_B[1]
    , mean_B_ci_upper = ci_mean_B[2]
    , splith_B = splith_B
    , alpha_mix = alpha_mix$total$std.alpha
    , ave_r_mix = alpha_mix$total$average_r
    , mean_mix = alpha_mix$total$mean
    , sd_mix = alpha_mix$total$sd
    , splith_mix = splith_mix
  )
  
  if (!missing(untreated) && !missing(mim)) {
    # scale totals
    scale_A <- rowSums(dt[, 1:6])
    scale_B <- rowSums(dt[, 7:12])
    
    # scale score errors
    unt_scale_A <- rowSums(untreated[cc, 1:6])
    mase_A <- sum(abs(scale_A - unt_scale_A)) / imp_case
    
    unt_scale_B <- rowSums(untreated[cc, 7:12])
    mase_B <- sum(abs(scale_B - unt_scale_B)) / imp_case
  
    results$mase_A <- mase_A
    results$mase_B <- mase_B
    
    # ability
    
    results$ability_case <- imp_case / (tot_case - bef_imp_cc)
    results$ability_attr <- imp_attr / (tot_attr - bef_imp_attr)
  }
  return(results)
}

run_wu_ruleselect <- function(to_impute, untreated, results_only = TRUE) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  
  mv_sorted <- missing_values(ti_mim)
  c_control <- cars_control(support = 0.02
                            , confidence = 0.2, sort_by = "confidence"
  )
  cars_conf <- make_cars(ti_factor
                         , c_control = c_control
                         , var_names = names(mv_sorted))
  
  imputations <- list(bestrule = ARImpute(cars_conf, ti_factor
                                   , ari_control = 
                                     arulesimp_control(
                                       method = "best_rule"
                                       , use_default_classes = 0))
                   , topnm3 = ARImpute(cars_conf, ti_factor
                                        , ari_control = 
                                          arulesimp_control(
                                            method = "top_n_mean"
                                            , top_n = 3
                                            , use_default_classes = 0
                                            , weighted_chisq = FALSE
                                          ))
                   , topnm7 = ARImpute(cars_conf, ti_factor
                                       , ari_control = 
                                         arulesimp_control(
                                           method = "top_n_mean"
                                           , top_n = 7
                                           , use_default_classes = 0
                                           , weighted_chisq = FALSE
                                         ))
                   , topnm = ARImpute(cars_conf, ti_factor
                                       , ari_control = 
                                         arulesimp_control(
                                           method = "top_n_mean"
                                           , use_default_classes = 0
                                           , weighted_chisq = FALSE
                                         ))
                   , topnmjv3 = ARImpute(cars_conf, ti_factor
                                       , ari_control = 
                                         arulesimp_control(
                                           method = "top_n_majv"
                                           , top_n = 3
                                           , use_default_classes = 0
                                           , weighted_chisq = FALSE
                                         ))
                   , topnmjv7 = ARImpute(cars_conf, ti_factor
                                       , ari_control = 
                                         arulesimp_control(
                                           method = "top_n_majv"
                                           , top_n = 7
                                           , use_default_classes = 0
                                           , weighted_chisq = FALSE
                                         ))
                   , topnmjv = ARImpute(cars_conf, ti_factor
                                         , ari_control = 
                                           arulesimp_control(
                                             method = "top_n_majv"
                                             , use_default_classes = 0
                                             , weighted_chisq = FALSE
                                           ))
                   , rhsfreq3 = ARImpute(cars_conf, ti_factor
                                          , ari_control = 
                                            arulesimp_control(
                                              method = "consequent_frequency"
                                              , top_n = 3
                                              , use_default_classes = 0
                                              , weighted_chisq = FALSE
                                            ))
                   , rhsfreq7 = ARImpute(cars_conf, ti_factor
                                         , ari_control = 
                                           arulesimp_control(
                                             method = "consequent_frequency"
                                             , top_n = 7
                                             , use_default_classes = 0
                                             , weighted_chisq = FALSE
                                           ))
                   , rhsfreq = ARImpute(cars_conf, ti_factor
                                         , ari_control = 
                                           arulesimp_control(
                                             method = "consequent_frequency"
                                             , use_default_classes = 0
                                             , weighted_chisq = FALSE
                                           ))
                   )
  
  results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  if (results_only) {
    return(results)
  } else {
    imputations$untreated <- untreated
    return(imputations)
  }
}

run_wu_defclass <- function(to_impute, untreated) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  
  mv_sorted <- missing_values(ti_mim)
  c_control <- cars_control(support = 0.02
                            , confidence = 0.2, sort_by = "confidence"
  )
  cars_conf <- make_cars(ti_factor
                         , c_control = c_control
                         , var_names = names(mv_sorted))
  
  imputations <- list(bestruledc1 = ARImpute(cars_conf, ti_factor
                                           , ari_control = 
                                             arulesimp_control(
                                               method = "best_rule"
                                               , use_default_classes = 1
                                               , weighted_chisq = FALSE
                                             ))
                      , bestruledc2 = ARImpute(cars_conf, ti_factor
                                             , ari_control = 
                                               arulesimp_control(
                                                 method = "best_rule"
                                                 , use_default_classes = 2
                                                 , weighted_chisq = FALSE
                                               ))
                      )
  
  results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  return(results)
}

run_wu_intmeasure <- function(to_impute, untreated) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  mv_sorted <- missing_values(ti_mim)

  c_control <- cars_control(support = 0.02
                            , confidence = 0.2
                            , sort_by = "confidence"
  )
  cars_conf <- make_cars(ti_factor
                        , c_control = c_control
                        , var_names = names(mv_sorted))
  
  c_control$sort_by <- "laplace"
  cars_lap <- make_cars(ti_factor
                        , c_control = c_control
                        , var_names = names(mv_sorted))
  
  c_control$sort_by <- "chiSquared"
  cars_chi <- make_cars(ti_factor
                        , c_control = c_control
                        , var_names = names(mv_sorted))
  
  imputations <- list(bestdc1conf = ARImpute(cars_conf, ti_factor
                                              , ari_control =
                                                arulesimp_control(
                                                  method = "best_rule"
                                                  , use_default_classes = 1
                                                  , weighted_chisq = FALSE
                                                ))
                      , bestdc1lap = ARImpute(cars_lap, ti_factor
                                            , ari_control =
                                              arulesimp_control(
                                                method = "best_rule"
                                                , use_default_classes = 1
                                                , weighted_chisq = FALSE
                                              ))
                      , bestdc1chi = ARImpute(cars_chi, ti_factor
                                                 , ari_control =
                                                   arulesimp_control(
                                                     method = "best_rule"
                                                     , use_default_classes = 1
                                                     , weighted_chisq = FALSE
                                                   ))
                      , bestdc1wchi = ARImpute(cars_chi, ti_factor
                                            , ari_control =
                                              arulesimp_control(
                                                method = "best_rule"
                                                , use_default_classes = 1
                                                , weighted_chisq = TRUE
                                              ))
                      )
  
  results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  return(results)
}

# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_variant <- function(to_impute, untreated) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  mv_sorted <- missing_values(ti_mim)
  
  ti_combi <- ord_combi_expand(ti_data[1:12]
                               , likert_scales = 
                                 list(A = names(ti_data[1:6])
                                      , B = names(ti_data[7:12]))
                               , keep_orig = TRUE)
  if (ncol(ti_data) == 14) ti_combi <- cbind(ti_combi, ti_data[, 13:14])
  ti_combi_fac <- all_factor(ti_combi)
  
  c_control <- cars_control(support = 0.02
                            , confidence = 0.2
                            , sort_by = "chiSquared"
  )
  cars_chi <- make_cars(ti_factor
                        , c_control = c_control
                        , var_names = names(mv_sorted))
  
  co_control <- cars_control(support = 0.05
                             , confidence = 0.1
                             , sort_by = "chiSquared"
  )
  co_cars <- make_cars(ti_combi_fac
                       , c_control = co_control
                       , var_names = names(mv_sorted))
  
  ari_control <- arulesimp_control(method = "best_rule"
                        , use_default_classes = 1
                        , weighted_chisq = FALSE)
  
  imputations <- list(bestdc1chi = ARImpute(cars_chi, ti_factor
                                            , ari_control = ari_control)
                      , bestdc1chico = ARImpute(cars_chi, ti_combi_fac
                                                 , ari_control = ari_control)[1:12]
                      , ibestdc1chi = ARImpute_iter(ti_data
                                               , missing_values(ti_data)
                                               , iter_control =
                                                 iteration_control(
                                                   method = "none"
                                                   , max_iter = 5
                                                   , target_convergence = 18)
                                               , ari_control = ari_control
                                               , c_control = c_control)
                      , ibestdc1chico = ARImpute_iter(ti_combi
                                                     , missing_values(ti_data)
                                                     , iter_control =
                                                       iteration_control(
                                                         method = "none"
                                                         , max_iter = 5
                                                         , target_convergence = 18)
                                                     , ari_control = ari_control
                                                     , c_control = co_control)[1:12]
                      , ibestdc1chip = ARImpute_iter(ti_data
                                               , missing_values(ti_data)
                                               , iter_control =
                                                 iteration_control(
                                                   method = "propensity"
                                                   , class_balance = list(method = "both")
                                                   , splits = 2
                                                   , max_iter = 5
                                                   , target_convergence = 36)
                                               , ari_control = ari_control
                                               , c_control = c_control)
                      , ibestdc1chipco = ARImpute_iter(ti_combi
                                                      , missing_values(ti_data)
                                                      , iter_control =
                                                        iteration_control(
                                                          method = "propensity"
                                                          , class_balance = list(method = "both")
                                                          , splits = 2
                                                          , max_iter = 5
                                                          , target_convergence = 36)
                                                      , ari_control = ari_control
                                                      , c_control = co_control)[1:12]
                      )
 results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  return(results)
}

# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_benchmark <- function(to_impute, untreated) {
  
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  mv_sorted <- missing_values(ti_mim)
  
  ti_combi <- ord_combi_expand(ti_data[1:12]
                               , likert_scales = 
                                 list(A = names(ti_data[1:6])
                                      , B = names(ti_data[7:12]))
                               , keep_orig = TRUE)
  if (ncol(ti_data) == 14) ti_combi <- cbind(ti_combi, ti_data[, 13:14])
  ti_combi_fac <- all_factor(ti_combi)
  
  c_control <- cars_control(support = 0.02
                            , confidence = 0.2
                            , sort_by = "chiSquared"
  )
  co_control <- cars_control(support = 0.05
                             , confidence = 0.1
                             , sort_by = "chiSquared"
  )
  ari_control <- arulesimp_control(method = "best_rule"
                                   , use_default_classes = 1
                                   , weighted_chisq = FALSE)
  
  # conduct imputations
  imputations <- list(
              PM = LikertImpute(
                ti_data, ti_mim
                , "PM", rounding = nd_round)
              , CIM = LikertImpute(
                ti_data, ti_mim
                , "CIM", rounding = nd_round)
              , TW = LikertImpute(
                ti_data, ti_mim
                , "TW", rounding = nd_round)
              , ICS = LikertImpute(
                ti_data, ti_mim
                , "TW", rounding = nd_round)
              , ibestc1chip = ARImpute_iter(ti_data
                                              , missing_values(ti_data)
                                              , iter_control =
                                                iteration_control(
                                                  method = "propensity"
                                                  , class_balance = list(method = "both")
                                                  , splits = 2
                                                  , max_iter = 5
                                                  , target_convergence = 36)
                                              , ari_control = ari_control
                                              , c_control = c_control)
              )
  results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })

  return(results)
}

run_wu_ibenchmark <- function(to_impute, untreated, mi_mult = "rubin", results_only = TRUE) {
  if(!(mi_mult %in% c("rubin", "white"))) stop("mi_mult should be either \"rubin\" or \"white\"")
  
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  mv_sorted <- missing_values(ti_mim)
  
  ti_combi <- ord_combi_expand(ti_data[1:12]
                               , likert_scales = 
                                 list(A = names(ti_data[1:6])
                                      , B = names(ti_data[7:12]))
                               , keep_orig = TRUE)
  if (ncol(ti_data) == 14) ti_combi <- cbind(ti_combi, ti_data[, 13:14])
  ti_combi_fac <- all_factor(ti_combi)
  
  c_control <- cars_control(support = 0.02
                            , confidence = 0.2
                            , sort_by = "chiSquared"
  )
  ari_control <- arulesimp_control(method = "best_rule"
                                   , use_default_classes = 1
                                   , weighted_chisq = FALSE)
  
  imputations <- list(
    PM = LikertImpute(
      ti_data, ti_mim
      , "PM", rounding = nd_round)
    , CIM = LikertImpute(
      ti_data, ti_mim
      , "CIM", rounding = nd_round)
    , TW = LikertImpute(
      ti_data, ti_mim
      , "TW", rounding = nd_round)
    , ICS = LikertImpute(
      ti_data, ti_mim
      , "TW", rounding = nd_round)
  )
  results <- lapply(imputations, function(rout) {
    
    res <- as.data.frame(sapply(rout, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })  
  
  # multiple imputation settings
  if (mi_mult == "white") {
    mi_runs <- round(to_impute$syn_control$prob * 100)
  } else {
    if (to_impute$syn_control$prob <= 0.15) mi_runs <- 3
    if (to_impute$syn_control$prob > 0.15) mi_runs <- 5
  }
  
  # conduct imputations
  ari.out <- list()
  for (n in 1:mi_runs) {
    ari.out[[n]] <- 
      ARImpute_iter(ti_data
                    , missing_values(ti_data)
                    , iter_control =
                      iteration_control(
                        method = "propensity"
                        , class_balance = list(method = "both")
                        , splits = 2
                        , max_iter = 5
                        , target_convergence = 36)
                    , ari_control = ari_control
                    , c_control = c_control)
  }
  isarbi.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(ari.out[[n]], untreated, ti_mim)
  })
  headers <- names(isarbi.stats[[1]])
  
  mean_params <- function(x) { 
    res <- lapply(headers, function(h) {
      mean(unlist(lapply(1:mi_runs, function(n)
        x[[n]][[h]])))
    })
    names(res) <- headers
    return(res)
  }
  
  results$ibestdc1chip <- mean_params(isarbi.stats)
  
  # multiple imputation methods
  amelia.out <- amelia(ti_data, p2s = 0
                       , ords = non_response_cols
                       , m = mi_runs)
  amelia.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(amelia.out$imputations[[n]], untreated, ti_mim)
  })
  
  results$amelia <- mean_params(amelia.stats)
  
  mice.out <- mice(ti_data, print = FALSE
                   , m = mi_runs, meth = "pmm")
  mice.stats <- lapply(1:mi_runs, function(n) {
    wu_collect_stats(mice::complete(mice.out, n)
                     , untreated, ti_mim)
  })
  results$mice <- mean_params(mice.stats)
  
  if (results_only) {
    return(results)
  } else {
    imputations$ari <- ari.out
    imputations$amelia <- amelia.out$imputations
    imputations$mice <- lapply(1:mi_runs, function(n) {
      mice::complete(mice.out, n)})
    imputations$untreated <- untreated
    return(imputations)
  }
}

mi_mult = "rubin"

collect_wu_benchmarks <- function(stage, seed, results_only = TRUE) {
  
  wu_data_master <- lapply(sample_sizes
                           , synth_wu_data
                           , model = wu_data_model
                           , seed = seed)
  ssize <- paste0("s", sample_sizes)
  names(wu_data_master) <- ssize
  
  wu_data <- lapply(wu_labels, function(w) {
    dt <- wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
    dt <- if (wu_design_matrix[w, "mpatt"] == "MCAR") {
      dt
    } else {
      cbind(dt, z1 = z1[1:wu_design_matrix[w, "ss"]]
            , z2 = z2[1:wu_design_matrix[w, "ss"]]
            , sum_z = sum_z[1:wu_design_matrix[w, "ss"]])
    }
  })
  names(wu_data) <- wu_labels  
  wu_data <- lapply(wu_labels, function(w) {
    if (wu_design_matrix[w , "mpatt"] == "MCAR") {
      synth_missing(wu_data[[w]]
                    , syn_control = missing_control(
                      pattern = "MCAR"
                      , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                      , prob = wu_design_matrix[w, "mprop"]))
    } else {
      if (wu_design_matrix[w , "mpatt"] == "MAR") {
        synth_missing(wu_data[[w]]
                      , syn_control = missing_control(
                        # using MNAR to drop the sum_z col. Model is MAR
                        # dep_cols z1 and sum(z1, z2)
                        pattern = "MNAR"
                        , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                        , dep_cols = c("z1", "sum_z")
                        , unobs_cols = "sum_z"
                        , method = "wu_ranking"
                        , prob = wu_design_matrix[w, "mprop"]))
      } else {
        if (wu_design_matrix[w , "mpatt"] == "MNAR") {
          synth_missing(wu_data[[w]]
                        , syn_control = missing_control(
                          pattern = "MNAR"
                          , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                          , dep_cols = c("z1", "sum_z")
                          , unobs_cols = c("z1", "z2", "sum_z")
                          , method = "wu_ranking"
                          , prob = wu_design_matrix[w, "mprop"]))
        }
      }
    }
  })
  # they lose their names in the last operation
  names(wu_data) <- wu_labels
  
  # special case for MAR with covariates
  mar <- wu_data[[grep("MAR", wu_labels)]]$data[, 1:12]
  mn <- min(sapply(mar, as.numeric), na.rm = TRUE)
  mx <- max(sapply(mar, as.numeric), na.rm = TRUE)
  num_classes <- mx - (mn - 1)
  mar_cov <- lapply(wu_data[[grep("MAR", wu_labels)]]$data[, 13:14], function(mc) {
    as.numeric(cut(mc, breaks = num_classes))
  })
  wu_data[[grep("MAR", wu_labels)]]$data[, 13:14] <- mar_cov
  
  if (stage == 1) {
    results <- lapply(wu_labels, function(w) {
      run_wu_ruleselect(to_impute = wu_data[[w]]
                        , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
                        , results_only)
    })  
  }
  if (stage == 2) {
    results <- lapply(wu_labels, function(w) {
      run_wu_defclass(to_impute = wu_data[[w]]
                         , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })
  }
  if (stage == 3) {
    results <- lapply(wu_labels, function(w) {
      run_wu_intmeasure(to_impute = wu_data[[w]]
                      , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })
  }
  if (stage == 4) {
    results <- lapply(wu_labels, function(w) {
      run_wu_variant(to_impute = wu_data[[w]]
                        , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })
  }
  
  if (stage == 5) {
    results <- lapply(wu_labels, function(w) {
      run_wu_benchmark(to_impute = wu_data[[w]]
                       , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
      )
    })
  }
  if (stage == 6) {
    results <- lapply(wu_labels, function(w) {
      run_wu_ibenchmark(to_impute = wu_data[[w]]
                       , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
                       , mi_mult = mi_mult
                       , results_only
      )
    })
  }
  names(results) <- wu_labels
  return(results)
}

format_qdata <- function(qm) {
  qdata <- qm
  qdata <- subset(qdata
                  , qdata$q_measure %in%
                    c("mean", "st_err", "mae", "rel_bias"))
  qdata <- tidyr::spread(qdata, key = q_measure, value = value)
  qdata <- qdata[, c("dataset", "variant", "stats", "mean", "st_err", "rel_bias", "mae")]
  qdata_ci <- t(sapply(1:nrow(qdata), function(x) {
    t_ci(stat = qdata$mean[x], st_dev = qdata$st_err[x], n = sample_sizes, include_s = FALSE)
  }))
  colnames(qdata_ci) <- c("ci_lower", "ci_upper")
  qdata <- cbind(qdata, qdata_ci)
  return(qdata)
}

format_cidata <- function(res, dc = TRUE) {
  dataset_names <- unique(res$dataset)
  variant_names <- unique(res$variant)
  stats_names <- unique(res$stats)
  if (dc) {
    stats_names <- 
      stats_names[!(stats_names %in% c("ability_case"
                                     , "ability_attr" ))]
    }
  
  cis <- lapply(dataset_names, function(ds) {
    vrn <- lapply(variant_names, function(v) {
      sts <- lapply(stats_names, function(sts) {
        tt <- with(res
                   , t.test(res[dataset == ds &
                                       variant == v &
                                       stats == sts, "value"]))
        tt <- c(mn = as.numeric(tt$estimate)
                , ci_lwr = tt$conf.int[1]
                , ci_upr = tt$conf.int[2])
        tt
      })
      names(sts) <- stats_names
      sts
    })
    names(vrn) <- variant_names
    vrn
  })
  names(cis) <- dataset_names
  
  # unlist and create a matrix of all the results
  cis_long <- expand.grid(dataset = dataset_names
                          , variant = variant_names
                          , stats = stats_names
                          , stringsAsFactors = FALSE)
  
  cis_long$mean <- sapply(1:nrow(cis_long), function(x) {
    cis[[cis_long[x, 1]]][[cis_long[x, 2]]][[cis_long[x, 3]]]["mn"]
  })
  cis_long$ci_lwr <- sapply(1:nrow(cis_long), function(x) {
    cis[[cis_long[x, 1]]][[cis_long[x, 2]]][[cis_long[x, 3]]]["ci_lwr"]
  })
  cis_long$ci_upr <- sapply(1:nrow(cis_long), function(x) {
    cis[[cis_long[x, 1]]][[cis_long[x, 2]]][[cis_long[x, 3]]]["ci_upr"]
  })
  
  return(cis_long)
}

top_2_count <- function(res) {
  v_names <- unique(res$results$variant)
  top_2 <- numeric(length(v_names))
  names(top_2) <- v_names
  
  b1 <- by(res$qdata[, "rel_bias"]
           , res$qdata[, c("dataset", "variant", "stats")]
           , function(x) return(x))
  # stats mae
  top_2 <- 
    table(sapply(1:3, function(d) {
      apply(b1[d, , c("alpha_A", "alpha_B"
                      , "mean_A", "mean_B"
                      , "sd_A", "sd_B"
                      , "splith_mix")]
            , 2
            , function(x) {
              names(head(sort(abs(x)), n = 2))
            })
    }))[v_names]
  top_2 <- ifelse(is.na(top_2), 0, top_2)
  names(top_2) <- v_names
  
  return(top_2)
}

bottom_2_count <- function(res) {
  v_names <- unique(res$results$variant)
  bottom_2 <- numeric(length(v_names))
  names(bottom_2) <- v_names
  
  b1 <- by(res$qdata[, "rel_bias"]
           , res$qdata[, c("dataset", "variant", "stats")]
           , function(x) return(x))
  # stats mae
  bottom_2 <- 
    table(sapply(1:3, function(d) {
      apply(b1[d, , c("alpha_A", "alpha_B"
                      , "mean_A", "mean_B"
                      , "sd_A", "sd_B"
                      , "splith_mix")]
            , 2
            , function(x) {
              names(tail(sort(abs(x)), n = 2))
            })
    }))[v_names]
  bottom_2 <- ifelse(is.na(bottom_2), 0, bottom_2)
  names(bottom_2) <- v_names
  
  return(bottom_2)
}


bottom_2_count_old <- function(res) {
  v_names <- unique(res$results$variant)
  bottom_2 <- numeric(length(v_names))
  names(bottom_2) <- v_names
  
  b1 <- by(res$qdata[, "mae"]
           , res$qdata[, c("dataset", "variant", "stats")]
           , function(x) return(x))
  b2 <- by(res$qdata[, "mean"]
           , res$qdata[, c("dataset", "variant", "stats")]
           , function(x) return(x))
  
  b3 <- by(res$ci_data[, "alpha_A_cover"]
           , res$ci_data[, c("dataset", "variant")]
           , function(x) return(x))
  
  b4 <- by(res$ci_data[, "alpha_B_cover"]
           , res$ci_data[, c("dataset", "variant")]
           , function(x) return(x))
  
  b5 <- by(res$ci_data[, "mean_A_cover"]
           , res$ci_data[, c("dataset", "variant")]
           , function(x) return(x))
  
  b6 <- by(res$ci_data[, "mean_B_cover"]
           , res$ci_data[, c("dataset", "variant")]
           , function(x) return(x))
  
  
  # stats mae
  this <- 
    table(sapply(1:3, function(d) {
      apply(b1[d, , c("alpha_A", "alpha_B"
                      , "mean_A", "mean_B"
                      , "sd_A", "sd_B"
                      , "splith_mix")]
            , 2
            , function(x) {
              names(tail(sort(x), n = 2))
            })
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  # mean mase
  this <- 
    table(sapply(1:3, function(d) {
      apply(b2[d, , c("mase_A", "mase_B")]
            , 2
            , function(x) {
              names(tail(sort(x), n = 2))
            })
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  # mean ability
  this <-
    table(sapply(1:3, function(d) {
      apply(b2[d, , c("ability_attr", "ability_case")]
            , 2
            , function(x) {
              names(tail(sort(x, decreasing = TRUE), n = 2))
            })
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  # ci cover
  this <-
    table(sapply(1:3, function(d) {
      names(tail(sort(b3[d, ]), n = 2))
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  this <- 
    table(sapply(1:3, function(d) {
      names(tail(sort(b4[d, ]), n = 2))
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  this <-
    table(sapply(1:3, function(d) {
      names(tail(sort(b5[d, ]), n = 2))
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  this <-
    table(sapply(1:3, function(d) {
      names(tail(sort(b6[d, ]), n = 2))
    }))[v_names]
  this <- ifelse(is.na(this), 0, this)
  names(this) <- v_names
  bottom_2 <- bottom_2 + this
  
  return(bottom_2)
}
