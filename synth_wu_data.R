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

run_wu_ruleselect <- function(to_impute, untreated) {
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
  return(results)
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
  
  imputations <- list(bestrule = ARImpute(cars_conf, ti_factor
                                      , ari_control = 
                                        arulesimp_control(
                                          method = "best_rule"
                                          , use_default_classes = 1))
                      , topnm3 = ARImpute(cars_conf, ti_factor
                                          , ari_control = 
                                            arulesimp_control(
                                              method = "top_n_mean"
                                              , top_n = 3
                                              , use_default_classes = 1
                                              , weighted_chisq = FALSE
                                            ))
                      , topnm7 = ARImpute(cars_conf, ti_factor
                                          , ari_control = 
                                            arulesimp_control(
                                              method = "top_n_mean"
                                              , top_n = 7
                                              , use_default_classes = 1
                                              , weighted_chisq = FALSE
                                            ))
                      , topnmjv3 = ARImpute(cars_conf, ti_factor
                                            , ari_control = 
                                              arulesimp_control(
                                                method = "top_n_majv"
                                                , top_n = 3
                                                , use_default_classes = 1
                                                , weighted_chisq = FALSE
                                              ))
                      , topnmjv7 = ARImpute(cars_conf, ti_factor
                                            , ari_control = 
                                              arulesimp_control(
                                                method = "top_n_majv"
                                                , top_n = 7
                                                , use_default_classes = 1
                                                , weighted_chisq = FALSE
                                              ))
                      , rhsfreq = ARImpute(cars_conf, ti_factor
                                           , ari_control = 
                                             arulesimp_control(
                                               method = "consequent_frequency"
                                               , use_default_classes = 1
                                               , weighted_chisq = FALSE
                                             ))
                      , bestrule_dc = ARImpute(cars_conf, ti_factor
                                           , ari_control = 
                                             arulesimp_control(
                                               method = "best_rule"
                                               , use_default_classes = 2))
                      , topnm3_dc = ARImpute(cars_conf, ti_factor
                                             , ari_control = 
                                               arulesimp_control(
                                                 method = "top_n_mean"
                                                 , top_n = 3
                                                 , use_default_classes = 2
                                                 , weighted_chisq = FALSE
                                               ))
                      , topnm7_dc = ARImpute(cars_conf, ti_factor
                                             , ari_control = 
                                               arulesimp_control(
                                                 method = "top_n_mean"
                                                 , top_n = 7
                                                 , use_default_classes = 2
                                                 , weighted_chisq = FALSE
                                               ))
                      , topnmjv3_dc = ARImpute(cars_conf, ti_factor
                                               , ari_control = 
                                                 arulesimp_control(
                                                   method = "top_n_majv"
                                                   , top_n = 3
                                                   , use_default_classes = 2
                                                   , weighted_chisq = FALSE
                                                 ))
                      , topnmjv7_dc = ARImpute(cars_conf, ti_factor
                                               , ari_control = 
                                                 arulesimp_control(
                                                   method = "top_n_majv"
                                                   , top_n = 7
                                                   , use_default_classes = 2
                                                   , weighted_chisq = FALSE
                                                 ))
                      , rhsfreq_dc = ARImpute(cars_conf, ti_factor
                                              , ari_control = 
                                                arulesimp_control(
                                                  method = "consequent_frequency"
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
                            , confidence = 0.2, sort_by = "confidence"
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
  
  best_cars_list <- list(best_conf = cars_conf
                         , best_lap = cars_lap
                         , best_chi = cars_chi)
  bestrule <- sapply(best_cars_list, function(cl) {
    res <- ARImpute(cl, ti_factor)
    res <- as.data.frame(sapply(res, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  
  bestrule_wchi <- ARImpute(cars_chi, ti_factor
                        , ari_control = 
                          arulesimp_control(
                            method = "best_rule"
                            , use_default_classes = TRUE
                            , weighted_chisq = TRUE
                          ))
  bestrule_wchi <- as.data.frame(sapply(best_wchi, function(b) {
    if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
  }))
  bestrule_wchi <- unlist(wu_collect_stats(best_wchi
                                       , untreated = untreated
                                       , mim = ti_mim))
  
  return(cbind(bestrule, bestrule_wchi))
}


# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_variants <- function(to_impute, untreated) {
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim
  ti_factor <- all_factor(ti_data)
  
  c_control$sort_by <- "chiSquared"
  cars_chi <- make_cars(ti_factor
                , c_control = c_control
                , var_names = names(mv_sorted))
  
  ari_best <- 
    arulesimp_control(
      method = "best_rule"
      , use_default_classes = TRUE
      , weighted_chisq = TRUE
    )
  ari_topnm <-
    arulesimp_control(
      method = "best_rule"
      , use_default_classes = TRUE
      , weighted_chisq = TRUE
    )
  
  
  bestrule <- sapply(best_cars_list, function(cl) {
    res <- ARImpute(cl, ti_factor)
    res <- as.data.frame(sapply(res, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  
  topnm_cars_list <- list(topnm_conf = cars_conf
                         , topnm_lap = cars_lap
                         , topnm_chi = cars_chi)
  top_n_mean <- sapply(topnm_cars_list, function(cl) {
    res <- ARImpute(cl, ti_factor
                    , ari_control =
                      arulesimp_control(
                        method = "top_n_mean"
                        , top_n = 10
                        , use_default_classes = TRUE
                      ))
    res <- as.data.frame(sapply(res, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  topnmjv_cars_list <- list(topnmjv_conf = cars_conf
                          , topnmjv_lap = cars_lap
                          , topnmjv_chi = cars_chi)
  top_n_majv <- sapply(topnmjv_cars_list, function(cl) {
    res <- ARImpute(cl, ti_factor
                    , ari_control =
                      arulesimp_control(
                        method = "top_n_mean"
                        , top_n = 10
                        , use_default_classes = TRUE
                      ))
    res <- as.data.frame(sapply(res, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  
  cf_cars_list <- list(cf_conf = cars_conf
                            , cf_lap = cars_lap
                            , cf_chi = cars_chi)
  cons_freq <- sapply(cf_cars_list, function(cl) {
    res <- ARImpute(cl, ti_factor
                    , ari_control =
                      arulesimp_control(
                        method = "consequent_frequency"
                        , use_default_classes = TRUE
                      ))
    res <- as.data.frame(sapply(res, function(b) {
      if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
    }))
    res <- wu_collect_stats(res
                            , untreated = untreated
                            , mim = ti_mim)
    return(res)
  })
  
  bestrule_wchi <- ARImpute(cars_chi, ti_factor
                        , ari_control = 
                          arulesimp_control(
                            method = "best_rule"
                            , use_default_classes = TRUE
                            , weighted_chisq = TRUE
                            ))
  bestrule_wchi <- as.data.frame(sapply(best_wchi, function(b) {
    if ("factor" %in% class(b)) as.numeric(as.character(b)) else b
  }))
  bestrule_wchi <- unlist(wu_collect_stats(best_wchi
                          , untreated = untreated
                          , mim = ti_mim))
  
  return(cbind(bestrule, top_n_mean, top_n_majv, cons_freq, bestrule_wchi))
}

# function to run the imputations and gather results
# takes an object produced by synth_missing
run_wu_benchmark <- function(to_impute, untreated, mi_mult = "rubin") {
  if(!(mi_mult %in% c("rubin", "white"))) stop("mi_mult should be either \"rubin\" or \"white\"")
  ti_data <- to_impute$data
  ti_mim <- to_impute$mim

  # conduct benchmark imputations
  benchmark <- sapply(lik_imps, function(li) {
    wu_collect_stats(LikertImpute(
      ti_data, ti_mim
      , li, rounding = nd_round)
      , untreated = untreated
      , mim = ti_mim)    
  })
  
  # conduct multiple imputations
  if (mi_mult == "white") {
    mi_runs <- round(to_impute$syn_control$prob * 100)
  } else {
    if (to_impute$syn_control$prob <= 0.15) mi_runs <- 3
    if (to_impute$syn_control$prob > 0.15) mi_runs <- 5
  }
  
  amelia.out <- amelia(ti_data, p2s = 0
                  , ords = non_response_cols
                  , m = mi_runs)
  amelia.stats <- unlist(lapply(1:mi_runs, function(n) {
    wu_collect_stats(amelia.out$imputations[[n]], untreated, ti_mim)
  }))
  headers <- unique(names(amelia.stats))
  mean_params <- function(x) {
    res <- sapply(headers, function(h) {
      mean(x[names(x) == h])
    })
    return(res)
  }
  
  amelia.results <- mean_params(amelia.stats)
  
  mice.out <- mice(ti_data, print = FALSE
                   , m = mi_runs, meth = "pmm")
  mice.stats <- unlist(lapply(1:mi_runs, function(n) {
    wu_collect_stats(mice::complete(mice.out, n)
                     , untreated, ti_mim)
  }))
  mice.results <- mean_params(mice.stats)
  
  return(cbind(benchmark
        , amelia = amelia.results
        , mice = mice.results))
}

mi_mult = "rubin"

collect_wu_benchmarks <- function(stage) {
  
  wu_data_master <- lapply(sample_sizes, synth_wu_data, model = wu_data_model)
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
                        , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })  
  }
  if (stage == 2) {
    results <- lapply(wu_labels, function(w) {
      run_wu_intmeasures(to_impute = wu_data[[w]]
                         , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })
  }
  if (stage == 3) {
    results <- lapply(wu_labels, function(w) {
      run_wu_variants(to_impute = wu_data[[w]]
                      , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]])
    })
  }
  if (stage == 4) {
    results <- lapply(wu_labels, function(w) {
      run_wu_benchmark(to_impute = wu_data[[w]]
                       , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
                       , mi_mult = mi_mult
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

format_cidata <- function(res, pop) {
  
  ci_data <- report_results$results
  ci_data <- subset(ci_data
                    , stats %in% c("alpha_A_ci_lower"
                                   , "alpha_A_ci_upper"
                                   , "mean_A_ci_lower"
                                   , "mean_A_ci_upper"
                                   , "alpha_B_ci_lower"
                                   , "alpha_B_ci_upper"
                                   , "mean_B_ci_lower"
                                   , "mean_B_ci_upper"))
  ci_data <- tidyr::spread(ci_data, key = stats, value = value)
  ci_groups <- unique(ci_data[, c("dataset", "variant")])
  ci_data <- lapply(1:nrow(ci_groups), function(x, pop) {
    all_reps <- ci_data[ci_data$dataset == ci_groups$dataset[x] &
                          ci_data$variant == ci_groups$variant[x], ]
    alpha_A_cover <- ci_cover(
      all_reps[, c("alpha_A_ci_lower", "alpha_A_ci_upper")]
      , pop$alpha_A)
    alpha_B_cover <- ci_cover(
      all_reps[, c("alpha_B_ci_lower", "alpha_B_ci_upper")]
      , pop$alpha_B)
    mean_A_cover <- ci_cover(
      all_reps[, c("mean_A_ci_lower", "mean_A_ci_upper")]
      , pop$mean_A)
    mean_B_cover <- ci_cover(
      all_reps[, c("mean_B_ci_lower", "mean_B_ci_upper")]
      , pop$mean_B)
    
    return(data.frame(
      dataset = ci_groups$dataset[x]
      , variant = ci_groups$variant[x]
      , alpha_A_cover = alpha_A_cover
      , alpha_B_cover = alpha_B_cover
      , mean_A_cover = mean_A_cover
      , mean_B_cover = mean_B_cover
      , stringsAsFactors = FALSE
    ))
    
  }, pop = pop)
  
  ci_data <- as.data.frame(data.table::rbindlist(ci_data))
  return(ci_data)
}
