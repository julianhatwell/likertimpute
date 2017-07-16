rm(list=ls())
library(arulesimp)
library(lavaan)
library(psych)
library(foreach)
library(doParallel)

source("run_experiments_header.R")
source("synth_wu_data.R")

collect_benchmarks <- function() {
  results <- list()
  for (ss in sample_sizes) {
    wu_data <- synth_wu_data(ss)
    for (wtype in wu_types) {
      dt <- wu_data[[wtype]]
      dt_cov <- cbind(dt, z1 = z1[1:ss], z2 = z2[1:ss], sum_z = sum_z[1:ss])
      for (mprop in missing_prop) {
        this_label <- paste0("wu_", wtype, "_mcar_", mprop, "_", ss)
        this_set <- synth_missing(dt
                                  , syn_control = missing_control(
                                    pattern = "MCAR"
                                    , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
                                    , prob = mprop))
        results[[this_label]] <- run_wu_benchmark(to_impute = this_set)
        
        # using MNAR to drop the sum_z col. Model is MAR
        # dep_cols z1 and sum(z1, z2)
        this_label <- paste0("wu_", wtype, "_mar_", mprop, "_", ss)
        this_set <- synth_missing(dt = dt_cov
          , syn_control = missing_control(
            pattern = "MNAR"
            , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
            , dep_cols = c("z1", "sum_z")
            , unobs_cols = "sum_z"
            , method = "wu_ranking"
            , prob = mprop))
        results[[this_label]] <-
          run_wu_benchmark(to_impute = this_set)


        this_label <- paste0("wu_", wtype, "_mnar_", mprop, "_", ss)
        this_set <- synth_missing(dt = dt_cov
          , syn_control = missing_control(
            pattern = "MNAR"
            , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
            , dep_cols = c("z1", "sum_z")
            , unobs_cols = c("z1", "z2", "sum_z")
            , method = "wu_ranking"
            , prob = mprop))
        results[[this_label]] <-
          run_wu_benchmark(to_impute = this_set)
      }
    }
  }
  return(results)
}

replicates <- 1000

# for parallel computation
cores=detectCores()
cl <- makeCluster(cores-1) #not to overload your computer
registerDoParallel(cl)

res <- foreach(i=1:replicates
               , .combine = list
               , .multicombine = TRUE
        , .packages = c("arulesimp", "lavaan", "psych")
        ) %dopar% {
  return(collect_benchmarks())
}

#stop cluster
stopCluster(cl)

