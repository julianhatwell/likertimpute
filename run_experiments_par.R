rm(list=ls())
library(arulesimp)
library(lavaan)
library(psych)
library(foreach)
library(doParallel)
library(doRNG)

source("run_experiments_header.R")
source("synth_wu_data.R")

collect_benchmarks <- function() {
  
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

  results <- lapply(wu_labels, function(w) {
    run_wu_benchmark(to_impute = wu_data[[w]]
    , untreated = wu_data_master[[paste0("s", wu_design_matrix[w, "ss"])]][[wu_design_matrix[w, "wtype"]]]
                     )
  })
  names(results) <- wu_labels
  return(results)
}

set.seed(10101)
replicates <- 100

# for parallel computation
cl <- makeCluster(detectCores())
registerDoParallel(cl)

system.time(
  results <- foreach(i=1:replicates
                 , .combine = list
                 , .multicombine = TRUE
          , .packages = c("arulesimp", "lavaan", "psych")
          ) %dorng% {
    return(collect_benchmarks())
  }
)
#stop cluster
stopCluster(cl)


# collect_benchmarks <- function() {
#   results <- list()
#   for (ss in sample_sizes) {
#     wu_data <- synth_wu_data(ss)
#     for (wtype in wu_types) {
#       dt <- wu_data[[wtype]]
#       dt_cov <- cbind(dt, z1 = z1[1:ss], z2 = z2[1:ss], sum_z = sum_z[1:ss])
#       for (mprop in missing_prop) {
#         this_label <- paste0("wu_", wtype, "_mcar_", mprop, "_", ss)
#         this_set <- synth_missing(dt
#                                   , syn_control = missing_control(
#                                     pattern = "MCAR"
#                                     , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
#                                     , prob = mprop))
#         results[[this_label]] <-
#           run_wu_benchmark_alt(to_impute = this_set
#                                , untreated = dt)
#         
#         # using MNAR to drop the sum_z col. Model is MAR
#         # dep_cols z1 and sum(z1, z2)
#         this_label <- paste0("wu_", wtype, "_mar_", mprop, "_", ss)
#         this_set <- synth_missing(dt = dt_cov
#                                   , syn_control = missing_control(
#                                     pattern = "MNAR"
#                                     , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
#                                     , dep_cols = c("z1", "sum_z")
#                                     , unobs_cols = "sum_z"
#                                     , method = "wu_ranking"
#                                     , prob = mprop))
#         results[[this_label]] <-
#           run_wu_benchmark_alt(to_impute = this_set
#                                , untreated = dt)
#         
#         
#         this_label <- paste0("wu_", wtype, "_mnar_", mprop, "_", ss)
#         this_set <- synth_missing(dt = dt_cov
#                                   , syn_control = missing_control(
#                                     pattern = "MNAR"
#                                     , nr_cols = c(paste0(c("A"), 1:3), paste0("B", 1:3))
#                                     , dep_cols = c("z1", "sum_z")
#                                     , unobs_cols = c("z1", "z2", "sum_z")
#                                     , method = "wu_ranking"
#                                     , prob = mprop))
#         results[[this_label]] <-
#           run_wu_benchmark_alt(to_impute = this_set
#                                , untreated = dt)
#       }
#     }
#   }
#   return(results)
# }
