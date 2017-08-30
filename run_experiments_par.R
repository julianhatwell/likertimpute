rm(list=ls())
library(arulesimp)
library(arules) # does not export method for as(x, "transactions)
library(lavaan)
library(psych)
library(Amelia)
library(mice)
library(foreach)
library(doParallel)
library(doRNG)

source("run_experiments_header.R")
source("synth_wu_data.R")

# clear previous results
# do not run!! think carefully
# rm(list = ls()[grep("results", ls())][-1])

replicates <- 4 # mininum 2
runs <- 25

k <- 0
while (k < replicates * runs) {
  seed <- round(runif(1, 0,1) * 100000000)
  set.seed(seed)
  
  # for parallel computation
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  
  tm <- system.time(
    results <- foreach(i=1:replicates
                   , .combine = list
                   , .multicombine = TRUE
                   , .packages = c("arulesimp", "arules"
                                   , "lavaan", "psych"
                                   , "Amelia", "mice")
    ) %dorng% {
      # return(collect_wu_benchmarks(stage = 1, seed = seed))
      # return(collect_wu_benchmarks(stage = 2, seed = seed))
      # return(collect_wu_benchmarks(stage = 3, seed = seed))
      # return(collect_wu_benchmarks(stage = 4, seed = seed))
      # return(collect_wu_benchmarks(stage = 5, seed = seed))
      return(collect_wu_benchmarks(stage = 6, seed = seed))
    }
  )
  #stop cluster
  stopCluster(cl)
  
  assign(paste0("results_", seed), results)
  k <- sum(sapply(ls()[grep("^results", ls())][-1], function(res) {
    length(get(res))
  }))
  print(paste("done", k, "replicates"))
  print(tm)
}

# check
ls()[grep("^results", ls())][-1]

results <- list()
results <- append(results, sapply(ls()[grep("^results", ls())][-1], get))

load("wu_stats_pop.RData")

wu_data <- synth_wu_data(wu_data_model, sample_sizes)
mn <- min(wu_data$sym)
mx <- max(wu_data$sym)
num_classes <- mx - (mn - 1)
