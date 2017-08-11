# designMatrix
sample_sizes <- 1000
missing_prop <- 0.3

wu_missing_patt <- c("MCAR", "MAR", "MNAR")

wu_data_sets <- "sasym" # masym, # sym

wu_design_matrix <- expand.grid(ss = sample_sizes
                                , mprop = missing_prop
                                , mpatt = wu_missing_patt
                                , wtype = wu_data_sets
                                , stringsAsFactors = FALSE)

wu_labels <- sapply(1:nrow(wu_design_matrix), function(n) {
  return(paste("wu", wu_design_matrix[n, "wtype"]
               , wu_design_matrix[n, "mpatt"]
               , wu_design_matrix[n, "mprop"]
               , wu_design_matrix[n, "ss"]
               , sep = "_"))
})

row.names(wu_design_matrix) <- wu_labels

lik_imps <- c("PM", "CIM", "TW", "ICS")

# Step 1: Configure the test data
# covariates for missingness (create once, use for all)
set.seed(10001)
z1 <- rnorm(1500, 10, 2)
z2 <- rpois(1500, 5)
sum_z <- rowSums(cbind(z1, z2))

# the built-in split half function (psych package)
# doesn't give fine control over halves
split_half <- function(first_half, second_half
                       , spearman_brown = TRUE
                       , to_z = TRUE) {
  r <- cor(rowMeans(first_half), rowMeans(second_half))
  # Spearman-Brown reliability formula for a split half
  if (spearman_brown) r <- (2 * r) / (1 + r)
  if (to_z) r <- psych::fisherz(r)
  return(r)
}

format_results <- function(res, pop) {
  dataset_names <- names(res[[1]])
  variant_names <- names(res[[1]][[1]])
  stats_names <- names(res[[1]][[1]][[1]])
  qm_names <- names(quality_measures(1:5, 3))
  
  # unlist and create a matrix of all the results
  res_long <- expand.grid(replicate = 1:length(res)
                              , dataset = dataset_names
                              , variant = variant_names
                              , stats = stats_names
                              , stringsAsFactors = FALSE)
  
  res_long$value <- sapply(1:nrow(res_long), function(x) {
    res[[res_long[x, 1]]][[res_long[x, 2]]][[res_long[x, 3]]][[res_long[x, 4]]]
  })
  out <- list(results = res_long)
  if (!(missing(pop))) {
    
    res_groups <- unique(res_long[, 2:4])
    qml <- lapply(1:nrow(res_groups), function(x) {
      qm <- as.numeric(unlist(quality_measures(theta = with(res_long, 
                    res_long[dataset == res_groups$dataset[x] &
                             variant == res_groups$variant[x] &
                             stats == res_groups$stats[x], "value"])
                       , theta_p = if (is.null(pop[[res_groups$stats[x]]])) 0 else pop[[res_groups$stats[x]]])))
      data.frame(res_groups[rep(x, length(qm_names)), ]
        , q_measure = qm_names
        , value = qm, stringsAsFactors = FALSE)
    })
    qm_long = as.data.frame(data.table::rbindlist(qml))
    
    out$q_measures <- qm_long
  }
  return(out)
}
