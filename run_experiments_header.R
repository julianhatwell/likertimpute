
# designMatrix
sample_sizes <- c(500
                  , 1500
                  )
missing_prop <- c(0.1
                  , 0.3
                  )

missing_patt <- c("MCAR", "MAR", "NRX", "NRXZ")
wu_missing_patt <- c("MCAR", "MAR", "MNAR")

data_sets <- c("dewinter1", "dewinter2"
               , "carpita_js")
wu_data_sets <- c("sym", "masym", "sasym")

designMatrix <- expand.grid(sample_sizes
                            , missing_prop
                            , missing_patt
                            , data_sets)

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
split_half <- function(first_half, second_half) {
  r <- cor(rowMeans(first_half), rowMeans(second_half))
  # Spearman-Brown reliability formula for a split half
  return((2 * r) / (1 + r))
}