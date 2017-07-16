
# designMatrix
sample_sizes <- c(500
                  # , 1000
                  , 1500)
missing_prop <- c(0.1
                  # , 0.2
                  , 0.3)
missing_patt <- c("MCAR", "MAR", "NRX", "NRXZ")
data_sets <- c("dewinter1", "dewinter2"
               , "wu_sym", "wu_masym", "wu_sasym"
               , "carpita_js")
wu_types <- c("sym", "masym", "sasym")

# stats to collect std.alpha, average_r, mean, sd

designMatrix <- expand.grid(sample_sizes
                            , missing_prop
                            , missing_patt
                            , data_sets)

lik_imps <- c("PM", "CIM", "TW", "ICS")

# Step 1: Configure the test data
# covariates for missingness (create once, use for all)
set.seed(10001)
z1 <- rnorm(1500, 10, 2)
z2 <- rpois(1500, 5)
sum_z <- rowSums(cbind(z1, z2))