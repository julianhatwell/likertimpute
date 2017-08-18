set.seed(1020384)

my_vars <- c("strongly_agree", "neutral_to_agree"
             , "agree_peak", "agree_flat", "multimodal"
             , "strongly_disagree", "very_strongly_disagree"
             , "neutral_peak", "neutral_to_disagree"
             , "neutral_flat")

# take a quick look at the generating parameters
data("dewinter_dist")
dewinter_dist[my_vars, ]

# gen some data
dw <- synth_dewinter(var_names = my_vars
                     , dists = my_vars
                     , 2000)
d_fmla_agree <- as.formula(paste("~"
                                 , paste(my_vars[1:5], collapse = " + ")))
d_fmla_disagree <- as.formula(paste("~"
                                    , paste(my_vars[6:10], collapse = " + ")))

densityplot(d_fmla_agree
            , data = dw
            , par.settings = MyLatticeTheme
            , scales = MyLatticeScale
            , xlab = "Selection of De Winter distributions"
            , auto.key = list(columns = 2)
            , plot.points = FALSE)

densityplot(d_fmla_disagree
            , data = dw
            , par.settings = MyLatticeTheme
            , scales = MyLatticeScale
            , xlab = "Selection of De Winter distributions"
            , auto.key = list(columns = 2)
            , plot.points = FALSE)

# synthesise some missingness
dw_mnar1 <- dw
dw_mnar1$cov1 <- rpois(2000, 3)
dw_mnar2 <- dw_mnar1
dw_mnar1 <- synth_missing(dw_mnar1
                          , syn_control = missing_control(
                            pattern = "MNAR"
                            , method = "carpita"
                            , dep_cols = "cov1"
                            , unobs_cols = "cov1"
                            , prob = 0.1
                          )
                          , plot_probs = TRUE)

# cov1 has been deleted
names(dw_mnar1$data)
mv1_sorted <- missing_values(dw_mnar1$data) # order of missingness, ascending
mv1_sorted
sum(complete.cases(dw_mnar1$data))
# defaulted to use all other columns
dw_mnar1$syn_control$nr_cols
dw_mnar1$dplot$par.settings <- MyLatticeTheme
dw_mnar1$dplot
dw_mnar1$data_factors <- all_factor(dw_mnar1$data)
