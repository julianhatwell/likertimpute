# results comparisons
# wu_data_pop <- synth_wu_data(wu_data_model, 500000, seed = 100001)
# wu_stats_pop_sym <- wu_collect_stats(wu_data_pop$sym)
# wu_stats_pop_masym <- wu_collect_stats(wu_data_pop$masym)
# wu_stats_pop_sasym <- wu_collect_stats(wu_data_pop$sasym)
# save(wu_stats_pop_sym
#      , wu_stats_pop_masym
#      , wu_stats_pop_sasym
#      , file = "wu_stats_pop.RData")
load("wu_stats_pop.RData")

wu_data <- synth_wu_data(wu_data_model, sample_sizes)
mn <- min(wu_data$sym)
mx <- max(wu_data$sym)
num_classes <- mx - (mn - 1)

# report_results <- format_results(results, wu_stats_pop_sym)
# wu_sym_results <- report_results
# wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# save(wu_sym_results, file = "wu_sym_results.RData")

report_results <- format_results(results, wu_stats_pop_sasym)
wu_sasym_results <- report_results
wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
save(wu_sasym_results, file = "wu_sasym_results.RData")


# if not want to run experiments again
# load("wu_results.RData")








source("C:\\Dev\\Study\\R\\R_Themes\\MarketingTheme.R")
MyTempTheme <- MyLatticeTheme
MyTempTheme$superpose.symbol$col <- myPal
MyTempTheme$add.line$col <- myPalDark[3]


lattice::dotplot(factor(variant)~value | factor(stats)
                   , groups = factor(dataset)
                   , data = subset(report_results$q_measures
                      , stats %in% stats_names[c(1, 6)] &
                        q_measure == "mean")
                   , scales = "free"
                   , par.settings = MyTempTheme
                   , strip = MyLatticeStrip
                   , auto.key = list(columns = 3)
                   , panel = function(x, y, ...) {
                     panel.dotplot(x, y, ...)
                     panel.abline(
                       v = unlist(pop_stats[stats_names[c(1, 6)[panel.number()]]]))
                   })

lattice::dotplot(factor(variant)~value | factor(stats)
                 , groups = factor(dataset)
                 , data = subset(report_results$q_measures
                                 , stats %in% stats_names[c(19, 20)] &
                                   q_measure == "mean")
                 , scales = "free"
                 , par.settings = MyTempTheme
                 , strip = MyLatticeStrip
                 , auto.key = list(columns = 3)
                 , panel = function(x, y, ...) {
                   panel.dotplot(x, y, ...)
                   if (!(is.null(unlist(pop_stats[stats_names[c(19, 20)[panel.number()]]])))) {
                     panel.abline(
                       v = unlist(pop_stats[stats_names[c(19, 20)[panel.number()]]]))
                   }
                  })






stats_names[c(1, 3, 4, 6, 8, 9)[panel.number()]][[1]]

lattice::stripplot(factor(variant)~value | factor(stats)
                   , groups = factor(dataset)
                   , data = subset(report_results$results
                                   , stats %in%
                                     stats_names[c(15, 17, 18, 19, 20)])
                   , jitter.data = TRUE
                   , scales = "free"
                   , par.settings = MyTempTheme
                   , strip = MyLatticeStrip
                   , auto.key = list(columns = 3)
                   , panel = function(x, y, ...) {
                     panel.stripplot(x, y, ...)
                     panel.abline(
                       v = pop_stats[stats_names[c(15, 17, 18, 19, 20)[panel.number()]]][[1]])
                   })

lattice::dotplot(factor(variant)~value
                 , groups = factor(dataset)
  , data = subset(report_results$q_measures
                  , stats == "alpha_A" & q_measure == "mean")
  , par.settings = MyTempTheme
  , strip = MyLatticeStrip
  , auto.key = list(columns = 3)
)



g <- ggplot(data = subset(report_results$results
                 , stats %in%
                   stats_names[c(1, 3, 4, 6, 8, 9)])
            , aes(x = value
                  , y = variant
                  , colour = dataset
                  )
) +
  facet_wrap(~stats, scales = "free") +
  geom_point() +
  myGgTheme
g

g <- ggplot(data = subset(report_results$q_measures
                          , stats %in% stats_names[c(19, 20)] &
                            q_measure == "mean")
            , aes(x = value
                  , y = variant
                  , colour = dataset
            )
) +
  facet_wrap(~stats, scales = "free") +
  geom_point(size = 2) +
  myGgTheme
g

pop_stats_names <- names(pop_stats)
pops <- data.frame(stats = pop_stats_names, value = unlist(pop_stats))

g <- ggplot(data = subset(report_results$q_measures
                          , stats %in% stats_names[c(1, 6)] &
                            q_measure == "mean")
            , aes(x = value
                  , y = variant
                  , colour = dataset
            )
) +
  facet_wrap(~stats, scales = "free") +
  geom_point(size = 2) +
  geom_vline(data = subset(pops
            , stats %in% c("alpha_A", "alpha_B"))
            , aes(xintercept = value)
            , colour = myPalDark[3]
            , linetype = "dotted") +
  myGgTheme
g
