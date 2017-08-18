# results comparisons
# wu_data_pop <- synth_wu_data(wu_data_model, 500000, seed = 100001)
# wu_stats_pop_sym <- wu_collect_stats(wu_data_pop$sym)
# wu_stats_pop_masym <- wu_collect_stats(wu_data_pop$masym)
# wu_stats_pop_sasym <- wu_collect_stats(wu_data_pop$sasym)
# save(wu_stats_pop_sym
#      , wu_stats_pop_masym
#      , wu_stats_pop_sasym
#      , file = "wu_stats_pop.RData")


# report_results <- format_results(results, wu_stats_pop_sym)
# rs_wu_sym_results <- report_results
# rs_wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# rs_wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# rs_wu_sym_results$bottom_2 <- bottom_2_count(rs_wu_sym_results)
# save(rs_wu_sym_results, file = "rs_wu_sym_results.RData")
# load("rs_wu_sym_results.RData") ; report_results <- rs_wu_sym_results

# report_results <- format_results(results, wu_stats_pop_sasym)
# rs_wu_sasym_results <- report_results
# rs_wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
# rs_wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
# rs_wu_sasym_results$bottom_2 <- bottom_2_count(rs_wu_sasym_results)
# save(rs_wu_sasym_results, file = "rs_wu_sasym_results.RData")
# load("rs_wu_sasym_results.RData") ; report_results <- rs_wu_sasym_results

# report_results <- format_results(results, wu_stats_pop_sym)
# dc_wu_sym_results <- report_results
# dc_wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# dc_wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# dc_wu_sym_results$bottom_2 <- bottom_2_count(dc_wu_sym_results)
# save(dc_wu_sym_results, file = "dc_wu_sym_results.RData")
# load("dc_wu_sym_results.RData") ; report_results <- dc_wu_sym_results
# 
# report_results <- format_results(results, wu_stats_pop_sasym)
# dc_wu_sasym_results <- report_results
# dc_wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
# dc_wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
# dc_wu_sasym_results$bottom_2 <- bottom_2_count(dc_wu_sasym_results)
# save(dc_wu_sasym_results, file = "dc_wu_sasym_results.RData")
# load("dc_wu_sasym_results.RData") ; report_results <- dc_wu_sasym_results

# report_results <- format_results(results, wu_stats_pop_sym)
# im_wu_sym_results <- report_results
# im_wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# im_wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# im_wu_sym_results$bottom_2 <- bottom_2_count(im_wu_sym_results)
# save(im_wu_sym_results, file = "im_wu_sym_results.RData")
# load("im_wu_sym_results.RData") ; report_results <- im_wu_sym_results

# report_results <- format_results(results, wu_stats_pop_sasym)
# im_wu_sasym_results <- report_results
# im_wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
# im_wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
# im_wu_sasym_results$bottom_2 <- bottom_2_count(im_wu_sasym_results)
# save(im_wu_sasym_results, file = "im_wu_sasym_results.RData")
# load("im_wu_sasym_results.RData") ; report_results <- im_wu_sasym_results

# report_results <- format_results(results, wu_stats_pop_sym)
# vr_wu_sym_results <- report_results
# vr_wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# vr_wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# vr_wu_sym_results$bottom_2 <- bottom_2_count(vr_wu_sym_results)
# save(vr_wu_sym_results, file = "vr_wu_sym_results.RData")
# load("vr_wu_sym_results.RData") ; report_results <- vr_wu_sym_results

# report_results <- format_results(results, wu_stats_pop_sasym)
# vr_wu_sasym_results <- report_results
# vr_wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
# vr_wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
# vr_wu_sasym_results$bottom_2 <- bottom_2_count(vr_wu_sasym_results)
# save(vr_wu_sasym_results, file = "vr_wu_sasym_results.RData")
# load("vr_wu_sasym_results.RData") ; report_results <- vr_wu_sasym_results

# report_results <- format_results(results, wu_stats_pop_sym)
# bm_wu_sym_results <- report_results
# bm_wu_sym_results$qdata <- format_qdata(report_results$q_measures)
# bm_wu_sym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sym)
# bm_wu_sym_results$bottom_2 <- bottom_2_count(bm_wu_sym_results)
# save(bm_wu_sym_results, file = "bm_wu_sym_results.RData")
# load("bm_wu_sym_results.RData") ; report_results <- bm_wu_sym_results

# report_results <- format_results(results, wu_stats_pop_sasym)
# bm_wu_sasym_results <- report_results
# bm_wu_sasym_results$qdata <- format_qdata(report_results$q_measures)
# bm_wu_sasym_results$ci_data <- format_cidata(report_results$results, wu_stats_pop_sasym)
# bm_wu_sasym_results$bottom_2 <- bottom_2_count(bm_wu_sasym_results)
# save(bm_wu_sasym_results, file = "bm_wu_sasym_results.RData")
# load("bm_wu_sasym_results.RData") ; report_results <- bm_wu_sasym_results

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
