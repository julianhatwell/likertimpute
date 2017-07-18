# amelia
library(Amelia)
data("freetrade")
summary(freetrade)

# default listwise delection of lm
summary(lm(tariff~polity +
             pop +
             gdp.pc +
             year +
             country
           , data = freetrade))
# 60 observations deleted
# partially observed and containing valuable information

# include all vars, note the time series
a.out <- amelia(freetrade, m = 5, ts = "year", cs = "country")
a.out

hist(a.out$imputations[[3]]$tariff
     , col = "grey"
     , border = "white")
hist(freetrade$tariff
     , col = "grey"
     , border = "white")
range(freetrade$tariff, na.rm = TRUE)
# clearly an issue if
# MI imputes outside of acutal range
range(a.out$imputations[[3]]$tariff, na.rm = TRUE)
# negative tariff?

# save and write options
# save(a.out, file = "imputations.RData")
# write.amelia(obj = a.out, file.stem = "outdata")

# can combine multiple runs
a.out.more <- amelia(freetrade
                     , m = 10
                     , ts = "year"
                     , cs = "country"
                     , p2s = 0 )# less verbose
a.out.more
a.out.more <- ameliabind(a.out
                         , a.out.more)
a.out.more
# this allows many runs in parallel.

table(round(a.out$imputations[[3]]$polity
            , digits = 3))
# problem is, making a count data analysis
# on ordinal or nominal data.
# is it OK just to round?
a.out.ords <- amelia(freetrade
                     , m = 10
                     , ords = "polity"
                     , ts = "year"
                     , cs = "country"
                     , p2s = 0 )# less verbose
table(round(a.out.ords$imputations[[3]]$polity
            , digits = 3))

# nominal
table(round(a.out$imputations[[3]]$signed
            , digits = 3))
a.out.noms <- amelia(freetrade
                 , m = 5
                 , noms = "signed"
                 , ts = "year"
                 , cs = "country"
                 , p2s = 0) 
# uses dummy coding
table(round(a.out.noms$imputations[[3]]$signed
            , digits = 3))

# also available, log, sqrt and logistic

# use idvars to exclude id from imputation
# but keep it in the model
a.out.trans <- amelia(freetrade
        , m = 5       
        , idvars = "country"
        , noms = "signed"
        , ords = "polity"
        , ts = "year"
        , p2s = 0
)
hist(a.out.trans$imputations[[3]]$tariff
     , col = "grey"
     , border = "white")
# still got some minus numbers

# also available very good time series handling

# bounding - tariff 0 - 100
names(freetrade)
# tariff is column 3
bds <- matrix(c(3, 0, 100)
              , nrow = 1
              , ncol = 3)
a.out.bds <- amelia(freetrade
                    , m = 5       
                    , idvars = "country"
                    , noms = "signed"
                    , ords = "polity"
                    , ts = "year"
                    , p2s = 0
                    , bounds = bds  
)
hist(a.out.bds$imputations[[3]]$tariff
     , col = "grey"
     , border = "white")
hist(freetrade$tariff
     , col = "grey"
     , border = "white")
# note the inflation across the hist.
plot(a.out, which.vars = 3)
plot(a.out.bds, which.vars = 3)
plot(a.out, which.vars = 3:6)
plot(a.out.trans, which.vars = 3:6)
plot(a.out)
# looks like no signed may be imputed
# bad

compare.density(a.out, "signed")

# problems are with skew
# might need to transform var
overimpute(a.out, var = "tariff")
overimpute(a.out.bds, var = "tariff")

# convergence
disperse(a.out, dims = 1, m = 5)
disperse(a.out, dims = 2, m = 5)
disperse(a.out, dims = 3, m = 5)
disperse(a.out, dims = 4, m = 5)

missmap(a.out)

# analysis of amelia object
library(Zelig)
z.out <- zelig(tariff ~ polity +
                 pop +
                 gdp.pc +
                 year +
                 country
               , data = freetrade
               , model = "ls"
               , cite = FALSE)
summary(z.out)

# imputed
z.out.imp <- zelig(tariff ~ polity +
                 pop +
                 gdp.pc +
                 year +
                 country
               , data = a.out.trans$imputations
               , model = "ls"
               , cite = FALSE)
summary(z.out.imp)

# VIM
library(VIM)
# good information in vignette about other packages
# VIM vignette package demo using MCAR
# VIM is single imputation methods

# mice
dat <- airquality
dat[4:10,3] <- rep(NA,7)
dat[1:5,4] <- NA
dat <- dat[-c(5,6)]
summary(dat)

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(dat,2,pMiss)
apply(dat,1,pMiss)

library(mice)
md.pattern(dat)
# from VIM
aggr_plot <- aggr(dat
          , col = c('navyblue'
                    , 'red')
          , numbers = TRUE
          , sortVars = TRUE
          , labels = names(dat)
          , cex.axis = .7
          , gap = 3
          , ylab = c("Histogram of missing data","Pattern"))
marginplot(dat[c(1,2)])
marginplot(dat[c(1,3)])
marginplot(dat[c(1,4)])

pbox(dat)

tempData <- mice(dat
                 , m = 5
                 , maxit = 50
                 , meth = 'pmm'
                 , seed = 500)
summary(tempData)
completedData <- complete(tempData,1)

hist(dat$Ozone
     , col = "grey"
     , border = "white")
hist(completedData$Ozone
     , col = "grey"
     , border = "white")
library(lattice)
xyplot(tempData
       , Ozone ~ Wind +
         Temp +
         Solar.R
       , pch = 18
       , cex = 1
       , scales = "free")
densityplot(tempData)
# prob because not many values to impute
# in latter 3

stripplot(tempData
          , pch = 20
          , cex = 1.2)

modelFit1 <- with(tempData,lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit1))

# use a different random seed and compare
tempData2 <- mice(dat,m=50,seed=245435)
modelFit2 <- with(tempData2
                  , lm(Temp~ Ozone+Solar.R+Wind))
summary(pool(modelFit2))

library(arulesimp)
# more mice
iris.mis <- synth_missing(iris)

#Check missing values introduced in the data
summary(iris.mis)
str(iris.mis)

library(mice)
md.pattern(iris.mis$data)

library(VIM)
mice_plot <- aggr(iris.mis$data, col=c('navyblue','yellow'),
            numbers=TRUE, sortVars=TRUE,
            labels=names(iris.mis), cex.axis=.7,
            gap=3, ylab=c("Missing data","Pattern"))

iris.mis <- iris.mis$data[, -5]
imputed_Data <- mice(iris.mis
               , m = 5
               , maxit = 50
               , method = 'pmm'
               , seed = 500)
summary(imputed_Data)
fit <- with(data = imputed_Data
  , exp = lm(Sepal.Width ~ Sepal.Length + Petal.Width)) 

#combine results of all 5 models
combine <- pool(fit)
summary(combine)

library(Amelia)
amelia_fit <- amelia(iris.mis$data
                     , m = 5
                     , parallel = "multicore"
                     , noms = "Species")

#access imputed outputs
amelia_fit$imputations[[1]]
amelia_fit$imputations[[2]]
amelia_fit$imputations[[3]]
amelia_fit$imputations[[4]]
amelia_fit$imputations[[5]]

overimpute(amelia_fit, 1)
overimpute(amelia_fit, 2)
overimpute(amelia_fit, 3)
overimpute(amelia_fit, 4)
plot(amelia_fit)

library(missForest)
iris.imp <- missForest(iris.mis$data
                       )
iris.imp$ximp
iris.imp$OOBerror

# this is very unwieldy
library(Hmisc)
iris.mis <- synth_missing(iris, prob = 0.1)

# impute with mean value
iris.mis$imputed_seplen <-
  with(iris.mis
  , impute(Sepal.Length, mean))

# impute with random value
iris.mis$imputed_seplen2 <-
  with(iris.mis
  , impute(Sepal.Length, 'random'))

library(lattice)
densityplot(~Sepal.Length +
              imputed_seplen +
              imputed_seplen2
            , data = iris.mis
            , auto.key = TRUE)
#similarly you can use min, max, median to impute missing value

#using argImpute
impute_arg <- aregImpute(~
              Sepal.Length +
                Sepal.Width +
                Petal.Length +
                Petal.Width +
                Species
              , data = iris.mis
              , n.impute = 5)

impute_arg

# mi
library(mi)
library(arulesimp)
mi_data <- mi(iris.mis$data, seed = 335)
summary(mi_data)


data(nlsyV, package = "mi")
mdf <- missing_data.frame(nlsyV)
mdf
summary(mdf)
str(mdf)
mdf@patterns

show(mdf) # momrace and income are wrong
mdf <- change(mdf
      , y = c("income", "momrace")
      , what = "type"
      , to = c("non", "un"))

show(mdf) # changed income to non-neg
# and momrace to unordered

summary(mdf)
image(mdf)
hist(mdf)

rm(nlsyV) # good to remove large unnecessary objects to save RAM
options(mc.cores = 2)
imputations <- mi(mdf, n.iter = 30, n.chains = 4, max.minutes = 20)
show(imputations)

# check if they all converged roughly the same
round(mipply(imputations, mean, to.matrix = TRUE), 3)
Rhats(imputations)

# this runs for more iterations if necessary
# it wasn't in the first time it ran
# but that depends on random seed
imputations <- mi(imputations, n.iter = 5)

plot(imputations)
image(imputations)
summary(imputations)

analysis <- pool(ppvtr.36 ~ first + b.marr + income + momage + momed + momrace,
                 data = imputations, m = 5)
display(analysis) # no credible intervals?
dfs <- complete(imputations, m = 2) # not sure what this does

library(mitools)

sum(is.na(iris.mis$data$Species))

m <- 5
nc <- ncol(iris.mis)
iris.imp.list <-
for (i in 1:m) {
  assign(paste0("iris.imp", i)
         , VIM::kNN(iris.mis, k = i
                    , numFun = mean)[1:nc]
  )
  }
for (i in 1:m) {
  iris.imp.list[[i]] <- get(paste0("iris.imp", i))
}

iris.imp.list <- imputationList(iris.imp.list)
model1 <- with(iris.imp.list
, lm(Sepal.Length ~ Sepal.Width +
       Petal.Length + Petal.Width +
       Species))

iris.combi <- MIcombine(model1)
summary(iris.combi)
# also
beta <- MIextract(model1, fun=coef)
vars <- MIextract(model1, fun=vcov)
summary(MIcombine(beta,vars))

library(mi)
resp <- char_to_na(responses)
mi.info(resp)
resp.mi <- mi::mi(resp)




# TestDataImputation has a PersonMean and others

# maybe get alpha confidence intervals by bootstrapping psych package.
