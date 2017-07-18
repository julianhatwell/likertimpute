library(ggplot2)
# reliability analysis
temp <- tempfile()
download.file("http://personality-testing.info/_rawdata/BIG5.zip", temp, mode="wb")
perstest <- read.table(unz(temp, "BIG5/data.csv"), header = TRUE, sep="\t")
unlink(temp); rm(temp)

# first 500, Extraverson questions
d <- perstest[1:500, paste0("E", 1:10)]
str(d)

# some are reverse scored
d[, paste0("E"
  , c(2, 4, 6, 8, 10))] <- 6 -
  d[, paste0("E", c(2, 4, 6, 8, 10))]

# Average inter-item correlation
library(corrr)
d %>% correlate() # cor(d) will do
inter_item <- d %>%
  correlate() %>%
  select(-rowname) %>%
  colMeans(na.rm = TRUE)
inter_item

mean(inter_item)

data.frame(inter_item) %>% 
  ggplot(aes(x = inter_item)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(inter_item), color = "red") +
  xlab("Mean inter-item correlation") +
  theme_bw()

# Average item-total correlation
d$score <- rowMeans(d) # could be sums
head(d)
item_total <- d %>%
  correlate() %>%
  focus(score)
item_total

mean(item_total$score)
item_total %>% 
  ggplot(aes(x = score)) +
  geom_histogram(bins = 10, alpha = .5) +
  geom_vline(xintercept = mean(item_total$score), color = "red") +
  xlab("Mean item-total correlation") +
  theme_bw()

# Cronbach's alpha
d$score <- NULL  # delete the score column we made earlier
psych::alpha(d)

# Split-half reliability
# (adjusted using the Spearman–Brown prophecy formula)
# Calculating total score...
score_e <- rowMeans(d[, c(TRUE, FALSE)])  # with even items
score_o <- rowMeans(d[, c(FALSE, TRUE)])  # with odd items

# Correlating scores from even and odd items
r <- cor(score_e, score_o)
r
# Adjusting with the Spearman-Brown prophecy formula
(2 * r) / (1 + r)

# Composite reliability
library(lavaan)
# Define the model
items <- paste(names(d), collapse = "+")
model <- paste("extraversion", items, sep = "=~")
model

# Fit the model
fit <- cfa(model, data = d)

# extract standardised factor loadings
sl <- standardizedSolution(fit)
sl <- sl$est.std[sl$op == "=~"]
names(sl) <- names(d)
sl  # These are the standardized factor loadings for each item
# Compute residual variance of each item
re <- 1 - sl^2

# Compute composite reliability
sum(sl)^2 / (sum(sl)^2 + sum(re))

# ordinal - for cumulative odds models
library(ordinal)
data(wine)
head(wine)
str(wine)

# fit cum. odds model
# logit(P(Y_i <= j)) = theta_j - beta(temp_i) - beta(contact_i)
# i = 1,...,n, j = 1,...,J-1,
# i = num observations,
# J = index of response categories,
# theta_j is intercept for jth cumulative logit
# known as threshold parameters, intercepts or cut-points

# proportional odds assumption / equal slopes assumption
# that all thresholds are constant for each value of remaining explanatory vars
fm1 <- clm(rating ~ temp + contact, data=wine)
fm1
summary(fm1)
# max grad should be very near zero, cond.H should not be above 10e4
# positive coeffs means increasing values makes higher rating more likely

# odds ratio of being rated more bitter at warmer temperature
exp(coef(fm1)[5])

# likelihood ratio test (ANOVA) of contact
fm2 <- clm(rating ~ temp, data=wine)
# this is a more accurate test
# it returns a lower p-value than the full model summary. 
anova(fm2, fm1)

# this can be achieved for each term this
drop1(fm1, test = "Chi")

# these tests ignore the other variables
fm0 <- clm(rating ~ 1, data=wine)
add1(fm0, scope = ~ temp + contact, test = "Chi")

confint(fm1)
confint(fm1, type="Wald") # less accurate, symmetrical based on standard errors

# nominal effects relax the proportional odds/equal slopes assumption
fm.nom <- clm(rating ~ temp, nominal=~contact, data=wine)
summary(fm.nom)
# no regression coeff for contact
# but an additional set of thresholds for levels contact
# values of contactyes coeffs are rather constant - little evidence of effect

# likelihood ratio test
anova(fm1, fm.nom)
# p value of 0.9, no evidence of difference in models

# scale effects - a latent variable interpretation
# S is a continuous random var with distrubition ~N(alpha, sigma)
# S_i is an unobserved linear response to a set of continuous independent vars X^T_i * beta_i + epsilon_i
# Y_i is an observed ordinal response mapping to values of S between thresholds theta*star_j
# theta*star parameters are also unobserved but map to the distribution of S by
# theta_j = ( theta^star_j + alpha ) / sigma and beta = beta^* / sigma

# because we assume S is normally distributed, we use the probit link
# inverse probit is the normal CDF
# logit(P(Y_i <= j)) = (theta_j - beta(temp_i) - beta(contact_i)) / exp(likelihood(temp_i))
fm.sca <- clm(rating ~ temp + contact, scale=~temp, data=wine)
summary(fm.sca)

# estimate k, scale ratio sigma(tempwarm) = k * sigma(tempcold)
exp(fm.sca$zeta)
# with such a high p-value, this doesn't look important.
confint(fm.sca)
confint(fm.sca, type = "Wald") # more appropriate due to log scale
# see how temp warm contains zero
anova(fm1, fm.sca) # high p-value, not signif.

# structure thresholds - e.g. equidistant
fm.equi <- clm(rating ~ temp + contact, data=wine,
               threshold="equidistant")
summary(fm.equi)
c(with(fm.equi, tJac %*% alpha))
# mean difference between thresholds of original model
mean(diff(fm1$alpha))
anova(fm1, fm.equi)
# non-significant difference between the two models
# favour the equi model which has fewer parameters (lower AIC)
# can also do symmetric thresholds, not shown

fitted(fm1) # probability that ith obs falls into the observed category
predict(fm1, type="class") # most likely category for each obs

# probability table
newData <- expand.grid(temp=levels(wine$temp),
                       contact=levels(wine$contact))
cbind(newData, predict(fm1, newdata=newData)$fit)

head(do.call("cbind", predict(fm1, se.fit=TRUE, interval=TRUE)))

# mixed models - taking judges ratings into account
fm1 <- clmm2(rating ~ temp + contact, random=judge, data=wine)
fm1
# alternative (better?) approach to ML estimation
fm2 <- clmm2(rating ~ temp + contact, random=judge, data=wine, Hess=TRUE, nAGQ=10)
summary(fm2)
# small changes to param estimates indicate first version was already accurate

# odds ratio of rating bitterness in category j above when temp is warm
exp(coef(fm1)[5])
fm3 <- clmm2(rating ~ temp, random=judge, data=wine, nAGQ=10)
anova(fm3, fm2)
# a significant difference. temp is a relevant parameter

# difference in models with/without mixed effects (judges)
fm4 <- clm2(rating ~ temp + contact, data=wine)
anova(fm4, fm2)
# this is a test of whether the estimate of variance is signif different from zero. It is.

pr2 <- profile(fm2, range=c(.1, 4), nSteps=30, trace=0)
confint(pr2)
plot(pr2)


# ordPens - ordinal penalised regression
# smoothing and selection of ordinal covariates using a random dataset
set.seed(123)
# generate (ordinal) predictors
x1 <- sample(1:8,100,replace=TRUE)
x2 <- sample(1:6,100,replace=TRUE)
x3 <- sample(1:7,100,replace=TRUE)
# the response
y <- -1 + log(x1) + sin(3*(x2-1)/pi) + rnorm(100)
# x matrix 
x <- cbind(x1,x2,x3)
# lambda values 
lambda <- c(1000,500,200,100,50,30,20,10,1)
# selecting and/or smoothing
o1 <- ordSmooth(x = x, y = y, lambda = lambda)
o2 <- ordSelect(x = x, y = y, lambda = lambda)

ordAOV(x, y)
# results
round(o1$coef,digits=3)
plot(o1)
# If for a certain plot the x-axis should be annotated in a different way,
# this can (for example) be done as follows:
plot(o1, whichx = 1
     , xlim = c(0,9)
     , xaxt = "n")
axis(side = 1, at = c(1,8)
     , labels = c("no agreement","total agreement"))

# results 
round(o2$coef,digits=3)
plot(o2)
# If for a certain plot the x-axis should be annotated in a different way, 
# this can (for example) be done as follows: 
plot(o2, whichx = 1, xlim = c(0,9), xaxt = "n") 
axis(side = 1, at = c(1,8)
     , labels = c("no agreement","total agreement"))

# new data
x1 <- sample(1:8,10,replace=TRUE)
x2 <- sample(1:6,10,replace=TRUE)
x3 <- sample(1:7,10,replace=TRUE)
newx <- cbind(x1,x2,x3)
# prediction
p1 <- round(predict(o1, newx), digits=3)
p2 <- round(predict(o2, newx), digits=3)

with(as.data.frame(newx), newy <- -1 +
       log(x1) +
       sin(3*(x2-1)/pi) +
       rnorm(10))

pen <- "10"
preds <- p1[, pen] 
plot(preds~newx[x2]
     , ylim = c(min(c(preds, newy))
                , c(max(c(preds, newy)))
                )
     )
points(newx[x2], newy, col = "red")

(preds - newy)^2
# selecting best penalty
# MSE

xpr <- as.data.frame(x)
row.names(x) <- seq_along(x[, 1])
lvs <- 1:8
ordGene(xpr, y)

# calculate p-values pvals <- ordGene(xpr = xpr, lvs = dose, nsim=1e6)
# compare distribution of (small) p-values plot(ecdf(pvals[,1]), xlim=c(0,0.05), ylim=c(0,0.12), main="dopamine", xlab="p-value", ylab="F(p-value)") plot(ecdf(pvals[,2]), xlim=c(0,0.05), add=TRUE, col=2) plot(ecdf(pvals[,3]), xlim=c(0,0.05), add=TRUE, col=3) legend('topleft', colnames(pvals), col=1:3, lwd=2, lty=1)





# new data
x1 <- sample(1:5,100,replace=TRUE)
x2 <- sample(1:5,100,replace=TRUE)
x3 <- sample(1:5,100,replace=TRUE)
x4 <- sample(1:5,100,replace=TRUE)
x5 <- sample(1:5,100,replace=TRUE)

newx <- cbind(x1, x2, x3, x4, x5)
betas <- c(2, 3, 1.5, 2.2, 0.3)

# linear
y <- -1 + newx%*%betas

# prediction



# real data example
# some data
data(ICFCoreSetCWP)

# available variables
names(ICFCoreSetCWP)

# the pysical health component summary
y <- ICFCoreSetCWP$phcs

# consider the first ordinal factor
x <- ICFCoreSetCWP[,1]

# adequate coding
x <- as.integer(x - min(x) + 1)

# ANOVA - simulates from NULL dist
ordAOV(x, y
       , type = "RLRT"
       , nsim=1000000) # testing if relevant

# full model
# adequate coding of x matrix (using levels 1,2,...)
# because they don't all use the same levels
p <- ncol(ICFCoreSetCWP) - 1
n <- nrow(ICFCoreSetCWP)
add <- c(rep(1,50),rep(5,16),1)
add <- matrix(add,n,p,byrow=TRUE)
x <- ICFCoreSetCWP[,1:p] + add

# make sure that also a coefficient is fitted for levels
# that are not observed in the data
addrow <- c(rep(5,50),rep(9,16),5)
x <- rbind(x,addrow)
y <- c(ICFCoreSetCWP$phcs,NA)

# some lambda values
lambda <- c(600,500,400,300,200,100)
# smoothing and selection
modelICF <- ordSelect(x = x
                      , y = y
                      , lambda = lambda)
# results
plot(modelICF)
# plot a selected ICF category (e.g. e1101 'drugs')
# with adequate class labels
plot(modelICF, whichx = 51, xaxt = "n")
axis(side = 1, at = 1:9, labels = -4:4)

# Testing Ordinal Predictors
a <- 1 # effect strength
x1 <- sample(0:9,100,replace=TRUE)

# linear
y <- x1 * a * 1 + rnorm(100, 0, 0.05) # linear
plot(y~x1)

# log
y <- log(1 + sqrt(x1) * 5) * a + rnorm(100, 0, 0.05) # linear
plot(y~x1)

y <- sqrt(x1) * a + rnorm(100, 0, 0.05) # quadratic
plot(y~x1)

y <- sqrt(x1) + 
  sin(sqrt(sqrt((x1)^2 + 
                  (x1)))) * a + rnorm(100, 0, 0.05)
plot(y~x1)



cd <- function(x, by = NULL, allPen = FALSE, diag = FALSE, varying = NULL) {
  n <- length(x)
  # call <- as.list(expand.call())
  # X <- matrix(0, n, 0)
  X <- cbind(x - 1)
  Z <- matrix(rep(x, max(x) - 1), n, max(x) - 1)
  Z <- Z - matrix(rep(1:(max(x) - 1), n), dim(Z)[1], dim(Z)[2], byrow = T)
  Z[Z < 0] <- 0
  Z[Z > 1] <- 1
  res <- list(X = X, Z = Z)
  # attr(res, "call") <- as.call(call)
  return(res)
}

ld <- function(x, by = NULL, allPen = FALSE, diag = FALSE, varying = NULL) {
  n <- length(x)
  # call <- as.list(expand.call())
  X <- cbind(x - 1)
  Z <- matrix(rep(x, max(x) - 2), n, max(x) - 2)
  Z <- Z - matrix(rep(2:(max(x) - 1), n), dim(Z)[1], dim(Z)[2], byrow = T)
  Z[Z < 0] <- 0
  res <- list(X = X, Z = Z)
  # attr(res, "call") <- as.call(call)
  return(res)
}

dyx = data.frame(X = cd(x)$X, Z = cd(x)$Z)
fm1 <- lmer(y ~ Z.1 + (Z.1 | X) +
              Z.2 + (Z.2 | X) +
              Z.3 + (Z.3 | X) +
              Z.4 + (Z.4 | X) +
              Z.5 + (Z.5 | X) +
              Z.6 + (Z.6 | X) +
              Z.7 + (Z.7 | X) +
              Z.8 + (Z.8 | X) +
              Z.9 + (Z.9 | X), data = dyx)

set.seed(1701)
n <- 100
x <- c(1:10, sample(1:10, n - 10, replace = T))

y <- 4/9 *
  (x - 1) -
  1/30 *
  (x - 1) *
  (x - 10) +
  rnorm(100)
dyx <- data.frame(y, x) 
plot(y~x)
