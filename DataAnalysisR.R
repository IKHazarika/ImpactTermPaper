#Importing and cleaning the data
library(stargazer)
library(readxl)
dfRaw <- read_excel("E:/Semester 3/Impact Evaluation/Term Paper/CleanedData3.xlsm")

summary(df1$ScoreUG)

df1 <- dfRaw[! is.na(dfRaw$`HRC Cutoff`), ]
df1[(df1$ScoreUG == 86.0) & (df1$College == 'Hindu'), ]
df1$Score12 <- as.numeric(df1$Score12)
df1 <- df1[! is.na(df1$Score12), ]

df1$HinduExcess <- df1$Score12 - df1$`Hindu Cutoff`
df1$SRCCExcess <- df1$Score12 - df1$`SRCC Cutoff`

nrow(df1[(df1$HRC == 1) & (df1$HinduExcess >= 0), ])
nrow(df1[(df1$HRC == 1) & (df1$SRCCExcess >= 0), ])
nrow(df1[(df1$Hindu == 1) & (df1$SRCCExcess >= 0), ])

df1 <- df1[df1$Category == "OBC", ]

nrow(df1[(df1$HRC == 1) & (df1$HinduExcess >= 0), ])
nrow(df1[(df1$HRC == 1) & (df1$SRCCExcess >= 0), ])
nrow(df1[(df1$Hindu == 1) & (df1$SRCCExcess >= 0), ])

#Transforming variables

polAppr <- function(y, x, et) {
  
  df = data.frame(x)
  mdl = lm(y ~ ., data = df)
  er = max(abs(mdl[["residuals"]]))
  
  k = 1
  while(er > et) {
    k = k + 1
    df[, k] <- (x^k)
    
    mdl = lm(y ~ ., data = df)
    er = max(abs(mdl[["residuals"]]))
  }
  
  coefs = mdl[["coefficients"]]
  trms = length(coefs)
  
  trFun = function(inp) {
    pred = 0
    for (roll in 1:trms) {
      if (! is.na(coefs[roll]))
        pred = pred + coefs[roll] * inp^(roll - 1)
    }
    return(pred)
  }
  
  print(coefs)
  return(trFun)
}

dataGen <- function(a, b, mn, mx, fr) {
  x = seq(mn, mx, fr)
  
  y1 = seq(mn, a, fr)
  y3 = seq(a, mx - b + a, fr)
  y2 = rep(a, length(x) - length(y1) - length(y3))
  
  y = c(y1, y2, y3)
  
  dfRes = data.frame(y, x)
  
  return(dfRes)
}

ulim <- max(df1$`SRCC Cutoff`)
llim <- min(df1$`SRCC Cutoff`)
lVal <- min(df1$Score12)
uVal <- max(df1$Score12)

ficData <- dataGen(95, 96, 92.6, 98.2, 0.1)
transformer <- polAppr(ficData$y, ficData$x, 0.1)

df1$Score12Tr <- transformer(df1$Score12)
df1$Score12Tr <- df1$Score12Tr - 94.5

#Checking RDD power

library(rdpower)
library(rddtools)
library(texreg)
library(xtable)

pow <- rdpower(as.matrix(df1[, c(3, 13)]), tau = 0.5)
pow <- rdpower(as.matrix(df1[, c(3, 13)]), tau = 1)
pow <- rdpower(as.matrix(df1[, c(3, 13)]), tau = 2)

stargazer(pow)

pow <- rdpower(as.matrix(df1[, c(3, 12)]), tau = 0.5)
pow <- rdpower(as.matrix(df1[, c(3, 12)]), tau = 1)
pow <- rdpower(as.matrix(df1[, c(3, 12)]), tau = 2)

rm(pow)

data <- rdd_data(df1$ScoreUG, df1$SRCCExcess, cutpoint = 0)

#Plot the sample data
plot(data,
     col = "steelblue",
     cex = 0.35, 
     xlab = "W", 
     ylab = "Y",)

rm(data)

min(df1[df1$Category == 'General', ]$`SRCC Cutoff`)
max(df1[df1$Category == 'General', ]$`SRCC Cutoff`)

min(df1[df1$Category == 'General', ]$`Hindu Cutoff`)
max(df1[df1$Category == 'General', ]$`Hindu Cutoff`)

plot(df1$SRCCExcess, df1$ScoreUG)

#Checking for manipulation

library(rddensity)

test_density <- rddensity(df1$SRCCExcess, c = 0)
summary(test_density)

plot_density_test <- rdplotdensity(rdd = test_density, 
                                   X = df1$SRCCExcess, 
                                   type = "both")

test_density2 <- rddensity(df1$HinduExcess, c = 0)
summary(test_density2)

plot_density_test <- rdplotdensity(rdd = test_density2, 
                                   X = df1$HinduExcess, 
                                   type = "both")

#Running a strict RDD

rdd_mod <- rdd_reg_lm(rdd_object = data, 
                      slope = "same")
summary(rdd_mod)

#Alternatively

library(dplyr)
library(ggplot2)
library(rddtools)
library(magrittr)

model1 <- rdd_data(y = df1$ScoreUG, 
         x = df1$SRCCExcess, 
         cutpoint = 0) %>% 
  rdd_reg_lm(slope = "same")

stargazer(model1)

rdd_data(y = df1$ScoreUG, 
         x = df1$SRCCExcess, 
         cutpoint = 0) %>% 
  rdd_reg_lm(slope = "separate") %>% 
  summary()

rdd_data(y = df1$ScoreUG, 
         x = df1$SRCCExcess, 
         cutpoint = 0) %>% 
  rdd_reg_lm(slope = "separate", order = 2) %>% 
  summary()

library(rdrobust)

rdrobust(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0) %>% 
  summary()

rdplot(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0)

rdbwselect(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0, all = TRUE) %>% 
  summary()

rdrobust(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0, h = 0.920 * 2) %>% 
  summary()

rdrobust(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0, h = 0.920 / 2) %>% 
  summary()

#Fuzzy regression for SRCC

#Parametric

library(estimatr)
library(rdd)

modelNewPar1 <- RDestimate(ScoreUG ~ SRCCExcess + SRCC, data = df1, cutpoint = 0)
summary(modelNewPar1)

stargazer(modelNewPar1)

#https://cran.r-project.org/web/packages/rdd/rdd.pdf

#Non-parametric

model_nonpar <- rdrobust(y = df1$ScoreUG, x = df1$SRCCExcess, 
         c = 0, fuzzy = df1$SRCC)

summary(model_nonpar)

#Fuzzy regression for Hindu

#Parametric

library(estimatr)
library(rdd)

df2 <- df1[df1$SRCC != 1, ]

modelNewPar2 <- RDestimate(ScoreUG ~ HinduExcess + Hindu, data = df2, cutpoint = 0)
summary(modelNewPar2)

#https://cran.r-project.org/web/packages/rdd/rdd.pdf

#Non-parametric

model_nonpar2 <- rdrobust(y = df2$ScoreUG, x = df2$HinduExcess, 
                         c = 0, fuzzy = df2$Hindu)

summary(model_nonpar2)

#Plots

rdplot(y = df1$ScoreUG, x = df1$SRCCExcess, c = 0)
rdplot(y = df1$ScoreUG, x = df1$HinduExcess, c = 0)
