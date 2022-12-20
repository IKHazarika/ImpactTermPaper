a1 = 0
a2 = 0
a3 = 0
a4 = 0
a5 = 0
b1 = runif(1, 0, 1)
b2 = runif(1, -1, 0)
b3 = runif(1, -1, 1)
b4 = runif(1, -1, 0)
b5 = runif(1, 0, 1)
c2 = runif(1, 0, 1)
c3 = runif(1, 0, 1)

#b1 = 10
#b2 = -20
#b3 = 30
#b4 = -40
#b5 = 50
#c2 = 60
#c3 = 70

coll = sample.int(2, 10000, replace = TRUE) - 1
oppo = a1 + b1 * coll + rnorm(10000, 0, 0.1)
desi = a4 + b4 * oppo + rnorm(10000, 0, 0.1)
effo = a2 + b2 * oppo + c2 * desi + rnorm(10000, 0, 0.1)
cgpa = a3 + b3 * coll + c3 * effo + rnorm(10000, 0, 0.1)
appl = a5 + b5 * desi + rnorm(10000, 0, 0.1)
appb = rep(0, 10000)
gpab = rep(0, 10000)
appc = runif(1, 0, 0.2)
for (i in 1:10000) {
  if (appl[i] >= appc) {
    appb[i] = 1
  }
}
gpac = runif(1, 0.1, 0.5)
for (i in 1:10000) {
  if (cgpa[i] >= gpac) {
    gpab[i] = 1
  }
}
admi = appb *gpab

colleges = c()
oppos = c()
gpas = c()

for (i in 1:10000) {
  if (admi[i] == 1) {
    colleges = c(colleges, coll[i])
    oppos = c(oppos, oppo[i])
    gpas = c(gpas, cgpa[i])
  }
}

model <- lm(gpas ~ colleges + oppos)
summary(model)

model2 <- lm(cgpa ~ coll + oppo)
summary(model2)

b3

#########################################################################

install.packages('rdrobust')

library(rdrobust)

install.packages('rdpower')

library(rdpower)

?rdsampsi
