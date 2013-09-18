rm(list=ls())
options(width = 200)
library(spBayes)
library(geoR)
library(gstat)
library(MBA)
## Holdout Data #########################
y.hold <- as.vector(read.csv("model_data/y.hold", header=TRUE))
x.hold <- as.matrix(read.csv("model_data/x.hold", header=TRUE))
coords.hold <- as.matrix(read.csv("model_data/coords.hold", header=TRUE))
## Model Data ###########################
y.mod <- as.vector(read.csv("model_data/y.mod", header=TRUE))
x.mod <- as.matrix(read.csv("model_data/x.mod", header=TRUE))
coords.mod <- as.matrix(read.csv("model_data/coords.mod", header=TRUE))
##
########################################
## linear model
########################################
X <-  as.matrix(x.mod[,c("P50","P95")])
coords <- coords.mod[,1:2]
Y <- as.vector(y.mod[,"biomass"])
mod <- lm(sqrt(Y)~X)
summary(mod)
vario <- variog(coords = coords,
                data = resid(mod),
                max.dist=max(iDist(coords))/2,
                uvec=(seq(0, 500, length=10)))
variomod <- variofit(vario, cov.model="exponential", fix.nugget=TRUE, nugget=10, c(120, 20))
plot(vario);lines(variomod, col = "red")

tau.sq <- variomod$nugget
sigma.sq <- variomod$cov.pars[1]
phi <- 3/variomod$cov.pars[2]
########################################
## spatial model ##
########################################
n.samples <- 100000
knots <- c(10,10,0.1)
starting <- list("phi"=phi,"sigma.sq"=sigma.sq, "tau.sq"=tau.sq)
tuning <- list("phi"=0.003, "sigma.sq"=0.003, "tau.sq"=0.003)
priors <- list("phi.Unif"=c(3/max(iDist(coords)), 3/1),
               "sigma.sq.IG"=c(2,sigma.sq),
               "tau.sq.IG"=c(2,tau.sq))
cov.model <- "exponential"
verbose <- TRUE
n.report <- 10

set.seed(1)
spmod <- spLM(sqrt(Y)~X, coords=coords,
              knots=knots, starting=starting, tuning=tuning,
              priors=priors, cov.model=cov.model,
              n.samples=n.samples, verbose=verbose,
              n.report=n.report)

plot(spmod$p.theta.samples,density=F)
colMeans(spmod$p.theta.samples)

5401 6830 9627 7840

11 13

330
