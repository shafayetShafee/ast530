####################################################################
#
# Example of joint, marginal, and conditional distributions (sample)
# Using the Obama voting data.
#
####################################################################

#Load data

dat <- read.csv("http://www4.stat.ncsu.edu/~reich/ST590/assignments/Obama2012.csv")
pctObama <- 100 * dat[, 2]
pctUnEmp <- dat[, 18]

############################################
# Convert to discrete variables
############################################

X <- ifelse(pctObama > 50, 1, 0)
Y <- ifelse(pctUnEmp > 10, 1, 0) + ifelse(pctUnEmp > 15, 1, 0)
plot(Y,pctUnEmp)

# Compute the sample joint distribution
table(X, Y) / 100

# Compute the sample marginal distributions
table(X) / 100
table(Y) / 100

# Compute the conditional probabilities
mean(X[Y == 0])
mean(X[Y == 1])
mean(X[Y == 2])



############################################
# Plot for continuous variables
############################################

X <- pctObama
Y <- pctUnEmp

#Joint
plot(X, Y, main = "Joint distribution")

# Marginals
hist(X, main = "Marginal distribution of X")
hist(Y, main = "Marginal distribution of Y")

# Probability in a set
inA <- (X > 50) & (Y > 10) & (Y < 15)
plot(X, Y, col = ifelse(inA, 2, 1), main = "Prob in set A")
polygon(c(50, 50, 100, 100), c(10, 15, 15, 10))

# Approximate conditional pdf
Y10 <- Y > 9.5 & Y < 10.5
plot(X, Y, col = ifelse(Y10, 2, 1))
abline(9.5, 0)
abline(10.5, 0)

X10 <- X[Y10]
hist(X10,
     main = "f(x|Y=10)",
     xlim = range(X),
     prob = TRUE)

##########################################################################
#
# Exploring the bivariate normal distribution
#
##########################################################################

binorm <- function(x, y, muX=0, muY=0, sigmaX=1, sigmaY=1, rho=0) {
  c <- sigmaX * sigmaY * sqrt(1 - rho^2) * 2 * pi
  d <- 1 / (1 - rho ^ 2)
  z_x <- (x - muX) / sigmaX
  z_y <- (y - muY) / sigmaY
  pdf <- (1 / c) * exp(-0.5 * d * (z_x^2 + z_y^2 - 2 * rho * z_x * z_y))

  return(pdf)
}

## Marginal distribution

m <- 100
pts <- seq(-3, 3, length=m)
grid <- expand.grid(pts, pts)
plot(grid)


muX <- 0
muY <- 0
sigmaX <- 1
sigmaY <- 1
rho <- 0.9

pdf <- binorm(grid[,1], grid[,2], muX, muY, sigmaX, sigmaY, rho)
pdf <- matrix(pdf, m, m)

library(fields)

image.plot(pts, pts, pdf,
           xlab = "x", ylab="y",
           main = "Bivariate normal PDF",
           col = gray(1-seq(0,1,.05)))

# Marginal distribution

fx <- colSums(pdf)
fx <- fx/sum(fx)

par(mfrow = c(1, 2))
plot(pts, fx, type="l", xlab="x", ylab="f(x)")
plot(pts, dnorm(pts)/sum(dnorm(pts)), type = "l")


## Conditional distribution

grd <- round(grid, 3)
x <- grd$Var1[grd$Var2==-1.848]

p <- binorm(x,rep(-1.848,length(x)),muX,muY,sigmaX,sigmaY,rho)
p1 <- p/sum(p)

p_y <- dnorm(-.184)
p_y1 <- p_y/sum(p_y)

p_con_x <- p1/p_y1
plot(x, p_con_x, type = "l")


par(mfrow=c(2,2))
image.plot(pts, pts, pdf,
           xlab="x",ylab="y",
           main="Bivariate normal PDF",
           col=gray(1-seq(0,1,.05)))

abline(pts[80],0,lty=2)
abline(pts[50],0,lty=2)
abline(pts[20],0,lty=2)

cond20 <- pdf[20, ]
cond20 <- cond20/sum(cond20)
plot(pts, cond20, type="l", xlab="x", ylab="f(x|y)",
     main=paste("Y =", round(pts[20],3)))

cond50 <- pdf[50,]
cond50 <- cond50/sum(cond50)
plot(pts, cond50, type="l", xlab="x", ylab="f(x|y)",
     main=paste("Y =",round(pts[50],3)))

cond80 <- pdf[80,]
cond80 <- cond80/sum(cond80)
plot(pts,cond80,type="l",xlab="x",ylab="f(x|y)",
     main=paste("Y =",round(pts[80],3)))


###########################################################################
# HIV testing example of Bayes' rule
# In this example, the patient takes an HIV test and we compute the
# posterior probability that the patient has HIV. Let

# p=prior probability that the patient has HIV
# q0=probability of a positive test given the patient is negative
# q1=probability of a positive test given the patient is positive

post_prob <- function(p, q0, q1) {
  p * q1 / (p * q1 + (1-p) * q0)
}

# base case
p  <- 0.50   # Prior probability
q0 <- 0.01   # False positive probability
q1 <- 0.90   # True positive probability

post_prob(p, q0, q1) # prob that test is + and patient is +

# effect of the prior

grid  <- seq(0.01, 0.99, .01)

plot(grid, post_prob(grid, q0, q1),
     type="l",
     xlab="Prior probability",
     ylab="Posterior probability")

# Effect of the likelihood - false positive rate

plot(grid,post_prob(p, grid, q1),
     type="l",
     xlab="False positive rate",
     ylab="Posterior probability")

# Effect of the likelihood - true positive rate

plot(grid, post_prob(p, q0, grid),
     type="l",
     xlab="True positive rate",
     ylab="Posterior probability")


# Monte carlo
n     <- 10000
theta <- NULL
Y     <- NULL

# start sampling
for(i in 1:n){
  theta[i] <- rbinom(1,1,p)
  prob <- ifelse(theta[i]==1,q1,q0)
  Y[i] <- rbinom(1,1,prob)
}

table(Y,theta)/n  # Approximate joint distribution

mean(theta[Y==1]) # Approximate conditional probability


# Normal/normal model

# 1. Plot the prior, likelihood, and posterior on a grid

Y      <- 0.10
sigma  <- 0.005
m      <- 0.05
s      <- 0.025

grid   <- seq(0,0.15, .001)

like   <- dnorm(Y, grid, sigma)
like   <- like/sum(like) #standardize

prior  <- dnorm(grid, m, s)
prior  <- prior/sum(prior) #standardize

post   <- like*prior
post   <- post/sum(post)

plot(grid,like,type="l",lty=2,
     xlab="mu",ylab="Density")

lines(grid,prior)
lines(grid,post,lwd=2)

legend("topleft",c("Likelihood","Prior","Posterior"),
       lwd=c(1,1,2),lty=c(2,1,1),inset=0.05)

# 2. Exact

post_var <- 1/(sigma^{-2}+s^{-2})          # Posterior var
post_sd  <- sqrt(post_var)                 # Posterior sd
post_mn  <- post_var*(Y/sigma^2+m/s^2)     # Posterior mean

post_mn
post_sd
qnorm(c(0.025,0.975),post_mn,post_sd)


########################################################################
#########################################################################
#########################################################################

# Define real pars mu and sigma, sample 100x
trueMu <- 5
trueSig <- 2
set.seed(100)

randomSample <- rnorm(100, trueMu, trueSig)

# Grid approximation, mu %in% [0, 10] and sigma %in% [1, 3]

grid <- expand.grid(mu = seq(0, 10, length.out = 200),
                    sigma = seq(1, 3, length.out = 200))
# Compute likelihood
lik <- sapply(1:nrow(grid), function(x){
  sum(dnorm(x = randomSample, mean = grid$mu[x],
            sd = grid$sigma[x], log = T))
})

# Multiply (sum logs) likelihood and priors
prod <- lik + dnorm(grid$mu, mean = 0, sd = 5, log = T) +
  dexp(grid$sigma, 1, log = T)

# Standardize the lik x prior products to sum up to 1, recover unit
prob <- exp(prod - max(prod))

# Sample from posterior dist of mu and sigma, plot
postSample <- sample(1:nrow(grid), size = 1e3, prob = prob)

plot(grid$mu[postSample], grid$sigma[postSample],
     xlab = "Mu", ylab = "Sigma", pch = 16, col = rgb(0,0,0,.2))

abline(v = trueMu, h = trueSig, col = "red", lty = 2)

